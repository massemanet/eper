%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 25 May 2009 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module('bread').
-author('Mats Cronqvist').

-export([xml/3
         , term/3
         , line/3
         , trc/3
         , fold/4]).

-define(BLOCK, 1048576).

%% this is an attempt to use "abstract patterns"
-define(tail_0(),             {<<>>,0,''}).
-define(tail(Bin,Offset),     {Bin,Offset,''}).
-define(tail_eof(Bin,Offset), {Bin,Offset,eof}).
-define(state(Acc,Tail),      {Acc,Tail}).

-define(log(T),
        error_logger:info_report(
          [process_info(self(),current_function),{line,?LINE}]++T)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% the API
line(Filename,Fun,Acc) ->
  fold(Filename,Fun,Acc,[line]).

xml(Filename,Fun,Acc) ->
  fold(Filename,Fun,Acc,[xml]).

term(Filename,Fun,Acc) ->
  fold(Filename,Fun,Acc,[term]).

trc(Filename,Fun,Acc) ->
 fold(Filename,Fun,Acc,[trc]).

fold(Filename,Fun,Acc,Opts) ->
  Type = take_first(Opts,[line,xml,term,trc]),
  case file:open(Filename, [read, raw, binary, compressed]) of
    {ok, FD} ->
      try fold(read(FD),FD,wrap(Fun,Type),chunker(Type),?state(Acc,?tail_0()))
      after file:close(FD)
      end;
    {error,R} ->
      exit({open_error, R, Filename})
  end.

wrap(Fun,line) -> Fun;
wrap(Fun,xml)  -> Fun;
wrap(Fun,term) -> term_f(Fun);
wrap(Fun,trc)  -> Fun.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% term funs
term_f(Fun) ->
  fun(TermBin,O) ->
      try Fun(to_term(TermBin),O)
      catch {parsing_failed,R} -> ?log([{parse_error,R},{string,TermBin}]),O
      end
  end.

to_term(Bin) ->
  try
    {ok,Ts,_} = erl_scan:string(binary_to_list(Bin)),
    {ok,Term} = erl_parse:parse_term(Ts),
    Term
  catch
    _:R -> throw({parsing_failed,R})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% implementation
fold(eof,_,Fun,Chunker,State) ->
  ?state(Acc,Tail) = folder(Chunker,Fun,state_eof(State)),
  [?log([{trailing,Tail}]) || not is_empty(Tail)],
  Acc;
fold({ok,Chunk},FD,Fun,Chunker,State) ->
  fold(read(FD),FD,Fun,Chunker,folder(Chunker,Fun,add_chunk(Chunk,State))).

folder(Chunker,Fun,?state(Acc,Tail)) ->
  case Chunker(Tail) of
    {ok,Term,NTail} -> folder(Chunker,Fun,?state(Fun(Term,Acc),NTail));
    {cont,NTail} -> ?state(Acc,NTail)
  end.

read(FD) ->
  file:read(FD, ?BLOCK).

add_chunk(Chunk,?state(Acc,?tail(Cnt,Off))) ->
  erlang:garbage_collect(),
  <<_:Off/binary,C/binary>> = Cnt,
  ?state(Acc,?tail(<<C/binary,Chunk/binary>>,0)).

is_empty(?tail(Bin,Off)) ->
  byte_size(Bin) == Off.

state_eof(?state(Acc,?tail(Bin,Off))) ->
  ?state(Acc,?tail_eof(Bin,Off)).

-define(is_empty(X), X=:=<<>>; X=:=<<"\n">>; X=:=<<"\n\r">>).
chunker(trc) ->
  fun(?tail_eof(Bin,Off)) ->
     case Bin of
       <<_:Off/binary>> ->
         {cont,?tail_0()};
       <<_:Off/binary,0,I:4/integer,Rs/binary>> ->
         ?log([{trailing_bytes,byte_size(Rs)+4},{needed,I}]),
         {cont,?tail_0()};
       <<_:Off/binary,R/binary>> ->
         ?log([{garbarge_at_eof,byte_size(R)}]),
         {cont,?tail_0()}
     end;
     (?tail(Bin,Off)) ->
      case Bin of
        <<_:Off/binary,0,S:32/integer,B:S/binary,_/binary>> ->
          {ok,binary_to_term(B),?tail(Bin,Off+5+S)};
        <<_:Off/binary,R/binary>> ->
          {cont,?tail(R,0)}
      end
  end;
chunker(Type) ->
  {ok,Patt} = chunker_patt(Type),
  fun(?tail_eof(Bin,Off)) ->
      case split_binary(Bin,Off) of
        {_,T} when ?is_empty(T) -> {cont,?tail_0()};
        {_,Trail}      -> {ok,Trail,?tail_0()}
      end;
     (Tail=?tail(Bin,Off)) ->
      case re:run(Bin,Patt,[{offset,Off},{capture,[1]}]) of
        nomatch         -> {cont,Tail};
        {match,[{O,L}]} -> {ok,snip(Bin,O,L),?tail(Bin,O+L)}
      end
  end.

chunker_patt(term) ->
  re:compile("(.*\\.\\R)",[dotall,ungreedy]);
chunker_patt(xml) ->
  re:compile("(<\\?xml.*\\?>.*)<\\?xml.*\\?>",[dotall,caseless,ungreedy]);
chunker_patt(line) ->
  re:compile("\\R*(.+)\\R").

snip(Bin,B,L) ->
  <<_:B/binary,R:L/binary,_/binary>> = Bin,
  R.

take_first(Opts,[Alt|Alts]) ->
  case proplists:is_defined(Alt,Opts) of
    true -> Alt;
    false-> take_first(Opts,Alts)
  end.
