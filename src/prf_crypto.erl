%% -*- erlang-indent-level: 2 -*-
%%% Created :  7 Aug 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module(prf_crypto).
-author('Mats Cronqvist').
-export([encrypt/1,encrypt/2,decrypt/1,decrypt/2]).

phrase() -> atom_to_list(erlang:get_cookie()).

encrypt(Data) -> encrypt(phrase(),Data).

encrypt(Phrase,Data) ->
  assert_crypto(),
  {Key,Ivec} = make_key(Phrase),
  crypto:des_cbc_encrypt(Key,Ivec,pad(Data)).

decrypt(Data) -> decrypt(phrase(),Data).

decrypt(Phrase,Data) ->
  assert_crypto(),
  {Key,Ivec} = make_key(Phrase),
  unpad(crypto:des_cbc_decrypt(Key,Ivec,Data)).

make_key(Phrase) ->
  <<Key:8/binary,Ivec:8/binary>> = crypto:md5(Phrase),
  {Key,Ivec}.

pad(Term) ->
  Bin = term_to_binary(Term),
  BinSize = size(Bin),
  PadSize = 7-((BinSize+3) rem 8),
  PadBits = 8*PadSize,
  <<BinSize:32,Bin/binary,0:PadBits>>.

unpad(<<BinSize:32,R/binary>>) ->
  <<Bin:BinSize/binary,_/binary>> = R,
  binary_to_term(Bin).

assert_crypto() ->
  case whereis(crypto) of
    undefined -> crypto:start();
    _ -> ok
  end.
