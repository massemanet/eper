%%%-------------------------------------------------------------------
%%% File    : sherk_tab.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 21 Aug 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk_tab).

-export([assert/1,check_file/1]).

-import(filename,[dirname/1,join/1,basename/2]).

-include_lib("kernel/include/file.hrl").

-define(LOG(T), sherk:log(process_info(self()),T)).

assert(File) ->
    TabFile = dirname(File)++"/."++basename(File,".trz")++".etz",
    {ok,#file_info{mtime=MT}} = file:read_file_info(File),
    case file:read_file_info(TabFile) of
        {ok,#file_info{mtime=TabMT}} when MT < TabMT -> 
            %% the tab file exists and is up-to-date
            case sherk_ets:lup(sherk_prof, file) of
                File -> ok;                     % table is in memory
                _ -> sherk_ets:f2t(TabFile)     % need to load table
            end;
        _ -> 
            %% make tab and save it
            sherk_scan:action(File,'',sherk_prof,0,''),
            ets:foldl(fun store_pid/2, [], sherk_prof),
            ets:insert(sherk_prof, {file, File}),
            try 
                sherk_ets:t2f([sherk_prof,sherk_scan],TabFile),
                ?LOG({created,TabFile})
            catch 
                _:_ -> ?LOG({creation_failed,TabFile})
            end
    end.

store_pid({{{pid,time},P},_},_) -> ets:insert(sherk_prof,{pid_to_list(P),P});
store_pid(_,_) -> ok.

check_file(File) -> ".trz" = filename:extension(File).
