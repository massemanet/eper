%% -*- erlang-indent-level: 2 -*-
%%% Created : 14 Mar 2008 by Mats Cronqvist <masse@kreditor.se>

%% this has infinitely high bitefactor.
%% alas, it is a consequence of the braindead textual macros.

-define(log(T),
        '?log'([process_info(self(),current_function)
                , {line,?LINE}]
               ,T)).
-define(log_bt(T),
        '?log'([process_info(self(),current_function)
                , {line,?LINE}
                , {bt,erlang:get_stacktrace()}]
               , T)).

'?log'(HD,T) when not is_integer(hd(T)) -> error_logger:info_report(HD++T);
'?log'(HD,T)  -> '?log'(HD,[T]).
