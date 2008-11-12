%% -*- erlang-indent-level: 2 -*-
%%% Created : 14 Mar 2008 by Mats Cronqvist <masse@kreditor.se>

%% this has infinitely high bitefactor.
%% alas, it is a consequence of the braindead textual macros.

-define(log(T),'?log'(process_info(self(),current_function),{line,?LINE},T)).
'?log'(CF,Line,T) when not is_integer(hd(T)) -> 
  %% T is a list and it's not a string.
  error_logger:info_report([CF,Line|T]);
'?log'(CF,Line,T)  -> '?log'(CF,Line,[T]).
