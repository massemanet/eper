%% -*- erlang-indent-level: 2 -*-
%%% Created : 14 Mar 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-author('Mats Cronqvist').

-define(log(T),	error_logger:info_report(
		  [process_info(self(),current_function),{line,?LINE}|
		   try is_integer(lists:nth(1,T)) of 
		     true -> [T];
		     false-> T
		   catch error:_ -> [T]
		   end])).
