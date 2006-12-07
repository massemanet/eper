%%%-------------------------------------------------------------------
%%% File    : sherk.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 14 Aug 2006 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(sherk).

-export([go/0]).  % interactive
-export([ni/0]).  % non-interactive
-export([to_str/1]).

%% internal exports
-export([log/2]).
-export([loop/1]).

-import(filename,[dirname/1,join/1]).
-import(lists,[foreach/2,member/2,flatten/1,usort/1,foldl/3]).
-import(dict,[from_list/1,to_list/1,fetch/2,store/3,new/0,append/3]).

-define(LOG(T), sherk:log(process_info(self()),T)).
-define(LOOP(X), ?MODULE:loop(X)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
go() -> spawn_link(fun init/0).
ni() -> init().
    
init() ->
    
    %% start the GUI and load the glade file
    gtknode:start(sherk),
    g('GN_glade_init',[join([code:priv_dir(eper),glade,"sherk.glade"])]),

    %% check the trc source
    check_file(),
    %% set default trc destination
    g('Gtk_file_chooser_set_current_folder',[aq_filechoose,"/tmp"]),
    g('Gtk_file_chooser_set_current_folder',[main_filechoose,"/tmp"]),

    %% init nodes treeview
    NodeList = init_list_store([string]),
    init_tree_view(aq_treeview,{NodeList,init_tree_view_cols([{0,"Nodes"}])}),
    set_selection_mode(aq_treeview,'GTK_SELECTION_MULTIPLE'),

    %% init calls treeview
    CallList = {init_list_store([string,integer,integer]),
                init_tree_view_cols([{0,"MFA"},{1,"calls"},{2,"%"}])},
    CallTree = {init_tree_store([string,integer,integer,integer]),
                init_tree_view_cols([{0,"MFA"},{1,"calls"},
                                     {2,"time"},{3,"cumtime"}])},
    
    %% init procs treeview
    ProcsTree = init_tree_store([string,integer,integer,integer]),
    ColDescr = [{0,"ID"},{1,"#"},{2,"gc"},{3,"cpu%"}],
    init_tree_view(procs_treeview,{ProcsTree,init_tree_view_cols(ColDescr)}),
    
    %% init calls combobox
    init_combobox(ProcsTree),

    loop(from_list([{targs         ,[]},
                    {bad_targs     ,[]},
                    {aq_mon        ,undefined},
                    {proxy         ,node()},
                    {orig_ticktime ,net_kernel:get_net_ticktime()},
                    {targ_mon      ,query_targs(node())},
                    {call_list     ,CallList},
                    {call_tree     ,CallTree}])).

loop(LD) ->
    AqMon = fetch(aq_mon,LD),
    TargMon = fetch(targ_mon,LD),
    receive
	%% user bored
	quit                                 -> ok;
	{sherk,{signal,{quit,_}}}            -> ok;
	{sherk,{signal,{main_window,_}}}     -> ok;

        loopdata                             -> ?LOOP(show_ld(LD));
	%% user wants stuff hidden
	{sherk,{signal,{call_window,_}}}     -> hide(call_window),?LOOP(LD);
	{sherk,{signal,{aq_window,_}}}       -> hide_aq_window(),?LOOP(LD);
	{sherk,{signal,{show_aquire,_}}}     -> toggle_aq_window(),?LOOP(LD);

	%% user curious
	{sherk,{signal,{about,_}}}           -> show(about_window),?LOOP(LD);
	{sherk,{signal,{about_window,_}}}    -> hide(about_window),?LOOP(LD);
	{sherk,{signal,{about_ok,_}}}        -> hide(about_window),?LOOP(LD);

	%% user changed directory
	{sherk,{signal,{main_filechoose,_}}} -> check_file(),?LOOP(LD);
        
        %% user is trying to aquire data
	{sherk,{signal,{aq_filechoose,_}}}   -> aq_check(),?LOOP(LD);
        {sherk,{signal,{aq_treeview,_}}}     -> aq_check(),?LOOP(LD);
        {sherk,{signal,{aq_time_entry,_}}}   -> aq_check(),?LOOP(LD);
	{sherk,{signal,{aq_go_button,_}}}    -> ?LOOP(aq_go(LD));
	{sherk,{signal,{aq_stop_button,_}}}  -> ?LOOP(aq_stop(LD));

	%% let's go
	{sherk,{signal,{go_button,_}}}       -> ?LOOP(procs(LD));

	%% show/change view of call data
	{sherk,{signal,{procs_treeview,'row-activated'}}}->?LOOP(call_procs(LD));
        {sherk,{signal,{call_heavy_radio,toggled}}}      ->?LOOP(call_combo(LD));
        {sherk,{signal,{call_combobox,changed}}}         ->?LOOP(call_combo(LD));

        %% configuring
        {sherk,{signal,{configure,_}}}       -> show(conf_window),?LOOP(LD);
        {sherk,{signal,{conf_window,_}}}     -> hide(conf_window),?LOOP(LD);
        {sherk,{signal,{conf_cancel,_}}}     -> hide(conf_window),?LOOP(LD);
        {sherk,{signal,{conf_ok,_}}}         -> ?LOOP(conf(LD));
        {sherk,{signal,{bad_proxy_ok,_}}}    -> bad_proxy_ok(),?LOOP(LD);
        {sherk,{signal,{bad_proxy_cancel,_}}}-> bad_proxy_cancel(),?LOOP(LD);

        %% a target node went away
        {nodedown, Node}                     -> ?LOOP(downed_target(Node,LD));

        %% aquire proc exited
        {'DOWN',AqMon,_,_,Info}              -> ?LOOP(do_aq_stop(LD,Info));

        %% we got target data from proxy
        {timeout, _, re_query}               -> ?LOOP(re_query_targs(LD));
        {'DOWN',TargMon,_,_,Info}            -> ?LOOP(chk_targs(LD,Info));
        
	%% user doing some wierd stuff
	X                                    -> ?LOG([{received,X}]),?LOOP(LD)
    end.

show_ld(LD) ->
    ?LOG(to_list(LD)),
    LD.

check_file() ->
    try 
	Dir = g('Gtk_file_chooser_get_filename',[main_filechoose]),
	sherk_tab:check_file(Dir),
	g('Gtk_widget_set_sensitive',[go_button,true])
    catch 
	_:_ -> g('Gtk_widget_set_sensitive',[go_button,false])
    end.


conf(LD) ->
    try 
        NLD = proxy(LD),
        hide(conf_window),
        NLD
    catch
        _:_ -> 
            g('Gtk_widget_set_sensitive',[conf_window,false]),
            show(bad_proxy_window),
            LD
    end.

proxy(LD) ->
    case list_to_atom(g('Gtk_entry_get_text',[conf_proxy_entry])) of
        '' -> 
            Proxy = node(),
            Cookie = erlang:get_cookie(),
            Tick = fetch(orig_ticktime,LD);
        Proxy -> 
            Cookie = list_to_atom(g('Gtk_entry_get_text',[conf_cookie_entry])),
            erlang:set_cookie(Proxy,Cookie),
            Tick = rpc:call(Proxy,net_kernel,get_net_ticktime,[])
    end,
    net_kernel:set_net_ticktime(Tick),
    store(cookie,Cookie,
          store(proxy,Proxy,
                store(targs,[],
                      store(bad_targs,[],LD)))).

bad_proxy_cancel() ->
    g('Gtk_widget_set_sensitive',[conf_window,true]),
    hide(bad_proxy_window).

bad_proxy_ok() ->
    g('Gtk_widget_set_sensitive',[conf_window,true]),
    hide(conf_window),
    hide(bad_proxy_window).

aq_go(LD) -> 
    g('Gtk_widget_set_sensitive',[aq_radiobutton_call,false]),
    g('Gtk_widget_set_sensitive',[aq_radiobutton_proc,false]),
    g('Gtk_widget_set_sensitive',[aq_filechoose,false]),
    g('Gtk_widget_set_sensitive',[aq_time_entry,false]),
    g('Gtk_widget_set_sensitive',[aq_treeview,false]),
    g('Gtk_widget_set_sensitive',[aq_go_button,false]),
    g('Gtk_widget_set_sensitive',[aq_stop_button,true]),
    Time = aq_get_time(),
    Flags = aq_get_flags(),
    RTPs = aq_get_rtps(Flags),
    Procs = all,
    Targs = aq_get_nodes(),
    Dest = {file,aq_get_dest(),0,"/tmp"},
    ?LOG([{time,Time},
          {flags,Flags},
          {rTPs,RTPs},
          {procs,Procs},
          {targs,Targs},
          {dest,Dest}]),
    P = sherk_aquire:go(Time,Flags,RTPs,Procs,Targs,Dest),
    store(aq_mon,erlang:monitor(process,P),LD).

aq_stop(LD) -> 
    sherk_aquire:stop(),
    do_aq_stop(LD,aq_stop_wait(fetch(aq_mon,LD))).

aq_stop_wait(Monitor) ->
    receive
        {'DOWN',Monitor,_Type,_Object,Info} -> Info
    end.

do_aq_stop(LD,Reason) -> 
    ?LOG([{aquire_finshed,Reason}]),
    g('Gtk_widget_set_sensitive',[aq_radiobutton_call,true]),
    g('Gtk_widget_set_sensitive',[aq_radiobutton_proc,true]),
    g('Gtk_widget_set_sensitive',[aq_filechoose,true]),
    g('Gtk_widget_set_sensitive',[aq_time_entry,true]),
    g('Gtk_widget_set_sensitive',[aq_treeview,true]),
    g('Gtk_widget_set_sensitive',[aq_stop_button,false]),    
    aq_check(),
    store(aq_mon,undefined,LD).

aq_check() ->
    try
        Dir = g('Gtk_file_chooser_get_filename',[aq_filechoose]),
        sherk_aquire:check_dir(Dir),
        [_|_] = get_selected_data(aq_treeview,0),
        list_to_integer(g('Gtk_entry_get_text',[aq_time_entry])),
    	g('Gtk_widget_set_sensitive',[aq_go_button,true])
    catch 
	_:_ -> g('Gtk_widget_set_sensitive',[aq_go_button,false])
    end.

aq_get_time() ->
    1000*list_to_integer(g('Gtk_entry_get_text',[aq_time_entry])).

aq_get_flags() ->
    case g('Gtk_toggle_button_get_active',[aq_radiobutton_proc]) of
        true -> proc_flags();
        false -> call_flags()
    end.

aq_get_rtps(Flags) ->
    case member(call,Flags) of
        true -> [{'_','_'}];
        false -> []
    end.

aq_get_nodes() ->
    [list_to_atom(N) || N<-get_selected_data(aq_treeview,0)].

aq_get_dest() ->
    g('Gtk_file_chooser_get_filename',[aq_filechoose]).

show_aq_window() ->
    show(aq_window),
    g('Gtk_check_menu_item_set_active',[show_aquire,true]).
hide_aq_window() ->
    hide(aq_window),
    g('Gtk_check_menu_item_set_active',[show_aquire,false]).
toggle_aq_window() ->
    case g('Gtk_check_menu_item_get_active',[show_aquire]) of
	true -> show_aq_window();
	false -> hide_aq_window()
    end.

re_query_targs(LD) ->
    store(targ_mon,query_targs(fetch(proxy,LD)),LD).

query_targs(Proxy) ->
    sherk_aquire:ass_loaded(Proxy,sherk_target),
    erlang:monitor(process,spawn(Proxy,sherk_target,get_nodes,[])).

chk_targs(LD,Atom) when is_atom(Atom) -> ?LOG({no_proxy,Atom}),LD;
chk_targs(LD,{Pid,Nodes,EpmdStr}) when is_pid(Pid) ->
    try
        erlang:start_timer(5000,self(),re_query),
        OldTargs = fetch(targs,LD),
        BadTargs = fetch(bad_targs,LD),
        [_,Host] = string:tokens(to_str(node(Pid)),"@"),
        Nods = string:tokens(EpmdStr,"\n"),
        CPs = [N || ["name",N|_] <- [string:tokens(Str," ") || Str <- Nods]],
        EpmdTargs = [list_to_atom(CP++"@"++Host) || CP <-CPs],
        Targs = usort(Nodes++EpmdTargs)--[node()],
        foldl(fun new_target/2, LD, (Targs--OldTargs)--BadTargs)
    catch 
        _:R -> ?LOG([{r,R},{pid,Pid},{nodes,Nodes},{epmd,EpmdStr}]),LD
    end.

downed_target(Node,LD) ->
    Ts = fetch(targs,LD),
    BTs = fetch(bad_targs,LD),
    case {member(Node,Ts),member(Node,BTs)} of
        {false,true} -> LD;
        {true,false} -> update_targs(Ts--[Node], append(bad_targs,Node,LD));
        {false,false}-> wierd(down,Node,Ts,BTs),append(bad_targs,Node,LD);
        {true,true}  -> wierd(down,Node,Ts,BTs),update_targs(Ts--[Node], LD)
    end.

new_target(Node,LD) ->
    Ts = fetch(targs,LD),
    BTs = fetch(bad_targs,LD),
    case {member(Node,Ts),member(Node,BTs)} of
        {false,false} -> new_target(Node,Ts,LD);
        {false,true} -> new_target(Node,Ts,store(bad_targs,BTs--[Node],LD));
        {true,false} -> wierd(up,Node,Ts,BTs),LD;
        {true,true} -> wierd(up,Node,Ts,BTs),store(bad_targs,BTs--[Node],LD)
    end.

new_target(Node,Ts,LD) ->
    catch erlang:set_cookie(Node,fetch(cookie,LD)),
    erlang:monitor_node(Node,true),
    update_targs([Node|Ts], LD).

wierd(UpDown,Node,Ts,BTs) ->
    ?LOG([{node_came,UpDown},{node,Node},{targs,Ts},{bad_targs,BTs}]).

update_targs(Ts,LD) ->
    update_treeview_list(aq_treeview,[[to_str(T)]||T<-Ts]),
    store(targs,Ts,LD).

proc_flags() ->
    ['procs','running','garbage_collection',
     'timestamp','cpu_timestamp','set_on_spawn'].

call_flags() ->
    ['call','return_to','arity'|proc_flags()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
procs(LD) ->
    g('Gtk_widget_set_sensitive',[go_button,false]),
    File = g('Gtk_file_chooser_get_filename',[main_filechoose]),
    sherk_tab:assert(File),
    Tree = sherk_tree:go(procs),
    update_treeview_tree(procs_treeview,Tree),
    g('Gtk_widget_set_sensitive',[go_button,true]),
    LD.

call_procs(LD) ->
    Model = g('Gtk_tree_view_get_model',[procs_treeview]),
    [Path] = g('GN_tree_view_get_selected',[procs_treeview]),
    set_active_combo(Model, Path),
    update_call(LD,Model,Path).

call_combo(LD) ->
    Model = g('Gtk_combo_box_get_model',[call_combobox]),
    g('Gtk_combo_box_get_active_iter',[call_combobox,iter]),
    Path = g('Gtk_tree_model_get_string_from_iter',[Model,iter]),
    set_active_treeview(Model, Path),
    update_call(LD,Model,Path).

update_call(LD,Model,Path) -> 
    show(call_window),
    [PidStr] = get_data(Model,0,[Path]),
    case g('Gtk_toggle_button_get_active',[call_heavy_radio]) of
        true -> 
            init_tree_view(call_treeview,fetch(call_list,LD)),
            List = sherk_list:go({call,PidStr}),
            update_treeview_list(call_treeview,List);
        false-> 
            init_tree_view(call_treeview,fetch(call_tree,LD)),
            Tree = sherk_tree:go({callgraph,PidStr}),
            update_treeview_tree(call_treeview,Tree)
    end,
    LD.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_tree_view(TreeView,{Model,Cols}) ->
    g('Gtk_tree_view_collapse_all',[call_treeview]),
    remove_cols(TreeView),
    [g('Gtk_tree_view_append_column',[TreeView,Col]) || Col <- Cols],
    g('Gtk_tree_view_set_model',[TreeView,Model]).

remove_cols(TreeVw) ->
    case g('Gtk_tree_view_get_column',[TreeVw,0]) of
        'NULL' -> ok;
        Col -> 
            g('Gtk_tree_view_remove_column',[TreeVw,Col]),
            remove_cols(TreeVw)
    end.

init_tree_view_cols(DataColTitles) ->
    [init_tree_view_col(DC,T) || {DC,T} <- DataColTitles].

init_tree_view_col(DataCol,Title) ->
    Col = g('Gtk_tree_view_column_new',[]),
    Renderer = g('Gtk_cell_renderer_text_new',[]),
    g('Gtk_tree_view_column_pack_start',[Col,Renderer,false]),
    g('Gtk_tree_view_column_set_title', [Col,Title]),
    g('Gtk_tree_view_column_set_resizable',[Col,true]),
    g('Gtk_tree_view_column_add_attribute',[Col,Renderer,"text",DataCol]),
    Col.

set_active_treeview(Model, Path) ->
    g('Gtk_tree_model_get_iter_from_string',[Model,iter,Path]),
    Sel = g('Gtk_tree_view_get_selection',[procs_treeview]),
    g('Gtk_tree_selection_unselect_all',[Sel]),
    g('Gtk_tree_selection_select_iter',[Sel,iter]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_list_store(Cols)->
    g('Gtk_list_store_newv',[length(Cols),Cols]).

update_treeview_list(View,List) ->
    Model = g('Gtk_tree_view_get_model',[View]),
    hide(View),
    g('Gtk_list_store_clear', [Model]),
    g('Gtk_widget_freeze_child_notify',[View]),
    list_insert(Model, List),
    g('Gtk_widget_thaw_child_notify',[View]),
    show(View).

list_insert(Store,Rows) ->
    Row_f = fun() -> {'Gtk_list_store_append',[Store,iter]} end,
    Col_f = fun(Row) -> foldl(fun col_f/2, {0,Store,[]}, Row) end,
    g(flatten([[Row_f()|element(3,Col_f(Row))] || Row<-Rows])).

col_f(Val,{N,Store,O}) ->
    {N+1,Store,
     [{'GN_value_set',[val,Val]},
      {'Gtk_list_store_set_value',[Store,iter,N,val]}|O]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_combobox(Model) ->
    g('Gtk_combo_box_set_model',[call_combobox,Model]),
    Rend = g('Gtk_cell_renderer_text_new',[]),
    g('Gtk_cell_layout_pack_start',[call_combobox,Rend,false]),
    g('Gtk_cell_layout_add_attribute',[call_combobox,Rend,"text",0]).

set_active_combo(Model, Path) ->
    g('Gtk_tree_model_get_iter_from_string',[Model,iter,Path]),
    g('Gtk_combo_box_set_active_iter',[call_combobox,iter]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_tree_store(Cols)->
    g('Gtk_tree_store_newv',[length(Cols),Cols]).

update_treeview_tree(View,Tree) ->
    Model = g('Gtk_tree_view_get_model',[View]),
    hide(View),
    g('Gtk_tree_store_clear',[Model]),
    g('Gtk_widget_freeze_child_notify',[View]),
    tree_insert(Model, Tree),
    g('Gtk_widget_thaw_child_notify',[View]),
    show(View).

tree_insert(Store,Tree) -> 
    g(flatten(tree_insert(Store,[0],Tree))).

tree_insert(_Store,_Path,[]) -> [];
tree_insert(Store,Path,[{_,Row,SubTree}|T]) -> 
    [update_iter(Path, Store),
     tree_insert_row(Store,0,Row),
     tree_insert(Store,[0|Path],SubTree),
     tree_insert(Store,[hd(Path)+1|tl(Path)],T)].

tree_insert_row(_Store,_N,[]) -> [];
tree_insert_row(Store,N,[Col|Cols]) ->
    [tree_insert_val(Store,N,Col),
     tree_insert_row(Store,N+1,Cols)].

tree_insert_val(Store,N,Val) ->
    [{'GN_value_set',[val,Val]},
     {'Gtk_tree_store_set_value', [Store,iter,N,val]}].

update_iter([I], Store) ->
    {'Gtk_tree_store_insert',[Store,iter,'NULL',I]};
update_iter([I|D], Store) ->
    [{'Gtk_tree_model_get_iter_from_string',[Store,daddy,pth(D)]},
     {'Gtk_tree_store_insert',[Store,iter,daddy,I]}].

pth(P) -> flatten(pth(P,[])).
pth([I],O) -> [to_str(I)|O];
pth([H|T],O) -> pth(T,[$:,to_str(H)|O]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_selection_mode(View,Mode) ->
    Sel = g('Gtk_tree_view_get_selection',[View]),
    g('Gtk_tree_selection_set_mode',[Sel,Mode]).

get_selected_data(View,Col) ->
    Model = g('Gtk_tree_view_get_model',[View]),
    get_data(Model,Col,g('GN_tree_view_get_selected',[View])).

get_data(_Model,_Col,[]) -> [];
get_data(Model,Col,[Path|Paths]) ->
    g('Gtk_tree_model_get_iter_from_string',[Model,daddy,Path]),
    g('GN_value_unset',[val]),
    g('Gtk_tree_model_get_value',[Model,daddy,Col,val]),
    [g('GN_value_get',[val])|get_data(Model,Col,Paths)].

show(W) -> g('Gtk_widget_show',[W]).
hide(W) -> g('Gtk_widget_hide',[W]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

log(ProcInfo,Term) when not is_list(Term) -> log(ProcInfo,[Term]);
log(ProcInfo,List) ->
    error_logger:info_report([{in,CF}||{current_function,CF}<-ProcInfo]++List).

to_str(X) when is_pid(X) -> pid_to_list(X);
to_str(X) when is_atom(X) -> atom_to_list(X);
to_str(X) when is_integer(X) -> integer_to_list(X);
to_str(X) when is_float(X) ->  float_to_list(X);
to_str(X) when is_list(X) ->
    case is_string(X) of
        true -> X;
        false -> flatten(io_lib:fwrite("~p",[X]))
    end;
to_str(X) -> flatten(io_lib:fwrite("~p",[X])).

is_string(X) when not is_list(X) -> false;
is_string([]) -> true;
is_string([H|T]) when integer(H), H >= $ , H =< $~ -> is_string(T);
is_string(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
g(C,As) -> g([{C,As}]).

g(CAs) -> gtknode:cmd(sherk,CAs).
