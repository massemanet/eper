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
    init_tree_view(aq_treeview,NodeList,[{0,"Nodes"}]),
    set_selection_mode(aq_treeview,'GTK_SELECTION_MULTIPLE'),

    %% init calls treeview
    CallList = init_list_store([string,integer,integer]),
    init_tree_view(call_treeview,CallList,[{0,"MFA"},{1,"calls"},{2,"%"}]),
    %% init calls combobox
    init_combobox(),

    %% init procs treeview
    PerfList = init_list_store([string,string,integer,integer]),
    init_tree_view(perf_treeview,PerfList,[{1,"tag"},{2,"us"},{3,"%"}]),

    %% hide the radio buttons
    hide(call_heavy_radio),
    hide(call_tree_radio),
    
    loop(from_list([{targs,[]},
                    {bad_targs,[]},
                    {aq_mon,undefined},
                    {proxy,node()},
                    {orig_ticktime,net_kernel:get_net_ticktime()},
                    {targ_mon,query_targs(node())}])).

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
	{sherk,{signal,{go_button,_}}}       -> ?LOOP(perf(LD));

	%% inspect process
	{sherk,{signal,{perf_treeview,_}}}   -> ?LOOP(call(perf,LD));
        {sherk,{signal,{call_combobox,_}}}   -> ?LOOP(call(combo,LD));

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
        "" -> 
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
        [_,Host] = string:tokens(atom_to_list(node(Pid)),"@"),
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
    update_treeview(aq_treeview,[[atom_to_list(T)]||T<-Ts]),
    store(targs,Ts,LD).

proc_flags() ->
    ['procs','running','garbage_collection',
     'timestamp','cpu_timestamp','set_on_spawn'].

call_flags() ->
    ['call','return_to','arity'|proc_flags()].

perf(LD) ->
    g('Gtk_widget_set_sensitive',[go_button,false]),
    File = g('Gtk_file_chooser_get_filename',[main_filechoose]),
    sherk_tab:assert(File),
    List = sherk_list:go(perf),
    update_treeview(perf_treeview,List),
    Indexs = update_combo(call_combobox,List),
    g('Gtk_widget_set_sensitive',[go_button,true]),
    store(combo,Indexs,LD).

update_combo(Combobox,List) ->
    Model = g('Gtk_combo_box_get_model',[Combobox]),
    list_clear(Model),
    list_insert(Model,[[H] || [_,H|_] <- List]),
    from_list(indexs([[H] || [H|_] <- List],0)).

indexs([],_) -> [];
indexs([[P]|R],N) -> flatten([[{N,P},{P,N}]|indexs(R,N+1)]).

call(perf,LD) -> 
    [Pid] = get_selected_data(perf_treeview,0),
    g('Gtk_combo_box_set_active',[call_combobox,fetch(Pid,fetch(combo,LD))]),
    call(Pid),
    LD;
call(combo,LD) ->
    N = g('Gtk_combo_box_get_active',[call_combobox]),
    Pid = fetch(N,fetch(combo,LD)),
    call(Pid),
    LD.
call(Pid) ->
    show(call_window),
    List = sherk_list:go({call,Pid}),
    update_treeview(call_treeview,List).

update_treeview(View,List) ->
    ?LOG({rows,length(List)}),
    Model = g('Gtk_tree_view_get_model',[View]),
    hide(View),
    list_clear(Model),
    g('Gtk_widget_freeze_child_notify',[View]),
    list_insert(Model, List),
    g('Gtk_widget_thaw_child_notify',[View]),
    show(View).

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

show(W) -> g('Gtk_widget_show',[W]).
hide(W) -> g('Gtk_widget_hide',[W]).

list_clear(List) -> g('Gtk_list_store_clear', [List]).

init_list_store(Cols)->
    g('Gtk_list_store_newv',[length(Cols),Cols]).

list_insert(Store,Rows) ->
    Row_f = fun() -> {'Gtk_list_store_append',[Store,iter]} end,
    Col_f = fun(Row) -> foldl(fun col_f/2, {0,Store,[]}, Row) end,
    g(flatten([[Row_f()|element(3,Col_f(Row))] || Row<-Rows])).

col_f(Val,{N,Store,O}) ->
    {N+1,Store,
     [{'GN_value_set',[val,Val]},
      {'Gtk_list_store_set_value',[Store,iter,N,val]}|O]}.

init_combobox() ->
    CallComboList = init_list_store([string]),
    g('Gtk_combo_box_set_model',[call_combobox,CallComboList]),
    Rend = g('Gtk_cell_renderer_text_new',[]),
    g('Gtk_cell_layout_pack_start',[call_combobox,Rend,false]),
    g('Gtk_cell_layout_add_attribute',[call_combobox,Rend,"text",0]).

init_tree_view(TreeView,Model,Cols) ->
    g('Gtk_tree_view_set_model',[TreeView,Model]),
    foreach(fun({DC,T})->init_tree_view_column(TreeView,DC,T) end,Cols).

init_tree_view_column(TreeView,DataCol,Title) ->
    Col = g('Gtk_tree_view_column_new',[]),
    Renderer = g('Gtk_cell_renderer_text_new',[]),
    g('Gtk_tree_view_column_pack_start',[Col,Renderer,false]),
    g('Gtk_tree_view_column_set_title', [Col,Title]),
    g('Gtk_tree_view_column_set_resizable',[Col,true]),
    g('Gtk_tree_view_column_add_attribute',[Col,Renderer,"text",DataCol]),
    g('Gtk_tree_view_append_column',[TreeView,Col]).

tree_insert(_Store,_Path,[]) -> ok;
tree_insert(Store,Path,[{Str,SubList}|T]) -> 
    tree_insert_str(Store,Path,Str),
    tree_insert(Store,[0|Path],SubList),
    tree_insert(Store,[hd(Path)+1|tl(Path)],T);
tree_insert(Store,Path,[Str|T]) ->
    tree_insert_str(Store,Path,Str),
    tree_insert(Store,[hd(Path)+1|tl(Path)],T).

tree_insert_str(Store,Path,Str) ->
    g('GN_value_set',[val,Str]),
    case Path of
	[I] -> 
	    g('Gtk_tree_store_insert',[Store,iter,'NULL',I]);
	[I|D] -> 
	    g('Gtk_tree_model_get_iter_from_string',[Store,daddy,pth(D)]),
	    g('Gtk_tree_store_insert',[Store,iter,daddy,I])
    end,
    g('Gtk_tree_store_set_value', [Store,iter,0,val]).

pth(P) -> pth(P,[]).
pth([I],O) -> [$0+I|O];
pth([H|T],O) -> pth(T,[$:,$0+H|O]).

g(C,As) -> g([{C,As}]).
g(CAs) ->
    case gtknode:cmd(sherk,CAs) of
        [{ok,Rep}] -> Rep;
	Reps -> 
            case [R || {error,R} <- Reps] of
                [] -> ok;
                Es -> throw({errors,Es})
            end
    end.

log(ProcInfo,Term) when not is_list(Term) -> log(ProcInfo,[Term]);
log(ProcInfo,List) ->
    error_logger:info_report([{in,CF}||{current_function,CF}<-ProcInfo]++List).
