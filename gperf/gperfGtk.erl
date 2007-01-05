%%%-------------------------------------------------------------------
%%% File    : gperfFoo.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 12 Sep 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(gperfGtk).

-export([start/0,stop/0,init/0]).
-export([loop/1]).				%internal export
-import(filename,[join/1,dirname/1]).

-import(random,[uniform/1]).
-import(lists,[foreach/2,flatten/1,keysearch/3,nth/2,reverse/1,seq/2,sort/1]).

-define(LP(X), ?MODULE:loop(X)).
-define(MARG,10).
-define(XSIZE,2000).
-define(YSIZE,100).
-define(WXSIZE,200).
-define(WYSIZE,?YSIZE).
-define(XHALF,(?XSIZE div 2)).
-define(DAREAS, [{drawingarea1,[blue,red,green,magenta]},
		 {drawingarea2,[black,blue,red,yellow]},
		 {drawingarea3,[red,green]}]).

-define(LOG(T), gperf:log(process_info(self()),T)).

-record(dArea, {win,px_fg,px_bg,gc_fg,gc_bg,gcs,layout}).
-record(ld, {node='', tick, cookie, 
             minute,x=?XHALF,dAreas=[],state=disc,stat_ctxt}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> spawn(fun()-> init() end).
stop() -> catch (gperf ! quit),quit.

init() ->
    register(gperf,self()),
    gtknode:start(gperf_gtk),
    g('GN_glade_init',[glade_file()]),
    maybe_show(),
    try loop(do_init(#ld{}))
    catch
        _:dying -> ok;
          _:R -> ?LOG([{error,R},{stack,erlang:get_stacktrace()}])
    end.

glade_file() -> join([code:priv_dir(eper),glade,"gperf.glade"]).

loop(LD) ->
    receive
	quit                                        -> die(LD);
	{gperf_gtk,{signal,{window,'delete-event'}}}-> die(LD);
        
        {gperf_gtk,{signal,{conf_cancel,_}}} -> hide_cf(),?LP(LD);
        {gperf_gtk,{signal,{conf_ok,_}}}     -> hide_cf(),?LP(beg(conf(LD)));
        
        %% menu items
	{gperf_gtk,{signal,{quit,'activate'}}}    -> die(LD);
        {gperf_gtk,{signal,{conf,'activate'}}}    -> show(conf_window),?LP(LD);
	{gperf_gtk,{signal,{about,'activate'}}}   -> show(about),?LP(LD);
	{gperf_gtk,{signal,{load,'activate'}}}    -> maybe_show(),?LP(LD);
	{gperf_gtk,{signal,{memory,'activate'}}}  -> maybe_show(),?LP(LD);
	{gperf_gtk,{signal,{netload,'activate'}}} -> maybe_show(),?LP(LD);

	{gperf_gtk,{signal,{Darea,'expose-event'}}}->?LP(do_expose(LD,Darea));

        {args,['']}                               -> ?LP(LD);
        {args,[Node]}                             -> ?LP(beg(LD#ld{node=Node}));
	{tick, Stuff}                             -> ?LP(do_tick(LD,Stuff));
	dbg                                       -> ?LP(dump_ld(LD));
	X                                         -> ?LP(do_unknown(LD,X))
    end.

dump_ld(LD) ->
    F = fun({N,V})->io:fwrite("~p~n",[{N,V}]) end,
    foreach(F,zip(record_info(fields,ld),tl(tuple_to_list(LD)))),
    LD.

beg(LD) ->
    prf:stop(gperf_prf),
    prf:start(gperf_prf,LD#ld.node,gperfConsumer),
    statbar("waiting - "++atom_to_list(LD#ld.node), LD),
    LD.

conf(LD) ->
    case g('Gtk_entry_get_text',[conf_node_entry]) of
        "" -> LD#ld{node = node()};
        NodeS -> 
            Node = list_to_atom(NodeS),
            case g('Gtk_entry_get_text',[conf_cookie_entry]) of
                "" -> ok;
                CS -> erlang:set_cookie(Node,list_to_atom(CS))
            end,
            LD#ld{node =Node}
    end.

zip([],[]) -> [];
zip([A|As],[B|Bs]) -> [{A,B}|zip(As,Bs)].

die(_LD) ->
    io:fwrite("~w - terminating~n", [?MODULE]),
    exit(dying).

do_unknown(LD,X) -> 
    io:fwrite("~w - unknown signal - ~p~n",[?MODULE,X]),
    LD.

do_expose(LD,Darea) ->
    redraw(Darea,LD).

do_tick(LD, {Time, Stuff}) ->
    redraw(stuff(timeline(switch(LD),Time), Stuff)).

maybe_show() ->
    L = [{load,drawingarea1},{memory,drawingarea2},{netload,drawingarea3}],
    lists:foreach(fun maybe_show/1, L).

maybe_show({Item, Darea}) ->
    case g('Gtk_check_menu_item_get_active',[Item]) of
	true -> show(Darea);
	false -> hide(Darea),resize_toplevel(Darea)
    end.

show(Darea) -> g('Gtk_widget_show',[Darea]).

hide(Darea) -> g('Gtk_widget_hide',[Darea]).

hide_cf() -> hide(conf_window).

resize_toplevel(Widget) ->
    TopLevel = g('Gtk_widget_get_toplevel',[Widget]),
    g('Gtk_window_resize',[TopLevel,1,1]).

%%%
redraw(LD) ->
    foreach(fun({DA,_Colors}) -> redraw(DA,LD) end, ?DAREAS),
    LD.

redraw(Darea,LD = #ld{x=X,dAreas=Dareas}) ->
    #dArea{win=Win,px_fg=Pixmap,gc_fg=GC} = lks(Darea,Dareas),
    Width = g('GN_widget_get_attr',[Darea,width]),
    g('Gdk_draw_drawable',[Win,GC,Pixmap,0,0,Width-?MARG-X,0,X+1,-1]),
    LD.

stuff(LD = #ld{dAreas=Dareas,x=X},Datas) ->
    stuffit(X,Datas,Dareas),
    LD#ld{x=X+1}.

stuffit(_X,[],[]) -> ok;
stuffit(X,[Data|Datas],[{_DAname,Darea}|Dareas]) ->
    VIs = reverse(sort(zip(Data,seq(1,length(Data))))),
    foreach(fun(VI) -> stuffer(X,Darea,VI) end, VIs),
    stuffit(X,Datas,Dareas).

stuffer(X,Darea,{Val,I}) ->
    draw_line(Darea,I,X,Val).

timeline(LD = #ld{state=disc}, no_time) -> LD;
timeline(LD = #ld{state=disc}, {_H,M,_S}) -> stat_change(up,LD#ld{minute=M});
timeline(LD = #ld{state=conn}, no_time) -> stat_change(down,LD);
%%timeline(LD = #ld{minute=undefined},{_H,M,_S}) -> LD#ld{minute=M};
timeline(LD = #ld{minute=M},{_H,M,_S}) -> LD;
timeline(LD = #ld{dAreas=Dareas},{_,M,_}=HMS) ->
    foreach(fun(Darea)->draw_timeline(Darea,LD,HMS) end,Dareas),
    LD#ld{minute=M}.

stat_change(up,LD) ->     
    statbar("connected - "++atom_to_list(LD#ld.node),LD),
    LD#ld{state=conn};
stat_change(down,LD) ->
    statbar("disconnected - "++atom_to_list(LD#ld.node),LD),
    LD#ld{state=disc}.

statbar(Msg, #ld{stat_ctxt=Ctxt}) ->
    g('Gtk_statusbar_pop',[statusbar,Ctxt]),
    g('Gtk_statusbar_push',[statusbar,Ctxt,Msg]).
    
draw_timeline({_,Darea},LD,{H,M,_S}) ->
    draw_line(Darea,fg,LD#ld.x,1),
    draw_time(Darea,LD,{H,M}).

draw_time(#dArea{px_fg=Pxfg,px_bg=Pxbg,layout=Layout,gc_fg=GC},LD,{H,M}) ->
    Tm = flatten(io_lib:fwrite("~2.2.0w:~2.2.0w",[H,M])),
    g('GN_pango_layout_set_text',[Layout,Tm,"geneva 6"]),
    g('Gdk_draw_layout',[Pxfg,GC,LD#ld.x,90,Layout]),
    g('Gdk_draw_layout',[Pxbg,GC,LD#ld.x-?XHALF,90,Layout]).

draw_line(DA,GCtag,X,Y) ->
    %% draws a line from {X,0} to {X,Y} where 0<Y<1. 
    %% Y=0: upper margin, Y=1: lower margin
    Y0 = ?YSIZE-?MARG,
    Y1 = Y0-round(Y*(?YSIZE-2*?MARG)),
    GC = get_gc(DA,GCtag),
    g('Gdk_draw_line',[DA#dArea.px_fg,GC,X,Y0,X,Y1]),
    g('Gdk_draw_line',[DA#dArea.px_bg,GC,X-?XHALF,Y0,X-?XHALF,Y1]).

get_gc(DA,N) when is_integer(N) -> nth(N,DA#dArea.gcs);
get_gc(DA,fg) -> DA#dArea.gc_fg;
get_gc(DA,bg) -> DA#dArea.gc_bg.

switch(LD) ->
    case LD#ld.x > ?XSIZE of
	false-> LD;
	true -> LD#ld{x=?XHALF,dAreas=switch_das(LD#ld.dAreas)}
    end.

switch_das([]) -> [];
switch_das([{D,DA = #dArea{px_fg=Px1,px_bg=Px2,gc_fg=GCfg,gc_bg=GCbg}}|DAs]) ->
    clear_px(GCfg,GCbg,Px1),
    [{D,DA#dArea{px_fg=Px2,px_bg=Px1}}|switch_das(DAs)].

do_init(LD) ->
    redraw(clear_all(LD#ld{dAreas=init_das(),stat_ctxt=init_stat()})).

init_stat() ->
    Msg = "not started",
    Id = g('Gtk_statusbar_get_context_id',[statusbar,"state"]),
    g('Gtk_statusbar_push',[statusbar,Id,Msg]),
    Id.

init_das() ->
    [init_da(DA_Colors) || DA_Colors <- ?DAREAS].

init_da({DA,Colors})->
    Win = g('GN_widget_get_attr',[DA,window]),
    Layout = g('Gtk_widget_create_pango_layout',[DA,""]),
    g('Gdk_window_clear',[Win]),
    [GCblack,GCwhite] = gcs(Win,[black,white]),
    [P1,P2] = pixmaps(2,Win),
    g( 'Gtk_widget_set_size_request',[DA,?WXSIZE,?WYSIZE]),
    g('Gtk_widget_modify_bg', [DA,'GTK_STATE_NORMAL',white]),
    {DA,#dArea{win=Win,layout=Layout,
	       gc_fg=GCblack,gc_bg=GCwhite,
	       px_fg=P1,px_bg=P2,
	       gcs=gcs(Win,Colors)}}.

pixmaps(0, _Win) -> [];
pixmaps(N,Win) ->
    Pixmap = g('Gdk_pixmap_new',[Win,?XSIZE,?YSIZE,-1]),
    [Pixmap|pixmaps(N-1,Win)].

clear_all(LD) ->
    foreach(fun(DA)->clear_one(DA) end, LD#ld.dAreas),
    LD.

clear_one({_,#dArea{px_fg=Px1,px_bg=Px2,gc_fg=GCfg,gc_bg=GCbg}}) ->
    clear_px(GCfg,GCbg,Px1),
    clear_px(GCfg,GCbg,Px2).

clear_px(GCfg,GCbg,Pixmap) ->
    g('Gdk_draw_rectangle', [Pixmap,GCbg, true, 0, 0, -1, -1]),
    g('Gdk_draw_rectangle', 
      [Pixmap,GCfg,false,?MARG,?MARG,?XSIZE-2*?MARG,?YSIZE-2*?MARG]).

gcs(_Win,[]) -> [];
gcs(Win,[Color|T]) ->
    GC = g('Gdk_gc_new',[Win]),
    ColorMap = g('Gdk_gc_get_colormap',[GC]),
    g('Gdk_color_parse',[atom_to_list(Color),Color]),
    g('Gdk_colormap_alloc_color',[ColorMap,Color,false,true]),
    g('Gdk_gc_set_foreground',[GC,Color]),
    [GC|gcs(Win,T)].

g(C,As) -> g([{C,As}]).

g(CAs) -> gtknode:cmd(gperf_gtk,CAs).

lks(Tag,List) ->
    {value,{Tag,Val}} = keysearch(Tag,1,List),
    Val.

%%%GUI=gperf_GUI,G = fun(Wid,Cmd,Args)->GUI! {self(),[{Cmd,[Wid|Args]}]}, receive {GUI,{reply,[{ok,Rep}]}} -> Rep;{GUI,{reply,[{error,Rep}]}} -> erlang:fault({Cmd,Args,Rep}) end end.
