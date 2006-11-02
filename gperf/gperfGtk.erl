%%%-------------------------------------------------------------------
%%% File    : gperfFoo.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description : 
%%%
%%% Created : 12 Sep 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(gperfGtk).

-export([start/2,stop/1]).
-export([loop/1]).				%internal export
-import(filename,[join/1,dirname/1]).
-import(random,[uniform/1]).
-import(lists,[foreach/2,flatten/1,keysearch/3,nth/2,reverse/1,seq/2,sort/1]).

-define(MARG,10).
-define(XSIZE,2000).
-define(YSIZE,100).
-define(WXSIZE,200).
-define(WYSIZE,?YSIZE).
-define(XHALF,(?XSIZE div 2)).
-define(DAREAS, [{drawingarea1,[blue,red,green,magenta]},
		 {drawingarea2,[black,blue,red,yellow]},
		 {drawingarea3,[red,green]}]).

-record(dArea, {win,px_fg,px_bg,gc_fg,gc_bg,gcs,layout}).
-record(ld, {name,node,minute,x=?XHALF,dAreas=[],state=disc,stat_ctxt}).

start(Name, Node) -> spawn_link(fun()-> init(Name,Node) end).
stop(Name) -> catch (Name ! quit), quit.

init(Name,Node) ->
    put(gui_name,Name),
    gtknode:start(Name),
    g([],'GN_glade_init',[glade_file()]),
    loop(do_init(#ld{name=Name,node=atom_to_list(Node)})).

glade_file() -> join([code:priv_dir(eper),glade,"gperf.glade"]).

loop(LD = #ld{name=Name}) ->
    receive
	{Name,{signal,{window,'delete-event'}}} -> die(LD);
	{Name,{signal,{But,'activate'}}} -> loop(do_button(LD,But));
	{Name,{signal,{Darea,'expose-event'}}} -> loop(do_expose(LD,Darea));
	{tick, Stuff} -> ?MODULE:loop(do_tick(LD,Stuff));
	dbg -> dump_ld(LD), ?MODULE:loop(LD);
	quit -> die(LD);
	X -> loop(do_unknown(LD,X))
    end.

dump_ld(LD) ->
    F = fun({N,V})->io:fwrite("~p~n",[{N,V}]) end,
    foreach(F,zip(record_info(fields,ld),tl(tuple_to_list(LD)))).

zip([],[]) -> [];
zip([A|As],[B|Bs]) -> [{A,B}|zip(As,Bs)].

die(LD) ->
    gtknode:stop(LD#ld.name),
    io:fwrite("~w - terminating~n", [?MODULE]),
    exit(dying).

do_unknown(LD,X) -> 
    io:fwrite("~w - unknown signal - ~p~n",[?MODULE,X]),
    LD.

do_expose(LD,Darea) ->
    redraw(Darea,LD).

do_tick(LD, {Time, Stuff}) ->
    redraw(stuff(timeline(switch(LD),Time), Stuff)).

do_button(LD,quit) -> die(LD);
do_button(LD,about) -> g(about,'Gtk_widget_show',[]),LD;
do_button(LD,load) -> maybe_show(load,drawingarea1),LD;
do_button(LD,memory) -> maybe_show(memory,drawingarea2),LD;
do_button(LD,netload) -> maybe_show(netload,drawingarea3),LD.

maybe_show(Item, Darea) ->
    case g(Item,'Gtk_check_menu_item_get_active',[]) of
	true -> g(Darea,'Gtk_widget_show',[]);
	false -> g(Darea,'Gtk_widget_hide',[]),resize_toplevel(Darea)
    end.

resize_toplevel(Widget) ->
    TopLevel = g(Widget,'Gtk_widget_get_toplevel',[]),
    g(TopLevel,'Gtk_window_resize',[1,1]).

%%%
redraw(LD) ->
    foreach(fun({DA,_Colors}) -> redraw(DA,LD) end, ?DAREAS),
    LD.

redraw(Darea,LD = #ld{x=X,dAreas=Dareas}) ->
    #dArea{win=Win,px_fg=Pixmap,gc_fg=GC} = lks(Darea,Dareas),
    Width = g(Darea,'GN_widget_get_attr',[width]),
    g(Win,'Gdk_draw_drawable',[GC,Pixmap,0,0,Width-?MARG-X,0,X+1,-1]),
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
    Msg = LD#ld.node++" - connected",
    g(statusbar,'Gtk_statusbar_push',[LD#ld.stat_ctxt,Msg]),
    LD#ld{state=conn};
stat_change(down,LD) ->
    
    g(statusbar,'Gtk_statusbar_pop',[LD#ld.stat_ctxt]),
    LD#ld{state=disc}.
    
draw_timeline({_,Darea},LD,{H,M,_S}) ->
    draw_line(Darea,fg,LD#ld.x,1),
    draw_time(Darea,LD,{H,M}).

draw_time(#dArea{px_fg=Pxfg,px_bg=Pxbg,layout=Layout,gc_fg=GC},LD,{H,M}) ->
    Tm = flatten(io_lib:fwrite("~2.2.0w:~2.2.0w",[H,M])),
    g(Layout,'GN_pango_layout_set_text',[Tm,"geneva 6"]),
    g(Pxfg,'Gdk_draw_layout',[GC,LD#ld.x,90,Layout]),
    g(Pxbg,'Gdk_draw_layout',[GC,LD#ld.x-?XHALF,90,Layout]).

draw_line(DA,GCtag,X,Y) ->
    %% draws a line from {X,0} to {X,Y} where 0<Y<1. 
    %% Y=0: upper margin, Y=1: lower margin
    Y0 = ?YSIZE-?MARG,
    Y1 = Y0-round(Y*(?YSIZE-2*?MARG)),
    GC = get_gc(DA,GCtag),
    g(DA#dArea.px_fg,'Gdk_draw_line',[GC,X,Y0,X,Y1]),
    g(DA#dArea.px_bg,'Gdk_draw_line',[GC,X-?XHALF,Y0,X-?XHALF,Y1]).

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
    redraw(clear_all(LD#ld{dAreas=init_das(),stat_ctxt=init_stat(LD)})).

init_stat(LD) ->
    Msg = LD#ld.node++" - disconnected",
    Id = g(statusbar,'Gtk_statusbar_get_context_id',["state"]),
    g(statusbar,'Gtk_statusbar_push',[Id,Msg]),
    Id.

init_das() ->
    [init_da(DA_Colors) || DA_Colors <- ?DAREAS].

init_da({DA,Colors})->
    Win = g(DA,'GN_widget_get_attr',[window]),
    Layout = g(DA,'Gtk_widget_create_pango_layout',[""]),
    g(Win,'Gdk_window_clear',[]),
    [GCblack,GCwhite] = gcs(Win,[black,white]),
    [P1,P2] = pixmaps(2,Win),
    g(DA, 'Gtk_widget_set_size_request',[?WXSIZE,?WYSIZE]),
    g(DA,'Gtk_widget_modify_bg', ['GTK_STATE_NORMAL',white]),
    {DA,#dArea{win=Win,layout=Layout,
	       gc_fg=GCblack,gc_bg=GCwhite,
	       px_fg=P1,px_bg=P2,
	       gcs=gcs(Win,Colors)}}.

pixmaps(0, _Win) -> [];
pixmaps(N,Win) ->
    Pixmap = g(Win,'Gdk_pixmap_new',[?XSIZE,?YSIZE,-1]),
    [Pixmap|pixmaps(N-1,Win)].

clear_all(LD) ->
    foreach(fun(DA)->clear_one(DA) end, LD#ld.dAreas),
    LD.

clear_one({_,#dArea{px_fg=Px1,px_bg=Px2,gc_fg=GCfg,gc_bg=GCbg}}) ->
    clear_px(GCfg,GCbg,Px1),
    clear_px(GCfg,GCbg,Px2).

clear_px(GCfg,GCbg,Pixmap) ->
    g(Pixmap,'Gdk_draw_rectangle', [GCbg, true, 0, 0, -1, -1]),
    g(Pixmap,'Gdk_draw_rectangle', 
      [GCfg,false,?MARG,?MARG,?XSIZE-2*?MARG,?YSIZE-2*?MARG]).

gcs(_Win,[]) -> [];
gcs(Win,[Color|T]) ->
    GC = g(Win,'Gdk_gc_new',[]),
    ColorMap = g(GC,'Gdk_gc_get_colormap',[]),
    g([],'Gdk_color_parse',[atom_to_list(Color),Color]),
    g(ColorMap,'Gdk_colormap_alloc_color',[Color,false,true]),
    g(GC,'Gdk_gc_set_foreground',[Color]),
    [GC|gcs(Win,T)].

g([],C,As) -> g([{C,As}]);
g(Wid,C,As) -> g([{C,[Wid|As]}]).

g(CAs) ->
    case gtknode:cmd(get(gui_name),CAs) of
        [{ok,Rep}] -> Rep;
	Reps -> 
            case [R || {error,R} <- Reps] of
                [] -> ok;
                Es -> throw({errors,Es})
            end
    end.

lks(Tag,List) ->
    {value,{Tag,Val}} = keysearch(Tag,1,List),
    Val.

%%%GUI=gperf_GUI,G = fun(Wid,Cmd,Args)->GUI! {self(),[{Cmd,[Wid|Args]}]}, receive {GUI,{reply,[{ok,Rep}]}} -> Rep;{GUI,{reply,[{error,Rep}]}} -> erlang:fault({Cmd,Args,Rep}) end end.
