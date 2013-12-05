%%%-------------------------------------------------------------------
%%% File    : gperfGtk.erl
%%% Author  : Mats Cronqvist <locmacr@mwlx084>
%%% Description :
%%%
%%% Created : 12 Sep 2005 by Mats Cronqvist <locmacr@mwlx084>
%%%-------------------------------------------------------------------
-module(gperfGtk).

-export([start/0,stop/0,init/0]).
-export([loop/1]).                              %internal export

-include("log.hrl").

-define(LP(X), ?MODULE:loop(X)).
-define(MARG,10).
-define(XSIZE,6000).
-define(YSIZE,100).
-define(WXSIZE,200).
-define(WYSIZE,?YSIZE).
-define(XHALF,(?XSIZE div 2)).
-define(DAREAS, [{drawingarea1,[blue,red,green,magenta]},
                 {drawingarea2,[black,blue,red,yellow]},
                 {drawingarea3,[red,green]}]).

-record(dArea, {win,px_fg,px_bg,gc_fg,gc_bg,gcs,layout}).
-record(conf, {widget, val, type}).
-record(ld, {conf=conf(),minute,x=?XHALF,dAreas=[],state=disc,stat_ctxt}).

-type orddict(X,Y) :: [{X,Y}].
-type tag_vals() :: orddict(atom(),any()).
-type confs() :: orddict(atom(),#conf{}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() -> spawn(fun init/0).
stop() -> catch (gperf ! quit),quit.

init() ->
  register(gperf,self()),
  gtknode:start(gperf_gtk),
  g('GN_glade_init',[glade_file("gperf.glade")]),
  maybe_show(),
  try loop(do_init(#ld{}))
  catch
    _:dying -> ok;
    _:R -> ?log([{error,R},{stack,erlang:get_stacktrace()}])
  end.

glade_file(Glade) ->
  try take_first(
        fun(F) -> true = filelib:is_regular(F) end,
        [filename:join([code:lib_dir(eper),"src",Glade])
         , filename:join([code:priv_dir(eper),"glade",Glade])])
  catch _:_ -> exit({not_found,Glade})
  end.

take_first(F,[H|T]) ->
  try F(H),H
  catch _:_ -> take_first(F,T)
  end.

loop(LD) ->
  receive
    %% 2 ways to quit
    quit                                         -> die(LD);
    {gperf_gtk,{signal,{window,'delete-event'}}} -> die(LD);

    %% the conf dialog
    {gperf_gtk,{signal,{conf_cancel,_}}} -> hide_cf(),?LP(LD);
    {gperf_gtk,{signal,{conf_ok,_}}}     -> hide_cf(),?LP(conf(LD));

    %% menu items
    {gperf_gtk,{signal,{quit,'activate'}}}    -> die(LD);
    {gperf_gtk,{signal,{conf,'activate'}}}    -> show_cf(LD),?LP(LD);
    {gperf_gtk,{signal,{about,'activate'}}}   -> show(about),?LP(LD);
    {gperf_gtk,{signal,{load,'activate'}}}    -> maybe_show(),?LP(LD);
    {gperf_gtk,{signal,{memory,'activate'}}}  -> maybe_show(),?LP(LD);
    {gperf_gtk,{signal,{netload,'activate'}}} -> maybe_show(),?LP(LD);

    %% event from X
    {gperf_gtk,{signal,{Darea,'expose-event'}}} -> ?LP(do_expose(LD,Darea));

    %% start args
    {args,['']}         -> ?LP(LD);
    {args,[Node]}       -> ?LP(conf(LD,orddict:from_list([{anode,Node}])));
    {args,[Node,Proxy]} -> ?LP(conf(LD,orddict:from_list([{aproxy,Proxy},
                                                  {anode,Node}])));

    %% ticker
    {tick, Stuff}       -> ?LP(do_tick(LD,Stuff));

    %% debugging
    dbg                 -> ?LP(dump_ld(LD));

    %% catchall
    X                   -> ?LP(do_unknown(LD,X))
  end.

dump_ld(LD) ->
  F = fun({N,V})->io:fwrite("~p~n",[{N,V}]) end,
  lists:foreach(F,zip(record_info(fields,ld),tl(tuple_to_list(LD)))),
  LD.

%% initialize the conf dialog
-spec conf() -> confs().
conf() ->
  orddict:from_list(
    [{anode, #conf{widget=conf_node,  val='',     type=atom}},
     {cookie,#conf{widget=conf_cookie,val='',     type=atom}},
     {aproxy,#conf{widget=conf_proxy, val='',     type=atom}},
     {cpu,   #conf{widget=conf_cpu,   val=300,    type=integer}},
     {mem,   #conf{widget=conf_mem,   val=8*1024, type=integer}},
     {net,   #conf{widget=conf_net,   val=32*1024,type=integer}}]).

%% sets and allpies conf values
%% TagVals and Confs are dicts
-spec conf(#ld{},tag_vals()) -> #ld{}.
conf(LD,TagVals) ->
  Confs = orddict:fold(fun conf_upd/3,LD#ld.conf,TagVals),
  conf_apply(TagVals,Confs),
  LD#ld{conf=Confs}.

-spec conf_upd(atom(),any(),confs()) -> confs().
conf_upd(Key,Val,Confs) ->
  orddict:update(Key, fun(Conf) -> Conf#conf{val=Val} end, Confs).

%% check if any conf values have changed, i.e. if the GUI and the conf
%% struct differ. if so, take the value from the GUI
%% TagVals and Confs are dicts
-spec conf(#ld{}) -> #ld{}.
conf(LD) ->
  {TagVals,Confs} = orddict:fold(fun conf_chk/3,
                                 {orddict:new(),LD#ld.conf},
                                 LD#ld.conf),
  conf_apply(TagVals,Confs),
  LD#ld{conf=Confs}.

-type tag_vals_confs() :: {tag_vals(),confs()}.
-spec conf_chk(atom(), #conf{}, tag_vals_confs()) -> tag_vals_confs().
conf_chk(Tag,Conf,{TagVals,Confs}) ->
  Val = get_gui_val(Conf#conf.widget,Conf#conf.type,Conf#conf.val),
  case Val == Conf#conf.val of
    true -> {TagVals,Confs};
    false-> {orddict:store(Tag,Val,TagVals),
             orddict:store(Tag,Conf#conf{val=Val},Confs)}
  end.

%% TagVals are the items that has changed
%% apply them. this is a side effect.
-spec conf_apply(tag_vals(),confs())->any().
conf_apply(TagVals,Confs) ->
  at_least_one_of([anode,aproxy],TagVals,fun restart/1,Confs),
  if_there([cpu,net,mem,cookie],TagVals,fun do_conf/2,Confs).

if_there(Tags,TagVals,Fun,Confs) ->
  _ = [Fun(Tag,Confs) || Tag<-Tags,orddict:is_key(Tag,TagVals)].

do_conf(cpu,Confs)   -> conf_send(cpu,conf_val(cpu,Confs));
do_conf(net,Confs)   -> conf_send(net,conf_val(net,Confs));
do_conf(mem,Confs)   -> conf_send(mem,conf_val(mem,Confs));
do_conf(cookie,Confs)-> setcookie(conf_val(anode,Confs),conf_val(cookie,Confs)).

conf_send(Key, Val) -> prf:config(gperf_prf,consumer,{Key,Val}).

conf_fill(Confs) -> orddict:fold(fun conf_fill/3, [], Confs).

conf_fill(_Key,#conf{widget=Widget, val=Val},_) ->
  g('Gtk_entry_set_text',[Widget,to_str(Val)]).

conf_val(Key,LD) when is_record(LD,ld) ->
  conf_val(Key,LD#ld.conf);
conf_val(Key,Confs) ->
  (orddict:fetch(Key,Confs))#conf.val.

get_gui_val(Widget,Type,Val) ->
  X = g('Gtk_entry_get_text',[Widget]),
  case Type of
    atom -> try list_to_atom(X) catch _:_ -> Val end;
    integer-> try list_to_integer(X) catch _:_ -> Val end
  end.

at_least_one_of(Tags,TagVals,Fun,Confs) ->
  case [Tag || Tag<-Tags,orddict:is_key(Tag,TagVals)] of
    []-> ok;
    _ -> Fun(Confs)
  end.

zip([],[]) -> [];
zip([A|As],[B|Bs]) -> [{A,B}|zip(As,Bs)].

to_str(L) when is_list(L) -> L;
to_str(A) when is_atom(A) -> atom_to_list(A);
to_str(F) when is_float(F) ->float_to_list(F);
to_str(I) when is_integer(I) ->integer_to_list(I).

setcookie(Node,Cookie) ->
  erlang:set_cookie(Node,Cookie).

restart(Confs) ->
  try restart(conf_val(anode,Confs),conf_val(aproxy,Confs)),
      [do_conf(Tag,Confs) || Tag <- [cpu,net,mem]]
  catch _:R ->
      ?log([{reason,R},Confs])
  end.

restart(Anode,Aproxy) ->
  prf:stop(gperf_prf),
  case Aproxy of
    ''-> prf:start(gperf_prf,Anode,gperfConsumer);
    _ -> prf:start(gperf_prf,Anode,gperfConsumer,Aproxy)
  end.

-spec die(_) -> no_return().
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

show_cf(LD) ->
  conf_fill(LD#ld.conf),
  show(conf_window).

resize_toplevel(Widget) ->
  TopLevel = g('Gtk_widget_get_toplevel',[Widget]),
  g('Gtk_window_resize',[TopLevel,1,1]).

%%%
redraw(LD) ->
  lists:foreach(fun({DA,_Colors}) -> redraw(DA,LD) end, ?DAREAS),
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
  VIs = lists:reverse(lists:sort(zip(Data,lists:seq(1,length(Data))))),
  lists:foreach(fun(VI) -> stuffer(X,Darea,VI) end, VIs),
  stuffit(X,Datas,Dareas).

stuffer(X,Darea,{Val,I}) ->
  draw_line(Darea,I,X,Val).

timeline(LD = #ld{state=disc}, no_time) -> LD;
timeline(LD = #ld{state=disc}, {_H,M,_S}) -> stat_change(up,LD#ld{minute=M});
timeline(LD = #ld{state=conn}, no_time) -> stat_change(down,LD);
%%timeline(LD = #ld{minute=undefined},{_H,M,_S}) -> LD#ld{minute=M};
timeline(LD = #ld{minute=M},{_H,M,_S}) -> LD;
timeline(LD = #ld{dAreas=Dareas},{_,M,_}=HMS) ->
  lists:foreach(fun(Darea)->draw_timeline(Darea,LD,HMS) end,Dareas),
  LD#ld{minute=M}.

stat_change(up,LD) ->
  Nod = to_str(conf_val(anode,LD)),
  g('Gtk_window_set_title',[window,"gperf - "++Nod]),
  statbar(Nod++" - connected",LD),
  LD#ld{state=conn};
stat_change(down,LD) ->
  g('Gtk_window_set_title',[window,"gperf"]),
  statbar(to_str(conf_val(anode,LD))++" - disconnected",LD),
  LD#ld{state=disc}.

statbar(Msg, #ld{stat_ctxt=Ctxt}) ->
  g('Gtk_statusbar_pop',[statusbar,Ctxt]),
  g('Gtk_statusbar_push',[statusbar,Ctxt,Msg]).

draw_timeline({_,Darea},LD,{H,M,_S}) ->
  draw_line(Darea,fg,LD#ld.x,1),
  draw_time(Darea,LD,{H,M}).

draw_time(#dArea{px_fg=Pxfg,px_bg=Pxbg,layout=Layout,gc_fg=GC},LD,{H,M}) ->
  Tm = lists:flatten(io_lib:fwrite("~2.2.0w:~2.2.0w",[H,M])),
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

get_gc(DA,N) when is_integer(N) -> lists:nth(N,DA#dArea.gcs);
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
  lists:foreach(fun(DA)->clear_one(DA) end, LD#ld.dAreas),
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
  g('Gdk_color_parse',[to_str(Color),Color]),
  g('Gdk_colormap_alloc_color',[ColorMap,Color,false,true]),
  g('Gdk_gc_set_foreground',[GC,Color]),
  [GC|gcs(Win,T)].

g(C,As) -> g([{C,As}]).

g(CAs) -> gtknode:cmd(gperf_gtk,CAs).

lks(Tag,List) ->
  {value,{Tag,Val}} = lists:keysearch(Tag,1,List),
  Val.

%%%GUI=gperf_GUI,G = fun(Wid,Cmd,Args)->GUI! {self(),[{Cmd,[Wid|Args]}]}, receive {GUI,{reply,[{ok,Rep}]}} -> Rep;{GUI,{reply,[{error,Rep}]}} -> erlang:fault({Cmd,Args,Rep}) end end.
