-module(eper).

-export([main/1]).

getopt_spec() ->
    [{command,    undefined,  undefined, atom,      "(dtop <node> | ntop <node>)"},
     {cookie,     $c,        "cookie",   atom,      "Erlang cookie to use for connecting to remote node."},
     {help,       undefined, "help",     undefined, "Display usage"},
     {nettick,    undefined, "nettick",  integer,   "net_ticktime kernel setting"},
     {shortnames, $s,        "snames",   boolean,   "Use shortnames for connecting to remote node."}
    ].

usage() ->
    getoptw:usage(getopt_spec(), "eper"),
    halt(1).

parse_args(Args) ->
    case getoptw:parse(getopt_spec(), Args) of
        {ok, {Options, ArgsRest}} ->
            case lists:member(help, Options) of
                true ->
                    usage();
                false ->
                    {Options, ArgsRest}
            end;
        {error, Reason} ->
            io:format("Failed to parse arguments: ~p\n", [Reason]),
            usage()
    end.

main(Args) ->
    {Options, ArgsRest} = parse_args(Args),
    case lists:keyfind(command, 1, Options) of
        {command, dtop} ->
            top_start(dtop, Options, ArgsRest);
        {command, ntop} ->
            top_start(ntop, Options, ArgsRest);
        Other ->
            io:format("Unknown command: ~p\n", [Other]),
            usage()
    end.

top_start(Module, Options, [NodeStr]) ->
    Node = init_net_kernel(Options, NodeStr),
    case {net_kernel:hidden_connect_node(Node), net_adm:ping(Node)} of
        {true, pong} ->
            wait_for(Module:start(Node));
        {_, pang} ->
            io:format("Node ~p not responding to pings.\n", [Node]),
            halt(1)
    end;
top_start(Module, _Options, _OtherArgs) ->
    io:format("Expected '~p' to be followed by exactly one nodename.\n", [Module]),
    usage().

init_net_kernel(Options, TargetNode) ->
    %% Generate a name for this node, based on target node
    ThisNode = append_node_suffix(TargetNode, "_eper_"),
    case lists:keymember(shortnames, 1, Options) of
        true ->
            {ok, _} = net_kernel:start([ThisNode, shortnames]);
        false ->
            {ok, _} = net_kernel:start([ThisNode, longnames])
    end,

    %% Set the cookie, if necessary
    case lists:keyfind(cookie, 1, Options) of
        {cookie, Cookie} ->
            erlang:set_cookie(node(), Cookie);
        _ ->
            ok
    end,

    %% Return the node name as an atom (with host appended to end, if necessary)
    nodename(TargetNode).


wait_for(Pid) ->
    Mref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Mref, _, _, _} ->
            ok
    end.

nodename(Name) ->
    case string:tokens(Name, "@") of
        [_Node, _Host] ->
            list_to_atom(Name);
        [Node] ->
            [_, Host] = string:tokens(atom_to_list(node()), "@"),
            list_to_atom(lists:concat([Node, "@", Host]))
    end.

append_node_suffix(Name, Suffix) ->
    case string:tokens(Name, "@") of
        [Node, Host] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid(), "@", Host]));
        [Node] ->
            list_to_atom(lists:concat([Node, Suffix, os:getpid()]))
    end.
