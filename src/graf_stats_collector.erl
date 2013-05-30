-module(graf_stats_collector).

-behaviour(gen_server).

-export([
    fetch/0
]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(st, {
    descriptions,
    stats,
    timer
}).

fetch() ->
    {ok, Stats} = gen_server:call(?MODULE, fetch),
    Stats.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    DescFiles = case application:get_env(graf, descriptions) of
        undefined -> [];
        Value -> Value
    end,
    Descriptions = lists:foldl(
        fun(File, Acc) -> {ok, D} = file:consult(File), D ++ Acc end,
        [],
        DescFiles
    ),
    lists:map(
        fun({Name, [{type, Type}, _]}) -> graf:new(Type, Name) end,
        Descriptions
    ),
    Interval = case application:get_env(graf, collection_interval) of
        {ok, I} -> I * 1000
    end,
    {ok, Timer} = timer:send_interval(Interval, self(), collect),
    {ok, #st{descriptions=Descriptions, stats=[], timer=Timer}}.

handle_call(fetch, _from, #st{stats = Stats}=State) ->
    {reply, {ok, Stats}, State};
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, error, State}.

handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

handle_info(collect, #st{descriptions=Descriptions}=State) ->
    Stats = lists:map(
        fun({Name, Props}) -> {Name, [{value, graf:sample(Name)}|Props]} end,
        Descriptions
    ),
    {noreply, State#st{stats=Stats}};
handle_info(Msg, State) ->
    {stop, {unknown_info, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
