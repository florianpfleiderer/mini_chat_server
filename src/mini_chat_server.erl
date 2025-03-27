-module(mini_chat_server).
-behaviour(gen_server).

-export([start_link/0, send_msg/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

send_msg(User, Msg) ->
    gen_server:cast(?MODULE, {send, User, Msg}).

handle_cast({send, User, Msg}, State) ->
    io:format("~p says: ~p~n", [User, Msg]),
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_, State, _) -> {ok, State}.
