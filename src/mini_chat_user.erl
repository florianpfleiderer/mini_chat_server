-module(mini_chat_user).
-behaviour(gen_server).

-export([start_link/1, send/2]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, Name, []).

send(User, Msg) ->
    gen_server:cast(User, {msg, Msg}).

init(Name) ->
    {ok, Name}.

handle_cast({msg, Msg}, Name) ->
    io:format("[~p] received: ~p~n", [Name, Msg]),
    {noreply, Name}.

handle_call(_, _, State) -> {reply, ok, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
