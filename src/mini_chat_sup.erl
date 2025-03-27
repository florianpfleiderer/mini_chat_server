-module(mini_chat_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChatServer = {mini_chat_server,
                  {mini_chat_server, start_link, []},
                  permanent, 5000, worker, [mini_chat_server]},
    {ok, {{one_for_one, 5, 10}, [ChatServer]}}.
