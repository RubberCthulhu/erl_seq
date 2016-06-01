%%%-------------------------------------------------------------------
%%% @author Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%% @copyright (C) 2016, Danil Onishchenko
%%% @doc
%%%
%%% @end
%%% Created :  1 Jun 2016 by Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%%-------------------------------------------------------------------
-module(seq_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, start_child/2, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Opts) ->
    start_child1([Opts]).

start_child(Name, Opts) ->
    start_child1([Name, Opts]).

start_child1(Args) ->
    Id = {seq, make_ref()},
    Spec = {Id, {seq, start_link, Args},
	    permanent, 2000, worker, [seq]},
    supervisor:start_child(?SERVER, Spec).

stop_child(Pid) when is_pid(Pid) ->
    case [Id || {Id, P, _, _} <- supervisor:which_children(?SERVER), P == Pid] of
	[Id] ->
	    case supervisor:terminate_child(?SERVER, Id) of
		ok ->
		    supervisor:delete_child(?SERVER, Id);
		Else ->
		    Else
	    end;
	[] ->
	    {error, not_started}
    end;
stop_child(Name) ->
    stop_child(whereis(Name)).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


