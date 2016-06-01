%%%-------------------------------------------------------------------
%%% @author Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%% @copyright (C) 2016, Danil Onishchenko
%%% @doc
%%%
%%% @end
%%% Created :  1 Jun 2016 by Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%%-------------------------------------------------------------------
-module(seq_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    case seq_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


