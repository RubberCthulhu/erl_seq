%%%-------------------------------------------------------------------
%%% @author Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%% @copyright (C) 2016, Danil Onishchenko
%%% @doc
%%%
%%% @end
%%% Created : 31 May 2016 by Danil Onishchenko <danil.onishchenko.info@gmail.com>
%%%-------------------------------------------------------------------
-module(seq).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, start_link/2,
	 start/0, start/1, start/2, stop/1,
	 next/1, reset/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SEQ_MIN, 0).
-define(SEQ_MAX, 1000000000).
-define(SEQ_STEP, 1).

-record(state, {
	  min = ?SEQ_MIN,
	  max = ?SEQ_MAX,
	  step = ?SEQ_STEP,
	  circle = true,
	  i = ?SEQ_MIN
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link([]).

start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

start_link(Name, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Opts], []).

start() ->
    start([]).

start(Opts) ->
    seq_sup:start_child(Opts).

start(Name, Opts) ->
    seq_sup:start_child(Name, Opts).

stop(Id) ->
    seq_sup:stop_child(Id).

next(Id) ->
    gen_server:call(Id, next).

reset(Id) ->
    gen_server:cast(Id, reset).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Opts]) ->
    State = #state{},
    State1 = #state{
		min = proplists:get_value(min, Opts, State#state.min),
		max = proplists:get_value(max, Opts, State#state.max),
		step = proplists:get_value(step, Opts, State#state.step),
		circle = proplists:get_value(circle, Opts, State#state.circle)
	       },
    {ok, State1}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(next, _From, #state{i = I, step = Step, min = Min, max = Max, circle = Circle} = State) ->
    case I > Max of
	true when Circle ->
	    {reply, {ok, Min}, State#state{i = Min}};
	true ->
	    {reply, {error, max_value}, State};
	false ->
	    I1 = I + Step,
	    {reply, {ok, I}, State#state{i = I1}}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(reset, #state{min = Min} = State) ->
    {noreply, State#state{i = Min}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


