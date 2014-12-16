%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014, Andrew Bennett
%%% @doc Inspired by https://github.com/ferd/pobox
%%%
%%% @end
%%% Created :  30 Sep 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------
-module(pmbag).
-behaviour(gen_fsm).

-include("pmbag_buffer.hrl").

%% API
-export([start_link/2]).
-export([start_link/3]).
-export([start_link/4]).
-export([start_link/5]).
-export([start_link/6]).

%% State API
-export([active/1]).
-export([notify/1]).
-export([once_notify/1]).
-export([once_passive/1]).
-export([passive/1]).

%% Queue API
-export([in/2]).
-export([in_safe/2]).
-export([is_empty/1]).
-export([is_full/1]).
-export([resize/2]).

%% gen_fsm callbacks
-export([init/1]).
-export([handle_sync_event/4]).
-export([handle_event/3]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

%% gen_fsm states
-export([active/2]).
-export([notify/2]).
-export([once_notify/2]).
-export([once_passive/2]).
-export([passive/2]).

-record(state_data, {
	buffer    = undefined :: undefined | pmbag_buffer:buffer(),
	hibernate = undefined :: undefined | infinity | timeout(),
	owner     = undefined :: undefined | pid()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link(buffer_max(), buffer_type())
	-> {ok, pid()}.
start_link(Size, Type)
		when (is_integer(Size) andalso Size > 0)
		andalso (Type =:= queue
			orelse Type =:= stack
			orelse Type =:= keep_old) ->
	start_link(self(), Size, Type).

-spec start_link(pid() | term(), buffer_max(), buffer_type())
	-> {ok, pid()}.
start_link(Owner, Size, Type)
		when (is_pid(Owner) orelse is_atom(Owner))
		andalso (is_integer(Size) andalso Size > 0)
		andalso (Type =:= queue
			orelse Type =:= stack
			orelse Type =:= keep_old) ->
	start_link(Owner, Size, Type, passive);
start_link(ServerName, Size, Type)
		when (is_integer(Size) andalso Size > 0)
		andalso (Type =:= queue
			orelse Type =:= stack
			orelse Type =:= keep_old) ->
	start_link(ServerName, self(), Size, Type).

-spec start_link(pid() | term(), buffer_max() | pid(), buffer_type() | buffer_max(), atom())
	-> {ok, pid()}.
start_link(Owner, Size, Type, StateName)
		when (is_pid(Owner) orelse is_atom(Owner))
		andalso (is_integer(Size) andalso Size > 0)
		andalso (Type =:= queue
			orelse Type =:= stack
			orelse Type =:= keep_old)
		andalso (StateName =:= active
			orelse StateName =:= notify
			orelse StateName =:= once_notify
			orelse StateName =:= once_passive
			orelse StateName =:= passive) ->
	start_link(Owner, Size, Type, StateName, infinity);
start_link(ServerName, Owner, Size, Type)
		when (is_pid(Owner) orelse is_atom(Owner))
		andalso (is_integer(Size) andalso Size > 0)
		andalso (Type =:= queue
			orelse Type =:= stack
			orelse Type =:= keep_old) ->
	start_link(ServerName, Owner, Size, Type, passive).

-spec start_link(pid() | term(), buffer_max() | pid(), buffer_type() | buffer_max(), atom() | buffer_type(), infinity | timeout())
	-> {ok, pid()}.
start_link(Owner, Size, Type, StateName, Hibernate)
		when (is_pid(Owner) orelse is_atom(Owner))
		andalso (is_integer(Size) andalso Size > 0)
		andalso (Type =:= queue
			orelse Type =:= stack
			orelse Type =:= keep_old)
		andalso (StateName =:= active
			orelse StateName =:= notify
			orelse StateName =:= once_notify
			orelse StateName =:= once_passive
			orelse StateName =:= passive)
		andalso (Hibernate =:= infinity
			orelse is_integer(Hibernate) andalso Hibernate >= 0) ->
	gen_fsm:start_link(?MODULE, {Owner, Size, Type, StateName, Hibernate}, []);
start_link(ServerName, Owner, Size, Type, StateName)
		when (is_pid(Owner) orelse is_atom(Owner))
		andalso (is_integer(Size) andalso Size > 0)
		andalso (Type =:= queue
			orelse Type =:= stack
			orelse Type =:= keep_old)
		andalso (StateName =:= active
			orelse StateName =:= notify
			orelse StateName =:= once_notify
			orelse StateName =:= once_passive
			orelse StateName =:= passive) ->
	start_link(ServerName, Owner, Size, Type, infinity).

-spec start_link(term(), pid(), buffer_max(), buffer_type(), atom(), infinity | timeout())
	-> {ok, pid()}.
start_link(ServerName, Owner, Size, Type, StateName, Hibernate)
		when (is_pid(Owner) orelse is_atom(Owner))
		andalso (is_integer(Size) andalso Size > 0)
		andalso (Type =:= queue
			orelse Type =:= stack
			orelse Type =:= keep_old)
		andalso (StateName =:= active
			orelse StateName =:= notify
			orelse StateName =:= once_notify
			orelse StateName =:= once_passive
			orelse StateName =:= passive)
		andalso (Hibernate =:= infinity
			orelse is_integer(Hibernate) andalso Hibernate >= 0) ->
	gen_fsm:start_link(ServerName, ?MODULE, {Owner, Size, Type, StateName, Hibernate}, []).

%%%===================================================================
%%% State API functions
%%%===================================================================

-spec active(pid())
	-> ok.
active(Mailbag) ->
	gen_fsm:send_event(Mailbag, active).

-spec notify(pid())
	-> ok.
notify(Mailbag) ->
	gen_fsm:send_event(Mailbag, notify).

-spec once_notify(pid())
	-> ok.
once_notify(Mailbag) ->
	gen_fsm:send_event(Mailbag, once_notify).

-spec once_passive(pid())
	-> ok.
once_passive(Mailbag) ->
	gen_fsm:send_event(Mailbag, once_passive).

-spec passive(pid())
	-> ok.
passive(Mailbag) ->
	gen_fsm:send_event(Mailbag, passive).

%%%===================================================================
%%% Queue API functions
%%%===================================================================

-spec in(pid(), term())
	-> ok.
in(Mailbag, Message) ->
	gen_fsm:send_event(Mailbag, {in, Message}).

-spec in_safe(pid(), term())
	-> ok | {error, full}.
in_safe(Mailbag, Message) ->
	gen_fsm:sync_send_all_state_event(Mailbag, {in_safe, Message}).

-spec is_empty(pid())
	-> boolean().
is_empty(Mailbag) ->
	gen_fsm:sync_send_all_state_event(Mailbag, is_empty).

-spec is_full(pid())
	-> boolean().
is_full(Mailbag) ->
	gen_fsm:sync_send_all_state_event(Mailbag, is_full).

-spec resize(pid(), buffer_max())
	-> ok.
resize(Mailbag, NewSize) when NewSize > 0 ->
	gen_fsm:sync_send_all_state_event(Mailbag, {resize, NewSize}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%% @private
init({OwnerPidOrAtom, Size, Type, StateName, Hibernate}) ->
	Owner = case OwnerPidOrAtom of
		_ when is_pid(OwnerPidOrAtom) ->
			OwnerPidOrAtom;
		_ when is_atom(OwnerPidOrAtom) ->
			erlang:whereis(OwnerPidOrAtom)
	end,
	true = erlang:link(Owner),
	StateData = #state_data{buffer=pmbag_buffer:new(Type, Size),
		hibernate=Hibernate, owner=Owner},
	{ok, StateName, StateData, Hibernate}.

%% @private
handle_sync_event({in_safe, Message}, _From, StateName, StateData=#state_data{buffer=Buffer}) ->
	case pmbag_buffer:is_full(Buffer) of
		false ->
			{next_state, NewStateName, NewStateData, Hibernate} = ?MODULE:StateName({in, Message}, StateData),
			{reply, ok, NewStateName, NewStateData, Hibernate};
		true ->
			{reply, {error, full}, StateName, StateData}
	end;
handle_sync_event(is_empty, _From, StateName, StateData=#state_data{buffer=Buffer}) ->
	{reply, pmbag_buffer:is_empty(Buffer), StateName, StateData};
handle_sync_event(is_full, _From, StateName, StateData=#state_data{buffer=Buffer}) ->
	{reply, pmbag_buffer:is_full(Buffer), StateName, StateData};
handle_sync_event({resize, NewSize}, _From, StateName, StateData=#state_data{buffer=Buffer}) ->
	{reply, ok, StateName, StateData#state_data{buffer=pmbag_buffer:resize(NewSize, Buffer)}};
handle_sync_event(_Event, _From, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% @private
handle_event(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% @private
handle_info({in, Message}, StateName, StateData) ->
	?MODULE:StateName({in, Message}, StateData);
handle_info(_Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%% @private
terminate(_Reason, _StateName, _StateData) ->
	ok.

%% @private
code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%===================================================================
%%% gen_fsm states
%%%===================================================================

%% @private
active({in, Message}, StateData=#state_data{buffer=Buffer, hibernate=Hibernate}) ->
	{next_state, active, send(StateData#state_data{buffer=pmbag_buffer:in(Message, Buffer)}), Hibernate};
active(notify, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, notify, StateData, Hibernate};
active(once_notify, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, once_notify, StateData, Hibernate};
active(once_passive, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, once_passive, StateData, Hibernate};
active(passive, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, passive, StateData, Hibernate};
active(timeout, StateData) ->
	{next_state, active, StateData, hibernate};
active(_Event, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, active, StateData, Hibernate}.

%% @private
notify({in, Message}, StateData=#state_data{buffer=Buffer, owner=Owner, hibernate=Hibernate}) ->
	Owner ! {pmbag_mail, self()},
	{next_state, passive, StateData#state_data{buffer=pmbag_buffer:in(Message, Buffer)}, Hibernate};
notify(active, StateData=#state_data{buffer=#pmbag_buffer{size=0}, hibernate=Hibernate}) ->
	{next_state, active, StateData, Hibernate};
notify(once_notify, StateData=#state_data{buffer=#pmbag_buffer{size=0}, hibernate=Hibernate}) ->
	{next_state, once_notify, StateData, Hibernate};
notify(once_passive, StateData=#state_data{buffer=#pmbag_buffer{size=0}, hibernate=Hibernate}) ->
	{next_state, once_passive, StateData, Hibernate};
notify(active, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, active, send(StateData), Hibernate};
notify(once_notify, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, notify, send(StateData), Hibernate};
notify(once_passive, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, passive, send(StateData), Hibernate};
notify(timeout, StateData) ->
	{next_state, notify, StateData, hibernate};
notify(_Event, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, notify, StateData, Hibernate}.

%% @private
once_notify({in, Message}, StateData=#state_data{buffer=Buffer, hibernate=Hibernate}) ->
	{next_state, notify, send(StateData#state_data{buffer=pmbag_buffer:in(Message, Buffer)}), Hibernate};
once_notify(active, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, active, StateData, Hibernate};
once_notify(notify, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, notify, StateData, Hibernate};
once_notify(once_passive, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, once_passive, StateData, Hibernate};
once_notify(passive, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, passive, StateData, Hibernate};
once_notify(timeout, StateData) ->
	{next_state, once_notify, StateData, hibernate};
once_notify(_Event, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, once_notify, StateData, Hibernate}.

%% @private
once_passive({in, Message}, StateData=#state_data{buffer=Buffer, hibernate=Hibernate}) ->
	{next_state, passive, send(StateData#state_data{buffer=pmbag_buffer:in(Message, Buffer)}), Hibernate};
once_passive(active, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, active, StateData, Hibernate};
once_passive(notify, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, notify, StateData, Hibernate};
once_passive(once_notify, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, once_notify, StateData, Hibernate};
once_passive(passive, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, passive, StateData, Hibernate};
once_passive(timeout, StateData) ->
	{next_state, once_passive, StateData, hibernate};
once_passive(_Event, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, once_passive, StateData, Hibernate}.

%% @private
passive({in, Message}, StateData=#state_data{buffer=Buffer, hibernate=Hibernate}) ->
	{next_state, passive, StateData#state_data{buffer=pmbag_buffer:in(Message, Buffer)}, Hibernate};
passive(active, StateData=#state_data{buffer=#pmbag_buffer{size=0}, hibernate=Hibernate}) ->
	{next_state, active, StateData, Hibernate};
passive(notify, StateData=#state_data{buffer=#pmbag_buffer{size=0}, hibernate=Hibernate}) ->
	{next_state, notify, StateData, Hibernate};
passive(once_notify, StateData=#state_data{buffer=#pmbag_buffer{size=0}, hibernate=Hibernate}) ->
	{next_state, once_notify, StateData, Hibernate};
passive(once_passive, StateData=#state_data{buffer=#pmbag_buffer{size=0}, hibernate=Hibernate}) ->
	{next_state, once_passive, StateData, Hibernate};
passive(active, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, active, send(StateData), Hibernate};
passive(notify, StateData=#state_data{owner=Owner, hibernate=Hibernate}) ->
	Owner ! {pmbag_mail, self()},
	{next_state, passive, StateData, Hibernate};
passive(once_notify, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, notify, send(StateData), Hibernate};
passive(once_passive, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, passive, send(StateData), Hibernate};
passive(timeout, StateData) ->
	{next_state, passive, StateData, hibernate};
passive(_Event, StateData=#state_data{hibernate=Hibernate}) ->
	{next_state, passive, StateData, Hibernate}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
send(StateData=#state_data{buffer=Buffer, owner=Owner}) ->
	{{values, Messages, Count, Dropped}, NewBuffer} = pmbag_buffer:flush(Buffer),
	Owner ! {pmbag_mail, self(), Messages, Count, Dropped},
	StateData#state_data{buffer=NewBuffer}.
