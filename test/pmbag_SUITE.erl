%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
-module(pmbag_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests.
-export([notify_to_active/1]).
-export([notify_to_overflow/1]).
-export([no_api_in/1]).

%% Macros
-define(wait_msg(PAT, RET),
	(fun() ->
		receive
			PAT ->
				RET
		after
			2000 ->
				error({wait_too_long})
		end
	end)()).

-define(wait_msg(PAT), ?wait_msg(PAT, ok)).

all() ->
	[
		{group, queue},
		{group, stack},
		{group, keep_old}
	].

groups() ->
	[
		{queue, [parallel], [{group, all}]},
		{stack, [parallel], [{group, all}]},
		{keep_old, [parallel], [{group, all}]},
		{all, [parallel], [
			notify_to_active,
			notify_to_overflow,
			no_api_in
		]}
	].

init_per_suite(Config) ->
	ok = application:start(pmbag),
	Config.

end_per_suite(_Config) ->
	ok = application:stop(pmbag),
	ok.

init_per_group(queue, Config) ->
	[{type, queue} | Config];
init_per_group(stack, Config) ->
	[{type, stack} | Config];
init_per_group(keep_old, Config) ->
	[{type, keep_old} | Config];
init_per_group(_Group, Config) ->
	Config.

end_per_group(_Group, _Config) ->
	ok.

init_per_testcase(_, Config) ->
	Type = ?config(type, Config),
	Size = 3,
	{ok, Pid} = pmbag:start_link(self(), Size, Type, notify),
	[{pmbag, Pid}, {size, Size} | Config].

end_per_testcase(_, Config) ->
	Pid = ?config(pmbag, Config),
	unlink(Pid),
	Ref = erlang:monitor(process, Pid),
	exit(Pid, shutdown),
	?wait_msg({'DOWN', Ref, process, Pid, _}).

%%====================================================================
%% Tests
%%====================================================================

notify_to_active(Config) ->
	%% Check that we can send messages to the PMBag and it will notify us
	%% about it. We should then be able to set it to active and it should
	%% send us the messages back.
	Bag = ?config(pmbag, Config),
	Size = ?config(size, Config),
	Sent = lists:seq(1, Size),
	[pmbag:in(Bag, N) || N <- Sent],
	?wait_msg({pmbag_mail, Bag}),
	ok = pmbag:active(Bag),
	Msgs = ?wait_msg({pmbag_mail, Bag, Msgs, Size, 0}, Msgs),
	%% Based on the type, we have different guarantees
	case ?config(type, Config) of
		queue ->
			% queues are in order
			Sent = Msgs;
		stack ->
			% stacks are in opposite order
			Sent = lists:reverse(Msgs);
		keep_old ->
			% in order, no benefit for out-of-order
			Sent = Msgs
	end,
	%% messages are not repeated, and we get good state transitions
	Msg = Size + 1,
	pmbag:in(Bag, Msg),
	pmbag:active(Bag),
	?wait_msg({pmbag_mail, Bag, [Msg], 1, 0}).

notify_to_overflow(Config) ->
	%% Check that we can send messages to the PMBag and it will notify
	%% us about it, but also overflow and tell us about how many messages
	%% overflowed.
	Bag = ?config(pmbag, Config),
	Size = ?config(size, Config),
	[pmbag:in(Bag, N) || N <- lists:seq(1, Size * 2)],
	?wait_msg({pmbag_mail, Bag}),
	ok = pmbag:active(Bag),
	Msgs = ?wait_msg({pmbag_mail, Bag, Msgs, Size, Size}, Msgs),
	%% Based on the type, we have different guarantees
	case ?config(type, Config) of
		queue ->
			% queues are in order. We expect to have lost the 1st msgs
			Msgs = lists:seq(Size + 1, Size * 2); % we dropped 1..Size
		stack ->
			% We don't care for the order. We have all oldest + 1 newest
			Kept = lists:sort([Size * 2 | lists:seq(1, Size - 1)]),
			Kept = lists:sort(Msgs);
		keep_old ->
			% In order. We expect to have lost the last messages
			Msgs = lists:seq(1, Size)
	end.

no_api_in(Config) ->
	%% We want to support the ability to post directly without going through
	%% the API. This can be done by sending a message directly with the
	%% form {post, Msg}, for each message.
	Bag = ?config(pmbag, Config),
	Size = ?config(size, Config),
	Sent = lists:seq(1, Size),
	[Bag ! {in, N} || N <- Sent],
	?wait_msg({pmbag_mail, Bag}),
	ok = pmbag:active(Bag),
	Sent = lists:sort(?wait_msg({pmbag_mail, Bag, Msgs, Size, 0}, Msgs)).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
