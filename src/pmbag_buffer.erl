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
-module(pmbag_buffer).

-include("pmbag_buffer.hrl").

%% API
-export([new/2]).
-export([flush/1]).
-export([in/2]).
-export([in_safe/2]).
-export([is_empty/1]).
-export([is_full/1]).
-export([out/1]).
-export([resize/2]).

-type buffer() :: #pmbag_buffer{}.

-export_type([buffer/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec new(buffer_type(), buffer_max())
	-> buffer().
new(queue, Size) when is_integer(Size) andalso Size > 0 ->
	#pmbag_buffer{type=queue, max=Size, data=queue:new()};
new(stack, Size) when is_integer(Size) andalso Size > 0 ->
	#pmbag_buffer{type=stack, max=Size, data=[]};
new(keep_old, Size) when is_integer(Size) andalso Size > 0 ->
	#pmbag_buffer{type=keep_old, max=Size, data=queue:new()}.

-spec flush(buffer())
	-> {{values, [any()], buffer_max(), buffer_drop()}, buffer()}.
flush(B=#pmbag_buffer{type=T, size=Size, drop=Drop, data=Data}) ->
	{Messages, NewData} = flush(T, Data, []),
	{{values, Messages, Size, Drop}, B#pmbag_buffer{size=0, drop=0, data=NewData}}.

-spec in(any(), buffer())
	-> buffer().
in(Item, B=#pmbag_buffer{type=T, max=Size, size=Size, drop=Drop, data=Data}) ->
	B#pmbag_buffer{drop=Drop+1, data=in_drop(T, Item, Size, Data)};
in(Item, B=#pmbag_buffer{type=T, size=Size, data=Data}) ->
	B#pmbag_buffer{size=Size+1, data=in(T, Item, Data)}.

-spec in_safe(any(), buffer())
	-> buffer() | {error, full}.
in_safe(_Item, #pmbag_buffer{max=Size, size=Size}) ->
	{error, full};
in_safe(Item, B=#pmbag_buffer{}) ->
	in(Item, B).

-spec is_empty(buffer())
	-> boolean().
is_empty(#pmbag_buffer{size=0}) ->
	true;
is_empty(#pmbag_buffer{}) ->
	false.

-spec is_full(buffer())
	-> boolean().
is_full(#pmbag_buffer{max=Size, size=Size}) ->
	true;
is_full(#pmbag_buffer{}) ->
	false.

-spec out(buffer())
	-> {{value, any()}, buffer()} | {empty, buffer()}.
out(B=#pmbag_buffer{type=T, size=Size, data=Data}) ->
	case out(T, Data) of
		{empty, Data} ->
			{empty, B};
		{{value, Item}, NewData} ->
			{{value, Item}, B#pmbag_buffer{size=Size-1, data=NewData}}
	end.

-spec resize(buffer_max(), buffer())
	-> buffer().
resize(NewMax, B=#pmbag_buffer{max=Max}) when Max =< NewMax ->
	B#pmbag_buffer{max=NewMax};
resize(NewMax, B=#pmbag_buffer{type=T, size=Size, drop=Drop, data=Data}) when Size > NewMax ->
	ToDrop = Size - NewMax,
	B#pmbag_buffer{size=NewMax, max=NewMax, drop=Drop+ToDrop, data=drop(T, ToDrop, Size, Data)};
resize(NewMax, B) ->
	B#pmbag_buffer{max=NewMax}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
drop(T, Size, Data) ->
	drop(T, 1, Size, Data).

%% @private
drop(_, 0, _Size, Data) ->
	Data;
drop(queue, 1, _Size, Queue) ->
	queue:drop(Queue);
drop(stack, 1, _Size, [_ | T]) ->
	T;
drop(keep_old, 1, _Size, Queue) ->
	queue:drop_r(Queue);
drop(queue, N, Size, Queue) when Size > N ->
	element(2, queue:split(N, Queue));
drop(queue, _N, _Size, _Queue) ->
	queue:new();
drop(stack, N, Size, Stack) when Size > N ->
	lists:nthtail(N, Stack);
drop(stack, _N, _Size, _Stack) ->
	[];
drop(keep_old, N, Size, Queue) when Size > N ->
	element(1, queue:split(N, Queue));
drop(keep_old, _N, _Size, _Queue) ->
	queue:new().

%% @private
flush(T, Data, Messages) ->
	case out(T, Data) of
		{empty, NewData} ->
			{lists:reverse(Messages), NewData};
		{{value, Message}, NewData} ->
			flush(T, NewData, [Message | Messages])
	end.

%% @private
in(queue, Message, Queue) ->
	queue:in(Message, Queue);
in(stack, Message, Stack) ->
	[Message | Stack];
in(keep_old, Message, Queue) ->
	queue:in(Message, Queue).

%% @private
in_drop(keep_old, _Item, _Size, Data) ->
	Data;
in_drop(T, Item, Size, Data) ->
	in(T, Item, drop(T, Size, Data)).

%% @private
out(queue, Q) ->
	queue:out(Q);
out(stack, []) ->
	{empty, []};
out(stack, [H|T]) ->
	{{value, H}, T};
out(keep_old, Q) ->
	queue:out(Q).
