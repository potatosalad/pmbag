%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <potatosaladx@gmail.com>
%%% @copyright 2014, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  01 Oct 2014 by Andrew Bennett <potatosaladx@gmail.com>
%%%-------------------------------------------------------------------

-ifndef(PMBAG_BUFFER_HRL).

-type buffer_data() :: queue:queue(any()) | list(any()).
-type buffer_drop() :: non_neg_integer().
-type buffer_max()  :: pos_integer().
-type buffer_type() :: keep_old | queue | stack.

-record(pmbag_buffer, {
	type = undefined :: undefined | buffer_type(),
	max  = undefined :: undefined | buffer_max(),
	size = 0         :: non_neg_integer(),
	drop = 0         :: buffer_drop(),
	data = undefined :: undefined | buffer_data()
}).

-define(PMBAG_BUFFER_HRL, 1).

-endif.
