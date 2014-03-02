%%% -*- erlang -*-
%%%
%%% This file is part of erlang-idna released under the BSD license.
%%% See the LICENSE for more information.
%%%
-module(idna_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% default number of objects in the cache
-define(CACHE_SIZE, 500).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    UnicodeData = {idna_unicode_data,
                   {idna_unicode_data, start_link, []},
                   permanent, 5000, worker, [idna_unicode_data]},

    %% set the cache
    CacheSize = case application:get_env(idna, cache_size) of
        {ok, Val} -> Val;
        _ -> ?CACHE_SIZE
    end,
    LruOptions = [{max_objects, CacheSize}],
    Cache = {idna_lru,
             {ets_lru, start_link, [idna_lru, LruOptions]},
             permanent, 5000, worker, [idna_lru]},

    {ok, { {one_for_one, 10, 1}, [Cache, UnicodeData]}}.
