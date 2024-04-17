%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc DOM module.
%%%
%%% Copyright 2024 William Fank Thomé
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @end
%%%---------------------------------------------------------------------
-module(bel_dom).

% API
-export([ scan_document/1
        , parse_document/1
        , scan_query/1
        , parse_query/1
        , query_selector/2
        , query_selector_all/2
        ]).

%%%=====================================================================
%%% API
%%%=====================================================================

scan_document(String) when is_binary(String) ->
    bel_dom_html:scan(String).

parse_document(Tokens) when is_list(Tokens) ->
    bel_dom_html:parse(Tokens);
parse_document(String) when is_binary(String) ->
    parse_document(scan_document(String)).

scan_query(String) when is_binary(String) ->
    bel_dom_css:scan(String).

parse_query(Tokens) when is_list(Tokens) ->
    bel_dom_css:parse(Tokens);
parse_query(String) when is_binary(String) ->
    {ok, Tokens, _} = scan_query(String),
    parse_query(Tokens).

query_selector(Selectors, DOM) when is_list(Selectors), is_list(DOM) ->
    bel_dom_css:find_one(Selectors, DOM);
query_selector(Query, DOM) when is_binary(Query) ->
    {ok, Selectors} = parse_query(Query),
    query_selector(Selectors, DOM);
query_selector(Selectors, DOM) when is_binary(DOM) ->
    query_selector(Selectors, parse_document(DOM)).

query_selector_all(Selectors, DOM) when is_list(Selectors), is_list(DOM) ->
    bel_dom_css:find_all(Selectors, DOM);
query_selector_all(Query, DOM) when is_binary(Query) ->
    {ok, Selectors} = parse_query(Query),
    query_selector_all(Selectors, DOM);
query_selector_all(Selectors, DOM) when is_binary(DOM) ->
    query_selector_all(Selectors, parse_document(DOM)).
