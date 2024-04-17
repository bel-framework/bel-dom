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

scan_document(String) ->
    bel_dom_html:scan(String).

parse_document(Tokens) ->
    bel_dom_html:parse(Tokens).

scan_query(String) ->
    bel_dom_css:scan(String).

parse_query(Tokens) ->
    bel_dom_css:parse(Tokens).

query_selector(Selectors, Tokens) ->
    bel_dom_css:find_one(Selectors, Tokens).

query_selector_all(Selectors, Tokens) ->
    bel_dom_css:find_all(Selectors, Tokens).
