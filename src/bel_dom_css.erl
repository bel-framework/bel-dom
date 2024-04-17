%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc CSS module.
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
-module(bel_dom_css).

% API
-export([ scan/1, parse/1, find_one/2, find_all/2 ]).

%%%=====================================================================
%%% API
%%%=====================================================================

scan(String) ->
    bel_css:scan_css3(String).

parse(Tokens) ->
    bel_css:parse_css3(Tokens).

% TODO
find_one(_Selectors, _Tokens) ->
    undefined.

% TODO
find_all(_Selectors, _Tokens) ->
    [].
