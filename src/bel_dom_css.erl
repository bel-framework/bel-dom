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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%=====================================================================
%%% API
%%%=====================================================================

scan(String) ->
    bel_css:scan_css3(String).

parse(Tokens) ->
    bel_css:parse_css3(Tokens).

% TODO: Change implementation to return the result on the first match.
find_one(Selectors, Tokens) ->
    case find_all(Selectors, Tokens) of
        [H | _] ->
            H;
        [] ->
            undefined
    end.

find_all(Selectors, Tokens) ->
    do_find(Selectors, Tokens, []).

%%%=====================================================================
%%% Internal functions
%%%=====================================================================

do_find([Selector | T], Tokens, Acc) ->
    do_find(T, Tokens, resolve_combinator(Selector, Tokens, Acc));
do_find([], _Tokens, Acc) ->
    Acc.

resolve_combinator({space, Selector}, Tokens, Acc) ->
    find_descendant(Tokens, Selector, Acc);
resolve_combinator({greater, Selector}, Tokens, Acc) ->
    find_child(Tokens, Selector, Acc);
resolve_combinator({plus, Selector}, Tokens, Acc) ->
    find_adjacent(Tokens, Selector, Acc);
resolve_combinator({tilde, Selector}, Tokens, Acc) ->
    find_general(Tokens, Selector, Acc);
resolve_combinator(Selector, Tokens, Acc) ->
    find_match(Tokens, Selector, Acc).

find_descendant(Tokens, {LeftSelector, RightSelector}, Acc) ->
    DescendantTokens = lists:reverse(do_find_descendant(Tokens, LeftSelector, [])),
    lists:reverse(do_find_descendant(DescendantTokens, RightSelector, Acc)).

find_child(Tokens, {LeftSelector, RightSelector}, Acc) ->
    ChildTokens = lists:reverse(do_find_child(Tokens, LeftSelector, [])),
    lists:reverse(lists:foldl(fun({_Tag, _Attrs, Nodes}, CAcc0) ->
        lists:foldl(fun(Token, CAcc) ->
            case match_all(RightSelector, Token) of
                true  -> [Token | CAcc];
                false -> CAcc
            end
        end, CAcc0, Nodes)
    end, Acc, ChildTokens)).

find_adjacent(Tokens, {LeftSelector, RightSelector}, Acc) ->
    lists:reverse(do_find_adjacent(Tokens, {LeftSelector, RightSelector}, Acc)).

find_general(Tokens, {LeftSelector, RightSelector}, Acc) ->
    lists:reverse(do_find_general(Tokens, {LeftSelector, RightSelector}, Acc)).

find_match(Tokens, Selector, []) ->
    lists:reverse(do_find_descendant(Tokens, Selector, [])).

do_find_descendant([{_Tag, _Attrs, Nodes} = Token | T], Selector, Acc) ->
    case match_all(Selector, Token) of
        true ->
            do_find_descendant(T, Selector, [Token | Acc]);
        false ->
            do_find_descendant(T, Selector, do_find_descendant(Nodes, Selector, Acc))
    end;
do_find_descendant([Text | T], Selector, Acc) when is_binary(Text) ->
    do_find_descendant(T, Selector, Acc);
do_find_descendant([], _, Acc) ->
    Acc.

do_find_child([{_Tag, _Attrs, Nodes} = Token | T], Selector, Acc) ->
    case match_all(Selector, Token) of
        true ->
            do_find_child(T, Selector, do_find_child(Nodes, Selector, [Token | Acc]));
        false ->
            do_find_child(T, Selector, do_find_child(Nodes, Selector, Acc))
    end;
do_find_child([Text | T], Selector, Acc) when is_binary(Text) ->
    do_find_child(T, Selector, Acc);
do_find_child([], _, Acc) ->
    Acc.

do_find_adjacent(
    [{_Tag, _Attrs, Nodes} = Token, {_, _, _} = Adjacent | T],
    {LeftSelector, RightSelector} = Selector,
    Acc
) ->
    case match_all(LeftSelector, Token) of
        true ->
            case match_all(RightSelector, Adjacent) of
                true ->
                    do_find_adjacent(
                        [Adjacent | T],
                        Selector,
                        do_find_adjacent(Nodes, Selector, [Adjacent | Acc])
                    );
                false ->
                    do_find_adjacent(
                        [Adjacent | T],
                        Selector,
                        do_find_adjacent(Nodes, Selector, Acc)
                    )
            end;
        false ->
            do_find_adjacent(
                [Adjacent | T],
                Selector,
                do_find_adjacent(Nodes, Selector, Acc)
            )
    end;
do_find_adjacent([{_Tag, _Attrs, Nodes} | T], Selector, Acc) ->
    do_find_adjacent(T, Selector, do_find_adjacent(Nodes, Selector, Acc));
do_find_adjacent([Text | T], Selector, Acc) when is_binary(Text) ->
    do_find_adjacent(T, Selector, Acc);
do_find_adjacent([], _, Acc) ->
    Acc.

do_find_general(
    [{_Tag, _Attrs, Nodes} = Token | T],
    {LeftSelector, RightSelector} = Selector,
    Acc0
) ->
    case match_all(LeftSelector, Token) of
        true ->
            do_find_general(Nodes, Selector, lists:foldl(fun(NextToken, Acc) ->
                case match_all(RightSelector, NextToken) of
                    true ->
                        [NextToken | Acc];
                    false ->
                        Acc
                end
            end, Acc0, T));
        false ->
            do_find_general(T, Selector, do_find_general(Nodes, Selector, Acc0))
    end;
do_find_general([Text | T], Selector, Acc) when is_binary(Text) ->
    do_find_general(T, Selector, Acc);
do_find_general([], _, Acc) ->
    Acc.

match_all([Selector | T], Token) ->
    match(Selector, Token) andalso match_all(T, Token);
match_all([], _Token) ->
    true.

% TODO: All kind o match.
% {attrib, {NamespacePrefix, Ident, Match}}
% {pseudo_class, {ident, Ident} | {'function, {Name, Exprs}}}
% {pseudo_element, {ident, Ident} | {'function, {Name, Exprs}}}
% {negation, Arg}
% {universal, NamespacePrefix}
match({type, Type}, Token) ->
    match_type(Type, Token);
match({id, Id}, Token) ->
    match_id(Id, Token);
match({class, Class}, Token) ->
    match_class(Class, Token).

match_type({_NamespacePrefix, ATag}, {BTag, _Attrs, _Nodes}) ->
    ATag =:= BTag.

match_id(Id, {_Tag, Attrs, _}) ->
    case Attrs of
        #{<<"id">> := Id} ->
            true;
        #{} ->
            false
    end.

match_class(Class, {_Tag, Attrs, _}) ->
    case Attrs of
        #{<<"class">> := Classes} ->
            lists:member(Class, Classes);
        #{} ->
            false
    end.

%%%=====================================================================
%%% Test
%%%=====================================================================

-ifdef(TEST).

find_query(Query, Html) ->
    {ok, CssTokens, _} = bel_dom:scan_query(Query),
    {ok, Selectors} = bel_dom:parse_query(CssTokens),
    Tokens = bel_dom:parse_document(bel_dom:scan_document(Html)),
    find_all(Selectors, Tokens).

find_descendant_test() ->
    Expect = [
        {<<"p">>,#{},[<<"Paragraph 1 in the div.">>]},
        {<<"p">>,#{},[<<"Paragraph 2 in the div.">>]},
        {<<"p">>,#{},[<<"Paragraph 3 in the div.">>]},
        {<<"p">>,#{},[<<"Paragraph 4 in the div.">>]},
        {<<"p">>,#{},[<<"Paragraph 5 in the div.">>]},
        {<<"p">>,#{},[<<"Paragraph 6 in the div.">>]},
        {<<"p">>,#{},[<<"Paragraph 7 in the div.">>]},
        {<<"p">>,#{},[<<"Paragraph 8 in the div.">>]},
        {<<"p">>,#{},[<<"Paragraph 9 in the div.">>]}
    ],
    Query = <<"div p">>,
    Html = <<"
    <body>
        <h2>Descendant Selector</h2>
        <p>The descendant selector matches all elements that are descendants of a specified element.</p>
        <div>
            <p>Paragraph 1 in the div.</p>
            <p>Paragraph 2 in the div.</p>
            <section><p>Paragraph 3 in the div.</p></section>
            <div>
                <p>Paragraph 4 in the div.</p>
                <p>Paragraph 5 in the div.</p>
                <section>
                    <p>Paragraph 6 in the div.</p>
                    <span>
                        <p>Paragraph 7 in the div.</p>
                        <p>Paragraph 8 in the div.</p>
                        <section><p>Paragraph 9 in the div.</p></section>
                    </span>
                </section>
            </div>
        </div>
        <p>Paragraph 10. Not in a div.</p>
        <p>Paragraph 11. Not in a div.</p>
    </body>
    ">>,
    ?assertEqual(Expect, find_query(Query, Html)).

find_child_test() ->
    Expect = [
        {<<"p">>,#{},[<<"Paragraph 1 in the div.">>]},
        {<<"p">>,#{},[<<"Paragraph 2 in the div.">>]},
        {<<"p">>,#{},[<<"Paragraph 5 in the div.">>]},
        {<<"p">>,#{},[<<"Paragraph 4 in the div.">>]}
    ],
    Query = <<"div > p">>,
    Html = <<"
    <body>
        <h2>Child Selector</h2>
        <p>The child selector (>) selects all elements that are the children of a specified element.</p>
        <div>
            <p>Paragraph 1 in the div.</p>
            <p>Paragraph 2 in the div.</p>
            <section>
                <!-- not Child but Descendant -->
                <p>Paragraph 3 in the div (inside a section element).</p>
            </section>
            <div>
                <p>Paragraph 4 in the div.</p>
            </div>
            <p>Paragraph 5 in the div.</p>
        </div>
        <p>Paragraph 6. Not in a div.</p>
        <p>Paragraph 7. Not in a div.</p>
    </body>
    ">>,
    ?assertEqual(Expect, find_query(Query, Html)).

find_adjacent_test() ->
    Expect = [
        {<<"p">>,#{},[<<"Paragraph 6. After a div.">>]},
        {<<"p">>,#{},[<<"Paragraph 5. After a div.">>]},
        {<<"p">>,#{},[<<"Paragraph 10. After a div.">>]}
    ],
    Query = <<"div + p">>,
    Html = <<"
    <body>
        <h2>Adjacent Sibling Selector</h2>
        <p>The + selector is used to select an element that is directly after another specific element.</p>
        <p>The following example selects the first p element that are placed immediately after div elements:</p>
        <div>
            <p>Paragraph 1 in the div.</p>
            <p>Paragraph 2 in the div.</p>
            <div>
                <p>Paragraph 3 in the div.</p>
                <p>Paragraph 4 in the div.</p>
                <div></div>
                <p>Paragraph 5. After a div.</p>
            </div>
        </div>
        <p>Paragraph 6. After a div.</p>
        <p>Paragraph 7. After a div.</p>
        <div>
            <p>Paragraph 8 in the div.</p>
            <p>Paragraph 9 in the div.</p>
        </div>
        <p>Paragraph 10. After a div.</p>
        <p>Paragraph 11. After a div.</p>
    </body>
    ">>,
    ?assertEqual(Expect, find_query(Query, Html)).

find_general_test() ->
    Expect = [
        {<<"p">>,#{},[<<"Paragraph 6.">>]},
        {<<"p">>,#{},[<<"Paragraph 7.">>]},
        {<<"p">>,#{},[<<"Paragraph 4.">>]},
        {<<"p">>,#{},[<<"Paragraph 5.">>]}
    ],
    Query = <<"div ~ p">>,
    Html = <<"
    <body>
        <h2>General Sibling Selector</h2>
        <p>The general sibling selector (~) selects all elements that are next siblings of a specified element.</p>
        <p>Paragraph 1.</p>
        <div>
            <p>Paragraph 2.</p>
            <div>
                <p>Paragraph 3.</p>
            </div>
            <p>Paragraph 4.</p>
            <p>Paragraph 5.</p>
        </div>
        <p>Paragraph 6.</p>
        <code>Some code.</code>
        <p>Paragraph 7.</p>
    </body>
    ">>,
    ?assertEqual(Expect, find_query(Query, Html)).

find_match_test() ->
    Query = <<"div#foo.bar.baz">>,
    Html = <<"
    <body>
        <div>
            <div id='foo' class='  bar    baz  '>
                <div>FOO</div>
            </div>
        </div>
    </body>
    ">>,
    ?assertMatch([
        {<<"div">>,#{
            <<"id">> := <<"foo">>,
            <<"class">> := [<<"bar">>,<<"baz">>]
        },[
            {<<"div">>,#{},[<<"FOO">>]}
        ]}
    ], find_query(Query, Html)).

-endif.
