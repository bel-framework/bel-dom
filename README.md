# bel-framework/bel-dom

DOM (Document Object Model) API for Erlang.

## HTML

### Scanner and Parser

#### Example

```erlang
1> String = <<"
   <!DOCTYPE html>
   <html lang=\"en\">
   <!-- Comment -->
   <head>
       <title><b>content inside <title> must be treated as plaintext</b></title>
       <script src=\"assets/foo.js\"></script>
       <style>
           :root {
               --foo: 0;
           }
       </style>
   </head>
   <body>
       <h1>Form</h1>
       <br>
       <br/>
       <form>
           <div>Foo Form</div>
           <input id=\"foo\" class=' foo   bar   ' name='foo' value='\"b\ar\"' />
           <input type=\"number\" value=10 />
       </form>
   </body>
   </html>
   ">>.

2> Tokens = bel_dom:scan_document(String).
[{open,{{2,4},undefined,undefined},
       {<<"!DOCTYPE">>,[{<<"html">>,{true,{2,4}}}]}},
 {open,{{3,4},undefined,undefined},
       {<<"html">>,[{<<"lang">>,{<<"en">>,{3,4}}}]}},
 {comment,{{4,4},undefined,undefined},<<" Comment ">>},
 {open,{{5,4},undefined,undefined},{<<"head">>,[]}},
 {open,{{6,8},undefined,undefined},{<<"title">>,[]}},
 {text,{{6,15},undefined,undefined},
       <<"<b>content inside <title> must be treated as plaintext</b>">>},
 {close,{{6,73},undefined,undefined},<<"title">>},
 {open,{{7,8},undefined,undefined},
       {<<"script">>,[{<<"src">>,{<<"assets/foo.js">>,{7,8}}}]}},
 {close,{{7,36},undefined,undefined},<<"script">>},
 {open,{{8,8},undefined,undefined},{<<"style">>,[]}},
 {text,{{8,15},undefined,undefined},
       <<":root {\n               --foo: 0;\n           }">>},
 {close,{{12,8},undefined,undefined},<<"style">>},
 {close,{{13,4},undefined,undefined},<<"head">>},
 {open,{{14,4},undefined,undefined},{<<"body">>,[]}},
 {open,{{15,8},undefined,undefined},{<<"h1">>,[]}},
 {text,{{15,12},undefined,undefined},<<"Form">>},
 {close,{{15,16},undefined,undefined},<<"h1">>},
 {void,{{16,8},undefined,undefined},{<<"br">>,[]}},
 {void,{{17,8},undefined,undefined},{<<"br">>,[]}},
 {open,{{18,8},undefined,undefined},{<<"form">>,[]}},
 {open,{{19,12},undefined,undefined},{<<"div">>,[]}},
 {text,{{19,17},undefined,undefined},<<"Foo Form">>},
 {close,{{19,25},undefined,undefined},<<"div">>},
 {void,{{20,12},undefined,undefined},
       {<<"input">>,
        [{<<"id">>,{<<"foo">>,{20,12}}},
         {<<"class">>,{[<<"foo">>,<<"bar">>],{20,21}}},
         {<<"name">>,{<<"foo">>,{20,43}}},
         {<<"value">>,{<<"\"bar\"">>,{20,54}}}]}},
 {void,{{21,12},undefined,undefined},
       {<<"input">>,
        [{<<"type">>,{<<"number">>,{21,12}}},
         {<<"value">>,{<<"10">>,{21,26}}}]}},
 {close,{{22,8},undefined,undefined},<<"form">>},
 {close,{{23,4},undefined,undefined},<<"body">>},
 {close,{{24,4},undefined,undefined},<<"html">>}]

3> bel_dom:parse_document(Tokens).
[{<<"!DOCTYPE">>,
  #{<<"html">> => true},
  [{<<"html">>,
    #{<<"lang">> => <<"en">>},
    [{<<"head">>,#{},
      [{<<"title">>,#{},
        [<<"<b>content inside <title> must be treated as plaintext</b>">>]},
       {<<"script">>,#{<<"src">> => <<"assets/foo.js">>},[]},
       {<<"style">>,#{},
        [<<":root {\n               --foo: 0;\n           }">>]}]},
     {<<"body">>,#{},
      [{<<"h1">>,#{},[<<"Form">>]},
       {<<"br">>,#{},[]},
       {<<"br">>,#{},[]},
       {<<"form">>,#{},
        [{<<"div">>,#{},[<<"Foo Form">>]},
         {<<"input">>,
          #{<<"class">> => [<<"foo">>,<<"bar">>],
            <<"id">> => <<"foo">>,<<"name">> => <<"foo">>,
            <<"value">> => <<"\"bar\"">>},
          []},
         {<<"input">>,
          #{<<"type">> => <<"number">>,<<"value">> => <<"10">>},
          []}]}]}]}]}]
```

## CSS

### Scanner and Parser

#### Example

```erlang
1> Query = <<"#foo > .bar + div.k1.k2 [id='baz']:hello(2):not(:where(div))::before, #bar + .baz.fizz div.buzz">>.

2> {ok, Tokens, _} = bel_dom:scan_query(Query).
{ok,[{hash,{1,1},<<"foo">>},
     {greater,{1,5}},
     {space,{1,7}},
     {'.',{1,8}},
     {ident,{1,9},<<"bar">>},
     {plus,{1,12}},
     {space,{1,14}},
     {ident,{1,15},<<"div">>},
     {'.',{1,18}},
     {ident,{1,19},<<"k1">>},
     {'.',{1,21}},
     {ident,{1,22},<<"k2">>},
     {space,{1,24}},
     {'[',{1,25}},
     {ident,{1,26},<<"id">>},
     {'=',{1,28}},
     {string,{1,29},<<"baz">>},
     {']',{1,34}},
     {':',{1,35}},
     {function,{1,36},<<"hello">>},
     {number,{1,42},<<"2">>},
     {')',{1,43}},
     {'not',{1,44}},
     {':',{1,49}},
     {function,{1,50},<<"where">>},
     {ident,{1,56},<<"div">>},
     {')',{1,59}},
     {')',{1,60}},
     {':',{1,61}},
     {':',{1,62}},
     {ident,{1,63},<<"before">>},
     {comma,{1,69}},
     {space,{1,70}},
     {hash,{1,71},<<"bar">>},
     {plus,{1,75}},
     {space,{1,77}},
     {'.',{1,78}},
     {ident,{1,79},<<"baz">>},
     {'.',{1,82}},
     {ident,{1,83},<<"fizz">>},
     {space,{1,87}},
     {ident,{1,88},<<"div">>},
     {'.',{1,91}},
     {ident,{1,92},<<"buzz">>}],
    1}

3> bel_dom:parse_query(Tokens).
{ok,[{greater,
         {[{id,<<"foo">>}],
          {plus,
              {[{class,<<"bar">>}],
               {space,
                   {[{type,{undefined,<<"div">>}},
                     {class,<<"k1">>},
                     {class,<<"k2">>}],
                    [{attrib,{undefined,<<"id">>,{'=',{string,<<"baz">>}}}},
                     {pseudo_class,{function,{<<"hello">>,[{number,<<"2">>}]}}},
                     {negation,
                         {pseudo_class,
                             {function,{<<"where">>,[{ident,<<"div">>}]}}}},
                     {pseudo_element,{ident,<<"before">>}}]}}}}}},
     {plus,
         {[{id,<<"bar">>}],
          {space,
              {[{class,<<"baz">>},{class,<<"fizz">>}],
               [{type,{undefined,<<"div">>}},{class,<<"buzz">>}]}}}}]}
```

## DOM

### Query

#### Example

```erlang
1> bel_dom:query_selector(<<"#foo.bar">>, <<"<div><p id='foo' class='bar'>bel-framework</p></div>">>).
{<<"p">>,
 #{<<"class">> => [<<"bar">>],<<"id">> => <<"foo">>},
 [<<"bel-framework">>]}
```

## TODO

- [ ] Specs
- [ ] Doc
- [ ] Improve tests
- [ ] Fix query issues and implement all missing patterns:
  - [ ] {attrib, {NamespacePrefix, Ident, Match}}
  - [ ] {pseudo_class, {ident, Ident} | {'function, {Name, Exprs}}}
  - [ ] {pseudo_element, {ident, Ident} | {'function, {Name, Exprs}}}
  - [ ] {negation, Arg}
  - [ ] {universal, NamespacePrefix}

## Build

```shell
$ rebar3 compile
```
