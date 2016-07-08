-module(lib_trigrams).

%% API
-export([for_each_trigram_in_the_english_language/2,
  make_tables/0,
  how_many_trigrams/0,
  make_ets_set/0, make_ets_ordered_set/0, make_mod_set/0
]).
-import(lists, [reverse/1]).

make_tables() ->
  {Micro1, N} = timer:tc(?MODULE, how_many_trigrams, []),
  io:format("Counting - No of trigrams=~p time/trigram=~p~n",[N,Micro1/N]),
  {Micro2, Ntri} = timer:tc(?MODULE, make_ets_ordered_set, []),
  FileSize1 = filelib:file_size("trigramsOS.tab"),
  io:format("Ets ordered Set size=~p time/trigram=~p~n",[FileSize1/Ntri,
    Micro2/N]),
  {Micro3, _} = timer:tc(?MODULE, make_ets_set, []),
  FileSize2 = filelib:file_size("trigramsS.tab"),
  io:format("Ets set size=~p time/trigram=~p~n",[FileSize2/Ntri, Micro3/N]),
  {Micro4, _} = timer:tc(?MODULE, make_mod_set, []),
  FileSize3 = filelib:file_size("trigrams.set"),
  io:format("Module sets size=~p time/trigram=~p~n",[FileSize3/Ntri, Micro4/N]).

%% This function applies the fun F to every trigram in the English language.
%% F is a fun of type
%%      fun(Str, A) -> A,
%%  Str ranges over all trigrams in the language, and A is an accumulator.
for_each_trigram_in_the_english_language(F, A0) ->
  {ok, Bin0} = file:read_file("354984si.ngl.gz"),
  Bin = zlib:gunzip(Bin0),
  scan_word_list(binary_to_list(Bin), F, A0).

scan_word_list([], _, A) ->
  A;
scan_word_list(L, F, A) ->
  {Word, L1} = get_next_word(L, []),
  A1 = scan_trigrams([$\s | Word], F, A),
  scan_word_list(L1, F, A1).

%% scan the word looking for \r\n
%% the second argument is the word (reversed) so it
%% has to be reversed when we find \r\n or run out of characters
get_next_word([$\r,$\n|T], L) -> {reverse([$\s|L]), T};
get_next_word([H|T], L)       -> get_next_word(T, [H|L]);
get_next_word([], L)          -> {reverse([$\s|L]), []}.

scan_trigrams([X,Y,Z], F, A) ->
  F([X,Y,Z], A);
scan_trigrams([X,Y,Z|T], F, A) ->
  A1 = F([X,Y,Z], A),
  scan_trigrams([Y,Z|T], F, A1);
scan_trigrams(_, _, A) ->
  A.

%% Build the Tables
make_ets_ordered_set() -> make_a_set(ordered_set, "trigramsOS.tab").
make_ets_set()         -> make_a_set(set, "trigramsS.tab").

make_a_set(Type, FileName) ->
  Tab = ets:new(table, [Type]),
  F = fun(Str, _) -> ets:insert(Tab, {list_to_binary(Str)}) end,
  for_each_trigram_in_the_english_language(F, 0),
  ets:tab2file(Tab, FileName),
  Size = ets:info(Tab, size),
  ets:delete(Tab),
  Size.

%%
make_mod_set() ->
  D = sets:new(),
  F = fun(Str, Set) -> sets:add_element(list_to_binary(Str), Set) end,
  D1 = for_each_trigram_in_the_english_language(F, D),
  file:write_file("trigrams.set", [term_to_binary(D1)]).

how_many_trigrams() ->
  F = fun(_, N) -> 1 + N  end,
  for_each_trigram_in_the_english_language(F, 0).
