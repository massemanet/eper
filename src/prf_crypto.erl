%% -*- erlang-indent-level: 2 -*-
%%% Created :  7 Aug 2008 by Mats Cronqvist <masse@kreditor.se>

%% @doc
%% @end

-module(prf_crypto).
-author('Mats Cronqvist').
-export([encrypt/1,encrypt/2,decrypt/1,decrypt/2]).

-ifdef(USE_CRYPTO_HASH).
-define(CRYPTO_HASH(Data), crypto:hash(md5, Data)).
-else.
-define(CRYPTO_HASH(Data), crypto:md5(Data)).
-endif.

-ifdef(USE_CRYPTO_BLOCK_CRYPT).
-define(CRYPTO_BLOCK_ENCRYPT(Key, IVec, Text),
  crypto:block_encrypt(des_cbc, Key, IVec, Text)).
-define(CRYPTO_BLOCK_DECRYPT(Key, IVec, Text),
  crypto:block_decrypt(des_cbc, Key, IVec, Text)).
-else.
-define(CRYPTO_BLOCK_ENCRYPT(Key, IVec, Text),
  crypto:des_cbc_encrypt(Key, IVec, Text)).
-define(CRYPTO_BLOCK_DECRYPT(Key, IVec, Text),
  crypto:des_cbc_decrypt(Key, IVec, Text)).
-endif.

phrase() -> atom_to_list(erlang:get_cookie()).

encrypt(Data) -> encrypt(phrase(),Data).

encrypt(Phrase,Data) ->
  assert_crypto(),
  {Key,Ivec} = make_key(Phrase),
  ?CRYPTO_BLOCK_ENCRYPT(Key, Ivec, pad(Data)).

decrypt(Data) -> decrypt(phrase(),Data).

decrypt(Phrase,Data) ->
  assert_crypto(),
  {Key,Ivec} = make_key(Phrase),
  unpad(?CRYPTO_BLOCK_DECRYPT(Key, Ivec, Data)).

make_key(Phrase) ->
  <<Key:8/binary,Ivec:8/binary>> = ?CRYPTO_HASH(Phrase),
  {Key,Ivec}.

pad(Term) ->
  Bin = term_to_binary(Term),
  BinSize = size(Bin),
  PadSize = 7-((BinSize+3) rem 8),
  PadBits = 8*PadSize,
  <<BinSize:32,Bin/binary,0:PadBits>>.

unpad(<<BinSize:32,R/binary>>) ->
  <<Bin:BinSize/binary,_/binary>> = R,
  binary_to_term(Bin).

assert_crypto() ->
  case whereis(crypto) of
    undefined -> crypto:start();
    _ -> ok
  end.
