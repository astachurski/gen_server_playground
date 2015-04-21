%%%-------------------------------------------------------------------
%%% @author adrian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2015 9:09 PM
%%%-------------------------------------------------------------------
-module(fileop_logic_test).
-author("adrian").

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").


-define(TESTBINFILE, "src/test.exe").
-define(TESTBINFILE_DEST, "src/test_dest.exe").
-define(TESTTXTFILE, "src/ipsum.txt").


simple_test() ->
  ?assert(true).


file_exists_test() ->
  ?assertEqual(fileop_logic:file_exists(?TESTBINFILE), true).

file_doesnt_exists_test() ->
  ?assertEqual(fileop_logic:file_exists("JUNK"), false).

chunk_dechunk_round_trip1_test() ->
  Res = fileop_logic:get_chunked_file_ser(?TESTBINFILE),
  %?debugFmt("\n~p\n", [Res]),

  fileop_logic:assemble_file_from_chunks(Res, ?TESTBINFILE_DEST),
  {ok, #file_info{size = SourceSize}} = file:read_file_info(?TESTBINFILE),
  {ok, #file_info{size = DestSize}} = file:read_file_info(?TESTBINFILE_DEST),
  ?debugFmt(" source size: ~p\n", [SourceSize]),
  ?debugFmt(" dest size: ~p\n", [DestSize]),
  ?assertEqual(SourceSize, DestSize).

generate_chunk_id_test() ->
  Id = fileop_logic:generate_chunk_id(<<"srakaptaka">>),
  <<HashVal:16/binary, _RandVal:8/binary>> = Id,
  <<FirstThree:3/binary, _Rest:(16 - 3)/binary>> = HashVal,
  ?assertEqual(<<171, 152, 247>>, FirstThree).

calculate_optimal_chunk_size2_test() ->
  %33409
  Res = fileop_logic:calculate_chunk_size(?TESTBINFILE),
  ?assertEqual(Res, 5000).


calculate_optimal_chunk_size1_test() ->
  %4477
  Res = fileop_logic:calculate_chunk_size(?TESTTXTFILE),
  %?debugFmt("\nchunk size calculated :~p\n", [Res]),
  ?assertEqual(Res, 2500).



get_first_middle_test() ->
  ChunkSize = 50,
  {ok, Chunk1} = fileop_logic:get_middle_chunk(?TESTTXTFILE, ChunkSize),
  %?debugFmt("\nChunk got :~p\n", [Chunk1]),
  ?assertEqual(Chunk1, <<"procure\r\nhim some great pleasure. To take a trivia">>).

get_first_chunk_test() ->
  ChunkSize = 50,
  {ok, Chunk1} = fileop_logic:get_first_chunk(?TESTTXTFILE, ChunkSize),
  ?assertEqual(Chunk1, <<"The standard Lorem Ipsum passage, used since the 1">>).

get_last_chunk_test() ->
  ChunkSize = 50,
  {ok, Chunk1} = fileop_logic:get_last_chunk(?TESTTXTFILE, ChunkSize),
  ?assertEqual(<<"pains to avoid worse pains.">>, Chunk1).

%check if n-th chunk has proper content and length equal chunksize
get_nth_chunk_check_length_content_test() ->
  ChunkSize = 10,
  {ok, Chunk1} = fileop_logic:get_nth_chunk_3(?TESTTXTFILE, 2, ChunkSize),
  %?assertEqual(length(binary_to_list(Chunk1)), ChunkSize),
  ?assertEqual(<<"rd Lorem I">>, Chunk1).


%get two consecutive chunks and check if combine correctly.
gent_nth_chunk_two_consecutive_test() ->
  %get chunks count
  ChunkSize = 10,
  {ok, Chunk1} = fileop_logic:get_nth_chunk_3(?TESTTXTFILE, 4, ChunkSize),
  %?debugFmt("\nChunk got :~p\n", [Chunk1]),
  ?assertEqual(<<"ge, used s">>, Chunk1),
  {ok, Chunk2} = fileop_logic:get_nth_chunk_3(?TESTTXTFILE, 5, ChunkSize),
  ?assertEqual(<<"ince the 1">>, Chunk2).
%?debugFmt("\nChunk got :~p\n", [Chunk2]).

%check against expeced number of chunks and size of remainder
calculateChunks1_test() ->
  %33409
  ChunkSize = 100,
  Res = fileop_logic:calculate_chunks_count_and_rem(?TESTBINFILE, ChunkSize),
  ?assertEqual({ok, 334, 9}, Res).

calculateChunks2_test() ->
  ChunkSize = 1,
  Res = fileop_logic:calculate_chunks_count_and_rem(?TESTBINFILE, ChunkSize),
  ?assertEqual({ok, 33409, 0}, Res).

calculateChunks3_test() ->
  ChunkSize = 50000,
  Res = fileop_logic:calculate_chunks_count_and_rem(?TESTBINFILE, ChunkSize),
  ?assertEqual({error_chunktoobig}, Res).

calculateChunks4_test() ->
  ChunkSize = 33409,
  Res = fileop_logic:calculate_chunks_count_and_rem(?TESTBINFILE, ChunkSize),
  ?assertEqual({ok, 1, 0}, Res).
