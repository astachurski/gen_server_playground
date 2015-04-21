%%%-------------------------------------------------------------------
%%% @author adrian stachurski
%%% @copyright (C) 2015, <DUPA>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2015 9:54 AM
%%%-------------------------------------------------------------------
-module(filetests).
-author("adrian").
-import(file, [open/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([]).

-define(TESTBINFILE, "src/test.exe").
-define(TESTBINFILE_DEST, "src/test_dest.exe").
-define(TESTTXTFILE, "src/ipsum.txt").


%get file content split into chunkCount equal parts plus remainder part.
%Result: list of binary chunks in form [<<binaryChunk1>>,..,<<binaryChunkCount>>, <<binaryChunkRes>>]
%function does sequential/recursive split of file with multiple open/close operations in between.
get_chunked_file_ser(Fname) ->
  ChunkSize = calculate_chunk_size(Fname),
  {ok, Count, Rem} = calculate_chunks_count_and_rem(Fname, ChunkSize),
  R = get_chunk_list(Fname, ChunkSize, Count, []),
  if Rem > 0 ->
    lists:append(R, [element(2, get_last_chunk(Fname, ChunkSize))])
  end.

get_chunk_list(_Fname, _ChunkSize, 0, Acc) -> Acc;

get_chunk_list(Fname, ChunkSize, Count, Acc) ->
  {ok, NthChunk} = get_nth_chunk_3(Fname, Count, ChunkSize),
  get_chunk_list(Fname, ChunkSize, Count - 1, [NthChunk | Acc]).

%%try to open/close to check if file exists.
%%todo: find better method to check file existence
file_exists(Fname) ->
  case file:open(Fname, read) of
    {ok, Device} -> file:close(Device), true;
    {error, _Reason} -> false
  end.

%%glue together all binary chunks and write to file overwriting
%%previously generated file of the same name.
assemble_file_from_chunks(BinaryChunkList, OutputFName) ->

  case file_exists(OutputFName) of
    true -> file:delete(OutputFName);
    _Otherwise -> nothing
  end,

  {ok, Device} = file:open(OutputFName, [append, binary]),
  lists:foreach(fun(Bytes) -> file:write(Device, Bytes) end, BinaryChunkList),
  file:close(Device).



chunk_dechunk_round_trip1_test() ->
  Res = get_chunked_file_ser(?TESTBINFILE),
  %?debugFmt("\n~p\n", [Res]),

  assemble_file_from_chunks(Res, ?TESTBINFILE_DEST),
  {ok, #file_info{size = SourceSize}} = file:read_file_info(?TESTBINFILE),
  {ok, #file_info{size = DestSize}} = file:read_file_info(?TESTBINFILE_DEST),
  ?debugFmt(" source size: ~p\n", [SourceSize]),
  ?debugFmt(" dest size: ~p\n", [DestSize]),
  ?assertEqual(SourceSize, DestSize).


%========================= low level binary files manipulation API ====================
%========================= I/O file access required !!! ===============================

%ChunkNo is 1-based
%returns binary data - nth(1 based) chunk of a file using ChunkSize as split size.
%call to this function Opens, Reads and Closes the file Fname.
get_nth_chunk_3(Fname, ChunkNo, ChunkSize) ->
  %?debugVal(ChunkNo),
  try file:open(Fname, [read, binary]) of
    {ok, Device} ->
      PosToReadFrom = (ChunkNo - 1) * ChunkSize,
      {ok, DataRead} = read_device_from_position_to_chunklist(Device, PosToReadFrom, ChunkSize),
      {file:close(Device), DataRead}
  catch
    _:X -> {catchederror, file_read_error, X}
  end.

%helper function, obtaining file size iteratively every time (filelib:file_size)
%MAY be costly.
%todo: check if there is any filesize penalty
get_nth_chunk_2(Fname, ChunkNo) ->
  ChunkSize = calculate_chunk_size(Fname),
  get_nth_chunk_3(Fname, ChunkNo, ChunkSize).

%helper function, account for number of chunks which is 0-based
get_last_chunk(Fname, ChunkSize) ->
  {ok, ChunksNo, _} = calculate_chunks_count_and_rem(Fname, ChunkSize),
  get_nth_chunk_3(Fname, ChunksNo + 1, ChunkSize).

%get first binary chunk.
get_first_chunk(Fname, ChunkSize) ->
  {ok, ChunksNo, _} = calculate_chunks_count_and_rem(Fname, ChunkSize),
  get_nth_chunk_3(Fname, 1, ChunkSize).

%gets chunk which beginning is located in the middle of binary file.
%This is rounded DOWN to the nearst lower chunk number.
get_middle_chunk(Fname, ChunkSize) ->
  {ok, ChunksNo, _} = calculate_chunks_count_and_rem(Fname, ChunkSize),
  get_nth_chunk_3(Fname, ChunksNo div 2, ChunkSize).

%given, desired offset in bytes from beginning of file and chunk size - returns
%binary content.
read_device_from_position_to_chunklist(IODevice, Position, ChunkSize) ->
  case file:position(IODevice, {bof, Position}) of
    {ok, Position} -> file:read(IODevice, ChunkSize);
    X -> {strasznyerror, {X, Position, ChunkSize}}
  end.

%how many chunks of given size there are in file and how many bytes remains
%if file doesn't split to integer value?, result: {ok, Count, Remainder}
calculate_chunks_count_and_rem(Fname, ChunkSize) ->
  Fsize = filelib:file_size(Fname),
  case ChunkSize =< Fsize of
    true -> {ok, Fsize div ChunkSize, Fsize rem ChunkSize};
    _Other -> {error_chunktoobig}
  end.

normalize_chunks_count(ChunksCountRaw, Remainder) ->
  case Remainder of
    0 -> ChunksCountRaw;
    _Other -> ChunksCountRaw + 1
  end.


%%based on size, choose chunk size from lookup table.
%%divisions and filesizes are 10 based for simplicity (base is not relevant).
%%fine tune chunk sizes separately for given file sizes.
calculate_chunk_size(Fname) ->
  Fsize = filelib:file_size(Fname),
  %tuple: file size, chunk size
  FchunkSizes = [{10, 2}, {100, 50}, {1000, 250}, {10000, 2500}, {100000, 5000}, {1000000, 100000}],
  case lists:filter(fun({Size, _B}) -> Size >= Fsize end, FchunkSizes) of
    [H | _] -> element(2, H);
    [] -> 1000000
  end.

%========================= chunk identification API ====================
%========================= no I/O file access ==========================

%for a binary chunk generate binary ID : <<16 bytes md4 hash, 8 random bytes>>
generate_chunk_id(BinaryChunk) ->
  %16 bytes - content sensitive id
  KeyPart1_16b = crypto:hash(md4, BinaryChunk),
  %in rare cases where chunks ar not unique use random value.
  %this can handle impractically HUGE (2^64)number of non-unique chunks.
  KeyPart2_8b = crypto:rand_bytes(8),
  <<KeyPart1_16b/binary, KeyPart2_8b/binary>>.


%============================================= TESTS ==============================
%decompose result and just check if first 3 bytes match. We don't
%check against random value.

generate_chunk_id_test() ->
  Id = generate_chunk_id(<<"srakaptaka">>),
  <<HashVal:16/binary, _RandVal:8/binary>> = Id,
  <<FirstThree:3/binary, _Rest:(16 - 3)/binary>> = HashVal,
  ?assertEqual(<<171, 152, 247>>, FirstThree).

calculate_optimal_chunk_size2_test() ->
  %33409
  Res = calculate_chunk_size(?TESTBINFILE),
  ?assertEqual(Res, 5000).


calculate_optimal_chunk_size1_test() ->
  %4477
  Res = calculate_chunk_size(?TESTTXTFILE),
  %?debugFmt("\nchunk size calculated :~p\n", [Res]),
  ?assertEqual(Res, 2500).



get_first_middle_test() ->
  ChunkSize = 50,
  {ok, Chunk1} = get_middle_chunk(?TESTTXTFILE, ChunkSize),
  %?debugFmt("\nChunk got :~p\n", [Chunk1]),
  ?assertEqual(Chunk1, <<"procure\r\nhim some great pleasure. To take a trivia">>).

get_first_chunk_test() ->
  ChunkSize = 50,
  {ok, Chunk1} = get_first_chunk(?TESTTXTFILE, ChunkSize),
  ?assertEqual(Chunk1, <<"The standard Lorem Ipsum passage, used since the 1">>).

get_last_chunk_test() ->
  ChunkSize = 50,
  {ok, Chunk1} = get_last_chunk(?TESTTXTFILE, ChunkSize),
  ?assertEqual(<<"pains to avoid worse pains.">>, Chunk1).

%check if n-th chunk has proper content and length equal chunksize
get_nth_chunk_check_length_content_test() ->
  ChunkSize = 10,
  {ok, Chunk1} = get_nth_chunk_3(?TESTTXTFILE, 2, ChunkSize),
  %?assertEqual(length(binary_to_list(Chunk1)), ChunkSize),
  ?assertEqual(<<"rd Lorem I">>, Chunk1).


%get two consecutive chunks and check if combine correctly.
gent_nth_chunk_two_consecutive_test() ->
  %get chunks count
  ChunkSize = 10,
  {ok, Chunk1} = get_nth_chunk_3(?TESTTXTFILE, 4, ChunkSize),
  %?debugFmt("\nChunk got :~p\n", [Chunk1]),
  ?assertEqual(<<"ge, used s">>, Chunk1),
  {ok, Chunk2} = get_nth_chunk_3(?TESTTXTFILE, 5, ChunkSize),
  ?assertEqual(<<"ince the 1">>, Chunk2).
%?debugFmt("\nChunk got :~p\n", [Chunk2]).

%check against expeced number of chunks and size of remainder
calculateChunks1_test() ->
  %33409
  ChunkSize = 100,
  Res = calculate_chunks_count_and_rem(?TESTBINFILE, ChunkSize),
  ?assertEqual({ok, 334, 9}, Res).

calculateChunks2_test() ->
  ChunkSize = 1,
  Res = calculate_chunks_count_and_rem(?TESTBINFILE, ChunkSize),
  ?assertEqual({ok, 33409, 0}, Res).

calculateChunks3_test() ->
  ChunkSize = 50000,
  Res = calculate_chunks_count_and_rem(?TESTBINFILE, ChunkSize),
  ?assertEqual({error_chunktoobig}, Res).

calculateChunks4_test() ->
  ChunkSize = 33409,
  Res = calculate_chunks_count_and_rem(?TESTBINFILE, ChunkSize),
  ?assertEqual({ok, 1, 0}, Res).

file_exists_test() ->
  ?assertEqual(file_exists(?TESTBINFILE), true).

file_doesnt_exists_test() ->
  ?assertEqual(file_exists("JUNK"), false).