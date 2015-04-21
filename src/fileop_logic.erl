%%%-------------------------------------------------------------------
%%% @author adrian stachurski
%%% @copyright (C) 2015, <DUPA>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2015 9:54 AM
%%%-------------------------------------------------------------------
-module(fileop_logic).
-author("adrian").
-import(file, [open/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% API
%-export([get_filename_list/0, file_exists/1]).
-compile(export_all).


%========================= low level files manipulation API ====================
%========================= I/O file access required !!! ===============================

get_filename_list() ->
  {ok, Dir} = file:get_cwd(),
  {ok, FileNamesList} = file:list_dir(Dir),
  FileNamesList.


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
%todo: check if there is any file size penalty
get_nth_chunk_2(Fname, ChunkNo) ->
  ChunkSize = calculate_chunk_size(Fname),
  get_nth_chunk_3(Fname, ChunkNo, ChunkSize).

%helper function, account for number of chunks which is 0-based
get_last_chunk(Fname, ChunkSize) ->
  {ok, ChunksNo, _} = calculate_chunks_count_and_rem(Fname, ChunkSize),
  get_nth_chunk_3(Fname, ChunksNo + 1, ChunkSize).

%get first binary chunk.
get_first_chunk(Fname, ChunkSize) ->
  {ok, _ChunksNo, _} = calculate_chunks_count_and_rem(Fname, ChunkSize),
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

%normalize_chunks_count(ChunksCountRaw, Remainder) ->
%  case Remainder of
%    0 -> ChunksCountRaw;
%    _Other -> ChunksCountRaw + 1
%  end.


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


%==================================================================================
%============================================= TESTS ==============================
%decompose result and just check if first 3 bytes match. We don't
%check against random value.
%============================================= TESTS ==============================
%==================================================================================



