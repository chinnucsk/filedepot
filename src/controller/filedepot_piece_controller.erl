-module(filedepot_piece_controller, [Req]).
-compile(export_all).

show('GET', [Id]) ->
	Piece = boss_db:find(Id),
	% io:format("Piece: ~p~n", [Piece:attributes()]),
	ContentType = filedepot_utility:get_content_type(Piece:name()),
	{output, Piece:data(), [{"Content-Type", ContentType}]}.

upload('POST', []) ->
	%% In the Filedepot case, we're only uploading one file at a time
	{uploaded_file, Name, TempPath, Size} = hd(Req:post_files()),
	%% Store this file as a new Piece
	{ok, Data} = file:read_file(TempPath),
	Piece = piece:new(id, Name, Data, erlang:now(), erlang:now(), []),
	%% Save this new Piece
	Piece:save(),
	%% Delete the temporary file
	file:delete(TempPath),
	%% TODO: Add support for error return too
	Message = lists:concat([Name," ", "saved successfully (", integer_to_list(Size), " bytes)"]),
	{json, [{status,"ok"}, {message, Message}]}.


