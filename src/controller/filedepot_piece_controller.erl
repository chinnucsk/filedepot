-module(filedepot_piece_controller, [Req]).
-compile(export_all).

show('GET', [Id]) ->
	Piece = boss_db:find(Id),
	io:format("Piece: ~p~n", [Piece:attributes()]),
	ContentType = filedepot_utility:get_content_type(Piece:name()),
	{output, Piece:data(), [{"Content-Type", ContentType}]}.

upload('POST', []) ->
	io:format("POST piece upload~n"),
	io:format("Req: ~p~n", [Req:post_files()]),
	
	{uploaded_file, Name, TempPath, Size} = hd(Req:post_files()),
	% File = [{name, Name}, {tempPath, TempPath}, {size, Size}],
	% io:format("Files: ~p~n", [File]),
	{json, [{tempPath,TempPath}]}.


