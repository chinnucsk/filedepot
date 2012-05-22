-module(filedepot_piece_controller, [Req]).
-compile(export_all).

show('GET', [Id]) ->
	Piece = boss_db:find(Id),
	io:format("Piece: ~p~n", [Piece:attributes()]),
	ContentType = filedepot_utility:get_content_type(Piece:name()),
	{output, Piece:data(), [{"Content-Type", ContentType}]}.

upload('POST', []) ->
	io:format("POST piece upload~n"),	
	%% In the Filedepot case, we're only uploading one file at a time
	{uploaded_file, Name, TempPath, Size} = hd(Req:post_files()),	
	{json, [{tempPath,TempPath}]}.


