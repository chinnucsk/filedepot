-module(filedepot_start_controller, [Req]).
-compile(export_all).


index('GET', []) ->
	%% Fetch all pieces (files)
	Pieces = boss_db:find(piece, []),	
	% io:format("Pieces: ~p~n", [Pieces]),
	%% Fetch all tags to allow filtering on pieces
	Tags = hd(boss_db:find(taglist, [])),
	io:format("start:index | Tags: ~p~n", [Tags]),
	{ok, [{pieces, Pieces}, {tags, Tags:taglist()}]};


index('GET', [Tag]) ->
	%% Fetch all pieces (files)
	Pieces = boss_db:find(piece, []),	
	io:format("Pieces: ~p~n", [Pieces]),
	Filt = fun(X) -> lists:member(Tag, X:tags()) end,
	FilteredPieces = lists:filter(Filt, Pieces),
	%% Fetch all tags to allow filtering on pieces
	Tags = hd(boss_db:find(taglist, [])),
	io:format("start:index | Tags: ~p~n", [Tags]),
	{ok, [
				{pieces, FilteredPieces}, 
				{tags, Tags:taglist()},
				{filtertag, Tag}
			]
	}.


% boss_db:find(greeting, [{id, 'equals', "greeting-1"}]).
