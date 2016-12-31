-module(snarkutils).
-export([snarkReq/1,firstToken/1,snark_to_json/1,out/1,outJson/1,outXml/1,snark_to_xml/1,handle/2,method/1,snarkId/1]).
-include("/appvar/include/snark_includes.hrl").
-include("/usr/lib64/yaws/include/yaws_api.hrl"). 

% The "meat" of dealing with YAWS and transforming snark

     % If there's no path, return "all", otherwise take the first token and ignore the rest (for now)
    snarkReq(undefined) -> "all";
	snarkReq(Pathinfo) ->
		Tokens = string:tokens(Pathinfo,"/."),
		First = firstToken(Tokens),
		case string:to_integer(First) of
			%% Whatever, return a string
			{error,no_integer} -> 
					First;
			% Otherwise return an int
			{N,_} -> N
		end.
    % If there's no path, return "all", otherwise take the first token and ignore the rest (for now)
    firstToken([]) -> "all";
	firstToken([Token|_]) -> Token.

    snarkId(Pathinfo) ->
		snarkIdFromReq(snarkReq(Pathinfo)).

    snarkIdFromReq(Req) when is_integer(Req) ->
		Req;
    % ID is the primary key - so we want to error if it's not a valid key
    snarkIdFromReq(_) -> error.

    snarkType(undefined) -> "json"; %Default to JSON
    snarkType(Pathinfo) ->
        case string:tokens(Pathinfo,".") of 
         [_,T] -> T;
         [ ] -> "json";
         _ -> "json"
        end.

	snark_to_json(Snarks) ->  	
    	     Data = 
	     [{struct,
		[
         {id, integer_to_list(Snark#snark_jokes.jid)},
	     {title, Snark#snark_jokes.jlabel},
	      {jokeline,  Snark#snark_jokes.joke},
	      {punchline, Snark#snark_jokes.punchLine},
	      {author,    Snark#snark_jokes.author},
	      {addedby,   Snark#snark_jokes.addedby},
	      {dateAdded, httpd_util:rfc1123_date(Snark#snark_jokes.dateAdded)}
		]} || Snark <- Snarks],
        json2:encode(Data).
    
    snark_to_xml(Snarks) ->
            	     Data = 
	     [{joke,
		[
         {id, [integer_to_list(Snark#snark_jokes.jid)]},
         {title, [Snark#snark_jokes.jlabel]},
          {jokeline,  [Snark#snark_jokes.joke]},
          {punchline, [Snark#snark_jokes.punchLine]},
          {author,    [Snark#snark_jokes.author]},
          {addedby,   [Snark#snark_jokes.addedby]},
          {dateAdded, [httpd_util:rfc1123_date(Snark#snark_jokes.dateAdded)]}
		]} || Snark <- Snarks],
       lists:flatten(xmerl:export_simple([{jokes,Data}], xmerl_xml)).
    
	json_to_snark(Arg) ->
		{ok,{array,[Data]}} = json2:decode_string(binary_to_list(Arg#arg.clidata)),
		{{ok,Title},{ok,Joke},{ok,Punchline}} = 
		    {json2:obj_find("title",Data), 
			json2:obj_find("jokeline",Data), 
			json2:obj_find("punchline",Data)},
        Author = case json2:obj_find("author", Data) of 
			{ok,Auth} -> Auth;
			error -> "unknown"
			end,
        AddedBy = case json2:obj_find("addedby", Data) of 
			{ok,Added} -> Added;
			error -> "unknown"
			end,
        {Title,Joke,Punchline,Author,AddedBy}.

    xml_to_snark(_Arg) ->
		{error, unsupported_parser}.

	method(Arg) ->
		Rec = Arg#arg.req,
		Rec#http_request.method.

	path(Arg) ->
		Arg#arg.pathinfo.

	out(Arg) ->
		handle(method(Arg),Arg).

    handle('GET', Arg) ->
        case snarkType(path(Arg)) of
            "xml" -> outXml(Arg);
            "json" -> outJson(Arg);
             _ -> outJson(Arg)
        end;

    handle('POST', Arg) ->
		{Title,Joke,Punchline,Author,AddedBy} = 
			case snarkType(path(Arg)) of
				"xml" -> xml_to_snark(Arg);
				"json" -> json_to_snark(Arg);
				_ -> json_to_snark(Arg)
			end,
        snarkapp:putsnark(Title,Joke,Punchline,Author,AddedBy),
		[{status,201},{ehtml,yaws_api:f("Title: ~s<br/>Joke: ~s<br/>Punchline: ~s<br/>Author: ~s<br/>AddedBy:~s<br/>",[Title,Joke,Punchline,Author,AddedBy])}];

    handle('DELETE', Arg) ->
		Id = snarkutils:snarkId(path(Arg)),
		snarkapp:delsnark(Id),
		[{status,204}];
    handle('PUT', Arg) ->
		Id = snarkutils:snarkId(path(Arg)),
		{Title,Joke,Punchline,Author,AddedBy} = 
			case snarkType(path(Arg)) of
				"xml" -> xml_to_snark(Arg);
				"json" -> json_to_snark(Arg);
				_ -> json_to_snark(Arg)
			end,
		snarkapp:updatesnark(Id,{Title,Joke,Punchline,Author,AddedBy}),
		[{status,201},{ehtml,yaws_api:f("Title: ~s<br/>Joke: ~s<br/>Punchline: ~s<br/>Author: ~s<br/>AddedBy:~s<br/>",[Title,Joke,Punchline,Author,AddedBy])}].

    outJson(Arg) ->
		SnarkReq = snarkutils:snarkReq(path(Arg)),
		Snark = snarkapp:getsnark(SnarkReq),
		if 
			length(Snark)>0 ->
				Json = snarkutils:snark_to_json(Snark),
				{content, "application/json", Json};
            true ->
				[{status,404}]
        end.
    outXml(Arg) ->
        SnarkReq = snarkutils:snarkReq(path(Arg)),
	    Snark = snarkapp:getsnark(SnarkReq),
		if
			 length(Snark)>0 ->
				Xml = snarkutils:snark_to_xml(Snark),
       		 	{content, "application/xml", Xml};
			true ->
				[{status,404}]
		end.
        

    
    