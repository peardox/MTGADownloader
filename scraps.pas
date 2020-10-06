unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

procedure MapJsonSetObject(const Json: TJsonNode);
var
  Node: TJsonNode;
begin
  {$ifdef useprototype}
  MemoMessage('--------------- members ---------------');
  {$endif}
  for Node in Json do
    begin
      case Node.Name of
      {$ifdef useprototype}
      'dummyForPrototype':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError');
            end;
        end;
      {$else}
      'dummyForPrototype':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError');
            end;
        end;
      {$endif}
      else
          begin
            {$ifdef useprototype}
            MemoMessage(Chr(39) + Node.Name + Chr(39) + ':' + LineEnding +
            '  begin' + LineEnding +
            '    if not(Node.Kind = ' + Node.KindAsString + ') then' + LineEnding +
            '      begin' + LineEnding +
            '        MemoMessage(' + Chr(39) +  'TypeError for ' +
              Node.Name + ' expected '  + Node.KindAsString +
              ' got ' + Chr(39) + ' + Node.KindAsString' + ');' + LineEnding +
            '      end;' + LineEnding +
            '  end;');
            {$else}
            MemoMessage('Unhandled node : ' + Node.Name + ' - ' + Node.KindAsString);
            {$endif}
          end;
      end;
    end;
  {$ifdef useprototype}
  MemoMessage('--------------- members ---------------');
  {$endif}
end;

procedure MapJsonSetListObject(const Json: TJsonNode);
var
  Node: TJsonNode;
begin
//  MemoMessage(Json.AsJson);
  for Node in Json do
    begin
      case Node.Name of
      'baseSetSize':
        begin
          if not(Node.Kind = nkNumber) then
            begin
              MemoMessage('TypeError for baseSetSize expected nkNumber got ' + Node.KindAsString);
            end;
        end;
      'code':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError for code expected nkString got ' + Node.KindAsString);
            end;
        end;
      'name':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError for name expected nkString got ' + Node.KindAsString);
            end;
        end;
      'releaseDate':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError for releaseDate expected nkString got ' + Node.KindAsString);
            end;
        end;
      'totalSetSize':
        begin
          if not(Node.Kind = nkNumber) then
            begin
              MemoMessage('TypeError for totalSetSize expected nkNumber got ' + Node.KindAsString);
            end;
        end;
      'type':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError for type expected nkString got ' + Node.KindAsString);
            end;
        end;
      'parentCode':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError for parentCode expected nkString got ' + Node.KindAsString);
            end;
        end;
      else
          begin
            MemoMessage(Chr(39) + Node.Name + Chr(39) + ':' + LineEnding +
            '  begin' + LineEnding +
            '    if not(Node.Kind = ' + Node.KindAsString + ') then' + LineEnding +
            '      begin' + LineEnding +
            '        MemoMessage(' + Chr(39) +  'TypeError for ' +
              Node.Name + ' expected '  + Node.KindAsString +
              ' got ' + Chr(39) + ' + Node.KindAsString' + ');' + LineEnding +
            '      end;' + LineEnding +
            '  end;');
          end;
      end;
    end;
end;

procedure MapJsonDeckListObject(const Json: TJsonNode);
var
  Node: TJsonNode;
begin
  for Node in Json do
    begin
      case Node.Name of
      'code':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError for code expected nkString got ' + Node.KindAsString);
            end;
        end;
      'fileName':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError for fileName expected nkString got ' + Node.KindAsString);
            end;
        end;
      'name':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError for name expected nkString got ' + Node.KindAsString);
            end;
        end;
      'releaseDate':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError for releaseDate expected nkString got ' + Node.KindAsString);
            end;
        end;
      'type':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError for type expected nkString got ' + Node.KindAsString);
            end;
        end;
      else
          begin
            MemoMessage(Chr(39) + Node.Name + Chr(39) + ':' + LineEnding +
            '  begin' + LineEnding +
            '    if not(Node.Kind = ' + Node.KindAsString + ') then' + LineEnding +
            '      begin' + LineEnding +
            '        MemoMessage(' + Chr(39) +  'TypeError for ' +
              Node.Name + ' expected '  + Node.KindAsString +
              ' got ' + Chr(39) + ' + Node.KindAsString' + ');' + LineEnding +
            '      end;' + LineEnding +
            '  end;');
          end;
      end;
    end;
end;

procedure ProcessMTGListObject(const Json: TJsonNode);
var
  Node: TJsonNode;
  {$ifdef useprototype}
  First: Boolean = True;
  {$endif}
begin
  MemoMessage('ProcessMTGListObject');
  for Node in Json do
    begin
      {$ifdef useprototype}
      if First then
        begin
          MemoMessage('=>' + Node.Name + ' - ' + Node.KindAsString);
          MapJsonSetListObject(Node);
//          MapJsonDeckListObject(Node);
        end;
//        First := False;
      {$else}
        MapJsonSetListObject(Node);
      {$endif}
    end;
end;

procedure ProcessMTGJsonSetObject(const Json: TJsonNode);
var
  Node: TJsonNode;
  {$ifdef useprototype}
  First: Boolean = True;
  {$endif}
begin
  MemoMessage('Process MTGListObject');
  for Node in Json do
    begin
      {$ifdef useprototype}
      if First then
        begin
          MemoMessage('->' + Node.Name + ' - ' + Node.KindAsString);
          if Node.Name = 'cards' then
            begin
//              MapJsonSetObject(Node.AsArray);
            end;
        end;
//        First := False;
      {$else}
        MapJsonSetObject(Node);
      {$endif}
    end;
end;

procedure ProcessMTGJsonList(const Data: TStream; const Title: String);
var
  Json: TJsonNode;
  Node: TJsonNode;
begin
  MemoMessage(Title);
  Json := TJsonNode.Create;
  try
    try
      Json.LoadFromStream(Data);
      for Node in Json do
        begin
          MemoMessage(Node.Name + ' - ' + Node.KindAsString);
          if ((Node.Name = 'data') and (Node.Kind = nkArray)) then
            begin
              ProcessMTGListObject(Node);
            end;
        end;
    except
      on E : Exception do
        begin
          MemoMessage('Oops' + LineEnding +
                       E.ClassName + LineEnding +
                       E.Message);
         end;
    end;
  finally
    FreeAndNil(Json);
  end;
end;

procedure ProcessMTGJsonSet(const Data: TStream; const Title: String);
var
  Json: TJsonNode;
  Node: TJsonNode;
begin
  MemoMessage(Title);
  Json := TJsonNode.Create;
  try
    try
      Json.LoadFromStream(Data);
      for Node in Json do
        begin
          MemoMessage(Node.Name + ' - ' + Node.KindAsString);
          if ((Node.Name = 'data') and (Node.Kind = nkObject)) then
            begin
              ProcessMTGJsonSetObject(Node);
            end;
        end;
    except
      on E : Exception do
        begin
          MemoMessage('Oops' + LineEnding +
                       E.ClassName + LineEnding +
                       E.Message);
         end;
    end;
  finally
    FreeAndNil(Json);
  end;
end;


end.









{
  data := CacheData(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', UseCache);
  if not(data = nil) then
    begin
      ProcessMTGJsonList(data, 'Sets');
    end;
  FreeAndNil(data);

  data := CacheData(MTGJSON_DECKLIST_URI, 'mtgjson_decklist.json', UseCache);
  if not(data = nil) then
    begin
      ProcessMTGJsonList(data, 'Decks');
    end;
  FreeAndNil(data);
}   