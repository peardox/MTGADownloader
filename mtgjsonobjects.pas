unit MTGJsonObjects;

{$mode objfpc}{$H+}
// {$define useprototype}

interface

uses
  Classes, SysUtils;

implementation
{
procedure process_scryfall_array_elements(const Json: TJsonNode);
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
            '    if not(Node.Kind = ' + JSONKindToString(Node) + ') then' + LineEnding +
            '      begin' + LineEnding +
            '        MemoMessage(' + Chr(39) +  'TypeError for ' +
              Node.Name + ' expected '  + JSONKindToString(Node) +
              ' got ' + Chr(39) + ' + JSONKindToString(Node)' + ');' + LineEnding +
            '      end;' + LineEnding +
            '  end;');
            {$else}
            MemoMessage('Unhandled node : ' + Node.Name + ' - ' + JSONKindToString(Node));
            {$endif}
          end;
      end;
    end;
  {$ifdef useprototype}
  MemoMessage('--------------- members ---------------');
  {$endif}
end;

procedure ProcessMTGObject(const Json: TJsonNode);
var
  Node: TJsonNode;
  {$ifdef useprototype}
  First: Boolean = True;
  {$endif}
begin
  MemoMessage('ProcessMTGObject');
  for Node in Json do
    begin
      {$ifdef useprototype}
      if First then
        begin
          MemoMessage('->' + Node.Name + ' - ' + JSONKindToString(Node));
//        process_scryfall_array_elements(Node);
        end;
//      First := False;
      {$else}
      process_scryfall_array_elements(Node);
      {$endif}
    end;
end;

procedure ProcessMTGJson(const Data: TStream; const Title: String);
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
          MemoMessage(Node.Name + ' - ' + JSONKindToString(Node));
          if ((Node.Name = 'data') and (Node.Kind = nkObject)) then
            begin
              ProcessMTGObject(Node);
            end;
        end;
    except
      on E : Exception do
        begin
          ShowMessage('Oops' + LineEnding +
                       E.ClassName + LineEnding +
                       E.Message);
         end;
    end;
  finally
    FreeAndNil(Json);
  end;
end;


}

end.

