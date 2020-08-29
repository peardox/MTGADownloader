unit AssetGatherers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SVGUtils;

procedure GetScryfallIcons(URI: String; Path: String; SaveAs: String; FetchField: String);
function GetMTGJsonEnumList(URI: String; SaveAs: String): TStringList;
procedure DumpList(EnumList: TStringList);

implementation

uses
  Unit1, CacheFileUtils,
  Dialogs,
  JsonTools, TypInfo,
  CastleTimeUtils, CastleURIUtils, CastleFilesUtils, CastleClassUtils
  ;

function JSONKindToString(Node: TJsonNode): string;
begin
  result := GetEnumName(TypeInfo(TJsonNodeKind), ord(Node.&Kind));
end;

function FetchScryfallSvgUri(const Json: TJsonNode; const FetchField: String): String;
var
  Node: TJsonNode;
begin
  Result := EmptyStr;

  for Node in Json do
    begin
      if Node.Name = FetchField then
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage(FetchField + ' is not a String');
            end
          else
            begin
              Result := Node.AsString;
            end;
        end;
    end;
end;

function ProcessScryfallData(const Json: TJsonNode; const FetchField: String): TStringList;
var
  Node: TJsonNode;
  URLList: TStringList;
begin
  URLList := TStringList.Create;
  for Node in Json do
    begin
      URLList.Add(FetchScryfallSvgUri(Node, FetchField));
    end;
  Result := URLList;
end;

function ProcessScryfall(const Data: TStream; const FetchField: String): TStringList;
var
  Json: TJsonNode;
  Node: TJsonNode;
begin
  Result := nil;

  MemoMessage(FetchField);
  Json := TJsonNode.Create;
  try
    try
      Json.LoadFromStream(Data);
      for Node in Json do
        begin
          if ((Node.Name = 'data') and (Node.Kind = nkArray)) then
            begin
              Result := ProcessScryfallData(Node, FetchField);
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

function ProcessMTGArray(const Json: TJsonNode): TStringList;
var
  Node: TJsonNode;
  SList: TStringList;
begin
  SList := TStringList.Create;
  SList.Sorted := True;

  for Node in Json do
    begin
      if (Node.Kind = nkString) then // Should always be true
        SList.Add(Node.AsString)
      else
        MemoMessage('-----> Type Error <-----');
    end;
  Result := SList;
end;

function ProcessMTGObject(const Json: TJsonNode): TStringList;
var
  Node: TJsonNode;
  ChildNode: TJsonNode;
  SList: TStringList;
  ChildList: TStringList;
begin
  SList := TStringList.Create;
  SList.Sorted := True;
  SList.OwnsObjects := True;

  for Node in Json do
    begin
      if (Node.Kind = nkObject) then // Should always be true
        begin
          ChildList := TStringList.Create;
          ChildList.Sorted := True;
          ChildList.OwnsObjects := True;

          for ChildNode in Node do
            begin
              if (ChildNode.Kind = nkArray) then // Should always be true
                begin
                  ChildList.AddObject(ChildNode.Name, ProcessMTGArray(ChildNode));
                end;
            end;
          SList.AddObject(Node.Name, ChildList);
        end;
    end;
  Result := Slist;
end;

function ProcessMTGJson(const Data: TStream): TStringList;
var
  Json: TJsonNode;
  Node: TJsonNode;
begin
  Result := nil;

  Json := TJsonNode.Create;
  try
    try
      Json.LoadFromStream(Data);
      for Node in Json do
        begin
          if ((Node.Name = 'data') and (Node.Kind = nkObject)) then
            begin
              Result := ProcessMTGObject(Node);
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

function GetMTGJsonEnumList(URI: String; SaveAs: String): TStringList;
var
  EnumList: TStringList = nil;
  data: TStream;
begin
  data := CacheData(URI, SaveAs, True);
  if not(data = nil) then
    begin
     EnumList := ProcessMTGJson(data);
    end;
  FreeAndNil(data);

  Result := EnumList;
end;

procedure GetScryfallIcons(URI: String; Path: String; SaveAs: String; FetchField: String);
var
  URLList: TStringList;
  data: TStream = nil;
  i: Integer;
  iconFilename: String;
  svg: TStream;
begin
  if CreateCastleDataDirectoryIfMissing(Path) then
    begin
      CreateCastleDataDirectoryIfMissing(Path + '/svg');
      CreateCastleDataDirectoryIfMissing(Path + '/png');
      data := CacheData(URI, SaveAs, True);
      if not(data = nil) then
        begin
          URLList := ProcessScryfall(data, FetchField);
          if not(URLList = nil) then
            begin
              for i := 0 to URLList.Count - 1 do
                begin
                  iconFilename := URIExcludeQuery(ExtractURIName(URLList.Strings[i]));
                  if iconFilename = 'con.svg' then
                    iconFilename := 'conflux.svg';
                  svg := CacheData(URLList.Strings[i], Path + '/svg/' + iconFilename, True, False);
                  if not(svg = nil) then
                    begin
//                  if not(URIFileExists('castle-data:/' + Path + '/png/' + iconFilename + '.png')) then
                      RasterizeSVG(URIToFilenameSafe('castle-data:/' + Path + '/png/' + iconFilename + '.png'), 512, svg);
                    FreeAndNil(svg);
                    end
                  else
                    MemoMessage('***********' + URLList.Strings[i] + '***********');
                end;
            end;
          FreeAndNil(URLList);
          FreeAndNil(data);
        end;
    end
  else
    MemoMessage(Path);
end;

procedure DumpList(EnumList: TStringList);
var
  i, j, k: Integer;
  sl1, sl2: TStringList;
  v: String;
begin
  for i := 0 to EnumList.Count -1 do
    begin
      MemoMessage(EnumList[i]);
      sl1 := EnumList.Objects[i] as TStringList;
      for j := 0 to sl1.Count -1 do
        begin
          MemoMessage('  ' + sl1[j]);
          sl2 := sl1.Objects[j] as TStringList;
          v := '';
          for k := 0 to sl2.Count -1 do
            begin
              if k > 0 then
                v += ', ';
              v += sl2[k];
            end;
          MemoMessage('    ' + v);
        end;
    end;
end;

end.

