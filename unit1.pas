unit Unit1;

{$mode objfpc}{$H+}

// {$define wotc_cards_only}
{$define usedecknet}
 {$define useprototype}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, JsonTools, TypInfo,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  CastleDownload, CastleParameters, CastleClassUtils, CastleURIUtils,
  CastleControl, CastleTimeUtils, CastleLog, CastleFilesUtils
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public

  end;

var
  Form1: TForm1;

const
  SCRYFALL_SETS_URI = 'https://api.scryfall.com/sets';
  SCRYFALL_SYMBOLOGY_URI = 'https://api.scryfall.com/symbology';
  {$ifndef usedecknet}
  MTGJSON_ENUMS_URI = 'https://mtgjson.com/api/v5/EnumValues.json.gz';
  MTGJSON_SETLIST_URI = 'https://mtgjson.com/api/v5/SetList.json.gz';
  MTGJSON_DECKLIST_URI = 'https://mtgjson.com/api/v5/DeckList.json.gz';
  {$else}
  MTGJSON_ENUMS_URI = 'https://decknet.co.uk/api/v5/EnumValues.json.gz';
  MTGJSON_SETLIST_URI = 'https://decknet.co.uk/api/v5/SetList.json.gz';
  MTGJSON_DECKLIST_URI = 'https://decknet.co.uk/api/v5/DeckList.json.gz';
  {$endif}

  UseCache = True; // Only set to True while developing

procedure MemoMessage(const msg: String);
function DownloadNetworkFile(const URI: String; const sOptions: TStreamOptions = []; const UsingCache: Boolean = False): TStream;
function CacheData(const URI: String; const FileName: String; const LoadFromCache: Boolean = False; FreeResult: Boolean = False): TStream;
function LoadCachedData(const FileName: String): TStream;
function URIDirectoryExists(const Path: String): Boolean;
function URIDirectoryCreate(const Url: String): Boolean;
function URIExcludeQuery(const Url: String): String;
function CreateCastleDataDirectoryIfMissing(const SubDir: String): Boolean;

implementation

{$R *.lfm}

function JSONKindToString(Node: TJsonNode): string;
begin
  result := GetEnumName(TypeInfo(TJsonNodeKind), ord(Node.&Kind));
end;

procedure MemoMessage(const msg: String);
begin
    Form1.Memo1.Lines.Add(msg);
    Application.ProcessMessages;
end;

function URIExcludeQuery(const Url: String): String;
var
  pos: Integer;
begin
  pos := Url.IndexOf('?');
  if pos > 0 then
    Result := Url.Remove(pos)
  else
    Result := Url;
end;

function URIDirectoryCreate(const Url: String): Boolean;
var
  F: String;
begin
  F := URIToFilenameSafe(Url);
  if F = '' then
    raise Exception.CreateFmt('URL "%s" is not a "file" URL, cannot create directories yet', [URIDisplay(Url)])
  else
    Result := CreateDir(F);
end;

function URIDirectoryExists(const Path: String): Boolean;
begin
  Result := URIExists(Path) in [ueDirectory];
end;

function CreateCastleDataDirectoryIfMissing(const SubDir: String): Boolean;
begin
  Result := False;

  if URIDirectoryExists('castle-data:/' + SubDir) then
    Result := True
  else
    if URIDirectoryCreate('castle-data:/' + SubDir) then
      Result := True;
end;

{
function DownloadNetworkFileSynchronous(const URI: String; const sOptions: TStreamOptions = []; const UsingCache: Boolean = False): TStream;
begin

  Download[I] := TCastleDownload.Create(Self);
  Download[I].Url := Urls[I];
  Download[I].OnFinish := @DownloadFinish;
  Download[I].Options := [soForceMemoryStream];
  Download[I].Start;
end;
}

function DownloadNetworkFile(const URI: String; const sOptions: TStreamOptions = []; const UsingCache: Boolean = False): TStream;
var
  stream: TStream;
  {$ifdef growstream}
  strOutput: TMemoryStream;
  {$endif}
begin
  {$ifdef growstream}
  strOutput:= TMemoryStream.Create;
  {$endif}
  try
    try
      stream := Download(URI, sOptions);
      {$ifdef growstream}
      ReadGrowingStream(stream, strOutput, false);
      {$endif}
    except
        on E : Exception do
          begin
            if not UsingCache then
              begin
                ShowMessage('Oops' + LineEnding +
                             E.ClassName + LineEnding +
                             E.Message);
                end;
              {$ifdef growstream}
              FreeAndNil(stream);
              strOutput := nil;
              {$else}
              stream := nil;
              {$endif}
           end;
      end;
  finally
    {$ifdef growstream}
    FreeAndNil(stream);
    Result := strOutput;
    {$else}
    Result := stream;
    {$endif}
  end;
end;


function getScryfallSymbols: TStream;
var
  data: TStream;
begin
  try
    data := DownloadNetworkFile(SCRYFALL_SYMBOLOGY_URI);
  finally
  end;

  Result := data;
end;

function LoadCachedData(const FileName: String): TStream;
var
  data: TStream = nil;
begin
  data := DownloadNetworkFile('castle-data:/' + FileName, [soForceMemoryStream], True);
  if not(data = nil) then
    begin
      MemoMessage('Loaded data from ' + URIToFilenameSafe('castle-data:/' + FileName));
    end
  else
    begin
      MemoMessage('No cached data!!!');
    end;

  Result := data;
end;

function CacheData(const URI: String; const FileName: String; const LoadFromCache: Boolean = False; FreeResult: Boolean = False): TStream;
var
  data: TStream = nil;
begin
  if LoadFromCache then
    begin
      if URIFileExists('castle-data:/' + FileName) then
        begin
          data := LoadCachedData(FileName);
        end
      else
        begin
          data := DownloadNetworkFile(URI, [soGzip, soForceMemoryStream]);
          if not(data = nil) then
            begin
              MemoMessage('Saving to data' + PathDelim + FileName);
              StreamSaveToFile(data, 'castle-data:/' + FileName);
            end;
        end;
    end
  else
    begin
      data := DownloadNetworkFile(URI, [soGzip, soForceMemoryStream]);
      if not(data = nil) then
        begin
          MemoMessage('Saving to data' + PathDelim + FileName);
          StreamSaveToFile(data, 'castle-data:/' + FileName);
        end
      else
        begin
          data := LoadCachedData(FileName);
        end;
    end;

  if FreeResult then
    FreeAndNil(data);

  Result := data;
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

procedure process_scryfall_array(const Json: TJsonNode);
var
  Node: TJsonNode;
  {$ifdef useprototype}
  First: Boolean = True;
  {$endif}
begin
  for Node in Json do
    begin
      {$ifdef useprototype}
      if First then
        begin
        MemoMessage(Node.Name + ' - ' + JSONKindToString(Node));
        process_scryfall_array_elements(Node);
        end;
      First := False;
      {$else}
      process_scryfall_array_elements(Node);
      {$endif}
    end;
end;

procedure process_scryfall(const Data: TStream; const FetchField: String);
var
  Json: TJsonNode;
  Node: TJsonNode;
begin
  MemoMessage(FetchField);
  Json := TJsonNode.Create;
  try
    try
      Json.LoadFromStream(Data);
      for Node in Json do
        begin
          MemoMessage(Node.Name + ' - ' + JSONKindToString(Node));
          if ((Node.Name = 'data') and (Node.Kind = nkArray)) then
            begin
              process_scryfall_array(Node);
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

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeLog;
  EnableBlockingDownloads := True;
  Memo1.Clear;
  Form1.Width := 800;
  Form1.Height := 600;
  Form1.Position:= poScreenCenter;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  URLList: TStringList;
  data: TStream = nil;
  ticks: Int64;
  i: Integer;
  iconFilename: String;
begin
  ticks := CastleGetTickCount64;
  Memo1.Clear;
  Button1.Enabled := False;

  MemoMessage('---------- SCRYFALL ----------');
  if CreateCastleDataDirectoryIfMissing('scryfall') then
    if CreateCastleDataDirectoryIfMissing('scryfall/sets/') then
      if CreateCastleDataDirectoryIfMissing('scryfall/sets/icons') then
        if CreateCastleDataDirectoryIfMissing('scryfall/sets/icons/svg') then
          begin
            data := CacheData(SCRYFALL_SETS_URI, 'scryfall_sets.json', UseCache);
            URLList := ProcessScryfall(data, 'icon_svg_uri');
            if not(URLList = nil) then
              begin
                for i := 0 to URLList.Count - 1 do
                  begin
                    iconFilename := URIExcludeQuery(ExtractURIName(URLList.Strings[i]));
                    if not(URIFileExists('castle-data:/' + 'scryfall/sets/icons/svg/' + iconFilename)) then
                      CacheData(URLList.Strings[i], 'scryfall/sets/icons/svg/' + iconFilename, UseCache, True);
                  end;
              end;
            FreeAndNil(URLList);
            FreeAndNil(data);
          end
        else
          MemoMessage('Cant create scryfall/sets/icons/svg')
      else
        MemoMessage('Cant create scryfall/sets/icons')
    else
      MemoMessage('Cant create scryfall/sets')
  else
    MemoMessage('Cant create scryfall');

  if CreateCastleDataDirectoryIfMissing('scryfall') then
    if CreateCastleDataDirectoryIfMissing('scryfall/symbols/') then
      if CreateCastleDataDirectoryIfMissing('scryfall/symbols/icons') then
        if CreateCastleDataDirectoryIfMissing('scryfall/symbols/icons/svg') then
          begin
            data := CacheData(SCRYFALL_SYMBOLOGY_URI, 'scryfall_symbology.json', UseCache);
            URLList := ProcessScryfall(data, 'svg_uri');
            if not(URLList = nil) then
              begin
                for i := 0 to URLList.Count - 1 do
                  begin
                    iconFilename := URIExcludeQuery(ExtractURIName(URLList.Strings[i]));
                    if not(URIFileExists('castle-data:/' + 'scryfall/symbols/icons/svg/' + iconFilename)) then
                      CacheData(URLList.Strings[i], 'scryfall/symbols/icons/svg/' + iconFilename, UseCache, True);
                  end;
              end;
            FreeAndNil(URLList);
            FreeAndNil(data);
          end
        else
          MemoMessage('Cant create scryfall/symbols/icons/svg')
      else
        MemoMessage('Cant create scryfall/symbols/icons')
    else
      MemoMessage('Cant create scryfall/symbols')
  else
    MemoMessage('Cant create scryfall');

  MemoMessage('---------- MTGJSON ----------');
  data := CacheData(MTGJSON_ENUMS_URI, 'mtgjson_enums.json', UseCache);
  FreeAndNil(data);

  data := CacheData(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', UseCache);
  FreeAndNil(data);

  data := CacheData(MTGJSON_DECKLIST_URI, 'mtgjson_decklist.json', UseCache);
  FreeAndNil(data);

  MemoMessage('----------- END -----------');
  ticks := CastleGetTickCount64 - ticks;
  MemoMessage('Time : ' + IntToStr(ticks) + 'ms');
  Button1.Enabled := True;
end;

end.

