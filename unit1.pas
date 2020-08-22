unit Unit1;

{$mode objfpc}{$H+}

// {$define wotc_cards_only}
{$define usedecknet}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  CastleDownload, CastleParameters, CastleClassUtils,
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

procedure MemoMessage(msg: String);
procedure StreamToFile(const URL: string; Stream: TStream);
function DownloadNetworkFile(URI: String; sOptions: TStreamOptions = []; UsingCache: Boolean = False): TStream;
function CacheData(const URI: String; const FileName: String; const LoadFromCache: Boolean = False): TStream;
function LoadCachedData(const FileName: String): TStream;

implementation

{$R *.lfm}

procedure StreamToFile(const URL: string; Stream: TStream);
begin
  StreamSaveToFile(Stream, URL);
end;

procedure MemoMessage(msg: String);
begin
    Form1.Memo1.Lines.Add(msg);
    Application.ProcessMessages;
end;

function DownloadNetworkFile(URI: String; sOptions: TStreamOptions = []; UsingCache: Boolean = False): TStream;
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
      MemoMessage('Loaded data from ' + PathDelim + FileName);
    end
  else
    begin
      MemoMessage('No cached data!!!');
    end;

  Result := data;
end;

function CacheData(const URI: String; const FileName: String; const LoadFromCache: Boolean = False): TStream;
var
  data: TStream = nil;
begin
  if LoadFromCache then
    begin
      data := LoadCachedData(FileName);
      // Cache miss - let's go get it again
      if data = nil then
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

  Result := data;
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
  data: TStream = nil;
  ticks: Int64;
begin
  ticks := CastleGetTickCount64;
  Memo1.Clear;
  Button1.Enabled := False;

  MemoMessage('---------- SCRYFALL ----------');
  data := CacheData(SCRYFALL_SETS_URI, 'scryfall_sets.json', UseCache);
  FreeAndNil(data);

  data := CacheData(SCRYFALL_SYMBOLOGY_URI, 'scryfall_symbology.json', UseCache);
  FreeAndNil(data);

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

