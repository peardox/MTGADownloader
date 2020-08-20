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
  SCRYFALL_SYMBOL_URI = 'https://api.scryfall.com/symbology';
  {$ifndef usedecknet}
  MTGJSON_ENUMS_URI = 'https://mtgjson.com/api/v5/EnumValues.json.gz';
  MTGJSON_SETLIST_URI = 'https://mtgjson.com/api/v5/SetList.json.gz';
  MTGJSON_DECKLIST_URI = 'https://mtgjson.com/api/v5/DeckList.json.gz';
  {$else}
  MTGJSON_ENUMS_URI = 'https://decknet.co.uk/api/v5/EnumValues.json.gz';
  MTGJSON_SETLIST_URI = 'https://decknet.co.uk/api/v5/SetList.json.gz';
  MTGJSON_DECKLIST_URI = 'https://decknet.co.uk/api/v5/DeckList.json.gz';
  {$endif}

procedure MemoMessage(msg: String);
function DownloadNetworkFile(URI: String; sOptions: TStreamOptions = []): TStream;
function DownloadNetworkString(URI: String; sOptions: TStreamOptions = []): String;

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

function DownloadNetworkString(URI: String; sOptions: TStreamOptions = []): String;
var
  stream: TStream;
  strOutput: TStringStream;
begin
  Result := EmptyStr;

  try
    strOutput:= TStringStream.Create;
    try
      stream := Download(URI, sOptions);
      ReadGrowingStream(stream, strOutput, false);
    except
        on E : Exception do
          begin
          ShowMessage('Error : ' + E.Message);
          stream := nil;
          FreeAndNil(strOutput);
          end;
    end;
  finally
    if strOutput <> nil then
      begin
        Result := StreamToString(strOutput);
        FreeAndNil(strOutput);
      end;
    if stream <> nil then
      begin
        FreeAndNil(stream);
      end;
  end;
end;

function DownloadNetworkFile(URI: String; sOptions: TStreamOptions = []): TStream;
var
  stream: TStream;
  {$ifdef growstream}
  strOutput: TStringStream;
  {$endif}
begin
  {$ifdef growstream}
  strOutput:= TStringStream.Create;
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
          ShowMessage('Oops' + LineEnding + LineEnding + E.Message);
          Result := nil;
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


function cache_file(Asset: String; Uri: String; sOptions: TStreamOptions = []): TStream;
var
  data: TStream;
begin
  Result := nil;

  data := DownloadNetworkFile(Uri, sOptions);
  if not(data = nil) then
    begin
      StreamToFile('castle-data:/' + Asset + '.json', data);
      Result := data;
    end;

end;

function getScryfallSymbols: TStream;
var
  data: TStream;
begin
  try
    data := DownloadNetworkFile(SCRYFALL_SYMBOL_URI);
  finally
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

  MemoMessage('---------- MTGJSON ----------');
  data := DownloadNetworkFile(MTGJSON_ENUMS_URI, [soGzip, soForceMemoryStream]);
  MemoMessage('Saving to data' + PathDelim + 'mtgjson_enums.json');
  StreamToFile('castle-data:/' + 'mtgjson_enums.json', data);

  data := DownloadNetworkFile(MTGJSON_SETLIST_URI, [soGzip, soForceMemoryStream]);
  MemoMessage('Saving to data' + PathDelim + 'mtgjson_setlist.json');
  StreamToFile('castle-data:/' + 'mtgjson_setlist.json', data);

  data := DownloadNetworkFile(MTGJSON_DECKLIST_URI, [soGzip, soForceMemoryStream]);
  MemoMessage('Saving to data' + PathDelim + 'mtgjson_decklist.json');
  StreamToFile('castle-data:/' + 'mtgjson_decklist.json', data);

  ticks := CastleGetTickCount64 - ticks;
  MemoMessage('----------- END -----------');
  MemoMessage('Time : ' + IntToStr(ticks) + 'ms');
  Button1.Enabled := True;
end;

end.

