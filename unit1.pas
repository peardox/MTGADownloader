unit Unit1;

{$mode objfpc}{$H+}
{$define usedecknet}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, SVGUtils,
  CastleParameters, CastleClassUtils,
  CastleControl, CastleLog, CastleTimeUtils, CastleURIUtils, CastleFilesUtils
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

implementation

uses CacheFileUtils, AssetGatherers;

{$R *.lfm}

procedure MemoMessage(const msg: String);
begin
    Form1.Memo1.Lines.Add(msg);
    Application.ProcessMessages;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeLog;
  Memo1.Clear;
  Form1.Width := 800;
  Form1.Height := 600;
  Form1.Position:= poScreenCenter;
  MemoMessage('ApplicationConfig = ' + ApplicationConfig(''));
  MemoMessage('ApplicationData = ' + ApplicationData(''));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  data: TStream = nil;
  ticks: Int64;
  EnumList: TStringList;
begin
  ticks := CastleGetTickCount64;
  Memo1.Clear;
  Button1.Enabled := False;

  MemoMessage('---------- SCRYFALL ----------');

  GetScryfallIcons(SCRYFALL_SETS_URI, 'scryfall/sets/icons', 'scryfall_sets.json', 'icon_svg_uri');
  GetScryfallIcons(SCRYFALL_SYMBOLOGY_URI, 'scryfall/symbols/icons', 'scryfall_symbology.json', 'svg_uri');

  MemoMessage('---------- MTGJSON ----------');

  EnumList := GetMTGJsonEnumList(MTGJSON_ENUMS_URI, 'mtgjson_enums.json');

  data := CacheData(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', UseCache);
  if not(data = nil) then
    begin
//     ProcessMTGJson(data, 'Sets');
    end;
  FreeAndNil(data);

  data := CacheData(MTGJSON_DECKLIST_URI, 'mtgjson_decklist.json', UseCache);
  if not(data = nil) then
    begin
//     ProcessMTGJson(data, 'Decks');
    end;
  FreeAndNil(data);

  MemoMessage('----------- END -----------');
  ticks := CastleGetTickCount64 - ticks;
  MemoMessage('Time : ' + IntToStr(ticks) + 'ms');
  Button1.Enabled := True;

  Memo1.Lines.BeginUpdate;
  DumpList(EnumList);
  Memo1.Lines.EndUpdate;

  FreeAndNil(EnumList);
end;

end.

