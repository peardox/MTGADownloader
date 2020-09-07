unit Unit1;

{$mode objfpc}{$H+}
// {$define usedecknet}

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
    Button2: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Abort: Boolean;
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
  MTGJSON_M21_URI = 'https://mtgjson.com/api/v5/M21.json.gz';

  UseCache = True; // Only set to True while developing

procedure MemoMessage(const msg: String);

implementation

uses CacheFileUtils, AssetGatherers, MTGJsonObjects;

{$R *.lfm}

procedure MemoMessage(const msg: String);
begin
    Form1.Memo1.Lines.Add(msg);
    Application.ProcessMessages;
end;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Abort := False;
  Button2.Enabled := False;
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
  MTGSet: TMTGSet;
  MTGSetList: TMTGSetList;
  MTGDeckList: TMTGDeckList;
  idx: Integer;
  cnt: Integer;
  cards: Integer;
  setDate: String;
begin
  Abort := False;
  Button2.Enabled := True;
  cnt := 0;
  cards := 0;
  ticks := CastleGetTickCount64;
  Memo1.Clear;
  Button1.Enabled := False;

{
  MemoMessage('---------- SCRYFALL ----------');

  GetScryfallIcons(SCRYFALL_SETS_URI, 'scryfall/sets/icons', 'scryfall_sets.json', 'icon_svg_uri');
  GetScryfallIcons(SCRYFALL_SYMBOLOGY_URI, 'scryfall/symbols/icons', 'scryfall_symbology.json', 'svg_uri');

  MemoMessage('---------- MTGJSON ----------');

  EnumList := GetMTGJsonEnumList(MTGJSON_ENUMS_URI, 'mtgjson_enums.json');
}

{
  MTGSet := TMTGSet.Create(MTGJSON_M21_URI, 'mtgjson/sets/json/set_M21.json', 'code');
  MTGSet.DumpList;
  FreeAndNil(MTGSet);
}

  MTGSetList := TMTGSetList.Create(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', 'code');
  for idx := 0 to MTGSetList.List.Count -1 do
    begin
      setDate := TSetListRecord(MTGSetList.List.Objects[idx]).setReleaseDate;
//      if MTGSetList.List[idx] = 'ZNR' then
//      if setDate > '2019-01-01' then
        begin
          data := GetMTGJsonSetJson(MTGSetList.List[idx], 'mtgjson/sets/json');
//          MTGSetList.Dump(idx);
          MTGSet := TMTGSet.Create(data, 'uuid');
          cards += MTGSet.setTotalSetSize;
//          MTGSet.DumpList;
          Inc(cnt);
          FreeAndNil(MTGSet);
          FreeAndNil(data);
          Application.ProcessMessages;
        end;
      if Abort then
        begin
          break;
        end;
    end;
//  MTGSet.DumpList;
  FreeAndNil(MTGSetList);

{
  MTGDeckList := TMTGDeckList.Create(MTGJSON_DECKLIST_URI, 'mtgjson_decklist.json', 'fileName');
  for idx := 0 to MTGDeckList.List.Count -1 do
    begin
//      MemoMessage('Key: ' + MTGDeckList.List[idx]);
      data := GetMTGJsonDeckJson(MTGDeckList.List[idx], 'mtgjson/decks/json');
      FreeAndNil(data);
    end;
//  MTGDeckList.DumpList;
  FreeAndNil(MTGDeckList);
}
  MemoMessage('----------- END -----------');
  ticks := CastleGetTickCount64 - ticks;
  MemoMessage('Time : ' + IntToStr(ticks) + 'ms');
  Button1.Enabled := True;
  MemoMessage('Sets analysed : ' + IntToStr(cnt));
  MemoMessage('Sets cards : ' + IntToStr(cards));
{
  Memo1.Lines.BeginUpdate;
  DumpList(EnumList);
  Memo1.Lines.EndUpdate;
}
  FreeAndNil(EnumList);
  Button2.Enabled := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Abort := True;
  Button2.Enabled := False;
  Button1.Enabled := True;
end;

end.

