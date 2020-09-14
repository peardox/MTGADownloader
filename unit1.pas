unit Unit1;

{$mode objfpc}{$H+}
// {$define usedecknet}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  {$IFDEF UNIX}
  // cthreads,
  {$ENDIF}
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
  public
  end;

var
  Form1: TForm1;
  Abort: Boolean;

const
  SCRYFALL_SETS_URI = 'https://api.scryfall.com/sets';
  SCRYFALL_SYMBOLOGY_URI = 'https://api.scryfall.com/symbology';
  {$ifndef usedecknet}
  MTGJSON_ENUMS_URI = 'https://mtgjson.com/api/v5/EnumValues.json.gz';
  MTGJSON_SETLIST_URI = 'https://mtgjson.com/api/v5/SetList.json.gz';
  MTGJSON_PRICELIST_URI = 'https://mtgjson.com/api/v5/AllPrices.json.gz';
  MTGJSON_DECKLIST_URI = 'https://mtgjson.com/api/v5/DeckList.json.gz';
  {$else}
  MTGJSON_ENUMS_URI = 'https://decknet.co.uk/api/v5/EnumValues.json.gz';
  MTGJSON_SETLIST_URI = 'https://decknet.co.uk/api/v5/SetList.json.gz';
  MTGJSON_SETLIST_URI_400 = 'https://decknet.co.uk/HTTPErrorCode/400';
  MTGJSON_SETLIST_URI_401 = 'https://decknet.co.uk/HTTPErrorCode/401';
  MTGJSON_SETLIST_URI_402 = 'https://decknet.co.uk/HTTPErrorCode/402';
  MTGJSON_SETLIST_URI_403 = 'https://decknet.co.uk/HTTPErrorCode/403';
  MTGJSON_SETLIST_URI_404 = 'https://decknet.co.uk/HTTPErrorCode/404';
  MTGJSON_SETLIST_URI_405 = 'https://decknet.co.uk/HTTPErrorCode/405';
  MTGJSON_SETLIST_URI_500 = 'https://decknet.co.uk/HTTPErrorCode/500';
  MTGJSON_DECKLIST_URI = 'https://decknet.co.uk/api/v5/DeckList.json.gz';
  {$endif}
  MTGJSON_M21_URI = 'https://mtgjson.com/api/v5/M21.json.gz';
  MTGSON_PRICES_URI = 'https://api.peardox.co.uk/prices/prices.json.gz';

  cardQuality = 'large'; // normal / large

  UseCache = True; // Only set to True while developing

{ Declare hooks for other units }

procedure EnableAbortButton(Status: Boolean);
procedure MemoMessage(const msg: String);
procedure TriggerProcessMessages;

implementation

uses CacheFileUtils, AssetGatherers, MTGJsonObjects;

{$R *.lfm}

{ Hooks for other units }

procedure TriggerProcessMessages;
begin
  Application.ProcessMessages;
end;

procedure EnableAbortButton(Status: Boolean);
begin
  Form1.Button2.Enabled := Status;
end;

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
  SList: TStringList;
  MTGSet: TMTGSet;
  MTGSetList: TMTGSetList;
  MTGDeckList: TMTGDeckList;
  idx: Integer;
  img: Integer;
  cnt: Integer;
  cards: Integer;
  setDate: String;
  imgMTGJsonID: String;
  imgScryID:  String;
  imgURI: String;
  imgPath: String;
  imgFile: String;
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

  SList := GetMTGJsonEnumList(MTGJSON_ENUMS_URI, 'mtgjson_enums.json');
}

{
  MTGSet := TMTGSet.Create(MTGJSON_M21_URI, 'mtgjson/sets/json/set_M21.json', 'code');
  MTGSet.DumpList;
  FreeAndNil(MTGSet);
}

  MTGSetList := TMTGSetList.Create(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', 'code', UseCache);
  if not (MTGSetList.List = nil) then
    begin
    for idx := 0 to MTGSetList.List.Count -1 do
      begin
        setDate := TSetListRecord(MTGSetList.List.Objects[idx]).setReleaseDate;
//        if MTGSetList.List[idx] = 'ZNR' then
        if setDate >= '2019-10-04' then
          begin
            data := GetMTGJsonSetJson(MTGSetList.List[idx], 'mtgjson/sets/json', UseCache);
  //          MTGSetList.Dump(idx);
            MTGSet := TMTGSet.Create(data, 'uuid');
            cards += MTGSet.setTotalSetSize;
//          MTGSet.DumpList;

            for img := 0 to MTGSet.Cards.Count - 1 do
              begin
                imgMTGJsonID := MTGSet.Cards[img];
                imgScryID := MTGSet.ImageID(img);
                if not (imgScryID = EmptyStr) then
                  begin
                    imgURI := 'https://api.scryfall.com/cards/' + imgScryID + '?format=image&version=' + cardQuality;
                    imgPath := 'scryfall/sets/images/' + MTGSetList.List[idx] + '/' + cardQuality;
                    imgFile := imgPath + '/'+ imgMTGJsonID + '.jpg';
{
                    MemoMessage(imgURI);
                    MemoMessage(imgPath);
                    MemoMessage(imgFile);
}
                    CreateCastleDataDirectoryIfMissing(imgPath);
                    CacheImage(imgURI, imgFile, True, True);
                  end;
              end;

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
  DumpList(SList);
  Memo1.Lines.EndUpdate;
}
  Button2.Enabled := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Abort := True;
  Button2.Enabled := False;
  Button1.Enabled := True;
end;

end.

