unit Unit1;

{$mode objfpc}{$H+}
// {$define usedecknet}
{$define useLog}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, StrUtils,
  {$IFDEF UNIX}
  // cthreads,
  {$ENDIF}
  ComCtrls, SVGUtils,
  CastleParameters, CastleClassUtils,
  CastleControl, CastleTimeUtils, CastleURIUtils, CastleFilesUtils
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

  UseCache: Boolean = True; // Only set to True while developing
  InitialSets: array [0 .. 4] of String = ('ELD', 'IKO', 'THB', 'M21', 'ZNR');
  InitialTypes: array [0 .. 1] of String = ('core', 'expansion');

{ Declare hooks for other units }

procedure EnableAbortButton(Status: Boolean);
procedure MemoMessage(const msg: String);
procedure TriggerProcessMessages;

implementation

uses
  CastleLog,
  CacheFileUtils, AssetGatherers, MTGJsonObjects;

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
  {$ifdef useLog}
  WriteLnLog(msg);
  {$endif}
  Form1.Memo1.Lines.Add(msg);
  Application.ProcessMessages;
end;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$ifdef useLog}
  InitializeLog;
  {$endif}
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
  setFullName: String;
  setType: String;
  setSize: String;
  imgMTGJsonID: String;
  imgScryID:  String;
  imgURI: String;
  imgPath: String;
  imgFile: String;
  itime: Int64;
  itott: Int64;
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
  SList.Free;
}
  MTGSetList := TMTGSetList.Create(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', 'code', UseCache);
  if not (MTGSetList.List = nil) then
    begin
    for idx := 0 to MTGSetList.List.Count -1 do
      begin
        setDate := TSetListRecord(MTGSetList.List.Objects[idx]).setReleaseDate;
        setFullName := TSetListRecord(MTGSetList.List.Objects[idx]).setName;
        setType := TSetListRecord(MTGSetList.List.Objects[idx]).setType;
        setSize := IntToStr(TSetListRecord(MTGSetList.List.Objects[idx]).setTotalSetSize);
        Caption := 'Set = ' + MTGSetList.List[idx] + '(' + IntToStr(idx) + '/' + IntToStr(MTGSetList.List.Count) + ')';
//        if MTGSetList.List[idx] = 'ZNE' then
//        if MTGSetList.List[idx] = 'ZNR' then
//        if setDate >= '2019-10-04' then
//        if IndexStr(MTGSetList.List[idx], InitialSets) <> -1 then
//        if IndexStr(SetType, InitialTypes) <> -1 then
          begin
            MemoMessage('===== ' + MTGSetList.List[idx] + ' (' + setSize + ') - ' +
              setType + ' : ' + setFullName + ' =====');
            data := GetMTGJsonSetJson(MTGSetList.List[idx], 'mtgjson/sets/json', UseCache);
  //          MTGSetList.Dump(idx);
            MTGSet := TMTGSet.Create(data, 'uuid');
            cards += MTGSet.setTotalSetSize;
//          MTGSet.DumpList;

            itott := 0;

            for img := 0 to MTGSet.Cards.Count - 1 do
              begin
                imgMTGJsonID := MTGSet.Cards[img];
                imgScryID := MTGSet.ImageID(img);
                itime := CastleGetTickCount64;
                if not (imgScryID = EmptyStr) then
                  begin
                    imgURI := 'https://api.scryfall.com/cards/' + imgScryID + '?format=image&version=' + cardQuality;
                    imgPath := 'scryfall/sets/images/' + MTGSetList.List[idx] + '/' + cardQuality;
                    imgFile := imgPath + '/'+ imgMTGJsonID + '.jpg';
                    CreateCastleDataDirectoryIfMissing(imgPath);
                    CacheImage(imgURI, imgFile, True, True);
                  end
                else
                  begin
                    MemoMessage('====== Bad Record ======');
                    MemoMessage(MTGSetList.List[idx]);
                    MemoMessage(MTGSet.Name(img));
                    MemoMessage(MTGSet.Number(img));
                    MemoMessage(MTGSet.Rarity(img));
                    MemoMessage(imgMTGJsonID);
                    MemoMessage('========================');
                  end;

                itime := CastleGetTickCount64 - itime;
                itott += itime;

                Caption := 'Set = ' + MTGSetList.List[idx] + '(' + IntToStr(idx) + '/' + IntToStr(MTGSetList.List.Count) + ')' +
                  ' - Image ' + '(' + IntToStr(img + 1) + '/' + IntToStr(MTGSet.Cards.Count) + ')' +
                  ' Time : ' + FormatFloat('####0.00', (itime / 1000)) + 's' +
                  ' Avg : ' + FormatFloat('####0.00', (itott / 1000) / (img + 1)) + 's' +
                  ' Total : ' + FormatFloat('####0.00', (itott / 1000)) + 's';
                {$ifdef useLog}
                if img = (MTGSet.Cards.Count - 1) then
                  WriteLnLog('Set = ' + MTGSetList.List[idx] + '(' + IntToStr(idx) + '/' + IntToStr(MTGSetList.List.Count) + ')' +
                    ' - Image ' + '(' + IntToStr(img + 1) + '/' + IntToStr(MTGSet.Cards.Count) + ')' +
                    ' Time : ' + FormatFloat('####0.00', (itime / 1000)) + 's' +
                    ' Avg : ' + FormatFloat('####0.00', (itott / 1000) / (img + 1)) + 's' +
                    ' Total : ' + FormatFloat('####0.00', (itott / 1000)) + 's');
                {$endif}

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

  Button2.Enabled := False;
{
  data := DownloadNetworkFile('https://noapi.peardox.co.uk/prices/prices.json.gz', [], False);
  if not(data = nil) then
    MemoMessage('OK');

  FreeAndNil(data);
}
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  {$if defined(debugMessages)}
  MemoMessage('Abort button clicked');
  {$endif}
  Abort := True;
  Button2.Enabled := False;
  Button1.Enabled := True;
end;

end.

