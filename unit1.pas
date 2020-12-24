unit Unit1;

{$mode objfpc}{$H+}
// {$define usedecknet}
{$define useLog}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls,
  {$IFDEF UNIX}
  // cthreads,
  {$ENDIF}
//  CastleControl,
  StrUtils, SVGUtils,
  CastleParameters, CastleClassUtils, CastleDownload,
  CastleTimeUtils, CastleURIUtils, CastleFilesUtils
  ;

type

  { TMTGApp }

  TMTGApp = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3SetCaption;
  private
  public
  end;

var
  MTGApp: TMTGApp;
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
  MTGSON_PRICES_ALL_fURI = 'https://api.peardox.co.uk/prices/prices.json.gz';
  MTGSON_PRICES_PAPER_URI = 'https://api.peardox.co.uk/prices/prices.json.gz';

  cardQuality = 'large'; // normal / large

  UseCache: Boolean = True; // Only set to True while developing
  InitialSets: array [0 .. 4] of String = ('ELD', 'IKO', 'THB', 'M21', 'ZNR');
  InitialTypes: array [0 .. 1] of String = ('core', 'expansion');

{ Declare hooks for other units }

procedure MemoMessage(const msg: String);
procedure TriggerProcessMessages;
procedure ExportUUIDs(const FileName: String; const UseCache: Boolean = True);
procedure ExportSetUUIDs(const setCode: String; const OutFile: TTextWriter; const UseCache: Boolean = True);
procedure ExportImages(const UseCache: Boolean = True);
procedure ExportSetImages(const setCode: String; const UseCache: Boolean = True);

implementation

uses
  CastleLog, fpjson,
  CacheFileUtils,
  AssetGatherers,
  MTGJsonObjects, MTGJsonDeckObjects, MTGJsonSetObjects, MTGJsonPriceObjects;

{$R *.lfm}

{ Hooks for other units }

procedure TriggerProcessMessages;
begin
  Application.ProcessMessages;
end;

procedure MemoMessage(const msg: String);
begin
  {$ifdef useLog}
  WriteLnLog(msg);
  {$endif}
  MTGApp.Memo1.Lines.Add(msg);
  Application.ProcessMessages;
end;

procedure ExportSetUUIDs(const setCode: String; const OutFile: TTextWriter; const UseCache: Boolean = True);
var
  data: TStream;
  MTGSet: TMTGSet;
  idx: Integer;
  txt: String;
begin
  data := GetMTGJsonSetJson(setCode, 'mtgjson/sets/json', UseCache);
  MTGSet := TMTGSet.Create(data, 'uuid');
  try
    for idx := 0 to MTGSet.Cards.Count - 1 do
      begin
        txt := '"' + StringToJSONString(MTGSet.Cards[idx]) + '"' +
          ',"' + StringToJSONString(MTGSet.Name(idx)) + '"' +
          ',"' + StringToJSONString(SetCode) + '"' +
          ',"' + StringToJSONString(MTGSet.CardType(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.CardLayout(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.Number(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.Side(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.Rarity(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.FrameVersion(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.ImageID(idx)) + '"';
        OutFile.WriteLn(txt);
      end;
  finally
    FreeAndNil(MTGSet);
    FreeAndNil(data);
  end;
end;

procedure ExportUUIDs(const FileName: String; const UseCache: Boolean = True);
var
  MTGSetList: TMTGSetList;
  OutFile: TTextWriter;
  ticks: Int64;
  idx: Integer;
begin
  MTGApp.Button3.Enabled := False;
  MTGApp.Button2.Enabled := True;
  MTGApp.Button1.Enabled := False;
  ticks := CastleGetTickCount64;
  OutFile := TTextWriter.Create(FileName);
  try
    OutFile.WriteLn('"uuid","cardname","setcode","cardtype","cardlayout","cardnum","side","rarity","frameversion","scryfall"');
    MTGSetList := TMTGSetList.Create(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', 'code', UseCache);
    try
      if not (MTGSetList.List = nil) then
        begin
        for idx := 0 to MTGSetList.List.Count -1 do
          begin
            if not(MTGSetList.List[idx] = 'MZNR') then
              begin
                ExportSetUUIDs(MTGSetList.List[idx], OutFile, UseCache);
                Application.ProcessMessages;
                if Abort then
                  begin
                    Break;
                  end;
              end;
          end;
        end;
    finally
      FreeAndNil(MTGSetList);
    end;
  finally
    FreeAndNil(OutFile);
  end;
  ticks := CastleGetTickCount64 - ticks;
  MemoMessage('Time : ' + IntToStr(ticks) + 'ms');
  MTGApp.Button3.Enabled := True;
  MTGApp.Button2.Enabled := False;
  MTGApp.Button1.Enabled := True;
end;

procedure ExportSetImages(const setCode: String; const UseCache: Boolean = True);
var
  data: TStream;
  MTGSet: TMTGSet;
  idx: Integer;
  itime: Int64;
  itott: Int64;
  imerr: Integer;
  imgMTGJsonID: String;
  imgScryID:  String;
  imgURI: String;
  imgPath: String;
  imgFile: String;
begin
  itott := 0;
  imerr := 0;
  data := GetMTGJsonSetJson(setCode, 'mtgjson/sets/json', UseCache);
  MTGSet := TMTGSet.Create(data, 'uuid');
  try
    for idx := 0 to MTGSet.Cards.Count - 1 do
      begin
        imgMTGJsonID := MTGSet.Cards[idx];
        imgScryID := MTGSet.ImageID(idx);
        itime := CastleGetTickCount64;
        if not (imgScryID = EmptyStr) then
          begin
            imgURI := 'https://api.scryfall.com/cards/' + imgScryID + '?format=image&version=' + cardQuality;
            imgPath := 'scryfall/sets/images/set_' + SetCode + '/' + cardQuality;
            imgFile := imgPath + '/'+ imgMTGJsonID + '.jpg';
            CreateCastleDataDirectoryIfMissing(imgPath);
            try
              CacheImage(imgURI, imgFile, True, True);
            except
              on E: EImageCacheException do
                begin
                  MemoMessage('====== Bad Download ======');
                  MemoMessage('set  : ' + SetCode);
                  MemoMessage('uuid : ' + imgMTGJsonID);
                  MemoMessage('name : ' + MTGSet.Name(idx));
                  MemoMessage('type : ' + MTGSet.CardType(idx));
                  MemoMessage('num# : ' + MTGSet.Number(idx));
                  MemoMessage('rare : ' + MTGSet.Rarity(idx));
                  MemoMessage('scry : ' + imgScryID);
                  MemoMessage('========================');
                  Inc(imerr);
                end;
            end;
          end
        else
          begin
            MemoMessage('====== Bad Record ======');
            MemoMessage(SetCode);
            MemoMessage(MTGSet.Name(idx));
            MemoMessage(MTGSet.Number(idx));
            MemoMessage(MTGSet.Rarity(idx));
            MemoMessage(imgMTGJsonID);
            MemoMessage('========================');
          end;

        itime := CastleGetTickCount64 - itime;
        itott += itime;
      end;
  finally
    FreeAndNil(MTGSet);
    FreeAndNil(data);
  end;
end;

procedure ExportImages(const UseCache: Boolean = True);
var
  MTGSetList: TMTGSetList;
  ticks: Int64;
  idx: Integer;
begin
  MTGApp.Button3.Enabled := False;
  MTGApp.Button2.Enabled := True;
  MTGApp.Button1.Enabled := False;
  ticks := CastleGetTickCount64;

  MTGSetList := TMTGSetList.Create(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', 'code', UseCache);
  try
    if not (MTGSetList.List = nil) then
      begin
      for idx := 0 to MTGSetList.List.Count -1 do
        begin
          ExportSetImages(MTGSetList.List[idx], UseCache);
          MTGApp.Caption := 'Set = ' + MTGSetList.List[idx] + '(' +
            IntToStr(idx + 1) + '/' + IntToStr(MTGSetList.List.Count) + ')';
          Application.ProcessMessages;
          if Abort then
            begin
              Break;
            end;
        end;
      end;
  finally
    FreeAndNil(MTGSetList);
  end;

  ticks := CastleGetTickCount64 - ticks;
  MemoMessage('Time : ' + IntToStr(ticks) + 'ms');
  MTGApp.Button3.Enabled := True;
  MTGApp.Button2.Enabled := False;
  MTGApp.Button1.Enabled := True;
end;

{ TMTGApp }

procedure TMTGApp.FormCreate(Sender: TObject);
begin
  {$ifdef useLog}
  InitializeLog;
  {$endif}
  Abort := False;
  Button2.Enabled := False;
  InitializeLog;
  Memo1.Clear;
  MTGApp.Width := 800;
  MTGApp.Height := 600;
  MTGApp.Position:= poScreenCenter;
  MemoMessage('ApplicationConfig = ' + ApplicationConfig(''));
  MemoMessage('ApplicationData = ' + ApplicationData(''));
  Button3SetCaption;
end;

procedure TMTGApp.Button1Click(Sender: TObject);
var
  data: TStream = nil;
  ticks: Int64;
  SList: TStringList;
  MTGPriceList: TMTGPriceList;
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
  imerr: Integer;
  txt: String;
begin
  Abort := False;
  Button2.Enabled := True;
  cnt := 0;
  cards := 0;
  imerr := 0;
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
{
 MTGPriceList := TMTGPriceList.Create('https://api.peardox.co.uk/prices/json/sets/paper.ZNR.json.gz', 'mtgjson_pricelist.json');
 MTGPriceList.Free;

 MTGSetList := TMTGSetList.Create(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', 'code', UseCache);
 if not (MTGSetList.List = nil) then
   begin
    for idx := 0 to MTGSetList.List.Count -1 do
      begin
        data := GetMTGJsonPriceJson(MTGSetList.List[idx], 'mtgjson/prices/json', UseCache);
        FreeAndNil(data);
        if Abort then
          begin
            break;
          end;
      end;
   end;
 FreeAndNil(MTGSetList);
}

MTGSetList := TMTGSetList.Create(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', 'code', UseCache);
  if not (MTGSetList.List = nil) then
    begin
    for idx := 0 to MTGSetList.List.Count -1 do
      begin
//        if MTGSetList.List[idx] = 'MZNR' then
        if not(MTGSetList.List[idx] = 'MZNR') then
//        if MTGSetList.List[idx] = 'skipAllSets' then
          begin
          setDate := TSetListRecord(MTGSetList.List.Objects[idx]).setReleaseDate;
          setFullName := TSetListRecord(MTGSetList.List.Objects[idx]).setName;
          setType := TSetListRecord(MTGSetList.List.Objects[idx]).setType;
          setSize := IntToStr(TSetListRecord(MTGSetList.List.Objects[idx]).setTotalSetSize);
          Caption := 'Set = ' + MTGSetList.List[idx] + '(' + IntToStr(idx + 1) + '/' + IntToStr(MTGSetList.List.Count) + ')';
          MemoMessage('Set = ' + MTGSetList.List[idx] + '(' + IntToStr(idx + 1) + '/' + IntToStr(MTGSetList.List.Count) + ')');
{
            MemoMessage('===== ' + MTGSetList.List[idx] + ' (' + setSize + ') - ' +
              setType + ' : ' + setFullName + ' =====');
}
            data := GetMTGJsonSetJson(MTGSetList.List[idx], 'mtgjson/sets/json', UseCache);
            MTGSet := TMTGSet.Create(data, 'uuid');
            cards += MTGSet.Cards.Count; // MTGSet.setTotalSetSize;

{
}
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
//  MemoMessage('----------- END -----------');
  ticks := CastleGetTickCount64 - ticks;
  MemoMessage('Time : ' + IntToStr(ticks) + 'ms');
  Button1.Enabled := True;
{
  MemoMessage('Sets analysed : ' + IntToStr(cnt));
  MemoMessage('Sets cards : ' + IntToStr(cards));
  MemoMessage('Img errors : ' + IntToStr(imerr));
}
  Button2.Enabled := False;
{
  data := DownloadNetworkFile('https://noapi.peardox.co.uk/prices/prices.json.gz', [], False);
  if not(data = nil) then
    MemoMessage('OK');

  FreeAndNil(data);
}
end;

procedure TMTGApp.Button2Click(Sender: TObject);
begin
  {$if defined(debugMessages)}
  MemoMessage('Abort button clicked');
  {$endif}
  Abort := True;
  Button2.Enabled := False;
  Button1.Enabled := True;
end;

procedure TMTGApp.Button3SetCaption;
begin
  if UseCache then
    Button3.Caption := 'Use Cache'
  else
    Button3.Caption := 'Download';
end;

procedure TMTGApp.Button3Click(Sender: TObject);
begin
  UseCache := not UseCache;
  Button3SetCaption;
end;

procedure TMTGApp.Button4Click(Sender: TObject);
begin
  MemoMessage('Start Exporting');
  ExportUUIDs('castle-data:/uuids.csv');
  MemoMessage('Finished Exporting');
end;

procedure TMTGApp.Button5Click(Sender: TObject);
begin
  ExportImages();
end;

end.

