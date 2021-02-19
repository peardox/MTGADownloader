unit Console;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  {$IFDEF WINDOWS}
  Interfaces,
  {$ENDIF}
  CastleParameters, CastleClassUtils, CastleDownload,
  CastleTimeUtils, CastleURIUtils, CastleFilesUtils,
  CastleLog, fpjson,
  CacheFileUtils,
//  AssetGatherers, MTGJsonPriceObjects,
  MTGJsonObjects, MTGJsonDeckObjects, MTGJsonSetObjects;

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

  cardQuality = 'large'; // normal / large / png

  UseCache: Boolean = False; // Only set to True while developing
  InitialSets: array [0 .. 4] of String = ('ELD', 'IKO', 'THB', 'M21', 'ZNR');
  InitialTypes: array [0 .. 1] of String = ('core', 'expansion');
  DFCTypes: array [0 .. 4] of String = ('modal_dfc', 'transform', 'double_sided',
       'art_series', 'double_faced_token');


{ Declare hooks for other units }

procedure ExportUUIDs(const FileName: String; const UseCache: Boolean = True);
procedure ExportSetUUIDs(const setCode: String; const OutFile: TTextWriter; const UseCache: Boolean = True);
procedure ExportImages(const UseCache: Boolean = True; const reloadSpoilers: Boolean = False);
procedure ExportSetImages(const setCode: String; const UseCache: Boolean = True; const reloadSpoilers: Boolean = False);
procedure GetSets(const UseCache: Boolean = True);
procedure GetDecks(const UseCache: Boolean = True);

implementation

procedure ExportSetUUIDs(const setCode: String; const OutFile: TTextWriter; const UseCache: Boolean = True);
var
  data: TStream;
  MTGSet: TMTGSet;
  idx: Integer;
  txt: String;
  imgLayout: String;
begin
  data := GetMTGJsonSetJson(setCode, 'mtgjson/sets/json', UseCache);
  MTGSet := TMTGSet.Create(data, 'uuid');
  try
    for idx := 0 to MTGSet.Cards.Count - 1 do
      begin
        imgLayout := MTGSet.CardLayout(idx);
        if (setCode = 'MZNR') and (MTGSet.ShortName(idx) <> 'Base Race') then
          begin
            imgLayout := 'double_faced_token'; // MTGJson Hack - layout marked as incorrectly as token
          end;

        txt := '"' + StringToJSONString(MTGSet.Cards[idx]) + '"' +
          ',"' + StringToJSONString(MTGSet.Name(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.ShortName(idx)) + '"' +
          ',"' + StringToJSONString(SetCode) + '"' +
          ',"' + StringToJSONString(MTGSet.CardType(idx)) + '"' +
          ',"' + StringToJSONString(imgLayout) + '"' +
          ',"' + StringToJSONString(MTGSet.Number(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.Side(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.Rarity(idx)) + '"' +
          ',' + IntToStr(MTGSet.ArenaID(idx)) +
          ',"' + StringToJSONString(MTGSet.ImageID(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.FrameEffects(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.PromoTypes(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.Colors(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.ColorIdentity(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.ColorIndicator(idx)) + '"' +
          ',' + BoolToStr(MTGSet.hasFoil(idx), True) +
          ',' + BoolToStr(MTGSet.hasNonFoil(idx), True) +
          ',' + MTGSet.tcgID(idx) +
          ',' + MTGSet.MultiverseID(idx) +
          ',"' + StringToJSONString(MTGSet.Power(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.Toughness(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.ManaCost(idx)) + '"' +
          ',"' + StringToJSONString(MTGSet.setParentCode) + '"' +
          ',"' + StringToJSONString(MTGSet.ScryfallIllustrationID(idx)) + '"';

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
  ticks := CastleGetTickCount64;
  OutFile := TTextWriter.Create(FileName);
  try
    OutFile.WriteLn('"uuid","cardname","shortname","setcode","cardtype","cardlayout","cardnum","side","rarity","arenaid","scryfall","FrameEffects","PromoTypes","Colors","ColorIdentity","ColorIndicator","hasFoil","hasNonFoil","tcgID","MultiverseID","Power","Toughness","ManaCost","ParentSetCode", "ScryfallIllustrationID"');
    MTGSetList := TMTGSetList.Create(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', 'code', UseCache);
    try
      if not (MTGSetList.List = nil) then
        begin
        ExportSetUUIDs('AKHM', OutFile, UseCache);
        for idx := 0 to MTGSetList.List.Count -1 do
          begin
            if not(MTGSetList.List[idx] = 'MZNRdummy') then
              begin
                ExportSetUUIDs(MTGSetList.List[idx], OutFile, UseCache);
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
end;

procedure ExportSetImages(const setCode: String; const UseCache: Boolean = True; const reloadSpoilers: Boolean = False);
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
  imgFace: String;
  imgSide: String;
  imgLayout: String;
  elm: Integer;
  isPartialPreview: Boolean;
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
        imgLayout := MTGSet.CardLayout(idx);
        isPartialPreview := MTGSet.setIsPartialPreview;
        if (setCode = 'MZNR') and (MTGSet.ShortName(idx) <> 'Base Race') then
          begin
            imgLayout := 'double_faced_token'; // MTGJson Hack - layout marked as incorrectly as token
          end;
        imgSide := MTGSet.Side(idx);
        imgFace := 'front';
        if not(imgSide = 'a') then
          begin
            for elm :=Low(DFCTypes) to High(DFCTypes) do
              begin
                if imgLayout = DFCTypes[elm] then
                  begin
                    imgFace := 'back';
                    break;
                  end;
              end;
          end;
        itime := CastleGetTickCount64;
        if not (imgScryID = EmptyStr) then
          begin
            imgURI := 'https://api.scryfall.com/cards/' + imgScryID + '?format=image&version=' + cardQuality  + '&face=' + imgFace;
            imgPath := 'scryfall/sets/images/set_' + SetCode + '/' + cardQuality;
            if(cardQuality = 'png') then
              imgFile := imgPath + '/'+ imgMTGJsonID + '.png'
            else
              imgFile := imgPath + '/'+ imgMTGJsonID + '.jpg';
            CreateCastleDataDirectoryIfMissing(imgPath);
            try
              if isPartialPreview and reloadSpoilers then
                CacheImage(imgURI, imgFile, True, True, True)
              else
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

procedure ExportImages(const UseCache: Boolean = True; const reloadSpoilers: Boolean = False);
var
  MTGSetList: TMTGSetList;
  ticks: Int64;
  idx: Integer;
  fcnt: Integer;
begin
  fcnt := 0;
  ticks := CastleGetTickCount64;

  MTGSetList := TMTGSetList.Create(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', 'code', UseCache);
  try
    if not (MTGSetList.List = nil) then
      begin
      for idx := 0 to MTGSetList.List.Count -1 do
        begin
          if not(MTGSetList.List[idx] = 'MZNRdummy') then
            begin
              newFiles := 0;
              ExportSetImages(MTGSetList.List[idx], UseCache, reloadSpoilers);
              if newFiles > 0 then
              MemoMessage('Set = ' + MTGSetList.List[idx] + '(' +
                IntToStr(idx + 1) + '/' + IntToStr(MTGSetList.List.Count) + ')' +
                ' New = ' + IntToStr(newFiles));
              fcnt += newFiles;
            end;
        end;
      end;
  finally
    FreeAndNil(MTGSetList);
  end;

  ticks := CastleGetTickCount64 - ticks;
  MemoMessage('Time : ' + IntToStr(ticks) + 'ms');
  MemoMessage('New Images : ' + IntToStr(fcnt));
end;

procedure GetSets(const UseCache: Boolean = True);
var
  data: TStream = nil;
  ticks: Int64;
  MTGSet: TMTGSet;
  MTGSetList: TMTGSetList;
  idx: Integer;
  cnt: Integer;
  cards: Integer;
  isPartialPreview: Boolean;
begin
  cnt := 0;
  cards := 0;
  ticks := CastleGetTickCount64;

  MTGSetList := TMTGSetList.Create(MTGJSON_SETLIST_URI, 'mtgjson_setlist.json', 'code', UseCache);
  if not (MTGSetList.List = nil) then
    begin
    for idx := 0 to MTGSetList.List.Count -1 do
      begin
        if not(MTGSetList.List[idx] = 'MZNRdummy') then
          begin
          isPartialPreview := TSetListRecord(MTGSetList.List.Objects[idx]).setIsPartialPreview;
          if isPartialPreview then
            MemoMessage('Set = ' + MTGSetList.List[idx] + '(' + IntToStr(idx + 1) + '/' + IntToStr(MTGSetList.List.Count) + ') [Preview]')
          else
            MemoMessage('Set = ' + MTGSetList.List[idx] + '(' + IntToStr(idx + 1) + '/' + IntToStr(MTGSetList.List.Count) + ')');
          data := GetMTGJsonSetJson(MTGSetList.List[idx], 'mtgjson/sets/json', UseCache);
          MTGSet := TMTGSet.Create(data, 'uuid');
          cards += MTGSet.Cards.Count; // MTGSet.setTotalSetSize;
          Inc(cnt);
          FreeAndNil(MTGSet);
          FreeAndNil(data);
          end;
      end;
    end;
  FreeAndNil(MTGSetList);

  ticks := CastleGetTickCount64 - ticks;
  MemoMessage('Time : ' + IntToStr(ticks) + 'ms');
end;

procedure GetDecks(const UseCache: Boolean = True);
var
  data: TStream = nil;
  ticks: Int64;
  MTGDeckList: TMTGDeckList;
  idx: Integer;
  cnt: Integer;
  decks: Integer;
begin
  cnt := 0;
  decks := 0;
  ticks := CastleGetTickCount64;

  MTGDeckList := TMTGDeckList.Create(MTGJSON_DECKLIST_URI, 'mtgjson_decklist.json', 'fileName');
  if not (MTGDeckList.List = nil) then
    begin
      for idx := 0 to MTGDeckList.List.Count -1 do
        begin
          data := GetMTGJsonDeckJson(MTGDeckList.List[idx], 'mtgjson/decks/json');
          MemoMessage('Deck = ' + MTGDeckList.List[idx] + '(' + IntToStr(idx + 1) + '/' + IntToStr(MTGDeckList.List.Count) + ')');
//          MTGDeck := TMTGDeck.Create(data, 'uuid'); // Wrong
//          decks += MTGDeck.Cards.Count; // Wrong
          Inc(cnt);
//          FreeAndNil(MTGDeck); // Wrong
          FreeAndNil(data);
        end;
    end;
  FreeAndNil(MTGDeckList);

  ticks := CastleGetTickCount64 - ticks;
  MemoMessage('Time : ' + IntToStr(ticks) + 'ms');
end;

end.
