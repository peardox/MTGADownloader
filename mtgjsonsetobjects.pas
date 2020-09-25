unit mtgJsonSetObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JsonTools, MTGJsonObjects;

type
  TSetListRecord = class(TPersistent)
  private
    FbaseSetSize: Integer;
    Fcode: String;
    Fname: String;
    FreleaseDate: String;
    FtotalSetSize: Integer;
    Ftype: String;
    FparentCode: String;
    FisPartialPreview: Boolean;
  published
    property setBaseSetSize: Integer read FbaseSetSize write FbaseSetSize;
    property setCode: String read Fcode write Fcode;
    property setName: String read Fname write Fname;
    property setReleaseDate: String read FreleaseDate write FreleaseDate;
    property setTotalSetSize: Integer read FtotalSetSize write FtotalSetSize;
    property setType: String read Ftype write Ftype;
    property setParentCode: String read FparentCode write FparentCode;
    property SetIsPartialPreview: Boolean read FisPartialPreview write FisPartialPreview;
  end;

  TMTGSetList = class(TMTGList)
    private
      FList: TStringList;
    protected
      function MapJsonObject(const Json: TJsonNode; out Rec: TSetListRecord): String;
      procedure ProcessListArrayDataObject(const Json: TJsonNode); override;
    public
      destructor Destroy; override;
      procedure DumpList; override;
      procedure Dump(idx: Integer);
    published
      property List: TStringList read FList write FList;
  end;

  TSetCardIdentifiersRecord = class(TPersistent)
   private
     FcardKingdomId: String;
     FmcmId: String;
     FmcmMetaId: String;
     FmtgjsonV4Id: String;
     FscryfallId: String;
     FscryfallIllustrationId: String;
     FscryfallOracleId: String;
     FtcgplayerProductId: String;
     FcardKingdomFoilId: String;
     FmtgoId: String;
     FmultiverseId: String;
     FmtgArenaId: String;
     FmtgoFoilId: String;
   published
     property setCardKingdomId: String read FcardKingdomId write FcardKingdomId;
     property setMcmId: String read FmcmId write FmcmId;
     property setMcmMetaId: String read FmcmMetaId write FmcmMetaId;
     property setMtgjsonV4Id: String read FmtgjsonV4Id write FmtgjsonV4Id;
     property setScryfallId: String read FscryfallId write FscryfallId;
     property setScryfallIllustrationId: String read FscryfallIllustrationId write FscryfallIllustrationId;
     property setScryfallOracleId: String read FscryfallOracleId write FscryfallOracleId;
     property setTcgplayerProductId: String read FtcgplayerProductId write FtcgplayerProductId;
     property setCardKingdomFoilId: String read FcardKingdomFoilId write FcardKingdomFoilId;
     property setMtgoId: String read FmtgoId write FmtgoId;
     property setMultiverseId: String read FmultiverseId write FmultiverseId;
     property setMtgArenaId: String read FmtgArenaId write FmtgArenaId;
     property setMtgoFoilId: String read FmtgoFoilId write FmtgoFoilId;
  end;

  TSetCardRecord = class(TPersistent)
  private
    Fartist: String;
    FborderColor: String;
    FconvertedManaCost: Integer;
    FflavorText: String;
    FframeVersion: String;
    FhasFoil: Boolean;
    FhasNonFoil: Boolean;
    FisStarter: Boolean;
    Flayout: String;
    FmanaCost: String;
    Fname: String;
    Fnumber: String;
    ForiginalText: String;
    ForiginalType: String;
    Fpower: String;
    Frarity: String;
    FsetCode: String;
    Ftext: String;
    Ftoughness: String;
    Ftype: String;
    Fuuid: String;
    FedhrecRank: Integer;
    FisReprint: Boolean;
    Floyalty: String;
    FisPromo: Boolean;
    FfaceConvertedManaCost: Integer;
    FfaceName: String;
    Fside: String;
    FisStorySpotlight: Boolean;
    Fwatermark: String;
    FisAlternative: Boolean;
    FisFullArt: Boolean;
    FflavorName: String;
    FisOnlineOnly: Boolean;
    FisOversized: Boolean;
    FisTextless: Boolean;
    FasciiName: String;
    FisTimeshifted: Boolean;
    Fhand: String;
    Flife: String;
    FhasAlternativeDeckLimit: Boolean;
    FduelDeck: String;
    FisReserved: Boolean;
    FhasContentWarning: Boolean;
    Fidentifiers: TSetCardIdentifiersRecord;
  public
    destructor Destroy; override;
  published
    property setArtist: String read Fartist write Fartist;
    property setBorderColor: String read FborderColor write FborderColor;
    property setConvertedManaCost: Integer read FconvertedManaCost write FconvertedManaCost;
    property setFlavorText: String read FflavorText write FflavorText;
    property setFrameVersion: String read FframeVersion write FframeVersion;
    property setHasFoil: Boolean read FhasFoil write FhasFoil;
    property setHasNonFoil: Boolean read FhasNonFoil write FhasNonFoil;
    property setIsStarter: Boolean read FisStarter write FisStarter;
    property setLayout: String read Flayout write Flayout;
    property setManaCost: String read FmanaCost write FmanaCost;
    property setName: String read Fname write Fname;
    property setNumber: String read Fnumber write Fnumber;
    property setOriginalText: String read ForiginalText write ForiginalText;
    property setOriginalType: String read ForiginalType write ForiginalType;
    property setPower: String read Fpower write Fpower;
    property setRarity: String read Frarity write Frarity;
    property setSetCode: String read FsetCode write FsetCode;
    property setText: String read Ftext write Ftext;
    property setToughness: String read Ftoughness write Ftoughness;
    property setType: String read Ftype write Ftype;
    property setUuid: String read Fuuid write Fuuid;
    property setEdhrecRank: Integer read FedhrecRank write FedhrecRank;
    property setIsReprint: Boolean read FisReprint write FisReprint;
    property setLoyalty: String read Floyalty write Floyalty;
    property setIsPromo: Boolean read FisPromo write FisPromo;
    property setFaceConvertedManaCost: Integer read FfaceConvertedManaCost write FfaceConvertedManaCost;
    property setFaceName: String read FfaceName write FfaceName;
    property setSide: String read Fside write Fside;
    property setIsStorySpotlight: Boolean read FisStorySpotlight write FisStorySpotlight;
    property setWatermark: String read Fwatermark write Fwatermark;
    property setIsAlternative: Boolean read FisAlternative write FisAlternative;
    property setIsFullArt: Boolean read FisFullArt write FisFullArt;
    property setFlavorName: String read FflavorName write FflavorName;
    property setIsOnlineOnly: Boolean read FisOnlineOnly write FisOnlineOnly;
    property setIsOversized: Boolean read FisOversized write FisOversized;
    property setIsTextless: Boolean read FisTextless write FisTextless;
    property setAsciiName: String read FasciiName write FasciiName;
    property setIsTimeshifted: Boolean read FisTimeshifted write FisTimeshifted;
    property setHand: String read Fhand write Fhand;
    property setLife: String read Flife write Flife;
    property setHasAlternativeDeckLimit: Boolean read FhasAlternativeDeckLimit write FhasAlternativeDeckLimit;
    property setDuelDeck: String read FduelDeck write FduelDeck;
    property setIsReserved: Boolean read FisReserved write FisReserved;
    property setHasContentWarning: Boolean read FhasContentWarning write FhasContentWarning;
    property setIdentifiers: TSetCardIdentifiersRecord read Fidentifiers write Fidentifiers;
 end;

  TMTGSet = class(TMTGList)
    private
      FbaseSetSize: Integer;
      Fblock: String;
      Fcode: String;
      FisFoilOnly: Boolean;
      FisOnlineOnly: Boolean;
      FkeyruneCode : String;
      FmtgoCode : String;
      Fname : String;
      FreleaseDate : String;
      FtcgplayerGroupId : Integer;
      FtotalSetSize : Integer;
      Ftype : String;
      FmcmId : Integer;
      FmcmName : String;
      FisNonFoilOnly: Boolean;
      FisForeignOnly: Boolean;
      FparentCode: String;
      FisPartialPreview: Boolean;
      FCards: TStringList;
    protected
//      procedure ProcessListArrayDataObject(const Json: TJsonNode); override;
      procedure MapJsonCardArray(const Json: TJsonNode; const isToken: Boolean);
      function MapJsonIdentifiersObject(const Json: TJsonNode): TSetCardIdentifiersRecord;
      procedure MapJsonObject(const Json: TJsonNode); override;
      function  MapJsonCardObject(const Json: TJsonNode; const isToken: Boolean; out Rec: TSetCardRecord): String;
      procedure MapJsonSetObject(const Json: TJsonNode);
      procedure ProcessListObjectDataObject(const Json: TJsonNode); override;
    public
      function ExtractImageList: TStringList;
      function ImageID(const idx: Integer): String;
      function Name(const idx: Integer): String;
      function Number(const idx: Integer): String;
      function Side(const idx: Integer): String;
      function Rarity(const idx: Integer): String;
      function CardType(const idx: Integer): String;
      destructor Destroy; override;
      procedure DumpList; override;
    published
      property setBaseSetSize: Integer read FbaseSetSize write FbaseSetSize;
      property setBlock: String read Fblock write Fblock;
      property setCode: String read Fcode write Fcode;
      property setIsFoilOnly: Boolean read FisFoilOnly write FisFoilOnly;
      property setIsOnlineOnly: Boolean read FisOnlineOnly write FisOnlineOnly;
      property setKeyruneCode: String read FkeyruneCode write FkeyruneCode;
      property setMtgoCode: String read FmtgoCode write FmtgoCode;
      property setName: String read Fname write Fname;
      property setReleaseDate: String read FreleaseDate write FreleaseDate;
      property setTcgplayerGroupId: Integer read FtcgplayerGroupId write FtcgplayerGroupId;
      property setTotalSetSize: Integer read FtotalSetSize write FtotalSetSize;
      property setType: String read Ftype write Ftype;
      property setMcmId: Integer read FmcmId write FmcmId;
      property setMcmName: String read FmcmName write FmcmName;
      property setIsNonFoilOnly: Boolean read FisNonFoilOnly write FisNonFoilOnly;
      property setIsForeignOnly: Boolean read FisForeignOnly write FisForeignOnly;
      property setParentCode: String read FparentCode write FparentCode;
      property setIsPartialPreview: Boolean read FisPartialPreview write FisPartialPreview;
      property Cards: TStringList read FCards write FCards;
  end;

const
  MTGJSON_SET_PREFIX_URI = 'https://mtgjson.com/api/v5/';
  MTGJSON_SET_POSTFIX_URI = '.json.gz';

  function GetMTGJsonSetJson(SetCode: String; Path: String; UseCache: Boolean = True): TStream;

implementation

uses Unit1, CacheFileUtils;

{ TSetCardRecord =============================================================}

destructor TSetCardRecord.Destroy;
begin
  FreeAndNil(Fidentifiers);
  inherited Destroy;
end;

{ TMTGSetList ================================================================}

destructor TMTGSetList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TMTGSetList.ProcessListArrayDataObject(const Json: TJsonNode);
var
  idx: Integer;
  Rec: TSetListRecord;
  Key: String;
begin
  FList := TStringList.Create;
  FList.OwnsObjects := True;
  FList.Sorted := True;
  FList.Duplicates := dupError;

  for idx := 0 to Json.Count - 1 do
    begin
      Key := MapJsonObject(Json.Child(idx), Rec);
      try
        FList.AddObject(Key, Rec);
      except
        on E : Exception do
          begin
            MemoMessage('Oops' + LineEnding +
                        'Key = ' + Key + LineEnding +
                         E.ClassName + LineEnding +
                         E.Message);
           end;
      end;
    end;
end;

function TMTGSetList.MapJsonObject(const Json: TJsonNode; out Rec: TSetListRecord): String;
var
  Node: TJsonNode;
  Key: String;
begin
  Key := '';
  Rec := TSetListRecord.Create;

  for Node in Json do
    begin
      if Node.Name = FKey then
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for code expected nkString got ' + JSONKindToString(Node))
          else
            Key := Node.AsString;
        end;
      case Node.Name of
      'baseSetSize':
        begin
          if not(Node.Kind = nkNumber) then
            MemoMessage('TypeError for baseSetSize expected nkNumber got ' + JSONKindToString(Node))
          else
            Rec.setBaseSetSize := Trunc(Node.AsNumber);
        end;
      'code':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for code expected nkString got ' + JSONKindToString(Node))
          else
            Rec.setCode := Node.AsString;
        end;
      'name':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for name expected nkString got ' + JSONKindToString(Node))
          else
            Rec.setName := Node.AsString;
        end;
      'releaseDate':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for releaseDate expected nkString got ' + JSONKindToString(Node))
          else
            Rec.setReleaseDate := Node.AsString;
        end;
      'totalSetSize':
        begin
          if not(Node.Kind = nkNumber) then
            MemoMessage('TypeError for totalSetSize expected nkNumber got ' + JSONKindToString(Node))
          else
            Rec.setTotalSetSize := Trunc(Node.AsNumber);
        end;
      'type':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for type expected nkString got ' + JSONKindToString(Node))
          else
            Rec.setType := Node.AsString;
        end;
      'isPartialPreview':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isPartialPreview expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.setIsPartialPreview := Node.AsBoolean;
        end;
      'parentCode':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for parentCode expected nkString got ' + JSONKindToString(Node))
          else
            Rec.setParentCode := Node.AsString;
        end;
      else
          begin
            MemoMessage('Unhandled node : ' + Node.Name + ' - ' + JSONKindToString(Node));
          end;
      end;
    end;

  Result := Key;
end;

procedure TMTGSetList.DumpList;
var
  idx: Integer;
  Rec: TSetListRecord;
begin
  for idx := 0 to FList.Count -1 do
    begin
      MemoMessage('Key: ' + FList[idx]);
      Rec := FList.Objects[idx] as TSetListRecord;
      MemoMessage('  setBaseSetSize: ' + IntToStr(Rec.setBaseSetSize));
      MemoMessage('  setCode: ' + Rec.setCode);
      MemoMessage('  setName: ' + Rec.setName);
      MemoMessage('  setTotalSetSize: ' + IntToStr(Rec.setTotalSetSize));
      MemoMessage('  setReleaseDate: ' + Rec.setReleaseDate);
      MemoMessage('  setType: ' + Rec.setType);
      MemoMessage('  setParentCode: ' + Rec.setParentCode);
      if Rec.setIsPartialPreview then
        MemoMessage('  setIsPartialPreview: True')
      else
        MemoMessage('  setIsPartialPreview: False');
    end;
end;

procedure TMTGSetList.Dump(idx: Integer);
var
  Rec: TSetListRecord;
begin
  Rec := FList.Objects[idx] as TSetListRecord;

  MemoMessage('== Set Dump (TSetListRecord) ==');
  MemoMessage('setBaseSetSize: ' + IntToStr(Rec.setBaseSetSize));
  MemoMessage('setCode: ' + Rec.setCode);
  MemoMessage('setName: ' + Rec.setName);
  MemoMessage('setTotalSetSize: ' + IntToStr(Rec.setTotalSetSize));
  MemoMessage('setReleaseDate: ' + Rec.setReleaseDate);
  MemoMessage('setType: ' + Rec.setType);
  MemoMessage('setParentCode: ' + Rec.setParentCode);
  if Rec.setIsPartialPreview then
    MemoMessage('setIsPartialPreview: True')
  else
    MemoMessage('setIsPartialPreview: False');
  MemoMessage('===============================');
end;

{ TMTGDeckList ===============================================================}

{ TMTGSet ====================================================================}

destructor TMTGSet.Destroy;
begin
  FreeAndNil(FCards);
  inherited Destroy;
end;

procedure TMTGSet.ProcessListObjectDataObject(const Json: TJsonNode);
begin
{
  if ClassName = 'TMTGSet' then
    MemoMessage('ProcessListObjectDataObject (TMTGSet)');
}
  MapJsonSetObject(Json);
end;

procedure TMTGSet.MapJsonCardArray(const Json: TJsonNode; const isToken: Boolean);
var
  idx: Integer;
  Rec: TSetCardRecord;
  Key: String;
begin
  if FCards = nil then
    begin
      FCards := TStringList.Create;
      FCards.OwnsObjects := True;
      FCards.Sorted := True;
      FCards.Duplicates := dupError;
    end;

  for idx := 0 to Json.Count - 1 do
    begin
      Key := MapJsonCardObject(Json.Child(idx), isToken, Rec);
      try
        FCards.AddObject(Key, Rec);
      except
        on E : Exception do
          begin
            MemoMessage('Oops' + LineEnding +
                        'Key = ' + Key + LineEnding +
                         E.ClassName + LineEnding +
                         E.Message);
           end;
      end;
    end;
end;


function TMTGSet.MapJsonCardObject(const Json: TJsonNode; const isToken: Boolean; out Rec: TSetCardRecord): String;
var
  Node: TJsonNode;
  Txt: String;
  Ext: String;
  propdec: String;
  membdec: String;
  Key: String;
begin
  Key := '';
  Rec := TSetCardRecord.Create;

  Ext := '';
  propdec := '';
  membdec := '';

  for Node in Json do
    begin
      if Node.Name = FKey then
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for code expected nkString got ' + JSONKindToString(Node))
          else
            begin
            Key := Node.AsString;
            if isToken then
              Rec.Frarity := 'token';
            end;
        end;
      case Node.Name of
      'artist':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for artist expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Fartist := Node.AsString;
        end;
      'borderColor':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for borderColor expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FborderColor := Node.AsString;
        end;
      'convertedManaCost':
        begin
          if not(Node.Kind = nkNumber) then
            MemoMessage('TypeError for convertedManaCost expected nkNumber got ' + JSONKindToString(Node))
          else
            Rec.FconvertedManaCost := Trunc(Node.AsNumber);
        end;
      'flavorText':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for flavorText expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FflavorText := Node.AsString;
        end;
      'frameVersion':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for frameVersion expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FframeVersion := Node.AsString;
        end;
      'hasFoil':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for hasFoil expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FhasFoil := Node.AsBoolean;
        end;
      'hasNonFoil':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for hasNonFoil expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FhasNonFoil := Node.AsBoolean;
        end;
      'isStarter':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isStarter expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FisStarter := Node.AsBoolean;
        end;
      'layout':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for layout expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Flayout := Node.AsString;
        end;
      'manaCost':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for manaCost expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FmanaCost := Node.AsString;
        end;
      'name':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for name expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Fname := Node.AsString;
        end;
      'number':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for number expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Fnumber := Node.AsString;
        end;
      'originalText':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for originalText expected nkString got ' + JSONKindToString(Node))
          else
            Rec.ForiginalText := Node.AsString;
        end;
      'originalType':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for originalType expected nkString got ' + JSONKindToString(Node))
          else
            Rec.ForiginalType := Node.AsString;
        end;
      'power':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for power expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Fpower := Node.AsString;
        end;
      'rarity':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for rarity expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Frarity := Node.AsString;
        end;
      'setCode':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for setCode expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FsetCode := Node.AsString;
        end;
      'text':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for text expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Ftext := Node.AsString;
        end;
      'toughness':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for toughness expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Ftoughness := Node.AsString;
        end;
      'type':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for type expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Ftype := Node.AsString;
        end;
      'uuid':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for uuid expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Fuuid := Node.AsString;
        end;
      'edhrecRank':
        begin
          if not(Node.Kind = nkNumber) then
            MemoMessage('TypeError for edhrecRank expected nkNumber got ' + JSONKindToString(Node))
          else
            Rec.FedhrecRank := Trunc(Node.AsNumber);
        end;
      'isReprint':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isReprint expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FisReprint := Node.AsBoolean;
        end;
      'loyalty':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for loyalty expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Floyalty := Node.AsString;
        end;
      'isPromo':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isPromo expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FisPromo := Node.AsBoolean;
        end;
      'faceConvertedManaCost':
        begin
          if not(Node.Kind = nkNumber) then
            MemoMessage('TypeError for faceConvertedManaCost expected nkNumber got ' + JSONKindToString(Node))
          else
            Rec.FfaceConvertedManaCost := Trunc(Node.AsNumber);
        end;
      'faceName':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for faceName expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FfaceName := Node.AsString;
        end;
      'side':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for side expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Fside := Node.AsString;
        end;
      'isStorySpotlight':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isStorySpotlight expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FisStorySpotlight := Node.AsBoolean;
        end;
      'watermark':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for watermark expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Fwatermark := Node.AsString;
        end;
      'isAlternative':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isAlternative expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FisAlternative := Node.AsBoolean;
        end;
      'isFullArt':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isFullArt expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FisFullArt := Node.AsBoolean;
        end;
      'flavorName':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for flavorName expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FflavorName := Node.AsString;
        end;
      'isOnlineOnly':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isOnlineOnly expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FisOnlineOnly := Node.AsBoolean;
        end;
      'isOversized':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isOversized expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FisOversized := Node.AsBoolean;
        end;
      'isTextless':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isTextless expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FisTextless := Node.AsBoolean;
        end;
      'asciiName':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for asciiName expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FasciiName := Node.AsString;
        end;
      'isTimeshifted':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isTimeshifted expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FisTimeshifted := Node.AsBoolean;
        end;
      'hand':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for hand expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Fhand := Node.AsString;
        end;
      'life':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for life expected nkString got ' + JSONKindToString(Node))
          else
            Rec.Flife := Node.AsString;
        end;
      'hasAlternativeDeckLimit':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for hasAlternativeDeckLimit expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FhasAlternativeDeckLimit := Node.AsBoolean;
        end;
      'duelDeck':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for duelDeck expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FduelDeck := Node.AsString;
        end;
      'isReserved':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isReserved expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FisReserved := Node.AsBoolean;
        end;
      'hasContentWarning':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for hasContentWarning expected nkBool got ' + JSONKindToString(Node))
          else
            Rec.FhasContentWarning := Node.AsBoolean;
        end;
      'identifiers':
        begin
          if not(Node.Kind = nkObject) then
            MemoMessage('TypeError for identifiers expected nkObject got ' + JSONKindToString(Node))
          else
            Rec.Fidentifiers := MapJsonIdentifiersObject(Node);
        end;
      'reverseRelated':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for reverseRelated expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.FreverseRelated := MapJsonArray(Node); // *** FIXME ***
        end;
      'colorIndicator':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for colorIndicator expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.FcolorIndicator := MapJsonArray(Node); // *** FIXME ***
        end;
      'otherFaceIds':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for otherFaceIds expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.FotherFaceIds := MapJsonArray(Node); // *** FIXME ***
        end;
      'availability':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for availability expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.Favailability := MapJsonObject(Node); // *** FIXME ***
        end;
      'colorIdentity':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for colorIdentity expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.FcolorIdentity := MapJsonObject(Node); // *** FIXME ***
        end;
      'colors':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for colors expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.Fcolors := MapJsonObject(Node); // *** FIXME ***
        end;
      'foreignData':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for foreignData expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.FforeignData := MapJsonObject(Node); // *** FIXME ***
        end;
      'legalities':
        begin
          if not(Node.Kind = nkObject) then
            MemoMessage('TypeError for legalities expected nkObject got ' + JSONKindToString(Node))
          else
            // Rec.Flegalities := MapJsonObject(Node); // *** FIXME ***
        end;
      'printings':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for printings expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.Fprintings := MapJsonObject(Node); // *** FIXME ***
        end;
      'purchaseUrls':
        begin
          if not(Node.Kind = nkObject) then
            MemoMessage('TypeError for purchaseUrls expected nkObject got ' + JSONKindToString(Node))
          else
            // Rec.FpurchaseUrls := MapJsonObject(Node); // *** FIXME ***
        end;
      'rulings':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for rulings expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.Frulings := MapJsonObject(Node); // *** FIXME ***
        end;
      'subtypes':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for subtypes expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.Fsubtypes := MapJsonObject(Node); // *** FIXME ***
        end;
      'supertypes':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for supertypes expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.Fsupertypes := MapJsonObject(Node); // *** FIXME ***
        end;
      'types':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for types expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.Ftypes := MapJsonObject(Node); // *** FIXME ***
        end;
      'keywords':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for keywords expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.Fkeywords := MapJsonArray(Node); // *** FIXME ***
        end;
      'frameEffects':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for frameEffects expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.FframeEffects := MapJsonArray(Node); // *** FIXME ***
        end;
      'promoTypes':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for promoTypes expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.FpromoTypes := MapJsonArray(Node); // *** FIXME ***
        end;
      'variations':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for variations expected nkArray got ' + JSONKindToString(Node))
          else
            // Rec.Fvariations := MapJsonArray(Node); // *** FIXME ***
        end;
      'leadershipSkills':
        begin
          if not(Node.Kind = nkObject) then
            MemoMessage('TypeError for leadershipSkills expected nkObject got ' + JSONKindToString(Node))
          else
            // Rec.FleadershipSkills := MapJsonObject(Node); // *** FIXME ***
        end;
      else
          begin
            Txt := Chr(39) + Node.Name + Chr(39) + ':' + LineEnding +
            '  begin' + LineEnding +
            '    if not(Node.Kind = ' + JSONKindToString(Node) + ') then' + LineEnding +
            '      MemoMessage(' + Chr(39) +  'TypeError for ' +
              Node.Name + ' expected '  + JSONKindToString(Node) +
              ' got ' + Chr(39) + ' + JSONKindToString(Node)' + ')' + LineEnding +
            '    else' + LineEnding;
            if Node.Kind = nkString then
              Txt += '      Rec.F' + Node.Name + ' := Node.AsString;' + LineEnding
            else if Node.Kind = nkNumber then
              Txt += '      Rec.F' + Node.Name + ' := Trunc(Node.AsNumber);' + LineEnding
            else if Node.Kind = nkBool then
              Txt += '      Rec.F' + Node.Name + ' := Node.AsBoolean;' + LineEnding
            else if Node.Kind = nkObject then
              Txt += '      // Rec.F' + Node.Name + ' := MapJsonObject(Node); // *** FIXME ***' + LineEnding
            else if Node.Kind = nkArray then
              Txt += '      // Rec.F' + Node.Name + ' := MapJsonArray(Node); // *** FIXME ***' + LineEnding
            else
              Txt += '      Rec.F' + Node.Name + ' := Node.AsString; // *** FIXME ***' + LineEnding;
            Txt += '  end;';
            MemoMessage(Txt);

            propdec += WritePropertyDeclaration('set', Node);
            membdec += WriteMemberDeclaration(Node);
          end;
      end;
    end;

  if not(Ext = EmptyStr) then
    MemoMessage('====================' + LineEnding +
      'Unhandled (ToDo)' + LineEnding +
      '====================' + LineEnding +
      Ext);
  if not(membdec = EmptyStr) then
    MemoMessage('====================' + LineEnding +
      'Members' + LineEnding +
      '====================' + LineEnding +
      membdec);
  if not(propdec = EmptyStr) then
    MemoMessage('====================' + LineEnding +
      'Properties' + LineEnding +
      '====================' + LineEnding +
      propdec);
  Result := Key;
end;


function TMTGSet.MapJsonIdentifiersObject(const Json: TJsonNode): TSetCardIdentifiersRecord;
var
  Rec: TSetCardIdentifiersRecord;
  Node: TJsonNode;
  Txt: String;
  Ext: String;
  propdec: String;
  membdec: String;
begin
  Rec := TSetCardIdentifiersRecord.Create;

  Ext := '';
  propdec := '';
  membdec := '';

  for Node in Json do
    begin
      if Node.Kind = nkObject then
        begin
          Ext += Node.Name + ' -> Object' + LineEnding;
          continue;
        end;
      if Node.Kind = nkArray then
        begin
          Ext += Node.Name + ' -> Array' + LineEnding;
          continue;
        end;
      case Node.Name of
      'cardKingdomId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for cardKingdomId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FcardKingdomId := Node.AsString;
        end;
      'mcmId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for mcmId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FmcmId := Node.AsString;
        end;
      'mcmMetaId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for mcmMetaId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FmcmMetaId := Node.AsString;
        end;
      'mtgjsonV4Id':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for mtgjsonV4Id expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FmtgjsonV4Id := Node.AsString;
        end;
      'scryfallId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for scryfallId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FscryfallId := Node.AsString;
        end;
      'scryfallIllustrationId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for scryfallIllustrationId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FscryfallIllustrationId := Node.AsString;
        end;
      'scryfallOracleId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for scryfallOracleId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FscryfallOracleId := Node.AsString;
        end;
      'tcgplayerProductId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for tcgplayerProductId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FtcgplayerProductId := Node.AsString;
        end;
      'cardKingdomFoilId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for cardKingdomFoilId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FcardKingdomFoilId := Node.AsString;
        end;
      'mtgoId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for mtgoId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FmtgoId := Node.AsString;
        end;
      'multiverseId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for multiverseId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FmultiverseId := Node.AsString;
        end;
      'mtgArenaId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for mtgArenaId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FmtgArenaId := Node.AsString;
        end;
      'mtgoFoilId':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for mtgoFoilId expected nkString got ' + JSONKindToString(Node))
          else
            Rec.FmtgoFoilId := Node.AsString;
        end;
      else
          begin
            Txt := Chr(39) + Node.Name + Chr(39) + ':' + LineEnding +
            '  begin' + LineEnding +
            '    if not(Node.Kind = ' + JSONKindToString(Node) + ') then' + LineEnding +
            '      MemoMessage(' + Chr(39) +  'TypeError for ' +
              Node.Name + ' expected '  + JSONKindToString(Node) +
              ' got ' + Chr(39) + ' + JSONKindToString(Node)' + ')' + LineEnding +
            '    else' + LineEnding;
            if Node.Kind = nkString then
              Txt += '      Rec.F' + Node.Name + ' := Node.AsString;' + LineEnding
            else if Node.Kind = nkNumber then
              Txt += '      Rec.F' + Node.Name + ' := Trunc(Node.AsNumber);' + LineEnding
            else if Node.Kind = nkBool then
              Txt += '      Rec.F' + Node.Name + ' := Node.AsBoolean;' + LineEnding
            else if Node.Kind = nkObject then
              Txt += '      // Rec.F' + Node.Name + ' := MapJsonObject(Node); // *** FIXME ***' + LineEnding
            else if Node.Kind = nkArray then
              Txt += '      // Rec.F' + Node.Name + ' := MapJsonArray(Node); // *** FIXME ***' + LineEnding
            else
              Txt += '      F' + Node.Name + ' := Node.AsString; // *** FIXME ***' + LineEnding;
            Txt += '  end;';
            MemoMessage(Txt);

            propdec += WritePropertyDeclaration('set', Node);
            membdec += WriteMemberDeclaration(Node);
          end;
      end;
    end;
  if not(Ext = EmptyStr) then
    MemoMessage('====================' + LineEnding +
      'Unhandled (ToDo)' + LineEnding +
      '====================' + LineEnding +
      Ext);
  if not(membdec = EmptyStr) then
    MemoMessage('====================' + LineEnding +
      'Members' + LineEnding +
      '====================' + LineEnding +
      membdec);
  if not(propdec = EmptyStr) then
    MemoMessage('====================' + LineEnding +
      'Properties' + LineEnding +
      '====================' + LineEnding +
      propdec);
  Result := Rec;
end;

function TMTGSet.ImageID(const idx: Integer): String;
var
  Ret: String;
begin
  Ret := EmptyStr;

  if((idx >= 0) and (idx < FCards.Count)) then
    Ret := TSetCardIdentifiersRecord(TSetCardRecord(FCards.Objects[idx]).Fidentifiers).FscryfallId;

  Result := Ret;
end;

function TMTGSet.Name(const idx: Integer): String;
var
  Ret: String;
begin
  Ret := EmptyStr;

  if((idx >= 0) and (idx < FCards.Count)) then
    Ret := TSetCardRecord(FCards.Objects[idx]).Fname;

  Result := Ret;
end;

function TMTGSet.Number(const idx: Integer): String;
var
  Ret: String;
begin
  Ret := EmptyStr;

  if((idx >= 0) and (idx < FCards.Count)) then
    Ret := TSetCardRecord(FCards.Objects[idx]).Fnumber;

  Result := Ret;
end;

function TMTGSet.Side(const idx: Integer): String;
var
  Ret: String;
begin
  Ret := 'a';

  if((idx >= 0) and (idx < FCards.Count)) then
    if not(TSetCardRecord(FCards.Objects[idx]).Fside = EmptyStr) then
      Ret := TSetCardRecord(FCards.Objects[idx]).Fside;

  Result := Ret;
end;

function TMTGSet.Rarity(const idx: Integer): String;
var
  Ret: String;
begin
  Ret := EmptyStr;

  if((idx >= 0) and (idx < FCards.Count)) then
    Ret := TSetCardRecord(FCards.Objects[idx]).Frarity;

  Result := Ret;
end;

function TMTGSet.CardType(const idx: Integer): String;
var
  Ret: String;
begin
  Ret := EmptyStr;

  if((idx >= 0) and (idx < FCards.Count)) then
    Ret := TSetCardRecord(FCards.Objects[idx]).Ftype;

  Result := Ret;
end;

function TMTGSet.ExtractImageList: TStringList;
var
  URL: String;
  uuid: String;
  sImageList: TStringList;
  idx: Integer;
begin
  sImageList := TStringList.Create;
  sImageList.Delimiter := '|';

  for idx := 0 to FCards.Count - 1 do
    begin
      URL := TSetCardIdentifiersRecord(TSetCardRecord(FCards.Objects[idx]).Fidentifiers).FscryfallId;
      uuid := FCards[idx];
      sImageList.Add(URL + '|' + uuid);
    end;

  Result := sImageList;
end;

procedure TMTGSet.MapJsonObject(const Json: TJsonNode);
var
  Node: TJsonNode;
  Txt: String;
  Ext: String;
  propdec: String;
  membdec: String;
begin
  Ext := '';
  propdec := '';
  membdec := '';

  for Node in Json do
    begin
      case Node.Name of
      'dummyForPrototype':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isPartialPreview expected nkBool got ' + JSONKindToString(Node))
          else
            FisPartialPreview := Node.AsBoolean;
        end;
      else
          begin
            Txt := Chr(39) + Node.Name + Chr(39) + ':' + LineEnding +
            '  begin' + LineEnding +
            '    if not(Node.Kind = ' + JSONKindToString(Node) + ') then' + LineEnding +
            '      MemoMessage(' + Chr(39) +  'TypeError for ' +
              Node.Name + ' expected '  + JSONKindToString(Node) +
              ' got ' + Chr(39) + ' + JSONKindToString(Node)' + ')' + LineEnding +
            '    else' + LineEnding;
            if Node.Kind = nkString then
              Txt += '      Rec.F' + Node.Name + ' := Node.AsString;' + LineEnding
            else if Node.Kind = nkNumber then
              Txt += '      Rec.F' + Node.Name + ' := Trunc(Node.AsNumber);' + LineEnding
            else if Node.Kind = nkBool then
              Txt += '      Rec.F' + Node.Name + ' := Node.AsBoolean;' + LineEnding
            else if Node.Kind = nkObject then
              Txt += '      // Rec.F' + Node.Name + ' := MapJsonObject(Node); // *** FIXME ***' + LineEnding
            else if Node.Kind = nkArray then
              Txt += '      // Rec.F' + Node.Name + ' := MapJsonArray(Node); // *** FIXME ***' + LineEnding
            else
              Txt += '      F' + Node.Name + ' := Node.AsString; // *** FIXME ***' + LineEnding;
            Txt += '  end;';
            MemoMessage(Txt);

            propdec += WritePropertyDeclaration('set', Node);
            membdec += WriteMemberDeclaration(Node);
          end;
      end;
    end;
  if not(Ext = EmptyStr) then
    MemoMessage('====================' + LineEnding +
      'Unhandled (ToDo)' + LineEnding +
      '====================' + LineEnding +
      Ext);
  if not(membdec = EmptyStr) then
    MemoMessage('====================' + LineEnding +
      'Members' + LineEnding +
      '====================' + LineEnding +
      membdec);
  if not(propdec = EmptyStr) then
    MemoMessage('====================' + LineEnding +
      'Properties' + LineEnding +
      '====================' + LineEnding +
      propdec);
end;

procedure TMTGSet.MapJsonSetObject(const Json: TJsonNode);
var
  Node: TJsonNode;
  Txt: String;
begin
  for Node in Json do
    begin
      case Node.Name of
      'booster':
        begin
          if not(Node.Kind = nkObject) then
            MemoMessage('TypeError for booster expected nkString got ' + JSONKindToString(Node))
          else
            begin
            //
            //  MemoMessage('Boosters');
            //  MapJsonObject(Node); // ToDo
            end;
        end;
      'cards':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for cards expected nkString got ' + JSONKindToString(Node))
          else
            begin
              MapJsonCardArray(Node, False);
            end;
        end;
      'tokens':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for tokens expected nkString got ' + JSONKindToString(Node))
          else
            begin
              MapJsonCardArray(Node, True);
            end;
        end;
      'translations':
        begin
          if not(Node.Kind = nkObject) then
              MemoMessage('TypeError for translations expected nkString got ' + JSONKindToString(Node))
          else
            begin
            //  MapJsonObject(Node); // ToDo
            end;
        end;
      'baseSetSize':
        begin
          if not(Node.Kind = nkNumber) then
              MemoMessage('TypeError for baseSetSize expected nkNumber got ' + JSONKindToString(Node))
          else
            FbaseSetSize := Trunc(Node.AsNumber);
        end;
      'block':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for block expected nkString got ' + JSONKindToString(Node))
          else
            Fblock := Node.AsString;
        end;
      'code':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for code expected nkString got ' + JSONKindToString(Node))
          else
            Fcode := Node.AsString;
        end;
      'isFoilOnly':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isFoilOnly expected nkBool got ' + JSONKindToString(Node))
          else
            FisFoilOnly := Node.AsBoolean;
        end;
      'isOnlineOnly':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isOnlineOnly expected nkBool got ' + JSONKindToString(Node))
          else
            FisOnlineOnly := Node.AsBoolean;
        end;
      'keyruneCode':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for keyruneCode expected nkString got ' + JSONKindToString(Node))
          else
            FkeyruneCode := Node.AsString;
        end;
      'mtgoCode':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for mtgoCode expected nkString got ' + JSONKindToString(Node))
          else
            FmtgoCode := Node.AsString;
        end;
      'name':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for name expected nkString got ' + JSONKindToString(Node))
          else
            Fname := Node.AsString;
        end;
      'releaseDate':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for releaseDate expected nkString got ' + JSONKindToString(Node))
          else
            FreleaseDate := Node.AsString;
        end;
      'tcgplayerGroupId':
        begin
          if not(Node.Kind = nkNumber) then
            MemoMessage('TypeError for tcgplayerGroupId expected nkNumber got ' + JSONKindToString(Node))
          else
            FtcgplayerGroupId := Trunc(Node.AsNumber);
        end;
      'totalSetSize':
        begin
          if not(Node.Kind = nkNumber) then
            MemoMessage('TypeError for totalSetSize expected nkNumber got ' + JSONKindToString(Node))
          else
            FtotalSetSize := Trunc(Node.AsNumber);
        end;
      'type':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for type expected nkString got ' + JSONKindToString(Node))
          else
            Ftype := Node.AsString;
        end;
      'mcmId':
        begin
          if not(Node.Kind = nkNumber) then
            MemoMessage('TypeError for mcmId expected nkNumber got ' + JSONKindToString(Node))
          else
            FmcmId := Trunc(Node.AsNumber);
        end;
      'mcmName':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for mcmName expected nkString got ' + JSONKindToString(Node))
          else
            FmcmName := Node.AsString;
        end;
      'isNonFoilOnly':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isNonFoilOnly expected nkBool got ' + JSONKindToString(Node))
          else
            FisNonFoilOnly := Node.AsBoolean;
        end;
      'isForeignOnly':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isForeignOnly expected nkBool got ' + JSONKindToString(Node))
          else
            FisForeignOnly := Node.AsBoolean;
        end;
      'parentCode':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for parentCode expected nkString got ' + JSONKindToString(Node))
          else
            FparentCode := Node.AsString;
        end;
      'isPartialPreview':
        begin
          if not(Node.Kind = nkBool) then
            MemoMessage('TypeError for isPartialPreview expected nkBool got ' + JSONKindToString(Node))
          else
            FisPartialPreview := Node.AsBoolean;
        end;
      else
          begin
            Txt := Chr(39) + Node.Name + Chr(39) + ':' + LineEnding +
            '  begin' + LineEnding +
            '    if not(Node.Kind = ' + JSONKindToString(Node) + ') then' + LineEnding +
            '      MemoMessage(' + Chr(39) +  'TypeError for ' +
              Node.Name + ' expected '  + JSONKindToString(Node) +
              ' got ' + Chr(39) + ' + JSONKindToString(Node)' + ')' + LineEnding +
            '    else' + LineEnding;
            if Node.Kind = nkString then
              Txt += '      F' + Node.Name + ' := Node.AsString;' + LineEnding
            else if Node.Kind = nkNumber then
              Txt += '      F' + Node.Name + ' := Trunc(Node.AsNumber);' + LineEnding
            else if Node.Kind = nkBool then
              Txt += '      F' + Node.Name + ' := Node.AsBoolean;' + LineEnding
            else if Node.Kind = nkObject then
              Txt += '      // Rec.F' + Node.Name + ' := MapJsonObject(Node); // *** FIXME ***' + LineEnding
            else if Node.Kind = nkArray then
              Txt += '      // Rec.F' + Node.Name + ' := MapJsonArray(Node); // *** FIXME ***' + LineEnding
            else
              Txt += '      F' + Node.Name + ' := Node.AsString; // *** FIXME ***' + LineEnding;
            Txt += '  end;';
            MemoMessage(Txt);
          end;
      end;
    end;
end;

procedure TMTGSet.DumpList;
begin
  MemoMessage('==== Set Dump (TSetRecord) ====');
  MemoMessage('setBaseSetSize: ' + IntToStr(setBaseSetSize));
  MemoMessage('setBlock: ' + setBlock);
  MemoMessage('setCode: ' + setCode);
  if setIsFoilOnly then
    MemoMessage('setIsFoilOnly: True')
  else
    MemoMessage('setIsFoilOnly: True');
  if setIsOnlineOnly then
    MemoMessage('setIsOnlineOnly: True')
  else
    MemoMessage('setIsOnlineOnly: True');
  MemoMessage('setKeyruneCode: ' + setKeyruneCode);
  MemoMessage('setMtgoCode: ' + setMtgoCode);
  MemoMessage('setName: ' + setName);
  MemoMessage('setReleaseDate: ' + setReleaseDate);
  MemoMessage('setTcgplayerGroupId: ' + IntToStr(setTcgplayerGroupId));
  MemoMessage('setTotalSetSize: ' + IntToStr(setTotalSetSize));
  MemoMessage('setType: ' + setType);
  MemoMessage('setMcmId: ' + IntToStr(setMcmId));
  MemoMessage('setMcmName: ' + setMcmName);
  if setIsNonFoilOnly then
    MemoMessage('setIsNonFoilOnly: True')
  else
    MemoMessage('setIsNonFoilOnly: True');
  if setIsForeignOnly then
    MemoMessage('setIsForeignOnly: True')
  else
    MemoMessage('setIsForeignOnly: True');
  MemoMessage('setParentCode: ' + setParentCode);
  if setIsPartialPreview then
    MemoMessage('setIsPartialPreview: True')
  else
    MemoMessage('setIsPartialPreview: True');
  MemoMessage('===============================');
end;

{=======================================================================}

function GetMTGJsonSetJson(SetCode: String; Path: String; UseCache: Boolean = True): TStream;
var
  data: TStream = nil;
  URI: String;
  SaveAs: String;
begin
  Result := nil;
  URI := MTGJSON_SET_PREFIX_URI + SetCode + MTGJSON_SET_POSTFIX_URI;
  if CreateCastleDataDirectoryIfMissing(Path) then
    begin
      CreateCastleDataDirectoryIfMissing(Path);
      SaveAs := Path + '/set_' + SetCode + '.json';
      data := CacheData(URI, SaveAs, UseCache);
      if not(data = nil) then
        begin
          Result := data;
        end;
    end
  else
    MemoMessage('Failed creating ' + Path);
end;


end.

