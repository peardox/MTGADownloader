unit MTGJsonObjects;

{$mode objfpc}{$H+}
{$define useprototype}

interface

uses
  Classes, SysUtils, JsonTools;

type
  TMTGList = class(TPersistent)
    private
      FDataUri: String;
      FDataFileName: String;
      FKey: String;
    protected
      procedure MapJsonArray(const Json: TJsonNode);
      procedure MapJsonObject(const Json: TJsonNode);
      procedure ProcessList(Stream: TStream);
      procedure ProcessListArrayDataObject(const Json: TJsonNode); virtual;
      procedure ProcessListObjectDataObject(const Json: TJsonNode); virtual;
    public
      constructor Create(const DataUri: String; const DataFileName: String; Key: String = '');
      constructor Create(const Stream: TStream; Key: String = '');
      destructor Destroy; override;
      procedure DumpList; virtual;
    published
  end;

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
    published
      property List: TStringLIst read FList write FList;
  end;

  TDeckListRecord = class(TPersistent)
  private
    Fcode: String;
    Fname: String;
    FfileName: String;
    FreleaseDate: String;
    Ftype: String;
  published
    property deckCode: String read Fcode write Fcode;
    property deckName: String read Fname write Fname;
    property deckFileName: String read FfileName write FfileName;
    property deckReleaseDate: String read FreleaseDate write FreleaseDate;
    property deckType: String read Ftype write Ftype;
  end;

  TMTGDeckList = class(TMTGList)
    private
      FList: TStringList;
    protected
      function MapJsonObject(const Json: TJsonNode; out Rec: TDeckListRecord): String;
      procedure ProcessListArrayDataObject(const Json: TJsonNode); override;
    public
      destructor Destroy; override;
      procedure DumpList; override;
    published
      property List: TStringLIst read FList write FList;
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
      FList: TStringList;
    protected
//      function MapJsonObject(const Json: TJsonNode; out Rec: TSetListRecord): String;
//      procedure ProcessListArrayDataObject(const Json: TJsonNode); override;
      procedure MapJsonSetObject(const Json: TJsonNode);
      procedure ProcessListObjectDataObject(const Json: TJsonNode); override;
    public
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
      property List: TStringLIst read FList write FList;
  end;

const
  MTGJSON_SET_PREFIX_URI = 'https://mtgjson.com/api/v5/';
  MTGJSON_SET_POSTFIX_URI = '.json.gz';
  MTGJSON_DECK_PREFIX_URI = 'https://mtgjson.com/api/v5/decks/';
  MTGJSON_DECK_POSTFIX_URI = '.json.gz';

  function GetMTGJsonSetJson(SetCode: String; Path: String): TStream;
  function GetMTGJsonDeckJson(SetCode: String; Path: String): TStream;

implementation

uses
  unit1, Typinfo, CacheFileUtils;

function JSONKindToString(Node: TJsonNode): string;
begin
  result := GetEnumName(TypeInfo(TJsonNodeKind), ord(Node.&Kind));
end;

{ TMTGList ===================================================================}

constructor TMTGList.Create(const Stream: TStream; Key: String = '');
begin
  inherited Create;
  FKey := Key;
  if not(Stream = nil) then
    begin
      ProcessList(Stream);
    end;
end;

constructor TMTGList.Create(const DataUri: String; const DataFileName: String; Key: String = '');
var
  Stream: TStream;
begin
  inherited Create;
  FDataUri := DataUri;
  FDataFileName := DataFileName;
  FKey := Key;
  try
  Stream := CacheData(FDataUri, FDataFileName, True);
  if not(Stream = nil) then
    begin
      ProcessList(Stream);
    end;
  finally
    FreeAndNil(Stream);
  end;
end;

destructor TMTGList.Destroy;
begin
  inherited Destroy;
end;

procedure TMTGList.ProcessList(Stream: TStream);
var
  Json: TJsonNode;
  Node: TJsonNode;
begin
  if ClassName = 'TMTGList' then
    MemoMessage('ProcessList');
  Json := TJsonNode.Create;
  try
    try
        Json.LoadFromStream(Stream);
        for Node in Json do
          begin
            if ClassName = 'TMTGList' then
              MemoMessage(Node.Name + ' - ' + JSONKindToString(Node));
            if (Node.Name = 'data') then
              begin
                if (Node.Kind = nkArray) then
                  ProcessListArrayDataObject(Node)
                else if (Node.Kind = nkObject) then
                  ProcessListObjectDataObject(Node)
                else
                  MemoMessage('PANIC = The world has ended - not an Array or Object');
              end;
          end;
      except
        on E : Exception do
          begin
            MemoMessage('Oops' + LineEnding +
                         E.ClassName + LineEnding +
                         E.Message);
           end;
      end;
  finally
    FreeAndNil(Json);
  end;
end;

procedure TMTGList.ProcessListArrayDataObject(const Json: TJsonNode);
var
  Node: TJsonNode;
  First: Boolean = True;
begin
  if ClassName = 'TMTGList' then
    MemoMessage('ProcessListArrayDataObject');
  for Node in Json do
    begin
      if First then
        begin
          if ClassName = 'TMTGList' then
            MemoMessage('=>' + Node.Name + ' - ' + JSONKindToString(Node));
          MapJsonObject(Node);
        end;
      First := False;
    end;
end;

procedure TMTGList.ProcessListObjectDataObject(const Json: TJsonNode);
begin
  if ClassName = 'TMTGList' then
    MemoMessage('ProcessListObjectDataObject');
  MapJsonObject(Json);
end;

procedure TMTGList.MapJsonArray(const Json: TJsonNode);
begin
  MemoMessage('MapJsonArray ' + IntToStr(Json.Count));
end;

procedure TMTGList.MapJsonObject(const Json: TJsonNode);
  var
    Node: TJsonNode;
  begin
  MemoMessage('--------------- members ---------------');
  for Node in Json do
    begin
      if Node.Kind = nkObject then
        begin
          MemoMessage(Node.Name + ' -> Object');
          break;
        end;
      if Node.Kind = nkArray then
        begin
          MemoMessage(Node.Name + ' -> Array');
          break;
        end;
      case Node.Name of
      'dummyForPrototype':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError');
            end;
        end;
      else
          begin
            MemoMessage(Chr(39) + Node.Name + Chr(39) + ':' + LineEnding +
            '  begin' + LineEnding +
            '    if not(Node.Kind = ' + JSONKindToString(Node) + ') then' + LineEnding +
            '      begin' + LineEnding +
            '        MemoMessage(' + Chr(39) +  'TypeError for ' +
              Node.Name + ' expected '  + JSONKindToString(Node) +
              ' got ' + Chr(39) + ' + JSONKindToString(Node)' + ');' + LineEnding +
            '      end;' + LineEnding +
            '  end;');
          end;
      end;
    end;
  MemoMessage('--------------- members ---------------');
end;

procedure TMTGList.DumpList;
begin
  // Dummy
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
      FList.AddObject(Key, Rec);
    end;
end;

function TMTGSetList.MapJsonObject(const Json: TJsonNode; out Rec: TSetListRecord): String;
var
  Node: TJsonNode;
  Key: String;
begin
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

{ TMTGDeckList ===============================================================}

destructor TMTGDeckList.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TMTGDeckList.ProcessListArrayDataObject(const Json: TJsonNode);
var
  idx: Integer;
  Rec: TDeckListRecord;
  Key: String;
begin
  FList := TStringList.Create;
  FList.OwnsObjects := True;
  FList.Sorted := True;
  FList.Duplicates := dupError;

  for idx := 0 to Json.Count - 1 do
    begin
      try
        Key := MapJsonObject(Json.Child(idx), Rec);
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

function TMTGDeckList.MapJsonObject(const Json: TJsonNode; out Rec: TDeckListRecord): String;
var
  Node: TJsonNode;
  Key: String;
begin
  Rec := TDeckListRecord.Create;

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
      'code':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for code expected nkString got ' + JSONKindToString(Node))
          else
            Rec.deckCode := Node.AsString;
        end;
      'name':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for name expected nkString got ' + JSONKindToString(Node))
          else
            Rec.deckName := Node.AsString;
        end;
      'fileName':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for name expected nkString got ' + JSONKindToString(Node))
          else
            Rec.deckFileName := Node.AsString;
        end;
      'releaseDate':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for releaseDate expected nkString got ' + JSONKindToString(Node))
          else
            Rec.deckReleaseDate := Node.AsString;
        end;
      'type':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for type expected nkString got ' + JSONKindToString(Node))
          else
            Rec.deckType := Node.AsString;
        end;
      else
          begin
            MemoMessage('Unhandled node : ' + Node.Name + ' - ' + JSONKindToString(Node));
          end;
      end;
    end;

  Result := Key;
end;

procedure TMTGDeckList.DumpList;
var
  idx: Integer;
  Rec: TDeckListRecord;
begin
  for idx := 0 to FList.Count -1 do
    begin
      MemoMessage('Key: ' + FList[idx]);
      Rec := FList.Objects[idx] as TDeckListRecord;
      MemoMessage('  deckCode: ' + Rec.deckCode);
      MemoMessage('  deckName: ' + Rec.deckName);
      MemoMessage('  deckFileName: ' + Rec.deckFileName);
      MemoMessage('  deckReleaseDate: ' + Rec.deckReleaseDate);
      MemoMessage('  deckType: ' + Rec.deckType);
    end;
end;

{ TMTGSet ====================================================================}

destructor TMTGSet.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TMTGSet.ProcessListObjectDataObject(const Json: TJsonNode);
begin
  if ClassName = 'TMTGSet' then
    MemoMessage('ProcessListObjectDataObject (TMTGSet)');
  MapJsonSetObject(Json);
end;

procedure TMTGSet.MapJsonSetObject(const Json: TJsonNode);
var
  Node: TJsonNode;
begin
  MemoMessage('--------------- members ---------------');
  for Node in Json do
    begin
      case Node.Name of
      'booster':
        begin
          if not(Node.Kind = nkObject) then
            MemoMessage('TypeError for booster expected nkString got ' + JSONKindToString(Node))
          else
            begin
            //  MapJsonObject(Node); // ToDo
            end;
        end;
      'cards':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for cards expected nkString got ' + JSONKindToString(Node))
          else
            begin
              MapJsonArray(Node); // ToDo
            end;
        end;
      'tokens':
        begin
          if not(Node.Kind = nkArray) then
            MemoMessage('TypeError for tokens expected nkString got ' + JSONKindToString(Node))
          else
            begin
              MapJsonArray(Node); // ToDo
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
      else
          begin
            MemoMessage(Chr(39) + Node.Name + Chr(39) + ':' + LineEnding +
            '  begin' + LineEnding +
            '    if not(Node.Kind = ' + JSONKindToString(Node) + ') then' + LineEnding +
            '      begin' + LineEnding +
            '        MemoMessage(' + Chr(39) +  'TypeError for ' +
              Node.Name + ' expected '  + JSONKindToString(Node) +
              ' got ' + Chr(39) + ' + JSONKindToString(Node)' + ');' + LineEnding +
            '      end;' + LineEnding +
            '  end;');
          end;
      end;
    end;
  MemoMessage('--------------- members ---------------');
end;

procedure TMTGSet.DumpList;
begin
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
end;


{=======================================================================}

function GetMTGJsonSetJson(SetCode: String; Path: String): TStream;
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
      data := CacheData(URI, SaveAs, True);
      if not(data = nil) then
        begin
          Result := data;
        end;
    end
  else
    MemoMessage('Failed creating ' + Path);
end;

function GetMTGJsonDeckJson(SetCode: String; Path: String): TStream;
var
  data: TStream = nil;
  URI: String;
  SaveAs: String;
begin
  Result := nil;
  URI := MTGJSON_DECK_PREFIX_URI + SetCode + MTGJSON_DECK_POSTFIX_URI;
  if CreateCastleDataDirectoryIfMissing(Path) then
    begin
      CreateCastleDataDirectoryIfMissing(Path);
      SaveAs := Path + '/deck_' + SetCode + '.json';
      data := CacheData(URI, SaveAs, True);
      if not(data = nil) then
        begin
          Result := data;
        end;
    end
  else
    MemoMessage('Failed creating ' + Path);
end;

end.

