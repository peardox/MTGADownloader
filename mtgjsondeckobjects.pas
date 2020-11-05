unit mtgJsonDeckObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JsonTools, MTGJsonObjects;

type
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
      function MapJsonDeckObject(const Json: TJsonNode; out Rec: TDeckListRecord): String;
      procedure ProcessListArrayDataObject(const Json: TJsonNode); override;
    public
      destructor Destroy; override;
      procedure DumpList; override;
    published
      property List: TStringList read FList write FList;
  end;

const
  MTGJSON_DECK_PREFIX_URI = 'https://mtgjson.com/api/v5/decks/';
  MTGJSON_DECK_POSTFIX_URI = '.json.gz';

  function GetMTGJsonDeckJson(SetCode: String; Path: String; UseCache: Boolean = True): TStream;

implementation

uses 
{$ifndef cgeapp}
  Unit1,
{$endif}
CacheFileUtils;

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
        Key := MapJsonDeckObject(Json.Child(idx), Rec);
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

function TMTGDeckList.MapJsonDeckObject(const Json: TJsonNode; out Rec: TDeckListRecord): String;
var
  Node: TJsonNode;
  Key: String;
begin
  Key := '';
  Rec := TDeckListRecord.Create;

  for Node in Json do
    begin
      if Node.Name = FKey then
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for code expected nkString got ' + Node.KindAsString)
          else
            Key := Node.AsString;
        end;
      case Node.Name of
      'code':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for code expected nkString got ' + Node.KindAsString)
          else
            Rec.deckCode := Node.AsString;
        end;
      'name':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for name expected nkString got ' + Node.KindAsString)
          else
            Rec.deckName := Node.AsString;
        end;
      'fileName':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for name expected nkString got ' + Node.KindAsString)
          else
            Rec.deckFileName := Node.AsString;
        end;
      'releaseDate':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for releaseDate expected nkString got ' + Node.KindAsString)
          else
            Rec.deckReleaseDate := Node.AsString;
        end;
      'type':
        begin
          if not(Node.Kind = nkString) then
            MemoMessage('TypeError for type expected nkString got ' + Node.KindAsString)
          else
            Rec.deckType := Node.AsString;
        end;
      else
          begin
            MemoMessage('(MapJsonDeckObject) Unhandled node : ' + Node.Name + ' - ' + Node.KindAsString);
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

function GetMTGJsonDeckJson(SetCode: String; Path: String; UseCache: Boolean = True): TStream;
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

