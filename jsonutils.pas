unit JsonUtils;

{$mode objfpc}{$H+}
//  Json: TJsonNode;

interface

uses
  Classes, SysUtils, TypInfo, JsonTools;

type
  TJsonObject = class
    fKind: TJsonNodeKind;
    fTypeUpgrade: Boolean;
    fCount: Integer;
  public
    procedure IncCount;
    property Kind: TJsonNodeKind read fKind write fKind;
    property TypeUpgrade: Boolean read fTypeUpgrade write fTypeUpgrade;
    property Count: Integer read fCount write fCount;
  end;

implementation

uses
{$ifndef cgeapp}
  Unit1
{$else}
, CacheFileUtils;
{$endif}

procedure TJsonObject.IncCount;
begin
  Inc(fCount);
end;

function json_create_schema(Json: TJsonNode; var Keys: TStringList): Boolean;
var
  CurrentNode: TJsonNode;
  ChangedFlag: Boolean;
  idx: Integer;
  dataType: TJsonNodeKind;
  oJson: TJsonObject;
begin
  ChangedFlag := false;
  for CurrentNode in Json do
    begin
      dataType := CurrentNode.Kind;
      if not Keys.Find(CurrentNode.Name, idx) then
        begin
          MemoMessage('=> ' + CurrentNode.Name + ' (' + dataType.KindAsString + ')');
          oJson := TJsonObject.Create;
          oJson.Kind := dataType;
          oJson.Count := 1;
          Keys.AddObject(CurrentNode.Name, oJson);
          end
      else
        begin
          oJson := Keys.Objects[idx] as TJsonObject;
          oJson.IncCount;
          if dataType <> oJson.Kind then
            begin
               // nkNull can upgrade to anything
              if oJson.Kind = nkNull then
                begin
                  MemoMessage(CurrentNode.Name + ' type changed from ' + oJson.Kind.KindAsString + ' to ' + dataType.KindAsString);
                  ChangedFlag := True;
                  oJson.Kind := dataType;
                  oJson.TypeUpgrade := True
                end
              // nkBool can upgrade to nkNumber, nkString or nkArray
              else if ((oJson.Kind = nkBool) and ((dataType = nkNumber) or (dataType = nkString) or (dataType = nkArray))) then
                begin
                  MemoMessage(CurrentNode.Name + ' type changed from ' + oJson.Kind.KindAsString + ' to ' + dataType.KindAsString);
                  ChangedFlag := True;
                  oJson.Kind := dataType;
                  oJson.TypeUpgrade := True
                end
              // nkNumber can upgrade to nkString or nkArray
              else if ((oJson.Kind = nkNumber) and ((dataType = nkString) or (dataType = nkArray))) then
                begin
                  MemoMessage(CurrentNode.Name + ' type changed from ' + oJson.Kind.KindAsString + ' to ' + dataType.KindAsString);
                  ChangedFlag := True;
                  oJson.Kind := dataType;
                  oJson.TypeUpgrade := True
                end
              else
                begin
                  if ((dataType <> nkNull) and (not oJson.TypeUpgrade)) then
                    begin
                      ChangedFlag := True;
                      MemoMessage(CurrentNode.Name + ' type mismatch ' +
                        ' marked as ' + oJson.Kind.KindAsString + ' found ' + dataType.KindAsString)
                    end;
                end;
            end;
        end;
    end;

  Result := ChangedFlag;
end;

procedure json_print_schema(Keys: TStringList; name: String);
var
  oJson: TJsonObject;
  idx: Integer;
begin
  MemoMessage('');
  MemoMessage('Keys found in ' + name);
  MemoMessage('============================');
  for idx := 0 to Keys.Count -1 do
    begin
      oJson := Keys.Objects[idx] as TJsonObject;
      MemoMessage(IntToStr(idx) + ' : ' + Keys[idx] +
        ' (' + oJson.Kind.KindAsString + ') = ' + IntToStr(oJson.Count));
    end;
  MemoMessage('============================');
end;

end.

