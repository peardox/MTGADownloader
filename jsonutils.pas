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

uses unit1;

procedure TJsonObject.IncCount;
begin
  Inc(fCount);
end;

function JSONKindToString(Node: TJsonNode): string;
begin
  result := GetEnumName(TypeInfo(TJsonNodeKind), ord(Node.&Kind));
end;

function JSONKindAsString(Node: TJsonNodeKind): string;
begin
  result := GetEnumName(TypeInfo(TJsonNodeKind), ord(Node));
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
      dataType := CurrentNode.Kind; // JSONKindAsString();
      if not Keys.Find(CurrentNode.Name, idx) then
        begin
          MemoMessage('=> ' + CurrentNode.Name + ' (' + JSONKindAsString(dataType) + ')');
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
                  MemoMessage(CurrentNode.Name + ' type changed from ' + JSONKindAsString(oJson.Kind) + ' to ' + JSONKindAsString(dataType));
                  ChangedFlag := True;
                  oJson.Kind := dataType;
                  oJson.TypeUpgrade := True
                end
              // nkBool can upgrade to nkNumber, nkString or nkArray
              else if ((oJson.Kind = nkBool) and ((dataType = nkNumber) or (dataType = nkString) or (dataType = nkArray))) then
                begin
                  MemoMessage(CurrentNode.Name + ' type changed from ' + JSONKindAsString(oJson.Kind) + ' to ' + JSONKindAsString(dataType));
                  ChangedFlag := True;
                  oJson.Kind := dataType;
                  oJson.TypeUpgrade := True
                end
              // nkNumber can upgrade to nkString or nkArray
              else if ((oJson.Kind = nkNumber) and ((dataType = nkString) or (dataType = nkArray))) then
                begin
                  MemoMessage(CurrentNode.Name + ' type changed from ' + JSONKindAsString(oJson.Kind) + ' to ' + JSONKindAsString(dataType));
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
                        ' marked as ' + JSONKindAsString(oJson.Kind) + ' found ' + JSONKindAsString(dataType))
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
        ' (' + JSONKindAsString(oJson.Kind) + ') = ' + IntToStr(oJson.Count));
    end;
  MemoMessage('============================');
end;

end.

