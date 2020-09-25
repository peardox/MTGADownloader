unit MTGJsonObjects;

{$mode objfpc}{$H+}
{$define useprototype}

interface

uses
  Classes, SysUtils, JsonTools;

type
  TMTGList = class(TPersistent)
    private
    protected
      FDataUri: String;
      FDataFileName: String;
      FKey: String;
      procedure MapJsonArray(const Json: TJsonNode);
      procedure MapJsonObject(const Json: TJsonNode); virtual;
      procedure ProcessList(Stream: TStream); virtual;
      procedure ProcessListArrayDataObject(const Json: TJsonNode); virtual;
      procedure ProcessListObjectDataObject(const Json: TJsonNode); virtual;
    public
      constructor Create(const DataUri: String; const DataFileName: String; Key: String = ''; UseCache: Boolean = True);
      constructor Create(const Stream: TStream; Key: String = '');
      destructor Destroy; override;
      procedure DumpList; virtual;
    published
  end;

  TMTGPriceList = class(TMTGList)
    private
      fNext: Boolean;
    protected
      procedure ProcessList(Stream: TStream); override;
      procedure ProcessListObjectDataObject(const Json: TJsonNode); override;
      procedure MapJsonObject(const Json: TJsonNode); override;
    public
    published
  end;

  function JSONKindToString(Node: TJsonNode): string;
  function UppercaseFirstChar(s: String): String;
  function WriteMemberDeclaration(Node: TJsonNode): String;
  function WritePropertyDeclaration(Prefix: String; Node: TJsonNode): String;

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

constructor TMTGList.Create(const DataUri: String; const DataFileName: String; Key: String = ''; UseCache: Boolean = True);
var
  Stream: TStream;
begin
  inherited Create;
  FDataUri := DataUri;
  FDataFileName := DataFileName;
  FKey := Key;
  try
  Stream := CacheData(FDataUri, FDataFileName, UseCache);
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
//  MemoMessage('MapJsonArray ' + IntToStr(Json.Count));
end;

procedure TMTGList.MapJsonObject(const Json: TJsonNode);
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

  MemoMessage('--------------- members ---------------');
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
      'dummyForPrototype':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError');
            end;
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
  MemoMessage('--------------- members ---------------');
end;

procedure TMTGList.DumpList;
begin
  // Dummy
end;

{ TMTGPriceList ==============================================================}

procedure TMTGPriceList.ProcessList(Stream: TStream);
var
  Json: TJsonNode;
  Node: TJsonNode;
begin
  fNext := False;
  if ClassName = 'TMTGPriceList' then
    MemoMessage('TMTGPriceList.ProcessList');
  Json := TJsonNode.Create;
  try
    try
        Json.LoadFromStream(Stream);
        for Node in Json do
          begin
            if ClassName = 'TMTGTMTGPriceListList' then
              MemoMessage(Node.Name + ' - ' + JSONKindToString(Node));
            if (Node.Name = 'data') then
              begin
                if (Node.Kind = nkObject) then
                  begin
                    if not fNext then
                      begin
                        ProcessListObjectDataObject(Node);
                        fNext := True;
                      end;
                  end
                else
                  MemoMessage('PANIC = The world has ended - not an Object');
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

procedure TMTGPriceList.ProcessListObjectDataObject(const Json: TJsonNode);
begin
  if ClassName = 'TMTGPriceList' then
    MemoMessage('ProcessListObjectDataObject');
  MapJsonObject(Json);
end;

procedure TMTGPriceList.MapJsonObject(const Json: TJsonNode);
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

  MemoMessage('--------------- members ---------------');
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
      'dummyForPrototype':
        begin
          if not(Node.Kind = nkString) then
            begin
              MemoMessage('TypeError');
            end;
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
  MemoMessage('--------------- members ---------------');
end;

{ ============================================================================}

function UppercaseFirstChar(s: String): String;
var
  ch, rest: String;
begin
  ch := Copy(s, 1, 1);
  rest := Copy(s, Length(ch)+1, MaxInt);
  Result := Uppercase(ch) + rest
end;

function WriteMemberDeclaration(Node: TJsonNode): String;
begin
  if Node.Kind = nkString then
    Result := 'F' + Node.Name + ': String;' + LineEnding
  else if Node.Kind = nkNumber then
    Result := 'F' + Node.Name + ': Integer;' + LineEnding
  else if Node.Kind = nkBool then
    Result := 'F' + Node.Name + ': Boolean;' + LineEnding
  else
    Result := 'F' + Node.Name + ': String; // *** FIXME ***' + LineEnding;
end;

function WritePropertyDeclaration(Prefix: String; Node: TJsonNode): String;
begin
  if Node.Kind = nkString then
    Result := 'property ' + Prefix + UppercaseFirstChar(Node.Name) + ': String read F' + Node.Name + ' write F' + Node.Name + ';' + LineEnding
  else if Node.Kind = nkNumber then
    Result := 'property ' + Prefix + UppercaseFirstChar(Node.Name) + ': Integer read F' + Node.Name + ' write F' + Node.Name + ';' + LineEnding
  else if Node.Kind = nkBool then
    Result := 'property ' + Prefix + UppercaseFirstChar(Node.Name) + ': Boolean read F' + Node.Name + ' write F' + Node.Name + ';' + LineEnding
  else
    Result := 'property ' + Prefix + UppercaseFirstChar(Node.Name) + ': String read F' + Node.Name + ' write F' + Node.Name + '; // *** FIXME ***' + LineEnding;
end;

end.

