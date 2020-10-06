unit MTGJsonPriceObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JsonTools, MTGJsonObjects;

type
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

const
  MTGJSON_PRICE_PREFIX_URI = 'https://api.peardox.co.uk/prices/json/sets/paper.';
  MTGJSON_PRICE_POSTFIX_URI ='.json.gz';

  function GetMTGJsonPriceJson(SetCode: String; Path: String; UseCache: Boolean = True): TStream;

implementation

uses
  unit1, CacheFileUtils;

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
              MemoMessage(Node.Name + ' - ' + Node.KindAsString);
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
            '    if not(Node.Kind = ' + Node.KindAsString + ') then' + LineEnding +
            '      MemoMessage(' + Chr(39) +  'TypeError for ' +
              Node.Name + ' expected '  + Node.KindAsString +
              ' got ' + Chr(39) + ' + Node.KindAsString' + ')' + LineEnding +
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

{=======================================================================}

function GetMTGJsonPriceJson(SetCode: String; Path: String; UseCache: Boolean = True): TStream;
var
  data: TStream = nil;
  URI: String;
  SaveAs: String;
begin
  Result := nil;
  URI := MTGJSON_PRICE_PREFIX_URI + SetCode + MTGJSON_PRICE_POSTFIX_URI;
  if CreateCastleDataDirectoryIfMissing(Path) then
    begin
      CreateCastleDataDirectoryIfMissing(Path);
      SaveAs := Path + '/price_' + SetCode + '.json';
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

