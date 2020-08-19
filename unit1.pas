unit Unit1;

{$mode objfpc}{$H+}
//{$define build_wotc}
// {$define build_mtool}
// {$define build_scryfall}
{$define build_mtgjson}

// {$define wotc_cards_only}
{$define usepeardox}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, md5,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  CastleDownload, CastleParameters, CastleClassUtils,
  CastleControl, CastleTimeUtils, CastleLog, CastleFilesUtils,
  TypInfo, JsonTools
  ;

type

  { TForm1 }

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

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public

  end;

var
  Form1: TForm1;
  have_manifest_hash: String;
  have_mtool_version: String;

const
//  WOTC_MANIFEST_URI = 'https://mtgarena.downloads.wizards.com/Live/Windows32/versionxxx';
  WOTC_MANIFEST_URI = 'https://mtgarena.downloads.wizards.com/Live/Windows32/version';
  WOTC_ASSETS_URI = 'https://assets.mtgarena.wizards.com/';
  WOTC_ASSETS_EXT = '.mtga';
  // Available endpoints... en, es, br, de, fr, it, jp, ru, ko-kr, zh-cn
  MTOOL_DATABASE_URI = 'https://mtgatool.com/database/en';
  MTOOL_VERSION_URI = 'https://mtgatool.com/database/latest/en';
  SCRYFALL_SET_URI = 'https://api.scryfall.com/sets';
  SCRYFALL_SYMBOL_URI = 'https://api.scryfall.com/symbology';
  {$ifndef usepeardox}
  MTGJSON_ENUMS_URI = 'https://mtgjson.com/api/v5/EnumValues.json.gz';
  MTGJSON_SETLIST_URI = 'https://mtgjson.com/api/v5/SetList.json.gz';
  MTGJSON_DECKLIST_URI = 'https://mtgjson.com/api/v5/DeckList.json.gz';
  {$else}
  MTGJSON_ENUMS_URI = 'https://peardox.com/api/v5/EnumValues.json.gz';
  MTGJSON_SETLIST_URI = 'https://peardox.com/api/v5/SetList.json.gz';
  MTGJSON_DECKLIST_URI = 'https://peardox.com/api/v5/DeckList.json.gz';
  {$endif}
implementation

{$R *.lfm}

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

procedure MemoMessage(msg: String; slen: Integer = 80);
begin
{
  if length(msg) > 80 then
    Form1.Memo1.Lines.Add(Copy(msg, 0, slen))
  else
}
    Form1.Memo1.Lines.Add(msg);
    Application.ProcessMessages;
end;

function DownloadNetworkFile(URI: String; sOptions: TStreamOptions = []): String;
var
  stream: TStream;
  strOutput: TStringStream;
begin
  Result := EmptyStr;

  try
    strOutput:= TStringStream.Create;
    try
      stream := Download(URI, sOptions);
      ReadGrowingStream(stream, strOutput, false);
    except
        on E : Exception do
          begin
          ShowMessage('Error : ' + E.Message);
          stream := nil;
          FreeAndNil(strOutput);
          end;
    end;
  finally
    if strOutput <> nil then
      begin
        Result := StreamToString(strOutput);
        FreeAndNil(strOutput);
      end;
    if stream <> nil then
      begin
        FreeAndNil(stream);
      end;
  end;
end;

function DownloadNetworkStream(URI: String; sOptions: TStreamOptions = []): TStream;
var
  stream: TStream;
  strOutput: TStream;
begin
  strOutput:= TStream.Create;
  try
    stream := Download(URI, sOptions);
    try
      ReadGrowingStream(stream, strOutput, false);
    except
        on E : Exception do
          begin
          ShowMessage(E.Message);
          Result := nil;
          end;
    end;
  finally
    FreeAndNil(stream);
    Result := strOutput;
  end;
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
          WritelnLog('=> ' + CurrentNode.Name + ' (' + JSONKindAsString(dataType) + ')');
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
                  WritelnLog(CurrentNode.Name + ' type changed from ' + JSONKindAsString(oJson.Kind) + ' to ' + JSONKindAsString(dataType));
                  ChangedFlag := True;
                  oJson.Kind := dataType;
                  oJson.TypeUpgrade := True
                end
              // nkBool can upgrade to nkNumber, nkString or nkArray
              else if ((oJson.Kind = nkBool) and ((dataType = nkNumber) or (dataType = nkString) or (dataType = nkArray))) then
                begin
                  WritelnLog(CurrentNode.Name + ' type changed from ' + JSONKindAsString(oJson.Kind) + ' to ' + JSONKindAsString(dataType));
                  ChangedFlag := True;
                  oJson.Kind := dataType;
                  oJson.TypeUpgrade := True
                end
              // nkNumber can upgrade to nkString or nkArray
              else if ((oJson.Kind = nkNumber) and ((dataType = nkString) or (dataType = nkArray))) then
                begin
                  WritelnLog(CurrentNode.Name + ' type changed from ' + JSONKindAsString(oJson.Kind) + ' to ' + JSONKindAsString(dataType));
                  ChangedFlag := True;
                  oJson.Kind := dataType;
                  oJson.TypeUpgrade := True
                end
              else
                begin
                  if ((dataType <> nkNull) and (not oJson.TypeUpgrade)) then
                    begin
                      ChangedFlag := True;
                      WritelnLog(CurrentNode.Name + ' type mismatch ' +
                        ' marked as ' + JSONKindAsString(oJson.Kind) + ' found ' + JSONKindAsString(dataType))
                    end;
                end;
            end;
        end;
    end;

  Result := ChangedFlag;
end;

procedure print_scryfall_schema(Keys: TStringList; name: String);
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

function create_scryfall_schema(data: String; key: String): TStringList;
var
  Json: TJsonNode;
  Child: TJsonNode;
  Rec: TJsonNode;
  Keys: TStringList;
begin
  Keys := TStringList.Create;
  Keys.Sorted := True;
  Keys.OwnsObjects := True;

  Result := nil;

  Json := TJsonNode.Create;
  try
    Json.Parse(data);
    for Child in Json do
      begin
        if (Child.Name = key) and (Child.Kind = nkArray) then
          begin
            for Rec in Child do
              begin
                if json_create_schema(Rec, Keys) then
                  begin
                    // if not First then
                  end;
              end;
            Result := Keys;
          end;
      end;
  finally
    Json.Free();
  end;
end;

procedure process_mtool_database(data: String; name: String);
var
  Json: TJsonNode;
  CurrentNode: TJsonNode;
  CardNode: TJsonNode;
  Keys: TStringList;
begin
  Keys := TStringList.Create;
  Keys.Sorted := True;
  Keys.OwnsObjects := True;

  WritelnLog('Analysing ' + name);

  Json := TJsonNode.Create;

  try
    if Json.TryParse(data) then
      begin
        CardNode := Json.Find('cards');
        for CurrentNode in CardNode do
          begin
            if json_create_schema(CurrentNode.AsObject, Keys) then
              begin
              end;
          end;

        print_scryfall_schema(Keys, name);
      end;
  finally
    Json.Free;
  end;

  Keys.Free();
end;

function cache_file(Asset: String; Uri: String; sOptions: TStreamOptions = []): String;
var
  contents: String;
begin
  Result := EmptyStr;

  contents := DownloadNetworkFile(Uri, sOptions);
  if not(contents = EmptyStr) then
    begin
      StringToFile('castle-data:/' + Asset + '.json', contents);
      Result := contents;
    end;

end;

function find_in_json_loc(data: String; key: String; value: String): TJsonNode;
var
  Json: TJsonNode;
  Element: TJsonNode;
  Child: TJsonNode;
begin
  Result := nil;

  Json := TJsonNode.Create;
  try
    Json.Parse(data);
    for Element in Json do
      for Child in Element do
        begin
          if (Child.Name = key) and (Child.AsString = value) then
            begin
              Result := TJsonNode.Create;
              Result.Parse(Element.ToString);
              WritelnLog('Found ' + key + ' = ' + value);
              Exit;
            end;
        end;
  finally
    WritelnLog('Freeing Json');
    Json.Free();
  end;
end;

procedure process_wotc_loc_data(data: String; name: String);
var
  Node: TJsonNode;
  CurrentNode: TJsonNode;
  ArrayNode: TJsonNode;
  Keys: TStringList;
  oJson: TJsonObject;
  idx: Integer;
  First: Boolean = True;
begin
  Keys := TStringList.Create;
  Keys.Sorted := True;
  Keys.OwnsObjects := True;

  WritelnLog('Loc data');
  if not(data = EmptyStr) then
    begin
      Node := find_in_json_loc(data, 'isoCode', 'en-US');
      try
        if ((Node <> nil) and (Node.Kind = nkObject)) then
          begin
            for CurrentNode in Node do
              begin
                if ((CurrentNode.Name = 'keys')  and (CurrentNode.Kind = nkArray)) then
                  begin
                    for ArrayNode in CurrentNode do
                      begin
                        if json_create_schema(ArrayNode, Keys) then
                          begin
                            if not First then
                              WritelnLog('In rec #' + ArrayNode.Name);
                          end;
                        if First then
                          First := False;
                      end;
                  end;
              end;

            print_scryfall_schema(Keys, name);
          end;
      finally
        Node.Free;
      end;
    end;
  Keys.Free();
end;

procedure process_wotc_array_data(data: String; title: String);
var
  Node: TJsonNode;
  CurrentNode: TJsonNode;
  Keys: TStringList;
  oJson: TJsonObject;
  idx: Integer;
  First: Boolean = True;
begin
  Keys := TStringList.Create;
  Keys.Sorted := True;
  Keys.OwnsObjects := True;

  WritelnLog(title);
  if not(data = EmptyStr) then
    begin
      Node := TJsonNode.Create;
      if Node.TryParse(data) then
        begin
          try
            if not (Node = nil) then
              begin
                for CurrentNode in Node do
                  begin
                    if json_create_schema(CurrentNode, Keys) then
                      begin
                        if not First then
                          WritelnLog('In rec #' + CurrentNode.Name);
                      end;
                    if First then
                      First := False;
                  end;

                WritelnLog('');
                WritelnLog('Keys found in ' + title);
                WritelnLog('============================');
                for idx := 0 to Keys.Count -1 do
                  begin
                    oJson := Keys.Objects[idx] as TJsonObject;
                    WritelnLog(IntToStr(idx) + ' : ' + Keys[idx] +
                      ' (' + JSONKindAsString(oJson.Kind) + ') = ' + IntToStr(oJson.Count));
                  end;
                WritelnLog('============================');

              end;
          finally
            Node.Free;
          end;
        end;
    end;
  Keys.Free();
end;

procedure process_wotc_asset(asset: String; useCache: Boolean = False);
var
  data: String;
begin
{$ifndef wotc_cards_only}
  if asset.StartsWith('data_loc') then
    begin
      data := cache_file('data_loc', WOTC_ASSETS_URI + asset, [soGzip]);
      process_wotc_loc_data(data, 'data_loc');
    end
  else
{$endif}
  if asset.StartsWith('data_cards') then
    begin
      data := cache_file('data_cards', WOTC_ASSETS_URI + asset, [soGzip]);
      process_wotc_array_data(data, 'data_cards');
{$ifndef wotc_cards_only}
    end
  else if asset.StartsWith('data_abilities') then
    begin
      data := cache_file('data_abilities', WOTC_ASSETS_URI + asset, [soGzip]);
      process_wotc_array_data(data, 'data_abilities');
    end
  else if asset.StartsWith('data_altFlavorTexts') then
    begin
      data := cache_file('data_altFlavorTexts', WOTC_ASSETS_URI + asset, [soGzip]);
    end
  else if asset.StartsWith('data_altArtCredits') then
    begin
      data := cache_file('data_altArtCredits', WOTC_ASSETS_URI + asset, [soGzip]);
    end
  else if asset.StartsWith('data_enums') then
    begin
      data := cache_file('data_enums', WOTC_ASSETS_URI + asset, [soGzip]);
      process_wotc_array_data(data, 'data_enums');
    end
  else if asset.StartsWith('data_prompts') then
    begin
      data := cache_file('data_prompts', WOTC_ASSETS_URI + asset, [soGzip]);
{$endif}
    end;
end;

procedure process_wotc_manifest_asset_node(Json: TJsonNode; useCache: Boolean = False);
var
  CurrentNode: TJsonNode;
  FileName: String;
  ProcessFlag: Boolean;
begin
  ProcessFlag := false;

  for CurrentNode in Json do
    begin
      if ((CurrentNode.Name = 'AssetType') and (CurrentNode.AsString = 'Data')) then
        begin
          ProcessFlag := True;
        end;
      if CurrentNode.Name = 'Name' then
        begin
          FileName := CurrentNode.AsString;
        end;
      if ProcessFlag and (not (FileName = EmptyStr)) then
        begin
          WritelnLog('--== Process ' + FileName + ' ==--');
          process_wotc_asset(FileName, useCache);
          Exit;
        end;
    end;
end;

procedure process_wotc_manifest(manifest: String; useCache: Boolean = False);
var
  Json: TJsonNode;
  AssetNode:  TJsonNode;
  CurrentNode: TJsonNode;
begin
  Json := TJsonNode.Create;

  try
    WritelnLog('In process_wotc_manifest');

    if Json.TryParse(manifest) then
      begin
        AssetNode := Json.Find('Assets');
        for CurrentNode in AssetNode do
          begin
            process_wotc_manifest_asset_node(CurrentNode, useCache);
          end;
      end;
  finally
    Json.Free;
  end;
end;

function get_wotc_manifest(hash: String): String;
var
  manifest_uri: String;
  manifest: String;
  checkHash: String;
begin
  WritelnLog('In get_wotc_manifest');
  WritelnLog('Manifest : ' + hash);

  manifest_uri := WOTC_ASSETS_URI + 'Manifest_' + hash + WOTC_ASSETS_EXT;
  Form1.Memo1.Lines.Add('manifest_uri : ' + manifest_uri);
  manifest := DownloadNetworkFile(manifest_uri, [soGzip]);
  checkHash := MD5Print(MD5String(manifest));
  Form1.Memo1.Lines.Add('MANIFEST CHECKHASH : ' + checkHash);
  Result := manifest;
end;

function getManifestHash: String;
var
  Json: TJsonNode;
  TmpNode: TJsonNode;
  CurrentNode: TJsonNode;
  TmpStr: String;
  manifest_version_major: String;
  manifest_version_minor: String;
  version_parts: TStringArray;
  manifest_header: String;
  manifest_lines:  TStringList;
  manifest_hash: String;
  manifest_uri: String;
  idx: Integer;
begin
  WritelnLog('In getManifestHash');

  manifest_lines := TStringList.Create;
  manifest_hash := '';
  idx := 0;
  Json := TJsonNode.Create;

  try
    manifest_header := DownloadNetworkFile(WOTC_MANIFEST_URI);
    if manifest_header <> EmptyStr then
      begin
        if Json.TryParse(manifest_header) then
          begin
            TmpNode := Json.Find('Versions');
            for CurrentNode in TmpNode do
              begin
                Inc(idx);
                if idx = 1 then
                  TmpStr := CurrentNode.Name;
              end;
            if idx > 0 then
              begin
                version_parts := TmpStr.Split('.');
                if Length(version_parts) = 4 then
                  begin
                    manifest_version_major := version_parts[2];
                    manifest_version_minor := version_parts[3];
                    manifest_uri := WOTC_ASSETS_URI + 'External_' + manifest_version_major + '_' + manifest_version_minor + WOTC_ASSETS_EXT;
                    manifest_hash := DownloadNetworkFile(manifest_uri);

                    manifest_lines.Text := manifest_hash;

                    manifest_hash := '';
                    if manifest_lines.Count > 0 then
                      begin
                        manifest_hash := manifest_lines[manifest_lines.Count - 1];
                      end;

                  end;
              end;
          end;
      end;
  finally
    Json.Free;
    manifest_lines.Free;
  end;

  Result := manifest_hash;
end;

procedure process_scryfall_data(data: String; name: String);
var
  Keys: TStringList;
begin
  MemoMessage(name);

  if not(data = EmptyStr) then
    begin
      Keys := create_scryfall_schema(data, 'data');
      print_scryfall_schema(Keys, name);
  end;
  Keys.Free();
end;

function getScryfallSets: String;
var
  data: String;
begin
  try
    data := DownloadNetworkFile(SCRYFALL_SET_URI);
  finally
  end;

  Result := data;
end;

function getScryfallSymbols: String;
var
  data: String;
begin
  try
    data := DownloadNetworkFile(SCRYFALL_SYMBOL_URI);
  finally
  end;

  Result := data;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeLog;
  EnableNetwork := True;
  EnableBlockingDownloads := True;
  Memo1.Clear;
  Form1.Width := 800;
  Form1.Height := 600;
  Form1.Position:= poScreenCenter;
  have_manifest_hash := '';
  have_mtool_version := '72';
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Json: TJsonNode;
  mtool_version: String = '';
  mtool_version_data: String = '';
  mtool_database: String = '';
  data: String = '';
  manifest_data: String = '';
  manifest_hash: String = '';
  scryfall_sets: String = '';
  scryfall_symbols: String = '';
  ticks: Int64;
begin
  ticks := CastleGetTickCount64;
  Memo1.Clear;
  Button1.Enabled := False;

{$ifdef build_wotc}
  Memo1.Lines.Add('------ WOTC MANIFEST ------');
  manifest_hash := getManifestHash;
  Memo1.Lines.Add('MANIFEST HASH : ' + manifest_hash);
  if manifest_hash <> EmptyStr then
    begin
      if manifest_hash <> have_manifest_hash then
        begin
          manifest_data := get_wotc_manifest(manifest_hash);
          StringToFile('castle-data:/' + 'manifest.json', manifest_data);
          WritelnLog('Call process_wotc_manifest');
          process_wotc_manifest(manifest_data);
        end
      else
        begin
          if FileExists(PathDelim + 'manifest.json') then
            begin
            WritelnLog('Reading data' + PathDelim + 'manifest.json');
            manifest_data := FileToString('castle-data:/' + 'manifest.json');
            WritelnLog('Call process_wotc_manifest');
            process_wotc_manifest(manifest_data);
            end
          else
            Memo1.Lines.Add('Missing manifest.json');
        end;
    end
  else
    begin
      Memo1.Lines.Add('Can''t read MANIFEST HASH');
    end;
{$endif}
{$ifdef build_mtool}
  Json := TJsonNode.Create;
  try
    Memo1.Lines.Add('---------- MTOOL ----------');
    mtool_version_data := DownloadNetworkFile(MTOOL_VERSION_URI);
    Memo1.Lines.Add(mtool_version_data);

    if Json.TryParse(mtool_version_data) then
      begin
        mtool_version := Json.Find('latest').AsString;
        if not (mtool_version = have_mtool_version) then
//        if not FileExists('castle-data:/' + 'mtool.json') then
          begin
            mtool_database := DownloadNetworkFile(MTOOL_DATABASE_URI);
            Memo1.Lines.Add('Saving to data' + PathDelim + 'mtool.json');
            StringToFile('castle-data:/' + 'mtool.json', mtool_database);
            process_mtool_database(mtool_database, 'mtool_database');
          end
        else
          begin
            mtool_database := FileToString('castle-data:/' + 'mtool.json');
            if not (mtool_database = EmptyStr) then
              begin
                process_mtool_database(mtool_database, 'mtool_database');
              end
            else
              Memo1.Lines.Add('No data');
          end;
      end
    else
      begin
        mtool_version := '';
      end;
    Memo1.Lines.Add('MTOOL Version ' + mtool_version);
    finally
      Json.Free;
    end;
{$endif}
{$ifdef build_scryfall}
  Memo1.Lines.Add('---------- SCRYFALL ----------');
  scryfall_sets := getScryfallSets;
  if scryfall_sets <> EmptyStr then
    begin
      StringToFile('castle-data:/' + 'scryfall_sets.json', scryfall_sets);
      process_scryfall_data(scryfall_sets, 'scryfall_sets');
    end;

  scryfall_symbols := getScryfallSymbols;
  if scryfall_symbols <> EmptyStr then
    begin
      StringToFile('castle-data:/' + 'scryfall_symbols.json', scryfall_symbols);
      process_scryfall_data(scryfall_symbols, 'scryfall_symbols');
    end;
{$endif}

{$ifdef build_mtgjson}
  Memo1.Lines.Add('---------- MTGJSON ----------');
  data := DownloadNetworkFile(MTGJSON_ENUMS_URI, [soGzip]);
  Memo1.Lines.Add('Saving to data' + PathDelim + 'mtgjson_enums.json');
  StringToFile('castle-data:/' + 'mtgjson_enums.json', data);

  data := DownloadNetworkFile(MTGJSON_SETLIST_URI, [soGzip]);
  Memo1.Lines.Add('Saving to data' + PathDelim + 'mtgjson_setlist.json');
  StringToFile('castle-data:/' + 'mtgjson_setlist.json', data);

  data := DownloadNetworkFile(MTGJSON_DECKLIST_URI, [soGzip]);
  Memo1.Lines.Add('Saving to data' + PathDelim + 'mtgjson_decklist.json');
  StringToFile('castle-data:/' + 'mtgjson_decklist.json', data);
{$endif}

  ticks := CastleGetTickCount64 - ticks;
  Memo1.Lines.Add('----------- END -----------');
  Memo1.Lines.Add('Time : ' + IntToStr(ticks) + 'ms');
  Button1.Enabled := True;
end;

end.

