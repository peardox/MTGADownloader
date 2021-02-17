program mtgapp;

{$mode objfpc}{$H+}
{$define hackmode}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  Console,
  CastleParameters, CastleClassUtils, CastleDownload,
  CastleTimeUtils, CastleURIUtils, CastleFilesUtils,
  CastleLog, fpjson, AssetGatherers,
  CacheFileUtils;

type

  { TMTGApp }

  TMTGApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

var
  Application: TMTGApp;

{ TMTGApp }

procedure TMTGApp.DoRun;
var
{$ifndef hackmode}
  ErrorMsg: String;
{$endif}
  fn: String;
  FinalURL: String;
  UnixTime: Int64;
  ProcTime: Int64;
begin
  FinalURL := EmptyStr;
  UnixTime := 0;
{$ifndef hackmode}
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help','r','rebuild');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;
{$endif}
  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    MemoMessage('');
    MemoMessage('ApplicationConfig = ' + ApplicationConfig(''));
    MemoMessage('ApplicationData = ' + ApplicationData(''));
    Terminate;
    Exit;
  end;

  if HasOption('r', 'rebuild') then begin
    MemoMessage('Rebuilding Cards and Decks...');
    GetDecks(False);
    GetSets(False);
    Terminate;
    Exit;
  end;

  if HasOption('c', 'cards') then begin
    MemoMessage('Rebuilding Cards...');
    GetSets(False);
    Terminate;
    Exit;
  end;

  if HasOption('d', 'decks') then begin
    MemoMessage('Rebuilding Decks...');
    GetDecks(False);
    Terminate;
    Exit;
  end;

  if HasOption('i', 'images') then begin
    MemoMessage('Fetching images...');
    ExportImages(True, False);
    Terminate;
    Exit;
  end;

  if HasOption('u', 'uuids') then begin
    MemoMessage('Fetching UUIDs...');
    ExportUUIDs('castle-data:/uuids.csv');
    MemoMessage('Finished Exporting');
    Terminate;
    Exit;
  end;

  if HasOption('t', 'test') then begin
    ProcTime := CastleGetTickCount64;
    MemoMessage('Testing download...');
    fn := 'https://api.scryfall.com/cards/b3b7a69c-75d2-49a6-ab56-ef608d0b0208?format=image&version=large&face=front';
    MemoMessage('InputURL : ' + fn);
    DirectDownloadNetworkFile(fn, FinalURL, UnixTime, True);
    ProcTime := CastleGetTickCount64 - ProcTime;
    MemoMessage('FinalURL : ' + FinalURL);
    MemoMessage('UnixTime : ' + IntToStr(UnixTime));
    MemoMessage('ProcTime : ' + FloatToStr(ProcTime / 1000));
    MemoMessage('Finished test');
    Terminate;
    Exit;
  end;

  if HasOption('s', 'acryfall') then begin
    MemoMessage('Fetching Scryfall data...');
    GetScryfallIcons(SCRYFALL_SETS_URI, 'scryfall/sets/icons', 'scryfall_sets.json', 'icon_svg_uri');
    GetScryfallIcons(SCRYFALL_SYMBOLOGY_URI, 'scryfall/symbols/icons', 'scryfall_symbology.json', 'svg_uri');
    Terminate;
    Exit;
  end;

  { add your program here }

//  MemoMessage('ApplicationConfig = ' + ApplicationConfig(''));
//  MemoMessage('ApplicationData = ' + ApplicationData(''));
//  InitializeLog;

  // stop program loop
  Terminate;
end;

constructor TMTGApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMTGApp.Destroy;
begin
  inherited Destroy;
end;

procedure TMTGApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

begin
  Application:=TMTGApp.Create(nil);
  Application.Title:='MTGApp';
  Application.Run;
  Application.Free;
end.

