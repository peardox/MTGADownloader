program mtgapp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  Console,
  CastleParameters, CastleClassUtils, CastleDownload,
  CastleTimeUtils, CastleURIUtils, CastleFilesUtils,
  CastleLog, fpjson, 
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
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

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
    GetSets(False);
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

