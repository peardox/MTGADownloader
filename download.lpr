program download;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}{$ENDIF}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, castle_components, Unit1, AssetGatherers, CacheFileUtils,
  mtgJsonSetObjects, MTGJsonPriceObjects;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

