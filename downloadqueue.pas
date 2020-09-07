unit DownloadQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation
  uses CastleClassUtils;

type

  TDownloadObjectQueue = class(TCastleObjectQueue)
  private
  public
  end;

  TDownloadObjectItem = class(TObject)
  private
    FURI: String;
    FSaveAs: String;
    FCompleted: Boolean;
    FRetries: Integer;
    FTickStamp: Int64;
    FElapsed: Int64;
    FDownload: TCastleDownload;
  public
    property URI: String read FURI write FURI;
    property SaveAs: String read FSaveAs write FSaveAs;
    property Elapsed: Int64 read FElapsed write FElapsed;
  end;

{ TDownloadObjectQueue ------------------------------------------------------ }

{ TDownloadItem ------------------------------------------------------------- }

function TCastleObjectQueue.GetCapacity: Integer;
begin
  Result := List.Capacity;
end;

procedure TCastleObjectQueue.SetCapacity(const Value: Integer);
begin
  List.Capacity := Value;
end;

end.

