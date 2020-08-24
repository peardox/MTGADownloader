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
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
  public
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

{ TDownloadObjectQueue ------------------------------------------------------------ }

function TCastleObjectQueue.GetCapacity: Integer;
begin
  Result := List.Capacity;
end;

procedure TCastleObjectQueue.SetCapacity(const Value: Integer);
begin
  List.Capacity := Value;
end;

end.

