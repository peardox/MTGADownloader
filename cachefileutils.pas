unit CacheFileUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$ifndef VER3_0} OpenSSLSockets, {$endif}
  CastleDownload
//  CastleURIUtils, CastleFilesUtils
;

function DownloadNetworkFile(const URI: String; const sOptions: TStreamOptions = []; const UsingCache: Boolean = False): TStream;
function CacheData(const URI: String; const FileName: String; const LoadFromCache: Boolean = False; FreeResult: Boolean = False): TStream;
function LoadCachedData(const FileName: String): TStream;
function URIDirectoryExists(const Path: String): Boolean;
function URIDirectoryCreate(const Url: String): Boolean;
function URIExcludeQuery(const Url: String): String;
function CreateCastleDataDirectoryIfMissing(const SubDir: String): Boolean;

implementation

uses
  Unit1,
  Dialogs,
  JsonTools, TypInfo,
  CastleFilesUtils, CastleURIUtils
  ;

function URIExcludeQuery(const Url: String): String;
var
  pos: Integer;
begin
  pos := Url.IndexOf('?');
  if pos > 0 then
    Result := Url.Remove(pos)
  else
    Result := Url;
end;

function URIDirectoryCreate(const Url: String): Boolean;
var
  F: String;
begin
  F := URIToFilenameSafe(Url);
  if F = '' then
    raise Exception.CreateFmt('URL "%s" is not a "file" URL, cannot create directories yet', [URIDisplay(Url)])
  else
    Result := CreateDir(F);
end;

function URIDirectoryExists(const Path: String): Boolean;
begin
  Result := URIExists(Path) in [ueDirectory];
end;

function CreateCastleMkDir(const SubDir: String): Boolean;
begin
  Result := False;

  if URIDirectoryExists('castle-data:/' + SubDir) then
    Result := True
  else
    if URIDirectoryCreate('castle-data:/' + SubDir) then
      Result := True;
end;

function CreateCastleDataDirectoryIfMissing(const SubDir: String): Boolean;
var
  SubDirs: TStringArray;
  Path: String;
  i: Integer;
begin
  Result := True;
  Path := Trim(SubDir);

  if not(Path = EmptyStr) then
    begin
      SubDirs := Path.Split(['/'], TStringSplitOptions.ExcludeEmpty);

      for i := 0 to Length(SubDirs) - 1 do
        begin
          if i = 0 then
            Path := SubDirs[i]
          else
            Path += '/' + SubDirs[i];

          if not(CreateCastleMkDir(Path)) then
            begin
             Result := False;
             Break;
            end;
        end;
    end
  else
    Result := False;
end;

function DownloadNetworkFile(const URI: String; const sOptions: TStreamOptions = []; const UsingCache: Boolean = False): TStream;
var
  stream: TStream;
  {$ifdef growstream}
  strOutput: TMemoryStream;
  {$endif}
begin
  EnableBlockingDownloads := True;

  {$ifdef growstream}
  strOutput:= TMemoryStream.Create;
  {$endif}
  try
    try
      stream := Download(URI, sOptions);
      {$ifdef growstream}
      ReadGrowingStream(stream, strOutput, false);
      {$endif}
    except
        on E : Exception do
          begin
            if not UsingCache then
              begin
                ShowMessage('Oops' + LineEnding +
                             E.ClassName + LineEnding +
                             E.Message);
                end;
              {$ifdef growstream}
              FreeAndNil(stream);
              strOutput := nil;
              {$else}
              stream := nil;
              {$endif}
           end;
      end;
  finally
    {$ifdef growstream}
    FreeAndNil(stream);
    Result := strOutput;
    {$else}
    Result := stream;
    {$endif}
  end;
end;

function LoadCachedData(const FileName: String): TStream;
var
  data: TStream = nil;
begin
  data := DownloadNetworkFile('castle-data:/' + FileName, [soForceMemoryStream], True);
  if not(data = nil) then
    begin
      MemoMessage('Loaded data from ' + URIToFilenameSafe('castle-data:/' + FileName));
    end
  else
    begin
      MemoMessage('No cached data!!!');
    end;

  Result := data;
end;

function CacheData(const URI: String; const FileName: String; const LoadFromCache: Boolean = False; FreeResult: Boolean = False): TStream;
var
  data: TStream = nil;
begin
  if LoadFromCache then
    begin
      if URIFileExists('castle-data:/' + FileName) then
        begin
          data := LoadCachedData(FileName);
        end
      else
        begin
          data := DownloadNetworkFile(URI, [soGzip, soForceMemoryStream]);
          if not(data = nil) then
            begin
              MemoMessage('Saving to data' + PathDelim + FileName);
              StreamSaveToFile(data, 'castle-data:/' + FileName);
            end;
        end;
    end
  else
    begin
      data := DownloadNetworkFile(URI, [soGzip, soForceMemoryStream]);
      if not(data = nil) then
        begin
          MemoMessage('Saving to data' + PathDelim + FileName);
          StreamSaveToFile(data, 'castle-data:/' + FileName);
        end
      else
        begin
          data := LoadCachedData(FileName);
        end;
    end;

  if FreeResult then
    FreeAndNil(data);

  Result := data;
end;

{
ApplicationDataOverride
ApplicationConfig

function DownloadNetworkFileSynchronous(const URI: String; const sOptions: TStreamOptions = []; const UsingCache: Boolean = False): TStream;
begin

  Download[I] := TCastleDownload.Create(Self);
  Download[I].Url := Urls[I];
  Download[I].OnFinish := @DownloadFinish;
  Download[I].Options := [soForceMemoryStream];
  Download[I].Start;
end;
}

end.

