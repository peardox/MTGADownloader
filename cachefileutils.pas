unit CacheFileUtils;

{$mode objfpc}{$H+}
// {$define debugMessages}
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
  Unit1, // To communicate with main form
  Dialogs,
  JsonTools, TypInfo,
  CastleFilesUtils, CastleURIUtils
  ;

function DownloadStatusToString(Status: TDownloadStatus): string;
begin
  result := GetEnumName(TypeInfo(TDownloadStatus), ord(Status));
end;

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

procedure ReportProgress(const Download: TCastleDownload);
begin
  if Download.TotalBytes > 0 then
    begin
      if Download.DownloadedBytes < Download.TotalBytes then
        MemoMessage(DownloadStatusToString(Download.Status) + ' - Downloaded '  + IntToStr(Download.DownloadedBytes) + ' of ' + IntToStr(Download.TotalBytes))
      else
        MemoMessage(DownloadStatusToString(Download.Status) + ' - Downloaded : ' + IntToStr(Download.TotalBytes));
    end
  else
    MemoMessage(DownloadStatusToString(Download.Status) + ' - Downloaded '  + IntToStr(Download.DownloadedBytes) + ' of (UnKnown)')
end;

function DownloadNetworkFile(const URI: String; const sOptions: TStreamOptions = []; const UsingCache: Boolean = False): TStream;
var
  fDownload: TCastleDownload;
begin
  EnableAbortButton(True);
  Result := nil;
  fDownload := TCastleDownload.Create(nil);
  try
    fDownload.HttpHeader('User-Agent', 'https://github.com/peardox/MTGADownloader');
    fDownload.Url := URI;
    fDownload.Options := sOptions;
    fDownload.OwnsContents := False;
    fDownload.Start;
    fDownload.WaitForFinish;

    try
      while fDownload.Status = dsDownloading do
      begin
        ReportProgress(fDownload);
        TriggerProcessMessages;
        if Abort then
          begin
            EnableAbortButton(False);
            break;
          end;
        sleep(100);
      end;

      if UsingCache then
        MemoMessage('Loaded from cache ' + URI)
      else
        MemoMessage('Downloaded ' + URI);

      case fDownload.Status of
        dsSuccess:
          Result := fDownload.Contents;
        dsError:
          MemoMessage('Download Error : ' + fDownload.ErrorMessage);
        else
          MemoMessage('Download Status : ' + DownloadStatusToString(fDownload.Status));
      end;
      except
        on E : Exception do
          begin
            if not UsingCache then
              begin
                ShowMessage('Oops' + LineEnding +
                            'Trying to download : ' + URI + LineEnding +
                             E.ClassName + LineEnding +
                             E.Message);
                end;
           end;
      end;
  finally
    Abort := False;
    FreeAndNil(fDownload);
  end;
end;

function LoadCachedData(const FileName: String): TStream;
var
  data: TStream;
begin
  data := DownloadNetworkFile('castle-data:/' + FileName, [soForceMemoryStream], True);
  if not(data = nil) then
    begin
      {$if defined(debugMessages)}
      MemoMessage('Loaded data from ' + URIToFilenameSafe('castle-data:/' + FileName));
      {$endif}
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
              {$if defined(debugMessages)}
              MemoMessage('Saving to data' + PathDelim + FileName);
              {$endif}
              StreamSaveToFile(data, 'castle-data:/' + FileName);
            end;
        end;
    end
  else
    begin
      data := DownloadNetworkFile(URI, [soGzip, soForceMemoryStream]);
      if not(data = nil) then
        begin
          {$if defined(debugMessages)}
          MemoMessage('Saving to data' + PathDelim + FileName);
          {$endif}
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

