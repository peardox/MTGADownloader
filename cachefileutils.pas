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
function CacheImage(const URI: String; const FileName: String; const LoadFromCache: Boolean = False; FreeResult: Boolean = False): TStream;
function LoadCachedData(const FileName: String): TStream;
function URIDirectoryExists(const Path: String): Boolean;
function URIDirectoryCreate(const Url: String): Boolean;
function URIExcludeQuery(const Url: String): String;
function CreateCastleDataDirectoryIfMissing(const SubDir: String): Boolean;

implementation

uses
  Unit1, // To communicate with main form
  Dialogs, // ShowMessage
  JsonTools, TypInfo,
  CastleFilesUtils, CastleURIUtils,
  CastleApplicationProperties { A work-around }
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
  {$if defined(debugMessages)}
  {
  if Download.TotalBytes > 0 then
    begin
      if Download.DownloadedBytes < Download.TotalBytes then
        MemoMessage(DownloadStatusToString(Download.Status) + ' - Downloaded '  + IntToStr(Download.DownloadedBytes) + ' of ' + IntToStr(Download.TotalBytes))
      else
        MemoMessage(DownloadStatusToString(Download.Status) + ' - Downloaded : ' + IntToStr(Download.TotalBytes));
    end
  else
    MemoMessage(DownloadStatusToString(Download.Status) + ' - Downloaded '  + IntToStr(Download.DownloadedBytes) + ' of (UnKnown)')
  }
  {$endif}
end;

function DownloadNetworkFile(const URI: String; const sOptions: TStreamOptions = []; const UsingCache: Boolean = False): TStream;
var
  fDownload: TCastleDownload;
  LastModified: String;
begin
  EnableAbortButton(True);
  Result := nil;

  fDownload := TCastleDownload.Create(nil);
  fDownload.HttpHeader('User-Agent', 'https://github.com/peardox/MTGADownloader');
  fDownload.Url := URI;
  fDownload.OwnsContents := True;
  fDownload.Options := sOptions;

  fDownload.Start;
  try
    try
      while fDownload.Status = dsDownloading do
      begin
        ReportProgress(fDownload);
        TriggerProcessMessages;
        ApplicationProperties._Update;
        if Abort then
          begin
            EnableAbortButton(False);
            break;
          end;
        sleep(100);
      end;

      if fDownload.Status = dsSuccess then
        begin
        Result := fDownload.Contents;
        if not(UsingCache) then
          LastModified := fDownload.HttpResponseHeaders.Values['Last-Modified'];
        {$if defined(debugMessages)}
        if UsingCache then
          MemoMessage('Loaded from cache ' + URI)
        else
          begin
            MemoMessage('Downloaded : ' + URI);
            MemoMessage('HttpResponseCode : ' + IntToStr(fDownload.HttpResponseCode));
            MemoMessage('LastModified : ' + LastModified);
          end;
        {$endif}

        if not UsingCache then
          begin
            if(fDownload.HttpResponseCode <> 200)  then
              begin
              MemoMessage('Error Downloading : ' + URI);
              MemoMessage('HttpResponseCode : ' + IntToStr(fDownload.HttpResponseCode));
              end;
           end;
        end
      else
        begin
          MemoMessage('Error Downloading : ' + URI);
          MemoMessage('HttpResponseCode : ' + IntToStr(fDownload.HttpResponseCode));
        end;

      except
        on E : Exception do
          begin
            {$if defined(debugMessages)}
            MemoMessage('Oops' + LineEnding +
                        'Trying to download : ' + URI + LineEnding +
                        'HttpResponseCode : ' + IntToStr(fDownload.HttpResponseCode) + LineEnding +
                         E.ClassName + LineEnding +
                         E.Message);
            {$endif}
           end;
      end;
  finally
    Abort := False;
    if(Result = nil) then
      fDownload.Contents.Free;
    fDownload.OwnsContents := False;
    FreeAndNil(fDownload);
  end;
end;

function LoadCachedData(const FileName: String): TStream;
var
  data: TStream;
begin
  data := DownloadNetworkFile('castle-data:/' + FileName, [soForceMemoryStream], True);

  {$if defined(debugMessages)}
  if not(data = nil) then
    begin
      MemoMessage('Loaded data from ' + URIToFilenameSafe('castle-data:/' + FileName));
    end
  else
    begin
      MemoMessage('No cached data!!!');
    end;
    {$endif}

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

function CacheImage(const URI: String; const FileName: String; const LoadFromCache: Boolean = False; FreeResult: Boolean = False): TStream;
var
  data: TStream = nil;
begin
  if LoadFromCache then
    begin
      if URIFileExists('castle-data:/' + FileName) then
        begin
          {$if defined(debugMessages)}
          MemoMessage('Found cached copy of castle-data:/' + FileName);
          {$endif}
          if not(FreeResult) then
            data := LoadCachedData(FileName);
        end
      else
        begin
          {$if defined(debugMessages)}
          MemoMessage('Cache miss - castle-data:/' + FileName);
          {$endif}
          data := DownloadNetworkFile(URI, [soForceMemoryStream]);
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
      {$if defined(debugMessages)}
      MemoMessage('Forced download - castle-data:/' + FileName);
      {$endif}
      data := DownloadNetworkFile(URI, [soForceMemoryStream]);
      if not(data = nil) then
        begin
          {$if defined(debugMessages)}
          MemoMessage('Saving to data' + PathDelim + FileName);
          {$endif}
          StreamSaveToFile(data, 'castle-data:/' + FileName);
        end
      else
        begin
          {$if defined(debugMessages)}
          MemoMessage('Download fail - try cache - castle-data:/' + FileName);
          {$endif}
          data := LoadCachedData(FileName);
        end;
    end;

  if FreeResult then
    FreeAndNil(data);

  Result := data;
end;


end.

