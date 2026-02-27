unit Delphi.Windows.Helper;

interface

uses System.Classes, System.SysUtils;

type
  TStreamHelper = class helper for TStream
  public
    function ReadByte: Byte;
  end;

  TStringsHelper = class helper for TStrings
    procedure GetNameValue(Index : Integer; Out AName,AValue : String);
  end;

  TFileSearchOption = (sfoImplicitCurrentDir, sfoStripQuotes);
  TFileSearchOptions = set of TFileSearchOption;

  DWord = Cardinal;
  PathStr = String;
  TRTLStringDynArray = TArray<String>;
  TCHAR = Char;

function ConcatPaths(const Paths: array of PathStr): PathStr;
function FileSearch (const Name, DirList: String; Options: TFileSearchoptions = [sfoImplicitCurrentDir]): String;
function ExcludeLeadingPathDelimiter(Const Path: PathStr): PathStr;
function ExecuteProcess(Const Path: UnicodeString; Const ComLine: UnicodeString): Integer;
function ExeSearch(const Name: String; const DirList: String = ''): String;
function GetAppConfigFile(Global : Boolean): String;
function GetEnvironmentString(Index: Integer): String;
function GetEnvironmentVariableCount : Integer;
function FileTruncate(Handle: THandle; Size: Int64): Boolean;
function StdErr: THandle;
function StdErrorHandle: THandle;
function StdInput: THandle;
function StdInputHandle: THandle;
function StdOut: THandle;
function StdOutputHandle: THandle;

procedure DumpExceptionBackTrace(Handle: THandle);
procedure ReadBarrier;

var
  OnGetApplicationName: TFunc<String>;

const
  ConfigExtension: String = '.cfg';
  FileNameCaseSensitive: Boolean = False;
  SExecuteProcessFailed  = 'Failed to execute "%s", error code: %d';

implementation

uses Winapi.Windows, Winapi.SHFolder, Winapi.ShlObj, Delphi.Helper;

Function GetSpecialDir(ID: Integer) : String;
Var
  APath : array[0..MAX_PATH] of WideChar;
begin
  if SHGetSpecialFolderPath(0, APath, ID, True) then
    Result := IncludeTrailingPathDelimiter(APath)
  else
    Result := '';
end;

function VendorName: String;
begin
  Result := '';
end;

function ApplicationName: String;
begin
  if Assigned(OnGetApplicationName) then
    Result := OnGetApplicationName()
  else
    Result := '';
end;

function DGetAppConfigDir(Global: Boolean): String;
begin
  Result:=ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

function GetAppConfigDir(Global : Boolean) : String;
begin
  If Global then
    Result := GetSpecialDir(CSIDL_WINDOWS)
  else
    Result := GetSpecialDir(CSIDL_APPDATA);

  if Result <> '' then
  begin
    if VendorName <> '' then
      Result := IncludeTrailingPathDelimiter(Result + VendorName);

    Result := IncludeTrailingPathDelimiter(Result + ApplicationName);
  end
  else
    Result := IncludeTrailingPathDelimiter(DGetAppConfigDir(Global));
end;

function DGetAppConfigFile(Global, SubDir: Boolean): String;
begin
  Result:=IncludeTrailingPathDelimiter(GetAppConfigDir(Global));

  if SubDir then
    Result:=IncludeTrailingPathDelimiter(Result + 'Config');

  Result:=Result + ApplicationName + ConfigExtension;
end;

function GetAppConfigFile(Global: Boolean): String;
begin
  Result := DGetAppConfigFile(Global, False);
end;

function FileSearch (Const Name, DirList: String; Options: TFileSearchoptions): String;
Var
  I : longint;
  Temp : RawByteString;
begin
  Result:=Name;
  temp:=SetDirSeparators(DirList);
  // Start with checking the file in the current directory
  If (sfoImplicitCurrentDir in Options) and (Result <> '') and FileExists(Result) Then
    exit;
  while True do begin
    If Temp = '' then
      Break; // No more directories to search - fail
    I:=pos(PathSeparator,Temp);
    If I<>0 then
      begin
        Result:=Copy (Temp,1,i-1);
        system.Delete(Temp,1,I);
      end
    else
      begin
        Result:=Temp;
        Temp:='';
      end;
    If Result<>'' then
      begin
      If (sfoStripQuotes in Options) and (Result[1]='"') and (Result[Length(Result)]='"') then
        Result:=Copy(Result,2,Length(Result)-2);
      if (Result<>'') then
        Result:=IncludeTrailingPathDelimiter(Result)+name;
      end;
    If (Result <> '') and FileExists(Result) Then
      exit;
  end;
  Result:='';
end;

function ExeSearch(const Name, DirList: String): String;
Var
  D : RawByteString;
  O : TFileSearchOptions;
begin
  D:=DirList;
  if (D='') then
    D:=GetEnvironmentVariable('PATH');
  O:=[sfoImplicitCurrentDir,sfoStripQuotes];
  Result := FileSearch(Name, D, O);
end;

function StdErr: THandle;
begin
  Result := StdErrorHandle;
end;

function StdErrorHandle: THandle;
begin
  Result := GetStdHandle(STD_ERROR_HANDLE);
end;

function StdInput: THandle;
begin
  Result := StdInputHandle;
end;

function StdInputHandle: THandle;
begin
  Result := GetStdHandle(STD_INPUT_HANDLE);
end;

function StdOut: THandle;
begin
  Result := StdOutputHandle;
end;

function StdOutputHandle: THandle;
begin
  Result := GetStdHandle(STD_OUTPUT_HANDLE);
end;

function FileTruncate(Handle: THandle; Size: Int64): Boolean;
begin
  if FileSeek(Handle, Size, FILE_BEGIN) = Size then
    Result := SetEndOfFile(Handle)
  else
    Result := False;
end;

function ExcludeLeadingPathDelimiter(Const Path: PathStr): PathStr;
begin
  Result:=Path;
  If (Length(Result)>0) and CharInSet(Result[1], AllowDirectorySeparators) then
    Delete(Result,1,1);
end;

function ConcatPaths(const Paths: array of PathStr): PathStr;
var
  I: Integer;
begin
  Result := '';
  if Length(Paths) > 0 then
  begin
    if Paths[0]<>'' then
      Result := Paths[0];
    for I := 1 to Length(Paths) - 1 do
      if Paths[i]<>'' then
        Result := IncludeTrailingPathDelimiter(Result) + ExcludeLeadingPathDelimiter(Paths[I]);
  end
end;

procedure DumpExceptionBackTrace(Handle: THandle);
begin

end;

procedure ReadBarrier;
begin

end;

Function GetEnvironmentVariableCount : Integer;
var
  hp,p : PChar;
begin
  Result:=0;
  p:=GetEnvironmentStrings;
  hp:=p;
  If (Hp<>Nil) then
    while hp^<>#0 do
      begin
      Inc(Result);
      hp:=hp+strlen(hp)+1;
      end;
  FreeEnvironmentStrings(p);
end;

Function GetEnvironmentString(Index : Integer) : String;

var
  hp,p : PChar;
  tmpstr : RawByteString;
begin
  Result:='';
  p:=GetEnvironmentStrings;
  hp:=p;
  If (Hp<>Nil) then
    begin
      while (hp^<>#0) and (Index>1) do
        begin
          Dec(Index);
          hp:=hp+strlen(hp)+1;
        end;
    If (hp^<>#0) then
      begin
        tmpstr:=hp;
        SetCodePage(tmpstr,CP_OEMCP,false);
        Result:=tmpstr;
      end;
    end;
  FreeEnvironmentStrings(p);
end;

function ExecuteProcess(Const Path: UnicodeString; Const ComLine: UnicodeString):integer;
// win specific  function
var
  SI: TStartupInfoW;
  PI: TProcessInformation;
  Proc : THandle;
  l    : DWord;
  CommandLine : unicodestring;
  e : EOSError;
  ExecInherits : longbool;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb:=SizeOf(SI);
  SI.wShowWindow:=1;
  { always surround the name of the application by quotes
    so that long filenames will always be accepted. But don't
    do it if there are already double quotes, since Win32 does not
    like double quotes which are duplicated!
  }
  if pos('"',path)=0 then
    CommandLine:='"'+path+'"'
  else
    CommandLine:=path;
  if ComLine <> '' then
    CommandLine:=Commandline+' '+ComLine+#0
  else
    CommandLine := CommandLine + #0;

  ExecInherits:=False;

  if not CreateProcessW(nil, pwidechar(CommandLine),
    Nil, Nil, ExecInherits,$20, Nil, Nil, SI, PI) then
    begin
      e:=EOSError.CreateFmt(SExecuteProcessFailed,[CommandLine,GetLastError]);
      e.ErrorCode:=GetLastError;
      raise e;
    end;
  Proc:=PI.hProcess;
  if WaitForSingleObject(Proc, dword($ffffffff)) <> $ffffffff then
    begin
      GetExitCodeProcess(Proc,l);
      CloseHandle(Proc);
      CloseHandle(PI.hThread);
      result:=l;
    end
  else
    begin
      e:=EOSError.CreateFmt(SExecuteProcessFailed,[CommandLine,GetLastError]);
      e.ErrorCode:=GetLastError;
      CloseHandle(Proc);
      CloseHandle(PI.hThread);
      raise e;
    end;
end;

{ TStreamHelper }

function TStreamHelper.ReadByte: Byte;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

{ TStringsHelper }

procedure TStringsHelper.GetNameValue(Index: Integer; out AName, AValue: String);
begin
  AName := KeyNames[Index];
  AValue := ValueFromIndex[Index];
end;

end.

