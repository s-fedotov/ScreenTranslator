unit Logger;

interface

uses SysUtils, Classes, SyncObjs, Windows, Dialogs;

const
  LOG_DAYS_FOR_DELETE_LOG = 15; // количество дней, старее которых файлы лога будут удаляться
  LOG_DATE_FORMAT = 'yyyy-mm-dd';
  LOG_TIME_FORMAT = 'hh:mm:ss';
  LOG_DATE_TIME_FORMAT = LOG_DATE_FORMAT + ' ' + LOG_TIME_FORMAT;
  LOGFILES_DIR = 'log';
  LOGFILE_EXTENSION = '.log';
  // LOGFILE_ERROR_MARKER = '_ERROR';
  LOG_ENGINE_MSG = '(***LOGGER***)';

type
  // llError - выводить ошибки,
  // llInfo - выводить ошибки и логи,
  // llDebug - выводить ошибки, логи и отладочную информацию
  // llDisturb - выводить ошибки, логи, отладочную информацию и дополнительные отладочные данные
  enmLogLevel = (llNone = 0, llError = 1, llInfo = 2, llDebug = 3, llDisturb = 4);

  // leANSI - Запись сообщений в Win1251
  // leUTF16 - Запись сообщений в UTF16
  enmLogEncoding = (leANSI = 0, leUTF16 = 1);

const
  enmLogLevelName: array [enmLogLevel] of string = ('LOG', 'ERROR', 'INFO', 'DEBUG', 'DISTURB');
  enmLogEncodingName: array [enmLogEncoding] of string = ('ANSI (Win-1251)', 'UTF-16');

type
  TLogger = class
  private
    fCS_WriteToLog: TCriticalSection;
    fLogPath: string;
    fLogFileName: string;
    fEncoding: enmLogEncoding;
    fDaysForDeleteLog: Word;
    fCreateNewLogOnEveryStart: Boolean;
    fModuleName: string; // нименование модуля (программного компонена)
    fLogFileStream: TFileStream;
    fLogFileStreamDateTime: TDateTime;
    class function GetPath(const HInstance: LongWord): TFileName;
    class function FileTimeToDateTime(aFileTime: TFileTime): TDateTime;
    function getEncoding: enmLogEncoding;
    function getDaysForDeleteLog: Word;
    function GetCreateNewLogOnEveryStart: Boolean;
    function ScanDirPlane(const Dir: string; Mask: string; ListFiles: TStrings): integer;
    procedure DeleteOldFiles();
    procedure CreateLogFileStream();
    procedure Log(aLoglevel: enmLogLevel; aClassName, aMSG: string); overload;
    procedure Log(aLoglevel: enmLogLevel; aClassName: string; aIntMsg: integer); overload;
    procedure Log(aLoglevel: enmLogLevel; aClassName: string; aExtMsg: extended); overload;
    procedure Log(aLoglevel: enmLogLevel; aClassName: string; aBoolMsg: Boolean); overload;
    procedure Log(aLoglevel: enmLogLevel; aClassName: string; aObj: TObject); overload;
    procedure WriteToLog(aMSG: string);

  public
    fLogLevel: enmLogLevel;
    constructor Create(aModuleName: string; aLoglevel: enmLogLevel; aEncoding: enmLogEncoding; aDaysForDeleteLog: Word; aCreateNewLogOnEveryStart: Boolean;
      aLogPath: string); overload;
    constructor Create(aModuleName: string; aLoglevel: enmLogLevel; aLogPath: string); overload;
    constructor Create(aModuleName: string; aLoglevel: enmLogLevel); overload;
    destructor Destroy(); override;

    property Encoding: enmLogEncoding read getEncoding;
    property DaysForDeleteLog: Word read getDaysForDeleteLog;
    property CreateNewLogOnEveryStart: Boolean read GetCreateNewLogOnEveryStart;

    procedure Log(aClassName, aMSG: string); overload;

    procedure Error(aClassName, aMSG: string); overload;
    procedure Error(aClassName: string; aIntMsg: integer); overload;
    procedure Error(aClassName: string; aExtMsg: extended); overload;
    procedure Error(aClassName: string; aBoolMsg: Boolean); overload;
    procedure Error(aClassName: string; aObj: TObject); overload;

    procedure Info(aClassName, aMSG: string); overload;
    procedure Info(aClassName: string; aIntMsg: integer); overload;
    procedure Info(aClassName: string; aExtMsg: extended); overload;
    procedure Info(aClassName: string; aBoolMsg: Boolean); overload;
    procedure Info(aClassName: string; aObj: TObject); overload;

    procedure Debug(aClassName, aMSG: string); overload;
    procedure Debug(aClassName: string; aIntMsg: integer); overload;
    procedure Debug(aClassName: string; aExtMsg: extended); overload;
    procedure Debug(aClassName: string; aBoolMsg: Boolean); overload;
    procedure Debug(aClassName: string; aObj: TObject); overload;

    procedure Disturb(aClassName, aMSG: string); overload;
    procedure Disturb(aClassName: string; aIntMsg: integer); overload;
    procedure Disturb(aClassName: string; aExtMsg: extended); overload;
    procedure Disturb(aClassName: string; aBoolMsg: Boolean); overload;
    procedure Disturb(aClassName: string; aObj: TObject); overload;
  end;

implementation

uses StrUtils, Masks, DateUtils;

procedure TLogger.WriteToLog(aMSG: string);
var
  vStrToWriteUTF16: string;
  vStrToWriteANSI: AnsiString;
  vDT: TDateTime;
begin
  if fLogLevel = llNone then
    exit;
  fCS_WriteToLog.Enter();
  try
    vDT := Now();
    if (DateOf(fLogFileStreamDateTime) <> DateOf(vDT)) then
      CreateLogFileStream();
{$WARNINGS OFF}
    case fEncoding of
      leANSI:
        begin
          vStrToWriteANSI := AnsiString(Format('%s %s%s', [FormatDateTime(LOG_DATE_TIME_FORMAT, vDT), aMSG, sLineBreak]));
          fLogFileStream.WriteBuffer(vStrToWriteANSI[1], Length(vStrToWriteANSI) * SizeOf(AnsiChar));
        end;
      leUTF16:
        begin
          vStrToWriteUTF16 := Format('%s %s%s', [FormatDateTime(LOG_DATE_TIME_FORMAT, vDT), aMSG, sLineBreak]);
          fLogFileStream.WriteBuffer(vStrToWriteUTF16[1], Length(vStrToWriteUTF16) * SizeOf(Char));
        end;
    else
      Assert(False, 'Unknown log encoding');
    end;
{$WARNINGS ON}
  finally
    fCS_WriteToLog.Leave();
  end;
end;

constructor TLogger.Create(aModuleName: string; aLoglevel: enmLogLevel; aLogPath: string);
begin
  Create(aModuleName, aLoglevel, leANSI, LOG_DAYS_FOR_DELETE_LOG, True, aLogPath);
end;

constructor TLogger.Create(aModuleName: string; aLoglevel: enmLogLevel; aEncoding: enmLogEncoding; aDaysForDeleteLog: Word; aCreateNewLogOnEveryStart: Boolean;
  aLogPath: string);
begin
  fCS_WriteToLog := TCriticalSection.Create();
  fModuleName := aModuleName;
  fLogLevel := aLoglevel;
  fEncoding := aEncoding;
  fDaysForDeleteLog := aDaysForDeleteLog;
  fCreateNewLogOnEveryStart := aCreateNewLogOnEveryStart;
  fLogPath := aLogPath + LOGFILES_DIR;
  if not DirectoryExists(fLogPath) and (fLogLevel > llNone) then
    CreateDir(fLogPath);
  fLogPath := IncludeTrailingPathDelimiter(fLogPath);
  CreateLogFileStream();
end;

constructor TLogger.Create(aModuleName: string; aLoglevel: enmLogLevel);
begin
  Create(aModuleName, aLoglevel, leANSI, LOG_DAYS_FOR_DELETE_LOG, True, GetPath(HInstance) + LOGFILES_DIR);
end;

procedure TLogger.CreateLogFileStream;
var
  vCreation: TFileTime;
  vLastAccess: TFileTime;
  vLastWrite: TFileTime;
  vExsitFileDT: TDateTime;
  vBackupFileName: TFileName;
  randomString: String;
begin

  if fLogLevel = llNone then
    exit;
  fLogFileName := fLogPath + fModuleName + LOGFILE_EXTENSION;
  if fLogFileStream <> nil then
    FreeAndNil(fLogFileStream);
  fLogFileStreamDateTime := Now();
  if FileExists(fLogFileName) then
    begin
      try
        ChangeFileExt(fLogFileName, LOGFILE_EXTENSION);
        fLogFileStream := TFileStream.Create(fLogFileName, fmOpenWrite or fmShareDenyNone);
      except
        on e: EFOpenError do
          begin
            // Ошибка открытия файла. Скорее всего файл уже существует и используется копией программы.
            // Создаем отдельный файл лога, что бы туда можно было писать туда.
            Randomize();
            randomString := IntToHex(Random(100000), 2);
            if Length(randomString) < 6 then
              randomString := DupeString('0', 6 - Length(randomString)) + randomString;
            fLogFileName := fLogPath + fModuleName + '_TMP' + randomString + LOGFILE_EXTENSION;
            fLogFileStream := TFileStream.Create(fLogFileName, fmCreate or fmShareDenyWrite);
          end;
      end;
{$WARNINGS OFF}
      GetFileTime(fLogFileStream.Handle, @vCreation, @vLastAccess, @vLastWrite);
{$WARNINGS ON}
      vExsitFileDT := TLogger.FileTimeToDateTime(vLastWrite);
      if (DateOf(fLogFileStreamDateTime) = DateOf(vExsitFileDT)) then
        begin
          fLogFileStream.Position := fLogFileStream.Size;
        end
      else
        begin
          FreeAndNil(fLogFileStream);
          vBackupFileName := fLogPath + FormatDateTime(LOG_DATE_FORMAT, vExsitFileDT) + '_' + fModuleName + LOGFILE_EXTENSION;
          SysUtils.DeleteFile(vBackupFileName);
          RenameFile(fLogFileName, vBackupFileName);
          fLogFileStream := TFileStream.Create(fLogFileName, fmCreate or fmShareDenyNone);
        end;
    end
  else
    fLogFileStream := TFileStream.Create(fLogFileName, fmCreate or fmShareDenyNone);
  Log(llNone, ClassName, '');
  Log(llNone, ClassName,
    '************************************************************************************************************************************************');
  Log(llNone, ClassName, Format('%s Log engine created [logLevel: %-8s; Encoding: %s; DaysForDeleteOldFiles: %d]', [LOG_ENGINE_MSG, enmLogLevelName[fLogLevel],
      enmLogEncodingName[fEncoding], fDaysForDeleteLog]) + ' Module: ' + fModuleName);
  Log(llNone, ClassName,
    '************************************************************************************************************************************************');
  Log(llNone, ClassName, '');
  DeleteOldFiles();
end;

procedure TLogger.Log(aClassName, aMSG: string);
begin
  Log(llDebug, aClassName, aMSG);
end;

procedure TLogger.Log(aLoglevel: enmLogLevel; aClassName: string; aObj: TObject);
var
  s: string;
begin

  if aObj = nil then
    s := 'NIL'
  else
    s := aObj.ClassName + ':' + aObj.ToString;
  WriteToLog(Format('[%-8s] [%s] (%s) %s', [enmLogLevelName[aLoglevel], fModuleName, aClassName, s]));
end;

procedure TLogger.Debug(aClassName, aMSG: string);
begin
  if llDebug <= fLogLevel then
    Log(llDebug, aClassName, aMSG);
end;

procedure TLogger.Debug(aClassName: string; aBoolMsg: Boolean);
begin
  if llDebug <= fLogLevel then
    Log(llDebug, aClassName, aBoolMsg);
end;

procedure TLogger.Debug(aClassName: string; aExtMsg: extended);
begin
  if llDebug <= fLogLevel then
    Log(llDebug, aClassName, aExtMsg);
end;

procedure TLogger.Debug(aClassName: string; aIntMsg: integer);
begin
  if llDebug <= fLogLevel then
    Log(llDebug, aClassName, aIntMsg);
end;

procedure TLogger.DeleteOldFiles;
var
  vStrs: TStringList;
  vTestFileName: TFileName;
  vI: integer;
  vTestFilenameInd: integer;
begin
  vStrs := TStringList.Create();
  try
    vStrs.BeginUpdate();
    try
      if ScanDirPlane(fLogPath, '*' + fModuleName + LOGFILE_EXTENSION, vStrs) = 0 then
        exit;
    finally
      vStrs.BeginUpdate();
    end;
    vStrs.Sorted := True;
    vTestFileName := fLogPath + FormatDateTime(LOG_DATE_FORMAT, IncDay(Now, -fDaysForDeleteLog)) + '_' + fModuleName + LOGFILE_EXTENSION;
    vTestFilenameInd := vStrs.Add(vTestFileName);
    for vI := 0 to vTestFilenameInd - 1 do
      begin
        try
          SysUtils.DeleteFile(vStrs[vI]);
        except
          Error(ClassName, 'Cannot delete old log file:' + vStrs[vI]);
        end;
      end;
  finally
    FreeAndNil(vStrs);
  end;
end;

destructor TLogger.Destroy;
begin
  Log(llNone, ClassName, Format('%s Log engine destroyed.', [LOG_ENGINE_MSG]));
  FreeAndNil(fCS_WriteToLog);
  inherited;
end;

procedure TLogger.Disturb(aClassName: string; aBoolMsg: Boolean);
begin
  if llDisturb <= fLogLevel then
    Log(llDisturb, aClassName, aBoolMsg);
end;

procedure TLogger.Disturb(aClassName: string; aExtMsg: extended);
begin
  if llDisturb <= fLogLevel then
    Log(llDisturb, aClassName, aExtMsg);
end;

procedure TLogger.Disturb(aClassName: string; aIntMsg: integer);
begin
  if llDisturb <= fLogLevel then
    Log(llDisturb, aClassName, aIntMsg);

end;

procedure TLogger.Disturb(aClassName: string; aMSG: string);
begin
  if llDisturb <= fLogLevel then
    Log(llDisturb, aClassName, aMSG);
end;

procedure TLogger.Error(aClassName: string; aIntMsg: integer);
begin
  Log(llError, aClassName, aIntMsg);
end;

procedure TLogger.Error(aClassName: string; aExtMsg: extended);
begin
  Log(llError, aClassName, aExtMsg);
end;

procedure TLogger.Error(aClassName: string; aBoolMsg: Boolean);
begin
  Log(llError, aClassName, aBoolMsg);
end;

procedure TLogger.Error(aClassName: string; aMSG: string);
begin
  Log(llError, aClassName, aMSG);
end;

class function TLogger.FileTimeToDateTime(aFileTime: TFileTime): TDateTime;
var
  vModifiedTime: TFileTime;
  vSystemTime: TSystemTime;
begin
  if (aFileTime.dwLowDateTime = 0) and (aFileTime.dwHighDateTime = 0) then
    begin
      Result := 0;
      exit;
    end;

  FileTimeToLocalFileTime(aFileTime, vModifiedTime);
  FileTimeToSystemTime(vModifiedTime, vSystemTime);
  Result := SystemTimeToDateTime(vSystemTime);
end;

function TLogger.GetCreateNewLogOnEveryStart: Boolean;
begin
  Result := fCreateNewLogOnEveryStart;
end;

function TLogger.getDaysForDeleteLog: Word;
begin
  Result := fDaysForDeleteLog;
end;

function TLogger.getEncoding: enmLogEncoding;
begin
  Result := fEncoding;
end;

class function TLogger.GetPath(const HInstance: LongWord): TFileName;
var
  vFileName: array [0 .. MAX_PATH] of Char;
begin
{$WARNINGS OFF}
  ZeroMemory(@vFileName[0], SizeOf(vFileName));
{$WARNINGS ON}
  GetModuleFilename(HInstance, vFileName, MAX_PATH);
  Result := ExtractFilePath(vFileName);
end;

procedure TLogger.Info(aClassName: string; aIntMsg: integer);
begin
  if llInfo <= fLogLevel then
    Log(llInfo, aClassName, aIntMsg);
end;

procedure TLogger.Info(aClassName: string; aExtMsg: extended);
begin
  if llInfo <= fLogLevel then
    Log(llInfo, aClassName, aExtMsg);
end;

procedure TLogger.Info(aClassName: string; aBoolMsg: Boolean);
begin
  if llInfo <= fLogLevel then
    Log(llInfo, aClassName, aBoolMsg);
end;

procedure TLogger.Info(aClassName, aMSG: string);
begin
  if llInfo <= fLogLevel then
    Log(llInfo, aClassName, aMSG);
end;

procedure TLogger.Log(aLoglevel: enmLogLevel; aClassName: string; aExtMsg: extended);
begin
  WriteToLog(Format('[%-8s] [%s] (%s) %s', [enmLogLevelName[aLoglevel], fModuleName, aClassName, FloatToStr(aExtMsg)]));
end;

procedure TLogger.Log(aLoglevel: enmLogLevel; aClassName: string; aBoolMsg: Boolean);
begin
  WriteToLog(Format('[%-8s] [%s] (%s) %s', [enmLogLevelName[aLoglevel], fModuleName, aClassName, IfThen(aBoolMsg, 'TRUE', 'FALSE')]));
end;

procedure TLogger.Log(aLoglevel: enmLogLevel; aClassName: string; aIntMsg: integer);
begin
  WriteToLog(Format('[%-8s] [%s] (%s) %s', [enmLogLevelName[aLoglevel], fModuleName, aClassName, IntToStr(aIntMsg)]));
end;

procedure TLogger.Log(aLoglevel: enmLogLevel; aClassName, aMSG: string);
begin
  WriteToLog(Format('[%-8s] [%s] (%s) %s', [enmLogLevelName[aLoglevel], fModuleName, aClassName, aMSG]));
end;

function TLogger.ScanDirPlane(const Dir: string; Mask: string; ListFiles: TStrings): integer;
var
  vSR: TSearchRec;
  vPath: string;
begin
  Result := 0;
{$WARN SYMBOL_PLATFORM OFF}
  vPath := IncludeTrailingPathDelimiter(Dir);
{$WARN SYMBOL_PLATFORM ON}
  if SysUtils.FindFirst(vPath + Mask, faAnyFile, vSR) <> 0 then
    exit;
  try
    repeat
      if (vSR.name = '.') or (vSR.name = '..') then
        Continue;
      if (vSR.Attr and faDirectory) = faDirectory then
        Continue;
      if MatchesMask(vSR.name, Mask) then
        ListFiles.Add(vPath + vSR.name);
      Inc(Result);
    until SysUtils.FindNext(vSR) <> 0;
  finally
    SysUtils.FindClose(vSR);
  end;
end;

procedure TLogger.Debug(aClassName: string; aObj: TObject);
begin
  if llDebug <= fLogLevel then
    Log(llDebug, aClassName, aObj);
end;

procedure TLogger.Disturb(aClassName: string; aObj: TObject);
begin
  if llDebug <= fLogLevel then
    Log(llDebug, aClassName, aObj);
end;

procedure TLogger.Error(aClassName: string; aObj: TObject);
begin
  if llDebug <= fLogLevel then
    Log(llDebug, aClassName, aObj);
end;

procedure TLogger.Info(aClassName: string; aObj: TObject);
begin
  if llDebug <= fLogLevel then
    Log(llDebug, aClassName, aObj);
end;

end.
