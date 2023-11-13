unit TextAnalyser;

interface
uses SysUtils, DateUtils, Classes, Logger;
type

  RHistory = record
    text: string; // собственно строка
    version: word;
    createdTime: longint;
  end;

  REntry = record
    blockId: integer; // номер блока (номер картинки)
    stringId: integer; // уникальный номер строки
    createdTime: longint; // время создания в мс
    text: string; // собственно строка
    version: word;
    history: array of RHistory;
    read: boolean;
    translated: boolean;
  end;

  RSourceBlock = record
    blockId: integer;
    text: string;
    createdTime: longint; // время создания в мс
    processed: boolean;
  end;

  TTextAnalyser = class
  private
    blockReadPeriod: longint; //период чтения с экрана содержимого блока текста
    historyDepthPeriod: longint; // время на какую глубину в прошлое смотреть при слиянии строк
    entries: array of REntry;
    source: array of RSourceBlock;
    log: TLogger;

  public
    constructor Create(aBlockReadPeriod, aHistoryDepthPeriod: longint; aLogger: TLogger);
    procedure DoAnalysis();
    procedure LoadFromFile(aFileName: string; aBlockDelimeter: string); // загружает блоки из файла, разделяет на блоки на базе разделителся
    procedure SaveToFile(aFileName: string); // сохраняет результат в файл
    procedure AddSourceRec(aText: string); overload;
    procedure AddSourceRec(aBlockId: integer; aText: string; aProcessed: boolean); overload;
    procedure AddSourceRec(aBlockId: integer; aText: string; aCreatedTime: longint; aProcessed: boolean); overload;
    procedure AddEntry(aBlockId: integer; aStringId: integer; aCreatedTime: longint; aStr: string; aVersion: word; aRead, aTranslated: boolean);
    procedure AddEntryHistory(aEntryIdx: integer; aCreatedTime: longint; aStr: string; aVersion: word);
    function ReadEntry(aMinVersion: integer; var aText: string; var aTime: longint; var aEntryIdx: integer): boolean;

  end;

implementation
uses StringSimilarity;

{ TTextAnalyser }

procedure TTextAnalyser.AddEntry(aBlockId, aStringId, aCreatedTime: Integer; aStr: string; aVersion: word; aRead, aTranslated: boolean);
begin
  Setlength(entries, Length(entries) + 1);
  entries[High(entries)].blockId := aBlockId;
  entries[High(entries)].stringId := aStringId;
  entries[High(entries)].createdTime := aCreatedTime;
  entries[High(entries)].text := aStr;
  entries[High(entries)].version := aVersion;
  entries[High(entries)].read := aRead;
  entries[High(entries)].translated := aTranslated;

end;

procedure TTextAnalyser.AddSourceRec(aBlockId: integer; aText: string; aProcessed: boolean);
begin
  AddSourceRec(aBlockId, atext, DateUtils.MilliSecondOfTheDay(Now()), aProcessed);
end;

procedure TTextAnalyser.AddEntryHistory(aEntryIdx, aCreatedTime: Integer; aStr: string; aVersion: word);
var
  newLen: integer;
begin
  newLen := Length(entries[aEntryIdx].history) + 1;
  SetLength(entries[aEntryIdx].history, newLen);
  entries[aEntryIdx].history[newLen - 1].text := aStr;
  entries[aEntryIdx].history[newLen - 1].version := aVersion;
  entries[aEntryIdx].history[newLen - 1].createdTime := aCreatedTime;
end;

procedure TTextAnalyser.AddSourceRec(aBlockId: integer; aText: string; aCreatedTime: Integer; aProcessed: boolean);
begin
  Setlength(source, Length(source) + 1);
  source[High(source)].blockId := aBlockId;
  source[High(source)].text := aText;
  source[High(source)].createdTime := aCreatedTime;
  source[High(source)].processed := false;
end;

procedure TTextAnalyser.AddSourceRec(aText: string);
begin
  AddSourceRec(High(source) + 1, aText, DateUtils.MilliSecondOfTheDay(Now()), false);
end;

constructor TTextAnalyser.Create(aBlockReadPeriod, aHistoryDepthPeriod: longint; aLogger: Tlogger);
begin
  blockReadPeriod := aBlockReadPeriod;
  historyDepthPeriod := aHistoryDepthPeriod;
  log := aLogger;
end;

procedure TTextAnalyser.DoAnalysis;
var
  blockFound, prevBlockContainsString, allDone: boolean;
  sourceBlockIdx, entryIdx: integer;
  stringNo: integer;
  prevBlockTime: longint;
  strs: TStrings;
  newStrFromBlock, prevStrFromBlock: string;
  i: integer;
begin
  strs := TStringList.Create;
  strs.Delimiter := Chr(10);
  strs.StrictDelimiter := true;
  sourceBlockIdx := 0;
  stringNo := 0;
  allDone := false;
  while not allDone do
    begin
      //сначала ищем в массиве, с самого начала, первый необработанный блок
      blockFound := false;
      while not blockFound do
        begin
          if not source[sourceBlockIdx].processed then
            begin
              blockFound := true;
              break;
            end;
          Inc(sourceBlockIdx);
          if sourceBlockIdx > High(source) then
            begin
              allDone := true;
              break;
            end;
        end;
      if blockFound then //необработанный блок найден
        begin
          //смотрим есть ли предыдущий блок, если нет то просто из этого блока делаем энтри построчно
          //пустые строки пропускаем
          strs.Clear;
          strs.DelimitedText := source[sourceBlockIdx].text;
          if sourceBlockIdx = 0 then //номер необработанного блока=0, т.е. он единственный пока
            begin
              for i := 0 to strs.Count - 1 do
                begin
                  newStrFromBlock := Trim(strs[i]);
                  if Length(newStrFromBlock) > 1 then
                    begin
                      AddEntry(source[sourceBlockIdx].blockId, stringNo, source[sourceBlockIdx].createdTime, newStrFromBlock, 1, false, false);
                      Inc(stringNo);
                    end;
                end;
            end
          else
            begin
              for i := 0 to strs.Count - 1 do
                begin
                  newStrFromBlock := Trim(strs[i]);

                  if Length(newStrFromBlock) <= 1 then //односимвольное и пустое пропускаем
                    continue;
                  //проверяем нет ли такой похожей строки в предыдущем блоке
                  //определяем время, до которого смотрим "назад" в блоки для поиска похожих строк
                  prevBlockTime := Round(source[sourceBlockIdx].createdTime - historyDepthPeriod);
                  prevBlockContainsString := false;
                  //начинаем просмотр с последней строки
                  entryIdx := High(entries);
                  while true do
                    begin
                      //пока в диапазоне просмотра
                      if entries[entryIdx].createdTime > prevBlockTime then
                        begin
                          //todo: вынести в конфиг
                          //ищем похожую строку с степенью похожести 0.6
                          if StringSimilarityRatio(entries[entryIdx].text, newStrFromBlock, true) > 0.6 then
                            begin
                              AddEntryHistory(entryIdx, entries[entryIdx].createdTime, entries[entryIdx].text, entries[entryIdx].version);
                              entries[entryIdx].text := newStrFromBlock;
                              entries[entryIdx].blockId := source[sourceBlockIdx].blockId;
                              //???
                              entries[entryIdx].createdTime := source[sourceBlockIdx].createdTime;
                              entries[entryIdx].version := entries[entryIdx].version + 1;
                              prevBlockContainsString := true;
                              break;
                            end;
                        end;
                      Dec(entryIdx);
                      if entryIdx < 0 then
                        break;
                      if entries[entryIdx].createdTime <= prevBlockTime then
                        break;
                    end;
                  if not prevBlockContainsString then
                    begin
                      Inc(stringNo);
                      AddEntry(source[sourceBlockIdx].blockId, stringNo, source[sourceBlockIdx].createdTime, newStrFromBlock, 1, false, false);
                    end;
                end;
            end;
          source[sourceBlockIdx].processed := true;
          Inc(sourceBlockIdx);
          if sourceBlockIdx > High(source) then
            allDone := true;
        end;
    end;
end;

procedure TTextAnalyser.LoadFromFile(aFileName, aBlockDelimeter: string);
var
  f: TextFile;
  s: string;
  txt: string;
  id: integer;
  isEof: boolean;
  blockTime: longint;
begin
  id := 0;
  s := '';
  txt := '';
  AssignFile(f, aFileName);
  Reset(f);
  //  blockTime := DateUtils.MilliSecondOfTheDay(Now());
  blockTime := 0;
  while true do
    begin
      if not Eof(f) then
        ReadLn(f, s);
      if ((s = aBlockDelimeter) and (Length(txt) > 0) and (txt <> aBlockDelimeter)) or Eof(f) then
        begin
          AddSourceRec(id, txt, blockTime, false);
          blockTime := blockTime + blockReadPeriod;
          inc(id);
          txt := '';
        end
      else
        begin
          txt := txt + s + Chr(10);
        end;
      if Eof(f) then
        break;
    end;
  CloseFile(f);
end;

function TTextAnalyser.ReadEntry(aMinVersion: integer; var aText: string; var aTime: longint; var aEntryIdx: integer): boolean;
var
  entriesEnd: boolean;
  //  entryIdx: integer;
begin
  entriesEnd := false;
  //  entryIdx := 0;
  Result := false;

  while not entriesEnd do
    begin
      if aEntryIdx > High(entries) then
        break;
      if (not entries[aEntryIdx].read) and (entries[aEntryIdx].version >= aMinVersion) then
        begin
          aText := entries[aEntryIdx].text;
          aTime := entries[aEntryIdx].createdTime;
          entries[aEntryIdx].read := true;
          Result := true;
          break;
        end;
      Inc(aEntryIdx);
    end;
end;

procedure TTextAnalyser.SaveToFile(aFileName: string);
var
  f: TextFile;
  i, j: integer;
begin
  AssignFile(f, aFileName);
  Rewrite(f);
  for i := 0 to Length(entries) - 1 do
    begin
      //      if entries[i].version > 1 then
      WriteLn(f, IntToStr(entries[i].version) + ' ' + entries[i].text);
      for j := 0 to Length(entries[i].history) - 1 do
        WriteLn(f, '  ' + IntToStr(entries[i].history[j].version) + ' ' + entries[i].history[j].text);
    end;
  CloseFile(f);
end;

end.

