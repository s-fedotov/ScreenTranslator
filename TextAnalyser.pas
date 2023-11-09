unit TextAnalyser;

interface
uses SysUtils, DateUtils, Classes;
type

  REntry = record
    blockId: integer; // ����� ����� (����� ��������)
    stringId: integer; // ���������� ����� ������
    createdTime: longint; // ����� �������� � ��
    str: string; // ���������� ������
    version: word;
    translated: boolean;
  end;

  RSourceBlock = record
    blockId: integer;
    text: string;
    createdTime: longint; // ����� �������� � ��
    processed: boolean;
  end;

  TTextAnalyser = class
  private
    blockTimePeriod: longint; // ����� ����� �������, ����� ���������� �� ����� ������� � ������� ��������
    entries: array of REntry;
    source: array of RSourceBlock;
    procedure AddSourceRec(aText: string); overload;
    procedure AddSourceRec(aBlockId: integer; aText: string; aProcessed: boolean); overload;
    procedure AddSourceRec(aBlockId: integer; aText: string; aCreatedTime: longint; aProcessed: boolean); overload;
    procedure AddEntry(aBlockId: integer; aStringId: integer; aCreatedTime: longint; aStr: string; aVersion: word; aTranslated: boolean);

  public
    constructor Create(aBlockTimePeriod: integer);
    procedure LoadFromFile(aFileName: string; aBlockDelimeter: string); // ��������� ����� �� �����, ��������� �� ����� �� ���� ������������
    procedure SaveToFile(aFileName: string); // ��������� ��������� � ����
    procedure DoAnalysis();
  end;

implementation
uses StringSimilarity;

{ TTextAnalyser }

procedure TTextAnalyser.AddEntry(aBlockId, aStringId, aCreatedTime: Integer; aStr: string; aVersion: word; aTranslated: boolean);
begin
  Setlength(entries, Length(entries) + 1);
  entries[High(entries)].blockId := aBlockId;
  entries[High(entries)].stringId := aStringId;
  entries[High(entries)].createdTime := aCreatedTime;
  entries[High(entries)].str := aStr;
  entries[High(entries)].version := aVersion;
  entries[High(entries)].translated := aTranslated;
end;

procedure TTextAnalyser.AddSourceRec(aBlockId: integer; aText: string; aProcessed: boolean);
begin
  AddSourceRec(aBlockId, atext, DateUtils.MilliSecondOfTheDay(Now()), aProcessed);
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

constructor TTextAnalyser.Create(aBlockTimePeriod: integer);
begin
  blockTimePeriod := aBlockTimePeriod;
end;

procedure TTextAnalyser.DoAnalysis;
var
  blockFound, prevBlockContains, allDone: boolean;
  sourceIdx, entryIdx: integer;
  stringNo: integer;
  prevBlockTime: longint;
  strs: TStrings;
  s: string;
  i: integer;
begin
  strs := TStringList.Create;
  sourceIdx := 0;
  stringNo := 0;
  allDone := false;
  while not allDone do
    begin
      //������� ���� � ������� ������ �������������� ����
      blockFound := false;
      while not blockFound do
        begin
          if not source[sourceIdx].processed then
            begin
              blockFound := true;
              break;
            end;
          if sourceIdx = High(source) then
            begin
              break;
              allDone := true;
            end;

        end;

      if blockFound then //�������������� ���� ������
        begin
          strs.Delimiter := Chr(10);
          strs.StrictDelimiter := true;
          //������� ���� �� ���������� ����, ���� ��� �� ������ �� ����� ����� ������ ����� ���������
          //������ ������ ����������
          strs.DelimitedText := source[sourceIdx].text;
          if sourceIdx = 0 then
            begin
              for i := 0 to strs.Count - 1 do
                begin
                  s := Trim(strs[i]);
                  if Length(s) > 1 then
                    begin
                      AddEntry(source[sourceIdx].blockId, stringNo, source[sourceIdx].createdTime, s, 1, false);
                      Inc(stringNo);
                    end;
                end;
            end
          else
            begin
              for i := 0 to strs.Count - 1 do
                begin
                  s := Trim(strs[i]);
                  if Length(s) > 1 then
                    begin
                      //��������� ��� �� ����� ������� ������ � ���������� �����
                      prevBlockTime := Round(source[sourceIdx].createdTime - blockTimePeriod);
                      prevBlockContains := false;
                      entryIdx := High(entries);
                      while true do
                        begin
                          if entries[entryIdx].createdTime > prevBlockTime then
                            begin
                              if StringSimilarityRatio(entries[entryIdx].str, s, true) > 0.6 then
                                begin
                                  entries[entryIdx].str := s;
                                  entries[entryIdx].blockId := source[sourceIdx].blockId;
                                  //???
                                  entries[entryIdx].createdTime := source[sourceIdx].createdTime;
                                  entries[entryIdx].version := entries[entryIdx].version + 1;
                                  prevBlockContains := true;
                                  break;
                                end;
                            end;
                          Dec(entryIdx);
                          if entryIdx < 0 then
                            break;
                          if entries[entryIdx].createdTime <= prevBlockTime then
                            break;
                        end;
                      if not prevBlockContains then
                        begin
                          Inc(stringNo);
                          AddEntry(source[sourceIdx].blockId, stringNo, source[sourceIdx].createdTime, s, 1, false);
                        end;
                    end;
                end;
            end;
          source[sourceIdx].processed := true;
          Inc(sourceIdx);
          if sourceIdx > High(source) then
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
          blockTime := blockTime + 1000;
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

procedure TTextAnalyser.SaveToFile(aFileName: string);
var
  f: TextFile;
  i: integer;
begin
  AssignFile(f, aFileName);
  Rewrite(f);

  for i := 0 to Length(entries) - 1 do
    //    WriteLn(f, IntToStr(entries[i].createdTime) + ' ' + entries[i].str + ' ' + IntTostr(entries[i].version));
    WriteLn(f, entries[i].str);
  CloseFile(f);

end;

end.

