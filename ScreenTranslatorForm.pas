unit ScreenTranslatorForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Masks, Ocr, ExtCtrls, Buttons, ScreenShotHelper, TextAnalyser;

type

  TAppWorkMode = (awmWindowDefine, awmRangeDefine, awmTranslation);

  TfrmScreenTranslator = class(TForm)
    Ocr1: TOcr;
    Image1: TImage;
    Panel4: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    p_top: TPanel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    e_paddingLeft: TEdit;
    Label5: TLabel;
    e_paddingBottom: TEdit;
    Label6: TLabel;
    e_paddingRight: TEdit;
    b_run: TButton;
    Button3: TButton;
    tm_translation: TTimer;
    Button1: TButton;
    Button4: TButton;
    Label3: TLabel;
    e_paddingTop: TEdit;
    procedure b_runClick(Sender: TObject);
    function GetWindowTitle(aWndHandle: HWND; var aWndName: string): Boolean;
    // function FindWindowByTite(aWindowTitleMask: string; var aWndHandle: HWND; var aWndName: string): Boolean;
    function ListWindows(aList: TStrings): Boolean;
    procedure b_findWindowClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure b_setClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OnMove(var Msg: TWMMove); message WM_MOVE;
    procedure Button3Click(Sender: TObject);
    procedure tm_translationTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }

    helper: TScreenShotHelper;
    ta: TTextAnalyser;
    procedure UpdatePaddingEdits();
    procedure DrawFrame();
    // procedure doFindWindow(wndHandle: HWND);
    // remove the below
    // procedure DrawCaption;

  protected
    // procedure CreateParams(var Params: TCreateParams); override; // ADD THIS LINE!
    // procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
//    procedure WMNCPaint(var Msg: TWMNCPaint); message WM_NCPAINT;
//    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;

  public
    appWorkMode: TAppWorkMode;
    { Public declarations }
  end;

var
  frmScreenTranslator: TfrmScreenTranslator;

implementation

uses ScreenShot, RangeSelectForm, StringSimilarity;
{$R *.dfm}

procedure MsgBox(s: string);
begin
  Application.Messagebox(PChar(s), 'Сообщение', MB_OK + MB_ICONINFORMATION);
end;

{ procedure TfrmScreenTranslator.WMNCHitTest(var Message: TWMNCHitTest);
  var
  D: Integer;
  P: TPoint;
  P1: TPoint;
  begin

  D := GetSystemMetrics(SM_CXSIZEFRAME);
  P1.X := Message.Pos.x;
  P1.Y := Message.Pos.y;

  P := Self.ScreenToClient(P1);

  if P.Y < D then
  begin
  if P.X < D then
  Message.Result := HTTOPLEFT
  else if P.X > ClientWidth - D then
  Message.Result := HTTOPRIGHT
  else
  Message.Result := HTTOP;
  end
  else if P.Y > ClientHeight - D then
  begin
  if P.X < D then
  Message.Result := HTBOTTOMLEFT
  else if P.X > ClientWidth - D then
  Message.Result := HTBOTTOMRIGHT
  else
  Message.Result := HTBOTTOM;
  end
  else
  begin
  if P.X < D then
  Message.Result := HTLEFT
  else if P.X > ClientWidth - D then
  Message.Result := HTRIGHT
  end;

  end; }

//procedure TfrmScreenTranslator.WMNCPaint(var Msg: TWMNCPaint);
//var
//  dc: hDc;
//  Pen: hPen;
//  OldPen: hPen;
//  OldBrush: hBrush;
//
//begin
//  inherited;
//  dc := GetWindowDC(Handle);
//  Msg.Result := 1;
//  // Change the RGB value to change the color
//  Pen := CreatePen(PS_SOLID, 4, RGB(15, 255, 0));
//  OldPen := SelectObject(dc, Pen);
//  OldBrush := SelectObject(dc, GetStockObject(NULL_BRUSH));
//  Rectangle(dc, 0, 0, frmScreenTranslator.Width, frmScreenTranslator.Height);
//  SelectObject(dc, OldBrush);
//  SelectObject(dc, OldPen);
//  DeleteObject(Pen);
//  ReleaseDC(Handle, Canvas.Handle);
//end;
//
//procedure TfrmScreenTranslator.WMActivate(var Msg: TWMActivate);
//begin
//end;

function TfrmScreenTranslator.GetWindowTitle(aWndHandle: HWND; var aWndName: string): Boolean;
var
  ls: Integer;
  sName: array[0..256] of char;
begin
  Result := false;
  if aWndHandle = 0 then
    Exit;
  // Only top window
  if GetWindow(aWndHandle, GW_OWNER) <> 0 then
    Exit;
  if GetParent(aWndHandle) <> 0 then
    Exit;
  ls := GetWindowLong(aWndHandle, GWL_STYLE);
  if ls and WS_CHILD <> 0 then
    Exit;
  if GetWindowText(aWndHandle, sName, 255) = 0 then
    Exit;
  aWndName := StrPas(sName);
  Result := true;
end;

// procedure TfrmScreenTranslator.Button2Click(Sender: TObject);
// var
// wndHandle: HWND;
// wndName: string;
// begin
// if FindWindowByTite('*' + e_searchQuery.Text + '*', wndHandle, wndName) then
// GetScreenShot(wndHandle, Image1)
// else
// Image1.Picture := nil;
// end;

// procedure TfrmScreenTranslator.Button5Click(Sender: TObject);
// var
// hlpr: TScreenShotHelper;
// begin
// // hlpr := TScreenShotHelper.Create(adtPixels, 0, 0, 0, 0, '*chrome*');
// // try
// // hlpr.GetScreenShot();
// // Image1.Picture.Assign(hlpr.GetBitmap());
// // finally
// // hlpr.Destory;
// // end;
// end;

procedure TfrmScreenTranslator.Button1Click(Sender: TObject);
begin
  ta.LoadFromFile('Text\5.txt', '----------');
  ta.DoAnalysis();
  //  ta.SaveToFile('Text\5_o.txt');
end;

procedure TfrmScreenTranslator.DrawFrame();
var
  desktopHandle: HWND;
  desktopDC: hDc;
  desktopCanvas: TCanvas;
  r: TRect;
begin
  try
    desktopHandle := GetDesktopWindow;
    desktopDC := GetWindowDC(desktopHandle);
    desktopCanvas := TCanvas.Create;
    desktopCanvas.Handle := desktopDC;
    desktopCanvas.Pen.Color := clRed;
    desktopCanvas.brush.Color := clNone;
    desktopCanvas.brush.Style := bsClear;
    desktopCanvas.Pen.Mode := pmNotXor;
    desktopCanvas.Pen.Width := 1;
    desktopCanvas.Pen.Style := psDot;
    desktopCanvas.Rectangle(helper.GetTargetRect());
  finally
    desktopCanvas.Free;
    ReleaseDC(desktopHandle, desktopDC);
  end;

end;

procedure TfrmScreenTranslator.Button3Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmScreenTranslator.Button4Click(Sender: TObject);
var
  newStr: string;
  newStrTime: longint;
begin
  ta.DoAnalysis();
  while ta.ReadEntry(newStr, newStrTime) do
    begin
      Application.ProcessMessages;
      frmRangeSelect.Memo1.Lines.Add(newStr);
    end;
end;

procedure TfrmScreenTranslator.b_findWindowClick(Sender: TObject);
//var
//  wndHandle: HWND;
//  wndName: string;
begin
  //  if helper.FindWindowByTite(e_searchQuery.Text, wndHandle, wndName) then
  //    begin
  //      frmScreenTranslator.TransparentColor := true;
  //      frmScreenTranslator.TransparentColorValue := clAppWorkSpace;
  //      frmScreenTranslator.BorderIcons := [];
  //      frmScreenTranslator.SetFocus;
  //      SetWindowPos(frmScreenTranslator.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
  //      BringWindowToTop(wndHandle);
  //      ShowWindow(wndHandle, SW_SHOWNORMAL);
  //      GetWindowRect(wndHandle, helper.windowRect);
  //      // helper.windowRect.Inflate
  //      // InflateRect(windowRect, 4, 2);
  //      appWorkMode := awmRangeDefine;
  //      UpdatePaddingEdits();
  //      e_targetWindow.Text := IntToStr(wndHandle) + ':' + wndName;
  //    end
  //  else
  //    begin
  //      Image1.Picture := nil;
  //      frmScreenTranslator.TransparentColor := false;
  //      SetWindowPos(frmScreenTranslator.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
  //    end;

end;

procedure TfrmScreenTranslator.b_setClick(Sender: TObject);
begin
end;

{ procedure TfrmScreenTranslator.CreateParams(var Params: TCreateParams);
  begin
  inherited;
  Params.Style := Params.Style or WS_BORDER or WS_THICKFRAME;
  end; }

// function TfrmScreenTranslator.FindWindowByTite(aWindowTitleMask: string; var aWndHandle: HWND; var aWndName: string): Boolean;
// var
// windowNames: TStrings;
// wndName: string;
// begin
// Result := false;
// try
// windowNames := TStringList.Create;
// ListWindows(windowNames);
// for wndName in windowNames do
// if MatchesMask(wndName, aWindowTitleMask) then
// begin
// aWndHandle := HWND(windowNames.Objects[windowNames.IndexOf(wndName)]);
// aWndName := wndName;
// Result := true;
// end;
// finally
// windowNames.Free;
// end;
// end;

procedure TfrmScreenTranslator.FormCreate(Sender: TObject);
begin
  appWorkMode := awmWindowDefine;
  helper := TScreenShotHelper.Create(adtPixels, Image1.Picture);
  ta := TTextAnalyser.Create(10000);
  if not Ocr1.Active then
    begin
      Ocr1.DataPath := ExtractFilePath(Application.ExeName) + 'tessdata';
      Ocr1.Active := true;
    end;
end;

procedure TfrmScreenTranslator.FormDestroy(Sender: TObject);
begin
  helper.Destory;
end;

procedure TfrmScreenTranslator.FormResize(Sender: TObject);
begin
  UpdatePaddingEdits();
end;

procedure TfrmScreenTranslator.FormShow(Sender: TObject);
var
  wndHandle: HWND;
  wndName: string;

begin
  if helper.FindWindowByTite('', wndHandle, wndName) then
    begin
      frmScreenTranslator.TransparentColor := true;
      frmScreenTranslator.TransparentColorValue := clAppWorkSpace;
      frmScreenTranslator.BorderIcons := [];
      frmScreenTranslator.SetFocus;
      SetWindowPos(frmScreenTranslator.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
      BringWindowToTop(wndHandle);
      ShowWindow(wndHandle, SW_SHOWNORMAL);
      GetWindowRect(wndHandle, helper.windowRect);
      // helper.windowRect.Inflate
      // InflateRect(windowRect, 4, 2);
      appWorkMode := awmRangeDefine;
      UpdatePaddingEdits();
    end
  else
    begin
      Image1.Picture := nil;
      frmScreenTranslator.TransparentColor := false;
      SetWindowPos(frmScreenTranslator.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
    end;

end;

function TfrmScreenTranslator.ListWindows(aList: TStrings): Boolean;
var
  wndHandle: HWND;
  s: string;

  function IsWindowInMyList(aNewWndHandle: HWND): Boolean;
  var
    i: Integer;
  begin
    Result := false;
    for i := 0 to Pred(aList.Count) do
      begin
        if HWND(aList.Objects[i]) = aNewWndHandle then
          begin
            Result := true;
            Exit;
          end;
      end;
  end;

begin
  // ListBox1.Clear;
  wndHandle := GetTopWindow(0);
  repeat
    if not IsWindowInMyList(wndHandle) then
      begin

        if (GetWindowTitle(wndHandle, s) and IsWindowVisible(wndHandle)) then
          aList.AddObject(s, TObject(wndHandle));
      end;
    wndHandle := GetNextWindow(wndHandle, GW_HWNDNEXT);
  until (wndHandle = 0);
end;

procedure TfrmScreenTranslator.OnMove(var Msg: TWMMove);
begin
  inherited;
  UpdatePaddingEdits();
end;

procedure TfrmScreenTranslator.tm_translationTimer(Sender: TObject);
var
  newStr: string;
  newStrTime: longint;
begin
  if appWorkMode = awmTranslation then
    begin
      DrawFrame();
      Image1.Picture := nil;
      Application.ProcessMessages;
      helper.GetScreenShot(false);
      Ocr1.Picture.Assign(Image1.Picture);
      Application.ProcessMessages;
      Ocr1.Recognize;
      //      frmRangeSelect.Memo1.Lines.Add('----------');
      //      frmRangeSelect.Memo1.Lines.Add(Ocr1.Text);
      ta.AddSourceRec(Ocr1.Text);
      ta.DoAnalysis();
      Application.ProcessMessages;
      while ta.ReadEntry(newStr, newStrTime) do
        begin
          frmRangeSelect.Memo1.Lines.Add(newStr);
        end;
      DrawFrame();
    end;
end;

procedure TfrmScreenTranslator.UpdatePaddingEdits;
begin
  if appWorkMode = awmRangeDefine then
    begin
      Image1.Picture := nil;
      Image1.Visible := false;
      helper.UpdateWindowRect();
      e_paddingLeft.Text := IntToStr(frmScreenTranslator.Left - helper.GetWndRect().Left + 12);
      e_paddingTop.Text := IntToStr(frmScreenTranslator.Top + p_top.Height + 42 - helper.GetWndRect().Top);
      e_paddingRight.Text := IntToStr(helper.GetWndRect().Right - (frmScreenTranslator.Left + frmScreenTranslator.Width));
      e_paddingBottom.Text := IntToStr(helper.GetWndRect().Bottom - (frmScreenTranslator.Top + frmScreenTranslator.Height));
    end;
end;

procedure TfrmScreenTranslator.b_runClick(Sender: TObject);
var
  i: Integer;
begin
  if not helper.IsInitialised() then
    begin
      MsgBox('Set translation area first');
      Exit;
    end;

  if appWorkMode = awmRangeDefine then
    begin
      Image1.Picture := nil;
      Application.ProcessMessages;
      helper.SetParams(StrToInt(e_paddingLeft.Text), StrToInt(e_paddingRight.Text), StrToInt(e_paddingTop.Text), StrToInt(e_paddingBottom.Text));
      helper.GetScreenShot(true);
      Image1.Visible := true;
      Application.ProcessMessages;
      Sleep(100);
      Application.ProcessMessages;
      helper.GetScreenShot(true);
      Application.ProcessMessages;
      Sleep(100);
      Application.ProcessMessages;
      helper.GetScreenShot(true);
      Application.ProcessMessages;
      Sleep(100);
      helper.GetScreenShot(true);
      Application.ProcessMessages;
      frmRangeSelect.Memo1.Clear;
      appWorkMode := awmTranslation;
      WindowState := wsMinimized;
      frmRangeSelect.Left := frmScreenTranslator.Left + frmScreenTranslator.Width;
      frmRangeSelect.Show;
      tm_translation.Enabled := true;
    end;

end;

end.

