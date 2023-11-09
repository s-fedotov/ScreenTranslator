unit ScreenShotHelper;

interface

uses Windows, Graphics;

type

  TAreaDetectType = (adtPixels, adtPercents);

  TScreenShotHelper = class
  private
    areaDetectType: TAreaDetectType;
    paddingLeft, paddingRight, paddingTop, paddingBottom: double;
    //    desktopRect: TRect;
    windowHandle: HWND;
    // windowTitleMask: string;
    windowTitle: string;
    bitmap: TPicture;
    //    bitmap: TBitmap;
    function IIf(aValue: boolean; aIfTrue, aIfFalse: integer): integer;

  public
    windowRect: TRect;
    constructor Create(aAreaDetectType: TAreaDetectType; aBitmap: TPicture);
    destructor Destory();
    procedure SetParams(aPaddingLeft, aPaddingRight, aPaddingTop, aPaddingBottom: double);
    function FindWindowByTite(aWindowTitleMask: string; var aWndHandle: HWND; var aWindowName: string): boolean;
    function GetScreenShot(aInvertMode: boolean): boolean;
    //    function GetBitmap(): TBitmap;
    function GetWndRect(): TRect;
    function GetTargetRect(): TRect;
    function GetWndHandle(): HWND;

    procedure UpdateWindowRect();
    function IsInitialised(): boolean;

  end;

implementation

uses SysUtils, Classes, Masks, StrUtils;

{ TScreenShotHelper }

constructor TScreenShotHelper.Create(aAreaDetectType: TAreaDetectType; aBitmap: TPicture);
begin
  areaDetectType := aAreaDetectType;
  bitmap := aBitmap;
  //  bitmap := TBitmap.Create;
  windowHandle := 0;
end;

//function TScreenShotHelper.GetBitmap: TBitmap;
//begin
//  Result := bitmap;
//end;

function TScreenShotHelper.GetScreenShot(aInvertMode: boolean): boolean;
var
  desktopHandle: HWND;
  desktopDC: HDC;
  targetRect: TRect;
  // screenWidth, screenHeight: integer;
  tmpBmp: TBitmap;
  w, h: integer;

begin
  Result := false;
  desktopHandle := GetDesktopWindow;
  desktopDC := GetWindowDC(desktopHandle);
  //  GetWindowRect(desktopHandle, desktopRect);
    // GetWindowRect(windowHandle, windowRect);
  targetRect := GetTargetRect();
  tmpBmp := TBitmap.Create;

  try
    tmpBmp.Width := targetRect.Right - targetRect.Left;
    tmpBmp.Height := targetRect.Bottom - targetRect.Top;
    //     BitBlt(tmpBmp.Canvas.Handle, 0, 0, tmpBmp.Width, tmpBmp.Height, desktopDC, targetRect.Left, targetRect.Top, SRCCOPY);
    BitBlt(tmpBmp.Canvas.Handle, 0, 0, tmpBmp.Width, tmpBmp.Height, desktopDC, targetRect.Left, targetRect.Top, IIf(aInvertMode, NOTSRCCOPY, SRCCOPY));
    bitmap.Assign(tmpBmp);
  finally
    ReleaseDC(desktopHandle, desktopDC);
    FreeAndNil(tmpBmp);
  end;
end;

function TScreenShotHelper.GetTargetRect: TRect;
begin
  // обработаем паддинги внутри целевого окна
  Result.Left := windowRect.Left + Round(paddingLeft);
  Result.Top := windowRect.Top + Round(paddingTop);
  Result.Right := windowRect.Right - Round(paddingRight);
  Result.Bottom := windowRect.Bottom - Round(paddingBottom);

end;

function TScreenShotHelper.GetWndHandle: HWND;
begin
  Result := windowHandle;
end;

function TScreenShotHelper.GetWndRect: TRect;
begin
  Result := windowRect;
end;

function TScreenShotHelper.IIf(aValue: boolean; aIfTrue, aIfFalse: integer): integer;
begin
  if aValue then
    Result := aIfTrue
  else
    Result := aIfFalse;
end;

function TScreenShotHelper.IsInitialised: boolean;
begin
  Result := (windowHandle <> 0) or (windowTitle<>'') ;
end;

procedure TScreenShotHelper.SetParams(aPaddingLeft, aPaddingRight, aPaddingTop, aPaddingBottom: double);
begin
  paddingLeft := aPaddingLeft;
  paddingRight := aPaddingRight;
  paddingTop := aPaddingTop;
  paddingBottom := aPaddingBottom;
  // windowTitleMask := aWindowTitleMask;
end;

procedure TScreenShotHelper.UpdateWindowRect;
begin
  GetWindowRect(windowHandle, windowRect);
end;

destructor TScreenShotHelper.Destory;
begin
  //  bitmap.Free;
end;

function TScreenShotHelper.FindWindowByTite(aWindowTitleMask: string; var aWndHandle: HWND; var aWindowName: string): boolean;
var
  hWndTemp: HWND;
  iLenText: integer;
  cTitletemp: array[0..254] of Char;
  sTitleTemp: string;
begin
  Result := false;
  windowHandle := 0;
  if aWindowTitleMask = '' then
    begin
      aWndHandle := GetDesktopWindow();
      aWindowName := 'Desktop';
      windowHandle:=aWndHandle;
      windowTitle:=aWindowName;
      Result := true;
      Exit;
    end;
  hWndTemp := FindWindow(nil, nil);
  while hWndTemp <> 0 do
    begin
      iLenText := GetWindowText(hWndTemp, cTitletemp, 255);
      sTitleTemp := cTitletemp;
      sTitleTemp := UpperCase(copy(sTitleTemp, 1, iLenText));
      aWindowTitleMask := UpperCase(aWindowTitleMask);
      if MatchesMask(sTitleTemp, aWindowTitleMask) then
        begin
          Result := true;
          aWndHandle := hWndTemp;
          aWindowName := sTitleTemp;
          windowHandle := aWndHandle;
          Break;
        end;
      hWndTemp := GetWindow(hWndTemp, GW_HWNDNEXT);
    end;
end;

end.

