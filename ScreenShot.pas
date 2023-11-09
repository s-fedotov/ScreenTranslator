unit ScreenShot;

interface

uses Windows, SysUtils, Forms, Graphics, ExtCtrls;

type // define an ENUM to describe the possible screenshot types.
  TScreenShotType = (sstActiveWindow, sstActiveClientArea, sstPrimaryMonitor, sstDesktop);

procedure GetScreenShot(hWin: HWND; var img: TImage);

implementation

procedure GetScreenShot(hWin: HWND; var img: TImage);
var
  w, h: integer;
  DC: HDC;

  r: TRect;
  tmpBmp: TBitmap;
begin
  if hWin = 0 then
    begin
      hWin := GetDesktopWindow;
      DC := GetWindowDC(hWin);
      w := GetDeviceCaps(DC, HORZRES);
      h := GetDeviceCaps(DC, VERTRES);
    end
  else
    begin
      DC := GetWindowDC(hWin);
      GetWindowRect(hWin, r);
      w := r.Right - r.Left;
      h := r.Bottom - r.Top;
    end;

  // convert to jpg
  tmpBmp := TBitmap.Create;
  try
    tmpBmp.Width := w;
    tmpBmp.Height := h;
    BitBlt(tmpBmp.Canvas.Handle, 0, 0, tmpBmp.Width, tmpBmp.Height, DC, 0, 0, SRCCOPY);
    img.Picture.Assign(tmpBmp);
  finally
    ReleaseDC(hWin, DC);
    FreeAndNil(tmpBmp);
  end; // try-finally
end;

end.
