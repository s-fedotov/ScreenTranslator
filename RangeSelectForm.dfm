object frmRangeSelect: TfrmRangeSelect
  Left = 0
  Top = 0
  BorderIcons = []
  BorderWidth = 2
  Caption = 'frmRangeSelect'
  ClientHeight = 490
  ClientWidth = 253
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 253
    Height = 463
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 463
    Width = 253
    Height = 27
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 260
    ExplicitWidth = 629
    object Panel2: TPanel
      Left = 66
      Top = 0
      Width = 187
      Height = 27
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 654
      object b_startStop: TBitBtn
        Left = 30
        Top = 2
        Width = 77
        Height = 25
        Caption = 'Stop'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 0
        OnClick = b_startStopClick
      end
      object BitBtn2: TBitBtn
        Left = 110
        Top = 2
        Width = 75
        Height = 25
        Caption = 'Close'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 1
        OnClick = BitBtn2Click
      end
    end
  end
end
