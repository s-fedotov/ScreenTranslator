object frmScreenTranslator: TfrmScreenTranslator
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'frmScreenTranslator'
  ClientHeight = 337
  ClientWidth = 762
  Color = clAppWorkSpace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object Image1: TImage
    Left = 3
    Top = 42
    Width = 756
    Height = 292
    Align = alClient
    ExplicitTop = 51
    ExplicitHeight = 286
  end
  object Panel4: TPanel
    Left = 0
    Top = 39
    Width = 762
    Height = 3
    Align = alTop
    BevelOuter = bvNone
    Color = clRed
    ParentBackground = False
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 42
    Width = 3
    Height = 292
    Align = alLeft
    BevelOuter = bvNone
    Color = clRed
    ParentBackground = False
    TabOrder = 1
  end
  object Panel3: TPanel
    Left = 759
    Top = 42
    Width = 3
    Height = 292
    Align = alRight
    BevelOuter = bvNone
    Color = clRed
    ParentBackground = False
    TabOrder = 2
  end
  object Panel5: TPanel
    Left = 0
    Top = 334
    Width = 762
    Height = 3
    Align = alBottom
    BevelOuter = bvNone
    Color = clRed
    ParentBackground = False
    TabOrder = 3
  end
  object p_top: TPanel
    Left = 0
    Top = 0
    Width = 762
    Height = 39
    Align = alTop
    ParentBackground = False
    TabOrder = 4
    object GroupBox1: TGroupBox
      Left = 6
      Top = 1
      Width = 331
      Height = 34
      Caption = 'Paddings'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -10
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      object Label4: TLabel
        Left = 76
        Top = 11
        Width = 28
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Left'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 150
        Top = 11
        Width = 50
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Bottom'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 246
        Top = 11
        Width = 37
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Right'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label3: TLabel
        Left = 4
        Top = 11
        Width = 27
        Height = 14
        Alignment = taRightJustify
        AutoSize = False
        Caption = 'Top'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object e_paddingLeft: TEdit
        Left = 110
        Top = 11
        Width = 33
        Height = 17
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        Text = '0'
      end
      object e_paddingBottom: TEdit
        Left = 206
        Top = 11
        Width = 33
        Height = 17
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        Text = '0'
      end
      object e_paddingRight: TEdit
        Left = 289
        Top = 11
        Width = 33
        Height = 17
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        Text = '0'
      end
      object e_paddingTop: TEdit
        Left = 34
        Top = 11
        Width = 33
        Height = 17
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        Text = '0'
      end
    end
    object b_run: TButton
      Left = 429
      Top = 8
      Width = 50
      Height = 25
      Caption = 'RUN'
      TabOrder = 1
      OnClick = b_runClick
    end
    object Button3: TButton
      Left = 485
      Top = 8
      Width = 48
      Height = 25
      Caption = 'Close'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button1: TButton
      Left = 659
      Top = 8
      Width = 30
      Height = 25
      Caption = 'b1'
      TabOrder = 3
      OnClick = Button1Click
    end
    object Button4: TButton
      Left = 695
      Top = 8
      Width = 29
      Height = 25
      Caption = 'b2'
      TabOrder = 4
      OnClick = Button4Click
    end
    object Button2: TButton
      Left = 726
      Top = 8
      Width = 33
      Height = 25
      Caption = 'b3'
      TabOrder = 5
      OnClick = Button2Click
    end
    object c_savePict: TCheckBox
      Left = 343
      Top = 12
      Width = 82
      Height = 17
      Caption = 'Save pict'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
  end
  object Ocr1: TOcr
    Left = 12
    Top = 44
  end
  object tm_translation: TTimer
    Enabled = False
    Interval = 2000
    OnTimer = tm_translationTimer
    Left = 86
    Top = 44
  end
end
