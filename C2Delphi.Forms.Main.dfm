object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'C to Delphi'
  ClientHeight = 834
  ClientWidth = 1184
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 777
    Top = 21
    Width = 8
    Height = 672
    ResizeStyle = rsUpdate
    ExplicitLeft = 1009
    ExplicitTop = 77
    ExplicitHeight = 671
  end
  object Splitter2: TSplitter
    Left = 241
    Top = 21
    Width = 8
    Height = 672
    ResizeStyle = rsUpdate
    ExplicitLeft = 162
    ExplicitTop = -11
    ExplicitHeight = 887
  end
  object Splitter3: TSplitter
    Left = 0
    Top = 710
    Width = 1184
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
    ExplicitLeft = 8
    ExplicitTop = 939
    ExplicitWidth = 1778
  end
  object SearchBox1: TSearchBox
    Left = 0
    Top = 0
    Width = 1184
    Height = 21
    Align = alTop
    TabOrder = 0
    TextHint = 'Search'
    OnChange = SearchBox1Change
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 21
    Width = 241
    Height = 672
    Align = alLeft
    HideSelection = False
    Indent = 19
    ParentShowHint = False
    ReadOnly = True
    RowSelect = True
    ShowHint = True
    ShowRoot = False
    TabOrder = 1
    OnChange = TreeView1Change
    OnCustomDrawItem = TreeView1CustomDrawItem
  end
  object ListBox1: TListBox
    Left = 0
    Top = 718
    Width = 1184
    Height = 97
    Align = alBottom
    ItemHeight = 13
    TabOrder = 2
    OnDblClick = ListBox1DblClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 815
    Width = 1184
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 100
      end
      item
        Width = 500
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 693
    Width = 1184
    Height = 17
    Align = alBottom
    Smooth = True
    MarqueeInterval = 2
    TabOrder = 4
  end
  object Panel1: TPanel
    Left = 249
    Top = 21
    Width = 528
    Height = 672
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 5
    object StatusBar3: TStatusBar
      Left = 0
      Top = 653
      Width = 528
      Height = 19
      Panels = <
        item
          Width = 80
        end
        item
          Width = 50
        end>
    end
    object edCCode: TSynEdit
      Left = 0
      Top = 0
      Width = 528
      Height = 653
      Align = alClient
      Color = 2238503
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      TabOrder = 1
      OnClick = edCCodeSelectionChanged
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Gutter.Visible = False
      Gutter.Width = 0
      Highlighter = SynCppSyn1
      Lines.Strings = (
        'edCCode')
      Options = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
      RightEdge = 0
      OnChange = edCCodeChange
      OnSpecialLineColors = edCCodeSpecialLineColors
      FontSmoothing = fsmNone
      ExplicitWidth = 312
    end
  end
  object Panel2: TPanel
    Left = 785
    Top = 21
    Width = 399
    Height = 672
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 6
    object StatusBar2: TStatusBar
      Left = 0
      Top = 653
      Width = 399
      Height = 19
      Panels = <
        item
          Width = 80
        end
        item
          Width = 50
        end>
    end
    object edPascalCode: TSynEdit
      Left = 0
      Top = 0
      Width = 399
      Height = 653
      Align = alClient
      Color = 2238503
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      TabOrder = 1
      OnClick = edPascalCodeClick
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Gutter.Visible = False
      Gutter.Width = 0
      Highlighter = SynPasSyn1
      Lines.Strings = (
        'syndt1')
      Options = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
      RightEdge = 0
      OnSpecialLineColors = edPascalCodeSpecialLineColors
      FontSmoothing = fsmNone
    end
  end
  object ActionManager1: TActionManager
    Left = 368
    Top = 256
    StyleName = 'Platform Default'
    object Action1: TAction
      Caption = '&Save'
      ShortCut = 16467
      OnExecute = Action1Execute
    end
    object actRun: TAction
      Caption = 'Run'
      ShortCut = 120
      OnExecute = actRunExecute
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 992
    Top = 448
    object Run1: TMenuItem
      Action = actRun
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnException = ApplicationEvents1Exception
    OnHint = ApplicationEvents1Hint
    Left = 448
    Top = 352
  end
  object SynCppSyn1: TSynCppSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    AsmAttri.Background = clBlack
    AsmAttri.Foreground = clLime
    CommentAttri.Background = 2238503
    CommentAttri.Foreground = 9671571
    IdentifierAttri.Foreground = clSilver
    KeyAttri.Foreground = 7481081
    NumberAttri.Foreground = 16744878
    HexAttri.Foreground = 16744878
    OctalAttri.Foreground = 16744878
    StringAttri.Foreground = 7658470
    CharAttri.Foreground = clLime
    Left = 464
    Top = 192
  end
  object SynPasSyn1: TSynPasSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    AsmAttri.Background = clBlack
    AsmAttri.Foreground = clLime
    CommentAttri.Background = 2238503
    CommentAttri.Foreground = 9671571
    IdentifierAttri.Foreground = clSilver
    KeyAttri.Foreground = 7481081
    NumberAttri.Foreground = 16744878
    HexAttri.Foreground = 16744878
    StringAttri.Foreground = 7658470
    CharAttri.Foreground = clLime
    Left = 912
    Top = 160
  end
end
