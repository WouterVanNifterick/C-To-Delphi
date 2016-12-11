object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'C to Delphi'
  ClientHeight = 833
  ClientWidth = 1339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 793
    Top = 21
    Width = 8
    Height = 688
    ResizeStyle = rsUpdate
    ExplicitTop = 0
    ExplicitHeight = 1058
  end
  object Splitter2: TSplitter
    Left = 241
    Top = 21
    Width = 8
    Height = 688
    ResizeStyle = rsUpdate
    ExplicitLeft = 162
    ExplicitTop = -11
    ExplicitHeight = 887
  end
  object Splitter3: TSplitter
    Left = 0
    Top = 709
    Width = 1339
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
    ExplicitLeft = 8
    ExplicitTop = 939
    ExplicitWidth = 1778
  end
  object BCEditor1: TBCEditor
    Left = 249
    Top = 21
    Width = 544
    Height = 688
    Cursor = crIBeam
    ActiveLine.Indicator.Visible = False
    Align = alLeft
    Caret.MultiEdit.Enabled = True
    Caret.NonBlinking.Enabled = False
    Caret.Options = []
    CodeFolding.Colors.Indent = clBlack
    CodeFolding.Hint.Font.Charset = DEFAULT_CHARSET
    CodeFolding.Hint.Font.Color = clWindowText
    CodeFolding.Hint.Font.Height = -11
    CodeFolding.Hint.Font.Name = 'Courier New'
    CodeFolding.Hint.Font.Style = []
    CodeFolding.Hint.Indicator.Glyph.Visible = False
    CodeFolding.Visible = True
    CompletionProposal.CloseChars = '()[]. '
    CompletionProposal.Columns = <
      item
      end>
    CompletionProposal.Font.Charset = DEFAULT_CHARSET
    CompletionProposal.Font.Color = clWindowText
    CompletionProposal.Font.Height = -11
    CompletionProposal.Font.Name = 'Courier New'
    CompletionProposal.Font.Style = []
    CompletionProposal.ShortCut = 16416
    CompletionProposal.Trigger.Chars = '.'
    CompletionProposal.Trigger.Enabled = False
    Constraints.MinHeight = 150
    Constraints.MinWidth = 200
    Directories.Colors = 'Colors'
    Directories.Highlighters = 'Highlighters'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    LeftMargin.Font.Charset = DEFAULT_CHARSET
    LeftMargin.Font.Color = 13408665
    LeftMargin.Font.Height = -11
    LeftMargin.Font.Name = 'Courier New'
    LeftMargin.Font.Style = []
    LeftMargin.Width = 55
    Lines.Strings = (
      'void hello(int x){'
      '    printf("Hello, world, %d\n",x);'
      '}'
      ''
      'int main(){'
      '  for(int i=0;i<=10;i++){'
      '    hello(i);'
      '  } '
      '}')
    LineSpacing = 0
    MatchingPair.Enabled = True
    Minimap.Font.Charset = DEFAULT_CHARSET
    Minimap.Font.Color = clWindowText
    Minimap.Font.Height = -1
    Minimap.Font.Name = 'Courier New'
    Minimap.Font.Style = []
    Minimap.Width = 140
    OnCaretChanged = BCEditor1CaretChanged
    OnChange = BCEditor1Change
    RightMargin.Position = 80
    RightMargin.Visible = False
    Scroll.Shadow.Visible = True
    Selection.Options = [soExpandRealNumbers, soHighlightSimilarTerms, soTermsCaseSensitive]
    SpecialChars.Style = scsDot
    SyncEdit.ShortCut = 24650
    TabOrder = 0
    WordWrap.Enabled = False
    WordWrap.Indicator.Glyph.Data = {
      7E030000424D7E0300000000000036000000280000000F0000000E0000000100
      2000000000004803000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
      0000FF00FF00FF00FF00FF00FF00FF00FF008000000080000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF008000000080000000800000008000000080000000FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF008000000080000000FF00FF00FF00FF0080000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0080000000FF00FF00FF00FF0080000000FF00FF00FF00
      FF00FF00FF000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0080000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00800000008000000080000000800000008000
      00008000000080000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00}
    WordWrap.Indicator.MaskColor = clFuchsia
    WordWrap.Width = wwwPage
  end
  object BCEditor2: TBCEditor
    Left = 801
    Top = 21
    Width = 538
    Height = 688
    Cursor = crIBeam
    ActiveLine.Indicator.Visible = False
    Align = alClient
    Caret.MultiEdit.Enabled = True
    Caret.NonBlinking.Enabled = False
    Caret.Options = []
    CodeFolding.Colors.Indent = clBlack
    CodeFolding.Hint.Font.Charset = DEFAULT_CHARSET
    CodeFolding.Hint.Font.Color = clWindowText
    CodeFolding.Hint.Font.Height = -11
    CodeFolding.Hint.Font.Name = 'Courier New'
    CodeFolding.Hint.Font.Style = []
    CodeFolding.Hint.Indicator.Glyph.Visible = False
    CodeFolding.Visible = True
    CompletionProposal.CloseChars = '()[]. '
    CompletionProposal.Columns = <
      item
      end>
    CompletionProposal.Font.Charset = DEFAULT_CHARSET
    CompletionProposal.Font.Color = clWindowText
    CompletionProposal.Font.Height = -11
    CompletionProposal.Font.Name = 'Courier New'
    CompletionProposal.Font.Style = []
    CompletionProposal.ShortCut = 16416
    CompletionProposal.Trigger.Chars = '.'
    CompletionProposal.Trigger.Enabled = False
    Constraints.MinHeight = 150
    Constraints.MinWidth = 200
    Directories.Colors = 'Colors'
    Directories.Highlighters = 'Highlighters'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    LeftMargin.Font.Charset = DEFAULT_CHARSET
    LeftMargin.Font.Color = 13408665
    LeftMargin.Font.Height = -11
    LeftMargin.Font.Name = 'Courier New'
    LeftMargin.Font.Style = []
    LeftMargin.Width = 55
    Lines.Strings = (
      '')
    LineSpacing = 0
    MatchingPair.Enabled = True
    Minimap.Font.Charset = DEFAULT_CHARSET
    Minimap.Font.Color = clWindowText
    Minimap.Font.Height = -1
    Minimap.Font.Name = 'Courier New'
    Minimap.Font.Style = []
    Minimap.Width = 140
    OnCaretChanged = BCEditor2CaretChanged
    OnChange = BCEditor2Change
    PopupMenu = PopupMenu1
    RightMargin.Position = 80
    RightMargin.Visible = True
    Scroll.Shadow.Visible = True
    Selection.Options = [soExpandRealNumbers, soHighlightSimilarTerms, soTermsCaseSensitive]
    SpecialChars.Style = scsDot
    SyncEdit.ShortCut = 24650
    TabOrder = 1
    WordWrap.Enabled = False
    WordWrap.Indicator.Glyph.Data = {
      7E030000424D7E0300000000000036000000280000000F0000000E0000000100
      2000000000004803000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000
      000000000000000000000000000000000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0000000000000000000000
      0000FF00FF00FF00FF00FF00FF00FF00FF008000000080000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF008000000080000000800000008000000080000000FF00
      FF00FF00FF00FF00FF00FF00FF00000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF008000000080000000FF00FF00FF00FF0080000000FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0080000000FF00FF00FF00FF0080000000FF00FF00FF00
      FF00FF00FF000000000000000000000000000000000000000000FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF0080000000FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF0080000000FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00800000008000000080000000800000008000
      00008000000080000000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
      FF00}
    WordWrap.Indicator.MaskColor = clFuchsia
    WordWrap.Width = wwwPage
  end
  object SearchBox1: TSearchBox
    Left = 0
    Top = 0
    Width = 1339
    Height = 21
    Align = alTop
    TabOrder = 2
    TextHint = 'Search'
    OnChange = SearchBox1Change
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 21
    Width = 241
    Height = 688
    Align = alLeft
    AutoExpand = True
    Indent = 19
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnChange = TreeView1Change
  end
  object ListBox1: TListBox
    Left = 0
    Top = 717
    Width = 1339
    Height = 97
    Align = alBottom
    ItemHeight = 13
    TabOrder = 4
    OnDblClick = ListBox1DblClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 814
    Width = 1339
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object BCDragDrop1: TBCDragDrop
    AcceptDrag = False
    DropTarget = BCEditor1
    Left = 616
    Top = 352
  end
  object BCDragDrop2: TBCDragDrop
    AcceptDrag = False
    DropTarget = BCEditor2
    Left = 912
    Top = 352
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
end
