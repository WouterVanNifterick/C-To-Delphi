object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'C to Delphi'
  ClientHeight = 1058
  ClientWidth = 1778
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
    Left = 1042
    Top = 21
    Width = 8
    Height = 913
    ResizeStyle = rsUpdate
    ExplicitLeft = 793
    ExplicitTop = 0
    ExplicitHeight = 1058
  end
  object Splitter2: TSplitter
    Left = 241
    Top = 21
    Width = 8
    Height = 913
    ResizeStyle = rsUpdate
    ExplicitLeft = 162
    ExplicitTop = -11
    ExplicitHeight = 887
  end
  object Splitter3: TSplitter
    Left = 0
    Top = 934
    Width = 1778
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
    ExplicitLeft = 8
    ExplicitTop = 939
  end
  object BCEditor1: TBCEditor
    Left = 249
    Top = 21
    Width = 793
    Height = 913
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
      '#include <stdlib.h>'
      '#include <stdio.h>'
      ''
      '#define NB_ITS 1000000'
      '//#define NB_ITS 1'
      '#define TAB_SIZE 100'
      ''
      'int tab[TAB_SIZE];'
      'int ret_sum;'
      'char tab3[256];'
      ''
      'int test1(void)'
      '{'
      '    int i, sum = 0;'
      '    for(i=0;i<TAB_SIZE;i++) {'
      '        sum += tab[i];'
      '    }'
      '    return sum;'
      '}'
      ''
      '/* error */'
      'int test2(void)'
      '{'
      '    int i, sum = 0;'
      '    for(i=0;i<TAB_SIZE + 1;i++) {'
      '        sum += tab[i];'
      '    }'
      '    return sum;'
      '}'
      ''
      '/* actually, profiling test */'
      'int test3(void)'
      '{'
      '    int sum;'
      '    int i, it;'
      ''
      '    sum = 0;'
      '    for(it=0;it<NB_ITS;it++) {'
      '        for(i=0;i<TAB_SIZE;i++) {'
      '            sum += tab[i];'
      '        }'
      '    }'
      '    return sum;'
      '}'
      ''
      '/* ok */'
      'int test4(void)'
      '{'
      '    int i, sum = 0;'
      '    int *tab4;'
      ''
      '    tab4 = malloc(20 * sizeof(int));'
      '    for(i=0;i<20;i++) {'
      '        sum += tab4[i];'
      '    }'
      '    free(tab4);'
      ''
      '    return sum;'
      '}'
      ''
      '/* error */'
      'int test5(void)'
      '{'
      '    int i, sum = 0;'
      '    int *tab4;'
      ''
      '    tab4 = malloc(20 * sizeof(int));'
      '    for(i=0;i<21;i++) {'
      '        sum += tab4[i];'
      '    }'
      '    free(tab4);'
      ''
      '    return sum;'
      '}'
      ''
      '/* error */'
      '/* XXX: currently: bug */'
      'int test6(void)'
      '{'
      '    int i, sum = 0;'
      '    int *tab4;'
      '    '
      '    tab4 = malloc(20 * sizeof(int));'
      '    free(tab4);'
      '    for(i=0;i<21;i++) {'
      '        sum += tab4[i];'
      '    }'
      ''
      '    return sum;'
      '}'
      ''
      '/* error */'
      'int test7(void)'
      '{'
      '    int i, sum = 0;'
      '    int *p;'
      ''
      '    for(i=0;i<TAB_SIZE + 1;i++) {'
      '        p = &tab[i];'
      '        if (i == TAB_SIZE)'
      '            printf("i=%d %x\n", i, p);'
      '        sum += *p;'
      '    }'
      '    return sum;'
      '}'
      ''
      '/* ok */'
      'int test8(void)'
      '{'
      '    int i, sum = 0;'
      '    int tab[10];'
      ''
      '    for(i=0;i<10;i++) {'
      '        sum += tab[i];'
      '    }'
      '    return sum;'
      '}'
      ''
      '/* error */'
      'int test9(void)'
      '{'
      '    int i, sum = 0;'
      '    char tab[10];'
      ''
      '    for(i=0;i<11;i++) {'
      '        sum += tab[i];'
      '    }'
      '    return sum;'
      '}'
      ''
      '/* ok */'
      'int test10(void)'
      '{'
      '    char tab[10];'
      '    char tab1[10];'
      ''
      '    memset(tab, 0, 10);'
      '    memcpy(tab, tab1, 10);'
      '    memmove(tab, tab1, 10);'
      '    return 0;'
      '}'
      ''
      '/* error */'
      'int test11(void)'
      '{'
      '    char tab[10];'
      ''
      '    memset(tab, 0, 11);'
      '    return 0;'
      '}'
      ''
      '/* error */'
      'int test12(void)'
      '{'
      '    void *ptr;'
      '    ptr = malloc(10);'
      '    free(ptr);'
      '    free(ptr);'
      '    return 0;'
      '}'
      ''
      '/* error */'
      'int test13(void)'
      '{'
      '    char pad1 = 0;'
      '    char tab[10];'
      '    char pad2 = 0;'
      '    memset(tab, '#39'a'#39', sizeof(tab));'
      '    return strlen(tab);'
      '}'
      ''
      'int test14(void)'
      '{'
      '    char *p = alloca(TAB_SIZE);'
      '    memset(p, '#39'a'#39', TAB_SIZE);'
      '    p[TAB_SIZE-1] = 0;'
      '    return strlen(p);'
      '}'
      ''
      '/* error */'
      'int test15(void)'
      '{'
      '    char *p = alloca(TAB_SIZE-1);'
      '    memset(p, '#39'a'#39', TAB_SIZE);'
      '    p[TAB_SIZE-1] = 0;'
      '    return strlen(p);'
      '}'
      ''
      'int (*table_test[])(void) = {'
      '    test1,'
      '    test1,'
      '    test2,'
      '    test3,'
      '    test4,'
      '    test5,'
      '    test6,'
      '    test7,'
      '    test8,'
      '    test9,'
      '    test10,'
      '    test11,'
      '    test12,'
      '    test13,'
      '    test14,'
      '    test15,'
      '};'
      ''
      'int main(int argc, char **argv)'
      '{'
      '    int index;'
      '    int (*ftest)(void);'
      ''
      '    if (argc < 2) {'
      '        printf("usage: boundtest n\n"'
      '               "test TCC bound checking system\n"'
      '               );'
      '        exit(1);'
      '    }'
      ''
      '    index = 0;'
      '    if (argc >= 2)'
      '        index = atoi(argv[1]);'
      '    /* well, we also use bounds on this ! */'
      '    ftest = table_test[index];'
      '    ftest();'
      ''
      '    return 0;'
      '}'
      ''
      '/*'
      ' * without bound   0.77 s'
      ' * with bounds    4.73'
      ' */   ')
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
    Left = 1050
    Top = 21
    Width = 728
    Height = 913
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
    Width = 1778
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
    Height = 913
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
    Top = 942
    Width = 1778
    Height = 97
    Align = alBottom
    ItemHeight = 13
    TabOrder = 4
    OnDblClick = ListBox1DblClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 1039
    Width = 1778
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
end
