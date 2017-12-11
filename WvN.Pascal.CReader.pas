unit WvN.Pascal.CReader;

interface

uses
  Windows,
  WvN.Pascal.Model, System.Threading;

type
  TOnProgress = reference to procedure(progress:double;const text:string);

  function c_FunctionToPas(const aUnit:TPascalUnit; const c:string;var aRoutine:TRoutine):boolean;
  procedure c_to_pas(const aCCode:string; var t:string;aName:string;aOnProgress:TOnProgress; var result:TPascalUnit);
  function FixComments(const aCCode:string):string;

implementation

uses
  IoUTils,
  SysUtils,
  System.RegularExpressions,
  Classes;

const
  PARSED_MARKER = ' ';
  PARSED_MARKER_STR = PARSED_MARKER+PARSED_MARKER+PARSED_MARKER;

const
  NonFunctions:array[0..12] of string = ('if','enum','struct','for','else','while','switch','for','case','class','namespace','do','const');

const
  rxID     = '[a-zA-Z_\$][\w_]*(\s*\*)?';
  rxT = '(\*\s*)?(static\s+)?(unsigned\s+|signed\s+)?(long\s+)?[a-zA-Z_\$][\w_\<\>]*\s*(\*\s*)?';
  rxType   = rxT;
  rxMultiLinecomment = '(?<comment>\s*)?';
  rxStringLiteral = '[a-zA-Z_]?\"(\\.|[^\\"\n])*\"';

  rxHexadecimal = '0[xX]([a-fA-F0-9]+)((u|U)|((u|U)?(l|L|ll|LL))|((l|L|ll|LL)(u|U)))?';
  rxNum    = '(?:[^a-zA-Z])[\+\-]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?';
  rxChar = 'L?\''([^\\n]|(\(.|\n)))*?\''';

  rxMethodDef=
             '^'+ // only do this trick from the start.. otherwise we might accidently match somewhere else
             rxMultiLineComment+ // preserve multiline comments..
             '(?<comment3>(\/\/([^\n]*)\n)*)?'+ // preserve single line comments
             '(?:\s*)'                 + // to preseve indents of original code
             '(?<template>template\s*\<typename \w+\>\s*)?'+ // template
             '(?<auto>(auto))?'+
             '(?<static>(const|static|auto|extern|register|Static|local|LOCAL))?'             + // text 'static' or 'local'
             '\s*'+
             '(?<inline>inline)?'             + // text 'inline'
             '((?<virtual>virtual)\s+)?'+
             '\s*'+
             '(?<returntype>'+rxType+')'   + // constructors don't have a return type
             '\s+'+
             // some special modifiers.. not sure how to convert this to Delphi though, so let's leave those for now
             '((?<pascal>__pascal)\s+)?'+
             '((?<fastcall>__fastcall)\s+)?'+
             '((?<classname>'+rxID+')\:\:)?'+ // support classname::method() kind of signature
             '(?<destructor>\~)?'+ // constructor looks like MyClass::~MyClass()
             '(?<funcname>'+rxID+')\s*'+      // the function name itself
             '('+ // parameters are not always provided, so the whole parameter section including the brackets is optional
             '\(\s*(?<parameters>[^\)]*?)\s*\)'+
             ')?'+
             '\s*'+
             '(?<comment2>\/\*[^*]*\*\/)?'+ // preserve comments
             '\s*';


  rxMethodHeader=rxMethodDef + '\{';


  rxMethod = rxMethodHeader + '(?<code>.*)?\}';
  rxClassDef = '^(?<indent>\s*)class\s+(?<classname>'+rxId+')\s*\:?\s*(?<public>public)?\s*(?<parentclass>'+rxType+')?\s*\{';

type
  TLoc=(None,InStringQ1,InStringQ2,InLineComment,InMultiLineComment);

function IsFunction(const aCCode:string):boolean;
var s:string;
begin
  for s in NonFunctions do
    if aCCode.StartsWith(s) then
      Exit(False);

  Result := True;
end;


procedure ReplaceMatch(const c: string; const m: TMatch; aReplace: Char);
begin
  c.Remove(m.Index, m.Length);
  c.Insert(m.Index, StringOfChar(aReplace, m.Length));
end;

function Clear(const aCCode, aSearch:string; aReplace:char):string;
var m:TMatch;c:string;
begin
  c := aCCode;
  for m in TRegEx.Matches(aCCode, aSearch) do
    ReplaceMatch(c, m, aReplace);
  Result := c;
end;

function ClearComments(const aCCode:string):string;
begin
  Result := Clear(aCCode,rxMultiLinecomment,' ')
end;


function FixComments(const aCCode:string):string;
var
  loc:TLoc;
  line:string;
  l,i: Integer;
  IsFirstMulti: Boolean;
  sl:TStringList;
begin
  Loc := None;

  IsFirstMulti := True;

  sl := TStringList.Create;
  sl.Text := aCCode;

  for l := 0 to sl.Count-1 do
  begin
    line := sl[l];
    for I := 1 to line.Length do
    begin
      case loc of
        None: begin
                if line[I]='''' then Loc := InStringQ1;
                if line[I]='"'  then Loc := InStringQ2;

                if I < line.Length-1 then
                  if line[I] = '/' then
                    if line[I + 1] = '/' then
                      Loc := InLineComment;

                if I < line.Length-1 then
                  if line[I] = '/' then
                    if line[I + 1] = '*' then
                    begin
                      Loc := InMultiLineComment;
                      line[I+1] := '/';
                      sl[l] := line;
                      IsFirstMulti := True;
                    end;
              end;

        InStringQ1        : if (I>1) and (line[I-1]<>'\')  and (line[I]='''') then Loc := None;
        InStringQ2        : if (I>1) and (line[I-1]<>'\')  and (line[I]='"' ) then Loc := None;
        InMultiLineComment: if (I>1) and (line[I-1] = '*') and (line[I]='/' ) then
                            begin
                              Delete(line,i-1,2);
                              // we're turning multi-line comments into // comments.
                              // that means that we need to move content behind it to
                              // the next line.
                              Insert('//@@@@@@@@',line,i-1);
                              sl[l] := line;
                              loc := None;
                            end;
      end;
    end;
    case loc of
      InStringQ1   : Loc := None;
      InStringQ2   : Loc := None;
      InLineComment: Loc := None;
      InMultiLineComment :
        begin
          if not IsFirstMulti then
            sl[l] := '//' + sl[l] ;
          IsFirstMulti := False;
        end;
    end;
  end;
  Result := sl.Text.Replace('@@@@@@@@','');
  sl.Free;
end;


function StripComments(const aCCode:string):string;
var
  loc:TLoc;
  i,j: Integer;
begin
  Loc := None;

  setlength(result,length(aCCode));

  I := 1; J:=1;
  while I<=aCCode.length do
  begin
    case loc of
      None:
        begin
          if I < aCCode.Length-1 then
            if aCCode[I] = '/' then
              if aCCode[I + 1] = '/' then
                Loc := InLineComment;

          if I < aCCode.Length then
            if aCCode[I] = '/' then
              if aCCode[I + 1] = '*' then
                Loc := InMultiLineComment;

        end;

      InLineComment:
        if CharInSet(aCCode[I], [#13, #10]) then
          Loc := None;

      InMultiLineComment:
        if I > 1 then
          if aCCode[I - 1] = '*' then
            if aCCode[I] = '/' then
              loc := None;

    end;

    if loc = None then
    begin
      Result[J] := aCCode[I];
      Inc(J);
    end;
    inc(I);
  end;
  Setlength(Result,J);
end;

function ReplaceOutsideCommentsAndStrings(aCCode,aSearch,aReplace:string):string;
var
  loc:TLoc;
  i,j: Integer;
  found: Boolean;
begin
  Loc := None;

  I := 1;
  while I<=aCCode.length do
  begin
    case loc of
      None:
        begin
          if aCCode[I]='''' then
            Loc := InStringQ1;

          if aCCode[I]='"' then
            Loc := InStringQ2;

          if I < aCCode.Length-1 then
            if aCCode[I] = '/' then
              if aCCode[I + 1] = '/' then
                Loc := InLineComment;

          if I < aCCode.Length then
            if aCCode[I] = '/' then
              if aCCode[I + 1] = '*' then
                Loc := InMultiLineComment;

        end;

      InStringQ1:
        if I>1 then
          if aCCode[I]='''' then
            if aCCode[I-1]<>'\' then
              Loc := None;

      InStringQ2:
        if I>1 then
          if aCCode[I]='"' then
            if aCCode[I-1]<>'\' then
              Loc := TLoc.None;

      InLineComment:
        if CharInSet(aCCode[I], [#13, #10]) then
          Loc := None;

      InMultiLineComment:
        if I > 1 then
          if aCCode[I - 1] = '*' then
            if aCCode[I] = '/' then
              loc := None;

    end;



    if loc = None then
    begin
      found := true;
      if i+aSearch.Length-1<=length(ACCode) then
      begin
        for j := 1 to length(aSearch) do
        begin
          if aCCode[j+i-1]<>aSearch[J] then
          begin
            found:=False;
            break;
          end;
        end;
      end
      else
        found := false;

      if found then
      begin
        Result := Result + aReplace;
        inc(I,length(aSearch));
        Continue;
      end
    end;
    Result := Result + aCCode[I];
    inc(I);
  end;
end;

procedure ScanUntilMatchingChar(c1: Char; c2: Char; const aCCode: string; var Res: string);overload;
var
  loc:TLoc;
  level: Integer;
  i: Integer;
begin
  level := 1;
  Loc := None;

  for I := 1 to aCCode.Length do
  begin
    case loc of
      None:
        begin
          if aCCode[I]='''' then
            Loc := InStringQ1;

          if aCCode[I]='"' then
            Loc := InStringQ2;

          if I < aCCode.Length-1 then
            if aCCode[I] = '/' then
              if aCCode[I + 1] = '/' then
                Loc := InLineComment;

          if I < aCCode.Length then
            if aCCode[I] = '/' then
              if aCCode[I + 1] = '*' then
                Loc := InMultiLineComment;

        end;

      InStringQ1:
        if I>1 then
          if aCCode[I]='''' then
            if aCCode[I-1]<>'\' then
              Loc := None;

      InStringQ2:
        if I>1 then
          if aCCode[I]='"' then
            if aCCode[I-1]<>'\' then
              Loc := TLoc.None;

      InLineComment:
        if CharInSet(aCCode[I], [#13, #10]) then
          Loc := None;

      InMultiLineComment:
        if I > 1 then
          if aCCode[I - 1] = '*' then
            if aCCode[I] = '/' then
              loc := None;

    end;


    if Loc=none then
    begin
      if aCCode[I] = c1 then
        inc(level);

      if aCCode[I] = c2 then
        dec(level);
      if level = 0 then
      begin
        // ok, we're back at level 0, so we've found the closing bracket.
        Res := trim(copy(aCCode, 0, 2 + I - length(aCCode)));
        Break;
      end;
    end;
  end;
end;

procedure ScanUntilMatchingChar(c1: Char; c2: Char; const aCCode: string; aMatch: TMatch; var Res: string);overload;
var
  loc:TLoc;
  level: Integer;
  i: Integer;
begin
  level := 0;
  Loc := None;

  I := aMatch.Index + aMatch.Length - 1;
  while I<=aCCode.length do
  begin
    case loc of
      None:
        begin
          if aCCode[I]='''' then
            Loc := InStringQ1;

          if aCCode[I]='"' then
            Loc := InStringQ2;

          if I < aCCode.Length-1 then
            if aCCode[I] = '/' then
              if aCCode[I + 1] = '/' then
                Loc := InLineComment;

          if I < aCCode.Length then
            if aCCode[I] = '/' then
              if aCCode[I + 1] = '*' then
                Loc := InMultiLineComment;

        end;

      InStringQ1:
        if I>1 then
          if aCCode[I]='''' then
            if aCCode[I-1]<>'\' then
              Loc := None;

      InStringQ2:
        if I>1 then
          if aCCode[I]='"' then
            if aCCode[I-1]<>'\' then
              Loc := TLoc.None;

      InLineComment:
        if CharInSet(aCCode[I], [#13, #10]) then
          Loc := None;

      InMultiLineComment:
        if I > 1 then
          if aCCode[I - 1] = '*' then
            if aCCode[I] = '/' then
              loc := None;

    end;


    if Loc=none then
    begin
      if aCCode[I] = c1 then
        inc(level);

      if aCCode[I] = c2 then
        dec(level);
      if level = 0 then
      begin
        // ok, we're back at level 0, so we've found the closing bracket.
        Res := trim(copy(aCCode, aMatch.Index, 2 + I - aMatch.Index));
        Break;
      end;
    end;
    inc(I);

  end;
end;

function RemoveRoutines(const c:string;aOnProgress:TOnProgress=nil):string;
var
  m: TMatch;
  mc:TMatchCollection;
  n: Integer;
  Routine: string;
  rt:TRoutine;
  Newcode:string;
  IsNonFunction:boolean;s:string;
  u:TPascalUnit;
const
  minv  = 0.8;
  scale = 0.4;
begin
  // search for functions by pattern..
  NewCode := c;
  mc := TRegEx.Matches(NewCode, '^(?<indent>\s*)(static\s+)?(inline\s+)?(?<return_type>'+rxId+')\s*(?<classname>'+rxID+'::)?(?<funcname>'+rxID+')\s*\(\s*(?<params>[^\)]*)\s*\)[^{|^;]*\{' ,
                              [roMultiLine]);

  u := TPascalUnit.Create(nil);
  try

    for n := mc.Count-1 downto 0 do
    begin
      m := mc[n];
      // sometimes we accidentially ran into somethign that looks like a function
      if not IsFunction(m.Value.Trim) then
        Continue;

      // we've found a function signature.
      // now let's scan until we've found the final closing bracket
      // there can be nested brackets
      Routine := '';
      ScanUntilMatchingChar('{','}',c,m,Routine);

      if Routine<>'' then
      begin
        if c_FunctionToPas(u ,Routine,rt) then
        begin
          try
            if rt <> nil then
              Delete(NewCode, m.Index, length(Routine));

            if Assigned(aOnProgress) then
              if rt<>nil then
                aOnProgress(minv+scale*n/mc.count,rt.Name);
          finally
//            rt.Free;
          end;
        end;
      end;
    end;
    Result := NewCode;
  finally
    u.Free;
  end;
end;

function RemoveStructs(const c:string):string;
var
  m: TMatch;
  mc:TMatchCollection;
  n: Integer;
  aRoutine: string;
  Newcode:string;
begin
  // search for functions by pattern..
  NewCode := c;
  mc := TRegEx.Matches(NewCode, 'struct\s+(?<packed>PACKED)?\s*(?<name>'+rxId+')[^{]*\{' , [roMultiLine]);
  for n := mc.Count-1 downto 0 do
  begin
    m := mc[n];
    // we've found a function signature.
    // now let's scan until we've found the final closing bracket
    // there can be nested brackets
    aRoutine := '';
    ScanUntilMatchingChar('{','}',c,m,aRoutine);
    if aRoutine<>'' then
      Delete(NewCode, m.Index, length(aRoutine));
  end;
  Result := NewCode;
end;



function convertType(const s:string):string;
begin
  // let's rule out the most common ones first

  if SameText(s,'int'   ) then Exit('integer');
  if SameText(s,'short' ) then Exit('byte');
  if SameText(s,'char'  ) then Exit('byte');
  if SameText(s,'float') then Exit('Single');
  if SameText(s,'double') then Exit('Double');

  if SameText(s,'unsigned char') then Exit('byte');
  if SameText(s,'signed char') then Exit('ShortInt');

  if SameText(s,'signed long') then Exit('int32');
  if SameText(s,'unsigned long') then Exit('uint32');


  if SameText(s,'wchar_t') then Exit('WideChar');

  if SameText(s,'uint'  ) then Exit('cardinal');
  if SameText(s,'signed int'   ) then Exit('int32');
  if SameText(s,'unsigned int'   ) then Exit('uint32');

  if SameText(s,'uint8' ) then Exit('byte');
  if SameText(s,'uint16') then Exit('uint16');
  if SameText(s,'uint32') then Exit('uint32');
  if SameText(s,'uint64') then Exit('uint64');
  if SameText(s,'int32' ) then Exit('integer');
  if SameText(s,'int16' ) then Exit('int16');
  if SameText(s,'int64' ) then Exit('int64');

  if SameText(s,'uint8_t')  then Exit('byte');
  if SameText(s,'uint16_t') then Exit('uint16');
  if SameText(s,'uint32_t') then Exit('uint32');
  if SameText(s,'uint64_t') then Exit('uint64');
  if SameText(s,'int32_t') then Exit('integer');
  if SameText(s,'int16_t') then Exit('int16');
  if SameText(s,'int64_t') then Exit('int64');

  if SameText(s,'long int')    then Exit('LongInt');
  if SameText(s,'long double') then Exit('Extended');
  if SameText(s,'__int64') then Exit('Int64');
  if SameText(s,'void') then Exit('');
  if SameText(s,'void*') then Exit('Pointer');
  if SameText(s,'char*') then Exit('PAnsiChar');
  if SameText(s,'char *') then Exit('PAnsiChar');
  if SameText(s,'wchar_t*') then Exit('PWideChar');
  if SameText(s,'wchar_t *') then Exit('PWideChar');

  if SameText(s,'BOOL') then Exit('Boolean');
  if SameText(s,'UBYTE') then Exit('Byte');
  if SameText(s,'UWORD') then Exit('Word');
  if SameText(s,'ULONG') then Exit('Cardinal');



  Result := esc(s);
end;



procedure setV(var s:string;const m:TMatch;const v:string);
begin
  s := s.Replace('<' + v + '>', m.Groups[v].Value);
end;


function FixTernary(const c:string):string;
var m:TMatch;s:string;
begin
  Result := '';
  // fix ternary if
  for m in TRegEx.Matches(c, '(?<pre>)(?<condition>\w+\s*[\>\<=!\/\*\+\-]\s*\w+)\s*\?\s*(?<if_true>.*):\s*(?<if_false>[^\)])([\)]?)(?<post>)',[ roSingleLine ]) do
  begin
    s := '<pre>ifthen(<condition> , <if_true> , <if_false>)<post>';
    setV(s,m,'pre');
    setV(s,m,'post');
    setV(s,m,'condition');
    setV(s,m,'if_true');
    setV(s,m,'if_false');
    Result := Result + s;
  end;

  if Result='' then
    Result := c;
end;

procedure FindDefines(const Defines:TStringList; const c:string);
var m:TMatch; d,s:string; i:integer; sl:TStringList;
begin
  Defines.Clear;
  sl := TStringList.Create;
  try
    sl.Text := StripComments(c);;
    for i := 0 to sl.Count-1 do
    begin
      s := sl[i].Trim;
      m := TRegEx.Match(s,'^(\s*)\#define\s+(?<def>'+rxID+')\s*$' ,[ roMultiLine ]);
      if m.Success then
      begin
        d := m.Groups['def'].Value;
        Defines.Add( d );
      end;
    end;
  finally
    sl.Free
  end;
end;

type
  TMacro=record
    Identifier:string;
    Parameters,Replacements:TArray<string>;
  end;

function ApplyMacro(const aCCode:string; const aMacro:TMacro):string;
var
  loc:TLoc;
  i,j: Integer;
begin
  if Length(aMacro.Replacements)<1 then
    Exit(aCCode);

  Result := ReplaceOutsideCommentsAndStrings(aCCode,aMacro.Identifier, aMacro.Replacements[0]);
  Exit;

  Loc := None;

  I := 0;
  J := 1;
  while I<=aCCode.length do
  begin
    case loc of
      None:
        begin
          if aCCode[I]='''' then
            Loc := InStringQ1;

          if aCCode[I]='"' then
            Loc := InStringQ2;

          if I < aCCode.Length-1 then
            if aCCode[I] = '/' then
              if aCCode[I + 1] = '/' then
                Loc := InLineComment;

          if I < aCCode.Length then
            if aCCode[I] = '/' then
              if aCCode[I + 1] = '*' then
                Loc := InMultiLineComment;
        end;

      InStringQ1:
        if I>1 then
          if aCCode[I]='''' then
            if aCCode[I-1]<>'\' then
              Loc := None;

      InStringQ2:
        if I>1 then
          if aCCode[I]='"' then
            if aCCode[I-1]<>'\' then
              Loc := None;

      InLineComment:
        if CharInSet(aCCode[I], [#13, #10]) then
          Loc := None;

      InMultiLineComment:
        if I > 1 then
          if aCCode[I - 1] = '*' then
            if aCCode[I] = '/' then
              loc := None;
    end;

    if Loc=none then
    begin
      // increase J if we found a match
      if aCCode[I] = aMacro.Identifier[J+1] then
        inc(J)
      else
        J := 0;

      if J>=aMacro.Identifier.Length then
      begin
        // found identifier!
        //  ReplaceOutsideCommentsAndStrings()
      end;
    end
    else
      j := 0;

    inc(I);
  end;
end;

function c_inlinevardef(const c:string;out pas:string):boolean;
var m: TMatch;
begin
  pas := '';
  Result := false;

  if c='' then
  begin
    pas := '';
    Exit(False)
  end;

  if c.Trim.StartsWith('return') then
    Exit;

  // "int test = 5 * 5 + xxx;"
  // varname : test
  // vartype : int
  // expr    : 5 * 5 + xxx
  m := TRegEx.Match(c,'^(?<indent>\s*)(?<vartype>'+rxType+')\s+(\*)?(?<varname>'+rxID+')\s*=\s*(?<expr>.*)\s*;',[roSingleLine]);
  if m.Success then
  begin
    Result := True;
    pas := '<indent><varname> := <expr>;';
    setV(pas,m,'indent');
    setV(pas,m,'varname');
    setV(pas,m,'vartype');
    setV(pas,m,'expr');
    Exit;
  end;


  // char dest[20]="Hello";
  m := TRegEx.Match(c,'^(?<indent>\s*)char\s+(?<varname>'+rxID+')\s*\[\s*(?<arraysize>[^\]^\s]+)\s*\]\s*=\s*(?<expr>[^;]*)\s*;',[roSingleLine]);
  if m.Success then
  begin
    Result := True;
    pas := '<indent><varname> := <expr>;';
    setV(pas,m,'indent');
    setV(pas,m,'varname');
    setV(pas,m,'expr');
//    if m.Groups['comment'].Value<>'' then
//      pas := pas + ' // '+ m.Groups['comment'].Value;
    Exit;
  end;

  // int var[4] = xx; // foo
  m := TRegEx.Match(c,'^(?<indent>\s*)((?<vartype>\w*)\s+)?(?<varname>'+rxID+')\s*\[\s*(?<arraysize>[^\]^\s]+)\s*\]\s*=\s*(?<expr>[^;]+)\s*;\s*(?<comment>\/\*.*\*\/)?',[roSingleLine]);
  if m.Success then
  begin
    Result := True;
    pas := '<indent><varname>[<arraysize>] := <expr>;';
    setV(pas,m,'indent');
    setV(pas,m,'varname');
    setV(pas,m,'arraysize');
    setV(pas,m,'expr');
//    if m.Groups['comment'].Value<>'' then
//      pas := pas + ' // '+ m.Groups['comment'].Value;
    Exit;
  end;

  // type dest[20]="Hello";
  m := TRegEx.Match(c,'^(?<indent>\s*)((?<vartype>\w*)\s+)?(?<varname>'+rxID+')\s*\[\s*(?<arraysize>[^\]^\s]+)\s*\]\s*=\s*(?<expr>[^;]*)\s*;',[roSingleLine]);
  if m.Success then
  begin
    Result := True;
    pas := '<indent><varname>[<arraysize>] := <expr>;';
    setV(pas,m,'indent');
    setV(pas,m,'varname');
    setV(pas,m,'arraysize');
    setV(pas,m,'expr');
//    if m.Groups['comment'].Value<>'' then
//      pas := pas + ' // '+ m.Groups['comment'].Value;
    Exit;
  end;



  // "int test;"
  // "int *test;"
  // vartype : int
  // varname : test
  m := TRegEx.Match(c,'^(?<indent>\s*)(?<vartype>'+rxType+')\s+[\*]?(?<varname>'+rxID+')\s*;',[roSingleLine]);
  if m.Success then
  begin
    pas := '';
    if m.Groups['vartype'].Value='return' then
      Exit;
    Exit(true);
  end;

  // "int test[4];"
  // vartype   : int
  // varname   : test
  // arraysize : 4
  m := TRegEx.Match(c,'(?<indent>\s*)(?<vartype>'+rxType+')\s+(?<varname>'+rxID+')\s*\[\s*(?<arraysize>[^\]]*)\s*\]\s*;',[roSingleLine]);
  if m.Success then
  begin
    pas := '';
    Exit(true);
  end;

  // const int base = 4344
  m := TRegEx.Match(c, '^\s*(?<const>const)\s+(struct\s+)?(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*=\s*(?<expression>.*)\s*;\s*$');
  if m.Success then
  begin
    pas := '';
    Exit(true);
  end;

  // const int base = 4344
  m := TRegEx.Match(c, '^\s*(?<const>const)\s+(struct\s+)?(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*=\s*(?<expression>.*)\s*;\s*\/\/(?<comment>.*)$');
  if m.Success then
  begin
    pas := '';
    Exit(true);
  end;

  // 	UBYTE a,b,inst,note,eff,dat;
  m := TRegEx.Match(c,'^(?<indent>\s*)(?<vartype>'+rxType+')\s+(?<vars>(('+rxID+')\s*\,\s*)+\s*(?<varname>'+rxID+'))\s*;',[roSingleLine]);
  if m.Success then
  begin
    pas := '';
    Exit(true);
  end;


  // 	int i, test=0;
  // output:
  // test := 0;
  m := TRegEx.Match(c,'^(?<indent>\s*)(?<vartype>'+rxType+')\s+(?<vars>(('+rxID+')\s*\,\s*)+\s*(?<varname>'+rxID+'))\s*(=\s*(?<expr>[^;]*));',[roSingleLine]);
  if m.Success then
  begin
    pas := '<indent><varname> := <expr>;';
    setV(pas,m,'indent');
    setV(pas,m,'varname');
    setV(pas,m,'vartype');
    setV(pas,m,'expr');
    Exit(true);
  end;
end;

procedure DeleteArrayIndex(var X: TArray<string>; Index: Integer);
begin
  if Index > High(X) then Exit;
  if Index < Low(X) then Exit;
  if Index = High(X) then
  begin
    SetLength(X, Length(X) - 1);
    Exit;
  end;
  Finalize(X[Index]);
  System.Move(X[Index +1], X[Index],
  (Length(X) - Index -1) * SizeOf(string) + 1);
  SetLength(X, Length(X) - 1);
end;

procedure FixTypeCasts(var Result: string);
begin
  // remove unnecessary casts:
  // convert float(123)
  //      to 123
  Result := TRegEx.Replace(Result, 'float\s*\(\s*(?<val>' + rxNum + ')\s*\)', '\1');
  // convert float(varname)
  //      to varname
  Result := TRegEx.Replace(Result, 'float\s*\(\s*(?<val>' + rxID + ')\s*\)', '\1');
  // convert float(<expression>)
  //      to (<expression>)
  Result := TRegEx.Replace(Result, 'float\s*\(\s*(?<val>[^)]*)\s*\)', '(\1)');
end;


procedure FixSwitchCaseStatementsSimple(var Result: string);
begin
  // old method, based on search/replacing switch and case stuff..
  // didn't find errors with this approach, but it doesn't finish the job:
  // - it doesn't remove break statements,
  // - it won't enclose multiple statements into code blocks

  // convert case XXX :
  //      to XXX:
  Result := TRegEx.Replace(Result, '^(\s*)case\s*(?<val>[^\:]*)\s*:', '\1\2: ', [roMultiLine]);

  // convert switch( expression )
  //      to case expression do
  Result := TRegEx.Replace(Result, '^(\s*)switch\s*\((?<cond>[^)]*)\s*\)\s*{', '\1case \2 of', [roMultiLine]);

  // convert default: command = 0;
  //      to else command = 0;
  Result := TRegEx.Replace(Result, '^(\s*)default\s*\:', '\1else', [roMultiLine]);
end;

procedure FixCaseStatementsFull(const c:TCode; var aAllCCode: string);
var
  matchSwitch,
  matchCase : TMatch;
  SwitchStatementStr, ex, cond: string;
  cs      : TSwitch;
  cc      : TCase;
begin
  // new method, with more detailed interpretation:
  repeat
//    matchSwitch := TRegEx.Match(aAllCCode, '^(?<indent>\s*)switch\s*\((?<cond>.*)\s*\)\s*\{', [roMultiLine]);
    matchSwitch := TRegEx.Match(aAllCCode, '^(?<indent>\s*)switch\s*\(', [roMultiLine]);

    if not matchSwitch.Success then
      Break;

    cs := TSwitch.Create(c);
    cs.Indent := length(matchSwitch.Groups['indent'].Value.Replace(sLineBreak,''));
//    cs.Switch := matchSwitch.Groups['cond'].Value;
    ScanUntilMatchingChar('(',')', aAllCCode, matchSwitch, cond);
    delete(cond,high(cond),1);
    cs.Switch := cond.Substring(cond.IndexOf('(')+1) ;

    ScanUntilMatchingChar('{', '}', aAllCCode, matchSwitch, SwitchStatementStr);
    SwitchStatementStr := Copy(aAllCCode, matchSwitch.Length+ matchSwitch.index, Pos('}',aAllCCode,matchSwitch.Index) - (matchSwitch.Length+ matchSwitch.index));
    cs.Name := cs.Switch.Trim;

    cs.Sourceinfo.Position := matchSwitch.Index;
    cs.Sourceinfo.Length   := SwitchStatementStr.Length;



    for matchCase in TRegEx.Matches(SwitchStatementStr, '(case\s+(?<case>[_\w\'']+))?(default)?\s*:\s*(?<code>.+?)(?=break|case|default)', [roSingleLine]) do
    begin
      cc := TCase.Create(cs);
      cc.Sourceinfo.Position := cs.Sourceinfo.Position + matchCase.Index;
      cc.Sourceinfo.Length   := matchCase.Value.Length;
      cc.Id := matchCase.Groups['case'].Value;
      if cc.Id = '' then
        cc.Id := 'else';
      cc.Name := cc.Id;
      cc.Code := TCode.Create(cc,matchCase.Groups['code'].Value.Trim.Split([';']) );
      cs.Cases := cs.Cases + [cc];
    end;

    ex := copy(SwitchStatementStr, matchCase.index + matchCase.length, Maxint).Trim;
    if ex.startsWith(';') then ex := ex.Substring(2).trim;
    if ex.startsWith('break;') then ex := ex.Substring(6).trim;
    ex := ex.trim;

    if ex<>'' then
    begin
      matchCase := TRegEx.Match(ex, '(case\s+(?<case>[_\w\'']+))?(default)?\s*:\s*(?<code>[^}]*)', [roSingleLine]);
      if matchCase.Success then
      begin
        cc := TCase.Create(cs);
        cc.Id := matchCase.Groups['case'].Value;
        if cc.Id = '' then
          cc.Id := 'else';
        cc.Name := 'case_'+cc.Id;
        cc.Code := TCode.Create(cc,matchCase.Groups['code'].Value.Trim.Split([';']));
        cs.Cases := cs.Cases + [cc];
      end;
    end;

    cs.Renderinfo.Position := matchSwitch.Index+1;
    Delete(aAllCCode, matchSwitch.Index+1, SwitchStatementStr.Length+4);
    Insert(cs.ToPascal, aAllCCode, matchSwitch.Index);
    cs.Renderinfo.Length   := aAllCCode.Length - cs.Renderinfo.Position-1;

  until not matchSwitch.Success;
end;


procedure ConvertCLinesToPas(const code:TCode);
var m:TMatch; l:string;
  c,I: Integer;
  loop: TLoop;
  s,ps,tmp: string;
  linesAr:TArray<string>;
  expr: string;
  Result:string;
begin
  c := 0;
  // replace lines that contain a variable declaration.
  // when it also contains an assignment, leave the assignment,
  // otherwise remove the line
  setlength(linesAr,code.lines.count);
  for I := 0 to code.lines.Count-1 do
  begin
    if not c_inlinevardef(code.lines[I],ps) then
    begin
      linesAr[c] := code.lines[I];
      inc(c);
    end
    else
    begin
      if ps<>'' then
      begin
        linesAr[c] := ps;
        inc(c);
      end;
    end;
  end;

  // strip emtpy lines at then end
  i := length(linesAr)-1;
  while (i>=0) and (linesAr[i].Trim='') do
  begin
    setlength(linesAr,i);
    dec(i);
  end;

  setlength(linesAr,c);
  code.lines.Clear;
  code.lines.InsertRange(0,linesAr);

  if code.Lines.Count > 0 then
  begin
    l := code.Lines[code.Lines.Count-1];
         code.lines[code.Lines.Count-1] := TRegEx.Replace(l,'^(\s*)return\s*;\s*','\1Exit;',[roSingleLine]) ;
    l := code.Lines[code.Lines.Count-1];
         code.lines[code.Lines.Count-1] := TRegEx.Replace(l,'^(\s*)Exit\s*\((?<expr>[^\)])\)\s*;\s*','\1Result := \2;',[roSingleLine]) ;
    l := code.Lines[code.Lines.Count-1];
         code.lines[code.Lines.Count-1] := TRegEx.Replace(l,'^(\s*)return\s*(?<expr>[^;]+)\s*;\s*','\1Result := \2;',[roSingleLine]) ;
  end;

  for I := 0 to code.Lines.Count-1 do
  begin
      l := code.lines[I];

      l := TRegEx.Replace(l,'^(\s*)return\s*(?<expr>[^;]+)\s*;?$','\1Exit(\2);');

      // fix special operators
      for m in TRegEx.Matches(l, '^(?<indent>\s*)(?<varname>.*)\s*(?<op>[\+\-\*\/\%\&\^\~])\s*\=\s*(?<expr>.*);') do
      begin
        s :='<indent><varname> := <varname> <op> <expr>;';
        setV(s,m,'indent');
        setV(s,m,'varname');
        setV(s,m,'op');
        expr := FixTernary(m.Groups['expr'].Value);
        if not TRegEx.Match(expr,'^[a-zA-Z0-9\-\>\.\$\_]+$').Success then
          expr := '('+expr+')';

        s := s.Replace('<expr>', expr );
        s := s.Replace('^','xor');
        s := s.Replace('  xor',' xor');
        s := s.Replace('%','mod');
        s := s.Replace('&','and');
        s := s.Replace('~','not');
        s := s.Replace('  not',' not');
        s := s.Replace('  and',' and');
        s := s.Replace('  mod',' mod');
        s := s.Replace('  +',' +');
        s := s.Replace('  -',' -');
        s := s.Replace('|','or');
        l := s;
      end;

      // convert : printf("Usage: %s filename\n", argv[0], x, 565)
      // to      : writeln(format('Usage: %s filename',[argv[0], x, 565]))
      l := TRegEx.Replace(l, 'printf\s*\(\s*\"(?<format_str>.*)\\n\"\s*,\s*(?<params>.*)\s*\)\s*;','writeln(format(''\1'',[\2]));');

      // convert : printf("Usage: %s filename\n", argv[0], x, 565)
      // to      : write(format('Usage: %s filename\n',[argv[0], x, 565]))
      l := TRegEx.Replace(l, 'printf\s*\(\s*\"(?<format_str>.*)\"\s*,\s*(?<params>.*)\s*\)\s*;','write(format(''\1'',[\2]));');

      for m in TRegEx.Matches(l,'(?<indent>\s*)printf\s*\(\s*\"(?<format_str>.*(?<cr>\\n)?)\"\s*,\s*(?<params>.*)\s*\)\s*;') do
      begin
        s := '(Format(''<format_str>'',[<params>]));';
        tmp := m.Groups['format_str'].Value.Trim;
        if tmp.EndsWith('\n') then
        begin
          setlength(tmp,length(tmp)-2);
          s := 'Writeln'+s;
        end
        else
          s := 'Write'+s;

        s := '<indent>'+s;
        s := s.Replace('<format_str>', tmp );

        setV(s,m,'indent');
        setV(s,m,'params');
        l := s;
      end;

      // convert parameterless printf statements to write/writeln
      for m in TRegEx.Matches(l,'(?<indent>\s*)printf\s*\(\s*\"(?<format_str>.*(?<cr>\\n)?)\"\s*\)\s*;') do
      begin
        s := '(''<format_str>'');';
        tmp := m.Groups['format_str'].Value.Trim;
        if tmp.EndsWith('\n') then
        begin
          setlength(tmp,length(tmp)-2);
          s := 'Writeln'+s;
        end
        else
          s := 'Write'+s;
        s := s.Replace('<format_str>', tmp );
        s := '<indent>'+s;
        setV(s,m,'indent');
        l := s;
      end;


      for m in TRegEx.Matches(l,'(?<indent>\s*)scanf\(\s*\"(?<format_str>.*(?<cr>\\n)?)\"\s*,\s*(?<params>.*)\s*\)\s*;') do
      begin
        s := '<indent>Readln(<params>);';
        setV(s,m,'indent');
        setV(s,m,'params');
        s := s.Replace('&','');
        l := s;
      end;


      // replace
      // cout << xxx << "cccvbcvbvc" << endl;
      // Write(xxx,'cccvbcvbvc',endl);
      if l.Trim.StartsWith('cout') then
      begin
        s := 'Write(';
        for m in TRegEx.Matches(l, '\<\<\s*(?<str>["]?[^\<]+["]?)',[ roSingleLine ]) do
        begin
          if s<>'Write(' then
            s := s + ',';

          s := s + m.Groups['str'].Value.Replace('"','''');
        end;
        s := s +')';
        s := copy(l,1,pos('cout',l)) + s;
        l := s;
      end;

      if c_inlinevardef(s,ps) then
        s := ps;

      // convert for-loop
      for m in TRegEx.Matches(l, '^(?<indent>\s*)for\s*\(\s*(?<vartype>int(?:\s+))?(?<varname>\w+)\s*\=\s*(?<min>[^\;]*)\s*;\s*(?<varname2>\w+)\s*(?<op><[\=]{0,1})\s*(?<max>.*)\s*\;\s*(?<varname3>\w+\+\+|\w+\-\-)\s*\)\s*(?<code>.*)',[ roSingleLine ]) do
      begin
        loop := TLoop.Create(nil);
        try
          loop.IndexerVar := TVariable.Create(loop, m.Groups['varname'].Value, ConvertType( m.Groups['vartype'].Value ));
          loop.StartVal   := m.Groups['min'].Value;
          loop.EndVal     := m.Groups['max'].Value;
          if loop.StartVal > loop.EndVal then loop.Dir := down else loop.Dir := up;
          if m.Groups['varname3'].Value.EndsWith('--') then
            loop.Dir := down;

          if m.Groups['op'].Value = '<'  then loop.Op := LT;
          if m.Groups['op'].Value = '>'  then loop.Op := GT;
          if m.Groups['op'].Value = '>=' then loop.Op := GT_EQ;
          if m.Groups['op'].Value = '<=' then loop.Op := LT_EQ;
          if m.Groups['op'].Value = '==' then loop.Op := EQ;

          l := m.Groups['indent'].Value
                    + loop.toPascal
                    + ' '
                    + m.Groups['code'].Value ;
        finally
          loop.Free;
        end;
      end;
      code.lines[I] := l;
  end;

  Result := string.join(sLineBreak,Code.lines.ToArray);

  FixTypeCasts(Result);

  FixCaseStatementsFull(code, Result);

  // let's try to deal with crappy C string handling.
  // this is probably where C sucks the most :)
  // convert strcpy( xxxx, 123 );
  //      to xxxx := 123;
  Result := TRegEx.Replace(Result,'(?<indent>\s*)strcpy\s*\(\s*(?<arg1>[^,^(]*)\s*\,\s*(?<arg2>[^)]*)\s*\)\s*\;','\1\2 := \3;',[ roMultiLine ]);

  // convert strcat(xxx,yyy);
  //      to xxx := xxx + yyy;
  Result := TRegEx.Replace(Result,'(?<indent>\s*)strcat\s*\(\s*(?<arg1>[^,^(]*)\s*\,\s*(?<arg2>[^)]*)\s*\)\s*\;','\1\2 := \2 + \3;',[ roMultiLine ]);

  // convert strlen(xxx)
  //      to Length(xxx)
  Result := TRegEx.Replace(Result,'strlen\(\s*(?<index>[^\)]+)\s*\)','Length(\1)',[ roMultiLine ]);


  // Convert argv[1]
  //      to ParamStr(1)
  // we'll make sure that it's not a part of another string for example
  Result := TRegEx.Replace(Result,'(\s*|[^a-z^A-Z^0-9^_]*)argc(\s*|[^a-z^A-Z^0-9^_]*)','\1ParamCount\2',[ roMultiLine ]);

  // Convert argv[1]
  //      to ParamStr(1)
  Result := TRegEx.Replace(Result,'argv\[\s*(?<index>[^\]]+)\s*\]','ParamStr(\1)',[ roMultiLine ]);


  // Convert puts(foo)
  //      to WriteLn(foo)
  Result := TRegEx.Replace(Result,'puts\(\s*(?<index>[^\)]+)\s*\)','WriteLn(\1)',[ roMultiLine ]);


  // replace "end; else" with "end else"
  Result := TRegEx.Replace(Result, 'end;(\w+)else\w+','end\1else');
  // replace i++ with PostInc(i)
  Result := TRegEx.Replace(Result, '(\w+)\+\+','PostInc(\1)');
  // replace ++i with PreInc(i)
  Result := TRegEx.Replace(Result, '\+\+(\w+)','PreInc(\1)');
  // replace i-- with PostDec(i)
  Result := TRegEx.Replace(Result, '(\w+)\-\-','PostDec(\1)');
  // replace --i with PreDec(i)
  Result := TRegEx.Replace(Result, '\-\-(\w+)','PreDec(\1)');
  // replace "hallo" with 'hallo'
  Result := TRegEx.Replace(Result, '\"(.*)\"','''\1''');
  // replace '\0' with #0
  Result := Result.Replace('''\0''','#0');



  // replace (d?:0:1) with (ifthen(d,0,1))
  Result := TRegEx.Replace(Result,'\((?<condition>.*)\s*\?\s*(?<if_true>.*):\s*(?<if_false>.*)\s*\)','( ifthen(\1, \2, \3) )' );


  // return statements that are not on their own line are not converted. Let's fix that:
  Result := TRegEx.Replace(Result,'^(\s*)if\s*\((?<condition>[^\)]+)\s*\)\s*return\s*;','\1if \2 then Exit;',[ roMultiLine ]);
  // return statements that are not on their own line are not converted. Let's fix that:
  Result := TRegEx.Replace(Result,'^(\s*)if\s*\((?<condition>[^\)]+)\s*\)\s*return\s*([^;]*)\s*;','\1if \2 then Exit(\3);',[ roMultiLine ]);


  // convert single line if statement
  // convert if(XXX) YYY;
   //     to if XXX then YYY;
  Result := TRegEx.Replace(Result,'^(?<indent>\s*)if\s*\(\s*(?<expr>[^\)]*)\s*\)\s*(?<then>[^\;]*)\s*\;','\1if \2 then \3;',[ roMultiLine ]);


  // convert if(XXX)
  //      to if XXX then
  Result := TRegEx.Replace(Result,'^(?<indent>\s*)if\s*\(\s*(?<expr>.*)\s*\)(?<trail>\s*)','\1if \2 then \3',[ roMultiLine ]);


  // convert while(XXX)
  //      to while XXX do
  Result := TRegEx.Replace(Result,'^(?<indent>\s*)while\s*\(\s*(?<expr>.*)\s*\)(?<trail>\s*)','\1while \2 do \3',[ roMultiLine ]);

  //  int main(){
  //do {
  //x = 2 * x;
  //y--;
  //}
  //while (x < y);
  //
  //}
  Result := TRegEx.Replace(Result,'^(?<indent>\s*)do\s*\{(?<body>[^\}]*)\}\s*(while)\s*\((?<expr>[^)]*)\s*\)\s*;','\1repeat \2 until not(\4);',[ roMultiLine ]);



  // convert atoi to StrToInt
  Result := TRegEx.Replace(Result,'atoi\s*\(\s*([^\)]*)\s*\)','StrToInt(\1)',[ roMultiLine ]);

  // convert putchar("\n") to Write(sLineBreak)
  Result := TRegEx.Replace(Result,'putchar\s*\(\s*([^\)]*)\s*\)','Write(\1)',[ roMultiLine ]);

  // well.. we've exchausted all the tricks we've got on our sleeve
  // let's just do some simplistic substitutions to convert whatever's left
  // not very accurate, but it works in the majority of the situations
  //Result := Result.Replace(' = ', ' := '    , [rfReplaceAll]);

  Result := Result.Replace('==' , '='    , [rfReplaceAll]);
  Result := Result.Replace('!=' , '<>'   , [rfReplaceAll]);

  // replace ! that's not in a string. Else "Hello, World!" becomes "Hello, World not "
  Result := ReplaceOutsideCommentsAndStrings(Result,'!',' not ');
  Result := Result.Replace('&&' , ' and ', [rfReplaceAll]);
  Result := Result.Replace('||' , ' or ' , [rfReplaceAll]);
  Result := ReplaceOutsideCommentsAndStrings(Result,'^',' xor ');
  Result := Result.Replace('>>' , ' shr ', [rfReplaceAll]);
  Result := Result.Replace('<<' , ' shl ', [rfReplaceAll]);
  Result := Result.Replace('->' , '^.'    , [rfReplaceAll]);
  Result := Result.Replace('::' , '.'    , [rfReplaceAll]);
  Result := ReplaceOutsideCommentsAndStrings(Result,'{','begin ');
  Result := ReplaceOutsideCommentsAndStrings(Result,'}','end; '+sLineBreak);

  Result := Result.Replace('(*','( *'); // (* could appear in C code
  Result := Result.Replace('/*'  , '(*');
  Result := Result.Replace('*/'  , '*)' + sLineBreak);

  Result := TRegEx.Replace(Result,'\s\|\s',' or ',[ roMultiLine ]);
  Result := TRegEx.Replace(Result,'\s\&\s',' and ',[ roMultiLine ]);

  Result := TRegex.Replace(Result,'atan\s*\(' , 'arctan(',[roMultiLine ]);

  // convert conditional defines #ifdef XXX to {$ifdef XXX}
  Result := TRegEx.Replace(Result,'^(\s*)\#ifdef\s+(.*)$','\1{$IFDEF \2}',[  roMultiLine ]);
  Result := TRegEx.Replace(Result,'^(\s*)\#ifndef\s+('+rxID+')\s*$','\1{$IFNDEF \2}',[ roMultiLine ]);
  Result := TRegEx.Replace(Result,'^(\s*)\#if\s+(.*)$'   ,'\1{$IF \2}',[ roMultiLine ]);
  Result := TRegEx.Replace(Result,'^(\s*)\#else\s*$' ,'\1{$ELSE}',[ roMultiLine ]);
  Result := TRegEx.Replace(Result,'^(\s*)\#endif\s*$' ,'\1{$ENDIF}',[ roMultiLine ]);
  Result := TRegEx.Replace(Result,'^(\s*)\#define\s+('+rxID+')\s*$' ,'\1{$DEFINE \2}',[ roMultiLine ]);

  // fprintf with newline and parameters
  // fprintf(stderr, "[*.PAT loader] highnote to high (sh.high_frequency=%d highnote=%d sizeof(ins->note)=%d\n", sh.high_frequency, highnote, (int)sizeof(ins->note));
  // WriteLn(format('[*.PAT loader] highnote to high (sh.high_frequency=%d highnote=%d sizeof(ins->note)=%d',[sh.high_frequency, highnote, (int)sizeof(ins->note)]);
  Result := TRegEx.Replace(Result,'^(?<indent>\s*)fprintf\s*\(\s*(?<output>\w[\w\d_]*),\s*[''"](?<formatstr>[^''"]*)(?<newline>\\n)[''"]\s*,\s*(?<params>.*)\);', '\1WriteLn(Format(''\3'',[\5]));',[ roMultiLine ]);

  // fprintf without newline and parameters
  // fprintf(stderr, "[*.PAT loader] highnote to high (sh.high_frequency=%d highnote=%d sizeof(ins->note)=%d", sh.high_frequency, highnote, (int)sizeof(ins->note));
  // Write(format('[*.PAT loader] highnote to high (sh.high_frequency=%d highnote=%d sizeof(ins->note)=%d',[sh.high_frequency, highnote, (int)sizeof(ins->note)]);
  Result := TRegEx.Replace(Result,'^(?<indent>\s*)fprintf\s*\(\s*(?<output>\w[\w\d_]*),\s*[''"](?<formatstr>[^''"]*)[''"]\s*,\s*(?<params>.*)\);', '\1Write(Format(''\3'',[\5]));',[ roMultiLine ]);

  // fprintf with newline
  // fprintf(stderr, "[*.PAT loader] highnote to high (sh.high_frequency=%d highnote=%d sizeof(ins->note)=%d\n", sh.high_frequency, highnote, (int)sizeof(ins->note));
  // WriteLn(format('[*.PAT loader] highnote to high (sh.high_frequency=%d highnote=%d sizeof(ins->note)=%d',[sh.high_frequency, highnote, (int)sizeof(ins->note)]);
  Result := TRegEx.Replace(Result,'^(?<indent>\s*)fprintf\s*\(\s*(?<output>\w[\w\d_]*),\s*[''"](?<formatstr>[^''"]*)(?<newline>\\n)[''"]\s*\);', '\1WriteLn(''\3'');',[ roMultiLine ]);

  // fprintf without newline
  // fprintf(stderr, "[*.PAT loader] highnote to high (sh.high_frequency=%d highnote=%d sizeof(ins->note)=%d", sh.high_frequency, highnote, (int)sizeof(ins->note));
  // Write(format('[*.PAT loader] highnote to high (sh.high_frequency=%d highnote=%d sizeof(ins->note)=%d',[sh.high_frequency, highnote, (int)sizeof(ins->note)]);
  Result := TRegEx.Replace(Result,'^(?<indent>\s*)fprintf\s*\(\s*(?<output>\w[\w\d_]*),\s*[''"](?<formatstr>[^''"]*)[''"]\s*\);', '\1Write(''\3'');',[ roMultiLine ]);


  // pointers
  // convert (*xxx) to @xxx
  Result := TRegEx.Replace(Result,'\(\*(\s*'+rxID+')\)' ,'@\1',[ roMultiLine ]);

    // convert XXX[534] = 34gfdjeklrtj354;
  //      to XXX[534] := 34gfdjeklrtj354;
  // convert XXX[534].xxx = 34gfdjeklrtj354;
  //      to XXX[534].xxx := 34gfdjeklrtj354;
  Result := TRegEx.Replace(Result,'^(\s*)(\w[\w\[\]_\d\.]*)\s*\=\s*(.*)$' ,'\1\2 := \3',[ roMultiLine ]);

  // convert hex

  Result := TRegEx.Replace(Result,rxHexadecimal ,'\$\1',[ roMultiLine ]);

  // convert null to nil
  Result := TRegEx.Replace(Result,'NULL' ,'nil',[ roMultiLine ]);

  Result := TRegEx.Replace(Result,'(\d+)U','\1',[ roMultiLine ]);
  Result := TRegEx.Replace(Result,'^(\s*)(return);','\1Exit;',[ roMultiLine ]);

  // convert null to nil
  Result := Result.Replace('Writeln('''')','Writeln');
  Result := Result.Replace('(int32_t)','');
  Result := Result.Replace('(int64_t)','');
  Result := Result.Replace('\n''','''+sLineBreak');
  Result := Result.Replace('''''+','');
  Result := Result.Replace(' +''''','');
  Result := Result.Replace('Write(sLineBreak)','WriteLn');

  Result := TRegEx.Replace(Result,'else(\s*);(\s*)' ,'else\1\2',[ roMultiLine ]);
  Result := TRegEx.Replace(Result,';(\s*)else(\s*)' ,'\1else\2',[ roMultiLine ]);



  Result := Result.TrimRight;
  Code.Lines.Clear;
  Code.Lines.InsertRange(0,Result.Split([sLineBreak]));
end;



function GetVariablesFromLine(const Line: string; aVariableList: TVariableList):boolean;
var
  m: TMatch;
  i: Integer;
  s: String;
  lType: string;
  lvarName: string;
begin
  if Line.Trim='' then
    Exit(False);

  // int test4_TT
  m := TRegEx.Match(Line,'^\s*(struct\s+)?(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*;\s*$');
  if m.Success then
  begin
    if m.Groups['vartype'].Value <> 'return' then
    begin
      TVariable.Create(aVariableList, m.Groups['varname'].Value, convertType(m.Groups['vartype'].Value),TDir.inout);
      exit(true);
    end;
  end;

  // int test = 5*5+xxx
  m := TRegEx.Match(Line, '^\s*(struct\s+)?(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*=\s*(?<expression>.*)\s*;\s*$');
  if m.Success then
  begin
    if m.Groups['vartype'].Value <> 'return' then
    begin
      TVariable.Create(aVariableList, m.Groups['varname'].Value, convertType(m.Groups['vartype'].Value),TDir.inout);
      exit(true);
    end;
  end;

  // const int base = 508887777
  m := TRegEx.Match(Line, '^\s*(?<const>const)\s+(struct\s+)?(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*=\s*(?<expression>.*)\s*;\s*(?<comment>.*)\s*;\s*$');
  if m.Success then
  begin
    TVariable.Create( aVariableList,  m.Groups['varname'].Value, convertType(m.Groups['vartype'].Value), TDir.in, False, True, m.Groups['expression'].Value, m.Groups['comment'].Value.Trim.TrimLeft(['/']).Trim);
    exit(true);
  end;

  // int test;
  // int *test;
  // int **test;
  m := TRegEx.Match(Line, '^(?<indent>\s*)(struct\s+)?(?<vartype>' + rxType + ')\s+(?<pointer>[\*]{0,2})?(?<varname>' + rxID + ')\s*;');
  if m.Success then
  begin
    if m.Groups['vartype'].Value <> 'return' then
    begin
      TVariable.Create(aVariableList, m.Groups['varname'].Value,
        m.Groups['pointer'].Value.Replace('*','^') + ConvertType(m.Groups['vartype'].Value),
        TDir.inout);
      exit(true);
    end;
 end;


  // catch loop variable (for int i=0;i<10;i++)
  m := TRegEx.Match(Line, '^(?<indent>\s*)for\s*\(\s*(?<vartype>int(?:\s+))(?<varname>' + rxId + ')\s*\=\s*(?<min>[^\;])\s*;\s*(?<varname2>' + rxID + ')\s*(?<op><[\=]{0,1})\s*(?<max>.*)\s*\;\s*(?<varname3>\w+\+\+)\s*\)\s*(.*)', [roSingleLine]);
  if m.Success then
  begin
    TVariable.Create(aVariableList, m.Groups['varname'].Value, convertType(m.Groups['vartype'].Value.Trim),TDir.inout);
    exit(true);
  end;

  // int xxx[17][18]
  m := TRegEx.Match(Line, '^\s*(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*\[\s*(?<arraysize1>.*)\s*\]\s*\[\s*(?<arraysize2>.*)\s*\]');
  if m.Success then
  begin
    if TryStrToInt(m.Groups['arraysize1'].Value, i) then
    if TryStrToInt(m.Groups['arraysize2'].Value, i) then
      TVariable.Create(aVariableList, m.Groups['varname'].Value, format('array[0..%d,0..%d] of %s', [
        StrToInt(m.Groups['arraysize1'].Value) - 1,
        StrToInt(m.Groups['arraysize2'].Value) - 1,
        convertType(m.Groups['vartype'].Value)]),TDir.inout);
    exit(true);
  end;


  // int xxx[4]
  m := TRegEx.Match(Line, '^\s*(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*\[\s*(?<arraysize>.*)\s*\]\s*');
  if m.Success then
  begin
    if m.Groups['arraysize'].Value='' then
      // int test[]
      TVariable.Create(aVariableList, m.Groups['varname'].Value, format('array of %s', [convertType(m.Groups['vartype'].Value)]),TDir.inout)
    else
      if TryStrToInt(m.Groups['arraysize'].Value, i) then
        // int test[4]
        TVariable.Create(aVariableList, m.Groups['varname'].Value, format('array[0..%d] of %s', [StrToInt(m.Groups['arraysize'].Value) - 1, convertType(m.Groups['vartype'].Value)]),TDir.inout)
      else
        // char name[NAME_MAX+1]
        TVariable.Create(aVariableList, m.Groups['varname'].Value, format('array[0..(%s)-1] of %s', [m.Groups['arraysize'].Value, convertType(m.Groups['vartype'].Value)]),TDir.inout);
    exit(true);
  end;

  if line.Contains('void') then
    Exit(False);

  m := TRegEx.Match(Line,'^\s*(?<vartype>'+rxType+')\s+'
                        +'(?<vars>(('+rxID+')\s*'
                        +'(\[\s*(?<arraysize1>\d)\s*\])?\s*'
                        +'(\[\s*(?<arraysize2>\d)\s*\])?\s*'
                        +'\,\s*)+\s*('+rxID+'))\s*(=\s*[^;]*)?;',[roSingleLine]);
  if m.Success then
  begin
    if m.Groups['vartype'].Value<>'return' then
      for s in m.Groups['vars'].Value.Split([',']) do
      begin
        lType := convertType(m.Groups['vartype'].Value );
        lvarName := s.Split(['['])[0];

        if m.Groups['arraysize1'].Value <> '' then
        begin
          if m.Groups['arraysize2'].Value <> '' then
            lType := Format('Array[0..%d,0..%d] of %s',[
              StrToInt(m.Groups['arraysize1'].Value),
              StrToInt(m.Groups['arraysize2'].Value),
              lType
            ])
          else
            if m.Groups['arraysize1'].Value <> '' then
              lType := Format('Array[0..%d] of %s',[
                StrToInt(m.Groups['arraysize1'].Value),
                lType
              ])
        end;

        TVariable.Create(aVariableList,  lVarName, lType,TDir.inout) ;
      end;

    exit(true);
  end;

  // static const int lg_n = 6;
  m := TRegEx.Match(Line, '^\s*(?<static>static)\s+(?<const>const)\s+(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*\=\s*(?<value>.*)\;$');
  if m.Success then
  begin
    TVariable.Create(aVariableList, m.Groups['varname'].Value, convertType(m.Groups['vartype'].Value), TDir.&in, true, true, m.Groups['value'].Value );
    exit(true);
  end;

  Exit(False);
end;


procedure getMethodParams(const s: String; var params:TVariableList);
var
  p     : string;
  m     : TMatch;
  t,dir:string;
  d:TDir;
begin
  // parse the routine arguments
  for p in s.Split([',']) do
    for m in TRegEx.Matches(p, '\s*(?<direction>in\s+|out\s+|inout\s+|const\s+)?(?<type>'+rxType+')\s+(?<pointer>[\*\&]?)(?<varname>'+rxID+')\s*') do
    begin
      dir := m.Groups['direction'].Value.Trim;
      if dir='inout' then d := TDir.inout else
      if Dir='out'   then d := TDir.out   else
      if Dir='in'    then d := TDir.in    else
      if Dir='const' then d := TDir.in    else
                          d := TDir.none;

      if m.Groups['pointer'].Value='*' then
        d := inout;

      t := ConvertType( m.Groups['type'].Value );
      if t.EndsWith('*') then
      begin
        d := inout;
        t := t.TrimRight(['*']);
      end;

      TVariable.Create(
        params,
        m.Groups['varname'].Value,
        t,
        d,
        False,
        False);

    end;
end;

function c_Array1DToPas(const u: TPascalElement; c:string;out pas:TArrayDef1D):Boolean;
var
  m:TMatch;
  i:integer;ns:string;

begin
  Result := false;
  pas := TArrayDef1D.Create(u);

  for m in TRegEx.Matches(c,'\s*(?<eltype>'+rxType+')\s+(?<varname>'+rxID+')\s*\[(?<arraysize>.*)?\]\s*=\s*\{(?<elements>.*)\};',[roIgnoreCase, roSingleLine] ) do
  begin
    Result := True;
    pas.itemType := convertType(m.Groups['eltype'].Value);
    pas.rangeMin := '0';
//    pas.rangeMax := StrToIntDef(m.Groups['arraysize'].Value,-1);
    pas.Name     := m.Groups['varname'].Value+ns;
    pas.Items    := m.Groups['elements'].Value.Replace(' ','').Replace(#9,'').Replace(sLineBreak,'').Split([',']);

    // if the items look like a string, let's just make it an array of string instead of CHAR* or something crappy like that.
    for I := Low(pas.Items) to high(pas.Items) do
      if Length(pas.Items[I])>3 then // for something to be enclosed in quotes, we need at least a length of 2.. more than 3 because we don't want to convert chars to string
        if pas.Items[0].Contains('"') then
          pas.itemType := 'String';

    for I := Low(pas.Items) to high(pas.Items) do
    begin
      pas.Items[I] := trim(pas.Items[I].Replace('"',''''));
    end;
    pas.rangeMax := IntToStr(length(pas.Items)-1);
    Exit;
  end;
end;


function c_Array2DToPas(const u: TPascalElement; c:string;out pas:TArrayDef2D):Boolean;
var
  m,n:TMatch;
  I,J:integer;
  SubItems:string;
begin
  Result := false;
  for m in TRegEx.Matches(c,'\s*(?<modifier>\w+)*\s+(?<eltype>'+rxType+')\s+(?<varname>'+rxID+')\s*\[(?<arraysize1>.*)?\]\[(?<arraysize2>.*)?\]\s*=\s*\{(?<elements>.*)\};',[roIgnoreCase, roSingleLine] ) do
  begin
    pas := TArrayDef2D.Create(u);
    Result := True;
    pas.itemType := ConvertType(m.Groups['eltype'].Value);
    pas.ranges[0].rangeMin := '0';
    pas.ranges[1].rangeMin := '0';
    pas.ranges[0].rangeMax := m.Groups['arraysize1'].Value;
    pas.ranges[1].rangeMax := m.Groups['arraysize2'].Value;
    pas.Name     := m.Groups['varname'].Value;
    J := 0;
    for n in TRegEx.Matches(m.Groups['elements'].Value,'\{(?<el>[^\}]*)}') do
    begin
      SubItems := n.Groups['el'].Value.Replace(' ','').Replace(#9,'').Replace(sLineBreak,'');
      setlength(Pas.Items,J+1);
      Pas.Items[J] := subItems.Split([',']);

      for I := Low(pas.Items[J]) to high(pas.Items[J]) do
        pas.Items[J][I] := trim(pas.Items[J][I].Replace('"',''''));

      // pas.ranges[1].rangeMax := IntToStr(length(pas.Items[J])-1);

      inc(J);
    end;
    Exit;
  end;
end;






procedure getLocalVars(const aCode:TCode; aVarList:TVariableList);
var l:string; vis:TVisibility; i:integer;
begin
  vis := TVisibility.DefaultVisibility;
  for l in aCode.Lines do
  begin
    if l.Trim = 'public:' then
      vis := TVisibility.&Public;

    if l.Trim = 'private:' then
      vis := TVisibility.&Private;

//    va := [];
    GetVariablesFromLine(l, aVarList);
    for I := 0 to aVarList.Count-1 do
      TVariable(aVarList[I]).Visibility := vis;
  end;
end;

procedure CleanComments(var comment: string);
var
  a: TArray<string>;
  s: string;
  t: string;
begin
  if Comment <> '' then
  begin
    Comment := comment.Trim.Replace('//','');
    a := [];
    for s in Comment.Split([sLineBreak]) do
    begin
      t := s.Trim;
      if t.StartsWith('/*') then
        t := t.Substring(3);
      if t.EndsWith('*/') then
        setlength(t, t.Length - 2);
      if t.Trim <> '' then
        a := a + [t];
    end;
    Comment := string.Join(sLineBreak, a);
  end;
end;


function c_FunctionDefToPas(const c:string;var aRoutine:TRoutine):boolean;
var
  m: TMatch;
  ReturnType, FuncName, Parameters,
  ClassName: string;
  rt:TRoutineType;
  params,
  localvars:TVariableList;
  code:TCode;
  isStatic,isInline,isDestructor, isVirtual:boolean;
  s,comment:string;
begin
  Assert(not Assigned(aRoutine));

  try
    m := TRegEx.Match(c.Trim, rxMethodDef , [roSingleLine] );
  except
    on e:Exception do
      Exit(false);
  end;

  if not m.Success then
    Exit(False);


  ReturnType   := m.Groups['returntype'].Value.Trim;

  if not IsFunction(c.Trim) then
    Exit(False);

  ReturnType   := convertType( ReturnType );

  if ReturnType.EndsWith('*') then
    ReturnType := '^'+ReturnType.TrimRight(['*']);


  ClassName    := m.Groups['classname'].Value;
  FuncName     := m.Groups['funcname'].Value;
  try
    Parameters   := m.Groups['parameters'].Value;
  except
    Parameters := '';
  end;

  isDestructor := m.Groups['destructor'].Value='~';

  if SameText(ClassName,FuncName) then
  begin
    if isDestructor then
    begin
      rt := TRoutineType.&destructor;
      FuncName := 'Destroy';
    end
    else
    begin
      rt := TRoutineType.&constructor;
      FuncName := 'Create';
    end;
  end
  else
    if SameText(ReturnType,'void') then
      rt := TRoutineType.&procedure
    else
      rt := TRoutineType.&function;

  Params := TVariableList.Create(nil);
  Params.Name := 'Params';
  getMethodParams(Parameters,Params);
  code := TCode.Create(nil, c.Split([';']) );

  localvars := TVariableList.Create(nil);
  localvars.Name := 'LocalVars';
  getLocalVars(Code, localvars);

  isInline  := SameText(m.Groups['inline'].Value.Trim,'inline');
  isStatic  := SameText(m.Groups['static'].Value.Trim,'static');
  isVirtual := SameText(m.Groups['virtual'].Value.Trim,'virtual');

  Comment := m.Groups['comment'].value
           + m.Groups['comment3'].value;
  CleanComments(comment);

  aRoutine := TRoutine.Create(
    nil,
    FuncName,
    ClassName,
    rt,
    ReturnType,
    params,
    localvars,
    code,
    false,
    false,
    isInline,
    isStatic,
    isVirtual,
    comment
  );

  ConvertCLinesToPas(code);

  exit(True);
end;




function c_FunctionToPas(const aUnit:TPascalUnit; const c:string;var aRoutine:TRoutine):boolean;
var
  m: TMatch;
  ReturnType, FuncName, ParameterStr,
  CodeStr: string;
  ClassName: string;
  RoutineType:TRoutineType;
  params,
  localvars:TVariableList;
  code:TCode;
  isStatic,isInline,isDestructor, isVirtual:boolean;
  comment:string;
  classDef:TClassDef;
begin
  try
    m := TRegEx.Match(c.Trim, rxMethod , [roSingleLine] );
  except
    on e:Exception do
      Exit(false);
  end;

  if not m.Success then
    Exit(False);

  ReturnType   := m.Groups['returntype'].Value.Trim;

  if ReturnType = 'typedef' then Exit(False);
  if ReturnType = 'enum'    then Exit(False);
  if ReturnType = 'struct'  then Exit(False);
  if ReturnType = 'class'   then Exit(False);

  ReturnType   := convertType( ReturnType );

  if ReturnType.EndsWith('*') then
    ReturnType := '^'+ReturnType.TrimRight(['*']);


  ClassName    := m.Groups['classname'].Value;
  FuncName     := m.Groups['funcname'].Value;
  ParameterStr := m.Groups['parameters'].Value;
  CodeStr      := m.Groups['code'].Value;

  isDestructor := m.Groups['destructor'].Value='~';

  if SameText(ClassName,FuncName) then
  begin
    if isDestructor then
    begin
      RoutineType := TRoutineType.&destructor;
      FuncName := 'Destroy';
    end
    else
    begin
      RoutineType := TRoutineType.&constructor;
      FuncName := 'Create';
    end;
  end
  else
    if SameText(ReturnType,'void') then
      RoutineType := TRoutineType.&procedure
    else
      RoutineType := TRoutineType.&function;

  Code := TCode.Create(nil,CodeStr.Split([sLineBreak]));
  Code.Sourceinfo.Position := m.Groups['code'].Index;

  params := TVariableList.Create(nil);
  params.Name := 'Params';
  getMethodParams(ParameterStr,params);
  localvars := TVariableList.Create(nil);
  localvars.Name := 'Local vars';

  getLocalVars(Code, localvars);

  isInline  := SameText(m.Groups['inline'].Value.Trim,'inline');
  isStatic  := SameText(m.Groups['static'].Value.Trim,'static');
  isVirtual := SameText(m.Groups['virtual'].Value.Trim,'virtual');

  Comment := m.Groups['comment'].value
           + m.Groups['comment3'].value;
  CleanComments(comment);

  if ClassName = '' then
    ClassName := 'TGlobal';

  classDef := aUnit.getClassByName(ClassName);

  aRoutine := TRoutine.Create(
    classDef,
    FuncName,
    ClassName,
    RoutineType,
    ReturnType,
    params,
    localvars,
    code,
    false,
    false,
    isInline,
    isStatic,
    isVirtual,
    comment
  );

  ConvertCLinesToPas(code);

  exit(True);
end;



function c_StructToPas(const aPascalUnit:TPascalUnit; c:string;var outClass:TClassDef):Boolean;
var
  m:TMatch;
  Name:string;
  Code:TCode;
  v:TVariableList;
begin
  Result := false;
  Code := TCode.Create(nil,[]);
  try
    for m in TRegEx.Matches(c,'struct\s+(?<packed>PACKED)?\s*(?<name>'+rxId+')[^{]*\{',[roMultiLine] ) do
    begin
      Result := True;
      Name := m.Groups['name'].Value;
      Code.Lines.Clear;
      Code.Lines.InsertRange(0,c.Split([sLineBreak]));
      v := TVariableList.Create(nil);
      v.Name := 'Fields';
      getLocalVars(Code,v);
      if v.Count = 0 then
      begin
        v.Free;
        exit(false);
      end;
      outClass := TClassDef.Create(aPascalUnit,Name,v,TClassKind.&record);
      outClass.FIsPacked := m.Groups['packed'].Value='PACKED';
      Exit(True);
    end;
  finally
    Code.Free;
  end;
end;




function c_return(const c:string;out pas:string):boolean;
var m: TMatch;
begin
  pas := '';
  Result := false;
  for m in TRegEx.Matches(c,'\s*return\s+(?<expression>.*)') do
  begin
    Result := True;
    Pas := 'exit( <expression> );';
    setV(Pas,m,'expression');
  end;
end;






procedure AddArrays1D(const u: TPascalUnit; const c: string);
var
  m: TMatch;
  ar: TArrayDef1D;
  I: Integer;
  level: Integer; aRoutine: string;
begin
  // search for array definitions
  for m in TRegEx.Matches(c, '(?<modifier>\w+\s+)*\s*(?<eltype>'+rxType+')\s+(?<varname>'+rxID+')\s*\[\s*(?<arraysize>[^\]]*)?\s*\]\s*=\s*\{', [roIgnoreCase]) do
  begin
    level := 0;
    for I := m.Index + m.Length - 1 to c.Length do
    begin
      if c[I] = '{' then inc(level);
      if c[I] = '}' then dec(level);
      // ok, we're back at level 0, so we've found the closing bracket.
      if level = 0 then
      begin
        aRoutine := trim(copy(c, m.Index, 2 + I - m.Index));
        if c_Array1DToPas(u, aRoutine, ar) then
          u.GlobalArrays1D := u.GlobalArrays1D + [ar];

        ar.Sourceinfo.Position := m.Index;
        ar.Sourceinfo.Length   := I - m.Index + 1;

        Break;
      end;
    end;
  end;
end;

procedure AddArrays2D(var u: TPascalUnit; const c: string);
var
  m: TMatch;
  ar: TArrayDef2D;
  I: Integer;
  level: Integer; aRoutine: string;
begin
  // search for array definitions
  for m in TRegEx.Matches(c, '(?<modifier>\w+\s)*\s*(?<eltype>'+rxType+')\s+(?<varname>'+rxID+')\s*\[\s*(?<arraysize1>[^\]]*)?\s*\]\[\s*(?<arraysize2>[^\]]*)?\s*\]\s*=\s*\{', []) do
  begin
    level := 0;
    for I := m.Index + m.Length - 1 to c.Length do
    begin
      if c[I] = '{' then inc(level);
      if c[I] = '}' then dec(level);
      // ok, we're back at level 0, so we've found the closing bracket.
      if level = 0 then
      begin
        aRoutine := trim(copy(c, m.Index, 2 + I - m.Index));
        ar := TArrayDef2D.Create(u);
        if c_Array2DToPas(u, aRoutine, ar) then
          u.GlobalArrays2D := u.GlobalArrays2D + [ar];

        ar.Sourceinfo.Position := m.Index;
        ar.Sourceinfo.Length   := I - m.Index + 1;

        Break;
      end;
    end;
  end;
end;


function c_class_to_pas(u:TPascalUnit; c:string; out pas:TArray<TClassDef>):Boolean;
var  mc:TMatchCollection; m,m2:TMatch; classDef:TArray<string>;i,j:integer;
  vis:TVisibility;
  cd:string;
  rt:TRoutine;
  def:TClassDef;
begin
  Result := True;

  mc := TRegEx.Matches(c, rxClassDef , [roMultiLine]);
  if mc.Count = 0 then
    Exit(False);

  for m in mc do
  begin
    def := TClassDef.Create(u,m.Groups['classname'].Value,nil,TClassKind.&class);
    pas := pas + [def];

    def.FParentType := '';
    cd := c.Substring( m.Index + m.Length, MaxInt).Trim;
    cd := cd.TrimRight(['}']);
    classDef       := cd{m.Groups['classdef'].Value}.Split([sLineBreak]);

    vis := TVisibility.DefaultVisibility;
    for i := low(classDef) to High(classDef) do
    begin
      if classDef[I].Trim = 'public:' then
      begin
        vis := TVisibility.&Public;
        Continue;
      end;
      if classDef[I].Trim = 'private:' then
      begin
        vis := TVisibility.&private;
        Continue;
      end;

      GetVariablesFromLine(classDef[i], def.FMembers);

      m2 := TRegEx.Match(classDef[i], rxMethodDef);
      if m2.Success then
      begin
        rt := nil;
        if c_FunctionDefToPas(classDef[i], rt) then
        begin
          if not def.AddRoutine(rt) then
            rt.Free;
        end;
      end;

      for J := 0 to def.FMembers.Count-1 do
        TVariable(def.FMembers[J]).Visibility := vis;
    end;
    u.AddClass(def);
  end;
end;

procedure AddClassDefs(var u: TPascalUnit; const aCCode: string;var t:string);
var
  m: TMatch;
  aRoutine: string;
  cl: TArray<TClassDef>;
  mc: TMatchCollection;
begin
//            class Dx7Note {
//             public:
//              Dx7Note();
//              void init(const char patch[156], int midinote, int velocity);
//
//              // Note: this _adds_ to the buffer. Interesting question whether it's
//              void compute(int32_t *buf, int32_t lfo_val, int32_t lfo_delay,
//                const Controllers *ctrls);
//
//              void keyup();
//
//              // PG:add the update
//              void update(const char patch[156], int midinote);
//              void peekVoiceStatus(VoiceStatus &status);
//              void transferSignal(Dx7Note &src);
//              void oscSync();
//
//             private:
//              Env env_[6];
//              FmOpParams params_[6];
//              PitchEnv pitchenv_;
//              int32_t basepitch_[6];
//
//              int pitchmodsens_;
//            };

// class xxx : public TForm

  mc := TRegEx.Matches(aCCode, rxClassDef , [roMultiLine]);
  for m in mc do
  begin
    // we've found a class signature.
    // now let's scan until we've found the final closing bracket
    // there can be nested brackets
    ScanUntilMatchingChar('{','}',aCCode,m,aRoutine);
    if aRoutine='' then
      Continue;
    aRoutine := trim(copy(aCCode, m.Index, length(aRoutine)));


    if c_class_to_pas(u,aRoutine,cl) then
    begin
//      cl.Sourceinfo.Position := m.Index;
//      cl.Sourceinfo.Length   := aRoutine.Length;
    end;
  end;

end;


procedure AddFunctions(const aPascalUnit: TPascalUnit; const aCCode: string; var t:string;aOnProgress:TOnProgress=nil);
var
  m: TMatch;
  aRoutine,c: string;
  rt: TRoutine;
  cl: TClassDef;
  mc: TMatchCollection;
  I,Index:integer;
  s: string;
  J: Integer;
const
  minv  = 0.3;
  scale = 0.4;
begin
  // search for functions by pattern..
  rt := nil;

  mc := TRegEx.Matches(aCCode, rxMethodHeader , [ roMultiLine ]);
  for m in mc do
  begin
    // sometimes we accidentially ran into somethign that looks like a function
    if not IsFunction(m.Value.Trim) then
      Continue;

    aRoutine := '';
    ScanUntilMatchingChar('{', '}', aCCode, m, aRoutine);


    //////////////////////////////
    ///  search backwards for multi-line comment
    I := m.Index;
    c := '';
    while I>0 do
    begin
      case aCCode[I] of
        #9,#13,#10,' ':
          begin
            dec(I);
            Continue;
          end;
        '/':
          // maybe found end of a multiline comment
          begin
            Dec(I);
            if aCCode[I]='*' then
            begin
              // found end of multiline comment
              while I>1 do
              begin
                if (aCCode[I]='*') and (aCCode[I-1]='/') then
                  I := 0
                else
                  aRoutine := aCCode[I] + c;


                Dec(I);

              end;
            end;
          end;
        else break;
      end;
      dec(I);
    end;
    //////////////////////////////


    if aRoutine<>'' then
    begin
      if c_FunctionToPas(aPascalUnit,aRoutine,rt) then
      begin
        cl := aPascalUnit.getClassByName(rt.ClassName);

        rt.Sourceinfo.Position := m.Index;
        rt.Sourceinfo.Length   := aRoutine.Length;

        rt.code.Sourceinfo.Position := rt.Sourceinfo.Position;
        if rt.Code.Count > 0 then
        for Index := 0 to rt.code.Count-1 do
        begin
          if rt.code[Index] is TSwitch then
          begin
            rt.code[Index].Sourceinfo.Position := rt.code[Index].Sourceinfo.Position + rt.Sourceinfo.Position;
            for J := 0 to rt.Code[Index].Count-1 do
            begin
              rt.Code[Index][J].Sourceinfo.Position := rt.Code[Index][J].Sourceinfo.Position + rt.Code.Sourceinfo.Position;
            end;
          end;
        end;

        if rt.Comment = '' then
          rt.Comment := c;

        if assigned(aOnprogress) then
          aOnProgress(minv+scale*I / mc.count,rt.ToDeclarationPascal);

        if not cl.AddRoutine(rt) then
          rt.Free;


      end;
      rt := nil;
    end;
  end;
//  aCCode := RemoveRoutines(aCCode);
end;



procedure AddConsts(var u: TPascalUnit; const c: string; var t:string);
var
  m: TMatch;
  val:string;
  WithoutRoutines:string;
  code:TCode;
  LType:String;
const
  rx = '^\s*#define\s+(?<name>'+rxID+')\s+(?<expr>.*)\s*$';
  rx3 = '^\s*const\s+(?<type>'+rxType+')\s+(?<name>'+rxID+')\s*\=\s*(?<expr>.*)\s*\;\s*$'; // const int kControllerPitchStep = 130;
  rx4 = '^\s*const\s+(?<type>'+rxType+')\s+(?<name>'+rxID+')\s*\[\s*\]\s*\=\s*(?<expr>.*)\s*\;\s*$'; // const char filename[] = "XYZ.DAT";
begin
  // search for const definitions
  for m in TRegEx.Matches(c, rx, [roMultiLine]) do
  begin
    val := m.Groups['expr'].Value;
    val := val
            .Replace('<<',' shl ')
            .Replace('>>',' shr ')
            .Replace('  ',' ')
            .Replace('"','''')
            ;

    LType := '';
    if val.StartsWith('''') then
      if val.Length=3 then
        LType := 'Char'
      else
        LType := 'String';


    TVariable.Create(u.GlobalVars, m.Groups['name'].Value,LType, TDir.&in, true, true, val);
  end;
  t := TRegEx.Replace(t,rx,PARSED_MARKER_STR,[roMultiLine] );


  for m in TRegEx.Matches(c, rx3, [roMultiLine]) do
  begin
    val   := m.Groups['expr'].Value;
    LType := convertType(m.Groups['type'].Value);
    if val.StartsWith('''') then
      if val.Length=3 then
        LType := 'Char'
      else
        LType := 'String';

    TVariable.Create(u.GlobalVars, m.Groups['name'].Value,lType, TDir.&inout, true, true, val);
  end;
  t := TRegEx.Replace(t,rx3,PARSED_MARKER_STR,[roMultiLine] );

  for m in TRegEx.Matches(c, rx4, [roMultiLine]) do
  begin
    val := m.Groups['expr'].Value;
    val := val.Replace('"','''');
    LType := convertType(m.Groups['type'].Value);
    if val.StartsWith('''') then
        LType := 'Char';

    LType := 'TArray<'+LType+'>';
    if LType='TArray<Char>' then
      LType := 'String';

    TVariable.Create(u.GlobalVars, m.Groups['name'].Value, LType, TDir.&inout, true, true, val);
  end;
  t := TRegEx.Replace(t,rx3,PARSED_MARKER_STR,[roMultiLine] );



  WithoutRoutines := RemoveRoutines(c);
  Code := TCode.Create(nil, WithoutRoutines.Split([sLineBreak]));
  try
//  vars := TVariableList.Create;
//  vars.Name := 'vars';
    getLocalVars( Code, u.GlobalVars );
  finally
    code.Free;
  end;

end;



procedure AddStructs(var u: TPascalUnit; const c: string; aOnProgress:TOnProgress);
var
  m: TMatch;
  mc:TMatchCollection;
  struct: TClassDef;
  StructStr,name: string;
  i: Integer;
const
  minv  = 0.75;
  scale = 0.1;
begin
  // search for struct definitions
  mc := TRegEx.Matches(c, 'struct\s+(?<packed>PACKED)?\s*(?<name>'+rxID+')[^{^)]*\{', [roMultiLine]);
  i := 0;
  for m in mc do
  begin
    name := m.Groups['name'].Value;
    if name.StartsWith('*') then
      Continue;


    if assigned(aOnprogress) then
      aOnProgress(minV+scale*i/mc.count,'Struct:'+Name);

    StructStr := '';
    ScanUntilMatchingChar('{','}',c,m,StructStr);
    if StructStr='' then
      Continue;
    if c_StructToPas(u,StructStr, struct) then
    begin
      struct.Sourceinfo.Position := m.Index;
      struct.Sourceinfo.Length := StructStr.Length;
      u.AddClass(struct);
    end;
    inc(i);
  end;
end;

procedure ReplaceOutsideCommentsOrStrings(var v: string;cFrom,cTo:char);
var
  inBlockComment: Boolean;
  inLineComment: Boolean;
  ix: Integer;
begin
  inBlockComment := false;
  inLineComment := false;
  if length(v) > 2 then
    for Ix := 1 to length(v) - 1 do
    begin
      if (v[Ix] = '/') and (v[Ix + 1] = '/') then inLineComment  := True;
      if CharInSet(v[iX],[#10,#13])          then inLineComment  := False;
      if (v[Ix] = '/') and (v[Ix + 1] = '*') then inBlockComment := True;
      if (v[Ix] = '*') and (v[Ix + 1] = '/') then inBlockComment := False;

      if (not inLineComment) and (not inBlockComment) then
        if SameText(copy(v,ix,length(cFrom)) , cFrom) then
          v[Ix] := cTo;
    end;
end;


procedure ReplaceInBlockComments(var v: string;cFrom,cTo:char);
var
  inComment: Boolean;
  ix: Integer;
begin
  inComment := false;
  if length(v) > 2 then
    for Ix := 1 to length(v) - 1 do
    begin
      if inComment then
        if v[Ix] = cFrom then
          v[Ix] := cTo
        else if (v[Ix] = '*') and (v[Ix + 1] = '/') then
          inComment := False;
      if (v[Ix] = '/') and (v[Ix + 1] = '*') then
        inComment := True;
    end;
end;

procedure ReplaceInLineComments(var v: string;cFrom,cTo:char);
var
  inComment: Boolean;
  ix: Integer;
begin
  inComment := false;
  if length(v) > 2 then
    for Ix := 1 to length(v) - 1 do
    begin
      if inComment then
        if v[Ix] = cFrom then
          v[Ix] := cTo
        else if (v[Ix] = #13) or (v[Ix] = #10) then
          inComment := False;
      if (v[Ix] = '/') and (v[Ix + 1] = '/') then
        inComment := True;
    end;
end;

procedure ReplaceInComments(var v: string;cFrom,cTo:char);
begin
  ReplaceInBlockComments(v,cFrom,cTo);
  ReplaceInLineComments (v,cFrom,cTo);
end;


procedure AddEnums(var u: TPascalUnit; const c: string);
var
  m,cm: TMatch; e:TEnumDef; Item:TEnumItem;
  v,s: string; n:integer;
begin
  // search for enum definitions
  for m in TRegEx.Matches(c, '(?<typedef>typedef\s+)?enum\s*(?<name>'+rxType+')?\s*{(?<values>[^\}]*)}\s*(?<typedefname>'+rxType+')?', [roMultiLine]) do
  begin
    e := TEnumDef.Create(u);
    if m.Groups['typedef'].Value<>'' then
      e.Name := m.Groups['typedefname'].Value
    else
      e.Name := m.Groups['name'].Value;

    e.SourceInfo.Position := m.Index;
    e.SourceInfo.Length := m.Length;
    n := 0;
    v := m.Groups['values'].Value;

    ReplaceInComments(v,',','.');

    for s in v.Split([',']) do
    begin
      Item := Default(TEnumItem);
      Item.Index := n;
      if length(s.Split(['=']))>1 then
      begin
        Item.Value := StrToIntDef(s.Split(['='])[1],n);
        Item.Name  := s.Split(['='])[0].Trim.Replace(sLineBreak,' ');
        n := Item.Value;
      end
      else
      begin
        Item.Value := n;
        Item.Name  := s.Trim;
      end;
      Item.Name  := TRegEx.Replace(Item.Name,'/\*(?<comment>.*)?\*/','',[  ]).Trim;
      cm := TRegEx.Match(s,'/\*(?<comment>.*)?\*/');
      if cm.Success then
        Item.Comment := cm.Groups['comment'].value;

      if Item.Name.Trim<>'' then
        e.Items := e.Items + [ Item ];
      inc(n);
    end;
    u.Enums := u.Enums + [ e ];
  end;
end;

procedure AddUnits(var Result: TPascalUnit; const aCCode: string; var t:string);
var
  m: TMatch;
  u: string;
const
  rx = '^\s*#include\s*(?<inc>.*)\s*$';
begin
  for m in TRegEx.Matches(aCCode, rx, [roMultiLine]) do
  begin
    u := m.Groups['inc'].Value;
    u := TRegEx.Replace(u, '\/\/(.*)$', '{ \1 }');
    u := TRegEx.Replace(u, '\/\*(.*)\*\/\s*', '');
    u := u.Trim.Replace('/', '.')
               .Replace('<', '')
               .Replace('>', '')
               .Replace('"', '')
               .Replace('..', '')
               ;
    if u.StartsWith('.') then
      u := u.TrimLeft(['.']);

    if ExtractFileExt(u) = '.h' then
      u := ChangeFileExt(u, '');

    if u='stdio' then Continue;
    if u='stdlib' then Continue;
    if u='string' then Continue;

    Result.usesListIntf.AddUnit(u);
  end;

  t:=TRegEx.Replace(t,rx, PARSED_MARKER_STR,[roMultiLine]);
end;


procedure FixTypes(var s: string);
var m:TMatch;indent,t1,t2,v:string;
begin
  s := s.Replace('std::string',
                 '     string');

  for m in tregex.Matches(s, '^(?<indent>\s*)(std::)?vector\<\s*(?<elType>'+rxType+')\s*\>\s*(?<val>'+rxID+')', [  roMultiLine ])  do
  begin
    indent := m.Groups['indent'].Value;
    t1 := m.Groups['elType'].Value;
    v := m.Groups['val'].Value;
    t2 := convertType(t1);
    if t1<>t2 then
    begin
      delete(s,m.Index, m.Length);
//      t2 := 'TArray<'+t2+'>';
      t2 := indent+t1+' '+v+'[]';
      Insert(t2, s, m.Index);
    end;
  end;
  s := s.Replace('std::vector',
                 '     TArray');
  s := s.Replace('(*','( *');
end;

procedure ApplyMacros(var s: string);
var u : TPascalUnit;t,fn:string; ul:TPascalElement; i:integer;
begin
  // include header files
  u := TPascalUnit.Create(nil);
  try
    AddUnits(u, s, t);
    for i := 0 to u.usesListIntf.Count-1 do
    begin
      ul := u.usesListIntf[i];
      fn := ul.Name;
      if not TFile.Exists(fn) then
        fn := ChangeFileExt(fn,'.h');
      if not TFile.Exists(fn) then
        fn := ChangeFileExt(fn,'.hpp');
      if TFile.Exists(fn) then
      s := s.Replace('#include '+ fn,TFile.ReadAllText(fn) )
    end;

    s := s.Replace('__DATE__',FormatDateTime('yyyy-mm-dd',now));
    s := s.Replace('__TIME__',FormatDateTime('hh:nn:ss',now));
    s := s.Replace('__FILE__',u.Name);
    s := s.Replace('__LINE__','0'  );


  finally
    u.Free
  end;

end;


procedure c_to_pas(const aCCode:string; var t:string; aName:string;aOnProgress:TOnProgress; var Result:TPascalUnit);
var
  s:string;
  macro:TMacro;
begin
  if not Assigned(Result) then
    Exit;

  aOnProgress(0,'Converting');

  FindDefines(result.Defines,aCCode);

  Result.Name := aName;
  Result.usesListIntf.&Unit := Result;
  Result.usesListIntf.Name := 'Intf';

  Result.usesListImpl.&Unit := Result;
  Result.usesListImpl.Name := 'Impl';

  s := aCCode;
  ApplyMacros(s);

  //  s :=  ClearComments(s);
  //  s := TRegEx.Replace(s,',\s*\r\n',', '); // when there's a comma at the end of the line, remove the linebreak

  FixTypes(s);
  s := FixComments(s);
  t := s;

  macro.Identifier := 'EXPORTCALL';
  macro.Replacements := ['__declspec(dllexport) __stdcall'];
  ApplyMacro(s,macro);
  aOnProgress(0.05,'Finding units...');
  AddUnits(Result, s, t);
//  aOnProgress(0.10,'Finding classes...');
//  AddClassDefs(Result,s,t);
  aOnProgress(0.20,'Finding routines...');

  AddFunctions(Result, aCCode, t,aOnProgress);
  aOnProgress(0.60,'Finding enums...');
  AddEnums(Result, s);
  aOnProgress(0.65,'Finding 1D arrays...');
  AddArrays1D(Result, s);
  aOnProgress(0.70,'Finding 2D arrays...');
  AddArrays2D(Result, s);
  aOnProgress(0.75,'Finding structs...');
  AddStructs(Result, s, aOnProgress);
  // to find globally defined consts/variables,
  // we first remove all detected routines and structs
  aOnProgress(0.83,'Removing routines...');
  s := RemoveRoutines(s, aOnProgress);
  aOnProgress(0.84,'Removing structs...');
  s := RemoveStructs(s);
  aOnProgress(0.95,'Finding global declarations...');
  AddConsts(Result, s, t);
  aOnProgress(1.00,'Done.');

  result.SetDefaultVisible;
end;


end.
