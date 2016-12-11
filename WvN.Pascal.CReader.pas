unit WvN.Pascal.CReader;

interface

uses
  WvN.Pascal.Model;

  function c_FunctionToPas(const c:string;var r:TRoutine):boolean;
  function c_to_pas(const aCCode:string; var t:string;aName:string='tmp'):TPascalUnit;
  function c_to_pas_str(const c:string; var t:string):string;

implementation

uses
  WvN.Log,
  SysUtils,
  System.RegularExpressions,
  Classes;

const
  PARSED_MARKER = 'X';
  PARSED_MARKER_STR = PARSED_MARKER+PARSED_MARKER+PARSED_MARKER;

  rxID     = '(\*?)[a-zA-Z_\$][\w_]*(\*?)';
  rxT = '(\*?)(?:unsigned\s+)?(?:long\s+)?[a-zA-Z_\$][\w_]*(\*?)';
//  rxType   = '('+rxT+')|('+rxT+'<\s*'+rxT+'\s*>)';
  rxType   = rxT;
//  rxNum    = '\d*';
//  rxNum    = '(?:[^a-zA-Z])[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?';
  rxNum    = '(?:[^a-zA-Z])[\+\-]?[0-9]*\.?[0-9]+(?:[eE][-+]?[0-9]+)?';




  rxMethod =  '^'+ // only do this trick from the start.. otherwise we might accidently match somewhere else
             '(?<comment>(?:\/\*).*(?:\*\/))?'+ // preserve comments
             '(?<indent>\s*)'                 + // to preseve indents of original code
             '(?<static>(static|Static|local|LOCAL))?'             + // text 'static' or 'local'
             '\s*'+
             '(?<inline>inline)?'             + // text 'inline'
             '\s*'+
             '((?<returntype>'+rxID+')\s+)'   +
             // some special modifiers.. not sure how to convert this to Delphi though, so let's leave those for now
             // '((?<pascal>__pascal)\s+)?'+
             // '((?<far>far)\s+)?'+
             '((?<classname>'+rxID+')\:\:)?'+ // support classname::method() kind of signature
             '(?<funcname>'+rxID+')\s*'+      // the function name itself
             '('+ // parameters are not always provided, so the whole parameter section including the brackets is optional
             '\(\s*(?<parameters>.*?)\s*\)'+
             ')?'+
             '\s*'+
             '\{(?<code>.*)?\}';

type
  TLoc=(None,InStringQ1,InStringQ2,InLineComment,InMultiLineComment);

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


procedure ScanUntilMatchingChar(c1: Char; c2: Char; const aCCode: string; m: TMatch; var r: string);
var
  loc:TLoc;
  level: Integer;
  i: Integer;
begin
  level := 0;
  Loc := None;

  I := m.Index + m.Length - 1;
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
        r := trim(copy(aCCode, m.Index, 2 + I - m.Index));
        Break;
      end;
    end;
    inc(I);

  end;
end;

function RemoveRoutines(const c:string):string;
var
  m: TMatch;
  mc:TMatchCollection;
  n: Integer;
  r: string;
  rt:TRoutine;
  Newcode:string;
begin
  // search for functions by pattern..
  NewCode := c;
  mc := TRegEx.Matches(NewCode, '^(?<indent>\s*)(static\s+)?(inline\s+)?(?<return_type>'+rxId+')\s*(?<classname>'+rxID+'::)?(?<funcname>'+rxID+')\s*\(\s*(?<params>[^\)]*)\s*\)[^{|^;]*\{' ,
                              [roMultiLine]);

  for n := mc.Count-1 downto 0 do
  begin
    m := mc[n];
    // sometimes we accidentially ran into somethign that looks like a function
    if m.Value.Trim.StartsWith('if'  )   then continue;
    if m.Value.Trim.StartsWith('for' )   then continue;
    if m.Value.Trim.StartsWith('else')   then continue;
    if m.Value.Trim.StartsWith('while')  then Continue;
    if m.Value.Trim.StartsWith('switch') then Continue;
    if m.Value.Trim.StartsWith('for')    then Continue;

    // we've found a function signature.
    // now let's scan until we've found the final closing bracket
    // there can be nested brackets
    r := '';
    ScanUntilMatchingChar('{','}',c,m,r);
    if r<>'' then
      if c_FunctionToPas(r,rt) then
        if rt <> nil then
          Delete(NewCode, m.Index, length(r));
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
  if SameText(s,'void') then Exit('nil');
  if SameText(s,'void*') then Exit('Pointer');
  if SameText(s,'char*') then Exit('PAnsiChar');
  if SameText(s,'wchar_t*') then Exit('PWideChar');

  if SameText(s,'BOOL') then Exit('Boolean');
  if SameText(s,'UBYTE') then Exit('Byte');
  if SameText(s,'UWORD') then Exit('Word');
  if SameText(s,'ULONG') then Exit('Cardinal');



  Result := esc(s);
end;



procedure setV(var s:string;const m:TMatch;const v:string);
begin
  s := s.Replace('<'+v+'>',m.Groups[v].Value);
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
  m := TRegEx.Match(c,'^(?<indent>\s*)(?<vartype>'+rxType+')\s+(?<varname>'+rxID+')\s*=\s*(?<expr>.*)\s*;',[roSingleLine]);
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


//                                   (?<varname>\w*)     \s*\[\s*(?<arraysize>[^\]]+)\s*\]\s*=\s*(?<expr>[^;]*)\s*;\s*(?<comment>\/\*.*\*\/)?
  m := TRegEx.Match(c,'^(?<indent>\s*)((?<vartype>\w*)\s+)?(?<varname>'+rxID+')\s*\[\s*(?<arraysize>[^\]^\s]+)\s*\]\s*=\s*(?<expr>[^;]*)\s*;\s*(?<comment>\/\*.*\*\/)?',[roSingleLine]);
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
  m := TRegEx.Match(c,'(?<indent>\s*)(?<vartype>'+rxType+')\s+[\*]?(?<varname>'+rxID+')\s*;',[roSingleLine]);
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
  m := TRegEx.Match(c,'(?<indent>\s*)(?<vartype>'+rxType+')\s+(?<varname>'+rxID+')\s*\[\s*(?<arraysize>[^\]]+)\s*\]\s*;',[roSingleLine]);
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



  // int test = 5*5+xxx
//  if TRegEx.Match(c, '^\s*(struct\s+)?(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*=\s*(?<expression>.*)\s*$').Success then pas := ''; Exit(True);
  // const int base = 508887777
//  if TRegEx.Match(c, '^\s*(?<const>const)\s+(struct\s+)?(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*=\s*(?<expression>.*)\s*;\s*(?<comment>.*)\s*$').Success then pas := ''; Exit(True);
  // int test;  // int *test;  // int **test
//  if TRegEx.Match(c, '^(?<indent>\s*)(struct\s+)?(?<vartype>' + rxType + ')\s+(?<pointer>[\*]{0,2})?(?<varname>' + rxID + ')\s*;').Success then pas := ''; Exit(True);
//  if TRegEx.Match(c, '^\s*(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*\[\s*(?<arraysize>.*)\s*\]\s*=\s*()').Success then pas := ''; Exit(True);
//  if TRegEx.Match(c,'^\s*(?<vartype>'+rxType+')\s+(?<vars>(('+rxID+')\s*\,?)+)\s*;',[roSingleLine]).Success then pas := ''; Exit(True);
  // static const int lg_n = 6;
//  if TRegEx.Match(c, '^\s*(?<static>static)\s+(?<const>const)\s+(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*\=\s*(?<value>.*)\;$').Success then pas := ''; Exit(True);

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


function ConvertCLinesToPas(var lines:TArray<string>):string;
var m:TMatch; l:string;
  c,I,t: Integer;
  v: string;
  loop: TLoop;
  s,ps,tmp: string;
  linesAr:TArray<string>;
  expr: string;
begin
  c := 0;
  setlength(linesAr,length(lines));
  for I := 0 to high(lines) do
  begin
    if not c_inlinevardef(lines[I],ps) then
    begin
      linesAr[c] := lines[I];
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
  setlength(linesAr,c);
  lines := linesAr;

  if Length(Lines)>0 then
  begin
      l := Lines[high(Lines)];
      lines[high(Lines)] := TRegEx.Replace(l,'^(\s*)Exit\s*\((?<expr>.*)\)\s*[;]?\s*;?$','\1Result := \2;') ;
      l := Lines[high(Lines)];
      lines[high(Lines)] := TRegEx.Replace(l,'^(\s*)return\s*(?<expr>[^;]+)\s*;?$','\1Result := \2;') ;
  end;

  for I := 0 to high(lines) do
  begin
      l := lines[I];

      l := TRegEx.Replace(l,'^(\s*)return\s*(?<expr>[^;]+)\s*;?$','\1Exit(\2);');

      // fix special operators
      for m in TRegEx.Matches(l, '^(?<indent>\s*)(?<varname>.*)\s*(?<op>[\+|\-|\*|\/\%\^])\s*\=\s*(?<expr>.*);') do
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
        s := s.Replace('  mod',' mod');
        s := s.Replace('  +',' +');
        s := s.Replace('  -',' -');
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
        s := s.Replace('@','');
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
      for m in TRegEx.Matches(l, '^(?<indent>\s*)for\s*\(\s*(?<vartype>int(?:\s+))?(?<varname>\w+)\s*\=\s*(?<min>[^\;]*)\s*;\s*(?<varname2>\w+)\s*(?<op><[\=]{0,1})\s*(?<max>.*)\s*\;\s*(?<varname3>\w+\+\+)\s*\)\s*(?<code>.*)',[ roSingleLine ]) do
      begin
        loop.IndexerVar := TVariable.Create(m.Groups['varname'].Value, ConvertType( m.Groups['vartype'].Value ));
        loop.StartVal   := m.Groups['min'].Value;
        loop.EndVal     := m.Groups['max'].Value;
        if loop.StartVal > loop.EndVal then loop.Dir := down else loop.Dir := up;
        if m.Groups['op'].Value = '<'  then loop.Op := LT;
        if m.Groups['op'].Value = '>'  then loop.Op := GT;
        if m.Groups['op'].Value = '>=' then loop.Op := GT_EQ;
        if m.Groups['op'].Value = '<=' then loop.Op := LT_EQ;
        if m.Groups['op'].Value = '==' then loop.Op := EQ;

        l := m.Groups['indent'].Value
                  + loop.toPascal
                  + ' '
                  + m.Groups['code'].Value ;

      end;
      lines[I] := l;
  end;

  Result := string.join(sLineBreak,lines);


  // remove unnecessary casts:
  // convert float(123)
  //      to 123
  Result := TRegEx.Replace(Result, 'float\s*\(\s*(?<val>'+rxNum+')\s*\)' ,'\1');
  // convert float(varname)
  //      to varname
  Result := TRegEx.Replace(Result, 'float\s*\(\s*(?<val>'+rxID+')\s*\)' ,'\1');
  // convert float(<expression>)
  //      to (<expression>)
  Result := TRegEx.Replace(Result, 'float\s*\(\s*(?<val>[^)]*)\s*\)' ,'(\1)');



  // convert case XXX :
  //      to XXX:
  Result := TRegEx.Replace(Result,'^(\s*)case\s*(?<val>[^\:]*)\s*:','\1\2: ',[ roMultiLine ]);

  // convert switch( expression )
  //      to case expression do
  Result := TRegEx.Replace(Result,'^(\s*)switch\s*\((?<cond>[^)]*)\s*\)\s*{','\1case \2 of',[ roMultiLine ]);

  // convert default: command = 0;
  //      to else command = 0;
  Result := TRegEx.Replace(Result,'^(\s*)default\s*\:','\1else',[ roMultiLine ]);

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



  // return statements that are not on their own line are not converted. Let's fix that:
  Result := TRegEx.Replace(Result,'^(\s*)if\s*\((?<condition>.*)\s*\)\s*return\s*([^;]+)\s*;','\1if \2 then Exit(\3);',[ roMultiLine ]);


  // convert single line if statement
  // convert if(XXX) YYY;
   //     to if XXX then YYY;
  Result := TRegEx.Replace(Result,'^(?<indent>\s*)if\s*\(\s*(?<expr>[^\)]*)\s*\)\s*(?<then>[^\;]+)\s*\;','\1if \2 then \3;',[ roMultiLine ]);


  // convert if(XXX)
  //      to if XXX then
  Result := TRegEx.Replace(Result,'^(?<indent>\s*)if\s*\(\s*(?<expr>.*)\s*\)(?<trail>\s*)','\1if \2 then \3',[ roMultiLine ]);

  // convert while(XXX)
  //      to while XXX do
  Result := TRegEx.Replace(Result,'^(?<indent>\s*)while\s*\(\s*(?<expr>.*)\s*\)(?<trail>\s*)','\1while \2 do \3',[ roMultiLine ]);

  // convert atoi to StrToInt
  Result := TRegEx.Replace(Result,'atoi\s*\(\s*([^\)]*)\s*\)','StrToInt(\1)',[ roMultiLine ]);

  // well.. we've exchausted all the tricks we've got on our sleeve
  // let's just do some simplistic substitutions to convert whatever's left
  // not very accurate, but it works in the majority of the situations
//Result := Result.Replace(' = ', ' := '    , [rfReplaceAll]);

  Result := Result.Replace('==' , '='    , [rfReplaceAll]);
  Result := Result.Replace('!=' , '<>'   , [rfReplaceAll]);

  // replace ! that's not in a string. Else "Hello, World!" becomes "Hello, World not "
//  Result := TRegEx.Replace(Result,'(\!)(?=(?:[^"'']|"[^"'']*")*$)',' not ',[roSingleLine]);
//  Result := Result.Replace('!' , ' not ', [rfReplaceAll]);

  Result := ReplaceOutsideCommentsAndStrings(Result,'!',' not ');
  Result := Result.Replace('&&' , ' and ', [rfReplaceAll]);
//  Result := Result.Replace(' & ', ' and ', [rfReplaceAll]);
  Result := Result.Replace('||' , ' or ' , [rfReplaceAll]);
//Result := Result.Replace(' | ', ' or ' , [rfReplaceAll]);
  Result := ReplaceOutsideCommentsAndStrings(Result,'^',' xor ');
  Result := Result.Replace('>>' , ' shr ', [rfReplaceAll]);
  Result := Result.Replace('<<' , ' shl ', [rfReplaceAll]);
  Result := Result.Replace('->' , '.'    , [rfReplaceAll]);
  Result := Result.Replace('::' , '.'    , [rfReplaceAll]);
  Result := ReplaceOutsideCommentsAndStrings(Result,'{','begin ');
  Result := ReplaceOutsideCommentsAndStrings(Result,'}','end; '+sLineBreak);
  Result := Result.Replace('/*'  , '{');
  Result := Result.Replace('*/'  , '}' + sLineBreak);

  Result := TRegEx.Replace(Result,'\s\|\s',' or ',[ roMultiLine ]);
  Result := TRegEx.Replace(Result,'\s\&\s',' and ',[ roMultiLine ]);

  Result := Result.Replace('atan' , 'arctan');
//Result := Result.Replace('log'  , 'ln' + sLineBreak);


  // convert conditional defines #ifdef XXX to {$ifdef XXX}
  Result := TRegEx.Replace(Result,'^(\s*)\#ifdef\s+(.*)$','\1{$IFDEF \2}',[ roMultiLine ]);
  Result := TRegEx.Replace(Result,'^(\s*)\#ifndef\s+('+rxID+')\s*$','\1{$IFNDEF \2}',[ roMultiLine ]);
  Result := TRegEx.Replace(Result,'^(\s*)\#if\s+(.*)$'   ,'\1{$IF \2}',[ roMultiLine ]);
  Result := TRegEx.Replace(Result,'^(\s*)\#else\s*(.*)$' ,'\1{$ELSE \2}',[ roMultiLine ]);
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

  // convert &variable to @variable
//  Result := TRegEx.Replace(Result,'&('+rxID+')','\1\^',[ roMultiLine ]);

  // convert XXX[534] = 34gfdjeklrtj354;
  //      to XXX[534] := 34gfdjeklrtj354;
  // convert XXX[534].xxx = 34gfdjeklrtj354;
  //      to XXX[534].xxx := 34gfdjeklrtj354;
  Result := TRegEx.Replace(Result,'^(\s*)(\w[\w\[\]_\d\.]*)\s*\=\s*(.*)$' ,'\1\2 := \3',[ roMultiLine ]);

  // convert hex
  Result := TRegEx.Replace(Result,'0[xX]([0-9a-fA-F]+)' ,'\$\1',[ roMultiLine ]);

  // convert null to nil
  Result := TRegEx.Replace(Result,'NULL' ,'nil',[ roMultiLine ]);

  Result := TRegEx.Replace(Result,'(\d+)U','\1',[ roMultiLine ]);

  // convert null to nil
  Result := Result.Replace('Writeln('''')','Writeln');
  Result := Result.Replace('(*','( *');
  Result := Result.Replace('(int32_t)','');
  Result := Result.Replace('(int64_t)','');



  Result := Result.TrimRight;
end;



function GetVariablesFromLine(var v: TArray<TVariable>; const Line: string):boolean;
var
  m: TMatch;
  i: Integer;
  s: String;
begin
  if Line.Trim='' then
    Exit(False);


  // int test4_TT
  m := TRegEx.Match(Line,'^\s*(struct\s+)?(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*$');
  if m.Success then
  begin
    if m.Groups['vartype'].Value <> 'return' then
    begin
      v := v + [TVariable.Create(m.Groups['varname'].Value, convertType(m.Groups['vartype'].Value),TDir.inout)];
      exit(true);
    end;
  end;

  // int test = 5*5+xxx
  m := TRegEx.Match(Line, '^\s*(struct\s+)?(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*=\s*(?<expression>.*)\s*$');
  if m.Success then
  begin
    if m.Groups['vartype'].Value <> 'return' then
    begin
      v := v + [TVariable.Create(m.Groups['varname'].Value, convertType(m.Groups['vartype'].Value),TDir.inout)];
      exit(true);
    end;
  end;

  // const int base = 508887777
  m := TRegEx.Match(Line, '^\s*(?<const>const)\s+(struct\s+)?(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*=\s*(?<expression>.*)\s*;\s*(?<comment>.*)\s*$');
  if m.Success then
  begin
    v := v + [TVariable.Create(m.Groups['varname'].Value, convertType(m.Groups['vartype'].Value),TDir.inout)];
    v[0].Dir := &in;
    v[0].HasValue := True;
    v[0].Value   := m.Groups['expression'].Value;
    v[0].Comment := m.Groups['comment'].Value.Trim.TrimLeft(['/']).Trim;

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
      v := v + [TVariable.Create(m.Groups['varname'].Value, ConvertType(m.Groups['vartype'].Value),TDir.inout)];
      v[0].&Type :=  m.Groups['pointer'].Value.Replace('*','^') + v[0].&Type;
      exit(true);
    end;
 end;


  // catch loop variable (for int i=0;i<10;i++)
  m := TRegEx.Match(Line, '^(?<indent>\s*)for\s*\(\s*(?<vartype>int(?:\s+))(?<varname>' + rxId + ')\s*\=\s*(?<min>[^\;])\s*;\s*(?<varname2>' + rxID + ')\s*(?<op><[\=]{0,1})\s*(?<max>.*)\s*\;\s*(?<varname3>\w+\+\+)\s*\)\s*(.*)', [roSingleLine]);
  if m.Success then
  begin
    v := v + [TVariable.Create(m.Groups['varname'].Value, convertType(m.Groups['vartype'].Value.Trim),TDir.inout)];
    exit(true);
  end;


//  // catch loop variable (for int i=0;i<10;++i)
//  m := TRegEx.Match(Line, '^(?<indent>\s*)for\s*\(\s*(?<vartype>int(?:\s+))?(?<varname>\w+)\s*\=\s*(?<min>' + rxID + ')\s*;\s*(?<varname2>' + rxID + ')\s*(?<op><[\=]{0,1})\s*(?<max>.*)\s*\;\s*\+\+(?<varname3>\w+)\s*\)\s*(.*)', [roSingleLine]);
//  if m.Success then
//  begin
//    v := TVariable.Create(m.Groups['varname'].Value, convertType(m.Groups['vartype'].Value));
//    exit(true);
//  end;

  m := TRegEx.Match(Line, '^\s*(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*\[\s*(?<arraysize>.*)\s*\]\s*');
  if m.Success then
  begin
    if TryStrToInt(m.Groups['arraysize'].Value, i) then
      // int test[4]
      v := v + [TVariable.Create(m.Groups['varname'].Value, format('array[0..%d] of %s', [StrToInt(m.Groups['arraysize'].Value) - 1, convertType(m.Groups['vartype'].Value)]),TDir.inout)]
    else
      // char name[NAME_MAX+1];
      v := v + [TVariable.Create(m.Groups['varname'].Value, format('array[0..(%s)-1] of %s', [m.Groups['arraysize'].Value, convertType(m.Groups['vartype'].Value)]),TDir.inout)];
    exit(true);
  end;

  if line.Contains('void') then
    Exit(False);


  // int i,test, x, z;
//m := TRegEx.Match(Line,'^\s*(?<vartype>'+rxType+')\s+(?<vars>(('+rxID+')\s*\,?)+)\s*;',[roSingleLine]);
  m := TRegEx.Match(Line,'^\s*(?<vartype>'+rxType+')\s+(?<vars>(('+rxID+')\s*\,\s*)+\s*('+rxID+'))\s*(=\s*[^;]*)?;',[roSingleLine]);
  if m.Success then
  begin
    if m.Groups['vartype'].Value<>'return' then
      for s in m.Groups['vars'].Value.Split([',']) do
        v := v + [TVariable.Create( s.Trim, convertType(m.Groups['vartype'].Value ),TDir.inout) ];

    exit(true);
  end;

  // static const int lg_n = 6;
  m := TRegEx.Match(Line, '^\s*(?<static>static)\s+(?<const>const)\s+(?<vartype>' + rxType + ')\s+(?<varname>' + rxID + ')\s*\=\s*(?<value>.*)\;$');
  if m.Success then
  begin
    v := v + [TVariable.Create(m.Groups['varname'].Value, convertType(m.Groups['vartype'].Value), TDir.&in, true, true, m.Groups['value'].Value )];
    exit(true);
  end;

  Exit(False);
end;


function ParseMethodParams(const s: String): TVariableList;
var
  p     : string;
  m     : TMatch;
  dir:string;
  param : TVariable;
  params: TVariableList;
begin
  // parse the routine arguments
  for p in s.Split([',']) do
//  for m in TRegEx.Matches(p, '\s*(?<direction>(?:struct\s+|in\s+|out\s+|inout\s+|const\s+)?)(?<type>'+rxType+')\s+(?<pointer>[\*\&]?)(?<varname>'+rxID+')\s*') do
    for m in TRegEx.Matches(p, '\s*(?<direction>in\s+|out\s+|inout\s+|const\s+)?(?<type>'+rxType+')\s+(?<pointer>[\*\&]?)(?<varname>'+rxID+')\s*') do
    begin
      param       := TVariable.Create;
      param.name  := m.Groups['varname'].Value;
      param.&Type := ConvertType( m.Groups['type'].Value );
      param.Dir   := TDir.none;
      dir := m.Groups['direction'].Value.Trim;

      if dir='inout' then param.Dir := TDir.inout else
      if Dir='out'   then param.Dir := TDir.out   else
      if Dir='in'    then param.Dir := TDir.in    else
      if Dir='const' then param.Dir := TDir.in    ;

      if m.Groups['pointer'].Value='*' then
        param.Dir := inout;

      if param.&Type.EndsWith('*') then
      begin
        param.Dir := inout;
        param.&Type := param.&Type.TrimRight(['*']);
      end;


      params.Add(param);
      // Result := Result + format('%s%s %s:%s',[ ifthen(result='','',';'), param.dir, param.name,param.&type ]);
    end;
  Result := params;
end;

function c_Array1DToPas(c:string;out pas:TArrayDef1D):Boolean;
var
  m:TMatch;
  i:integer;
begin
  Result := false;
  pas := TArrayDef1D.Create;
  for m in TRegEx.Matches(c,'\s*(?<modifier>\w+)*\s+(?<eltype>'+rxType+')\s+(?<varname>'+rxID+')\s*\[(?<arraysize>.*)?\]\s*=\s*\{(?<elements>.*)\};',[roIgnoreCase, roSingleLine] ) do
  begin
    Result := True;
    pas.itemType := convertType(m.Groups['eltype'].Value);
    pas.rangeMin := '0';
//    pas.rangeMax := StrToIntDef(m.Groups['arraysize'].Value,-1);
    pas.name     := m.Groups['varname'].Value;
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


function c_Array2DToPas(c:string;out pas:TArrayDef2D):Boolean;
var
  m,n:TMatch;
  I,J:integer;
  SubItems:string;
begin
  Result := false;
  for m in TRegEx.Matches(c,'\s*(?<modifier>\w+)*\s+(?<eltype>'+rxType+')\s+(?<varname>'+rxID+')\s*\[(?<arraysize1>.*)?\]\[(?<arraysize2>.*)?\]\s*=\s*\{(?<elements>.*)\};',[roIgnoreCase, roSingleLine] ) do
  begin
    pas := TArrayDef2D.Create;
    Result := True;
    pas.itemType := ConvertType(m.Groups['eltype'].Value);
    pas.ranges[0].rangeMin := '0';
    pas.ranges[1].rangeMin := '0';
    pas.ranges[0].rangeMax := m.Groups['arraysize1'].Value;
    pas.ranges[1].rangeMax := m.Groups['arraysize2'].Value;
    pas.name     := m.Groups['varname'].Value;
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






function parseLocalVars(const c:TCode):TVariableList;
var l:string; va:TArray<TVariable>;v:TVariable; vis:TVisibility;
begin
  Result := TVariableList.CreateEmpty;
  vis := TVisibility.DefaultVisibility;
  va := [];
  for l in c.Lines do
  begin
    if l.Trim = 'public:' then
      vis := TVisibility.&Public;

    if l.Trim = 'private:' then
      vis := TVisibility.&Private;

    va := [];
    GetVariablesFromLine(va, l);
    for v in va do
    begin
      v.Visibility := vis;
      Result.Add(v);
    end;
  end;
end;



function c_FunctionToPas(const c:string;var r:TRoutine):boolean;
var
  m: TMatch;
  ReturnType, FuncName, Parameters,
  CodeStr: string;
  ClassName: string;
  rt:TRoutineType;
  params,
  localvars:TVariableList;
  code:TCode;
  isStatic,isInline:boolean;
  comment:string;

//  isFar:boolean;
//  isPascal:boolean;
begin
  r := nil;
  try
  m := TRegEx.Match(c.Trim, rxMethod , [roSingleLine] );
  except
    on e:Exception do
      Exit(false);
  end;

  if not m.Success then
    Exit(False);

  ReturnType   := convertType( m.Groups['returntype'].Value );

  if ReturnType.EndsWith('*') then
    ReturnType := '^'+ReturnType.TrimRight(['*']);


  ClassName    := m.Groups['classname'].Value;
  FuncName     := m.Groups['funcname'].Value;
  Parameters   := m.Groups['parameters'].Value;
  CodeStr      := m.Groups['code'].Value;
//IsFar        := m.Groups['far'].Value<>'';
//IsPascal     := m.Groups['pascal'].Value<>'';

  if SameText(ClassName,FuncName) then
  begin
    rt := TRoutineType.&constructor;
    FuncName := 'Create';
  end
  else
    if SameText(ReturnType,'void') then
      rt := TRoutineType.&procedure
    else
      rt := TRoutineType.&function;

  Code := TCode.Create(CodeStr.Split([sLineBreak]));

  params := ParseMethodParams(Parameters);
  localvars := ParseLocalVars(Code);

  code.Lines := ConvertCLinesToPas(code.Lines).Split([sLineBreak]);

  isInline := SameText(m.Groups['inline'].Value.Trim,'inline');
  isStatic := SameText(m.Groups['static'].Value.Trim,'static');

  Comment := m.Groups['comment'].value;

  r := TRoutine.Create(
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
    comment
  );
  exit(True);
end;






function c_StructToPas(c:string;out outClass:TClassDef):Boolean;
var
  m:TMatch;
  Name:string;
  Code:TCode;
  v:TVariableList;
begin
  Result := false;
  for m in TRegEx.Matches(c,'struct\s+(?<packed>PACKED)?\s*(?<name>'+rxId+')[^{]*\{',[roMultiLine] ) do
  begin
    Result := True;
    Name := m.Groups['name'].Value;
    Code.Lines := c.Split([sLineBreak]);
{    if length(code.Lines)>1 then
    begin
      Move(Code.Lines[1],Code.Lines[0],length(Code.Lines)-1);
      Setlength(code.Lines,length(code.Lines)-1);
    end;
}
    v := parseLocalVars(Code);
    if v.Count = 0 then
      exit(false);
    outClass := TClassDef.Create(Name,v);
    outClass.Kind := TClassKind.&record;
    outClass.IsPacked := m.Groups['packed'].Value='PACKED';
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






procedure AddArrays1D(var u: TPascalUnit; const c: string);
var
  m: TMatch;
  ar: TArrayDef1D;
  I: Integer;
  level: Integer; r: string;
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
        r := trim(copy(c, m.Index, 2 + I - m.Index));
        if c_Array1DToPas(r, ar) then
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
  level: Integer; r: string;
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
        r := trim(copy(c, m.Index, 2 + I - m.Index));
        ar := TArrayDef2D.Create;
        if c_Array2DToPas(r, ar) then
          u.GlobalArrays2D := u.GlobalArrays2D + [ar];


        ar.Sourceinfo.Position := m.Index;
        ar.Sourceinfo.Length   := I - m.Index + 1;

        Break;
      end;
    end;
  end;
end;


function c_class_to_pas(c:string; pas:TClassDef):Boolean;
var m:TMatch; classDef:TArray<string>;i:integer;
  v:TVariable;va:tarray<TVariable>;
  vis:TVisibility;
begin
  Result := True;
  m := TRegEx.Match(c, '^(?<indent>\s*)class\s+(?<classname>'+rxId+')\s*(?<parent>\:\s*(?<public>public)?\s*(?<parentclass>'+rxType+')\s*)?\s*\{(?<classdef>[^\}]*)\}' , [roMultiLine]);
  if not m.Success then
    Exit(False);

  pas.Kind       := TClassKind.&class;
  pas.name       := m.Groups['classname'].Value;
  pas.ParentType := m.Groups['parentclass'].Value;
  classDef       := m.Groups['classdef'].Value.Split([sLineBreak]);

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

    va := [];
    GetVariablesFromLine( va, classDef[i]);
    for v in va  do
    begin
      v.Visibility := vis;
      pas.Vars.Add(v);
    end;
  end;

end;

procedure AddClassDefs(var u: TPascalUnit; const aCCode: string;var t:string);
var
  m: TMatch;
  r: string;
  cl: TClassDef;
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

  // search for class definitions
  mc := TRegEx.Matches(aCCode, '^(?<indent>\s*)class\s+(?<classname>'+rxId+')\s*\{' , [roMultiLine]);
  for m in mc do
  begin
    // we've found a class signature.
    // now let's scan until we've found the final closing bracket
    // there can be nested brackets
    ScanUntilMatchingChar('{','}',aCCode,m,r);
    if r='' then
      Continue;
    cl := TClassDef.Create;
    cl.Sourceinfo.Position := m.Index;
    cl.Sourceinfo.Length   := r.Length;
    if c_class_to_pas(r,cl) then
      u.AddClass(cl)
    else
      cl.Free;
  end;

  mc := TRegEx.Matches(aCCode, '^(?<indent>\s*)class\s+(?<classname>'+rxId+')\s*(?<parent>\:\s*(?<public>public)?\s*(?<parentclass>'+rxId+')\s*)\{' , [roMultiLine]);
  for m in mc do
  begin
    // we've found a class signature.
    // now let's scan until we've found the final closing bracket
    // there can be nested brackets
    ScanUntilMatchingChar('{','}',aCCode,m,r);
    if r='' then
      Continue;
    r := trim(copy(aCCode, m.Index, length(r)));
    cl := TClassDef.Create;
    cl.Sourceinfo.Position := m.Index;
    cl.Sourceinfo.Length   := r.Length;
    if c_class_to_pas(r,cl) then
      u.AddClass(cl)
    else
      cl.Free;
  end;

end;



procedure AddFunctions(u: TPascalUnit; const aCCode: string; GlobalClass: TClassDef; var t:string);
var
  m: TMatch;
  r: string;
  rt: TRoutine;
  cl: TClassDef;
  mc: TMatchCollection;
begin
  // search for functions by pattern..
  mc := TRegEx.Matches(aCCode, '^(?<indent>\s*)(static\s+)?(inline\s+)?(?<return_type>'+rxType+')\s*(?<classname>'+rxID+'::)?(?<funcname>'+rxID+')\s*\(\s*(?<params>[^\)]*)\s*\)[^{|^;]*\{' ,
                              [roMultiLine]);

  for m in mc do
  begin
    if m.Value.Trim.StartsWith('if'     ) then continue;
    if m.Value.Trim.StartsWith('for'    ) then continue;
    if m.Value.Trim.StartsWith('else'   ) then continue;
    if m.Value.Trim.StartsWith('while'  ) then continue;
    if m.Value.Trim.StartsWith('switch' ) then continue;
    if m.Value.Trim.StartsWith('for'    ) then continue;
    r := '';
    ScanUntilMatchingChar('{', '}', aCCode, m, r);
    if r<>'' then
    begin
//      for J := m.Index to I do
//        if not CharInSet(aCCode[J],[#13,#10]) then
//          t[J] := PARSED_MARKER;
      if c_FunctionToPas(r,rt) then
        if rt <> nil then
        begin
          if rt.ClassName = '' then
            cl := GlobalClass
          else
            cl := u.getClassByName(rt.ClassName);
          if cl = nil then
          begin
            cl := TClassDef.Create;
            cl.Name := rt.ClassName;
            u.AddClass(cl);
          end;
          rt.Sourceinfo.Position := m.Index;
          rt.Sourceinfo.Length   := r.Length;
          cl.AddRoutine(rt);
        end;
    end;
  end;
end;



procedure AddConsts(var u: TPascalUnit; const c: string; var t:string);
var
  m: TMatch;
  lvar : TVariable;
  val:string;
  WithoutRoutines:string;
  vars:TVariableList;
  code:TCode;
const
  rx = '^\s*#define\s+(?<name>'+rxID+')\s+(?<expr>.*)\s*$';
//  rx2 = '^\s*#define\s+'+rxID+'\s+.*$';

  rx3 = '^\s*const\s+(?<type>'+rxType+')\s+(?<name>'+rxID+')\s*\=\s*(?<expr>.*)\s*\;\s*$'; // const int kControllerPitchStep = 130;
begin
  // search for const definitions
  for m in TRegEx.Matches(c, rx, [roMultiLine]) do
  begin
    val := m.Groups['expr'].Value;
    val := val
            .Replace('<<',' shl ')
            .Replace('>>',' shr ')
            .Replace('  ',' ')
            ;
    lvar := TVariable.Create( m.Groups['name'].Value,'', TDir.&in, true, true, val);
    u.GlobalVars.Add( lvar );
  end;
  t := TRegEx.Replace(t,rx,PARSED_MARKER_STR,[roMultiLine] );


  for m in TRegEx.Matches(c, rx3, [roMultiLine]) do
  begin
    val := m.Groups['expr'].Value;
    lvar := TVariable.Create( m.Groups['name'].Value,convertType(m.Groups['type'].Value), TDir.&inout, true, true, val);
    u.GlobalVars.Add( lvar );
  end;
  t := TRegEx.Replace(t,rx3,PARSED_MARKER_STR,[roMultiLine] );


  WithoutRoutines := RemoveRoutines(c);
  Code := TCode.Create( WithoutRoutines.Split([sLineBreak]));
  vars := parseLocalVars( Code );
  for lVar in vars.Items do
    u.GlobalVars.Add(lvar);

end;



procedure AddStructs(var u: TPascalUnit; const c: string);
var
  m: TMatch;
  struct: TClassDef;
  r: string;
begin
  // search for struct definitions
  for m in TRegEx.Matches(c, '(struct)\s+(?<packed>PACKED)?\s*(?<name>'+rxID+')[^{^)]*\{', [roMultiLine]) do
  begin
    if m.Groups['name'].Value.StartsWith('*') then
      Continue;

    r := '';
    ScanUntilMatchingChar('{','}',c,m,r);
    if r='' then
      Continue;
    if c_StructToPas(r, struct) then
    begin
      struct.Sourceinfo.Position := m.Index;
      struct.Sourceinfo.Length   := r.Length;
      u.AddClass(struct);
    end;

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
  m,cm: TMatch; e:TEnumDef; i:TEnumItem;
  v,s: string; n:integer;
begin
  // search for enum definitions
  for m in TRegEx.Matches(c, '(?<typedef>typedef\s+)?enum\s*(?<name>'+rxType+')?\s*{(?<values>[^\}]*)}\s*(?<typedefname>'+rxType+')', [roMultiLine]) do
  begin
    e := TEnumDef.Create;
    if m.Groups['typedef'].Value<>'' then
      e.name := m.Groups['typedefname'].Value
    else
      e.name := m.Groups['name'].Value;

    e.Sourceinfo.Position := m.Index;
    e.Sourceinfo.Length   := m.Length;
    n := 0;
    v := m.Groups['values'].Value;

    ReplaceInComments(v,',','.');

    for s in v.Split([',']) do
    begin
      i := Default(TEnumItem);
      i.Index := n;
      if length(s.Split(['=']))>1 then
      begin
        i.Value := StrToIntDef(s.Split(['='])[1],n);
        i.Name  := s.Split(['='])[0].Trim.Replace(sLineBreak,' ');
      end
      else
      begin
        i.Value := n;
        i.Name  := s.Trim;
      end;
      i.Name  := TRegEx.Replace(i.Name,'/\*(?<comment>.*)?\*/','',[  ]).Trim;
      cm := TRegEx.Match(s,'/\*(?<comment>.*)?\*/');
      if cm.Success then
        i.Comment := cm.Groups['comment'].value;

      e.Items := e.Items + [ i ];
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
  rx = '#include\s*(?<inc>.*)\s*$';
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
var m:TMatch;t1,t2:string;
begin
  s := s.Replace('std::string', 'string');

  for m in tregex.Matches(s, 'std::vector\<\s*(?<elType>'+rxType+')\s*\>', [  roMultiLine ])  do
  begin
    t1 := m.Groups['elType'].Value;
    t2 := convertType(t1);
    if t1<>t2 then
    begin
      delete(s,m.Index, m.Length);
      t2 := 'TArray<'+t2+'>';
      Insert(t2, s, m.Index);
    end;
  end;
  s := s.Replace('std::vector', 'TArray');
  s := s.Replace('(*','( *');
end;



function c_to_pas(const aCCode:string; var t:string; aName:string='tmp'):TPascalUnit;
var
  GlobalClass:TClassDef;
  s:string;
begin
  Result := TPascalUnit.Create;
  Result.Name := aName;
  Result.usesListIntf.Units := [];
  Result.usesListIntf.&Unit := Result;
  Result.usesListImpl.&Unit := Result;
  s := aCCode;
  FixTypes(s);

  t := s;

//  Result.usesListIntf.AddUnit( 'SysUtils' );
  GlobalClass := TClassDef.Create;
  GlobalClass.Name := 'TGlobal';
  GlobalClass.Kind := &unit;
  Result.AddClass( GlobalClass );
//  GlobalClass.Add(c_FunctionToPas('int PostInc(inout int i){'+sLineBreak+'  result = i;'+sLineBreak+'  i += 1;'  +sLineBreak+'}'));
//  GlobalClass.Add(c_FunctionToPas('int PostDec(inout int i){'+sLineBreak+'  result = i;'+sLineBreak+'  i -= 1;'  +sLineBreak+'}'));
//  GlobalClass.Add(c_FunctionToPas('int PreInc(inout int i){ '+sLineBreak+'  i += 1;'    +sLineBreak+'result = i;'+sLineBreak+'}'));
//  GlobalClass.Add(c_FunctionToPas('int PreDec(inout int i){ '+sLineBreak+'  i -= 1;'    +sLineBreak+'result = i;'+sLineBreak+'}'));
  AddUnits(Result, s, t);
  AddClassDefs(Result,s,t);
  AddFunctions(Result, s, GlobalClass,t);
//  t := RemoveRoutines(s);
  AddEnums(Result, s);
  AddArrays1D(Result, s);
  AddArrays2D(Result, s);
  AddStructs(Result, s);
  AddConsts(Result, s, t);
end;

function c_to_pas_str(const c:string; var t:string):string;
begin
  Result := c_to_pas(c,t).toPascal;
//  Result := t;
//  Result := RemoveRoutines(c);
end;


end.
