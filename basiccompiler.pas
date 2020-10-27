unit BasicCompiler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  ParseState = (Starting,Number,Word,WhiteSpace,Unknown,Done,GetString,GotString,EatDone,EOL,EOF);
Const
  StateName : Array[ParseState] of String = ('Starting','Number','Word','WhiteSpace','Unknown','Done','Unterminated String','String','EatDone','EOL','EOF');
Type
  ttoken = record
    Name : String;
    Kind : ParseState;
  end;

  StringOutputRoutine = Procedure(S : String);

  function GetCharacter : Char;
  function PeekCharacter : Char;
  function Expanded(C : Char):String;
  procedure DefaultOutput(S : String);
  procedure GetToken(Var T : TToken);
  procedure SetSourceCode(SourceCode : String);
  procedure Interpret(SourceCode : String);

var
  StringOut    : StringOutputRoutine = @DefaultOutput;

implementation

type
  TSourceLine = Record
    Number : Integer;
    Source : String;
  end;

var
  SourceBuffer : string = '';
  SourcePos    : Integer = 1;

  SourceLines  : Array[1..200] of TSourceLine;
  SourceCount  : Integer = 0;

procedure ClearLines;
begin
  SourceCount := 0;  // ignore everything restart from zero
end;

procedure ListLines;
var
  i : integer;
begin
  for i := 1 to SourceCount do
    StringOut(SourceLines[i].Number.ToString + ' ' + SourceLines[i].Source);
end;

procedure AddLine(LineNumber : Integer; SourceCode : String);
var
  i,j,k : integer;
  foundindex : integer;
  insertpoint : integer;
begin
  foundindex := 0;
  insertpoint := 1;
  for i := 1 to SourceCount do
    if SourceLines[i].Number = LineNumber then
      FoundIndex := i
    else
      If LineNumber > SourceLines[i].Number then
        Insertpoint := i+1;
  if (foundindex = 0) then
  begin
    If SourceCode <> '' then                       // don't add empty lines
    begin
      for i := SourceCount downto InsertPoint+1 do
        SourceLines[i] := SourceLines[i-1];        // move lines up
      inc(SourceCount);
      SourceLines[Insertpoint].Number:= LineNumber; // put in newly free location
      SourceLines[Insertpoint].Source:= SourceCode;
    end;
  end
  else
  begin                                              // found it
    If SourceCode <> '' then
      SourceLines[FoundIndex].Source:= SourceCode    // replace it
    else
    begin  // delete an existing line, if we replace with nothing
      for i := FoundIndex to SourceCount-1 do
        SourceLines[i] := SourceLines[i+1];
      dec(SourceCount);
    end;
  end;
end;

procedure DefaultOutput(S : String);
begin
  // do nothing
end;

function GetCharacter : Char;
begin
  If SourcePos <= Length(SourceBuffer) then
  begin
    GetCharacter := SourceBuffer[SourcePos];
    Inc(SourcePos);
  end
  else
    GetCharacter := ^Z; // control-z if we're at end
end;

function PeekCharacter : Char;
begin
  If SourcePos <= Length(SourceBuffer) then
    PeekCharacter := SourceBuffer[SourcePos]
  else
    PeekCharacter := ^Z;
end;

function Expanded(C : Char):String;
var
  s : string;
begin
  s := '';
  case c of
    #0   : s := '#0';
    #27  : s := 'ESC';
    #1..#26  : s := '^' + char(ord(c)+64);
    #28..#31 : s := '^' + ord(c).ToString;
    #127 : s := 'DEL';
  else
    s := c;
  end; // case
  Expanded := S;
end;

procedure GetToken(Var T : TToken);
var
  S : string;
  state,nextstate : ParseState;
  c : char;
  delimiter : char;
begin
  T.Kind:= Unknown;
  T.Name:= '';
  State := Starting;
  Delimiter := '"';
  NextState := Done;  // default to done, for safety
  S := '';
  repeat
    C := PeekCharacter;
    Case State of
      Starting    : Case C of
                      '0'..'9' : NextState := Number;
                      'A'..'Z',
                      'a'..'z',
                      '_'      : NextState := Word;
                      #9,' '   : NextState := WhiteSpace;
                      #10,#13  : NextState := EOL;
                      #26      : NextState := EOF;
                      '"','''' : begin
                                   Delimiter := C;
                                   NextState := GetString;
                                 end
                    else
                      NextState := Unknown;
                    end;
      Number      : Case C of
                      '0'..'9' : NextState := Number;
                    Else
                      NextState := Done;
                    end;
      Word        : Case C of
                      'A'..'Z',
                      'a'..'z',
                      '0'..'9',
                      '_'       : NextState := Word;
                    Else
                      NextState := Done;
                    end;
      WhiteSpace  : Case C of
                      #9,#10,#13,' ' : NextState := WhiteSpace;
                    else
                      NextState := Done;
                    end;
      EOL         : Case C of
                      #10,#13  : NextState := EOL;
                    else
                      NextState := Done;
                    end;
      EOF         : NextState := Done;
      GetString   : begin
                      If C = Delimiter then
                        NextState := GotString;
                      If C = ^Z then
                        NextState := Done;
                    end; // otherwise stay in GetString
      GotString   : NextState := Done;
      Unknown     : NextState := Done;
    else
      StringOut('Unknown Token : ['+S+']');
      NextState := Done;
    end; // case State
    If NextState <> Done then
      S := S + GetCharacter;

    T.Kind:= State;
    T.Name:=S;
    State := NextState;
  until State in [Done,EatDone];
end;

procedure SetSourceCode(SourceCode : String);
begin
  SourceBuffer := SourceCode;
  SourcePos := 1;
end;

procedure Interpret(SourceCode : String);
var
  s : string;
  t,x : ttoken;
  i : integer;
begin
  SetSourceCode(SourceCode);
  Repeat
    GetToken(T);
    Case T.Kind of
      Word         : Case T.Name.ToUpper of
//                       'BYE'   : Application.Terminate;
                       'CLEAR' : ClearLines;
                       'LIST'  : ListLines;
                       'PRINT' : begin
                                   S := '';
                                   repeat
                                     GetToken(X);
                                     Case X.Kind of
                                       GotString,
                                       Number               : S := S + X.Name;
                                     else
                                       s := s + ' ';
                                     end;
                                   until X.Kind in [EOL,EOF];
                                   StringOut(S);
                                 end;
                     else
                       StringOut('Unhandled WORD : '+T.Name);
                     end;  // CaseT.Name.ToUpper

      Number       : begin
                       i := T.Name.ToInteger;  // line number we're adding goes in I
                       S := '';
                       Repeat
                         GetToken(X);
                         Case X.Kind of
                           EOF,EOL :  ;  // nothing
                           WhiteSpace : If S <> '' then S := S + X.Name;
                         else
                           S := S + X.Name;
                         end;
                       Until X.Kind in [EOL,EOF];
                       AddLine(I,S);  // add line #i
                     end;
      EOL,EOF      : // nothing
    else
      StringOut('Unhandled Token '+StateName[T.Kind] + ' ['+T.Name+']');
    end; // case T.Kind
  Until T.Kind = EOF;
end;

end.

