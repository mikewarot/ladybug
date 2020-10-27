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
  procedure SetSourceCode(S : String);

var
  StringOut    : StringOutputRoutine = @DefaultOutput;

implementation

var
  SourceBuffer : string = '';
  SourcePos    : Integer = 1;

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

procedure SetSourceCode(S : String);
begin
  SourceBuffer := S;
  SourcePos := 1;
end;

end.

