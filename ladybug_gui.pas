unit ladybug_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonTokenize: TButton;
    ButtonParse: TButton;
    MemoOutput: TMemo;
    SourceCode: TMemo;
    procedure ButtonParseClick(Sender: TObject);
    procedure ButtonTokenizeClick(Sender: TObject);
  private

  public

  end;

Type
  ParseState = (Starting,Number,Word,WhiteSpace,Unknown,Done);
Const
  StateName : Array[ParseState] of String = ('Starting','Number','Word','WhiteSpace','Unknown','Done');
Type
  ttoken = record
    Name : String;
    Kind : ParseState;
  end;

const
  ESC  : Char = #27;
  EOF  : Char = #26;
var
  Form1: TForm1;
  SourceBuffer : string = '';
  SourcePos    : Integer = 1;

implementation

{$R *.lfm}

{ TForm1 }

function GetCharacter : Char;
begin
  If SourcePos <= Length(SourceBuffer) then
  begin
    GetCharacter := SourceBuffer[SourcePos];
    Inc(SourcePos);
  end
  else
    GetCharacter := EOF; // control-z if we're at end
end;

function PeekCharacter : Char;
begin
  If SourcePos <= Length(SourceBuffer) then
    PeekCharacter := SourceBuffer[SourcePos]
  else
    PeekCharacter := EOF;
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

function GetToken : TToken;
var
  T : TToken;
  S : string;
  state,nextstate : ParseState;
  c : char;
begin
  T.Kind:= Unknown;
  T.Name:= '';
  State := Starting;
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
                      #9,#10,
                      #13,' '  : NextState := WhiteSpace;
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
      Unknown     : NextState := Done;
    else
      Form1.MemoOutput.Append('Unknown Token : ['+S+']');
      NextState := Done;
    end; // case State
    If NextState <> Done then
      S := S + GetCharacter
    else
    begin
      T.Kind:= State;
      T.Name:=S;
    end;
    State := NextState;
  until State in [Done];
  GetToken := T;
end;

procedure TForm1.ButtonParseClick(Sender: TObject);
var
  c     : char;
  expanded     : string;
begin
  SourceBuffer := SourceCode.Text;
  SourcePos    := 1;
  MemoOutput.Clear;
  c := GetCharacter;
  While C <> EOF do
  begin
    expanded := '';
    case c of
      #0   : expanded := '#0';
      #27  : expanded := 'ESC';
      #1..#26  : expanded := '^' + char(ord(c)+64);
      #28..#31 : expanded := '^' + ord(c).ToString;
      #127 : expanded := 'DEL';
    else
      expanded := c;
    end; // case
    MemoOutput.Lines.Append('Char ['+expanded+']');
    C := GetCharacter;
  end;
end;

procedure TForm1.ButtonTokenizeClick(Sender: TObject);
var
  s : string;
  t : ttoken;
begin
  SourceBuffer := SourceCode.Text;
  SourcePos    := 1;
  MemoOutput.Clear;
  T := GetToken;
  While (T.Name <> EOF) do
  begin
    MemoOutput.Lines.Append(StateName[T.Kind] + ' ['+T.Name+']');
    T := GetToken;
  end;
end;

end.

