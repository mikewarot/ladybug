unit ladybug_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, BasicCompiler;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonTokenize: TButton;
    ButtonParse: TButton;
    ButtonInterpret: TButton;
    KeyWords: TListBox;
    MainMenu1: TMainMenu;
    ProgramLines: TMemo;
    MemoOutput: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    DisplayWhitespace: TMenuItem;
    SourceCode: TMemo;
    procedure ButtonInterpretClick(Sender: TObject);
    procedure ButtonParseClick(Sender: TObject);
    procedure ButtonTokenizeClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

Procedure ShowOutput(S : String);
begin
  form1.MemoOutput.Append(s);
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
      ShowOutput('Unknown Token : ['+S+']');
      NextState := Done;
    end; // case State
    If NextState <> Done then
      S := S + GetCharacter;

    T.Kind:= State;
    T.Name:=S;
    State := NextState;
  until State in [Done,EatDone];
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
  While C <> ^Z do
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

procedure TForm1.ButtonInterpretClick(Sender: TObject);
var
  s : string;
  t,x : ttoken;
  i : integer;
begin
  SourceBuffer := SourceCode.Text;
  SourcePos    := 1;
  MemoOutput.Clear;
  Repeat
    GetToken(T);
    Case T.Kind of
      Word         : Case T.Name.ToUpper of
                       'BYE'   : Application.Terminate;
                       'CLEAR' : ProgramLines.Clear;
                       'LIST'  : MemoOutput.Text := ProgramLines.Text;
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
                                   MemoOutput.Append(S);
                                 end;
                     else
                       MemoOutput.Append('Unhandled WORD : '+T.Name);
                     end;  // CaseT.Name.ToUpper
      Number       : begin
                       S := T.Name;
                       Repeat
                         GetToken(T);
                         If T.Kind <> EOL then
                           S := S + T.Name;
                       Until T.Kind in [EOL,EOF];
                       ProgramLines.Append(S);
                     end;
      EOL,EOF      : // nothing
    else
      MemoOutput.Append('Unhandled Token '+StateName[T.Kind] + ' ['+T.Name+']');
    end; // case T.Kind
  Until T.Kind = EOF;
end;

procedure TForm1.ButtonTokenizeClick(Sender: TObject);
var
  s : string;
  t : ttoken;
  i : integer;
begin
  SourceBuffer := SourceCode.Text;
  SourcePos    := 1;
  MemoOutput.Clear;
  GetToken(T);
  While (T.Kind <> EOF) do
  begin
    Case T.Kind of
      WhiteSpace   : If Form1.DisplayWhitespace.Checked then
                       MemoOutput.Lines.Append(StateName[T.Kind] + ' ['+T.Name+']');
      Word         : Begin
                       I := KeyWords.Items.IndexOf(T.Name);
                       If I >= 0 then
                         MemoOutput.Lines.Append('KEYWORD: '+KeyWords.Items[i])
                       Else
                         MemoOutput.Lines.Append('UNKNOWN KEYWORD: '+T.Name);
                     end;
    else
      MemoOutput.Lines.Append(StateName[T.Kind] + ' ['+T.Name+']');
    end;
    GetToken(T);
  end;
end;

initialization
  BasicCompiler.StringOut := @ShowOutput;
end.

