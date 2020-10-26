unit ladybug_gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonParse: TButton;
    MemoOutput: TMemo;
    SourceCode: TMemo;
    procedure ButtonParseClick(Sender: TObject);
  private

  public

  end;

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
    GetCharacter := #27; // control-z if we're at end
end;

procedure TForm1.ButtonParseClick(Sender: TObject);
var
  i,j,k : integer;
  c     : char;
  expanded     : string;
begin
  SourceBuffer := SourceCode.Text;
  MemoOutput.Clear;
  c := GetCharacter;
  While C <> #27 do
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

end.

