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

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonParseClick(Sender: TObject);
var
  i,j,k : integer;
  c     : char;
  sourcebuffer : string;
begin
  SourceBuffer := SourceCode.Text;
  for i := 1 to length(SourceBuffer) do
  begin
    c := sourcebuffer[i];
    MemoOutput.Lines.Append('Char ['+c+']');
  end;
end;

end.

