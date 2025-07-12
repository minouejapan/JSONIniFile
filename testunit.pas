unit TestUnit;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, JSONIni;

type

  { TForm1 }

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    RadioButton1: TRadioButton;
    RadioGroup1: TRadioGroup;
    SpinEdit1: TSpinEdit;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    Ini: TJSONIni;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Ini := TJSONIni.Create;

  Ini.ReadWin('Window', 'Main', TForm(Form1));
  Edit1.Text := Ini.ReadStr('Options', 'Edit1', '');
  CheckBox1.Checked := Ini.ReadBool('Options', 'CheckBox1', False);
  RadioButton1.Checked := Ini.ReadBool('Options', 'RadioButton1', False);
  RadioGRoup1.ItemIndex := Ini.ReadInt('Options', 'RadioGroup1', 1);
  SpinEdit1.Value := Ini.ReadInt('Options', 'SpinEdit1', 100);
  FloatSpinEdit1.Value := Ini.ReadFloat('Options', 'FloatSpinEdit1', 1.5);
  Ini.Update;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Ini.WriteWin('Window', 'Main', Form1);
  Ini.WriteStr('Options', 'Edit1', Edit1.Text);
  Ini.WriteBool('Options', 'CheckBox1', CheckBox1.Checked);
  Ini.WriteBool('Options', 'RadioButton1', RadioButton1.Checked);
  Ini.WriteInt('Options', 'RadioGroup1', RadioGRoup1.ItemIndex);
  Ini.WriteInt('Options', 'SpinEdit1', SpinEdit1.Value);
  Ini.WriteFloat('Options', 'FloatSpinEdit1', FloatSpinEdit1.Value);

  Ini.Free;
end;

end.

