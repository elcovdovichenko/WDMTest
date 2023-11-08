unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DevClass, DevIEnum, ExtCtrls, Spin;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    bnEvent: TButton;
    SpinEdit1: TSpinEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure bnEventClick(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    { Private declarations }
    Device: TEventDevice;
    Interfaces: TDeviceInterfaceClassEnumerator;
    procedure NotifyManual(Data: integer);
    procedure NotifyTimer(Data: integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Interfaces.Free;
  Device.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
var i, n: integer;
begin

   Interfaces:=TDeviceInterfaceClassEnumerator.Create(@EventsIID);
   if Interfaces.Handle = INVALID_HANDLE_VALUE
   then begin
        Memo.Lines.Add('Device Interface Class is not exist');
        Interfaces.Free;
        Exit;
        end;

   i:=Interfaces.Count;
   Memo.Lines.Add(IntToStr(i)+' device interface(s) is found');
   if i > 0
   then for n:=0 to i-1 do Memo.Lines.Add(PChar(Interfaces[n]))
   else Exit;

   Device:=TEventDevice.Create(Interfaces[0]);
   if Device.HDevice = INVALID_HANDLE_VALUE
   then begin
        Memo.Lines.Add('Device is unable to open');
        Device.Free;
        Exit;
        end;

   Device.OnManual:= NotifyManual;
   Device.OnTimer:= NotifyTimer;

   Memo.Lines.Add('Device is ready');

end;

procedure TForm1.bnEventClick(Sender: TObject);
begin
   if Device.Manual = -1
   then Memo.Lines.Add('DeviceIOControl error on Manual');
end;

procedure TForm1.NotifyManual(Data: integer);
begin
  Memo.Lines.Add('Manual Event data is '+IntToStr(Data));
end;

procedure TForm1.NotifyTimer(Data: integer);
begin
  Memo.Lines.Add('Timer Event data is '+IntToStr(Data));
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
   if Device.Timing(SpinEdit1.Value) = -1
   then Memo.Lines.Add('DeviceIOControl error on Timing');
end;

end.
