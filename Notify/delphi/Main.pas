unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Thread;

type
  TForm1 = class(TForm)
    bnEvent: TButton;
    procedure bnEventClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    hDevice: THandle;
    EventThread: TEventThread;
    data: integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.bnEventClick(Sender: TObject);
var count: DWord;
begin
  inc(data);

  if DeviceIOControl(hDevice, IOCTL_GENERATE_EVENT, @data, 4, NIL, 0, count, NIL)
  then OutputDebugString(PChar('Event='+IntToStr(data)+#13#10))
  else OutputDebugString('Event DeviceIOControl ERROR'#13#10);

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  hDevice:=CreateFile('\\.\NOTIFY', 0, 0, NIL, OPEN_EXISTING, 0, 0);

  if hDevice = INVALID_HANDLE_VALUE
  then begin
       OutputDebugString(PChar('Error on open the device: '+IntToStr(GetLastError)+#13#10));
       exit;
       end
  else OutputDebugString('Device in process is opened'#13#10);

  EventThread:=TEventThread.Create;
  data:=0;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  OutputDebugString('Device in process is closed'#13#10);
  CloseHandle(hDevice);
end;

end.
