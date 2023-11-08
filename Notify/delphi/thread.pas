unit thread;

interface

uses
  Windows, Messages, SysUtils, ExtCtrls, Classes;

const
  { IOCTLs }
  IOCTL_WAIT_NOTIFY = $222000;
  IOCTL_GENERATE_EVENT = $222004;

type
  TEventThread = class(TThread)
  private
    hDevice : THandle;
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

implementation

{ TEventThread }
constructor TEventThread.Create;
begin
  inherited Create(False);
  FreeOnTerminate:=True;
end;

procedure TEventThread.Execute;
var complite: integer; count: DWord;
begin
  OutputDebugString('Execute Entry'#13#10);

  hDevice:=CreateFile('\\.\NOTIFY', 0, 0, NIL, OPEN_EXISTING, 0, 0);

  if hDevice = INVALID_HANDLE_VALUE
  then begin
       OutputDebugString(PChar('Error on open the device: '+IntToStr(GetLastError)+#13#10));
       exit;
       end
  else OutputDebugString('Device in thread is opened'#13#10);

  while TRUE do
  if DeviceIOControl(hDevice, IOCTL_WAIT_NOTIFY, NIL, 0, @complite, 4, count, NIL)
  then OutputDebugString(PChar('Execute Exit Successfully ='+IntToStr(complite)+#13#10))
  else OutputDebugString('Execute Exit DeviceIOControl ERROR'#13#10);

  CloseHandle(hDevice);
end;

end.




