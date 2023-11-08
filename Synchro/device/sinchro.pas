unit sinchro;

interface

uses
  Windows, Messages, SysUtils, ExtCtrls;

const { sinchro interface GUID }
  SinchroIID: TGUID = '{412EBBE6-C154-4F0D-8A2A-766A189C2A95}';

type

  TDevice = class
  private
    FHDevice : THandle;
  public
    constructor Create(DeviceName: string);
    destructor Destroy; override;
    function Wait(Duration: integer): integer;
    function State: integer;
    function Delay(Duration: integer): integer;
    property HDevice: THandle read FHDevice;
  end;

implementation

{  TDevice  }

constructor TDevice.Create(DeviceName: string);
begin
  inherited Create;

  FHDevice:=CreateFile(PChar(DeviceName), GENERIC_READ or GENERIC_WRITE, 0, NIL, OPEN_EXISTING, 0, 0);

  if FHDevice = INVALID_HANDLE_VALUE
  then OutputDebugString(PChar('Error on open the device: '+IntToStr(GetLastError)+#13#10))
  else OutputDebugString(PChar('Device '+ DeviceName +' is Opened'#13#10));
end;

destructor TDevice.Destroy;
begin
  CloseHandle(FHDevice);
  OutputDebugString('Device is Closed'#13#10);
  inherited Destroy;
end;

function TDevice.Wait(Duration: integer): integer;
var complite: integer; count: DWord;
begin
  if DeviceIOControl(FHDevice, $222000, @Duration, 4, @complite, 4, count, NIL)
  then Result:=complite
  else Result:=-1;
end;

function TDevice.State: integer;
var complite: integer; count: DWord;
begin
  if DeviceIOControl(FHDevice, $222004, NIL, 0, @complite, 4, count, NIL)
  then Result:=complite
  else Result:=-1;
end;

function TDevice.Delay(Duration: integer): integer;
var complite: integer; count: DWord;
begin
  if DeviceIOControl(FHDevice, $222008, @Duration, 4, @complite, 4, count, NIL)
  then Result:=complite
  else Result:=-1;
end;

end.

