unit SerialStream;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Serial, SysUtils;

type

  TSerialStream = class(THandleStream)
  public
    constructor Create(const ADevice: string; const ABaud: integer;
      const AByteSize: integer = 8; const AParity: TParityType = NoneParity;
      const AStopBits: integer = 1);
    destructor Destroy; override;
  public
    function Read(var ABuffer; ACount: longint): longint; override;
  end;

implementation

constructor TSerialStream.Create(const ADevice: string;
  const ABaud, AByteSize: integer; const AParity: TParityType; const AStopBits: integer);
begin
  inherited Create(SerOpen(ADevice));
  if Handle = 0 then
  begin
    raise Exception.Create('Unable to open serial device');
  end;
  SerSetParams(Handle, ABaud, AByteSize, AParity, AStopBits, []);
end;

destructor TSerialStream.Destroy;
begin
  SerClose(Handle);
  inherited;
end;

function TSerialStream.Read(var ABuffer; ACount: longint): longint;
begin
  Result := SerReadTimeout(Handle, byte(ABuffer), ACount, High(longint));
end;

end.
