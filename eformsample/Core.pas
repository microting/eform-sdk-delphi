unit Core;

interface

uses
  DllHelper, Events, SysUtils;

type
  TCore = class
  private
    FCoreStartEvent: TCoreStartEvent;

    procedure SetCoreStartEvent(Value: TCoreStartEvent);
    //procedure OnCoreStartEvent;
  public
    constructor Create;
    procedure Start(serverConnectionString: string);

    property CoreEvent: TCoreStartEvent read FCoreStartEvent write SetCoreStartEvent;

  end;

implementation

constructor TCore.Create;
begin
  inherited Create;
  TDllHelper.GetInstance.Core_Create;
end;

procedure TCore.Start(serverConnectionString: string);
begin
  TDllHelper.GetInstance.Core_Start(serverConnectionString);
end;

procedure OnCoreStartEvent(param: Integer);  stdcall;
begin
  WriteLn('On core test event, param: ' + IntToStr(param));
end;

procedure TCore.SetCoreStartEvent(Value: TCoreStartEvent);
begin
   FCoreStartEvent := Value;
   TDllHelper.GetInstance.Core_SubscribeStartEvent(LongInt(@OnCoreStartEvent));
end;





end.
