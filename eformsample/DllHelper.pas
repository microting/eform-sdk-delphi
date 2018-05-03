unit DllHelper;

interface

uses
  Windows, SysUtils, Events;

type
  TCore_CreateFunc = function: integer; stdcall;
  TCore_StartFunc = function(serverConnectionString: WideString): integer; stdcall;
  TCore_SubscribeStartEvent = function(callback: LongInt): integer; stdcall;

  {$region 'TemplatFromXml types'}
  TCore_TemplatFromXml = function(xml: WideString; var id: integer; var _label: WideString;
      var displayOrder: integer; var checkListFolderName: WideString; var repeated: integer;
      var startDate: WideString; var endDate: WideString; var language: WideString;
      var multiApproval: boolean; var fastNavigation: boolean; var downloadEntities: boolean;
      var manualSync: boolean; var caseType: WideString):   integer; stdcall;
  TCore_TemplatFromXml_ElementListCount = function(var count: integer): integer; stdcall;
  TCore_TemplatFromXml_GetElementType = function(n: integer; var elementType: WideString): integer; stdcall;
  TCore_TemplatFromXml_GetDataElement = function(n: integer; var id: integer; var _label: WideString;
        var description: WideString; var displayOrder: integer; var reviewEnabled: boolean;
        var extraFieldsEnabled: boolean; var approvalEnabled: boolean): integer; stdcall;
  {$endregion}

  TAdminTools_CreateFunc = function(serverConnectionString: WideString): integer; stdcall;
  TAdminTools_DbSetupFunc = function(token: WideString; var reply: WideString): integer; stdcall;
  TAdminTools_DbSetupCompletedFunc = function(var reply: WideString): integer; stdcall;
  TAdminTools_DbSettingsReloadRemoteFunc = function(var reply: WideString): integer; stdcall;
  TAdminTools_MigrateDbFunc = function(var reply: WideString): integer; stdcall;

  TLastErrorFunc = function: WideString; stdcall;

  {$region 'TDllHelper declaration'}
  TDllHelper = class
  private
    class var instance: TDllHelper;
    initialized: boolean;
    handle: Cardinal;

    Core_CreateFunc: TCore_CreateFunc;
    Core_StartFunc: TCore_StartFunc;
    Core_SubscribeStartEventFunc: TCore_SubscribeStartEvent;

    {$region 'TemplatFromXml variables'}
    Core_TemplatFromXmlFunc: TCore_TemplatFromXml;
    Core_TemplatFromXml_ElementListCountFunc: TCore_TemplatFromXml_ElementListCount;
    Core_TemplatFromXml_GetElementTypeFunc: TCore_TemplatFromXml_GetElementType;
    Core_TemplatFromXml_GetDataElementFunc: TCore_TemplatFromXml_GetDataElement;
    {$endregion}

    AdminTools_CreateFunc: TAdminTools_CreateFunc;
    AdminTools_DbSetupFunc: TAdminTools_DbSetupFunc;
    AdminTools_DbSetupCompletedFunc: TAdminTools_DbSetupCompletedFunc;
    AdminTools_DbSettingsReloadRemoteFunc: TAdminTools_DbSettingsReloadRemoteFunc;
    AdminTools_MigrateDbFunc: TAdminTools_MigrateDbFunc;
    LastErrorFunc: TLastErrorFunc;


    constructor Create;
    procedure Initialize;
    procedure LoadDll;

  public
    destructor Destroy; override;
    class function GetInstance: TDllHelper;
    procedure Core_Create;
    procedure Core_Start(serverConnectionString: string);
    procedure Core_SubscribeStartEvent(callback: LongInt);

    {$region 'TemplatFromXml functions'}
    procedure Core_TemplatFromXml(xml: WideString; var Id: integer; var _label: WideString;
       var displayOrder: integer; var checkListFolderName: WideString; var repeated: integer;
       var startDate: WideString; var endDate: WideString; var language: WideString;
       var multiApproval: boolean; var fastNavigation: boolean; var downloadEntities: boolean;
       var manualSync: boolean;  var caseType: WideString);
    function Core_TemplatFromXml_ElementListCount: integer;
    function Core_TemplatFromXml_GetElementType(n: integer): WideString;
    procedure Core_TemplatFromXml_GetDataElement(n: integer; var id: integer; var _label: WideString;
        var description: WideString; var displayOrder: integer; var reviewEnabled: boolean;
        var extraFieldsEnabled: boolean; var approvalEnabled: boolean);
    {$endregion}

    procedure AdminTools_Create(serverConnectionString: string);
    function AdminTools_DbSetup(token: string): string;
    function AdminTools_DbSetupCompleted: string;
    function AdminTools_MigrateDb: string;
    function AdminTools_DbSettingsReloadRemote: string;

  end;
  {$endregion}


implementation

{$region 'TDllHelper implementation'}
class function TDllHelper.GetInstance: TDllHelper;
begin
  if (not Assigned(instance)) then
  begin
    instance := TDllHelper.Create;
    instance.Initialize;
  end;
  Result := instance;
end;


constructor TDllHelper.Create;
begin
  inherited Create;
  initialized := false;
  handle := 0;
end;

destructor TDllHelper.Destroy;
begin
  if handle <> 0 then
     FreeLibrary(handle);
end;

procedure TDllHelper.Initialize;
begin
  LoadDll;
  initialized := true;
end;

procedure TDllHelper.LoadDll;
begin
   handle := LoadLibrary('eFormSDK.Wrapper.dll') ;
   if handle = 0 then
      raise Exception.Create('eFormSDK wrapper dll not found');

   @Core_CreateFunc := GetProcAddress(handle, 'Core_Create') ;
   if not Assigned (Core_CreateFunc) then
     raise Exception.Create('function Core_Create not found');

   @Core_StartFunc := GetProcAddress(handle, 'Core_Start') ;
   if not Assigned (Core_StartFunc) then
     raise Exception.Create('function Core_Start not found');

   @Core_SubscribeStartEventFunc := GetProcAddress(handle, 'Core_SubscribeStartEvent') ;
   if not Assigned (Core_StartFunc) then
     raise Exception.Create('function Core_SubscribeStartEvent not found');

   @Core_TemplatFromXmlFunc := GetProcAddress(handle, 'Core_TemplatFromXml') ;
   if not Assigned (Core_TemplatFromXmlFunc) then
     raise Exception.Create('function Core_TemplatFromXml not found');

   @Core_TemplatFromXml_ElementListCountFunc := GetProcAddress(handle, 'Core_TemplatFromXml_ElementListCount') ;
   if not Assigned (Core_TemplatFromXml_ElementListCountFunc) then
     raise Exception.Create('function Core_TemplatFromXml_ElementListCount not found');

   @Core_TemplatFromXml_GetElementTypeFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetElementType') ;
   if not Assigned (Core_TemplatFromXml_GetElementTypeFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetElementType not found');

   @Core_TemplatFromXml_GetDataElementFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetDataElement') ;
   if not Assigned (Core_TemplatFromXml_GetDataElementFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetDataElement not found');

   @AdminTools_CreateFunc := GetProcAddress(handle, 'AdminTools_Create') ;
   if not Assigned (AdminTools_CreateFunc) then
     raise Exception.Create('function AdminTools_Create not found');

   @AdminTools_DbSetupFunc := GetProcAddress(handle, 'AdminTools_DbSetup') ;
   if not Assigned (AdminTools_DbSetupFunc) then
     raise Exception.Create('function AdminTools_DbSetup not found');

   @AdminTools_DbSetupCompletedFunc := GetProcAddress(handle, 'AdminTools_DbSetupCompleted') ;
   if not Assigned (AdminTools_DbSetupCompletedFunc) then
     raise Exception.Create('function AdminTools_DbSetupCompleted not found');

//   @AdminTools_MigrateDbFunc := GetProcAddress(handle, 'AdminTools_MigrateDb') ;
//   if not Assigned (AdminTools_MigrateDbFunc) then
//     raise Exception.Create('function AdminTools_MigrateDb not found');
//
//   @AdminTools_DbSettingsReloadRemoteFunc := GetProcAddress(handle, 'AdminTools_DbSettingsReloadRemote') ;
//   if not Assigned (AdminTools_DbSettingsReloadRemoteFunc) then
//     raise Exception.Create('function AdminTools_DbSettingsReloadRemote not found');

   @LastErrorFunc := GetProcAddress(handle, 'GetLastError') ;
   if not Assigned (LastErrorFunc) then
     raise Exception.Create('function GetLastError not found');
end;

procedure TDllHelper.Core_Create;
var
  res: integer;
  err: WideString;
begin
  res := Core_CreateFunc;
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_Start(serverConnectionString: string);
var
  res: integer;
  err: WideString;
begin
  res := Core_StartFunc(serverConnectionString);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;


procedure TDllHelper.Core_SubscribeStartEvent(callback: LongInt);
var
  res: integer;
  err: WideString;
begin
  res := Core_SubscribeStartEventFunc(callback);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

{$region 'TemplatFromXml implementation'}
procedure TDllHelper.Core_TemplatFromXml(xml: WideString; var Id: integer; var _label: WideString;
   var displayOrder: integer; var checkListFolderName: WideString; var repeated: integer;
   var startDate: WideString; var endDate: WideString; var language: WideString;
   var multiApproval: boolean; var fastNavigation: boolean; var downloadEntities: boolean;
   var manualSync: boolean;  var caseType: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXmlFunc(xml, id, _label, displayOrder, checkListFolderName,repeated, startDate,
     endDate, language, multiApproval, fastNavigation, downloadEntities, manualSync, caseType);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

function  TDllHelper.Core_TemplatFromXml_ElementListCount: integer;
var
  res: integer;
  err: WideString;
  count: integer;
begin
  Result := 0;
  res := Core_TemplatFromXml_ElementListCountFunc(count);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := count;
end;

function TDllHelper.Core_TemplatFromXml_GetElementType(n: integer): WideString;
var
  res: integer;
  err: WideString;
  elementType: WideString;
begin
  Result := '';
  res := Core_TemplatFromXml_GetElementTypeFunc(n, elementType );
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := elementType;
end;

procedure TDllHelper.Core_TemplatFromXml_GetDataElement(n: integer; var id: integer; var _label: WideString;
        var description: WideString; var displayOrder: integer; var reviewEnabled: boolean;
        var extraFieldsEnabled: boolean; var approvalEnabled: boolean);
var
  res: integer;
  err: WideString;
  count: integer;
begin
  res := Core_TemplatFromXml_GetDataElementFunc(n, id, _label, description, displayOrder, reviewEnabled,
      extraFieldsEnabled, approvalEnabled);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;
{$endregion}

procedure TDllHelper.AdminTools_Create(serverConnectionString: string);
var
  res: integer;
  err: WideString;
begin
  res := AdminTools_CreateFunc(serverConnectionString);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

function TDllHelper.AdminTools_DbSetup(token: string): string;
var
  res: integer;
  reply: WideString;
  err: WideString;
begin
  res := AdminTools_DbSetupFunc(token, reply);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := reply;
end;


function TDllHelper.AdminTools_DbSetupCompleted: string;
var
  res: integer;
  reply: WideString;
  err: WideString;
begin
  res := AdminTools_DbSetupCompletedFunc(reply);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := reply;
end;

function TDllHelper.AdminTools_MigrateDb: string;
var
  res: integer;
  reply: WideString;
  err: WideString;
begin
  res := AdminTools_MigrateDbFunc(reply);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := reply;
end;

function TDllHelper.AdminTools_DbSettingsReloadRemote: string;
var
  res: integer;
  reply: WideString;
  err: WideString;
begin
  res := AdminTools_DbSettingsReloadRemoteFunc(reply);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := reply;
end;
{$endregion}


end.


