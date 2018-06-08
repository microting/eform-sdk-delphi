unit DllHelper;

interface

uses
  Windows, SysUtils, Events;

type
  TCore_CreateFunc = function: integer; stdcall;
  TCore_StartFunc = function(serverConnectionString: WideString): integer; stdcall;
  TCore_HandleCaseCreated = function(callback: LongInt): integer; stdcall;
  TCore_HandleCaseCompleted = function(callback: LongInt): integer; stdcall;
  TCore_HandleCaseDeleted = function(callback: LongInt): integer; stdcall;

  TCore_TemplateFromXml = function(xml: WideString; var json: WideString): integer; stdcall;
  TCore_TemplateCreate = function(json: WideString; var templateId: integer): integer; stdcall;
  TCore_TemplateRead = function(templateId: integer; var json: WideString): integer; stdcall;
  TCore_TemplateValidation = function (jsonMainElement: WideString;
         var jsonValidationErrors: WideString) : integer; stdcall;
  TCore_TemplateDelete = function (templateId: integer;  var deleteResult: boolean) : integer; stdcall;
  TCore_TemplateUploadData = function (jsonMainElementIn: WideString;
         var jsonMainElementOut: WideString) : integer; stdcall;
  TCore_Advanced_SiteItemReadAll = function (var json: WideString): integer; stdcall;
  TCore_TemplateItemRead = function(templateId: Integer; var json: WideString) : integer; stdcall;
  TCore_CaseCreate = function (jsonMainElement: WideString; caseUId: WideString; siteUId: integer;
       var resultCase: WideString): integer; stdcall;
  TCore_CaseCreate2 = function (jsonMainElement: WideString; caseUId: WideString; jsonSiteUIds: WideString;
       custom: WideString; var jsonResultCases: WideString): integer; stdcall;
  TCore_CaseRead = function(microtingUId: WideString; checkUId: WideString;
       var jsonReplyElement: WideString): integer; stdcall;
  TCore_CaseDelete = function (microtingUId: WideString; var deleteResult: boolean): integer; stdcall;


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
    Core_HandleCaseCreatedFunc: TCore_HandleCaseCreated;
    Core_HandleCaseCompletedFunc: TCore_HandleCaseCompleted;
    Core_HandleCaseDeletedFunc: TCore_HandleCaseDeleted;

    Core_TemplateFromXmlFunc: TCore_TemplateFromXml;
    Core_TemplateCreateFunc: TCore_TemplateCreate;
    Core_TemplateReadFunc: TCore_TemplateRead;
    Core_TemplateValidationFunc: TCore_TemplateValidation;
    Core_TemplateDeleteFunc: TCore_TemplateDelete;
    Core_TemplateUploadDataFunc: TCore_TemplateUploadData;
    Core_Advanced_SiteItemReadAllFunc: TCore_Advanced_SiteItemReadAll;
    Core_TemplateItemReadFunc: TCore_TemplateItemRead;
    Core_CaseCreateFunc: TCore_CaseCreate;
    Core_CaseCreate2Func: TCore_CaseCreate2;
    Core_CaseReadFunc: TCore_CaseRead;
    Core_CaseDeleteFunc: TCore_CaseDelete;

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
    procedure Core_HandleCaseCreated(callback: LongInt);
    procedure Core_HandleCaseCompleted(callback: LongInt);
    procedure Core_HandleCaseDeleted(callback: LongInt);

    function Core_TemplateFromXml(xml: WideString; var json: WideString) : integer;
    function Core_TemplateCreate(json: WideString) : integer;
    function Core_TemplateRead(templateId: Integer; var json: WideString) : integer;
    function Core_TemplateValidation(jsonMainElement: WideString;
         var jsonValidationErrors: WideString) : integer;
    function Core_TemplateDelete(templateId: integer;  var deleteResult: boolean) : integer;
    function Core_TemplateUploadData(jsonMainElementIn: WideString;
         var jsonMainElementOut: WideString) : integer;
    function Core_Advanced_SiteItemReadAll(var json: WideString): integer;
    function Core_TemplateItemRead(templateId: Integer; var json: WideString) : integer;
    function Core_CaseCreate(jsonMainElement: WideString; caseUId: WideString; siteUId: integer;
       var resultCase: WideString): integer; overload;
    function Core_CaseCreate(jsonMainElement: WideString; caseUId: WideString; jsonSiteUIds: WideString;
       custom: WideString; var jsonResultCases: WideString): integer; overload;
    function Core_CaseRead(microtingUId: WideString; checkUId: WideString; var jsonReplyElement: WideString): integer;
    function Core_CaseDelete(microtingUId: WideString; var deleteResult: boolean): integer;


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

   @Core_HandleCaseCreatedFunc := GetProcAddress(handle, 'Core_HandleCaseCreated') ;
   if not Assigned (Core_HandleCaseCreatedFunc) then
     raise Exception.Create('function Core_HandleCaseCreated not found');

   @Core_HandleCaseCompletedFunc := GetProcAddress(handle, 'Core_HandleCaseCompleted') ;
   if not Assigned (Core_HandleCaseCompletedFunc) then
     raise Exception.Create('function Core_HandleCaseCompleted not found');

   @Core_HandleCaseDeletedFunc := GetProcAddress(handle, 'Core_HandleCaseDeleted') ;
   if not Assigned (Core_HandleCaseDeletedFunc) then
     raise Exception.Create('function Core_HandleCaseDeleted not found');

   @Core_TemplateFromXmlFunc := GetProcAddress(handle, 'Core_TemplateFromXml') ;
   if not Assigned (Core_TemplateFromXmlFunc) then
     raise Exception.Create('function Core_TemplateFromXml not found');

   @Core_TemplateCreateFunc := GetProcAddress(handle, 'Core_TemplateCreate') ;
   if not Assigned (Core_TemplateCreateFunc) then
     raise Exception.Create('function Core_TemplateCreate not found');

   @Core_TemplateReadFunc := GetProcAddress(handle, 'Core_TemplateRead') ;
   if not Assigned (Core_TemplateReadFunc) then
     raise Exception.Create('function Core_TemplateRead not found');

   @Core_TemplateValidationFunc := GetProcAddress(handle, 'Core_TemplateValidation') ;
   if not Assigned (Core_TemplateValidationFunc) then
     raise Exception.Create('function Core_TemplateValidation not found');

   @Core_TemplateDeleteFunc := GetProcAddress(handle, 'Core_TemplateDelete') ;
   if not Assigned (Core_TemplateDeleteFunc) then
     raise Exception.Create('function Core_TemplateDelete not found');

   @Core_TemplateUploadDataFunc := GetProcAddress(handle, 'Core_TemplateUploadData') ;
   if not Assigned (Core_TemplateUploadDataFunc) then
     raise Exception.Create('function Core_TemplateUploadData not found');

   @Core_Advanced_SiteItemReadAllFunc := GetProcAddress(handle, 'Core_Advanced_SiteItemReadAll') ;
   if not Assigned (Core_Advanced_SiteItemReadAllFunc) then
     raise Exception.Create('function Core_Advanced_SiteItemReadAll not found');

   @Core_TemplateItemReadFunc := GetProcAddress(handle, 'Core_TemplateItemRead') ;
   if not Assigned (Core_TemplateItemReadFunc) then
     raise Exception.Create('function Core_TemplateItemRead not found');

   @Core_CaseCreateFunc := GetProcAddress(handle, 'Core_CaseCreate') ;
   if not Assigned (Core_CaseCreateFunc) then
     raise Exception.Create('function Core_CaseCreate not found');

   @Core_CaseCreate2Func := GetProcAddress(handle, 'Core_CaseCreate2') ;
   if not Assigned (Core_CaseCreate2Func) then
     raise Exception.Create('function Core_CaseCreate2 not found');

   @Core_CaseReadFunc := GetProcAddress(handle, 'Core_CaseRead') ;
   if not Assigned (Core_CaseReadFunc) then
     raise Exception.Create('function Core_CaseRead not found');

   @Core_CaseDeleteFunc := GetProcAddress(handle, 'Core_CaseDelete') ;
   if not Assigned (Core_CaseDeleteFunc) then
     raise Exception.Create('function Core_CaseDelete not found');


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


procedure TDllHelper.Core_HandleCaseCreated(callback: LongInt);
var
  res: integer;
  err: WideString;
begin
  res := Core_HandleCaseCreatedFunc(callback);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_HandleCaseCompleted(callback: LongInt);
var
  res: integer;
  err: WideString;
begin
  res := Core_HandleCaseCompletedFunc(callback);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;


procedure TDllHelper.Core_HandleCaseDeleted(callback: LongInt);
var
  res: integer;
  err: WideString;
begin
  res := Core_HandleCaseDeletedFunc(callback);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;


function TDllHelper.Core_TemplateFromXml(xml: WideString; var json: WideString) : integer;
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplateFromXmlFunc(xml, json);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := res;
end;

function TDllHelper.Core_TemplateCreate(json: WideString) : integer;
var
  res: integer;
  err: WideString;
  templateId: integer;
begin
  res := Core_TemplateCreateFunc(json, templateId);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := templateId;
end;



function TDllHelper.Core_TemplateRead(templateId: Integer; var json: WideString) : integer;
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplateReadFunc(templateId, json);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := res;
end;


function TDllHelper.Core_TemplateValidation(jsonMainElement: WideString;
  var jsonValidationErrors: WideString) : integer;
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplateValidationFunc(jsonMainElement, jsonValidationErrors);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := res;
end;


function TDllHelper.Core_TemplateDelete(templateId: integer;  var deleteResult: boolean) : integer;
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplateDeleteFunc(templateId, deleteResult);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := res;
end;

function TDllHelper.Core_TemplateUploadData(jsonMainElementIn: WideString;
   var jsonMainElementOut: WideString) : integer;
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplateUploadDataFunc(jsonMainElementIn, jsonMainElementOut);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := res;
end;


function TDllHelper.Core_Advanced_SiteItemReadAll(var json: WideString): integer;
var
  res: integer;
  err: WideString;
begin
  res := Core_Advanced_SiteItemReadAllFunc(json);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := res;
end;

function TDllHelper.Core_TemplateItemRead(templateId: Integer; var json: WideString) : integer;
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplateItemReadFunc(templateId, json);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := res;
end;


function TDllHelper.Core_CaseCreate(jsonMainElement: WideString; caseUId: WideString;
     siteUId: integer; var resultCase: WideString) : integer;
var
  res: integer;
  err: WideString;
begin
  res := Core_CaseCreateFunc(jsonMainElement, caseUId, siteUId, resultCase);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := res;
end;


function TDllHelper.Core_CaseCreate(jsonMainElement: WideString; caseUId: WideString; jsonSiteUIds: WideString;
   custom: WideString; var jsonResultCases: WideString): integer;
var
  res: integer;
  err: WideString;
begin
  res := Core_CaseCreate2Func(jsonMainElement, caseUId, jsonSiteUIds, custom, jsonResultCases);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := res;
end;

function TDllHelper.Core_CaseRead(microtingUId: WideString; checkUId: WideString;
   var jsonReplyElement: WideString): integer;
var
  res: integer;
  err: WideString;
begin
  res := Core_CaseReadFunc(microtingUId, checkUId, jsonReplyElement);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := res;
end;

function TDllHelper.Core_CaseDelete(microtingUId: WideString; var deleteResult: boolean): integer;
var
  res: integer;
  err: WideString;
begin
  res := Core_CaseDeleteFunc(microtingUId, deleteResult);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  result := res;
end;

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


