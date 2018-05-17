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
  TCore_TemplatFromXml_DataItemCount = function(path: WideString; var count: integer): integer; stdcall;

  TCore_TemplatFromXml_GetDataItemType = function(path: WideString;
        var dataItemType: WideString): integer; stdcall;

  TCore_TemplatFromXml_GetPicture = function(path: WideString; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString): integer; stdcall;

  TCore_TemplatFromXml_GetShowPdf = function(path: WideString; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer;
        var color: WideString; var value: WideString): integer; stdcall;

  TCore_TemplatFromXml_GetDate = function(path: WideString; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory: boolean;
        var _readonly: boolean;  var color: WideString; var value: WideString): integer; stdcall;

  TCore_TemplatFromXml_GetSignature = function(path: WideString; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString): integer; stdcall;

  TCore_TemplatFromXml_GetSaveButton = function(path: WideString; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer;
        var value: WideString): integer; stdcall;

  TCore_TemplatFromXml_GetTimer = function(path: WideString; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer;
        var stopOnSave: Boolean; var mandatory: Boolean): integer; stdcall;

  TCore_TemplatFromXml_GetNone = function(path: WideString; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer): integer; stdcall;

  TCore_TemplatFromXml_GetCheckBox = function(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean; var selected: boolean): integer; stdcall;

  TCore_TemplatFromXml_GetMultiSelect = function(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean): integer; stdcall;

  TCore_TemplatFromXml_GetSingleSelect = function(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean): integer; stdcall;

  TCore_TemplatFromXml_GetNumber = function(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory:
        boolean; var decimalCount: integer; var unitName: WideString): integer; stdcall;

  TCore_TemplatFromXml_GetText = function(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var geolocationEnabled: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean): integer; stdcall;

  TCore_TemplatFromXml_GetComment = function(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var splitScreen: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean): integer; stdcall;

  TCore_TemplatFromXml_GetFieldContainer = function(path: WideString; var value: WideString;
        var fieldType: WideString): integer; stdcall;

  TCore_TemplatFromXml_KeyValueListCount =  function(path: WideString; var count: integer): integer; stdcall;
  TCore_TemplatFromXml_GetKeyValuePair = function(location: WideString; var key: WideString;
        var value: WideString; var selected: Boolean; var displayOrder: WideString): integer; stdcall;
  TCore_TemplatFromXml_DataItemGroupCount = function(location: WideString; var count: integer): integer;  stdcall;
  {$endregion}
  {$region 'TemplateCreate types'}
  TCore_TemplateCreate = function(json: WideString; var templateId: integer): integer; stdcall;
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
    Core_TemplatFromXml_DataItemCountFunc: TCore_TemplatFromXml_DataItemCount;
    Core_TemplatFromXml_GetDataItemTypeFunc: TCore_TemplatFromXml_GetDataItemType;
    Core_TemplatFromXml_GetPictureFunc: TCore_TemplatFromXml_GetPicture;
    Core_TemplatFromXml_GetShowPdfFunc: TCore_TemplatFromXml_GetShowPdf;
    Core_TemplatFromXml_GetDateFunc: TCore_TemplatFromXml_GetDate;
    Core_TemplatFromXml_GetSignatureFunc: TCore_TemplatFromXml_GetSignature;
    Core_TemplatFromXml_GetSaveButtonFunc: TCore_TemplatFromXml_GetSaveButton;
    Core_TemplatFromXml_GetTimerFunc: TCore_TemplatFromXml_GetTimer;
    Core_TemplatFromXml_GetNoneFunc: TCore_TemplatFromXml_GetNone;
    Core_TemplatFromXml_GetCheckBoxFunc: TCore_TemplatFromXml_GetCheckBox;
    Core_TemplatFromXml_GetMultiSelectFunc: TCore_TemplatFromXml_GetMultiSelect;
    Core_TemplatFromXml_GetSingleSelectFunc: TCore_TemplatFromXml_GetSingleSelect;
    Core_TemplatFromXml_GetNumberFunc: TCore_TemplatFromXml_GetNumber;
    Core_TemplatFromXml_GetTextFunc: TCore_TemplatFromXml_GetText;
    Core_TemplatFromXml_GetCommentFunc: TCore_TemplatFromXml_GetComment;
    Core_TemplatFromXml_GetFieldContainerFunc: TCore_TemplatFromXml_GetFieldContainer;
    Core_TemplatFromXml_KeyValueListCountFunc: TCore_TemplatFromXml_KeyValueListCount;
    Core_TemplatFromXml_GetKeyValuePairFunc: TCore_TemplatFromXml_GetKeyValuePair;
    Core_TemplatFromXml_DataItemGroupCountFunc: TCore_TemplatFromXml_DataItemGroupCount;
    {$endregion}

    {$region 'TemplateCreate variables'}
    Core_TemplateCreateFunc: TCore_TemplateCreate;
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
    function Core_TemplatFromXml_DataItemCount(path: WideString): integer;
    function Core_TemplatFromXml_GetDataItemType(path: WideString): WideString;
    procedure Core_TemplatFromXml_GetPicture(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString);
    procedure Core_TemplatFromXml_GetShowPdf(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var color: WideString; var value: WideString);
    procedure Core_TemplatFromXml_GetDate(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory: boolean;
        var readonly: boolean; var color: WideString; var value: WideString);
    procedure Core_TemplatFromXml_GetSignature(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString);
    procedure Core_TemplatFromXml_GetSaveButton(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var value: WideString);
    procedure Core_TemplatFromXml_GetTimer(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var stopOnSave: Boolean; var mandatory: Boolean);
    procedure  Core_TemplatFromXml_GetNone(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer);
    procedure Core_TemplatFromXml_GetCheckBox(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean; var selected: boolean);
    procedure Core_TemplatFromXml_GetMultiSelect(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean);
    procedure Core_TemplatFromXml_GetSingleSelect(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean);
    procedure Core_TemplatFromXml_GetNumber(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory: boolean; var decimalCount: integer;
        var unitName: WideString);
    procedure Core_TemplatFromXml_GetText(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var geolocationEnabled: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean);
    procedure Core_TemplatFromXml_GetComment(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var splitScreen: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean);
    procedure Core_TemplatFromXml_GetFieldContainer(location: WideString; var value: WideString;
      var fieldType: WideString);
    function Core_TemplatFromXml_KeyValueListCount(location: WideString): integer;
    procedure Core_TemplatFromXml_GetKeyValuePair(location: WideString; var key: WideString;
        var value: WideString; var selected: Boolean; var displayOrder: WideString);
    function Core_TemplatFromXml_DataItemGroupCount(location: WideString): integer;
    {$endregion}
    {$region 'TemplateCreate functions'}
    function Core_TemplateCreate(json: WideString) : integer;
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

   @Core_TemplatFromXml_DataItemCountFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataItemCount') ;
   if not Assigned (Core_TemplatFromXml_DataItemCountFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataItemCount not found');

   @Core_TemplatFromXml_GetDataItemTypeFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetDataItemType') ;
   if not Assigned (Core_TemplatFromXml_GetDataItemTypeFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetDataItemType not found');

   @Core_TemplatFromXml_GetPictureFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetPicture') ;
   if not Assigned (Core_TemplatFromXml_GetPictureFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetPicture not found');

   @Core_TemplatFromXml_GetShowPdfFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetShowPdf') ;
   if not Assigned (Core_TemplatFromXml_GetShowPdfFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetShowPdf not found');

   @Core_TemplatFromXml_GetDateFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetDate') ;
   if not Assigned (Core_TemplatFromXml_GetDateFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetDate not found');

   @Core_TemplatFromXml_GetSignatureFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetSignature') ;
   if not Assigned (Core_TemplatFromXml_GetSignatureFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetSignature not found');

   @Core_TemplatFromXml_GetSaveButtonFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetSaveButton') ;
   if not Assigned (Core_TemplatFromXml_GetSaveButtonFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetSaveButton not found');

   @Core_TemplatFromXml_GetTimerFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetTimer') ;
   if not Assigned (Core_TemplatFromXml_GetTimerFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetTimer not found');

   @Core_TemplatFromXml_GetNoneFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetNone') ;
   if not Assigned (Core_TemplatFromXml_GetNoneFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetNone not found');

   @Core_TemplatFromXml_GetCheckBoxFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetCheckBox') ;
   if not Assigned (Core_TemplatFromXml_GetCheckBoxFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetCheckBox not found');

   @Core_TemplatFromXml_GetMultiSelectFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetMultiSelect') ;
   if not Assigned (Core_TemplatFromXml_GetMultiSelectFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetMultiSelect not found');

   @Core_TemplatFromXml_GetSingleSelectFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetSingleSelect') ;
   if not Assigned (Core_TemplatFromXml_GetSingleSelectFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetSingleSelect not found');

   @Core_TemplatFromXml_GetNumberFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetNumber') ;
   if not Assigned (Core_TemplatFromXml_GetNumberFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetNumber not found');

   @Core_TemplatFromXml_GetTextFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetText') ;
   if not Assigned (Core_TemplatFromXml_GetTextFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetText not found');

   @Core_TemplatFromXml_GetCommentFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetComment') ;
   if not Assigned (Core_TemplatFromXml_GetCommentFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetComment not found');

   @Core_TemplatFromXml_GetFieldContainerFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetFieldContainer') ;
   if not Assigned (Core_TemplatFromXml_GetFieldContainerFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetFieldContainer not found');

   @Core_TemplatFromXml_KeyValueListCountFunc := GetProcAddress(handle, 'Core_TemplatFromXml_KeyValueListCount') ;
   if not Assigned (Core_TemplatFromXml_KeyValueListCountFunc) then
     raise Exception.Create('function Core_TemplatFromXml_KeyValueListCount not found');

   @Core_TemplatFromXml_GetKeyValuePairFunc := GetProcAddress(handle, 'Core_TemplatFromXml_GetKeyValuePair') ;
   if not Assigned (Core_TemplatFromXml_GetKeyValuePairFunc) then
     raise Exception.Create('function Core_TemplatFromXml_GetKeyValuePair not found');

   @Core_TemplatFromXml_DataItemGroupCountFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataItemGroupCount') ;
   if not Assigned (Core_TemplatFromXml_DataItemGroupCountFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataItemGroupCount not found');


   @Core_TemplateCreateFunc := GetProcAddress(handle, 'Core_TemplateCreate') ;
   if not Assigned (Core_TemplateCreateFunc) then
     raise Exception.Create('function Core_TemplateCreate not found');

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
  count := 0;
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
begin
  res := Core_TemplatFromXml_GetDataElementFunc(n, id, _label, description, displayOrder, reviewEnabled,
      extraFieldsEnabled, approvalEnabled);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

function TDllHelper.Core_TemplatFromXml_DataItemCount(path: WideString): integer;
var
  res: integer;
  err: WideString;
  count: integer;
begin
  count := 0;
  res := Core_TemplatFromXml_DataItemCountFunc(path, count);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := count;
end;


function TDllHelper.Core_TemplatFromXml_GetDataItemType(path: WideString): WideString;
var
  res: integer;
  err: WideString;
  dataItemType: WideString;
begin
  Result := '';
  res := Core_TemplatFromXml_GetDataItemTypeFunc(path, dataItemType);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := dataItemType;
end;

procedure TDllHelper.Core_TemplatFromXml_GetPicture(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetPictureFunc(path, id, _label, description, displayOrder,
      mandatory, color);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;


procedure TDllHelper.Core_TemplatFromXml_GetShowPdf(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var color: WideString; var value: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetShowPdfFunc(path, id, _label, description, displayOrder,
       color, value);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_GetDate(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory: boolean;
        var readonly: boolean; var color: WideString; var value: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetDateFunc(path, id, _label, description, displayOrder,
       minValue, maxValue, mandatory, readonly, color, value);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_GetSignature(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetSignatureFunc(path, id, _label, description, displayOrder,
      mandatory, color);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_GetSaveButton(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var value: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetSaveButtonFunc(path, id, _label, description, displayOrder, value);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_GetTimer(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var stopOnSave: Boolean; var mandatory: Boolean);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetTimerFunc(path, id, _label, description, displayOrder,
    stopOnSave, mandatory);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_GetNone(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetNoneFunc(path, id, _label, description, displayOrder);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_GetCheckBox(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean; var selected: boolean);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetCheckBoxFunc(path,  id, _label, description, displayOrder,
      mandatory, selected);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_GetMultiSelect(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetMultiSelectFunc(path, id, _label, description, displayOrder,
      mandatory);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_GetSingleSelect(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetSingleSelectFunc(path, id, _label, description, displayOrder,
      mandatory);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;


procedure TDllHelper.Core_TemplatFromXml_GetNumber(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory: boolean; var decimalCount: integer;
        var unitName: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetNumberFunc(path, id, _label, description, displayOrder, minValue,
      maxValue, mandatory, decimalCount, unitName);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_GetText(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var geolocationEnabled: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetTextFunc(path, id, _label, description, geolocationEnabled,
      value, readOnly, mandatory);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_GetComment(path: WideString; var id: integer;
        var _label: WideString; var description: WideString; var splitScreen: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean);
var
  res: integer;
  err: WideString;
begin
   res := Core_TemplatFromXml_GetCommentFunc(path, id, _label, description, splitScreen,
      value, readOnly, mandatory);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_GetFieldContainer(location: WideString; var value: WideString;
  var fieldType: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetFieldContainerFunc(location, value, fieldType);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;


function TDllHelper.Core_TemplatFromXml_KeyValueListCount(location: WideString): integer;
var
  res: integer;
  err: WideString;
  count: integer;
begin
  count := 0;
  res := Core_TemplatFromXml_KeyValueListCountFunc(location, count);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := count;
end;


procedure TDllHelper.Core_TemplatFromXml_GetKeyValuePair(location: WideString; var key: WideString;
        var value: WideString; var selected: Boolean; var displayOrder: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_GetKeyValuePairFunc(location, key, value, selected, displayOrder);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;


function TDllHelper.Core_TemplatFromXml_DataItemGroupCount(location: WideString): integer;
var
  res: integer;
  err: WideString;
  count: integer;
begin
  count := 0;
  res := Core_TemplatFromXml_DataItemGroupCountFunc(location, count);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := count;
end;

{$endregion}

{$region 'TemplateCreate implementation'}
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


