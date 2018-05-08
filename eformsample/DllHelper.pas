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
  TCore_TemplatFromXml_DataElement_DataItemCount = function(n: integer; var count: integer): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetDataItemType = function(n: integer; m: integer;
        var dataItemType: WideString): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetPicture = function(n: integer; m: integer; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetShowPdf = function(n: integer; m: integer; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer;
        var color: WideString; var value: WideString): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetDate = function(n: integer; m: integer; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory: boolean;
        var _readonly: boolean;  var color: WideString; var value: WideString): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetSignature = function(n: integer; m: integer; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetSaveButton = function(n: integer; m: integer; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer;
        var value: WideString): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetTimer = function(n: integer; m: integer; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer;
        var stopOnSave: Boolean; var mandatory: Boolean): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetNone = function(n: integer; m: integer; var id: integer;
        var _label: WideString;  var description: WideString; var displayOrder: integer): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetCheckBox = function(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean; var selected: boolean): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetMultiSelect = function(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetSingleSelect = function(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetNumber = function(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory:
        boolean; var decimalCount: integer): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetText = function(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var geolocationEnabled: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean): integer; stdcall;
  TCore_TemplatFromXml_DataElement_GetComment = function(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var splitScreen: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean): integer; stdcall;
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
    Core_TemplatFromXml_DataElement_DataItemCountFunc: TCore_TemplatFromXml_DataElement_DataItemCount;
    Core_TemplatFromXml_DataElement_GetDataItemTypeFunc: TCore_TemplatFromXml_DataElement_GetDataItemType;
    Core_TemplatFromXml_DataElement_GetPictureFunc: TCore_TemplatFromXml_DataElement_GetPicture;
    Core_TemplatFromXml_DataElement_GetShowPdfFunc: TCore_TemplatFromXml_DataElement_GetShowPdf;
    Core_TemplatFromXml_DataElement_GetDateFunc: TCore_TemplatFromXml_DataElement_GetDate;
    Core_TemplatFromXml_DataElement_GetSignatureFunc: TCore_TemplatFromXml_DataElement_GetSignature;
    Core_TemplatFromXml_DataElement_GetSaveButtonFunc: TCore_TemplatFromXml_DataElement_GetSaveButton;
    Core_TemplatFromXml_DataElement_GetTimerFunc: TCore_TemplatFromXml_DataElement_GetTimer;
    Core_TemplatFromXml_DataElement_GetNoneFunc: TCore_TemplatFromXml_DataElement_GetNone;
    Core_TemplatFromXml_DataElement_GetCheckBoxFunc: TCore_TemplatFromXml_DataElement_GetCheckBox;
    Core_TemplatFromXml_DataElement_GetMultiSelectFunc: TCore_TemplatFromXml_DataElement_GetMultiSelect;
    Core_TemplatFromXml_DataElement_GetSingleSelectFunc: TCore_TemplatFromXml_DataElement_GetSingleSelect;
    Core_TemplatFromXml_DataElement_GetNumberFunc: TCore_TemplatFromXml_DataElement_GetNumber;
    Core_TemplatFromXml_DataElement_GetTextFunc: TCore_TemplatFromXml_DataElement_GetText;
    Core_TemplatFromXml_DataElement_GetCommentFunc: TCore_TemplatFromXml_DataElement_GetComment;
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
    function Core_TemplatFromXml_DataElement_DataItemCount(n: integer): integer;
    function Core_TemplatFromXml_DataElement_GetDataItemType(n: integer; m: integer): WideString;
    procedure Core_TemplatFromXml_DataElement_GetPicture(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString);
    procedure Core_TemplatFromXml_DataElement_GetShowPdf(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var color: WideString; var value: WideString);
    procedure Core_TemplatFromXml_DataElement_GetDate(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory: boolean;
        var readonly: boolean; var color: WideString; var value: WideString);
    procedure Core_TemplatFromXml_DataElement_GetSignature(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString);
    procedure Core_TemplatFromXml_DataElement_GetSaveButton(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var value: WideString);
    procedure Core_TemplatFromXml_DataElement_GetTimer(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var stopOnSave: Boolean; var mandatory: Boolean);
    procedure  Core_TemplatFromXml_DataElement_GetNone(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer);
    procedure Core_TemplatFromXml_DataElement_GetCheckBox(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean; var selected: boolean);
    procedure Core_TemplatFromXml_DataElement_GetMultiSelect(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean);
    procedure Core_TemplatFromXml_DataElement_GetSingleSelect(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean);
    procedure Core_TemplatFromXml_DataElement_GetNumber(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory: boolean; var decimalCount: integer);
    procedure Core_TemplatFromXml_DataElement_GetText(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var geolocationEnabled: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean);
    procedure Core_TemplatFromXml_DataElement_GetComment(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var splitScreen: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean);
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

   @Core_TemplatFromXml_DataElement_DataItemCountFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_DataItemCount') ;
   if not Assigned (Core_TemplatFromXml_DataElement_DataItemCountFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_DataItemCount not found');

   @Core_TemplatFromXml_DataElement_GetDataItemTypeFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetDataItemType') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetDataItemTypeFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetDataItemType not found');

   @Core_TemplatFromXml_DataElement_GetPictureFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetPicture') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetPictureFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetPicture not found');

   @Core_TemplatFromXml_DataElement_GetShowPdfFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetShowPdf') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetShowPdfFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetShowPdf not found');

   @Core_TemplatFromXml_DataElement_GetDateFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetDate') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetDateFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetDate not found');

   @Core_TemplatFromXml_DataElement_GetSignatureFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetSignature') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetSignatureFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetSignature not found');

   @Core_TemplatFromXml_DataElement_GetSaveButtonFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetSaveButton') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetSaveButtonFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetSaveButton not found');

   @Core_TemplatFromXml_DataElement_GetTimerFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetTimer') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetTimerFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetTimer not found');

   @Core_TemplatFromXml_DataElement_GetNoneFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetNone') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetNoneFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetNone not found');

   @Core_TemplatFromXml_DataElement_GetCheckBoxFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetCheckBox') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetCheckBoxFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetCheckBox not found');

   @Core_TemplatFromXml_DataElement_GetMultiSelectFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetMultiSelect') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetMultiSelectFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetMultiSelect not found');

   @Core_TemplatFromXml_DataElement_GetSingleSelectFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetSingleSelect') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetSingleSelectFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetSingleSelect not found');

   @Core_TemplatFromXml_DataElement_GetNumberFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetNumber') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetNumberFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetNumber not found');

   @Core_TemplatFromXml_DataElement_GetTextFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetText') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetTextFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetText not found');

   @Core_TemplatFromXml_DataElement_GetCommentFunc := GetProcAddress(handle, 'Core_TemplatFromXml_DataElement_GetComment') ;
   if not Assigned (Core_TemplatFromXml_DataElement_GetCommentFunc) then
     raise Exception.Create('function Core_TemplatFromXml_DataElement_GetComment not found');

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

function TDllHelper.Core_TemplatFromXml_DataElement_DataItemCount(n: integer): integer;
var
  res: integer;
  err: WideString;
  count: integer;
begin
  count := 0;
  res := Core_TemplatFromXml_DataElement_DataItemCountFunc(n, count);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := count;

end;

function TDllHelper.Core_TemplatFromXml_DataElement_GetDataItemType(n: integer; m: integer): WideString;
var
  res: integer;
  err: WideString;
  dataItemType: WideString;
begin
  Result := '';
  res := Core_TemplatFromXml_DataElement_GetDataItemTypeFunc(n, m, dataItemType);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
  Result := dataItemType;
end;

procedure TDllHelper.Core_TemplatFromXml_DataElement_GetPicture(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetPictureFunc(n, m, id, _label, description, displayOrder,
      mandatory, color);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;


procedure TDllHelper.Core_TemplatFromXml_DataElement_GetShowPdf(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var color: WideString; var value: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetShowPdfFunc(n, m, id, _label, description, displayOrder,
       color, value);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_DataElement_GetDate(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory: boolean;
        var readonly: boolean; var color: WideString; var value: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetDateFunc(n, m, id, _label, description, displayOrder,
       minValue, maxValue, mandatory, readonly, color, value);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_DataElement_GetSignature(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var mandatory: boolean;
        var color: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetSignatureFunc(n, m, id, _label, description, displayOrder,
      mandatory, color);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_DataElement_GetSaveButton(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer; var value: WideString);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetSaveButtonFunc(n, m, id, _label, description, displayOrder, value);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_DataElement_GetTimer(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var stopOnSave: Boolean; var mandatory: Boolean);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetTimerFunc(n, m, id, _label, description, displayOrder,
    stopOnSave, mandatory);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_DataElement_GetNone(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetNoneFunc(n, m, id, _label, description, displayOrder);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_DataElement_GetCheckBox(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean; var selected: boolean);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetCheckBoxFunc(n, m, id, _label, description, displayOrder,
      mandatory, selected);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_DataElement_GetMultiSelect(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetMultiSelectFunc(n, m, id, _label, description, displayOrder,
      mandatory);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_DataElement_GetSingleSelect(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var mandatory: boolean);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetSingleSelectFunc(n, m, id, _label, description, displayOrder,
      mandatory);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;


procedure TDllHelper.Core_TemplatFromXml_DataElement_GetNumber(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var displayOrder: integer;
        var minValue: WideString; var maxValue: WideString; var mandatory: boolean; var decimalCount: integer);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetNumberFunc(n, m, id, _label, description, displayOrder, minValue,
      maxValue, mandatory, decimalCount);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_DataElement_GetText(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var geolocationEnabled: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean);
var
  res: integer;
  err: WideString;
begin
  res := Core_TemplatFromXml_DataElement_GetTextFunc(n, m, id, _label, description, geolocationEnabled,
      value, readOnly, mandatory);
  if res <> 0 then
  begin
     err := LastErrorFunc;
     raise Exception.Create(err);
  end;
end;

procedure TDllHelper.Core_TemplatFromXml_DataElement_GetComment(n: integer; m: integer; var id: integer;
        var _label: WideString; var description: WideString; var splitScreen: boolean;
        var value: WideString; var readOnly: boolean; var mandatory: boolean);
var
  res: integer;
  err: WideString;
begin
   res := Core_TemplatFromXml_DataElement_GetCommentFunc(n, m, id, _label, description, splitScreen,
      value, readOnly, mandatory);
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


