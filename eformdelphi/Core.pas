unit Core;

interface

uses
  DllHelper, EEvents, SysUtils, MainElement, Element, Generics.Collections, Classes, DataItem, DataItemGroup,
    FieldContainer, Packer, System.Classes;

type
  {$region 'TCore declaration'}
  TCore = class
  private
    FCaseCreatedEvent: TCaseCreatedEvent;
    FCaseCompletedEvent: TCaseCompletedEvent;
    FCaseDeletedEvent: TCaseDeletedEvent;
    FCaseRetrivedEvent: TCaseRetrivedEvent;
    FEventExceptionEvent: TEventExceptionEvent;
    FSiteActivatedEvent: TSiteActivatedEvent;
    FFileDownloadedEvent: TFileDownloadedEvent;
    FNotificationNotFoundEvent: TNotificationNotFoundEvent;

    procedure SetCaseCreatedEvent(Value: TCaseCreatedEvent);
    procedure SetCaseCompletedEvent(Value: TCaseCompletedEvent);
    procedure SetCaseDeletedEvent(Value: TCaseDeletedEvent);
    procedure SetCaseRetrivedEvent(Value: TCaseRetrivedEvent);
    procedure SetEventExceptionEvent(Value: TEventExceptionEvent);
    procedure SetSiteActivatedEvent(Value: TSiteActivatedEvent);
    procedure SetFileDownloadedEvent(Value: TFileDownloadedEvent);
    procedure SetNotificationNotFoundEvent(Value: TNotificationNotFoundEvent);

  public
    constructor Create;
    function Start(serverConnectionString: string): boolean;
    function StartSqlOnly(serverConnectionString: string): boolean;
    function TemplatFromXml(xml: string): TMainElement;
    function TemplateCreate(mainElement: TMainElement): integer;
    function TemplateRead(templateId: integer): TMainElement;
    function TemplateValidation(mainElement: TMainElement): TStringList;
    function TemplateDelete(templateId: integer): boolean;
    function TemplateUploadData(mainElement: TMainElement): TMainElement;
    function Advanced_SiteItemRead(siteId: integer): TSiteName_Dto;
    function Advanced_SiteItemReadAll: TObjectList<TSiteName_Dto>;
    function TemplateItemRead(templateId: integer): TTemplate_Dto;
    function TemplateItemReadAll(includeRemoved: boolean): TObjectList<TTemplate_Dto>;
    function CaseCreate(mainElement: TMainElement; caseUId: string; siteUId: integer): string; overload;
    function CaseCreate(mainElement: TMainElement; caseUId: string; siteUIds: TList<integer>;
        custom: string): TStringList; overload;
    function CaseRead(microtingUId: string; checkUId: string): TReplyElement;
    function CaseReadAll(templateId: integer; start: TDateTime; _end: TDateTime): TObjectList<TCase>;
    function CaseReadByCaseId(caseId: integer): TCase_Dto;
    function CaseDelete(microtingUId: string): boolean; overload;
    function CaseDelete(templateId: integer; siteUId: integer): boolean; overload;
    function CaseUpdate(caseId: integer; newFieldValuePairLst: TStringList;
       newCheckListValuePairLst: TStringlist): boolean;
    function Advanced_TemplateDisplayIndexChangeDb(templateId: integer; displayIndex: integer): boolean;
    function Advanced_TemplateDisplayIndexChangeServer(templateId: integer; siteUId: integer;
       displayIndex: integer): boolean;
    function CasesToCsv(templateId: integer; start: TDateTime; _end: TDateTime; pathAndName: string;
      customPathForUploadedData: string): string;

    procedure OnCaseCreatedInternal(jsonCaseDto: WideString);
    procedure OnCaseCompletedInternal(jsonCaseDto: WideString);
    procedure OnCaseDeletedInternal(jsonCaseDto: WideString);
    procedure OnCaseRetrivedInternal(jsonCaseDto: WideString);
    procedure OnEventExceptionInternal(error: WideString);
    procedure OnSiteActivatedInternal(siteId: integer);
    procedure OnFileDownloadedInternal(jsonFileDto: WideString);
    procedure OnNotificationNotFoundInternal(jsonNoteDto: WideString);

    property HandleCaseCreated: TCaseCreatedEvent read FCaseCreatedEvent write SetCaseCreatedEvent;
    property HandleCaseCompleted: TCaseCompletedEvent read FCaseCompletedEvent write SetCaseCompletedEvent;
    property HandleCaseDeleted: TCaseDeletedEvent read FCaseDeletedEvent write SetCaseDeletedEvent;
    property HandleCaseRetrived: TCaseRetrivedEvent read FCaseRetrivedEvent write SetCaseRetrivedEvent;
    property HandleEventException: TEventExceptionEvent read FEventExceptionEvent write SetEventExceptionEvent;
    property HandleSiteActivated: TSiteActivatedEvent read FSiteActivatedEvent write SetSiteActivatedEvent;
    property HandleFileDownloaded: TFileDownloadedEvent read FFileDownloadedEvent write SetFileDownloadedEvent;
    property HandleNotificationNotFound: TNotificationNotFoundEvent read FNotificationNotFoundEvent
      write SetNotificationNotFoundEvent;

  end;
  {$endregion}

implementation

var
  gCore: TCore;

{$region 'TCore implementation'}
constructor TCore.Create;
begin
  inherited Create;
  TDllHelper.GetInstance.Core_Create;
  gCore := self;
end;

function TCore.Start(serverConnectionString: string): boolean;
var
  startResult: boolean;
begin
  TDllHelper.GetInstance.Core_Start(serverConnectionString, startResult);
  Result := startResult;
end;

function TCore.StartSqlOnly(serverConnectionString: string): boolean;
var
  startResult: boolean;
begin
  TDllHelper.GetInstance.Core_StartSqlOnly(serverConnectionString, startResult);
  Result := startResult;
end;

procedure TCore.OnCaseCreatedInternal(jsonCaseDto: WideString);
var
  packer: TPacker;
  caseDto: TCase_Dto;
begin
  //WriteLn('Result: ' + jsonCaseDto);
  packer := TPacker.Create;
  caseDto := packer.UnpackCaseDto(jsonCaseDto);
  if Assigned(FCaseCreatedEvent) then
       FCaseCreatedEvent(caseDto);
end;

procedure OnCaseCreated(jsonCaseDto: WideString); stdcall;
begin
  gCore.OnCaseCreatedInternal(jsonCaseDto);
end;

procedure TCore.SetCaseCreatedEvent(Value: TCaseCreatedEvent);
begin
   FCaseCreatedEvent := Value;
   TDllHelper.GetInstance.Core_HandleCaseCreated(LongInt(@OnCaseCreated));
end;

procedure TCore.OnCaseCompletedInternal(jsonCaseDto: WideString);
var
  packer: TPacker;
  caseDto: TCase_Dto;
begin
  //WriteLn('Result: ' + jsonCaseDto);
  packer := TPacker.Create;
  caseDto := packer.UnpackCaseDto(jsonCaseDto);
  if Assigned(FCaseCompletedEvent) then
       FCaseCompletedEvent(caseDto);
end;

procedure OnCaseCompleted(jsonCaseDto: WideString); stdcall;
begin
  gCore.OnCaseCompletedInternal(jsonCaseDto);
end;

procedure TCore.SetCaseCompletedEvent(Value: TCaseCompletedEvent);
begin
   FCaseCompletedEvent := Value;
   TDllHelper.GetInstance.Core_HandleCaseCompleted(LongInt(@OnCaseCompleted));
end;

procedure TCore.OnCaseDeletedInternal(jsonCaseDto: WideString);
var
  packer: TPacker;
  caseDto: TCase_Dto;
begin
  //WriteLn('Result: ' + jsonCaseDto);
  packer := TPacker.Create;
  caseDto := packer.UnpackCaseDto(jsonCaseDto);
  if Assigned(FCaseDeletedEvent) then
       FCaseDeletedEvent(caseDto);
end;


procedure OnCaseDeleted(jsonCaseDto: WideString); stdcall;
begin
  gCore.OnCaseDeletedInternal(jsonCaseDto);
end;

procedure TCore.SetCaseDeletedEvent(Value: TCaseDeletedEvent);
begin
   FCaseDeletedEvent := Value;
   TDllHelper.GetInstance.Core_HandleCaseDeleted(LongInt(@OnCaseDeleted));
end;

procedure TCore.OnCaseRetrivedInternal(jsonCaseDto: WideString);
var
  packer: TPacker;
  caseDto: TCase_Dto;
begin
  packer := TPacker.Create;
  caseDto := packer.UnpackCaseDto(jsonCaseDto);
  if Assigned(FCaseRetrivedEvent) then
       FCaseRetrivedEvent(caseDto);
end;


procedure OnCaseRetrived(jsonCaseDto: WideString); stdcall;
begin
  gCore.OnCaseRetrivedInternal(jsonCaseDto);
end;

procedure TCore.SetCaseRetrivedEvent(Value: TCaseRetrivedEvent);
begin
   FCaseRetrivedEvent := Value;
   TDllHelper.GetInstance.Core_HandleCaseRetrived(LongInt(@OnCaseRetrived));
end;

procedure TCore.OnEventExceptionInternal(error: WideString);
begin
  if Assigned(FEventExceptionEvent) then
      FEventExceptionEvent(error);
end;

procedure OnEventException(error: WideString); stdcall;
begin
  gCore.OnEventExceptionInternal(error);
end;

procedure TCore.SetEventExceptionEvent(Value: TEventExceptionEvent);
begin
   FEventExceptionEvent := Value;
   TDllHelper.GetInstance.Core_HandleEventException(LongInt(@OnEventException));
end;

procedure TCore.OnSiteActivatedInternal(siteId: integer);
begin
  if Assigned(FSiteActivatedEvent) then
     FSiteActivatedEvent(siteId);
end;

procedure OnSiteActivated(siteId: integer); stdcall;
begin
  gCore.OnSiteActivatedInternal(siteId);
end;

procedure TCore.SetSiteActivatedEvent(Value: TSiteActivatedEvent);
begin
   FSiteActivatedEvent := Value;
   TDllHelper.GetInstance.Core_HandleSiteActivated(LongInt(@OnSiteActivated));
end;


procedure TCore.OnFileDownloadedInternal(jsonFileDto: WideString);
var
  packer: TPacker;
  fileDto: TFile_Dto;
begin
  packer := TPacker.Create;
  fileDto := packer.UnpackFileDto(jsonFileDto);
  if Assigned(FFileDownloadedEvent) then
       FFileDownloadedEvent(fileDto);
end;

procedure OnFileDownloaded(jsonFileDto: WideString);
begin
  gCore.OnFileDownloadedInternal(jsonFileDto);
end;

procedure TCore.SetFileDownloadedEvent(Value: TFileDownloadedEvent);
begin
   FFileDownloadedEvent := Value;
   TDllHelper.GetInstance.Core_HandleFileDownloaded(LongInt(@OnFileDownloaded));
end;

procedure TCore.OnNotificationNotFoundInternal(jsonNoteDto: WideString);
var
  packer: TPacker;
  noteDto: TNote_Dto;
begin
  packer := TPacker.Create;
  noteDto := packer.UnpackNoteDto(jsonNoteDto);
  if Assigned(FNotificationNotFoundEvent) then
       FNotificationNotFoundEvent(noteDto);
end;

procedure OnNotificationNotFound(jsonNoteDto: WideString);
begin
  gCore.OnNotificationNotFoundInternal(jsonNoteDto);
end;

procedure TCore.SetNotificationNotFoundEvent(Value: TNotificationNotFoundEvent);
begin
   FNotificationNotFoundEvent := Value;
   TDllHelper.GetInstance.Core_HandleNotificationNotFound(LongInt(@OnNotificationNotFound));
end;


function TCore.TemplatFromXml(xml: string): TMainElement;
var
  jsonString: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  TDllHelper.GetInstance.Core_TemplateFromXml(xml, jsonString);
  result :=  packer.UnpackMainElement(jsonString);
end;

function TCore.TemplateCreate(mainElement: TMainElement): integer;
var
  jsonString: string;
  packer: TPacker;
begin
  packer := TPacker.Create;
  jsonString := packer.Pack(mainElement);
  result := TDllHelper.GetInstance.Core_TemplateCreate(jsonString);
end;

function TCore.TemplateRead(templateId: integer): TMainElement;
var
  jsonString: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  TDllHelper.GetInstance.Core_TemplateRead(templateId, jsonString);
  result :=  packer.UnpackMainElement(jsonString);
end;

function TCore.TemplateValidation(mainElement: TMainElement): TStringList;
var
  jsonMainElement: string;
  jsonValidation: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  jsonMainElement := packer.Pack(mainElement);
  TDllHelper.GetInstance.Core_TemplateValidation(jsonMainElement, jsonValidation);
  result := packer.UnpackStringList(jsonValidation);
end;

function TCore.TemplateDelete(templateId: integer): boolean;
var
  deleteResult: boolean;
begin
  TDllHelper.GetInstance.Core_TemplateDelete(templateId, deleteResult);
  result := deleteResult;
end;

function TCore.TemplateUploadData(mainElement: TMainElement): TMainElement;
var
  jsonMainElementIn: string;
  jsonMainElementOut: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  jsonMainElementIn := packer.Pack(mainElement);
  TDllHelper.GetInstance.Core_TemplateUploadData(jsonMainElementIn, jsonMainElementOut);
  result := packer.UnpackMainElement(jsonMainElementOut);
end;


function TCore.Advanced_SiteItemRead(siteId: integer): TSiteName_Dto;
var
  json: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  TDllHelper.GetInstance.Core_Advanced_SiteItemRead(siteId, json);
  result := packer.UnpackSiteNameDto(json);
end;

function TCore.Advanced_SiteItemReadAll: TObjectList<TSiteName_Dto>;
var
  json: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  TDllHelper.GetInstance.Core_Advanced_SiteItemReadAll(json);
  result := packer.UnpackSiteNameDtoList(json);
end;

function TCore.TemplateItemRead(templateId: integer): TTemplate_Dto;
var
  json: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  TDllHelper.GetInstance.Core_TemplateItemRead(templateId, json);
  result :=  packer.UnpackTemplateDto(json);
end;


function TCore.TemplateItemReadAll(includeRemoved: boolean):  TObjectList<TTemplate_Dto>;
var
  json: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  TDllHelper.GetInstance.Core_TemplateItemReadAll(includeRemoved, json);
  result :=  packer.UnpackTemplateDtoList(json);
end;


function TCore.CaseCreate(mainElement: TMainElement; caseUId: string; siteUId: integer): string;
var
  jsonMainElement: WideString;
  resultCase: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  jsonMainElement := packer.Pack(mainElement);
  TDllHelper.GetInstance.Core_CaseCreate(jsonMainElement, caseUId, siteUId, resultCase);
  result := resultCase;
end;

function  TCore.CaseCreate(mainElement: TMainElement; caseUId: string; siteUIds: TList<integer>;
    custom: string): TStringList;
var
  jsonMainElement: WideString;
  jsonSiteUIds: WideString;
  jsonResultCases: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  jsonMainElement := packer.Pack(mainElement);
  jsonSiteUIds := packer.PackIntegerList(siteUIds);
  TDllHelper.GetInstance.Core_CaseCreate(jsonMainElement, caseUId, jsonSiteUIds, custom, jsonResultCases);
  result := packer.UnpackStringList(jsonResultCases);
end;


function  TCore.CaseRead(microtingUId: string; checkUId: string): TReplyElement;
var
  jsonReplyElement: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  TDllHelper.GetInstance.Core_CaseRead(microtingUId, checkUId, jsonReplyElement);
  result := packer.UnpackReplyElement(jsonReplyElement);
end;

function TCore.CaseReadAll(templateId: integer; start: TDateTime; _end: TDateTime): TObjectList<TCase>;
var
  jsonCases: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  TDllHelper.GetInstance.Core_CaseReadAll(templateId, FormatDateTime('yyyy-MM-dd', start), FormatDateTime('yyyy-MM-dd', _end),
     jsonCases);
  result := packer.UnpackCasesList(jsonCases);
end;

function TCore.CaseReadByCaseId(caseId: integer): TCase_Dto;
var
  jsonCaseDto: WideString;
  packer: TPacker;
begin
  packer := TPacker.Create;
  TDllHelper.GetInstance.Core_CaseReadByCaseId(caseId, jsonCaseDto);
  result := packer.UnpackCaseDto(jsonCaseDto);
end;

function  TCore.CaseDelete(microtingUId: string): boolean;
var
  deleteResult: boolean;
begin
  TDllHelper.GetInstance.Core_CaseDelete(microtingUId, deleteResult);
  result := deleteResult;
end;

function  TCore.CaseDelete(templateId: integer; siteUId: integer): boolean;
var
  deleteResult: boolean;
begin
  TDllHelper.GetInstance.Core_CaseDelete(templateId, siteUId, deleteResult);
  result := deleteResult;
end;

function TCore.CaseUpdate(caseId: integer; newFieldValuePairLst: TStringList;
       newCheckListValuePairLst: TStringlist): boolean;
var
  updateResult: boolean;
  packer: TPacker;
  jsonNewFieldValuePairLst: WideString;
  jsonNewCheckListValuePairLst: WideString;

begin
  packer := TPacker.Create;
  jsonNewFieldValuePairLst := packer.PackStringList(newFieldValuePairLst);
  jsonNewCheckListValuePairLst := packer.PackStringList(newCheckListValuePairLst);
  TDllHelper.GetInstance.Core_CaseUpdate(caseId, jsonNewFieldValuePairLst, jsonNewCheckListValuePairLst,
    updateResult);
  result := updateResult;
end;


function TCore.Advanced_TemplateDisplayIndexChangeDb(templateId: integer; displayIndex: integer): boolean;
var
  changeResult: boolean;
begin
  TDllHelper.GetInstance.Core_Advanced_TemplateDisplayIndexChangeDb(templateId, displayIndex, changeResult);
  result := changeResult;
end;

function TCore.Advanced_TemplateDisplayIndexChangeServer(templateId: integer; siteUId: integer;
       displayIndex: integer): boolean;
var
  changeResult: boolean;
begin
  TDllHelper.GetInstance.Core_Advanced_TemplateDisplayIndexChangeServer(templateId, siteUId,
    displayIndex, changeResult);
  result := changeResult;
end;

function TCore.CasesToCsv(templateId: integer; start: TDateTime; _end: TDateTime; pathAndName: string;
      customPathForUploadedData: string): string;
var
  csvResult: WideString;
begin
  TDllHelper.GetInstance.Core_CasesToCsv(templateId, FormatDateTime('yyyy-MM-dd', start),
      FormatDateTime('yyyy-MM-dd', _end), pathAndName,  customPathForUploadedData, csvResult);
  result := csvResult;
end;


{$endregion}




end.
