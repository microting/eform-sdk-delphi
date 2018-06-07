unit Core;

interface

uses
  DllHelper, Events, SysUtils, MainElement, Element, Generics.Collections, Classes, DataItem, DataItemGroup,
    FieldContainer, Packer, System.Classes;

type
  {$region 'TCore declaration'}
  TCore = class
  private
    FCaseCreatedEvent: TCaseCreatedEvent;
    procedure SetCaseCreatedEvent(Value: TCaseCreatedEvent);

  public
    constructor Create;
    procedure Start(serverConnectionString: string);
    function TemplatFromXml(xml: string): TMainElement;
    function TemplateCreate(mainElement: TMainElement): integer;
    function TemplateRead(templateId: integer): TMainElement;
    function TemplateValidation(mainElement: TMainElement): TStringList;
    function TemplateDelete(templateId: integer): boolean;
    function TemplateUploadData(mainElement: TMainElement): TMainElement;
    function Advanced_SiteItemReadAll: TObjectList<TSiteName_Dto>;
    function TemplateItemRead(templateId: integer): TTemplate_Dto;
    function CaseCreate(mainElement: TMainElement; caseUId: string; siteUId: integer): string; overload;
    function CaseCreate(mainElement: TMainElement; caseUId: string; siteUIds: TList<integer>;
        custom: string): TStringList; overload;
    function  CaseRead(microtingUId: string; checkUId: string): TReplyElement;
    function  CaseDelete(microtingUId: string): boolean;

    procedure OnCaseCreatedInternal(jsonCaseDto: WideString);

    property HandleCaseCreated: TCaseCreatedEvent read FCaseCreatedEvent write SetCaseCreatedEvent;
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

procedure TCore.Start(serverConnectionString: string);
begin
  TDllHelper.GetInstance.Core_Start(serverConnectionString);
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

function  TCore.CaseDelete(microtingUId: string ): boolean;
var
  deleteResult: boolean;
begin
  TDllHelper.GetInstance.Core_CaseDelete(microtingUId, deleteResult);
  result := deleteResult;
end;

{$endregion}




end.
