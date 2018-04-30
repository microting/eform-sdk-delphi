unit Classes;

interface

uses Xml.XMLDoc, Generics.Collections, SysUtils;

type

  {$region 'TEntityItemUpdateInfo declaration'}
  TEntityItemUpdateInfo = class
  public
      EntityItemMUid: string;
      Status: string;

      constructor Create(entityItemMUid: string; status: string);
  end;
  {$endregion}

  {$region 'TExceptionClass declaration'}
  TExceptionClass = class
  public
     Description: string;
     Time: TDateTime;
     Occurrence: integer;

     constructor Create;overload;
     constructor Create(description: string; time: TDateTime);overload;
  end;
  {$endregion}

  {$region 'THolder declaration'}
  THolder = class
  public
      Index: integer;
      FieldType: string;

      constructor Create(index: integer; fieldType: string);
  end;
  {$endregion}

  {$region 'TTag declaration'}
  TTag = class
  public
      Id: integer;
      Name: string;
      TaggingCount: integer;

      constructor Create(id: integer; name: string; taggingCount: integer);
  end;
  {$endregion}

  {$region 'TKeyValuePair declaration'}
  TKeyValuePair = class
  private
    constructor Create; overload;
  public
     Key: string;
     Value: string;
     Selected: boolean;
     DisplayOrder: string;

     constructor Create(key: string; value: string; selected: boolean; displayOrder: string); overload;
  end;
  {$endregion}

  {$region 'TSiteName_Dto declaration'}
  TSiteName_Dto = class
  public
      SiteUId: integer;
      SiteName: string;
      CreatedAt: TDateTime;
      UpdatedAt: TDateTime;

      constructor Create; overload;
      constructor Create(siteUId: integer; siteName: string; createdAt: TDateTime; updatedAt: TDateTime);overload;
      function ToString: string; override;
  end;
  {$endregion}

  {$region 'TField_Dto declaration'}
  TField_Dto = class
  public
      Id: integer;
      _Label: string;
      Description: string;
      FieldTypeId: integer;
      FieldType: string;
      CheckListId: integer;

      constructor Create; overload;
      constructor Create(id: integer; _label: string; description: string; fieldTypeId: integer;
          fieldType: string; checkListId: integer);overload;
  end;
  {$endregion}

  {$region 'TTemplate_Dto declaration'}
  TTemplate_Dto = class
  public
      Id: integer;
      CreatedAt: TDateTime;
      UpdatedAt: TDateTime;
      _Label: string;
      Descripition: string;
      Repeated: integer;
      FolderName: string;
      WorkflowState: string;
      DeployedSites: TObjectList<TSiteName_Dto>;
      HasCases: boolean;
      DisplayIndex: integer;
      Field1: TField_Dto;
      Field2: TField_Dto;
      Field3: TField_Dto;
      Field4: TField_Dto;
      Field5: TField_Dto;
      Field6: TField_Dto;
      Field7: TField_Dto;
      Field8: TField_Dto;
      Field9: TField_Dto;
      Field10: TField_Dto;
      Tags: TObjectList<TKeyValuePair>;

      constructor Create; overload;
      constructor Create(id: integer; createdAt: TDateTime; updatedAt: TDateTime; _label: string;
          description: string; repeated: integer; folderName: string; workflowState: string;
          deployedSites: TObjectList<TSiteName_Dto>; hasCases: boolean; displayIndex: integer;
          tags: TObjectList<TKeyValuePair>);overload;

      constructor Create(id: integer; createdAt: TDateTime; updatedAt: TDateTime; _label: string;
          description: string; repeated: integer; folderName: string; workflowState: string;
          deployedSites: TObjectList<TSiteName_Dto>; hasCases: boolean; displayIndex: integer;
          field1: TField_Dto; field2: TField_Dto; field3: TField_Dto; field4: TField_Dto;
          field5: TField_Dto; field6: TField_Dto; field7: TField_Dto; field8: TField_Dto;
          field9: TField_Dto; field10: TField_Dto; tags: TObjectList<TKeyValuePair>);overload;
  end;
 {$endregion}

  {$region 'TCase'}
  TCase = class
  public
      Id: integer;
      WorkflowState: string;
      Version: integer;
      Status: integer;
      CreatedAt: TDateTime;
      UpdatedAt: TDateTime;
      DoneAt: TDateTime;
      SiteName: string;
      UnitId: integer;
      WorkerName: string;
      TemplatId: integer;
      CaseType: string;
      MicrotingUId: string;
      CheckUIid: string;
      CaseUId: string;
      FieldValue1: string;
      FieldValue2: string;
      FieldValue3: string;
      FieldValue4: string;
      FieldValue5: string;
      FieldValue6: string;
      FieldValue7: string;
      FieldValue8: string;
      FieldValue9: string;
      FieldValue10: string;
  end;
  {$endregion}

  {$region 'TCase_Dto declaration'}
  TCase_Dto = class
  public
      CaseId: integer;
      Stat: string;
      SiteUId: integer;
      CaseType: string;
      CaseUId: string;
      MicrotingUId: string;
      CheckUId: string;
      Custom: string;
      CheckListId: integer;
      WorkflowState: string;

      constructor Create; overload;
      constructor Create(caseId: integer; stat: string; siteUId: integer; caseType: string; caseUId: string;
    microtingUId: string; checkUId: string; custom: string; checkListId: integer; workflowState: string);overload;
      function ToString: string; override;
  end;

  {$region 'TNote_Dto declaration'}
  TNote_Dto = class
  public
      Id: string;
      MicrotingUId: string;
      Activity: string;

      constructor Create; overload;
      constructor Create(id: string; microtingUId: string; activity: string);overload;
      function ToString: string; override;
  end;
  {$endregion}

  {$region 'TWorker_Dto declaration'}
  TWorker_Dto = class
  public
      WorkerUId: integer;
      FirstName: string;
      LastName: string;
      Email: string;
      CreatedAt: TDateTime;
      UpdatedAt: TDateTime;

      constructor Create; overload;
      constructor Create(workerUId: integer; firstName: string; lastName: string; email: string;
          createdAt: TDateTime; updatedAt: TDateTime);overload;
      function ToString: string; override;
  end;
  {$endregion}

  {$region 'TCDataValue declaration'}
  TCDataValue = class
  private
     function GetDataWrapper: TXMLNodeCollection;
     procedure SetDataWrapper(Value: TXMLNodeCollection);

  public
      InderValue: string;

      property DataWrapper: TXMLNodeCollection read GetDataWrapper write SetDataWrapper;
  end;
  {$endregion}


implementation

{$region 'TSiteName_Dto implementation'}
constructor TSiteName_Dto.Create;
begin
end;

constructor TSiteName_Dto.Create(siteUId: integer; siteName: string; createdAt: TDateTime; updatedAt: TDateTime);
begin
   self.SiteUId := siteUId;
   self.SiteName := siteName;
   self.CreatedAt := createdAt;
   self.UpdatedAt := updatedAt;
end;

function TSiteName_Dto.ToString: string;
begin
  Result := 'SiteUId:'+ IntToStr(SiteUId) + ' / SiteName:' + SiteName + ' / CreatedAt:' +
    DateToStr(CreatedAt) + ' / UpdatedAt:' + DateToStr(UpdatedAt) + '.';
end;
{$endregion}

{$region 'TField_Dto implementation'}
constructor TField_Dto.Create;
begin
end;

constructor TField_Dto.Create(id: integer; _label: string; description: string; fieldTypeId: integer;
    fieldType: string; checkListId: integer);
begin
   self.Id := id;
   self._Label := _label;
   self.Description := description;
   self.FieldTypeId := fieldTypeId;
   self.FieldType := fieldType;
   self.CheckListId := checkListId;
end;
{$endregion}

{$region 'TTemplate_Dto implementation'}
constructor TTemplate_Dto.Create;
begin
end;

constructor TTemplate_Dto.Create(id: integer; createdAt: TDateTime; updatedAt: TDateTime; _label: string;
    description: string; repeated: integer; folderName: string; workflowState: string;
    deployedSites: TObjectList<TSiteName_Dto>; hasCases: boolean; displayIndex: integer;
    tags: TObjectList<TKeyValuePair>);
begin
  self.Id := id;
  self.CreatedAt := createdAt;
  self.UpdatedAt := updatedAt;
  self._Label := _label;
  self.Descripition := description;
  self.Repeated := repeated;
  self.FolderName := folderName;
  self.WorkflowState := workflowState;
  self.DeployedSites := deployedSites;
  self.HasCases := hasCases;
  self.DisplayIndex := displayIndex;
  self.Tags := tags;
end;

constructor TTemplate_Dto.Create(id: integer; createdAt: TDateTime; updatedAt: TDateTime; _label: string;
        description: string; repeated: integer; folderName: string; workflowState: string;
        deployedSites: TObjectList<TSiteName_Dto>; hasCases: boolean; displayIndex: integer;
        field1: TField_Dto; field2: TField_Dto; field3: TField_Dto; field4: TField_Dto;
        field5: TField_Dto; field6: TField_Dto; field7: TField_Dto; field8: TField_Dto;
        field9: TField_Dto; field10: TField_Dto; tags: TObjectList<TKeyValuePair>);
begin
    self.Id := id;
    self.CreatedAt := createdAt;
    self.UpdatedAt := updatedAt;
    self._Label := _label;
    self.Descripition := description;
    self.Repeated := repeated;
    self.FolderName := folderName;
    self.WorkflowState := workflowState;
    self.DeployedSites := deployedSites;
    self.HasCases := hasCases;
    self.DisplayIndex := displayIndex;
    self.Field1 := field1;
    self.Field2 := field2;
    self.Field3 := field3;
    self.Field4 := field4;
    self.Field5 := field5;
    self.Field6 := field6;
    self.Field7 := field7;
    self.Field8 := field8;
    self.Field9 := field9;
    self.Field10 := field10;
    self.Tags := tags;
end;
{$endregion}

{$region 'TCDataValue implementation'}
function TCDataValue.GetDataWrapper: TXMLNodeCollection;
begin
    Result := nil;
end;

procedure TCDataValue.SetDataWrapper(Value: TXMLNodeCollection);
begin
    if Value = nil then
    begin
      InderValue := '';
      exit;
    end;

end;
{$endregion}

{$region 'TKeyValuePair implementaton'}
constructor TKeyValuePair.Create;
begin
end;

constructor TKeyValuePair.Create(key: string; value: string; selected: boolean; displayOrder: string);
begin
  inherited Create;
  self.Key := key;
  self.Value := value;
  self.Selected := selected;
  self.DisplayOrder := displayOrder;
end;

{$endregion}

{$region 'TEntityItemUpdateInfo implementation'}
constructor TEntityItemUpdateInfo.Create(entityItemMUid: string; status: string);
begin
    self.EntityItemMUid := entityItemMUid;
    self.Status := status;
end;
{$endregion}

{$region 'TExceptionClass implementation'}
constructor TExceptionClass.Create;
begin
   self.Description := '';
   self.Time := Now;
   self.Occurrence := 1;
end;

constructor TExceptionClass.Create(description: string; time: TDateTime);
begin
   self.Description := description;
   self.Time := time;
   self.Occurrence := 1;
end;
{$endregion}

{$region 'TTag implementation'}
constructor TTag.Create(id: integer; name: string; taggingCount: integer);
begin
  Self.Id := id;
  Self.Name := name;
  self.TaggingCount := taggingCount;
end;
{$endregion}

{$region 'THolder implementation'}
constructor THolder.Create(index: integer; fieldType: string);
begin
  Self.Index := index;
  Self.FieldType := fieldType;
end;
{$endregion}

{$region 'TNote_Dto implementation'}
constructor TNote_Dto.Create;
begin
end;

constructor TNote_Dto.Create(id: string; microtingUId: string; activity: string);
begin
  self.Id := id;
  self.MicrotingUId := microtingUId;
  self.Activity := activity;
end;

function TNote_Dto.ToString: string;
begin
  Result := 'Id:' + Id + ' / MicrotingUId:' + MicrotingUId + ' / Activity:' + Activity + '.';
end;
{$endregion}

{$region 'TWorker_Dto implementation'}
constructor TWorker_Dto.Create;
begin
end;

constructor TWorker_Dto.Create(workerUId: integer; firstName: string; lastName: string; email: string;
      createdAt: TDateTime; updatedAt: TDateTime);
begin
    self.WorkerUId := workerUId;
    self.FirstName := firstName;
    self.LastName := lastName;
    self.Email := email;
    self.CreatedAt := createdAt;
    self.UpdatedAt := updatedAt;
end;

function TWorker_Dto.ToString: string;
begin
    Result := 'WorkerUId:' + IntToStr(WorkerUId) + ' / FirstName:' + FirstName + ' / LastName:' + LastName
        + ' / Email:' + Email + ' / CreatedAt:' + DateToStr(CreatedAt) + ' / UpdatedAt:'
        + DateToStr(UpdatedAt) + '.';
end;

{$endregion}

{$region 'TCase_Dto implementation'}
constructor TCase_Dto.Create;
begin
end;


constructor TCase_Dto.Create(caseId: integer; stat: string; siteUId: integer; caseType: string; caseUId: string;
    microtingUId: string; checkUId: string; custom: string; checkListId: integer; workflowState: string);
begin
  self.CaseId := caseId;
  self.Stat := stat;
  self.SiteUId := siteUId;
  self.CaseType := caseType;
  self.CaseUId := caseUId;
  self.MicrotingUId := microtingUId;
  self.CheckUId := checkUId;
  self.Custom := custom;
  self.CheckListId := checkListId;
  self.WorkflowState := workflowState;
end;

function TCase_Dto.ToString: string;
begin
  Result := 'CaseId:' + IntToStr(CaseId) + ' / Stat:' + Stat + ' / SiteUId:' + IntToStr(SiteUId) + ' / CaseType:'
     + CaseType + '/  CaseUId:' + CaseUId + ' / MicrotingUId:' + MicrotingUId + ' / CheckId:' + CheckUId + '.';

end;
{$endregion}

end.
