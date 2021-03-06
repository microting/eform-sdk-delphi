unit Packer;

interface

uses
  System.JSON, REST.Json, DataItem, Element, MainElement, Generics.Collections, Classes, SysUtils,
  FieldContainer, DataItemGroup, System.Classes;

type
  TPacker = class
  private
      function Pack(elementList: TObjectList<TElement>): TJSONArray; overload;
      function Pack(dataItemList: TObjectList<TDataItem>): TJSONArray; overload;
      function Pack(picture: TPicture): TJSONObject; overload;
      function Pack(signature: TSignature): TJSONObject; overload;
      function Pack(date: TDate): TJSONObject; overload;
      function Pack(showPdf: TShowPdf): TJSONObject; overload;
      function Pack(cdata: TCDataValue): TJSONObject; overload;
      function Pack(saveButton: TSaveButton): TJSONObject; overload;
      function Pack(timer: TTimer): TJSONObject; overload;
      function Pack(none: TNone): TJSONObject; overload;
      function Pack(checkBox: TCheckBox): TJSONObject; overload;
      function Pack(multiSelect: TMultiSelect): TJSONObject; overload;
      function Pack(singleSelect: TSingleSelect): TJSONObject; overload;
      function Pack(number: TNumber): TJSONObject; overload;
      function Pack(text: TText): TJSONObject; overload;
      function Pack(comment: TComment): TJSONObject; overload;
      function Pack(keyValuePairList: TObjectList<TKeyValuePair>): TJSONArray; overload;
      function Pack(fieldContainer: TFieldContainer): TJSONObject; overload;

      function UnpackPicture(obj: TJSONObject): TPicture;
      function UnpackSaveButton(obj: TJSONObject): TSaveButton;
      function UnpackField(obj: TJSONObject): TField;
      function UnpackSignature(obj: TJSONObject): TSignature;
      function UnpackDate(obj: TJSONObject): TDate;
      function UnpackShowPdf(obj: TJSONObject): TShowPdf;
      function UnpackTimer(obj: TJSONObject): TTimer;
      function UnpackNone(obj: TJSONObject): TNone;
      function UnpackCheckBox(obj: TJSONObject): TCheckBox;
      function UnpackMultiSelect(obj: TJSONObject): TMultiSelect;
      function UnpackSingleSelect(obj: TJSONObject): TSingleSelect;
      function UnpackNumber(obj: TJSONObject): TNumber;
      function UnpackText(obj: TJSONObject): TText;
      function UnpackComment(obj: TJSONObject): TComment;
      function UnpackKeyValuePairList(arr: TJSONArray): TObjectList<TKeyValuePair>;
      function UnpackKeyValuePair(obj: TJSONObject): TKeyValuePair;
      function UnpackFieldContainer(obj: TJSONObject): TFieldContainer;
      function UnpackFieldValues(arr: TJSONArray): TObjectList<TFieldValue>;
      function UnpackDataItemGroupList(arr: TJSONArray): TObjectList<TDataItemGroup>;
      function UnpackDataItemGroup(obj: TJSONObject): TDataItemGroup;
      function UnpackDataItemList(arr: TJSONArray): TObjectList<TDataItem>;
      function UnpackElementList(arr: TJSONArray): TObjectList<TElement>;
      function UnpackDataElement(obj: TJSONObject): TDataElement;
      function UnpackCheckListValue(obj: TJSONObject): TCheckListValue;
      function UnpackSiteNameDto(obj: TJSONValue): TSiteName_Dto; overload;
      function UnpackFieldDto(obj: TJSONValue): TField_Dto;
      function UnpackSiteNameDtoList(arr: TJSONArray): TObjectList<TSiteName_Dto>; overload;
      function UnpackTemplateDtoList(arr: TJSONArray): TObjectList<TTemplate_Dto>; overload;
      function UnpackUploadedData(obj: TJSONObject): TUploadedData;
      function UnpackCase(obj: TJSONObject): TCase;

  public
      function Pack(mainElement: TMainElement): string; overload;
      function PackIntegerList(list: TList<integer>): string;
      function PackStringList(list: TStringList): string;

      function UnpackMainElement(json: string): TMainElement;
      function UnpackReplyElement(json: string): TReplyElement;
      function UnpackStringList(jsonList: string): TStringList;
      function UnpackSiteNameDtoList(json: string): TObjectList<TSiteName_Dto>; overload;
      function UnpackSiteNameDto(json: string): TSiteName_Dto; overload;
      function UnpackTemplateDtoList(json: string): TObjectList<TTemplate_Dto>; overload;
      function UnpackTemplateDto(json: string): TTemplate_Dto;
      function UnpackCaseDto(json: string): TCase_Dto;
      function UnpackFileDto(json: string): TFile_Dto;
      function UnpackNoteDto(json: string): TNote_Dto;
      function UnpackCasesList(json: string): TObjectList<TCase>;
  end;

implementation

{$region 'Packers'}
function TPacker.Pack(signature: TSignature): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','Signature');
  obj.AddPair('Id', IntToStr(signature.Id));
  obj.AddPair('Mandatory', BoolToStr(signature.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(signature.ReadOnly, true));
  obj.AddPair('Label', signature._Label);
  obj.AddPair('Description', Pack(signature.Description));
  obj.AddPair('Color', signature.Color);
  obj.AddPair('DisplayOrder', IntToStr(signature.DisplayOrder));
  obj.AddPair('Dummy', BoolToStr(signature.Dummy, true));
  result := obj;
end;


function TPacker.Pack(date: TDate): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','Date');
  obj.AddPair('Id', IntToStr(date.Id));
  obj.AddPair('Mandatory', BoolToStr(date.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(date.ReadOnly, true));
  obj.AddPair('Label', date._Label);
  obj.AddPair('Description', Pack(date.Description));
  obj.AddPair('Color', date.Color);
  obj.AddPair('DisplayOrder', IntToStr(date.DisplayOrder));
  obj.AddPair('Dummy', BoolToStr(date.Dummy, true));
  obj.AddPair('DefaultValue', date.DefaultValue);
  obj.AddPair('MaxValue', FormatDateTime('yyyy-MM-dd hh:mm:ss', date.MaxValue));
  obj.AddPair('MinValue', FormatDateTime('yyyy-MM-dd hh:mm:ss', date.MinValue));
  result := obj;
end;

function TPacker.Pack(saveButton: TSaveButton): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','SaveButton');
  obj.AddPair('Id', IntToStr(saveButton.Id));
  obj.AddPair('Mandatory', BoolToStr(saveButton.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(saveButton.ReadOnly, true));
  obj.AddPair('Label', saveButton._Label);
  obj.AddPair('Description', Pack(saveButton.Description));
  obj.AddPair('Color', saveButton.Color);
  obj.AddPair('DisplayOrder', IntToStr(saveButton.DisplayOrder));
  obj.AddPair('Dummy', BoolToStr(saveButton.Dummy, true));
  obj.AddPair('Value', saveButton.Value);
  result := obj;
end;

function TPacker.Pack(timer: TTimer): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','Timer');
  obj.AddPair('Id', IntToStr(timer.Id));
  obj.AddPair('Mandatory', BoolToStr(timer.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(timer.ReadOnly, true));
  obj.AddPair('Label', timer._Label);
  obj.AddPair('Description', Pack(timer.Description));
  obj.AddPair('Color', timer.Color);
  obj.AddPair('DisplayOrder', IntToStr(timer.DisplayOrder));
  obj.AddPair('Dummy', BoolToStr(timer.Dummy, true));
  obj.AddPair('StopOnSave', BoolToStr(timer.StopOnSave, true));
  result := obj;
end;

function TPacker.Pack(none: TNone): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','None');
  obj.AddPair('Id', IntToStr(none.Id));
  obj.AddPair('Mandatory', BoolToStr(none.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(none.ReadOnly, true));
  obj.AddPair('Label', none._Label);
  obj.AddPair('Description', Pack(none.Description));
  obj.AddPair('Color', none.Color);
  obj.AddPair('DisplayOrder', IntToStr(none.DisplayOrder));
  obj.AddPair('Dummy', BoolToStr(none.Dummy, true));
  result := obj;
end;

function TPacker.Pack(checkBox: TCheckBox): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','CheckBox');
  obj.AddPair('Id', IntToStr(checkBox.Id));
  obj.AddPair('Mandatory', BoolToStr(checkBox.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(checkBox.ReadOnly, true));
  obj.AddPair('Label', checkBox._Label);
  obj.AddPair('Description', Pack(checkBox.Description));
  obj.AddPair('Color', checkBox.Color);
  obj.AddPair('DisplayOrder', IntToStr(checkBox.DisplayOrder));
  obj.AddPair('Dummy', BoolToStr(checkBox.Dummy, true));
  obj.AddPair('DefaultValue', BoolToStr(checkBox.DefaultValue, true));
  obj.AddPair('Selected', BoolToStr(checkBox.Selected, true));
  result := obj;
end;


function  TPacker.Pack(keyValuePairList: TObjectList<TKeyValuePair>): TJSONArray;
var
  obj: TJSONObject;
  arr: TJSONArray;
  keyPairValue: TKeyValuePair;

begin
   arr := TJSONArray.Create;
   for keyPairValue in keyValuePairList do
   begin
      obj := TJSONObject.Create;
      obj.AddPair('Key',keyPairValue.Key);
      obj.AddPair('Value', keyPairValue.Value);
      obj.AddPair('Selected', BoolToStr(keyPairValue.Selected, true));
      obj.AddPair('DisplayOrder', keyPairValue.DisplayOrder);
      arr.AddElement(obj);
   end;
   result := arr;
end;

function TPacker.Pack(multiSelect: TMultiSelect): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','MultiSelect');
  obj.AddPair('Id', IntToStr(multiSelect.Id));
  obj.AddPair('Mandatory', BoolToStr(multiSelect.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(multiSelect.ReadOnly, true));
  obj.AddPair('Label', multiSelect._Label);
  obj.AddPair('Description', Pack(multiSelect.Description));
  obj.AddPair('Color', multiSelect.Color);
  obj.AddPair('DisplayOrder', IntToStr(multiSelect.DisplayOrder));
  obj.AddPair('Dummy', BoolToStr(multiSelect.Dummy, true));
  obj.AddPair('KeyValuePairList', Pack(multiSelect.KeyValuePairList));
  result := obj;
end;

function TPacker.Pack(singleSelect: TSingleSelect): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','SingleSelect');
  obj.AddPair('Id', IntToStr(singleSelect.Id));
  obj.AddPair('Mandatory', BoolToStr(singleSelect.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(singleSelect.ReadOnly, true));
  obj.AddPair('Label', singleSelect._Label);
  obj.AddPair('Description', Pack(singleSelect.Description));
  obj.AddPair('Color', singleSelect.Color);
  obj.AddPair('DisplayOrder', IntToStr(singleSelect.DisplayOrder));
  obj.AddPair('Dummy', BoolToStr(singleSelect.Dummy, true));
  obj.AddPair('KeyValuePairList', Pack(singleSelect.KeyValuePairList));
  result := obj;
end;

function TPacker.Pack(number: TNumber): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','Number');
  obj.AddPair('Id', IntToStr(number.Id));
  obj.AddPair('Mandatory', BoolToStr(number.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(number.ReadOnly, true));
  obj.AddPair('Label', number._Label);
  obj.AddPair('Description', Pack(number.Description));
  obj.AddPair('Color', number.Color);
  obj.AddPair('DisplayOrder', IntToStr(number.DisplayOrder));
  obj.AddPair('Dummy', BoolToStr(number.Dummy, true));
  obj.AddPair('MinValue', number.MinValue);
  obj.AddPair('MaxValue', number.MaxValue);
  obj.AddPair('DefaultValue', IntToStr(number.DefaultValue));
  obj.AddPair('DecimalCount', IntToStr(number.DecimalCount));
  obj.AddPair('UnitName', number.UnitName);
  result := obj;
end;

function TPacker.Pack(text: TText): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','Text');
  obj.AddPair('Id', IntToStr(text.Id));
  obj.AddPair('Mandatory', BoolToStr(text.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(text.ReadOnly, true));
  obj.AddPair('Label', text._Label);
  obj.AddPair('Description', Pack(text.Description));
  obj.AddPair('DisplayOrder', IntToStr(text.DisplayOrder));
  obj.AddPair('Color', text.Color);
  obj.AddPair('Dummy', BoolToStr(text.Dummy, true));
  obj.AddPair('Value', text.Value);
  obj.AddPair('MaxLength', IntToStr(text.MaxLength));
  obj.AddPair('GeolocationEnabled', BoolToStr(text.GeolocationEnabled, true));
  obj.AddPair('GeolocationForced', BoolToStr(text.GeolocationForced, true));
  obj.AddPair('GeolocationHidden', BoolToStr(text.GeolocationHidden, true));
  obj.AddPair('BarcodeEnabled', BoolToStr(text.BarcodeEnabled, true));
  obj.AddPair('BarcodeType', text.BarcodeType);
  result := obj;
end;

function TPacker.Pack(comment: TComment): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','Comment');
  obj.AddPair('Id', IntToStr(comment.Id));
  obj.AddPair('Mandatory', BoolToStr(comment.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(comment.ReadOnly, true));
  obj.AddPair('Label', comment._Label);
  obj.AddPair('Description', Pack(comment.Description));
  obj.AddPair('Color', comment.Color);
  obj.AddPair('DisplayOrder', IntToStr(comment.DisplayOrder));
  obj.AddPair('Dummy', BoolToStr(comment.Dummy, true));
  obj.AddPair('Value', comment.Value);
  obj.AddPair('MaxLength', IntToStr(comment.DisplayOrder));
  obj.AddPair('SplitScreen', BoolToStr(comment.SplitScreen, true));
  result := obj;
end;

function TPacker.Pack(picture: TPicture): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','Picture');
  obj.AddPair('Id', IntToStr(picture.Id));
  obj.AddPair('Mandatory', BoolToStr(picture.Mandatory, true));
  obj.AddPair('ReadOnly', BoolToStr(picture.ReadOnly, true));
  obj.AddPair('Label', picture._Label);
  obj.AddPair('Description', Pack(picture.Description));
  obj.AddPair('Color', picture.Color);
  obj.AddPair('DisplayOrder', IntToStr(picture.DisplayOrder));
  obj.AddPair('Dummy', BoolToStr(picture.Dummy, true));
  obj.AddPair('Multi', IntToStr(picture.Multi));
  obj.AddPair('GeolocationEnabled', BoolToStr(picture.GeolocationEnabled, true));
  result := obj;
end;

function TPacker.Pack(showPdf: TShowPdf): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','ShowPdf');
  obj.AddPair('Id', IntToStr(showPdf.Id));
  obj.AddPair('Label', showPdf._Label);
  obj.AddPair('Description', Pack(showPdf.Description));
  obj.AddPair('Color', showPdf.Color);
  obj.AddPair('DisplayOrder', IntToStr(showPdf.DisplayOrder));
  obj.AddPair('Color', showPdf.Color);
  obj.AddPair('Value', showPdf.Value);
  result := obj;
end;


function TPacker.Pack(cdata: TCDataValue): TJSONObject;
var
  obj: TJSONObject;
  //arr: TJSONArray;
begin
  obj := TJSONObject.Create;
  obj.AddPair('InderValue', cdata.InderValue);
  //arr := TJSONArray.Create;
  //obj.AddPair('CDataWrapper', arr);
  result := obj;
end;

function TPacker.Pack(fieldContainer: TFieldContainer): TJSONObject;
var
  obj: TJSONObject;
begin
  obj := TJSONObject.Create;
  obj.AddPair('Type','FieldContainer');
  obj.AddPair('Id', IntToStr(fieldContainer.Id));
  obj.AddPair('Label', fieldContainer._Label);
  obj.AddPair('Description', Pack(fieldContainer.Description));
  obj.AddPair('Color', fieldContainer.Color);
  obj.AddPair('DisplayOrder', IntToStr(fieldContainer.DisplayOrder));
  obj.AddPair('Value', fieldContainer.Value);
  obj.AddPair('DataItemList', Pack(fieldContainer.DataItemList));
  result := obj;
end;


function TPacker.Pack(dataItemList: TObjectList<TDataItem>): TJSONArray;
var
  arr: TJSONArray;
  dataItem: TDataItem;
begin
   arr := TJSONArray.Create;
   for dataItem in dataItemList do
   begin
      if dataItem is TPicture then
         arr.AddElement(Pack(dataItem as TPicture))
      else if dataItem is TSignature then
         arr.AddElement(Pack(dataItem as TSignature))
      else if dataItem is TDate then
         arr.AddElement(Pack(dataItem as TDate))
      else if dataItem is TSaveButton then
         arr.AddElement(Pack(dataItem as TSaveButton))
      else if dataItem is TTimer then
         arr.AddElement(Pack(dataItem as TTImer))
      else if dataItem is TCheckBox then
         arr.AddElement(Pack(dataItem as TCheckBox))
      else if dataItem is TMultiSelect then
         arr.AddElement(Pack(dataItem as TMultiSelect))
      else if dataItem is TSingleSelect then
         arr.AddElement(Pack(dataItem as TSingleSelect))
      else if dataItem is TNumber then
         arr.AddElement(Pack(dataItem as TNumber))
      else if dataItem is TText then
         arr.AddElement(Pack(dataItem as TText))
      else if dataItem is TComment then
         arr.AddElement(Pack(dataItem as TComment))
      else if dataItem is TShowPdf then
         arr.AddElement(Pack(dataItem as TShowPdf))
      else if dataItem is TNone then
         arr.AddElement(Pack(dataItem as TNone))
      else if dataItem is TFieldContainer then
         arr.AddElement(Pack(dataItem as TFieldContainer));
   end;
   result := arr;
end;

function TPacker.Pack(elementList: TObjectList<TElement>): TJSONArray;
var
  arr: TJSONArray;
  obj: TJSONObject;
  element: TElement;
begin
  arr := TJSONArray.Create;
  for element in elementList do
  begin
    if element is TDataElement then
    begin
      obj := TJSONObject.Create;
      obj.AddPair('Type','DataElement');
      obj.AddPair('Id', IntToStr(element.Id));
      obj.AddPair('Label', element._Label);
      obj.AddPair('DisplayOrder', IntToStr(element.DisplayOrder));
      obj.AddPair('Description', Pack(element.Description));
      obj.AddPair('ReviewEnabled', BoolToStr(element.ReviewEnabled, true));
      obj.AddPair('ApprovalEnabled', BoolToStr(element.ApprovalEnabled, true));
      obj.AddPair('DoneButtonEnabled', BoolToStr(element.DoneButtonEnabled, true));
      obj.AddPair('ExtraFieldsEnabled', BoolToStr(element.ExtraFieldsEnabled, true));
      obj.AddPair('PinkBarText', element.PinkBarText);
      if ((element as TDataElement).DataItemList <> nil) and ((element as TDataElement).DataItemList.Count > 0) then
           obj.AddPair('DataItemList', Pack((element as TDataElement).DataItemList));
      arr.AddElement(obj);
    end;
  end;
  result := arr;
end;

function TPacker.Pack(mainElement: TMainElement): string;
var
   obj: TJSONObject;
begin
   obj := TJSONObject.Create;
   obj.AddPair('Id', IntToStr(mainElement.Id));
   obj.AddPair('Label', mainElement._Label);
   obj.AddPair('DisplayOrder', IntToStr(mainElement.DisplayOrder));
   obj.AddPair('CheckListFolderName', mainElement.CheckListFolderName);
   obj.AddPair('Repeated', IntToStr(mainElement.Repeated));
   obj.AddPair('MicrotingUId', mainElement.MicrotingUId);
   obj.AddPair('CaseType', mainElement.CaseType);
   obj.AddPair('Language', mainElement.Language);
   obj.AddPair('MultiApproval', BoolToStr(mainElement.MultiApproval, true));
   obj.AddPair('FastNavigation', BoolToStr(mainElement.FastNavigation, true));
   obj.AddPair('DownloadEntities', BoolToStr(mainElement.DownloadEntities, true));
   obj.AddPair('ManualSync', BoolToStr(mainElement.ManualSync, true));
   obj.AddPair('StartDate', FormatDateTime('yyyy-MM-dd hh:mm:ss', mainElement.StartDate));
   obj.AddPair('EndDate', FormatDateTime('yyyy-MM-dd hh:mm:ss',mainElement.EndDate));
   if mainElement.ElementList.Count > 0 then
     obj.AddPair('ElementList', Pack(mainElement.ElementList));

   result := obj.ToString();
end;

function TPacker.PackIntegerList(list: TList<integer>): string;
var
  arr: TJSONArray;
  val: integer;
begin
  arr := TJSONArray.Create;
  for val in list do
     arr.Add(IntToStr(val));
  result := arr.ToString();
end;

function TPacker.PackStringList(list: TStringList): string;
var
  arr: TJSONArray;
  val: string;
begin
  arr := TJSONArray.Create;
  for val in list do
     arr.Add(val);
  result := arr.ToString();
end;


{$endregion}

{$region 'Unpackers'}

function TPacker.UnpackField(obj: TJSONObject): TField;
var
  field: TField;
  s: string;
begin
  field := TField.Create;
  field.Id := obj.GetValue<integer>('Id');
  field._Label := obj.GetValue<string>('Label');
  field.Mandatory := obj.GetValue<boolean>('Mandatory');
  field.ReadOnly := obj.GetValue<boolean>('ReadOnly');
  field.Description := TCDataValue.Create;
  field.Description.InderValue := obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue');
  field.DisplayOrder := obj.GetValue<integer>('DisplayOrder');
  field.Color := obj.GetValue<string>('Color');
  field.Dummy := obj.GetValue<boolean>('Dummy');
  field.FieldType := obj.GetValue<string>('FieldType');
  field.FieldValue := obj.GetValue<string>('FieldValue');
  if obj.GetValue('EntityGroudId') <> nil then
     field.EntityGroudId := obj.GetValue<integer>('EntityGroudId');
  s := obj.GetValue('KeyValuePairList').Value;
  if (obj.GetValue('KeyValuePairList') <> nil) and (obj.GetValue('KeyValuePairList').Value <>'null') then
    field.KeyValuePairList := UnpackKeyValuePairList(obj.GetValue<TJSONArray>('KeyValuePairList'))
  else
    field.KeyValuePairList := TObjectList<TKeyValuePair>.Create;
  field.FieldValues := UnpackFieldValues(obj.GetValue<TJSONArray>('FieldValues'));
  result := field;
end;

function TPacker.UnpackFieldValues(arr: TJSONArray): TObjectList<TFieldValue>;
var
   fieldValues: TObjectList<TFieldValue>;
   fieldValue: TFieldValue;
   i: integer;
   obj: TJSONObject;
   s: string;
begin
   fieldValues := TObjectList<TFieldValue>.Create;
   for i := 0 to arr.Count - 1 do
   begin
      obj := arr.Items[i] as TJSONObject;
      fieldValue := TFieldValue.Create;
      fieldValue.FieldId := obj.GetValue<integer>('FieldId');
      fieldValue.FieldType := obj.GetValue<string>('FieldType');
      fieldValue.DateOfDoing := obj.GetValue<TDateTime>('DateOfDoing');
      fieldValue.Value := obj.GetValue<string>('Value');
      fieldValue.MicrotingUuid := obj.GetValue<string>('MicrotingUuid');
      fieldValue.Latitude := obj.GetValue<string>('Latitude');
      fieldValue.Longitude := obj.GetValue<string>('Longitude');
      fieldValue.Altitude := obj.GetValue<string>('Altitude');
      fieldValue.Heading := obj.GetValue<string>('Heading');
      fieldValue.Accuracy := obj.GetValue<string>('Accuracy');
      if (not (obj.Get('Date').JsonValue is  TJSONNull)) then
          fieldValue.Date := obj.GetValue<TDateTime>('Date');
      fieldValue.UploadedData := obj.GetValue<string>('UploadedData');
      fieldValue.UploadedDataObj := UnpackUploadedData(obj.GetValue<TJSONObject>('UploadedDataObj'));

      s := obj.GetValue('KeyValuePairList').Value;
      if (obj.GetValue('KeyValuePairList') <> nil) and ( not (obj.Get('KeyValuePairList').JsonValue is TJSONNull))  then
        fieldValue.KeyValuePairList := UnpackKeyValuePairList(obj.GetValue<TJSONArray>('KeyValuePairList'))
      else
        fieldValue.KeyValuePairList := TObjectList<TKeyValuePair>.Create;


      fieldValues.Add(fieldValue);
   end;
   result := fieldValues;
end;


function TPacker.UnpackUploadedData(obj: TJSONObject): TUploadedData;
var
  uploadedData: TUploadedData;
begin
  uploadedData := TUploadedData.Create;
  uploadedData.Checksum := obj.GetValue<string>('Checksum');
  uploadedData.Extension := obj.GetValue<string>('Extension');
  uploadedData.CurrentFile := obj.GetValue<string>('CurrentFile');
  uploadedData.UploaderId := obj.GetValue<integer>('UploaderId');
  uploadedData.UploaderType := obj.GetValue<string>('UploaderType');
  uploadedData.FileLocation := obj.GetValue<string>('FileLocation');
  uploadedData.FileName := obj.GetValue<string>('FileName');
  result := uploadedData;
end;


function TPacker.UnpackSignature(obj: TJSONObject): TSignature;
var
  signature: TSignature;
begin
  signature := TSignature.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy')
  );
  result := signature;
end;

function TPacker.UnpackDate(obj: TJSONObject): TDate;
var
  date: TDate;
begin
   date := TDate.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy'),
       obj.GetValue<string>('DefaultValue'),
       obj.GetValue<TDateTime>('MaxValue'),
       obj.GetValue<TDateTime>('MinValue')
  );
  result := date;
end;

function TPacker.UnpackShowPdf(obj: TJSONObject): TShowPdf;
var
   showPdf: TShowPdf;
begin
   showPdf := TShowPdf.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy'),
       obj.GetValue<string>('Value')
  );
  result := showPdf;
end;

function TPacker.UnpackTimer(obj: TJSONObject): TTimer;
var
   timer: TTimer;
begin
   timer := TTimer.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy'),
       obj.GetValue<boolean>('StopOnSave')
  );
  result := timer;
end;

function TPacker.UnpackNone(obj: TJSONObject): TNone;
var
   none: TNone;
begin
   none := TNone.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy')
  );
  result := none;
end;

function TPacker.UnpackCheckBox(obj: TJSONObject): TCheckBox;
var
   checkBox: TCheckBox;
begin
   checkBox := TCheckBox.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy'),
       obj.GetValue<boolean>('DefaultValue'),
       obj.GetValue<boolean>('Selected')
   );
   result := checkBox;
end;

function TPacker.UnpackMultiSelect(obj: TJSONObject): TMultiSelect;
var
   multiSelect: TMultiSelect;
begin
   multiSelect := TMultiSelect.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy'),
       UnpackKeyValuePairList(obj.GetValue<TJSONArray>('KeyValuePairList'))
   );
   result := multiSelect;
end;

function TPacker.UnpackSingleSelect(obj: TJSONObject): TSingleSelect;
var
   singleSelect: TSingleSelect;
begin
   singleSelect := TSingleSelect.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy'),
       UnpackKeyValuePairList(obj.GetValue<TJSONArray>('KeyValuePairList'))
   );
   result := singleSelect;
end;

function TPacker.UnpackNumber(obj: TJSONObject): TNumber;
var
   number: TNumber;
begin
   number := TNumber.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy'),
       obj.GetValue<string>('MinValue'),
       obj.GetValue<string>('MaxValue'),
       obj.GetValue<integer>('DefaultValue'),
       obj.GetValue<integer>('DecimalCount'),
       obj.GetValue<string>('UnitName')
   );
   result := number;
end;

function TPacker.UnpackText(obj: TJSONObject): TText;
var
   text: TText;
begin
   text := TText.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy'),
       obj.GetValue<string>('Value'),
       obj.GetValue<integer>('MaxLength'),
       obj.GetValue<boolean>('GeolocationEnabled'),
       obj.GetValue<boolean>('GeolocationForced'),
       obj.GetValue<boolean>('GeolocationHidden'),
       obj.GetValue<boolean>('BarcodeEnabled'),
       obj.GetValue<string>('BarcodeType')
   );
   result := text;
end;

function TPacker.UnpackComment(obj: TJSONObject): TComment;
var
   comment: TComment;
begin
  comment := TComment.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy'),
       obj.GetValue<string>('Value'),
       obj.GetValue<integer>('Maxlength'),
       obj.GetValue<boolean>('SplitScreen')
   );
   result := comment;
end;

function TPacker.UnpackKeyValuePairList(arr: TJSONArray): TObjectList<TKeyValuePair>;
var
   keyValuePairList: TObjectList<TKeyValuePair>;
   i: integer;
begin
   keyValuePairList := TObjectList<TKeyValuePair>.Create;
   for i := 0 to arr.Count - 1 do
       keyValuePairList.Add(UnpackKeyValuePair(arr.Items[i] as TJSONObject));
   result := keyValuePairList;
end;

function TPacker.UnpackKeyValuePair(obj: TJSONObject): TKeyValuePair;
var
   keyValuePair: TKeyValuePair;
begin
   keyValuePair := TKeyValuePair.Create(
       obj.GetValue<string>('Key'),
       obj.GetValue<string>('Value'),
       obj.GetValue<boolean>('Selected'),
       obj.GetValue<string>('DisplayOrder')
   );
   result := keyValuePair;
end;

function TPacker.UnpackFieldContainer(obj: TJSONObject): TFieldContainer;
var
   fieldContainer: TFieldContainer;
begin
   fieldContainer := TFieldContainer.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TCDataValue>('Description'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<string>('Value'),
       UnpackDataItemList(obj.GetValue<TJSONArray>('DataItemList'))
   );
   result := fieldContainer;
end;


function TPacker.UnpackPicture(obj: TJSONObject): TPicture;
var
  picture: TPicture;
begin
  picture := TPicture.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy'),
       obj.GetValue<integer>('Multi'),
       obj.GetValue<boolean>('GeolocationEnabled')
  );
  result := picture;
end;

function TPacker.UnpackSaveButton(obj: TJSONObject): TSaveButton;
var
  saveButton: TSaveButton;
begin
  saveButton := TSaveButton.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<boolean>('Mandatory'),
       obj.GetValue<boolean>('ReadOnly'),
       obj.GetValue<string>('Label'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<string>('Color'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<boolean>('Dummy'),
       obj.GetValue<string>('Value')
  );
  result := saveButton;
end;

function  TPacker.UnpackDataItemGroup(obj: TJSONObject): TDataItemGroup;
var
  dataItemGroup: TDataItemGroup;
begin
  dataItemGroup := TDataItemGroup.Create;
  dataItemGroup.Id :=  obj.GetValue<string>('Id');
  dataItemGroup._Label :=  obj.GetValue<string>('Label');
  if  (obj.GetValue('Description').Value <> '') then
    dataItemGroup.Description :=   obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue');
  dataItemGroup.Color := obj.GetValue<string>('Color');
  dataItemGroup.DisplayOrder := obj.GetValue<integer>('DisplayOrder');
  dataItemGroup.Value := obj.GetValue<string>('Value');
  dataItemGroup.DataItemList := UnpackDataItemList(obj.GetValue<TJSONArray>('DataItemList'));
  result := dataItemGroup;
end;

function TPacker.UnpackDataItemGroupList(arr: TJSONArray): TObjectList<TDataItemGroup>;
var
   dataItemGroupList: TObjectList<TDataItemGroup>;
   i: integer;
   obj: TJSONObject;
begin
   dataItemGroupList := TObjectList<TDataItemGroup>.Create;
   for i := 0 to arr.Count - 1 do
   begin
     obj := arr.Items[i] as TJSONObject;
     dataItemGroupList.Add(UnpackDataItemGroup(obj));
   end;
   result := dataItemGroupList;
end;

function TPacker.UnpackDataItemList(arr: TJSONArray): TObjectList<TDataItem>;
var
   obj: TJSONObject;
   dataItemList: TObjectList<TDataItem>;
   i: integer;
    _type: string;
begin
   dataItemList := TObjectList<TDataItem>.Create;
   for i := 0 to arr.Count - 1 do
   begin
     obj := arr.Items[i] as TJSONObject;
     _type := obj.GetValue<string>('Type');
     if _type = 'Picture' then
        dataItemList.Add(UnpackPicture(obj))
     else if _type = 'SaveButton' then
       dataItemList.Add(UnpackSaveButton(obj))
     else if _type = 'None' then
       dataItemList.Add(UnpackNone(obj))
     else if _type = 'CheckBox' then
       dataItemList.Add(UnpackCheckBox(obj))
     else if _type = 'Timer' then
       dataItemList.Add(UnpackTimer(obj))
     else if _type = 'ShowPdf' then
       dataItemList.Add(UnpackShowPdf(obj))
     else if _type = 'Date' then
       dataItemList.Add(UnpackDate(obj))
     else if _type = 'Number' then
       dataItemList.Add(UnpackNumber(obj))
     else if _type = 'MultiSelect' then
       dataItemList.Add(UnpackMultiSelect(obj))
     else if _type = 'SingleSelect' then
       dataItemList.Add(UnpackSingleSelect(obj))
     else if _type = 'Text' then
       dataItemList.Add(UnpackText(obj))
     else if _type = 'Comment' then
       dataItemList.Add(UnpackComment(obj))
     else if _type = 'FieldContainer' then
       dataItemList.Add(UnpackFieldContainer(obj))
     else if _type = 'Signature' then
       dataItemList.Add(UnpackSignature(obj))
     else if _type = 'Field' then
       dataItemList.Add(UnpackField(obj));

   end;
   result := dataItemList;
end;

function TPacker.UnpackDataElement(obj: TJSONObject): TDataElement;
var
   dataElement: TDataElement;
begin
   dataElement := TDataElement.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<string>('Label'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<boolean>('ApprovalEnabled'),
       obj.GetValue<boolean>('ReviewEnabled'),
       obj.GetValue<boolean>('DoneButtonEnabled'),
       obj.GetValue<boolean>('ExtraFieldsEnabled'),
       obj.GetValue<string>('PinkBarText'),
       UnpackDataItemGroupList(obj.GetValue<TJSONArray>('DataItemGroupList')),
       UnpackDataItemList(obj.GetValue<TJSONArray>('DataItemList'))
   );
   result := dataElement;
end;

function TPacker.UnpackCheckListValue(obj: TJSONObject): TCheckListValue;
var
   checkListValue: TCheckListValue;
   dataElement : TDataElement;
begin
   dataElement := TDataElement.Create(
       obj.GetValue<integer>('Id'),
       obj.GetValue<string>('Label'),
       obj.GetValue<integer>('DisplayOrder'),
       obj.GetValue<TJSONObject>('Description').GetValue<string>('InderValue'),
       obj.GetValue<boolean>('ApprovalEnabled'),
       obj.GetValue<boolean>('ReviewEnabled'),
       obj.GetValue<boolean>('DoneButtonEnabled'),
       obj.GetValue<boolean>('ExtraFieldsEnabled'),
       obj.GetValue<string>('PinkBarText'),
       UnpackDataItemGroupList(obj.GetValue<TJSONArray>('DataItemGroupList')),
       UnpackDataItemList(obj.GetValue<TJSONArray>('DataItemList'))
   );
   checkListValue := TCheckListValue.Create(dataElement, obj.GetValue<string>('Status'));
   result := checkListValue;
end;

function TPacker.UnpackElementList(arr: TJSONArray): TObjectList<TElement>;
var
  obj: TJSONObject;
  elementList: TObjectList<TElement>;
  dataElement: TDataElement;
  checkListValue: TCheckListValue;
  i: integer;
  _type: string;
begin
  elementList := TObjectList<TElement>.Create;
  for i := 0 to arr.Count - 1 do
  begin
    obj := arr.Items[i] as TJSONObject;
    _type := obj.GetValue<string>('Type');
    if _type = 'DataElement' then
    begin
       dataElement := UnpackDataElement(obj);
       elementList.Add(dataElement);
    end
    else if _type = 'CheckListValue' then
    begin
      checkListValue := UnpackCheckListValue(obj);
      elementList.Add(checkListValue);
    end;

  end;
  result := elementList;
end;

function TPacker.UnpackMainElement(json: string): TMainElement;
var
  mainElement: TMainElement;
  obj: TJSONValue;
  id: integer;
  elementList: TObjectList<TElement>;
begin
  obj := TJSONObject.ParseJSONValue(json);
  elementList := TObjectList<TElement>.Create;
  mainElement := TMainElement.Create(
     obj.GetValue<integer>('Id'),
     obj.GetValue<string>('Label'),
     obj.GetValue<integer>('DisplayOrder'),
     obj.GetValue<string>('CheckListFolderName'),
     obj.GetValue<integer>('Repeated'),
     obj.GetValue<TDateTime>('StartDate'),
     obj.GetValue<TDateTime>('EndDate'),
     obj.GetValue<string>('Language'),
     obj.GetValue<boolean>('MultiApproval'),
     obj.GetValue<boolean>('FastNavigation'),
     obj.GetValue<boolean>('DownloadEntities'),
     obj.GetValue<boolean>('ManualSync'),
     obj.GetValue<string>('CaseType'),
     obj.GetValue<string>('PushMessageTitle'),
     obj.GetValue<string>('PushMessageBody'),
     UnpackElementList( obj.GetValue<TJSONArray>('ElementList'))
     );
  result := mainElement;
end;

function TPacker.UnpackReplyElement(json: string): TReplyElement;
var
  replyElement: TReplyElement;
  coreElement: TCoreElement;
  obj: TJSONValue;
  id: integer;
  elementList: TObjectList<TElement>;
begin
  obj := TJSONObject.ParseJSONValue(json);
  elementList := TObjectList<TElement>.Create;
  coreElement := TCoreElement.Create(
     obj.GetValue<integer>('Id'),
     obj.GetValue<string>('Label'),
     obj.GetValue<integer>('DisplayOrder'),
     obj.GetValue<string>('CheckListFolderName'),
     obj.GetValue<integer>('Repeated'),
     obj.GetValue<TDateTime>('StartDate'),
     obj.GetValue<TDateTime>('EndDate'),
     obj.GetValue<string>('Language'),
     obj.GetValue<boolean>('MultiApproval'),
     obj.GetValue<boolean>('FastNavigation'),
     obj.GetValue<boolean>('DownloadEntities'),
     obj.GetValue<boolean>('ManualSync'),
     obj.GetValue<string>('CaseType'),
     UnpackElementList( obj.GetValue<TJSONArray>('ElementList'))
     );
  replyElement := TReplyElement(coreElement);
  replyElement.UnitId := obj.GetValue<integer>('UnitId');
  replyElement.Custom := obj.GetValue<string>('Custom');
  replyElement.DoneById := obj.GetValue<integer>('DoneById');
  replyElement.DoneAt := obj.GetValue<TDateTime>('DoneAt');
  result := replyElement;
end;


function TPacker.UnpackStringList(jsonList: string): TStringList;
var
  arr: TJSONArray;
  i: integer;
begin
  result := TStringList.Create;
  arr := TJSONObject.ParseJSONValue(jsonList) as TJSONArray;
  for i := 0 to arr.Count-1 do
    result.Add(arr.Items[i].Value);
end;

function TPacker.UnpackSiteNameDtoList(json: string): TObjectList<TSiteName_Dto>;
var
  arr: TJSONArray;
begin
  result := TObjectList<TSiteName_Dto>.Create;
  arr := TJSONObject.ParseJSONValue(json) as TJSONArray;
  result := UnpackSiteNameDtoList(arr)
end;

function TPacker.UnpackSiteNameDtoList(arr: TJSONArray): TObjectList<TSiteName_Dto>;
var
  i: integer;
  siteNameDto: TSiteName_Dto;
begin
  result := TObjectList<TSiteName_Dto>.Create;
  for i := 0 to arr.Count-1 do
  begin
    siteNameDto := UnpackSiteNameDto(arr.Items[i]);
    result.Add(siteNameDto);
  end;
end;

function TPacker.UnpackTemplateDtoList(json: string): TObjectList<TTemplate_Dto>;
var
  arr: TJSONArray;
begin
  result := TObjectList<TTemplate_Dto>.Create;
  arr := TJSONObject.ParseJSONValue(json) as TJSONArray;
  result := UnpackTemplateDtoList(arr)
end;


function TPacker.UnpackTemplateDtoList(arr: TJSONArray): TObjectList<TTemplate_Dto>;
var
  i: integer;
  templateDto: TTemplate_Dto;
begin
  result := TObjectList<TTemplate_Dto>.Create;
  for i := 0 to arr.Count-1 do
  begin
    templateDto := UnpackTemplateDto(arr.Items[i].ToString);
    result.Add(templateDto);
  end;
end;


function TPacker.UnpackSiteNameDto(json: string): TSiteName_Dto;
var
  obj: TJSONValue;
begin
  obj := TJSONObject.ParseJSONValue(json);
  result := UnpackSiteNameDto(obj);
end;

function TPacker.UnpackSiteNameDto(obj: TJSONValue): TSiteName_Dto;
var
  siteNameDto: TSiteName_Dto;
begin
  siteNameDto := TSiteName_Dto.Create(
     obj.GetValue<integer>('SiteUId'),
     obj.GetValue<string>('SiteName'),
     obj.GetValue<TDateTime>('CreatedAt'),
     obj.GetValue<TDateTime>('UpdatedAt')
     );
  result := siteNameDto;
end;


function TPacker.UnpackFieldDto(obj: TJSONValue): TField_Dto;
var
  fieldDto: TField_Dto;
begin
  if (obj.Value = '') or (obj.Value = 'null') then
  begin
    Result := nil;
    exit;
  end;
  WriteLn(obj.Value);
  fieldDto := TField_Dto.Create(
    obj.GetValue<integer>('Id'),
    obj.GetValue<string>('Label'),
    obj.GetValue<string>('Description'),
    obj.GetValue<integer>('FieldTypeId'),
    obj.GetValue<string>('FieldType'),
    obj.GetValue<integer>('CheckListId')
  );
  Result := fieldDto;
end;


function TPacker.UnpackCaseDto(json: string): TCase_Dto;
var
  caseDto: TCase_Dto;
  obj: TJSONValue;
begin
  obj := TJSONObject.ParseJSONValue(json);
  caseDto := TCase_Dto.Create(
    obj.GetValue<integer>('CaseId'),
    obj.GetValue<string>('Stat'),
    obj.GetValue<integer>('SiteUId'),
    obj.GetValue<string>('CaseType'),
    obj.GetValue<string>('CaseUId'),
    obj.GetValue<string>('MicrotingUId'),
    obj.GetValue<string>('CheckUId'),
    obj.GetValue<string>('Custom'),
    obj.GetValue<integer>('CheckListId'),
    obj.GetValue<string>('WorkflowState')
  );
  Result := caseDto;
end;


function TPacker.UnpackFileDto(json: string): TFile_Dto;
var
  fileDto: TFile_Dto;
  obj: TJSONValue;
begin
  obj := TJSONObject.ParseJSONValue(json);
  fileDto := TFile_Dto.Create(
    obj.GetValue<integer>('SiteUId'),
    obj.GetValue<string>('CaseType'),
    obj.GetValue<string>('CaseUId'),
    obj.GetValue<string>('MicrotingUId'),
    obj.GetValue<string>('CheckUId'),
    obj.GetValue<string>('FileLocation')
  );
  Result := fileDto;
end;


function TPacker.UnpackNoteDto(json: string): TNote_Dto;
var
  noteDto: TNote_Dto;
  obj: TJSONValue;
begin
  obj := TJSONObject.ParseJSONValue(json);
  noteDto := TNote_Dto.Create(
    obj.GetValue<string>('Id'),
    obj.GetValue<string>('MicrotingUId'),
    obj.GetValue<string>('Activity')
  );
  Result := noteDto;
end;

function TPacker.UnpackTemplateDto(json: string): TTemplate_Dto;
var
  templateDto: TTemplate_Dto;
  obj: TJSONValue;
begin
  templateDto := TTemplate_Dto.Create;
  obj := TJSONObject.ParseJSONValue(json);

  templateDto := TTemplate_Dto.Create(
     obj.GetValue<integer>('Id'),
     obj.GetValue<TDateTime>('CreatedAt'),
     obj.GetValue<TDateTime>('UpdatedAt'),
     obj.GetValue<string>('Label'),
     obj.GetValue<string>('Description'),
     obj.GetValue<integer>('Repeated'),
     obj.GetValue<string>('FolderName'),
     obj.GetValue<string>('WorkflowState'),
     UnpackSiteNameDtoList(obj.GetValue<TJSONArray>('DeployedSites')),
     obj.GetValue<boolean>('HasCases'),
     obj.GetValue<integer>('DisplayIndex'),
     UnpackFieldDto(obj.GetValue<TJSONValue>('Field1')),
     UnpackFieldDto(obj.GetValue<TJSONValue>('Field2')),
     UnpackFieldDto(obj.GetValue<TJSONValue>('Field3')),
     UnpackFieldDto(obj.GetValue<TJSONValue>('Field4')),
     UnpackFieldDto(obj.GetValue<TJSONValue>('Field5')),
     UnpackFieldDto(obj.GetValue<TJSONValue>('Field6')),
     UnpackFieldDto(obj.GetValue<TJSONValue>('Field7')),
     UnpackFieldDto(obj.GetValue<TJSONValue>('Field8')),
     UnpackFieldDto(obj.GetValue<TJSONValue>('Field9')),
     UnpackFieldDto(obj.GetValue<TJSONValue>('Field10')),
     UnpackKeyValuePairList(obj.GetValue<TJSONArray>('Tags'))
  );
  result := templateDto;
end;

function TPacker.UnpackCasesList(json: string): TObjectList<TCase>;
var
  arr: TJSONArray;
  i: integer;
begin
  result := TObjectList<TCase>.Create;
  arr := TJSONObject.ParseJSONValue(json) as TJSONArray;
  for i := 0 to arr.Count-1 do
    result.Add(UnpackCase(arr.Items[i] as TJSONObject));
end;

function TPacker.UnpackCase(obj: TJSONObject): TCase;
var
  _case: TCase;
begin
  _case := TCase.Create;
  _case.Id := obj.GetValue<integer>('Id');
  _case.WorkflowState := obj.GetValue<string>('WorkflowState');
  _case.Version := obj.GetValue<integer>('Version');
  _case.Status := obj.GetValue<integer>('Status');
  _case.CreatedAt := obj.GetValue<TDateTime>('CreatedAt');
  _case.UpdatedAt := obj.GetValue<TDateTime>('UpdatedAt');
  _case.DoneAt := obj.GetValue<TDateTime>('DoneAt');
  _case.SiteName := obj.GetValue<string>('SiteName');
  _case.UnitId := obj.GetValue<integer>('UnitId');
  _case.WorkerName := obj.GetValue<string>('WorkerName');
  _case.TemplatId := obj.GetValue<integer>('TemplatId');
  _case.CaseType := obj.GetValue<string>('CaseType');
  _case.MicrotingUId := obj.GetValue<string>('MicrotingUId');
  //_case.CheckUIid := obj.GetValue<string>('CheckUIid');
  _case.CaseUId := obj.GetValue<string>('CaseUId');
  _case.FieldValue1 := obj.GetValue<string>('FieldValue1');
  _case.FieldValue2 := obj.GetValue<string>('FieldValue2');
  _case.FieldValue3 := obj.GetValue<string>('FieldValue3');
  _case.FieldValue4 := obj.GetValue<string>('FieldValue4');
  _case.FieldValue5 := obj.GetValue<string>('FieldValue5');
  _case.FieldValue6 := obj.GetValue<string>('FieldValue6');
  _case.FieldValue7 := obj.GetValue<string>('FieldValue7');
  _case.FieldValue8 := obj.GetValue<string>('FieldValue8');
  _case.FieldValue9 := obj.GetValue<string>('FieldValue9');
  _case.FieldValue10 := obj.GetValue<string>('FieldValue10');

  result := _case;
end;

{$endregion}

end.
