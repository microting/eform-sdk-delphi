unit Packer;

interface

uses
  System.JSON, REST.JSON, DataItem, Element, MainElement, Generics.Collections, Classes, SysUtils, FieldContainer;

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
  public
      function Pack(mainElement: TMainElement): string; overload;
  end;

implementation

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


end.
