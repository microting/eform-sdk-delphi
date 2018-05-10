unit Core;

interface

uses
  DllHelper, Events, SysUtils, MainElement, Element, Generics.Collections, Classes, DataItem, DataItemGroup;

type
  {$region 'TCore declaration'}
  TCore = class
  private
    FCoreStartEvent: TCoreStartEvent;

    procedure SetCoreStartEvent(Value: TCoreStartEvent);
    //procedure OnCoreStartEvent;
  public
    constructor Create;
    procedure Start(serverConnectionString: string);
    function TemplatFromXml(xml: string): TMainElement;

    property CoreEvent: TCoreStartEvent read FCoreStartEvent write SetCoreStartEvent;
  end;
  {$endregion}

implementation

{$region 'TCore implementation'}
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

function TCore.TemplatFromXml(xml: string): TMainElement;
var
  mainElement: TMainElement;
  element: TElement;
  dataItem: TDataItem;
  keyValuePair: TKeyValuePair;

  _label, description: WideString;
  key, checkListFolderName, startDate, endDate: WideString;
  language, caseType, color, value:  WideString;
  minValue, maxValue, unitName, displayOrder: WideString;
  fs: TFormatSettings;

  i, j, k, dataItemCount, dataItemGroupCount, keyValueListCount: integer;
  elementType, dataItemType: WideString;
begin
  mainElement:= TMainElement.Create;
  TDllHelper.GetInstance.Core_TemplatFromXml(xml, mainElement.Id, _label, mainElement.DisplayOrder,
     checkListFolderName, mainElement.Repeated, startDate, endDate, language, mainElement.MultiApproval,
     mainElement.fastNavigation, mainElement.downloadEntities, mainelement.ManualSync, caseType);
  mainElement._Label := _label;
  mainElement.CheckListFolderName := checkListFolderName;
  mainElement.Language := language;
  mainElement.CaseType := caseType;

  fs := TFormatSettings.Create;
  fs.DateSeparator := '-';
  fs.ShortDateFormat := 'yyyy-MM-dd';

  mainElement.StartDate := StrToDate(startDate, fs);
  mainElement.EndDate := StrToDate(endDate, fs);

  mainElement.ElementList := TObjectList<TElement>.Create;
  for i := 0 to TDllHelper.GetInstance.Core_TemplatFromXml_ElementListCount - 1 do
  begin
      elementType := TDllHelper.GetInstance.Core_TemplatFromXml_GetElementType(i);
      if elementType = 'DataElement' then
      begin
          element := TDataElement.Create;
          TDllHelper.GetInstance.Core_TemplatFromXml_GetDataElement(i, element.Id, _label, description,
           element.DisplayOrder, element.ReviewEnabled, element.ExtraFieldsEnabled, element.ApprovalEnabled);
          element._Label := _label;
          element.Description := TCDataValue.Create;
          element.Description.InderValue := description;
          {$region 'DataItemList'}
          dataItemCount := TDllHelper.GetInstance.GetInstance.Core_TemplatFromXml_DataElement_DataItemCount(i);
          if dataItemCount > 0 then
               (element as TDataElement).DataItemList := TObjectList<TDataItem>.Create;
          for j := 0 to dataItemCount - 1 do
          begin
              dataItemType := TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetDataItemType(i, j);
              if dataItemType = 'Picture' then
              begin
                  dataItem := TPicture.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetPicture(i, j, dataItem.Id, _label,
                    description, dataItem.DisplayOrder, dataItem.Mandatory, color);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;
                  dataItem.Color := color;
              end
              else if dataItemType = 'ShowPdf' then
              begin
                  dataItem := TShowPdf.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetShowPdf(i, j, dataItem.Id, _label,
                    description, dataItem.DisplayOrder, color, value);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;
                  dataItem.Color := color;
                  (dataItem as TShowPdf).Value := value;
              end
              else if dataItemType = 'Date' then
              begin
                  dataItem := TDate.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetDate(i, j, dataItem.Id, _label,
                    description, dataItem.DisplayOrder, minValue, maxValue, dataItem.Mandatory, dataItem.ReadOnly,
                    color, value);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;
                  dataItem.Color := color;
                  (dataItem as TDate).MinValue := StrToDate(minValue, fs);
                  (dataItem as TDate).MaxValue := StrToDate(maxValue, fs);
                  (dataItem as TDate).DefaultValue := value;
              end
              else if dataItemType = 'Signature' then
              begin
                  dataItem := TSignature.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetSignature(i, j, dataItem.Id, _label,
                    description, dataItem.DisplayOrder, dataItem.Mandatory, color);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;
                  dataItem.Color := color;
              end
              else if dataItemType = 'SaveButton' then
              begin
                  dataItem := TSaveButton.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetSaveButton(i, j, dataItem.Id, _label,
                    description, dataItem.DisplayOrder, value);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;
                  dataItem.Color := color;
                  (dataItem as TSaveButton).Value := value;
              end
              else if dataItemType = 'Timer' then
              begin
                  dataItem := TTimer.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetTimer(i, j, dataItem.Id, _label,
                    description, dataItem.DisplayOrder, (dataItem as TTimer).StopOnSave, dataItem.Mandatory);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;
                  dataItem.Color := color;
              end
              else if dataItemType = 'None' then
              begin
                  dataItem := TNone.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetNone(i, j, dataItem.Id, _label,
                    description, dataItem.DisplayOrder);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;
              end
              else if dataItemType = 'CheckBox' then
              begin
                  dataItem := TCheckBox.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetCheckBox(i, j, dataItem.Id, _label,
                    description, dataItem.DisplayOrder, dataItem.Mandatory, (dataItem as TCheckBox).Selected);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;
              end
              else if dataItemType = 'MultiSelect' then
              begin
                  dataItem := TMultiSelect.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetMultiSelect(i, j, dataItem.Id, _label,
                    description, dataItem.DisplayOrder, dataItem.Mandatory);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;

                  keyValueListCount :=  TDllHelper.GetInstance.Core_TemplatFromXml_KeyValueListCount('DataElement_MultiSelect_' + IntToStr(i) + '_' + IntToStr(j));
                  (dataItem as TMultiSelect).KeyValuePairList := TObjectList<TKeyValuePair>.Create();
                  for k := 0 to keyValueListCount - 1 do
                  begin
                     keyValuePair := TKeyValuePair.Create();
                     TDllHelper.GetInstance.Core_TemplatFromXml_GetKeyValuePair('DataElement_MultiSelect_' + IntToStr(i) + '_' + IntToStr(j) + '_' + IntToStr(k), key, value,
                      keyValuePair.Selected, displayOrder);
                     keyValuePair.Key := key;
                     keyValuePair.Value := value;
                     keyValuePair.DisplayOrder := displayOrder;
                     (dataItem as TMultiSelect).KeyValuePairList.Add(keyValuePair);
                  end;
              end
              else if dataItemType = 'SingleSelect' then
              begin
                  dataItem := TMultiSelect.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetSingleSelect(i, j, dataItem.Id, _label,
                    description, dataItem.DisplayOrder, dataItem.Mandatory);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;

                  keyValueListCount :=  TDllHelper.GetInstance.Core_TemplatFromXml_KeyValueListCount('DataElement_SingleSelect_' + IntToStr(i) + '_' + IntToStr(j));
                  (dataItem as TMultiSelect).KeyValuePairList := TObjectList<TKeyValuePair>.Create();
                  for k := 0 to keyValueListCount - 1 do
                  begin
                     keyValuePair := TKeyValuePair.Create();
                     TDllHelper.GetInstance.Core_TemplatFromXml_GetKeyValuePair('DataElement_SingleSelect_' + IntToStr(i) + '_' + IntToStr(j) + '_' + IntToStr(k), key, value,
                      keyValuePair.Selected, displayOrder);
                     keyValuePair.Key := key;
                     keyValuePair.Value := value;
                     keyValuePair.DisplayOrder := displayOrder;
                     (dataItem as TMultiSelect).KeyValuePairList.Add(keyValuePair);
                  end;
              end
              else if dataItemType = 'Number' then
              begin
                  dataItem := TNumber.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetNumber(i, j, dataItem.Id, _label,
                    description, dataItem.DisplayOrder, minValue, maxValue, dataItem.Mandatory,
                    (dataItem as TNumber).DecimalCount, unitName);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;
                  (dataItem as TNumber).MinValue := minValue;
                  (dataItem as TNumber).MaxValue := maxValue;
                  (dataItem as TNumber).UnitName := unitName;
              end
              else if dataItemType = 'Text' then
              begin
                  dataItem := TText.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetText(i, j, dataItem.Id, _label,
                    description, (dataItem as TText).GeolocationEnabled, value, dataItem.Mandatory
                    , dataItem.ReadOnly);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;
                  (dataItem as TText).Value := value;
              end
              else if dataItemType = 'Comment' then
              begin
                  dataItem := TComment.Create;
                  TDllHelper.GetInstance.Core_TemplatFromXml_DataElement_GetComment(i, j, dataItem.Id, _label,
                    description, (dataItem as TComment).SplitScreen, value, dataItem.Mandatory
                    , dataItem.ReadOnly);
                  dataItem._Label := _label;
                  dataItem.Description := TCDataValue.Create;
                  dataItem.Description.InderValue := description;
                  (dataItem as TComment).Value := value;
              end;
             (element as TDataElement).DataItemList.Add(dataItem);
            end;
          {$endregion}
          {$region 'DataItemGroupList'}
          dataItemGroupCount := TDllHelper.GetInstance.GetInstance.Core_TemplatFromXml_DataItemGroupCount('DataElement' + '_' + IntToStr(i));
          if dataItemGroupCount > 0 then
               (element as TDataElement).DataItemGroupList := TObjectList<TDataItemGroup>.Create;

          {$endregion}
      end;
      mainElement.ElementList.Add(element);
  end;

  Result := mainElement;
end;

{$endregion}




end.
