unit Core;

interface

uses
  DllHelper, Events, SysUtils, MainElement, Element, Generics.Collections, Classes, DataItem, DataItemGroup,
    FieldContainer;

type
  {$region 'TCore declaration'}
  TCore = class
  private
    FCoreStartEvent: TCoreStartEvent;

    procedure SetCoreStartEvent(Value: TCoreStartEvent);
    //procedure OnCoreStartEvent;
    function GetDataItemList(path: string): TObjectList<TDataItem>;
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


function TCore.GetDataItemList(path: string): TObjectList<TDataItem>;
var
   dataItem: TDataItem;
   keyValuePair: TKeyValuePair;

   _label, description: WideString;
   key:  WideString;
   color, value, fieldType:  WideString;
   minValue, maxValue, unitName, displayOrder: WideString;
   fs: TFormatSettings;

   dataItemList: TObjectList<TDataItem>;
   i, j, k, dataItemCount, keyValueListCount: integer;
   dataItemType: WideString;
begin
    fs := TFormatSettings.Create;
    fs.DateSeparator := '-';
    fs.ShortDateFormat := 'yyyy-MM-dd';


    dataItemList := TObjectList<TDataItem>.Create;
    dataItemCount := TDllHelper.GetInstance.GetInstance.Core_TemplatFromXml_DataItemCount(path);
    for j := 0 to dataItemCount - 1 do
    begin
        dataItemType := TDllHelper.GetInstance.Core_TemplatFromXml_GetDataItemType(path + '_' + IntToStr(j));
        if dataItemType = 'Picture' then
        begin
            dataItem := TPicture.Create;
            TDllHelper.GetInstance.Core_TemplatFromXml_GetPicture(path + '_' + IntToStr(j), dataItem.Id, _label,
              description, dataItem.DisplayOrder, dataItem.Mandatory, color);
            dataItem._Label := _label;
            dataItem.Description := TCDataValue.Create;
            dataItem.Description.InderValue := description;
            dataItem.Color := color;
        end
        else if dataItemType = 'ShowPdf' then
        begin
            dataItem := TShowPdf.Create;
            TDllHelper.GetInstance.Core_TemplatFromXml_GetShowPdf(path + '_' + IntToStr(j), dataItem.Id, _label,
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
            TDllHelper.GetInstance.Core_TemplatFromXml_GetDate(path + '_' + IntToStr(j), dataItem.Id, _label,
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
            TDllHelper.GetInstance.Core_TemplatFromXml_GetSignature(path + '_' + IntToStr(j), dataItem.Id, _label,
              description, dataItem.DisplayOrder, dataItem.Mandatory, color);
            dataItem._Label := _label;
            dataItem.Description := TCDataValue.Create;
            dataItem.Description.InderValue := description;
            dataItem.Color := color;
        end
        else if dataItemType = 'SaveButton' then
        begin
            dataItem := TSaveButton.Create;
            TDllHelper.GetInstance.Core_TemplatFromXml_GetSaveButton(path + '_' + IntToStr(j), dataItem.Id, _label,
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
            TDllHelper.GetInstance.Core_TemplatFromXml_GetTimer(path + '_' + IntToStr(j), dataItem.Id, _label,
              description, dataItem.DisplayOrder, (dataItem as TTimer).StopOnSave, dataItem.Mandatory);
            dataItem._Label := _label;
            dataItem.Description := TCDataValue.Create;
            dataItem.Description.InderValue := description;
            dataItem.Color := color;
        end
        else if dataItemType = 'None' then
        begin
            dataItem := TNone.Create;
            TDllHelper.GetInstance.Core_TemplatFromXml_GetNone(path + '_' + IntToStr(j), dataItem.Id, _label,
              description, dataItem.DisplayOrder);
            dataItem._Label := _label;
            dataItem.Description := TCDataValue.Create;
            dataItem.Description.InderValue := description;
        end
        else if dataItemType = 'CheckBox' then
        begin
            dataItem := TCheckBox.Create;
            TDllHelper.GetInstance.Core_TemplatFromXml_GetCheckBox(path + '_' + IntToStr(j), dataItem.Id, _label,
              description, dataItem.DisplayOrder, dataItem.Mandatory, (dataItem as TCheckBox).Selected);
            dataItem._Label := _label;
            dataItem.Description := TCDataValue.Create;
            dataItem.Description.InderValue := description;
        end
        else if dataItemType = 'MultiSelect' then
        begin
            dataItem := TMultiSelect.Create;
            TDllHelper.GetInstance.Core_TemplatFromXml_GetMultiSelect(path + '_' + IntToStr(j), dataItem.Id, _label,
              description, dataItem.DisplayOrder, dataItem.Mandatory);
            dataItem._Label := _label;
            dataItem.Description := TCDataValue.Create;
            dataItem.Description.InderValue := description;

            keyValueListCount :=  TDllHelper.GetInstance.Core_TemplatFromXml_KeyValueListCount(path + '_' + IntToStr(j) + '_MultiSelect');
            (dataItem as TMultiSelect).KeyValuePairList := TObjectList<TKeyValuePair>.Create();
            for k := 0 to keyValueListCount - 1 do
            begin
               keyValuePair := TKeyValuePair.Create();
               TDllHelper.GetInstance.Core_TemplatFromXml_GetKeyValuePair(path + '_' + IntToStr(j) + '_MultiSelect_' + IntToStr(k), key, value,
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
            TDllHelper.GetInstance.Core_TemplatFromXml_GetSingleSelect(path + '_' + IntToStr(j), dataItem.Id, _label,
              description, dataItem.DisplayOrder, dataItem.Mandatory);
            dataItem._Label := _label;
            dataItem.Description := TCDataValue.Create;
            dataItem.Description.InderValue := description;

            keyValueListCount :=  TDllHelper.GetInstance.Core_TemplatFromXml_KeyValueListCount(path +'_' + IntToStr(j) + '_SingleSelect');
            (dataItem as TMultiSelect).KeyValuePairList := TObjectList<TKeyValuePair>.Create();
            for k := 0 to keyValueListCount - 1 do
            begin
               keyValuePair := TKeyValuePair.Create();
               TDllHelper.GetInstance.Core_TemplatFromXml_GetKeyValuePair(path + '_' + IntToStr(j) + '_SingleSelect_' + IntToStr(k), key, value,
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
            TDllHelper.GetInstance.Core_TemplatFromXml_GetNumber(path + '_' + IntToStr(j), dataItem.Id, _label,
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
            TDllHelper.GetInstance.Core_TemplatFromXml_GetText(path + '_' + IntToStr(j), dataItem.Id, _label,
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
            TDllHelper.GetInstance.Core_TemplatFromXml_GetComment(path + '_' + IntToStr(j), dataItem.Id, _label,
              description, (dataItem as TComment).SplitScreen, value, dataItem.Mandatory
              , dataItem.ReadOnly);
            dataItem._Label := _label;
            dataItem.Description := TCDataValue.Create;
            dataItem.Description.InderValue := description;
            (dataItem as TComment).Value := value;
        end
        else if dataItemType = 'FieldContainer' then
        begin
            dataItem := TFieldContainer.Create;
            TDllHelper.GetInstance.Core_TemplatFromXml_GetFieldContainer(path + '_' + IntToStr(j),
              value, fieldType);
            (dataItem as TFieldContainer).Value := value;
            (dataItem as TFieldContainer).FieldType := fieldType;
            (dataItem as TFieldContainer).DataItemList := GetDataItemList(path + '_' + IntToStr(j));
        end;
        dataItemList.Add(dataItem);
      end;
   result := dataItemList;
end;

function TCore.TemplatFromXml(xml: string): TMainElement;
var
  mainElement: TMainElement;
  element: TElement;
  _label, elementType, checkListFolderName: WideString;
  startDate, endDate, language, caseType: WideString;
  description: WideString;
  fs: TFormatSettings;
  i, dataItemGroupCount: integer;
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
          (element as TDataElement).DataItemList := GetDataItemList('DataElement_' + IntToStr(i));

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
