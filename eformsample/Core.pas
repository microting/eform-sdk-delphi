unit Core;

interface

uses
  DllHelper, Events, SysUtils, MainElement, Element, Generics.Collections, Classes, DataItem;

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

  _label, description: WideString;
  checkListFolderName, startDate, endDate: WideString;
  language, caseType, color, value:  WideString;
  minValue, maxValue: WideString;
  fs: TFormatSettings;

  i, j: integer;
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
          (element as TDataElement).DataItemList := TObjectList<TDataItem>.Create;
          for j := 0 to TDllHelper.GetInstance.GetInstance.Core_TemplatFromXml_DataElement_DataItemCount(i) - 1 do
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
              end;
            end;

            (element as TDataElement).DataItemList.Add(dataItem);
      end;
      mainElement.ElementList.Add(element);
  end;

  Result := mainElement;
end;

{$endregion}




end.
