unit Samples;

interface

uses
  Core, SysUtils, MainElement, IOUtils, Element, DataItem,  Generics.Collections, Classes,
  FieldContainer, DataItemGroup, System.Classes;

type
  {$region 'TSamples declaration'}
  TSamples = class
  private
      ServerConnectionString: string;
      Core: TCore;

      procedure Print(mainElement: TMainElement);
      procedure PrintDataItemList(dataItemList: TObjectList<TDataItem>; offset: string);
      procedure PrintDataItemGroupList(dataItemGroupList: TObjectList<TDataItemGroup>; offset: string);
      procedure PrintKeyValuePairList(list:  TObjectList<TKeyValuePair>; offset: string);
      procedure PrintValidationErrors(validationErrors: TStringList; offset: string);
      procedure Sample1;
      function GetDefaultFileName: string;
  public
      constructor Create(serverConnectionString: string);
      procedure Run;
  end;
  {$endregion}

implementation

{$region 'TSamples implementation'}
constructor TSamples.Create(serverConnectionString: string);
begin
   self.ServerConnectionString := serverConnectionString;
   Core := TCore.Create;
end;

procedure TSamples.Run;
var
  input: string;
begin

  while true do
  begin

    {$region 'text'}
    WriteLn('');
    WriteLn('Select which MainController element to start. Type:');
    WriteLn('');
    WriteLn('  ''E'', for exiting program');
    WriteLn('  ''1'', for sample 1 (Working with templates)');
    ReadLn(input);
    {$endregion}


    if UpperCase(input) = 'E' then
      break
    else if input = '1' then
      Sample1();
  end;
end;

function TSamples.GetDefaultFileName: string;
begin
  //  result := 'field_groups_example.xml';
  //  result := 'options_with_microting_example.xml';
  //  result := 'picture_test.xml';
  //  result := 'picture_signature_example.xml';
    result := 'pdf_test.xml';
  //  result := 'date_example.xml';
end;

procedure TSamples.Sample1;
var
   input: string;
   filename: string;
   mainElement: TMainElement;
   xml: WideString;
   templateId: integer;
   validationErrors: TStringList;
begin
   Core.Start(ServerConnectionString);

   while true do
   begin
       WriteLn('');
       WriteLn('  Type ''X'' Read main element from xml file and output it''s content to console (TemplatFromXml test)');
       WriteLn('  Type ''C'' Read main element from xml file and create it in database (TemplateCreate test)');
       WriteLn('  Type ''R'' Read main element from database by templateId and output it''s content to console (TemplateRead test)');
       WriteLn('  Type ''V'' Read main element from xml file and validate it (TemplateValidation test)');
       WriteLn('  Type ''Q'' to quit');
       WriteLn('  As long as the Core left running, the system is able to process eForms');
       ReadLn(input);
       if UpperCase(input) = 'Q' then
          break
       else if UpperCase(input) = 'X' then
       begin
          WriteLn('');
          WriteLn('  Type file name ('+ GetDefaultFileName +' by default):');
          ReadLn(input);
          filename := input;
          if filename = '' then
            filename := GetDefaultFileName;


          xml := TFile.ReadAllText(filename);
          mainElement := Core.TemplatFromXml(xml);
          Print(mainElement);
       end
       else if UpperCase(input) = 'C' then
       begin
          WriteLn('');
          WriteLn('  Type file name ('+ GetDefaultFileName +' by default):');
          ReadLn(input);
          filename := input;
          if filename = '' then
              filename := GetDefaultFileName();


          xml := TFile.ReadAllText(filename);
          mainElement := Core.TemplatFromXml(xml);
          templateId := Core.TemplateCreate(mainElement);
          WriteLn('  MainElement was successfully created in database, templateId: ' + IntToStr(templateId));
       end
       else if UpperCase(input) = 'R' then
       begin
          WriteLn('');
          WriteLn('  Type templateId: ');
          ReadLn(input);
          mainElement.Free;
          mainElement := Core.TemplateRead(StrToInt(input));
          Print(mainElement);
       end
       else if UpperCase(input) = 'V' then
       begin
          WriteLn('');
          WriteLn('  Type file name ('+ GetDefaultFileName +' by default):');
          ReadLn(input);
          filename := input;
          if filename = '' then
            filename := GetDefaultFileName;


          xml := TFile.ReadAllText(filename);
          mainElement := Core.TemplatFromXml2(xml);
          validationErrors := Core.TemplateValidation(mainElement);
          PrintValidationErrors(validationErrors, '  ');
       end;
   end;
end;

procedure TSamples.Print(mainElement: TMainElement);
var 
  element: TElement;  
  dataElement: TDataElement;
begin
   WriteLn('');
   WriteLn('Main element:');
   WriteLn('Id: ' + IntToStr(mainElement.Id));
   WriteLn('Label: ' + mainElement._Label);
   WriteLn('DisplayOrder: ' + IntToStr(mainElement.DisplayOrder));
   WriteLn('CheckListFolderName: ' + mainElement.CheckListFolderName);
   WriteLn('Repeated: ' + IntToStr(mainElement.Repeated));
   WriteLn('StartDate: ' + DateToStr(mainElement.StartDate));
   WriteLn('EndDate: ' + DateToStr(mainElement.EndDate));
   WriteLn('Language: ' + mainElement.Language);
   WriteLn('MultiApproval: ' + BoolToStr(mainElement.MultiApproval));
   WriteLn('FastNavigation: ' + BoolToStr(mainElement.FastNavigation));
   WriteLn('ManualSync: ' + BoolToStr(mainElement.ManualSync));
   WriteLn('CaseType: ' + mainElement.CaseType);
   WriteLn('ElementList:');
   for element in mainElement.ElementList do
   begin
       WriteLn('  Element:');
       if element is TDataElement then
       begin
         WriteLn('  Type: DataElement');
         dataElement := element as TDataElement;
         WriteLn('  Id: ' + IntToStr(dataElement.Id));
         WriteLn('  Label: ' + dataElement._Label);
         WriteLn('  Description: ' + dataElement.Description.InderValue);
         WriteLn('  DisplayOrder: ' + IntToStr(dataElement.DisplayOrder));
         WriteLn('  ReviewEnabled: ' + BoolToStr(dataElement.ReviewEnabled));
        // WriteLn('  ManualSync: ' + BoolToStr(element.ManualSync));
         WriteLn('  ExtraFieldsEnabled: ' + BoolToStr(dataElement.ExtraFieldsEnabled));
        // WriteLn('  DoneButtonDisabled: ' + BoolToStr(element.DoneButtonDisabled));
         WriteLn('  ApprovalEnabled: ' + BoolToStr(dataElement.ApprovalEnabled));
         {$region 'DataItemList'}
         if dataElement.DataItemList.Count > 0 then
         begin
            WriteLn('  DataItemList:');
            PrintDataItemList(dataElement.DataItemList,'    ');
         end;
         {$endregion}
         {$region 'DataItemGroupList'}
         if dataElement.DataItemGroupList.Count > 0 then
         begin
            WriteLn('  DataItemGroupList:');
            PrintDataItemGroupList(dataElement.DataItemGroupList,'    ');
         end;
         {$endregion}
       end;

   end;
   WriteLn('');
end;

procedure TSamples.PrintDataItemList(dataItemList: TObjectList<TDataItem>; offset: string);
var
  dataItem: TDataItem;
  picture: TPicture;
  showPdf: TShowPdf;
  signature: TSignature;
  date: TDate;
  checkBox: TCheckBox;
  saveButton: TSaveButton;
  timer: TTimer;
  none: TNone;
  multiSelect: TMultiSelect;
  singleSelect: TSingleSelect;
  number: TNumber;
  text: TText;
  comment: TComment;
  fieldContainer: TFieldContainer;
begin
   for dataItem in dataItemList do
   begin
      WriteLn(offset + 'DataItem:');
      if dataItem is TPicture then
      begin
          picture := dataItem as TPicture;
          WriteLn(offset + 'Type: Picture');
          WriteLn(offset + 'Id: ' + IntToStr(picture.Id));
          WriteLn(offset + 'Label: ' + picture._Label);
          WriteLn(offset + 'Description: ' + picture.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(picture.DisplayOrder));
          WriteLn(offset + 'Mandatory: ' + BoolToStr(picture.Mandatory));
          WriteLn(offset + 'Color: ' + picture.Color);
      end
      else if dataItem is TShowPdf then
      begin
          showPdf := dataItem as TShowPdf;
          WriteLn(offset + 'Type: ShowPdf');
          WriteLn(offset + 'Id: ' + IntToStr(showPdf.Id));
          WriteLn(offset + 'Label: ' + showPdf._Label);
          WriteLn(offset + 'Description: ' + showPdf.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(showPdf.DisplayOrder));
          WriteLn(offset + 'Color: ' + showPdf.Color);
          WriteLn(offset + 'Value: ' + showPdf.Value);
      end
      else if dataItem is TDate then
      begin
          date := dataItem as TDate;
          WriteLn(offset + 'Type: Date');
          WriteLn(offset + 'Id: ' + IntToStr(date.Id));
          WriteLn(offset + 'Label: ' + date._Label);
          WriteLn(offset + 'Description: ' + date.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(date.DisplayOrder));
          WriteLn(offset + 'MinValue: ' + DateToStr(date.MinValue));
          WriteLn(offset + 'MaxValue: ' + DateToStr(date.MaxValue));
          WriteLn(offset + 'Mandatory: ' + BoolToStr(date.Mandatory));
          WriteLn(offset + 'ReadOnly: ' + BoolToStr(date.ReadOnly));
          WriteLn(offset + 'Color: ' + date.Color);
          WriteLn(offset + 'Value: ' + date.DefaultValue);
      end
      else if dataItem is TSignature then
      begin
          signature := dataItem as TSignature;
          WriteLn(offset + 'Type: Signature');
          WriteLn(offset + 'Id: ' + IntToStr(signature.Id));
          WriteLn(offset + 'Label: ' + signature._Label);
          WriteLn(offset + 'Description: ' + signature.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(signature.DisplayOrder));
          WriteLn(offset + 'Mandatory: ' + BoolToStr(signature.Mandatory));
          WriteLn(offset + 'Color: ' + signature.Color);
      end
      else if dataItem is TCheckBox then
      begin
          checkBox := dataItem as TCheckBox;
          WriteLn(offset + 'Type: CheckBox');
          WriteLn(offset + 'Id: ' + IntToStr(checkBox.Id));
          WriteLn(offset + 'Label: ' + checkBox._Label);
          WriteLn(offset + 'Description: ' + checkBox.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(checkBox.DisplayOrder));
          WriteLn(offset + 'Mandatory: ' + BoolToStr(checkBox.Mandatory));
          WriteLn(offset + 'Selected: ' + BoolToStr(checkBox.Selected));
      end
      else if dataItem is TSaveButton then
      begin
          saveButton := dataItem as TSaveButton;
          WriteLn(offset + 'Type: SaveButton');
          WriteLn(offset + 'Id: ' + IntToStr(saveButton.Id));
          WriteLn(offset + 'Label: ' + saveButton._Label);
          WriteLn(offset + 'Description: ' + saveButton.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(saveButton.DisplayOrder));
          WriteLn(offset + 'Value: ' + saveButton.Value);
      end
      else if dataItem is TTimer then
      begin
          timer := dataItem as TTimer;
          WriteLn(offset + 'Type: Timer');
          WriteLn(offset + 'Id: ' + IntToStr(timer.Id));
          WriteLn(offset + 'Label: ' + timer._Label);
          WriteLn(offset + 'Description: ' + timer.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(timer.DisplayOrder));
          WriteLn(offset + 'StopOnSave: ' + BoolToStr(timer.StopOnSave));
          WriteLn(offset + 'Mandatory: ' + BoolToStr(timer.Mandatory));
      end
      else if dataItem is TNone then
      begin
          none := dataItem as TNone;
          WriteLn(offset + 'Type: None');
          WriteLn(offset + 'Id: ' + IntToStr(none.Id));
          WriteLn(offset + 'Label: ' + none._Label);
          WriteLn(offset + 'Description: ' + none.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(none.DisplayOrder));
      end
      else if dataItem is TMultiSelect then
      begin
          multiSelect := dataItem as TMultiSelect;
          WriteLn(offset + 'Type: MultiSelect');
          WriteLn(offset + 'Id: ' + IntToStr(multiSelect.Id));
          WriteLn(offset + 'Label: ' + multiSelect._Label);
          WriteLn(offset + 'Description: ' + multiSelect.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(multiSelect.DisplayOrder));
          WriteLn(offset + 'Mandatory: ' + BoolToStr(multiSelect.Mandatory));
          WriteLn(offset + 'KeyValuePairList:');
          PrintKeyValuePairList(multiSelect.KeyValuePairList, '      ');
      end
      else if dataItem is TSingleSelect then
      begin
          singleSelect := dataItem as TSingleSelect;
          WriteLn(offset + 'Type: SingleSelect');
          WriteLn(offset + 'Id: ' + IntToStr(singleSelect.Id));
          WriteLn(offset + 'Label: ' + singleSelect._Label);
          WriteLn(offset + 'Description: ' + singleSelect.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(singleSelect.DisplayOrder));
          WriteLn(offset + 'Mandatory: ' + BoolToStr(singleSelect.Mandatory));
          WriteLn(offset + 'KeyValuePairList:');
          PrintKeyValuePairList(singleSelect.KeyValuePairList, '      ');
      end
      else if dataItem is TNumber then
      begin
          number := dataItem as TNumber;
          WriteLn(offset + 'Type: Number');
          WriteLn(offset + 'Id: ' + IntToStr(number.Id));
          WriteLn(offset + 'Label: ' + number._Label);
          WriteLn(offset + 'Description: ' + number.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(number.DisplayOrder));
          WriteLn(offset + 'MinValue: ' + number.MinValue);
          WriteLn(offset + 'MaxValue: ' + number.MaxValue);
          WriteLn(offset + 'Mandatory: ' + BoolToStr(number.Mandatory));
          WriteLn(offset + 'DecimalCount: ' + IntToStr(number.DecimalCount));
          WriteLn(offset + 'UnitName: ' + number.UnitName);
      end
      else if dataItem is TText then
      begin
          text := dataItem as TText;
          WriteLn(offset + 'Type: Text');
          WriteLn(offset + 'Id: ' + IntToStr(text.Id));
          WriteLn(offset + 'Label: ' + text._Label);
          WriteLn(offset + 'Description: ' + text.Description.InderValue);
          WriteLn(offset + 'GeolocationEnabled: ' + BoolToStr(text.GeolocationEnabled));
          WriteLn(offset + 'Value: ' + text.Value);
          WriteLn(offset + 'ReadOnly: ' + BoolToStr(text.ReadOnly));
          WriteLn(offset + 'Mandatory: ' + BoolToStr(text.Mandatory));
       end
      else if dataItem is TComment then
      begin
          comment := dataItem as TComment;
          WriteLn(offset + 'Type: Comment');
          WriteLn(offset + 'Id: ' + IntToStr(comment.Id));
          WriteLn(offset + 'Label: ' + comment._Label);
          WriteLn(offset + 'Description: ' + comment.Description.InderValue);
          WriteLn(offset + 'SplitScreen: ' + BoolToStr(comment.SplitScreen));
          WriteLn(offset + 'Value: ' + comment.Value);
          WriteLn(offset + 'ReadOnly: ' + BoolToStr(comment.ReadOnly));
          WriteLn(offset + 'Mandatory: ' + BoolToStr(comment.Mandatory));
       end
       else if dataItem is TFieldContainer then
       begin
          fieldContainer := dataItem as TFieldContainer;
          WriteLn(offset + 'Type: Comment');
          WriteLn(offset + 'Value: ' + fieldContainer.Value);
          WriteLn(offset + 'FieldType: ' + fieldContainer.FieldType);
          PrintDataItemList(fieldContainer.DataItemList, offset + '    ');
       end;

   end;
end;

procedure TSamples.PrintKeyValuePairList(list:  TObjectList<TKeyValuePair>; offset: string);
var
  pair: TKeyValuePair;
begin

    for pair in list do
    begin
      WriteLn(offset + 'KeyValuePair:');
      WriteLn(offset + 'Key: ' + pair.Key);
      WriteLn(offset + 'Value: ' + pair.Value);
      WriteLn(offset + 'DisplayOrder: ' + pair.DisplayOrder);
      WriteLn(offset + 'Selected: ' + BoolToStr(pair.Selected));
    end;
end;

procedure TSamples.PrintDataItemGroupList(dataItemGroupList: TObjectList<TDataItemGroup>; offset: string);
var
  dataItemGroup: TDataItemGroup;
begin
   for dataItemGroup in dataItemGroupList do
   begin
     WriteLn(offset + 'DataItemGroup:');
     WriteLn(offset + 'Id: ' + dataItemGroup.Id);
     WriteLn(offset + 'Label: ' + dataItemGroup._Label);
     WriteLn(offset + 'Description: ' + dataItemGroup.Description);
     WriteLn(offset + 'DisplayOrder: ' + IntToStr(dataItemGroup.DisplayOrder));
     WriteLn(offset + 'Color: ' + dataItemGroup.Color);
     WriteLn(offset + 'Value: ' + dataItemGroup.Value);
     WriteLn(offset + 'DataItemList:');
     PrintDataItemList(dataItemGroup.DataItemList, offset + '  ');
   end;
end;

procedure TSamples.PrintValidationErrors(validationErrors: TStringList; offset: string);
var
  error: String;
begin
  WriteLn(offset + 'Validation errors:');
  if (validationErrors.Count = 0) then
    WriteLn(offset + 'None')
  else
  begin
    for error in validationErrors do
      WriteLn(offset + error);
  end;
end;

{$endregion}

end.
