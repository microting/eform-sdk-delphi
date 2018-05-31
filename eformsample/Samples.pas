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


      procedure PrintMainElement(mainElement: TMainElement);
      procedure PrintReplyElement(replyElement: TReplyElement);
      procedure PrintElementList(elementList: TObjectList<TElement>; offset: string);
      procedure PrintDataItemList(dataItemList: TObjectList<TDataItem>; offset: string);
      procedure PrintDataItemGroupList(dataItemGroupList: TObjectList<TDataItemGroup>; offset: string);
      procedure PrintKeyValuePairList(list:  TObjectList<TKeyValuePair>; offset: string);
      procedure PrintFieldValues(list: TObjectList<TFieldValue>; offset: string);
      procedure PrintUploadedData(uploadedData: TUploadedData; offset: string);
      procedure PrintValidationErrors(validationErrors: TStringList; offset: string);
      procedure PrintTemplateDto(templateDto: TTemplate_Dto);
      procedure PrintFieldDto(fieldDto: TField_Dto);
      function GetDefaultFileName: string;

      procedure Sample1;
      procedure Sample2;
      procedure Sample3;
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
    WriteLn('  ''2'', for sample 2 (Working with items)');
    WriteLn('  ''3'', for sample 3 (Working with cases)');
    ReadLn(input);
    {$endregion}


    if UpperCase(input) = 'E' then
      break
    else if input = '1' then
      Sample1()
    else if input = '2' then
      Sample2()
    else if input = '3' then
      Sample3();
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
   result: boolean;
begin
   Core.Start(ServerConnectionString);

   while true do
   begin
       WriteLn('');
       WriteLn('  Type ''X'' Read main element from xml file and output it''s content to console (TemplatFromXml test)');
       WriteLn('  Type ''C'' Read main element from xml file and create it in database (TemplateCreate test)');
       WriteLn('  Type ''R'' Read main element from database by templateId and output it''s content to console (TemplateRead test)');
       WriteLn('  Type ''V'' Read main element from xml file and validate it (TemplateValidation test)');
       WriteLn('  Type ''D'' Delete template by templateId (TemplateDelete test)');
       WriteLn('  Type ''U'' Read main element from database by templateId and upload its data (TemplateUploadData test)');
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
          PrintMainElement(mainElement);
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
          if (mainElement <> nil) then
             mainElement.Free;
          mainElement := Core.TemplateRead(StrToInt(input));
          PrintMainElement(mainElement);
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
          mainElement := Core.TemplatFromXml(xml);
          validationErrors := Core.TemplateValidation(mainElement);
          PrintValidationErrors(validationErrors, '  ');
       end
       else if UpperCase(input) = 'D' then
       begin
          WriteLn('');
          WriteLn('  Type templateId: ');
          ReadLn(input);

          templateId := StrToInt(input);
          result := Core.TemplateDelete(templateId);
          WriteLn('');
          WriteLn('Delete result: ' + BoolToStr(result));
       end
       else if UpperCase(input) = 'U' then
       begin
          WriteLn('');
          WriteLn('  Type templateId: ');
          ReadLn(input);
          if (mainElement <> nil) then
             mainElement.Free;

          templateId := StrToInt(input);
          mainElement := Core.TemplateRead(templateId);
          mainElement := Core.TemplateUploadData(mainElement);
          PrintMainElement(mainElement);
       end
   end;
end;

procedure TSamples.PrintMainElement(mainElement: TMainElement);
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
   PrintElementList(mainElement.ElementList,'  ');
end;

procedure TSamples.PrintElementList(elementList: TObjectList<TElement>; offset: string);
var
  element: TElement;
  dataElement: TDataElement;
  checkListValue: TCheckListValue;
begin
   for element in elementList do
   begin
       WriteLn(offset + 'Element:');
       if element is TDataElement then
       begin
         WriteLn(offset + 'Type: DataElement');
         dataElement := element as TDataElement;
         WriteLn(offset + 'Id: ' + IntToStr(dataElement.Id));
         WriteLn(offset + 'Label: ' + dataElement._Label);
         WriteLn(offset + 'Description: ' + dataElement.Description.InderValue);
         WriteLn(offset + 'DisplayOrder: ' + IntToStr(dataElement.DisplayOrder));
         WriteLn(offset + 'ReviewEnabled: ' + BoolToStr(dataElement.ReviewEnabled));
        // WriteLn('  ManualSync: ' + BoolToStr(element.ManualSync));
         WriteLn(offset + 'ExtraFieldsEnabled: ' + BoolToStr(dataElement.ExtraFieldsEnabled));
        // WriteLn('  DoneButtonDisabled: ' + BoolToStr(element.DoneButtonDisabled));
         WriteLn(offset + 'ApprovalEnabled: ' + BoolToStr(dataElement.ApprovalEnabled));
         {$region 'DataItemList'}
         if dataElement.DataItemList.Count > 0 then
         begin
            WriteLn(offset + 'DataItemList:');
            PrintDataItemList(dataElement.DataItemList,offset + '  ');
         end;
         {$endregion}
         {$region 'DataItemGroupList'}
         if dataElement.DataItemGroupList.Count > 0 then
         begin
            WriteLn('  DataItemGroupList:');
            PrintDataItemGroupList(dataElement.DataItemGroupList,offset + '  ');
         end;
         {$endregion}
       end
       else if element is TCheckListValue then
       begin
         WriteLn(offset + 'Type: CheckListValue');
         checkListValue := element as TCheckListValue;
         WriteLn(offset + 'Id: ' + IntToStr(checkListValue.Id));
         WriteLn(offset + 'Label: ' + checkListValue._Label);
         WriteLn(offset + 'Description: ' + checkListValue.Description.InderValue);
         WriteLn(offset + 'DisplayOrder: ' + IntToStr(checkListValue.DisplayOrder));
         WriteLn(offset + 'ReviewEnabled: ' + BoolToStr(checkListValue.ReviewEnabled));
         WriteLn(offset + 'ExtraFieldsEnabled: ' + BoolToStr(checkListValue.ExtraFieldsEnabled));
         WriteLn(offset + 'ApprovalEnabled: ' + BoolToStr(checkListValue.ApprovalEnabled));
         {$region 'DataItemList'}
         if checkListValue.DataItemList.Count > 0 then
         begin
            WriteLn(offset + 'DataItemList:');
            PrintDataItemList(checkListValue.DataItemList,offset + '  ');
         end;
         {$endregion}
         {$region 'DataItemGroupList'}
         if checkListValue.DataItemGroupList.Count > 0 then
         begin
            WriteLn('  DataItemGroupList:');
            PrintDataItemGroupList(checkListValue.DataItemGroupList,offset + '  ');
         end;
         {$endregion}
       end;


   end;
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
  field: TField;
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
       end
       else if dataItem is TField then
       begin
          field := dataItem as TField;
          WriteLn(offset + 'Type: Field');
          WriteLn(offset + 'Id: ' + IntToStr(field.Id));
          WriteLn(offset + 'Label: ' + field._Label);
          WriteLn(offset + 'Description: ' + field.Description.InderValue);
          WriteLn(offset + 'DisplayOrder: ' + IntToStr(field.DisplayOrder));
          WriteLn(offset + 'Mandatory: ' + BoolToStr(field.Mandatory));
          WriteLn(offset + 'Color: ' + field.Color);
          PrintKeyValuePairList(field.KeyValuePairList, offset + '  ');
          PrintFieldValues(field.FieldValues, offset + '  ');
       end;

   end;
end;


procedure TSamples.PrintFieldValues(list: TObjectList<TFieldValue>; offset: string);
var
  fieldValue: TFieldValue;
begin
    for fieldValue in list do
    begin
        WriteLn(offset + 'FieldValue:');
        WriteLn(offset + 'Key: ' + IntToStr(fieldValue.FieldId));
        WriteLn(offset + 'FieldType: ' + fieldValue.FieldType);
        WriteLn(offset + 'DateOfDoing: ' + DateToStr(fieldValue.DateOfDoing));
        WriteLn(offset + 'Value: ' + fieldValue.Value);
        WriteLn(offset + 'MicrotingUuid: ' + fieldValue.MicrotingUuid);
        WriteLn(offset + 'ValueReadable: ' + fieldValue.ValueReadable);
        WriteLn(offset + 'Latitude: ' + fieldValue.Latitude);
        WriteLn(offset + 'Longitude: ' + fieldValue.Longitude);
        WriteLn(offset + 'Altitude: ' + fieldValue.Altitude);
        WriteLn(offset + 'Heading: ' + fieldValue.Heading);
        WriteLn(offset + 'Accuracy: ' + fieldValue.Accuracy);
        WriteLn(offset + 'Date: ' + DateToStr(fieldValue.Date));
        WriteLn(offset + 'UploadedData: ' + fieldValue.UploadedData);
        PrintUploadedData(fieldValue.UploadedDataObj, offset + '  ');
        PrintKeyValuePairList(fieldValue.KeyValuePairList, offset + '  ');
     end;
end;


procedure TSamples.PrintUploadedData(uploadedData: TUploadedData; offset: string);
begin
    WriteLn(offset + 'UploadedData:');
    WriteLn(offset + 'Checksum: ' + uploadedData.Checksum);
    WriteLn(offset + 'Extension: ' + uploadedData.Extension);
    WriteLn(offset + 'CurrentFile: ' + uploadedData.CurrentFile);
    WriteLn(offset + 'UploaderId: ' + IntToStr(uploadedData.UploaderId));
    WriteLn(offset + 'UploaderType: ' + uploadedData.UploaderType);
    WriteLn(offset + 'FileLocation: ' + uploadedData.FileLocation);
    WriteLn(offset + 'FileName: ' + uploadedData.FileName);
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

procedure TSamples.Sample2;
var
  input: string;
  siteNameDtoList: TObjectList<TSiteName_Dto>;
  siteNameDto: TSiteName_Dto;
  templateDto: TTemplate_Dto;
begin
   Core.Start(ServerConnectionString);

   while true do
   begin
       WriteLn('');
       WriteLn('  Type ''A'' Read all site items (Advanced_SiteItemReadAll test)');
       WriteLn('  Type ''R'' Get Template_Dto object using templateId (TemplateItemRead test)');
       WriteLn('  Type ''Q'' to quit');
       WriteLn('  As long as the Core left running, the system is able to process eForms');
       ReadLn(input);
       if UpperCase(input) = 'Q' then
          break
       else if UpperCase(input) = 'A' then
       begin
          WriteLn('');
          siteNameDtoList := Core.Advanced_SiteItemReadAll();
          for siteNameDto in siteNameDtoList do
          begin
            WriteLn('SiteName_Dto:');
            WriteLn(siteNameDto.ToString);
          end;
       end
       else if UpperCase(input) = 'R' then
       begin
          WriteLn('');
          WriteLn('  Type templateId: ');
          ReadLn(input);
          templateDto := Core.TemplateItemRead(StrToInt(input));
          PrintTemplateDto(templateDto);
       end
    end;
end;

procedure TSamples.PrintFieldDto(fieldDto: TField_Dto);
begin
  WriteLn('FieldDto:');
  if fieldDto = nil then
    exit;
  WriteLn('Id: ' + IntToStr(fieldDto.Id));
  WriteLn('Label: ' + fieldDto._Label);
  WriteLn('Description: ' + fieldDto.Description);
  WriteLn('FieldTypeId: ' + IntToStr(fieldDto.FieldTypeId));
  WriteLn('FieldType: ' + fieldDto.FieldType);
  WriteLn('CheckListId: ' + IntToStr(fieldDto.CheckListId));
end;

procedure TSamples.PrintTemplateDto(templateDto: TTemplate_Dto);
var
  i: integer;
begin
   WriteLn('TemplateDto:');
   WriteLn('Id: ' + IntToStr(templateDto.Id));
   WriteLn('Label: ' + templateDto._Label);
   WriteLn('Description: ' + templateDto.Descripition);
   WriteLn('CreatedAt: ' + DateToStr(templateDto.CreatedAt));
   WriteLn('UpdatedAt: ' + DateToStr(templateDto.UpdatedAt));
   WriteLn('Repeated: ' + IntToStr(templateDto.Repeated));
   WriteLn('FolderName: ' + templateDto.FolderName);
   WriteLn('WorkflowState: ' + templateDto.WorkflowState);
   for i := 0 to templateDto.DeployedSites.Count-1 do
      WriteLn('SiteName_Dto: ' + templateDto.DeployedSites[i].ToString);
   WriteLn('HasCases: ' + BoolToStr(templateDto.HasCases));
   WriteLn('DisplayIndex: ' + IntToStr(templateDto.DisplayIndex));
   PrintFieldDto(templateDto.Field1);
   PrintFieldDto(templateDto.Field2);
   PrintFieldDto(templateDto.Field3);
   PrintFieldDto(templateDto.Field4);
   PrintFieldDto(templateDto.Field5);
   PrintFieldDto(templateDto.Field6);
   PrintFieldDto(templateDto.Field7);
   PrintFieldDto(templateDto.Field8);
   PrintFieldDto(templateDto.Field9);
   PrintFieldDto(templateDto.Field10);
   WriteLn('Tags:');
   PrintKeyValuePairList(templateDto.Tags,'  ');
end;

procedure TSamples.Sample3;
var
  mainElement: TMainElement;
  replyElement: TReplyElement;

  input: string;
  templateId: integer;
  microtingUId: string;
  checkUId: string;

  siteUId: integer;
  resultCase: string;

  siteUIds: TList<integer>;
  resultCases: TStringList;
  parts: TArray<string>;
  i: integer;
  custom: string;
  result: boolean;
begin
   Core.Start(ServerConnectionString);

   while true do
   begin
       WriteLn('');
       WriteLn('  Type ''C'' Read main element from database by templateId and siteUId and create eForm case  (CaseCreate test)');
       WriteLn('  Type ''M'' Read main element from database by templateId and multiple siteUIds and create eForm case  (CaseCreate test)');
       WriteLn('  Type ''R'' Read case database by microtingUId and checkUId (CaseRead test)');
       WriteLn('  Type ''D'' Delete case by microtingUId (CaseDelete test)');
       WriteLn('  Type ''Q'' to quit');
       WriteLn('  As long as the Core left running, the system is able to process eForms');
       ReadLn(input);
       if UpperCase(input) = 'Q' then
          break
       else if UpperCase(input) = 'C' then
       begin
          WriteLn('');
          WriteLn('');
          WriteLn('  Type templateId: ');
          ReadLn(input);
          templateId := StrToInt(input);

          WriteLn('  Type siteUId:');
          ReadLn(input);
          siteUId := StrToInt(input);

          mainElement := Core.TemplateRead(templateId);
          resultCase := Core.CaseCreate(mainElement, '', siteUId);
          WriteLn('Result: ');
          WriteLn(resultCase);
       end
       else if UpperCase(input) = 'M' then
       begin
          WriteLn('');
          WriteLn('  Type templateId: ');
          ReadLn(input);
          templateId := StrToInt(input);

          WriteLn('  Type siteUIds (comma separated):');
          ReadLn(input);
          siteUIds := TList<integer>.Create;
          parts := input.Split([',']);
          for i:=0 to Length(parts) - 1 do
            siteUIds.Add(StrToInt(parts[i]));

          WriteLn('  Type custom: ');
          ReadLn(input);
          custom := input;

          mainElement := Core.TemplateRead(templateId);
          resultCases := Core.CaseCreate(mainElement, '', siteUIds, custom);
          WriteLn('Results: ');
          for i := 0 to resultCases.Count - 1 do
              WriteLn(resultCases[i]);
       end
       else if UpperCase(input) = 'R' then
       begin
          WriteLn('');
          WriteLn('  Type microtingUId: ');
          ReadLn(input);
          microtingUId := input;

          WriteLn('  Type checkUId');
          ReadLn(input);
          checkUId := input;

          replyElement := Core.CaseRead(microtingUId, checkUId);
          PrintReplyElement(replyElement);
       end
       else if UpperCase(input) = 'D' then
       begin
          WriteLn('');
          WriteLn('  Type microtingUId: ');
          ReadLn(input);
          microtingUId := input;

          result := Core.CaseDelete(microtingUId);
          WriteLn('');
          WriteLn('Result of delete case: ' + BoolToStr(result));
       end
    end;
end;


procedure TSamples.PrintReplyElement(replyElement: TReplyElement);
begin
   WriteLn('');
   WriteLn('Reply element:');
   WriteLn('Id: ' + IntToStr(replyElement.Id));
   WriteLn('Label: ' + replyElement._Label);
   WriteLn('DisplayOrder: ' + IntToStr(replyElement.DisplayOrder));
   WriteLn('CheckListFolderName: ' + replyElement.CheckListFolderName);
   WriteLn('Repeated: ' + IntToStr(replyElement.Repeated));
   WriteLn('StartDate: ' + DateToStr(replyElement.StartDate));
   WriteLn('EndDate: ' + DateToStr(replyElement.EndDate));
   WriteLn('Language: ' + replyElement.Language);
   WriteLn('MultiApproval: ' + BoolToStr(replyElement.MultiApproval));
   WriteLn('FastNavigation: ' + BoolToStr(replyElement.FastNavigation));
   WriteLn('ManualSync: ' + BoolToStr(replyElement.ManualSync));
   WriteLn('CaseType: ' + replyElement.CaseType);
   WriteLn('UnitId: ' + IntToStr(replyElement.UnitId));
   WriteLn('DoneById: ' + IntToStr(replyElement.DoneById));
   WriteLn('DoneAt: ' + DateToStr(replyElement.DoneAt));
   WriteLn('Custom: ' + replyElement.Custom);
   WriteLn('ElementList:');
   PrintElementList(replyElement.ElementList,'  ');
end;
{$endregion}

end.
