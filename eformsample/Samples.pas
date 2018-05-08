unit Samples;

interface

uses
  Core, SysUtils, MainElement, IOUtils, Element, DataItem,  Generics.Collections, Classes;

type
  {$region 'TSamples declaration'}
  TSamples = class
  private
      ServerConnectionString: string;
      Core: TCore;

      procedure Print(mainElement: TMainElement);
      procedure PrintKeyValuePairList(list:  TObjectList<TKeyValuePair>; offset: string);
      procedure Sample1;
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
    WriteLn('  ''1'', for sample 1 (Create main element)');
    ReadLn(input);
    {$endregion}


    if UpperCase(input) = 'E' then
      break
    else if input = '1' then
      Sample1();
  end;
end;

procedure TSamples.Sample1;
var
   input: string;
   filename: string;
   mainElement: TMainElement;
   xml: WideString;
begin
   Core.Start(ServerConnectionString);

   while true do
   begin
       WriteLn('');
       WriteLn('  Type ''M'' Create main element from xml file and output it''s content to console');
       WriteLn('  Type ''Q'' to quit');
       WriteLn('  As long as the Core left running, the system is able to process eForms');
       ReadLn(input);
       if UpperCase(input) = 'Q' then
          break
       else if UpperCase(input) = 'M' then
       begin
          WriteLn('');
          WriteLn('    Type file name (picture_test.xml by default)');
          ReadLn(input);
          filename := input;
          if filename = '' then
            filename := 'options_with_microting_example.xml';
           //filename := 'picture_test.xml';
           //  filename := 'picture_signature_example.xml';
            //filename := 'pdf_test.xml';
           //  filename := 'date_example.xml';


          xml := TFile.ReadAllText(filename);
          mainElement := Core.TemplatFromXml(xml);
          Print(mainElement);
       end;
   end;
end;

procedure TSamples.Print(mainElement: TMainElement);
var 
  element: TElement;  
  dataElement: TDataElement;

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
         WriteLn('  DataItemList:');
         for dataItem in dataElement.DataItemList do
         begin
            WriteLn('    DataItem:');
            if dataItem is TPicture then
            begin
                picture := dataItem as TPicture;
                WriteLn('    Type: Picture');
                WriteLn('    Id: ' + IntToStr(picture.Id));
                WriteLn('    Label: ' + picture._Label);
                WriteLn('    Description: ' + picture.Description.InderValue);
                WriteLn('    DisplayOrder: ' + IntToStr(picture.DisplayOrder));
                WriteLn('    Mandatory: ' + BoolToStr(picture.Mandatory));
                WriteLn('    Color: ' + picture.Color);
            end
            else if dataItem is TShowPdf then
            begin
                showPdf := dataItem as TShowPdf;
                WriteLn('    Type: ShowPdf');
                WriteLn('    Id: ' + IntToStr(showPdf.Id));
                WriteLn('    Label: ' + showPdf._Label);
                WriteLn('    Description: ' + showPdf.Description.InderValue);
                WriteLn('    DisplayOrder: ' + IntToStr(showPdf.DisplayOrder));
                WriteLn('    Color: ' + showPdf.Color);
                WriteLn('    Value: ' + showPdf.Value);
            end
            else if dataItem is TDate then
            begin
                date := dataItem as TDate;
                WriteLn('    Type: Date');
                WriteLn('    Id: ' + IntToStr(date.Id));
                WriteLn('    Label: ' + date._Label);
                WriteLn('    Description: ' + date.Description.InderValue);
                WriteLn('    DisplayOrder: ' + IntToStr(date.DisplayOrder));
                WriteLn('    MinValue: ' + DateToStr(date.MinValue));
                WriteLn('    MaxValue: ' + DateToStr(date.MaxValue));
                WriteLn('    Mandatory: ' + BoolToStr(date.Mandatory));
                WriteLn('    ReadOnly: ' + BoolToStr(date.ReadOnly));
                WriteLn('    Color: ' + date.Color);
                WriteLn('    Value: ' + date.DefaultValue);
            end
            else if dataItem is TSignature then
            begin
                signature := dataItem as TSignature;
                WriteLn('    Type: Signature');
                WriteLn('    Id: ' + IntToStr(signature.Id));
                WriteLn('    Label: ' + signature._Label);
                WriteLn('    Description: ' + signature.Description.InderValue);
                WriteLn('    DisplayOrder: ' + IntToStr(signature.DisplayOrder));
                WriteLn('    Mandatory: ' + BoolToStr(signature.Mandatory));
                WriteLn('    Color: ' + signature.Color);
            end
            else if dataItem is TCheckBox then
            begin
                checkBox := dataItem as TCheckBox;
                WriteLn('    Type: CheckBox');
                WriteLn('    Id: ' + IntToStr(checkBox.Id));
                WriteLn('    Label: ' + checkBox._Label);
                WriteLn('    Description: ' + checkBox.Description.InderValue);
                WriteLn('    DisplayOrder: ' + IntToStr(checkBox.DisplayOrder));
                WriteLn('    Mandatory: ' + BoolToStr(checkBox.Mandatory));
                WriteLn('    Selected: ' + BoolToStr(checkBox.Selected));
            end
            else if dataItem is TSaveButton then
            begin
                saveButton := dataItem as TSaveButton;
                WriteLn('    Type: SaveButton');
                WriteLn('    Id: ' + IntToStr(saveButton.Id));
                WriteLn('    Label: ' + saveButton._Label);
                WriteLn('    Description: ' + saveButton.Description.InderValue);
                WriteLn('    DisplayOrder: ' + IntToStr(saveButton.DisplayOrder));
                WriteLn('    Value: ' + saveButton.Value);
            end
            else if dataItem is TTimer then
            begin
                timer := dataItem as TTimer;
                WriteLn('    Type: Timer');
                WriteLn('    Id: ' + IntToStr(timer.Id));
                WriteLn('    Label: ' + timer._Label);
                WriteLn('    Description: ' + timer.Description.InderValue);
                WriteLn('    DisplayOrder: ' + IntToStr(timer.DisplayOrder));
                WriteLn('    StopOnSave: ' + BoolToStr(timer.StopOnSave));
                WriteLn('    Mandatory: ' + BoolToStr(timer.Mandatory));
            end
            else if dataItem is TNone then
            begin
                none := dataItem as TNone;
                WriteLn('    Type: None');
                WriteLn('    Id: ' + IntToStr(none.Id));
                WriteLn('    Label: ' + none._Label);
                WriteLn('    Description: ' + none.Description.InderValue);
                WriteLn('    DisplayOrder: ' + IntToStr(none.DisplayOrder));
            end
            else if dataItem is TMultiSelect then
            begin
                multiSelect := dataItem as TMultiSelect;
                WriteLn('    Type: MultiSelect');
                WriteLn('    Id: ' + IntToStr(multiSelect.Id));
                WriteLn('    Label: ' + multiSelect._Label);
                WriteLn('    Description: ' + multiSelect.Description.InderValue);
                WriteLn('    DisplayOrder: ' + IntToStr(multiSelect.DisplayOrder));
                WriteLn('    Mandatory: ' + BoolToStr(multiSelect.Mandatory));
                WriteLn('    KeyValuePairList:');
                PrintKeyValuePairList(multiSelect.KeyValuePairList, '      ');
            end
            else if dataItem is TSingleSelect then
            begin
                singleSelect := dataItem as TSingleSelect;
                WriteLn('    Type: SingleSelect');
                WriteLn('    Id: ' + IntToStr(singleSelect.Id));
                WriteLn('    Label: ' + singleSelect._Label);
                WriteLn('    Description: ' + singleSelect.Description.InderValue);
                WriteLn('    DisplayOrder: ' + IntToStr(singleSelect.DisplayOrder));
                WriteLn('    Mandatory: ' + BoolToStr(singleSelect.Mandatory));
                WriteLn('    KeyValuePairList:');
                PrintKeyValuePairList(singleSelect.KeyValuePairList, '      ');
            end
            else if dataItem is TNumber then
            begin
                number := dataItem as TNumber;
                WriteLn('    Type: Number');
                WriteLn('    Id: ' + IntToStr(number.Id));
                WriteLn('    Label: ' + number._Label);
                WriteLn('    Description: ' + number.Description.InderValue);
                WriteLn('    DisplayOrder: ' + IntToStr(number.DisplayOrder));
                WriteLn('    MinValue: ' + number.MinValue);
                WriteLn('    MaxValue: ' + number.MaxValue);
                WriteLn('    Mandatory: ' + BoolToStr(number.Mandatory));
                WriteLn('    DecimalCount: ' + IntToStr(number.DecimalCount));
                WriteLn('    UnitName: ' + number.UnitName);
            end
            else if dataItem is TText then
            begin
                text := dataItem as TText;
                WriteLn('    Type: Text');
                WriteLn('    Id: ' + IntToStr(text.Id));
                WriteLn('    Label: ' + text._Label);
                WriteLn('    Description: ' + text.Description.InderValue);
                WriteLn('    GeolocationEnabled: ' + BoolToStr(text.GeolocationEnabled));
                WriteLn('    Value: ' + text.Value);
                WriteLn('    ReadOnly: ' + BoolToStr(text.ReadOnly));
                WriteLn('    Mandatory: ' + BoolToStr(text.Mandatory));
             end
            else if dataItem is TComment then
            begin
                comment := dataItem as TComment;
                WriteLn('    Type: Comment');
                WriteLn('    Id: ' + IntToStr(comment.Id));
                WriteLn('    Label: ' + comment._Label);
                WriteLn('    Description: ' + comment.Description.InderValue);
                WriteLn('    SplitScreen: ' + BoolToStr(comment.SplitScreen));
                WriteLn('    Value: ' + comment.Value);
                WriteLn('    ReadOnly: ' + BoolToStr(comment.ReadOnly));
                WriteLn('    Mandatory: ' + BoolToStr(comment.Mandatory));
             end
         end;
       end;

   end;
   WriteLn('');
end;

procedure TSamples.PrintKeyValuePairList(list:  TObjectList<TKeyValuePair>; offset: string);
var
  pair: TKeyValuePair;
begin
    for pair in list do
    begin
      WriteLn(offset + 'Key: ' + pair.Key);
      WriteLn(offset + 'Value: ' + pair.Value);
      WriteLn(offset + 'DisplayOrder: ' + pair.DisplayOrder);
      WriteLn(offset + 'Selected: ' + BoolToStr(pair.Selected));
    end;
end;


{$endregion}

end.
