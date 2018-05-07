unit Samples;

interface

uses
  Core, SysUtils, MainElement, IOUtils, Element, DataItem;

type
  {$region 'TSamples declaration'}
  TSamples = class
  private
      ServerConnectionString: string;
      Core: TCore;

      procedure Print(mainElement: TMainElement);
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
            filename := 'picture_test.xml';
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
  date: TDate;
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
            end;




         end;
       end;
         
   end;
     
     
   WriteLn('');
end;

{$endregion}

end.
