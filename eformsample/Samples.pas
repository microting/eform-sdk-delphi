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
    WriteLn('  ''1'', for sample 1');
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
   mainElement: TMainElement;
   xml: WideString;
begin
   Core.Start(ServerConnectionString);

   while true do
   begin
       WriteLn('  Type ''S'' for setting up the eForm templat from the picture_test.xml, and define the siteId');
       WriteLn('  Type ''C'' to create an eForm case based on the templat, and ''Q'' (to quit)');
       WriteLn('  As long as the Core left running, the system is able to process eForms');
       ReadLn(input);
       if UpperCase(input) = 'Q' then
          break
       else if UpperCase(input) = 'S' then
       begin
          xml := TFile.ReadAllText('picture_test.xml');
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
            WriteLn('   DataItem:');
         end;
       end;
         
   end;
     
     
   WriteLn('');
end;

{$endregion}

end.
