unit MainElement;

interface

uses  Generics.Collections, Element, SysUtils, DataItem;


type
  {$region 'TCoreElement declaration'}
  TCoreElement = class
  private
     function GetStartDateString: string;
     procedure SetStartDateString(Value: string);
     function GetEndDateString: string;
     procedure SetEndDateString(Value: string);

     function DataItemGetAllFromList(elementList: TObjectList<TElement>; dataItemLst: TObjectList<TDataItem>)
         :TObjectList<TDataItem>;
  public
     Id: integer;
     _Label: string;
     DisplayOrder: integer;
     CheckListFolderName: string;
     Repeated: integer;
     MicrotingUId: string;
     CaseType: string;
     Language: string;
     MultiApproval: boolean;
     FastNavigation: boolean;
     DownloadEntities: boolean;
     ManualSync: boolean;
     StartDate: TDateTime;
     EndDate: TDateTIme;
     ElementList: TObjectList<TElement>;

     property StartDateStr: string read GetStartDateString write SetStartDateString;
     property EndDateStr: string read GetEndDateString write SetEndDateString;


     constructor Create;overload;
     constructor Create(id: integer; _label: string; displayOrder: integer; checkListFolderName: string;
         repeated: integer; startDate: TDateTime;  endDate: TDateTime; language: string; multiApproval: boolean;
         fastNavigation: boolean; downloadEntities: boolean; manualSync: boolean;
         caseType: string; elementList: TObjectList<TElement>);overload;

     function DataItemGetAll: TObjectList<TDataItem>;
  end;
  {$endregion}

  {$region 'TMainElement declaration'}
  TMainElement = class(TCoreElement)
  private
     _pushMessageTitle: string;

     function GetPushMessageTitle: string;
     procedure SetPushMessageTitle(value: string);
  public
      PushMessageBody: string;
      property PushMessageTitle: string read GetPushMessageTitle write SetPushMessageTitle;

      constructor Create(coreElement: TCoreElement);overload;
      constructor Create(id: integer; _label: string; displayOrder: integer; checkListFolderName: string;
          repeated: integer; startDate: TDateTime; endDate: TDateTime; language:string; multiApproval: boolean;
          fastNavigation: boolean; downloadEntities: boolean; manualSync: boolean; caseType: string;
          pushMessageTitle: string; pushMessageBody: string; elementList: TObjectList<TElement>);overload;

      function XmlToClass(xmlStr: string): TMainElement;
      function ClassToXml: string;
  end;
  {$endregion}

 {$region 'TReplyElement declaration'}
  TReplyElement = class(TCoreElement)
  public
      Custom: string;
      DoneAt: TDateTime;
      DoneById: integer;
      UnitId: integer;

      constructor Create; overload;
      constructor Create(coreElement: TCoreElement);overload;
  end;
  {$endregion}

implementation


{$region 'TCoreElement implementation'}
constructor TCoreElement.Create;
begin
    ElementList := TObjectList<TElement>.Create;
end;

constructor TCoreElement.Create(id: integer; _label: string; displayOrder: integer; checkListFolderName: string;
       repeated: integer; startDate: TDateTime;  endDate: TDateTime; language: string; multiApproval: boolean;
       fastNavigation: boolean; downloadEntities: boolean; manualSync: boolean;
       caseType: string; elementList: TObjectList<TElement>);
begin
    self.Id := id;
    self._Label := _label;
    self.DisplayOrder := displayOrder;
    self.CheckListFolderName := checkListFolderName;
    self.Repeated := repeated;
    self.StartDate := startDate;
    self.EndDate := endDate;
    self.Language := language;
    self.MultiApproval := multiApproval;
    self.FastNavigation := fastNavigation;
    self.DownloadEntities := downloadEntities;
    self.ManualSync := manualSync;
    self.CaseType := caseType;
    self.ElementList := elementList;
end;

function TCoreElement.GetStartDateString: string;
begin
  Result := FormatDateTime('yyyy-MM-dd HH:mm:ss', StartDate);
end;

procedure TCoreElement.SetStartDateString(Value: string);
var
  fs : TFormatSettings;
Begin
  fs := TFormatSettings.Create;
  fs.DateSeparator := '-';
  fs.ShortDateFormat := 'yyyy-MM-dd HH:mm:ss';

  StartDate := StrToDate(Value, fs);
end;

function TCoreElement.GetEndDateString: string;
begin
  Result := FormatDateTime('yyyy-MM-dd HH:mm:ss', EndDate);
end;

procedure TCoreElement.SetEndDateString(Value: string);
var
  fs : TFormatSettings;
Begin
  fs := TFormatSettings.Create;
  fs.DateSeparator := '-';
  fs.ShortDateFormat := 'yyyy-MM-dd HH:mm:ss';

  EndDate := StrToDate(Value, fs);
end;

function TCoreElement.DataItemGetAll: TObjectList<TDataItem>;
begin
  try
     Result := DataItemGetAllFromList(ElementList, TObjectList<TDataItem>.Create);
  except
    on e: Exception do
    begin
       raise Exception.Create('DataItemGetAll failed, to get all DataItem ' + e.Message);
    end;
  end;
end;

function TCoreElement.DataItemGetAllFromList(elementList: TObjectList<TElement>; dataItemLst: TObjectList<TDataItem>)
  : TObjectList<TDataItem>;
var
  element: TElement;
  dataE: TDataElement;
  groupE: TGroupElement;
  item: TDataItem;
begin
  for element in elementList do
  begin
     if element is TDataElement then
     begin
        dataE := element as TDataElement;
        for item in dataE.DataItemList do
        begin
           dataItemLst.Add(item);
        end;
     end;

     if element is TGroupElement then
     begin
        groupE := element as TGroupElement;
        DataItemGetAllFromList(groupE.ElementList, dataItemLst);
     end;
  end;
  Result := dataItemLst;
end;
{$endregion }

{$region 'TMainElement implementation'}
constructor TMainElement.Create(coreElement: TCoreElement);
begin
    self.Id := coreElement.Id;
    self._Label := coreElement._Label;
    self.DisplayOrder := coreElement.DisplayOrder;
    self.CheckListFolderName := coreElement.CheckListFolderName;
    self.Repeated := coreElement.Repeated;
    self.StartDate := coreElement.StartDate;
    self.EndDate := coreElement.EndDate;
    self.Language := coreElement.Language;
    self.MultiApproval := coreElement.MultiApproval;
    self.FastNavigation := coreElement.FastNavigation;
    self.DownloadEntities := coreElement.DownloadEntities;
    self.ManualSync := coreElement.ManualSync;
    self.CaseType := coreElement.CaseType;
    self.ElementList := coreElement.ElementList;
    self.MicrotingUId := coreElement.MicrotingUId;

    self.PushMessageTitle := '';
    self.PushMessageBody := '';
end;

constructor TMainElement.Create(id: integer; _label: string; displayOrder: integer; checkListFolderName: string;
          repeated: integer; startDate: TDateTime; endDate: TDateTime; language:string; multiApproval: boolean;
          fastNavigation: boolean; downloadEntities: boolean; manualSync: boolean; caseType: string;
          pushMessageTitle: string; pushMessageBody: string; elementList: TObjectList<TElement>);
begin
    self.Id := id;
    self._Label := _label;
    self.DisplayOrder := displayOrder;
    self.CheckListFolderName := checkListFolderName;
    self.Repeated := repeated;
    self.StartDate := startDate;
    self.EndDate := endDate;
    self.Language := language;
    self.MultiApproval := multiApproval;
    self.FastNavigation := fastNavigation;
    self.DownloadEntities := downloadEntities;
    self.ManualSync := manualSync;
    self.CaseType := caseType;
    self.ElementList := elementList;
    self.PushMessageTitle := pushMessageTitle;
    self.PushMessageBody := pushMessageBody;
end;


function TMainElement.GetPushMessageTitle: string;
begin
  Result := _pushMessageTitle;
  if Length(_pushMessageTitle) > 255 then
  begin
    Result := Copy(_pushMessageTitle, 1, 255);
  end;
end;

procedure TMainElement.SetPushMessageTitle(value: string);
begin
  _pushMessageTitle := value;
end;

function TMainElement.XmlToClass(xmlStr: string): TMainElement;
begin
  // TODO
  Result := nil;
end;

function TMainElement.ClassToXml: string;
begin
  // TODO
  Result := '';
end;
{$endregion }


{$region 'TReplyElement implementation'}
constructor TReplyElement.Create;
begin
  ElementList := TObjectList<TElement>.Create;
end;

constructor TReplyElement.Create(coreElement: TCoreElement);
begin
    self.Id := coreElement.Id;
    self._Label := coreElement._Label;
    self.DisplayOrder := coreElement.DisplayOrder;
    self.CheckListFolderName := coreElement.CheckListFolderName;
    self.Repeated := coreElement.Repeated;
    self.StartDate := coreElement.StartDate;
    self.EndDate := coreElement.EndDate;
    self.Language := coreElement.Language;
    self.MultiApproval := coreElement.MultiApproval;
    self.FastNavigation := coreElement.FastNavigation;
    self.DownloadEntities := coreElement.DownloadEntities;
    self.ManualSync := coreElement.ManualSync;
    self.CaseType := coreElement.CaseType;
    self.ElementList := coreElement.ElementList;
    self.MicrotingUId := coreElement.MicrotingUId;
end;
{$endregion }
end.
