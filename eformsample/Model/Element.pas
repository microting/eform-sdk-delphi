unit Element;

interface

uses Classes, Generics.Collections, DataItem, DataItemGroup;

type
  {$region 'TElement declaration'}
  TElement = class
  private
      constructor Create;
  public
      Id: integer;
      _Label: string;
      DisplayOrder: integer;
      Description: TCDataValue;
      ReviewEnabled: boolean;
      ApprovalEnabled: boolean;
      DoneButtonEnabled: boolean;
      ExtraFieldsEnabled: boolean;
      PinkBarText: string;
  end;
  {$endregion}

   {$region 'TGroupElement declaration'}
   TGroupElement = class(TElement)
   private
      constructor Create; overload;
   public
      ElementList: TObjectList<TElement>;

      constructor Create(id: integer; _label: string; displayOrder: integer; description: string;
          approvedEnabled: boolean; reviewEnabled: boolean; doneButtonEnabled: boolean;
          extraDataElementsEnabled: boolean; pinkBarText: string; elementList: TObjectList<TElement>); overload;
   end;
   {$endregion}

   {$region 'TDataElement declaration'}
   TDataElement = class(TElement)
   private
      constructor Create; overload;
   public
      DataItemList: TObjectList<TDataItem>;
      DataItemGroupList: TObjectList<TDataItemGroup>;

      constructor Create(id: integer; _label: string; displayOrder: integer; description: string;
          approvalEnabled: boolean; reviewEnabled: boolean; doneButtonEnabled: boolean;
          extraDataElementsEnabled: boolean; pinkBarText: string; dataItemGroupList: TObjectList<TDataItemGroup>;
          dataItemList: TObjectList<TDataItem>); overload;

   end;
   {$endregion}

   {$region 'TCheckListValue declaration'}
   TCheckListValue = class(TElement)
   private
      constructor Create;overload;
   public
      DataItemList: TObjectList<TDataItem>;
      DataItemGroupList: TObjectList<TDataItemGroup>;
      Status: string;

      constructor Create(dataElement: TDataElement; status: string); overload;
   end;
   {$endregion}

implementation

{$region 'TElement implementation'}
constructor TElement.Create;
begin
end;
{$endregion}

{$region 'TGroupElement implementation'}
constructor TGroupElement.Create;
begin
  ElementList := TObjectList<TElement>.Create;
end;

constructor TGroupElement.Create(id: integer; _label: string; displayOrder: integer; description: string;
          approvedEnabled: boolean; reviewEnabled: boolean; doneButtonEnabled: boolean;
          extraDataElementsEnabled: boolean; pinkBarText: string; elementList: TObjectList<TElement>);
begin
  inherited Create;

  self.Id := id;
  self._Label := _label;
  self.DisplayOrder := displayOrder;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.ApprovalEnabled := approvedEnabled;
  self.ReviewEnabled := reviewEnabled;
  self.DoneButtonEnabled := self.doneButtonEnabled;
  self.ExtraFieldsEnabled := extraDataElementsEnabled;
  self.PinkBarText := pinkBarText;

  self.ElementList := elementList;
end;
{$endregion}

{$region 'TDataElement implementation'}
constructor TDataElement.Create;
begin
   DataItemList := TObjectList<TDataItem>.Create;
   DataItemGroupList := TObjectList<TDataItemGroup>.Create;
end;

constructor TDataElement.Create(id: integer; _label: string; displayOrder: integer; description: string;
        approvalEnabled: boolean; reviewEnabled: boolean; doneButtonEnabled: boolean;
        extraDataElementsEnabled: boolean; pinkBarText: string; dataItemGroupList: TObjectList<TDataItemGroup>;
        dataItemList: TObjectList<TDataItem>);
begin
  inherited Create;

  self.Id := id;
  self._Label := _label;
  self.DisplayOrder := displayOrder;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.ApprovalEnabled := approvalEnabled;
  self.ReviewEnabled := reviewEnabled;
  self.DoneButtonEnabled := self.doneButtonEnabled;
  self.ExtraFieldsEnabled := extraDataElementsEnabled;
  self.PinkBarText := pinkBarText;

  self.DataItemGroupList := dataItemGroupList;
  self.DataItemList := dataItemList;
end;
{$endregion}

{$region 'TCheckListValue implementation'}
constructor TCheckListValue.Create;
begin
   DataItemList := TObjectList<TDataItem>.Create;
   DataItemGroupList := TObjectList<TDataItemGroup>.Create;
end;

constructor TCheckListValue.Create(dataElement: TDataElement; status: string);
begin
  self.Id := dataElement.Id;
  self._Label := dataElement._Label;
  self.DisplayOrder := dataElement.DisplayOrder;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := dataElement.Description.InderValue;
  self.ApprovalEnabled := dataElement.ApprovalEnabled;
  self.ReviewEnabled := dataElement.ReviewEnabled;
  self.DoneButtonEnabled := dataElement.DoneButtonEnabled;
  self.ExtraFieldsEnabled := dataElement.ExtraFieldsEnabled;
  self.PinkBarText := dataElement.PinkBarText;

  self.DataItemGroupList := dataElement.DataItemGroupList;
  self.DataItemList := dataElement.DataItemList;
  self.Status := status;
end;
{$endregion}

end.
