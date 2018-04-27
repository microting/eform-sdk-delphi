unit DataItemGroup;

interface

uses  Generics.Collections, DataItem;
type
  {$region 'TDataItemGroup declaration'}
  TDataItemGroup = class
  private
     constructor Create; overload;
  public
    Id: string;
    _Label: string;
    Description: string;
    Color: string;
    DisplayOrder: integer;
    Value: string;

    DataItemList: TObjectList<TDataItem>;
  end;
  {$endregion}

  {$region 'TDateItemGroup declaration'}
  TFieldGroup = class(TDataItemGroup)
  private
     constructor Create; overload;
  public
     constructor Create(id: string; _label: string; description: string; color: string; displayOrder: integer;
        value: string; dataItemList: TObjectList<TDataItem>); overload;
  end;
  {$endregion}

implementation

{$region 'TDataItemGroup implementation'}
constructor TDataItemGroup.Create;
begin
end;
{$endregion}

{$region 'TFieldGroup implementation'}
constructor TFieldGroup.Create;
begin
end;

constructor TFieldGroup.Create(id: string; _label: string; description: string; color: string; displayOrder: integer;
        value: string; dataItemList: TObjectList<TDataItem>);
begin
   self.Id := id;
   self._Label := _label;
   self.Description := description;
   self.Color := color;
   self.DisplayOrder := displayOrder;
   self.Value := value;
   self.DataItemList := dataItemList;
end;

{$endregion}

end.
