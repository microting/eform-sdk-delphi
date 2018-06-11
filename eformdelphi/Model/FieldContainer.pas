unit FieldContainer;

interface

uses  DataItemGroup, Generics.Collections, Classes, DataItem, SysUtils;

type
 {$region 'TFieldContainer declaration'}
 TFieldContainer = class(TDataItem)
 public
    Value: string;
    FieldType: string;
    DataItemList: TObjectList<TDataItem>;

    constructor Create; overload;
    constructor Create(id: integer; _label: string; description: TCDataValue; color: string; displayOrder: integer;
      value: string; dataItemList: TObjectList<TDataItem>); overload;

    class function FromDataItemGroup(dataItemGroup: TDataItemGroup): TFieldContainer;

 end;
 {$endregion}
implementation

{$region 'TFieldContainer implementation'}
constructor TFieldContainer.Create;
begin
end;

constructor TFieldContainer.Create(id: integer; _label: string; description: TCDataValue; color: string; displayOrder: integer;
      value: string; dataItemList: TObjectList<TDataItem>);
begin
   self.Id := id;
   self._Label := _label;
   self.Description := description;
   self.Color := color;
   self.DisplayOrder := displayOrder;

   self.Value := value;
   self.DataItemList := dataItemList;
   self.FieldType := 'FieldContainer';
end;

class function TFieldContainer.FromDataItemGroup(dataItemGroup: TDataItemGroup): TFieldContainer;
var
  description: TCDataValue;
  fg: TFieldContainer;
begin
  description := TCDataValue.Create();
  description.InderValue := dataItemGroup.Description;
  fg := TFieldContainer.Create(StrToInt(dataItemGroup.Id), dataItemGroup._Label, description,dataItemGroup.Color,
    dataItemGroup.DisplayOrder, '',  dataItemGroup.DataItemList);
  Result := fg;
end;
{$endregion}

end.
