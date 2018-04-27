unit Classes;

interface

uses Xml.XMLDoc;

type

 {$region TCDataValue declaration}
  TCDataValue = class
  private
     function GetDataWrapper: TXMLNodeCollection;
     procedure SetDataWrapper(Value: TXMLNodeCollection);

  public
      InderValue: string;

      property DataWrapper: TXMLNodeCollection read GetDataWrapper write SetDataWrapper;
  end;
  {$endregion}

  {$region TKeyValuePair declaration}
  TKeyValuePair = class
  private
    constructor Create; overload;
  public
     Key: string;
     Value: string;
     Selected: boolean;
     DisplayOrder: string;

     constructor Create(key: string; value: string; selected: boolean; displayOrder: string); overload;
  end;
  {$endregion}


implementation

{$region TCDataValue implementation}
function TCDataValue.GetDataWrapper: TXMLNodeCollection;
begin
    Result := nil;
end;

procedure TCDataValue.SetDataWrapper(Value: TXMLNodeCollection);
begin
    if Value = nil then
    begin
      InderValue := '';
      exit;
    end;

end;
{$endregion}


{$region TKeyValuePair implementaton}
constructor TKeyValuePair.Create;
begin
end;

constructor TKeyValuePair.Create(key: string; value: string; selected: boolean; displayOrder: string);
begin
  inherited Create;
  self.Key := key;
  self.Value := value;
  self.Selected := selected;
  self.DisplayOrder := displayOrder;
end;

{$endregion}

end.
