unit Classes;

interface

uses Xml.XMLDoc;

type
  TCDataValue = class
  private
     function GetDataWrapper: TXMLNodeCollection;
     procedure SetDataWrapper(Value: TXMLNodeCollection);

  public
      InderValue: string;

      property DataWrapper: TXMLNodeCollection read GetDataWrapper write SetDataWrapper;
  end;

implementation

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

end.
