unit Check;

interface

uses Generics.Collections, ElementList;

type
  {$region 'TCheck declaration'}
  TCheck = class
  private
    _date: string;

    constructor Create; overload;

    function GetDate: string;
    procedure SetDate(value: string);
  public
    UnitId: string;
    Worker: string;
    WorkerId: string;
    ElementList: TObjectList<TElementList>;

    property Date: string read GetDate write SetDate;
  end;
  {$endregion}

implementation

{$region 'TCheck implementation'}
constructor TCheck.Create;
begin
end;

function TCheck.GetDate: string;
begin
  Result := Copy(_date, 1, 19);
end;

procedure TCheck.SetDate(value: string);
begin
  _date := value;
end;
{$endregion}


end.
