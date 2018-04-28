unit DataItemReply;

interface

uses GeolocationData, Classes;

type

  {$region 'TDataItemReply declaration'}
  TDataItemReply = class
  private
      constructor Create;
  public
      Id: string;
      Geolocation: TGeolocationData;
      Value: TCDataValue;
      Extension: string;
      URL: string;

  end;
  {$endregion}


implementation

{$region 'TDataItemReply declaration'}
constructor TDataItemReply.Create;
begin
end;
{$endregion}
end.
