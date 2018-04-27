unit ElementList;

interface

uses Generics.Collections, DataItemReply;

type
  {$region 'TElementList declaration'}
  TElementList = class
  private
    constructor Create;
  public
     Id: string;
     Status: string;
     DataItemList: TObjectList<TDataItemReply>;
     ExtraDataItemList:  TObjectList<TDataItemReply>;
  end;
  {$endregion}

implementation

{$region 'TElementList implementation'}
constructor TElementList.Create;
begin
  DataItemList := TObjectList<TDataItemReply>.Create;
  ExtraDataItemList := TObjectList<TDataItemReply>.Create;
end;
{$endregion}

end.
