unit Entities;

interface

uses Generics.Collections;

type

  {$region 'TEntityItem declaration'}
  TEntityItem = class
  public
      Id: integer;
      Name: string;
      Description: string;
      EntityItemUId: string;
      WorkflowState: string;
      MicrotingUId: string;

      constructor Create; overload;
      constructor Create(name: string; description: string; entityItemUId: string); overload;
      constructor Create(name: string; description: string; entityItemUId: string; workflowState: string); overload;
      constructor Create(id: integer; name: string; description: string; entityItemUId: string;
        microtingUId: string); overload;
     constructor Create(id: integer; name: string; description: string; entityItemUId: string;
        workflowState: string; microtingUId: string); overload;

  end;
  {$endregion}

  {$region 'TEntityGroup declaration'}
  TEntityGroup = class
  public
      Id: integer;
      Name: string;
      _Type: string;
      EntityGroupMUId: string;
      EntityGroupItemLst : TObjectList<TEntityItem>;
      WorkflowState: string;
      CreatedAt: TDateTime;
      UpdatedAt: TDateTime;

      constructor Create;overload;
      constructor Create(id: integer; name: string; _type: string; entityGroupMUId: string;
         entityGroupItem: TObjectList<TEntityItem>);overload;
      constructor Create(id: integer; name: string; _type: string; entityGroupMUId: string;
         entityGroupItem: TObjectList<TEntityItem>; workflowState: string; createdAt: TDateTime;
         updatedAt: TDateTime);overload;
  end;
  {$endregion}

  {$region 'TEntityGroupList declaration'}
  TEntityGroupList = class
  public
      NumOfElements: integer;
      PageNum: integer;
      EntityGroups: TObjectList<TEntityGroup>;

      constructor Create;overload;
      constructor Create(numOfElements: integer; pageNum: integer; entityGroupList: TObjectList<TEntityGroup>);overload;
  end;
  {$endregion}

implementation

{$region 'TEntityItem implementation'}
constructor TEntityItem.Create;
begin
    self.Name := '';
    self.Description := '';
    self.EntityItemUId := '';
    self.MicrotingUId := '';
end;

constructor TEntityItem.Create(name: string; description: string; entityItemUId: string);
begin
    self.Name := name;
    self.Description := description;
    self.EntityItemUId := entityItemUId;
end;

constructor TEntityItem.Create(name: string; description: string; entityItemUId: string; workflowState: string);
begin
    self.Name := name;
    self.Description := description;
    self.EntityItemUId := entityItemUId;
    self.WorkflowState := workflowState;
end;

constructor TEntityItem.Create(id: integer; name: string; description: string; entityItemUId: string;
  microtingUId: string);
begin
    self.Id := id;
    self.Name := name;
    self.Description := description;
    self.EntityItemUId := entityItemUId;
    self.MicrotingUId := microtingUId;
end;

constructor TEntityItem.Create(id: integer; name: string; description: string; entityItemUId: string;
  workflowState: string; microtingUId: string);
begin
    self.Id := id;
    self.Name := name;
    self.Description := description;
    self.EntityItemUId := entityItemUId;
    self.WorkflowState := workflowState;
    self.MicrotingUId := microtingUId;
end;
{$endregion}

{$region 'TEntityGroup implementation'}
constructor TEntityGroup.Create;
begin

end;

constructor TEntityGroup.Create(id: integer; name: string; _type: string; entityGroupMUId: string;
   entityGroupItem: TObjectList<TEntityItem>);
begin
    self.Id := id;
    self.Name := name;
    self._Type := _type;
    self.EntityGroupMUId := entityGroupMUId;
    self.EntityGroupItemLst := entityGroupItemLst;
end;

constructor TEntityGroup.Create(id: integer; name: string; _type: string; entityGroupMUId: string;
   entityGroupItem: TObjectList<TEntityItem>; workflowState: string; createdAt: TDateTime;
   updatedAt: TDateTime);
begin
    self.Id := id;
    self.Name := name;
    self._Type := _type;
    self.EntityGroupMUId := entityGroupMUId;
    self.EntityGroupItemLst := entityGroupItemLst;
    self.WorkflowState := workflowState;
    self.CreatedAt := createdAt;
    self.UpdatedAt := updatedAt;
end;
{$endregion}

{$region 'TEntityGroupList implementation'}
constructor TEntityGroupList.Create;
begin

end;

constructor TEntityGroupList.Create(numOfElements: integer; pageNum: integer; entityGroupList: TObjectList<TEntityGroup>);
begin
   self.NumOfElements := numOfElements;
   self.PageNum := pageNum;
   self.EntityGroups := entityGroupList;
end;
{$endregion}
end.
