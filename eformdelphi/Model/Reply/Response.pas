unit Response;

interface

uses Generics.Collections, Check, ElementList;

type
  TResponseTypes = (Error, Received, Parsing, Success, Invalid);

  {$region 'TResponse declaration'}
  TResponse = class
  private
      function XmlToClassCheck(xmlStr: string): TElementList;
      function ClassToXmlCheck(elementList: TElementList): string;
      function PureXml(xmlStr: string): string;
  public
      _Type: TResponseTypes;
      Value: string;
      UnitFetchedAt: string;
      UnitId: string;
      Checks: TObjectList<TCheck>;

      constructor Create;overload;
      constructor Create(_type: TResponseTypes; value: string);overload;

      function XmlToClassUsingXmlDocument(xmlStr: string): TResponse;
      function XmlToClass(xmlStr: string): TResponse;
      function ClassToXml: string;
  end;
  {$endregion}

implementation


{$region 'TResponse implementation'}
 constructor TResponse.Create;
 begin
    Checks := TObjectList<TCheck>.Create;
 end;

 constructor TResponse.Create(_type: TResponseTypes; value: string);
 begin
   self._Type := _type;
   self.Value := value;
   self.Checks := TObjectList<TCheck>.Create;
 end;

 function TResponse.XmlToClassUsingXmlDocument(xmlStr: string): TResponse;
 begin
    // TODO
    Result := nil;
 end;

 function TResponse.XmlToClass(xmlStr: string): TResponse;
 begin
   // TODO
    Result := nil;
 end;


 function TResponse.ClassToXml: string;
 begin
   // TODO
   Result := '';
 end;

 function TResponse.XmlToClassCheck(xmlStr: string): TElementList;
 begin
   // TODO
   Result := nil;
 end;

 function TResponse.ClassToXmlCheck(elementList: TElementList): string;
 begin
   // TODO
   Result := '';
 end;

 function TResponse.PureXml(xmlStr: string): string;
 begin
   // TODO
   Result := '';
 end;


 {$endregion}

end.
