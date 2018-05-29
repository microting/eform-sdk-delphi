unit DataItem;

interface

uses Classes, Generics.Collections, SysUtils;

{$HINTS OFF}
type
 {$region 'TDataItem declaration'}
  TDataItem = class
  private
    constructor Create;
  public
    Id: Integer;
    Mandatory: boolean;
    ReadOnly: boolean;
    _Label: string;
    Description: TCDataValue;
    Color: string;
    DisplayOrder: integer;
    Dummy: boolean;
  end;
  {$endregion}

 {$region 'children declaration'}
 {$region 'TAudio declaration'}
 TAudio = class(TDataItem)
 public
   Multi: integer;

   constructor Create; overload;
   constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
    displayOrder: integer; dummy: boolean; multi: integer); overload;
 end;
 {$endregion}

 {$region 'TCheckBox declaration'}
 TCheckBox = class(TDataItem)
 public
   DefaultValue: boolean;
   Selected: boolean;

   constructor Create; overload;
   constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
    displayOrder: integer; dummy: boolean; defaultValue: boolean; selected: boolean); overload;
 end;
 {$endregion}

 {$region 'TComment declaration'}
 TComment = class(TDataItem)
 public
    Value: string;
    MaxLength: integer;
    SplitScreen: boolean;

    constructor Create; overload;
    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
     displayOrder: integer; dummy: boolean; value: string; maxLength: integer; splitScreen: boolean); overload;
 end;
 {$endregion}

 {$region 'TDate declaration'}
 TDate = class(TDataItem)
 private
    function GetMaxValueString: string;
    procedure SetMaxValueString(Value: string);
    function GetMinValueString: string;
    procedure SetMinValueString(Value: string);
 public
    DefaultValue: string;
    MaxValue: TDateTime;
    MinValue: TDateTime;

    constructor Create; overload;
    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
     displayOrder: integer; dummy: boolean; defaultValue: string; maxValue: TDateTime; minValue: TDateTime); overload;

    property MaxValueString: string read GetMaxValueString write SetMaxValueString;
    property MinValueString: string read GetMinValueString write SetMinValueString;
 end;
 {$endregion}

 {$region 'TNone declaration'}
 TNone = class(TDataItem)
 public
   constructor Create; overload;
   constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
    displayOrder: integer; dummy: boolean); overload;
 end;
 {$endregion}

 {$region 'TNumber declaration'}
 TNumber = class(TDataItem)
 public
    MinValue: string;
    MaxValue: string;
    DefaultValue: integer;
    DecimalCount: integer;
    UnitName: string;

    constructor Create; overload;
    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
      displayOrder: integer; dummy: boolean; minValue: string; maxValue: string; defaultValue:
      integer; decimalCount: integer; unitName: string); overload;
  end;
 {$endregion}

 {$region 'TMultiSelect declaration'}
 TMultiSelect = class(TDataItem)
 public
    KeyValuePairList: TObjectList<TKeyValuePair>;

    constructor Create; overload;
    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
     displayOrder: integer; dummy: boolean; keyValuePairList: TObjectList<TKeyValuePair>); overload;
 end;
 {$endregion}

 {$region 'TPicture declaration'}
 TPicture = class(TDataItem)
 public
   Multi: integer;
   GeolocationEnabled: boolean;

   constructor Create; overload;
   constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
    displayOrder: integer; dummy: boolean; multi: integer; geolocationEnabled: boolean); overload;
 end;
 {$endregion}

 {$region 'TShowPdf declaration'}
 TShowPdf = class(TDataItem)
 public
    Value: string;

    constructor Create; overload;
    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
     displayOrder: integer; dummy: boolean; value: string); overload;
 end;
 {$endregion}

 {$region 'TSaveButton declaration'}
 TSaveButton = class(TDataItem)
 public
    Value: string;

    constructor Create; overload;
    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
     displayOrder: integer; dummy: boolean; value: string); overload;
 end;
 {$endregion}

 {$region 'TSignature declaration'}
 TSignature = class(TDataItem)
 public
   constructor Create; overload;
   constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
    displayOrder: integer; dummy: boolean); overload;
 end;
 {$endregion}

 {$region 'TSingleSelect declaration'}
 TSingleSelect = class(TDataItem)
 private
    constructor Create; overload;
 public
    KeyValuePairList: TObjectList<TKeyValuePair>;

    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
     displayOrder: integer; dummy: boolean; keyValuePairList: TObjectList<TKeyValuePair>); overload;
 end;
 {$endregion}

 {$region 'TText declaration'}
 TText = class(TDataItem)
 public
    Value: string;
    MaxLength: integer;
    GeolocationEnabled: boolean;
    GeolocationForced: boolean;
    GeolocationHidden: boolean;
    BarcodeEnabled: boolean;
    BarcodeType: string;

    constructor Create; overload;
    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
      displayOrder: integer; dummy: boolean; value: string; maxLength: integer; geolocationEnabled:
      boolean; geolocationForced: boolean; geolocationHidden: boolean; barcodeEnabled: boolean; barcodeType: string); overload;
 end;
 {$endregion}

 {$region 'TTimer declaration'}
 TTimer = class(TDataItem)
 public
    StopOnSave: boolean;

    constructor Create; overload;
    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
     displayOrder: integer; dummy: boolean; stopOnSave: boolean); overload;
 end;
 {$endregion}

 {$region 'TEntitySearch declaration'}
 TEntitySearch = class(TDataItem)
 private
    constructor Create; overload;
 public
    DefaultValue: integer;
    EntityTypeId: integer;
    IsNum: boolean;
    QueryType: string;
    MinSearchLenght: integer;
    BarcodeEnabled: boolean;
    BarcodeType: string;

    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
      displayOrder: integer; dummy: boolean; defaultValue: integer; entityTypeId: integer; isNum:
      boolean; queryType: string; minSearchLenght: integer; barcodeEnabled: boolean; barcodeType: string); overload;
  end;
 {$endregion}

 {$region 'TEntitySelect declaration'}
 TEntitySelect = class(TDataItem)
 private
    constructor Create; overload;
 public
    DefaultValue: integer;
    Source: integer;

    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
      displayOrder: integer; dummy: boolean; defaultValue: integer; source: integer); overload;
  end;

 {$endregion}
 {$endregion}


 {$region 'TUploadedData'}
 TUploadedData = class
  public
    Checksum: string;
    Extension: string;
    CurrentFile: string;
    UploaderId: Integer;
    UploaderType: string;
    FileLocation: string;
    Filename: string;
 end;
 {$endregion}

 {$region 'TFieldValue'}
 TFieldValue = class
 public
    FieldId: integer;
    FieldType: string;
    DateOfDoing: TDateTime;
    Value: string;
    MicrotingUuid: string;
    ValueReadable: string;
    Latitude: string;
    Longitude: string;
    Altitude: string;
    Heading: string;
    Accuracy: string;
    Date: TDateTime;
    UploadedData: string;
    UploadedDataObj: TUploadedData;
    KeyValuePairList: TObjectList<TKeyValuePair>;
 end;
 {$endregion}


{$region 'TField'}
TField = class(TDataItem)
public
  FieldType: string;
  FieldValue: string;
  EntityGroudId: integer;
  KeyValuePairList: TObjectList<TKeyValuePair>;
  FieldValues: TObjectList<TFieldValue>;
end;
{$endregion}
TDataItemColors = (e2f4fb_Blue, f5eafa_Purple, f0f8db_Green,  fff6df_Yellow, ffe4e4_Red, None_Default);

implementation

constructor TDataItem.Create;
begin
end;

{$region 'children implementation'}
{$region 'TAudio implementation'}
constructor TAudio.Create;
begin
end;

constructor TAudio.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean; multi: integer);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.Multi := multi;
end;
{$endregion}

{$region 'TCheckBox implementation'}
constructor TCheckBox.Create;
begin
end;

constructor TCheckBox.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean; defaultValue: boolean; selected: boolean);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.DefaultValue := defaultValue;
  self.Selected := selected;
end;
{$endregion}

{$region 'TComment implementation'}
constructor TComment.Create;
begin
end;

constructor TComment.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean; value: string; maxLength: integer; splitScreen: boolean);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.Value := value;
  self.MaxLength := maxLength;
  self.SplitScreen := splitScreen;
end;
{$endregion}

{$region 'TDate implementation'}
constructor TDate.Create;
begin
end;

constructor TDate.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean; defaultValue: string; maxValue: TDateTime; minValue: TDateTime);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.DefaultValue := defaultValue;
  self.MaxValue := maxValue;
  self.MinValue := minValue;
end;

function TDate.GetMaxValueString: string;
begin
  Result := FormatDateTime('yyyy-MM-dd', MaxValue);
end;

procedure TDate.SetMaxValueString(Value: string);
var
  fs : TFormatSettings;
Begin
  fs := TFormatSettings.Create;
  fs.DateSeparator := '-';
  fs.ShortDateFormat := 'yyyy-MM-dd';

  MaxValue := StrToDate(Value, fs);
end;

function TDate.GetMinValueString: string;
begin
  Result := FormatDateTime('yyyy-MM-dd', MinValue);
end;

procedure TDate.SetMinValueString(Value: string);
var
  fs : TFormatSettings;
Begin
  fs := TFormatSettings.Create;
  fs.DateSeparator := '-';
  fs.ShortDateFormat := 'yyyy-MM-dd';

  MinValue := StrToDate(Value, fs);
end;

{$endregion}

{$region 'TNone implementation'}
constructor TNone.Create;
begin
end;

constructor TNone.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;
end;
{$endregion}

{$region 'TNumber implementation'}
constructor TNumber.Create;
begin
end;

constructor TNumber.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
      displayOrder: integer; dummy: boolean; minValue: string; maxValue: string; defaultValue:
      integer; decimalCount: integer; unitName: string);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.MaxValue := maxValue;
  self.MinValue := minValue;
  self.DefaultValue := defaultValue;
  self.DecimalCount := decimalCount;
  self.UnitName := unitName;
end;
{$endregion}

{$region 'TPicture implementation'}
constructor TPicture.Create;
begin
end;

constructor TPicture.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean; multi: integer; geolocationEnabled: boolean);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.Multi := multi;
  self.GeolocationEnabled := geolocationEnabled;
end;
{$endregion}

{$region 'TShowPdf implemetation'}
constructor TShowPdf.Create;
begin
end;

constructor TShowPdf.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean; value: string);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.Value := value;
end;
{$endregion}

{$region 'TSaveButton implementation'}
constructor TSaveButton.Create;
begin
end;

constructor TSaveButton.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean; value: string);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.Value := value;
end;
{$endregion}

{$region 'TSignature implementation'}
constructor TSignature.Create;
begin
end;

constructor TSignature.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;
end;
{$endregion}

{$region 'TSingleSelect implementation'}
constructor TSingleSelect.Create;
begin
  self.KeyValuePairList := TObjectList<TKeyValuePair>.Create;
end;

constructor TSingleSelect.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean; keyValuePairList: TObjectList<TKeyValuePair> );
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.KeyValuePairList := keyValuePairList
end;
{$endregion}

{$region 'TMultiSelect implementation'}
constructor TMultiSelect.Create;
begin
  self.KeyValuePairList := TObjectList<TKeyValuePair>.Create;
end;

constructor TMultiSelect.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean; keyValuePairList: TObjectList<TKeyValuePair> );
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.KeyValuePairList := keyValuePairList
end;
{$endregion}

{$region 'TText implementation'}
constructor TText.Create;
begin
end;

constructor TText.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
      displayOrder: integer; dummy: boolean; value: string; maxLength: integer; geolocationEnabled:
      boolean; geolocationForced: boolean; geolocationHidden: boolean; barcodeEnabled: boolean; barcodeType: string);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.Value := value;
  self.MaxLength := maxLength;
  self.GeolocationEnabled := geolocationEnabled;
  self.GeolocationForced := geolocationForced;
  self.GeolocationHidden := geolocationHidden;
  self.BarcodeEnabled := barcodeEnabled;
  self.BarcodeType := barcodeType;
end;
{$endregion}

{$region 'TTimer implementation'}
constructor TTimer.Create;
begin
end;

constructor TTimer.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
  displayOrder: integer; dummy: boolean; stopOnSave: boolean);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.StopOnSave := stopOnSave;
end;
{$endregion}

{$region 'TEntitySearch implementation'}
constructor TEntitySearch.Create;
begin
end;

constructor TEntitySearch.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
      displayOrder: integer; dummy: boolean; defaultValue: integer; entityTypeId: integer; isNum:
      boolean; queryType: string; minSearchLenght: integer; barcodeEnabled: boolean; barcodeType: string);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.DefaultValue := defaultValue;
  self.EntityTypeId := entityTypeId;
  self.IsNum := isNum;
  self.QueryType := queryType;
  self.MinSearchLenght := minSearchLenght;
  self.BarcodeEnabled := barcodeEnabled;
  self.BarcodeType := barcodeType;
end;
{$endregion}

{$region 'TEntitySelect implementation'}
constructor TEntitySelect.Create;
begin
end;

constructor TEntitySelect.Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
      displayOrder: integer; dummy: boolean; defaultValue: integer; source: integer);
begin
  inherited Create;
  self.Id := id;
  self.Mandatory := mandatory;
  self.ReadOnly := readOnly;
  self._Label := _label;
  self.Description := TCDataValue.Create;
  self.Description.InderValue := description;
  self.Color := color;
  self.DisplayOrder := displayOrder;
  self.Dummy := dummy;

  self.DefaultValue := defaultValue;
  self.Source := source;
end;
{$endregion}


{$endregion}

{$HINTS ON}
end.
