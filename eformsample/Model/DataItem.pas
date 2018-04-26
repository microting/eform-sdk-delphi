unit DataItem;

interface

uses Classes;

{$HINTS OFF}
type
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

 {$region children}
 TAudio = class(TDataItem)
 private
    constructor Create; overload;
 public
   Multi: integer;

   constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
    displayOrder: integer; dummy: boolean; multi: integer); overload;
 end;

 TCheckBox = class(TDataItem)
 private
    constructor Create; overload;
 public
   DefaultValue: boolean;
   Selected: boolean;

   constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
    displayOrder: integer; dummy: boolean; defaultValue: boolean; selected: boolean); overload;
 end;

 TComment = class(TDataItem)
 private
    constructor Create; overload;
 public
    Value: string;
    MaxLength: integer;
    SplitScreen: boolean;

    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
     displayOrder: integer; dummy: boolean; value: string; maxLength: integer; splitScreen: boolean); overload;
 end;

 TNone = class(TDataItem)
 private
    constructor Create; overload;
 public
   constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
    displayOrder: integer; dummy: boolean); overload;
 end;

 TNumber = class(TDataItem)
 private
    constructor Create; overload;
 public
    MinValue: string;
    MaxValue: string;
    DefaultValue: integer;
    DecimalCount: integer;
    UnitName: string;

    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
      displayOrder: integer; dummy: boolean; minValue: string; maxValue: string; defaultValue:
      integer; decimalCount: integer; unitName: string); overload;
  end;


 TPicture = class(TDataItem)
 private
   constructor Create; overload;
 public
   Multi: integer;
   GeolocationEnabled: boolean;

   constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
    displayOrder: integer; dummy: boolean; multi: integer; geolocationEnabled: boolean); overload;
 end;

 TShowPdf = class(TDataItem)
 private
    constructor Create; overload;
 public
    Value: string;

    constructor Create(id: integer; mandatory: boolean; readOnly: boolean; _label: string; description: string; color: string;
     displayOrder: integer; dummy: boolean; value: string); overload;
 end;

 {$endregion}

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

  TDataItemColors = (e2f4fb_Blue, f5eafa_Purple, f0f8db_Green,  fff6df_Yellow, ffe4e4_Red, None_Default);

implementation

constructor TDataItem.Create;
begin
end;

{$region children}
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
{$HINTS ON}
end.
