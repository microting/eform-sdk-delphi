unit DataItem;

interface

type
  TDataItem = class

  end;

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

end.
