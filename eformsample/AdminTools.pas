unit AdminTools;

interface

uses
  SysUtils, DllHelper;

type

  TAdminTools = class
  public
    constructor Create(serverConnectionString: String);
    procedure RunConsole;
end;

implementation

constructor TAdminTools.Create(serverConnectionString: string);
begin
  inherited Create;
  TDllHelper.GetInstance.AdminTools_Create(serverConnectionString);
end;

procedure TAdminTools.RunConsole;
var
  input, token, reply: String;
begin
  while true do
  begin

    {$region text}
    WriteLn('');
    WriteLn('Press the following keys to run:');
    WriteLn('');
    WriteLn('''P'' to prime, configure and add sites database');
    WriteLn('');
    WriteLn('> ''Q'' to close admin tools');
    ReadLn(input);
    {$endregion}

    if UpperCase(input) = 'Q' then
      exit
    else if UpperCase(input) = 'P' then
    begin
       WriteLn('Prime, configure and add sites to  database');
       WriteLn('Enter your token:');
       ReadLn(token);
       reply := TDllHelper.GetInstance.AdminTools_DbSetup(token);
       if reply = '' then
          WriteLn('Done')
       else
          WriteLn(reply);
    end;

  end;
end;

end.


