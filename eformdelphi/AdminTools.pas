unit AdminTools;

interface

uses
  SysUtils, DllHelper;

type
  {$region 'TAdminTools declaration'}
  TAdminTools = class
  public
    constructor Create(serverConnectionString: String);
    procedure Run;
    function DbSetup(token: String): String;
  end;
  {$endregion}

implementation

{$region 'TAdminTools implementation'}
constructor TAdminTools.Create(serverConnectionString: string);
begin
  inherited Create;
  TDllHelper.GetInstance.AdminTools_Create(serverConnectionString);
end;

function TAdminTools.DbSetup(token: String): String;
begin
   Result := TDllHelper.GetInstance.AdminTools_DbSetup(token);
end;

procedure TAdminTools.Run;
var
  input, token, reply: String;
begin
  while true do
  begin

    {$region 'text'}
    WriteLn('');
    WriteLn('Press the following keys to run:');
    WriteLn('');
    WriteLn('> ''P'' to prime, configure and add sites database');
    WriteLn('  ''S'' to clear database for prime, configuration and sites');
    WriteLn('  ''I'' to check database is primed');
    WriteLn('');
    WriteLn('> ''C'' to retract all known eForm on devices');
    WriteLn('  ''E'' to retract all known Entities');
    WriteLn('  ''D'' to clear database for data');
    WriteLn('  ''T'' to clear database for templates');
    WriteLn('  ''A'' to complete database reset (all of the above)');
    WriteLn('');
    WriteLn('> ''M'' to force migration of database');
    WriteLn('  ''R'' to reload settings from Microting (Token needs to be in db)');
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
    end
    else if UpperCase(input) = 'I' then
    begin
       WriteLn('Chesk is database primed');
       reply := TDllHelper.GetInstance.AdminTools_DbSetupCompleted;
    end
    else if UpperCase(input) = 'M' then
    begin
       WriteLn('MigrateDb');
       reply := TDllHelper.GetInstance.AdminTools_MigrateDb;
    end
    else if UpperCase(input) = 'R' then
    begin
       WriteLn('DbSettingsReloadRemote');
       reply := TDllHelper.GetInstance.AdminTools_DbSettingsReloadRemote;
    end;

    if reply = '' then
       WriteLn('Done')
    else
       WriteLn(reply);
  end;
end;
{$endregion}

end.


