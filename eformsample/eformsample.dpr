program eformsample;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  AdminTools in 'AdminTools.pas',
  DllHelper in 'DllHelper.pas',
  Core in 'Core.pas',
  Events in 'Events.pas';

procedure OnCoreStartEvent;
begin
  WriteLn('On core start event');
end;

var
  databaseName: String;
  serverConnectionString: String;
  input: String;
  programm: TAdminTools;
  core: TCore;
begin
  try
    {$region pick database}
    WriteLn('Enter database to use:');
    WriteLn('> If left blank, it will use ''MicrotingDelphi''');
    WriteLn('  Enter name of database to be used');
    ReadLn(databaseName);


    if UpperCase(databaseName) <> '' then
      serverConnectionString := 'Data Source=.\SQLEXPRESS;Initial Catalog=' + databaseName + ';Integrated Security=True';
    if  UpperCase(databaseName) = 'T' then
      serverConnectionString := 'Data Source=.\SQLEXPRESS;Initial Catalog=' + 'MicrotingTest' + ';Integrated Security=True';
    if  UpperCase(databaseName) = 'O' then
      serverConnectionString := 'Data Source=.\SQLEXPRESS;Initial Catalog=' + 'MicrotingOdense' + ';Integrated Security=True';
    if serverConnectionString = '' then
      serverConnectionString := 'Data Source=.\SQLEXPRESS;Initial Catalog=' + 'MicrotingDelphi' + ';Integrated Security=True';

    WriteLn(serverConnectionString);
    {$endregion}


    {$region WriteLn(...text...)}
    WriteLn('');
    WriteLn('Enter one of the following keys to start:');
    WriteLn('  ''A'', for Admin tools');
    WriteLn('  ''I'', for pure run core');
    WriteLn('');
    WriteLn('Any other will close Console');
    ReadLn(input);
    {$endregion}

   if (UpperCase(input) = 'A') then
    begin
       programm := TAdminTools.Create(serverConnectionString);
       programm.RunConsole();
    end
    else if (UpperCase(input) = 'I') then
    begin
       core := TCore.Create;
       core.CoreEvent := OnCoreStartEvent;
       core.Start(serverConnectionString);
       WriteLn('Press any key to exit program');
       ReadLn;
    end;

    WriteLn('');
    WriteLn('Console will close in 1s');
    Sleep(1000);

  {$region ...catch all... }
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      WriteLn('');
      WriteLn('Console will close in 6s');
      Sleep(6000);
    end;
  end;
  {$endregion}
end.



