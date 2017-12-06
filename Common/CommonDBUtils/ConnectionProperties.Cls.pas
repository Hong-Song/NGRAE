unit ConnectionProperties.Cls;

interface

uses
  SysUtils, Classes, UIRestore, Spring.Container, Spring.Services,
  CommonConnection.Intf;

type
  TCommonConnectionProperties = class(TInterfacedObject, ICommonConnectionProperties)
    FItems : TStringList;

    function EncodeValue(s : string): string;
    function DecodeValue(s : string): string;

    procedure SetStrValue(sIdent, Value : string);
    function GetStrValue(sIdent: string): string;

    // UserName
    function GetUserName : string;
    procedure SetUserName(const Value: string);

    // Password
    function GetPassword : string;
    procedure SetPassword(const Value: string);

    // ServerName
    function GetServerName : string;
    procedure SetServerName(const Value: string);

    // DatabaseName
    function GetDatabaseName : string;
    procedure SetDatabaseName(const Value: string);

    // PortNumber
    function GetPortNumber : string;
    procedure SetPortNumber(const Value: string);

    // Protocol
    function GetProtocol : string;
    procedure SetProtocol(const Value: string);

    // ServerLogin
    function GetServerLogin : string;
    procedure SetServerLogin(const Value: string);

    // ServerPassword
    function GetServerPassword : string;
    procedure SetServerPassword(const Value: string);

    // OSAuthentication
    function GetOSAuthentication : boolean;
    procedure SetOSAuthentication(const Value: boolean);

    // ConnectionString
    function GetConnectionString : String;
    procedure SetConnectionString(Value: String);

    function GetAsString: string;
    procedure SetAsString(Value: string);

    // DatabaseType
    function GetServerTypeID : Integer;
    procedure SetServerTypeID(Value: Integer);

    property ServerTypeID : Integer read GetServerTypeID write SetServerTypeID;
    property UserName : string read GetUserName write SetUserName;
    property Password : string read GetPassword write SetPassword;
    property ServerName : string read GetServerName write SetServerName;
    property DatabaseName : string read GetDatabaseName write SetDatabaseName;
    property PortNumber : string read GetPortNumber write SetPortNumber;
    property Protocol: string read GetProtocol write SetProtocol;
    property ServerLogin: string read GetServerLogin write SetServerLogin;
    property ServerPassword: string read GetServerPassword write SetServerPassword;
    property OSAuthentication: boolean read GetOSAuthentication write SetOSAuthentication;
    property ConnectionString : string read GetConnectionString write SetConnectionString;
    property AsString : string read GetAsString write SetAsString;

    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);

    procedure Assign(Value: ICommonConnectionProperties);
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

const
  C_ServerTypeID = 'ServerTypeID';
  C_UserName = 'UserName';
  C_Password = 'Password';
  C_ServerName = 'ServerName';
  C_DatabaseName = 'DatabaseName';
  C_PortNumber = 'PortNumber';
  C_Protocol = 'Protocol';
  C_ServerLogin = 'ServerLogin';
  C_ServerPassword = 'ServerPassword';
  C_OSAuthentication = 'OSAuthentication';
  C_ConnectionString = 'ConnectionString';

  C_EQToText = 'xEQx';
  C_CommaToText = 'xCOx';
  C_SemiColonToText = 'xSCx';
  C_DblQuoteToText = 'xDCx';


constructor TCommonConnectionProperties.Create;
begin
  FItems := TStringList.Create;
  OSAuthentication := False;

  ServerName := '';
  DatabaseName := '';
  PortNumber := '';

  Protocol := '';
  ServerLogin := '';
  ServerPassword := '';

  ConnectionString := '';
end;


procedure TCommonConnectionProperties.Assign(Value: ICommonConnectionProperties);
begin
  ServerTypeID := Value.ServerTypeID;
  UserName := Value.UserName;
  Password := Value.Password;
  ServerName := Value.ServerName;
  DatabaseName := Value.DatabaseName;
  PortNumber := Value.PortNumber;
  Protocol := Value.Protocol;
  ServerLogin := Value.ServerLogin;
  ServerPassword := Value.ServerPassword;
  OSAuthentication := Value.OSAuthentication;
  ConnectionString := Value.ConnectionString;
  AsString := Value.AsString;
end;


function TCommonConnectionProperties.DecodeValue(s: string): string;
begin
  Result := StringReplace(s, C_SemicolonToText, ';', [rfReplaceAll]);
  Result := StringReplace(Result, C_CommaToText, ',', [rfReplaceAll]);
  Result := StringReplace(Result, C_EQToText, '=', [rfReplaceAll]);
  Result := StringReplace(Result, C_DblQuoteToText, '"', [rfReplaceAll]);
end;

destructor TCommonConnectionProperties.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TCommonConnectionProperties.EncodeValue(s: string): string;
begin
  Result := StringReplace(s, '=', C_EQToText, [rfReplaceAll]);
  Result := StringReplace(Result, ',', C_CommaToText, [rfReplaceAll]);
  Result := StringReplace(Result, ';', C_SemicolonToText, [rfReplaceAll]);
  Result := StringReplace(Result, '"', C_DblQuoteToText, [rfReplaceAll]);
end;

function TCommonConnectionProperties.GetAsString: string;
begin
  Result := FItems.Text;
end;

function TCommonConnectionProperties.GetConnectionString: String;
begin
  Result := GetStrValue(C_ConnectionString);
end;

function TCommonConnectionProperties.GetDatabaseName: string;
begin
  Result := GetStrValue(C_DatabaseName);
end;

function TCommonConnectionProperties.GetOSAuthentication: boolean;
begin
  Result := StrToBoolDef(FItems.Values[C_OSAuthentication], False);
end;

function TCommonConnectionProperties.GetPassword: string;
begin
  Result := GetStrValue(C_Password);
end;

function TCommonConnectionProperties.GetPortNumber: string;
begin
  Result := GetStrValue(C_PortNumber);
end;

function TCommonConnectionProperties.GetProtocol: string;
begin
  Result := GetStrValue(C_Protocol);
end;

function TCommonConnectionProperties.GetServerLogin: string;
begin
  Result := GetStrValue(C_ServerLogin);
end;

function TCommonConnectionProperties.GetServerName: string;
begin
  Result := GetStrValue(C_ServerName);
end;

function TCommonConnectionProperties.GetServerPassword: string;
begin
  Result := GetStrValue(C_ServerPassword);
end;

function TCommonConnectionProperties.GetServerTypeID: Integer;
begin
  Result := StrToIntDef(GetStrValue(C_ServerTypeID), 0);
end;

function TCommonConnectionProperties.GetStrValue(sIdent: string): string;
begin
  Result := DecodeValue(FItems.Values[sIdent]);
end;

function TCommonConnectionProperties.GetUserName: string;
begin
  Result := GetStrValue(C_UserName);
end;

procedure TCommonConnectionProperties.LoadFromFile(FileName: string);
var
  LItems : TStringList; iItems : IObjCleaner;
begin
  if not FileExists(FileName) then
    exit;

  LItems := TStringList.Create; iItems := CreateObjCleaner(LItems);
  LItems.LoadFromFile(FileName);

  LItems.Values[C_Password] := DecryptPassword(LItems.Values[C_Password]);
  LItems.Values[C_ServerPassword] := DecryptPassword(LItems.Values[C_ServerPassword]);

  FItems.Text := LItems.Text;
end;

procedure TCommonConnectionProperties.SaveToFile(FileName: string);
var
  LItems : TStringList; iItems : IObjCleaner;
begin
  LItems := TStringList.Create; iItems := CreateObjCleaner(LItems);
  LItems.Text := FItems.Text;

  LItems.Values[C_Password] := EncryptPassword(Password);
  LItems.Values[C_ServerPassword] := EncryptPassword(ServerPassword);

  LItems.SaveToFile(FileName);
end;

procedure TCommonConnectionProperties.SetAsString(Value: string);
var
  LItems : TStringList; iItems : IObjCleaner;
begin
  LItems := TStringList.Create; iItems := CreateObjCleaner(LItems);
  LItems.Text := StringReplace(Value, ',', #13#10, [rfReplaceAll]);

  ServerTypeID := StrToIntDef(LItems.Values[C_ServerTypeID], 0);
  UserName := LItems.Values[C_UserName];
  Password := LItems.Values[C_Password];
  ServerName := LItems.Values[C_ServerName];
  DatabaseName := LItems.Values[C_DatabaseName];
  PortNumber := LItems.Values[C_PortNumber];
  Protocol := LItems.Values[C_Protocol];
  ServerLogin := LItems.Values[C_ServerLogin];
  ServerPassword := LItems.Values[C_ServerPassword];
  OSAuthentication := StrToBoolDef(LItems.Values[C_OSAuthentication], False);
  ConnectionString := LItems.Values[C_ConnectionString];
end;

procedure TCommonConnectionProperties.SetConnectionString(Value: String);
begin
  SetStrValue(C_ConnectionString, Value);
end;

procedure TCommonConnectionProperties.SetDatabaseName(const Value: string);
begin
  SetStrValue(C_DatabaseName, Value);
end;

procedure TCommonConnectionProperties.SetOSAuthentication(const Value: boolean);
begin
  FItems.Values[C_OSAuthentication] := BoolToStr(Value);
end;

procedure TCommonConnectionProperties.SetPassword(const Value: string);
begin
  SetStrValue(C_Password, Value);
end;

procedure TCommonConnectionProperties.SetPortNumber(const Value: string);
begin
  SetStrValue(C_PortNumber, Value);
end;

procedure TCommonConnectionProperties.SetProtocol(const Value: string);
begin
  SetStrValue(C_Protocol, Value);
end;

procedure TCommonConnectionProperties.SetServerLogin(const Value: string);
begin
  SetStrValue(C_ServerLogin, Value);
end;

procedure TCommonConnectionProperties.SetServerName(const Value: string);
begin
  SetStrValue(C_ServerName, Value);
end;

procedure TCommonConnectionProperties.SetServerPassword(const Value: string);
begin
  SetStrValue(C_ServerPassword, Value);
end;

procedure TCommonConnectionProperties.SetServerTypeID(Value: Integer);
begin
  FItems.Values[C_ServerTypeID] := IntToStr(Value);
end;

procedure TCommonConnectionProperties.SetStrValue(sIdent, Value: string);
begin
  FItems.Values[sIdent] := EncodeValue(Value);
end;

procedure TCommonConnectionProperties.SetUserName(const Value: string);
begin
  SetStrValue(C_UserName, Value);
end;

initialization
  GlobalContainer.RegisterType<TCommonConnectionProperties>.Implements<ICommonConnectionProperties>;
end.
