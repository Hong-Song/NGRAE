unit CommonConnection.Intf;

interface

uses Spring.Container, Spring.Services, System.Classes, Data.DB, DBClient, System.SysUtils,
     FireDAC.Stan.Intf, FireDAC.Stan.Option, NGFDMemTable,
     FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
     FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

const
  CDP_IDENT_FD = 'FDConnection';
  CDP_IDENT_ADO = 'ADOConnection';

type
  EGlobalException = class(Exception);

  ICommonConnectionProperties = Interface
    ['{CE6C8655-AF96-49E0-BDCF-8F8124EEE200}']

    function EncodeValue(s : string): string;
    function DecodeValue(s : string): string;

    procedure SetStrValue(sIdent, Value : string);
    function GetStrValue(sIdent: string): string;


    // DatabaseType
    function GetServerTypeID : Integer;
    procedure SetServerTypeID(Value: Integer);
    property ServerTypeID : Integer read GetServerTypeID write SetServerTypeID;

    // UserName
    function GetUserName : string;
    procedure SetUserName(const Value: string);
    property UserName : string read GetUserName write SetUserName;

    // Password
    function GetPassword : string;
    procedure SetPassword(const Value: string);
    property Password : string read GetPassword write SetPassword;

    // ServerName
    function GetServerName : string;
    procedure SetServerName(const Value: string);
    property ServerName : string read GetServerName write SetServerName;

    // DatabaseName
    function GetDatabaseName : string;
    procedure SetDatabaseName(const Value: string);
    property DatabaseName : string read GetDatabaseName write SetDatabaseName;

    // PortNumber
    function GetPortNumber : string;
    procedure SetPortNumber(const Value: string);
    property PortNumber : string read GetPortNumber write SetPortNumber;

    // Protocol
    function GetProtocol : string;
    procedure SetProtocol(const Value: string);
    property Protocol: string read GetProtocol write SetProtocol;

    // ServerLogin
    function GetServerLogin : string;
    procedure SetServerLogin(const Value: string);
    property ServerLogin: string read GetServerLogin write SetServerLogin;

    // ServerPassword
    function GetServerPassword : string;
    procedure SetServerPassword(const Value: string);
    property ServerPassword: string read GetServerPassword write SetServerPassword;

    // OSAuthentication
    function GetOSAuthentication : boolean;
    procedure SetOSAuthentication(const Value: boolean);
    property OSAuthentication: boolean read GetOSAuthentication write SetOSAuthentication;

    // ConnectionString
    function GetConnectionString : String;
    procedure SetConnectionString(Value: String);
    property ConnectionString : string read GetConnectionString write SetConnectionString;

    // AsString
    function GetAsString: string;
    procedure SetAsString(Value: string);
    property AsString : string read GetAsString write SetAsString;

    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);

    procedure Assign(Value: ICommonConnectionProperties);

  End;


  TDatabaseTypeOption = (dtoSQLServer, dtoOracle, dtoACCESS, dtoExcel, dtoSQLite, dtoAll);

  ICommonConnection = Interface
    ['{33785BE9-9C57-43D7-A99B-091D2F4CA038}']

    function GetConnectionProperties : ICommonConnectionProperties;
    property ConnectionProperties : ICommonConnectionProperties read GetConnectionProperties;

    function GetServerType: TDatabaseTypeOption;
    procedure SetServerType(Value: TDatabaseTypeOption);
    property ServerType : TDatabaseTypeOption read GetServerType write SetServerType;

    // Connection
    function GetConnection : TObject;
    procedure SetConnection(Value: TObject);
    property Connection : TObject read GetConnection write SetConnection;

    // Connected
    function GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);
    property Connected : Boolean read GetConnected write SetConnected;

    //Events
    function GetAfterConnect: TNotifyEvent;
    procedure SetAfterConnect(Value: TNotifyEvent);
    property AfterConnect : TNotifyEvent read GetAfterConnect write SetAfterConnect;

    function GetAfterDisconnect: TNotifyEvent;
    procedure SetAfterDisconnect(Value: TNotifyEvent);
    property AfterDisconnect : TNotifyEvent read GetAfterDisconnect write SetAfterDisconnect;

    procedure SetConnectionString;
  End;


  TomControlState = (ocsLoading, ocsBusy, ocsIdle);


  ICommonDataProvider<T: TDataSet> = Interface
  ['{D600A174-6D01-45FE-A4C3-4CAEABA404C4}']
    function GetDBConnection : ICommonConnection;
    property DBConnection : ICommonConnection read GetDBConnection;

    procedure SetConnectionString(const ServerType : TDatabaseTypeOption;
            const HostName, DatabaseName, PortNumber,
            LoginName, LoginPassword: string; OSAuthentication: Boolean);

    function GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);
    property Connected : Boolean read GetConnected write SetConnected;

    // Checks
    procedure CheckCDS(cds : T; cdsName: string; CheckActive : Boolean);
    function GetKeyFieldValue(cds: T; KeyFieldName: string) : Variant;
    function LocateKeyField(cds: T; KeyFieldName: string; KeyValue: Variant) : Boolean;
    function CanEditCDS(cds : T) : Boolean;
    function CanInsertCDS(cds: T): Boolean;
    function CanCancelCDS(cds: T): Boolean;
    function CanSaveCDS(cds: T): Boolean;
    function CanDeleteCDS(cds: T): Boolean;
    procedure PostCDS(cds : T);

    function GetServerDateTimeNow : TDateTime;

    // Remove blanks
    function TextToListText(s : string): string;

    // construct the where and clause for key fields
    function KeyFieldsToWhereAnd(TableAlias, KeyFields : string): string;

    // Replaces all invalid characters with _
    function FieldToParam(FieldName: string): string;

    // cds Params
    procedure CreateParam(cds : T; ParamName: string;
            DataType : TFieldType; ParamValue: Variant);
    procedure CreateBlobParam(cds : T; ParamName: string;
            BlobType: TBlobType; Stream: TStream);
    procedure CreateDefaultParams (cds : T; PageSize, PageNum : Integer);

    // Add field identifier
    function AddFieldBrackets(FieldName: string): string;

    function GetQueryText(QryIndex: Integer; WhereAnd: string): string;
    function GetQueryTextTable(const TableName, TableAlias, WhereAnd, KeyFields, OrderBy : string) : string;

    procedure cdsExecQryText(QryText: string; cds : T);
    procedure cdsOpenQryTextExt(QryText: string;
            cds : T; ExecQry, CreateFieldDefs : Boolean;
            LAppend: Boolean = False; LUseDP: Boolean = False);
    procedure cdsOpenQryText(QryText: string;
            cds : T; CreateFieldDefs : Boolean; LAppend: Boolean = False; LUseDP: Boolean = False);

    procedure cdsExecQryIndex(QryIndex : Integer; WhereAnd: string;  cds : T);

    procedure cdsOpenQryIndex(QryIndex : Integer; WhereAnd: string;
            cds : T; CreateFieldDefs : Boolean);

    procedure cdsOpenTable(const TableName, TableAlias, WhereAnd, OrderBy : string;
            cds : T; CreateFieldDefs : Boolean);
    procedure cdsOpenTableExt(const TableName, TableAlias, WhereAnd, OrderBy : string;
            cds : T; CreateFieldDefs : Boolean;
            var DataSetState : TomControlState);

    procedure cdsApplyUpdatesTable(cds: T;
            TableName, TableAlias, WhereAnd, KeyFields : string);
    procedure cdsApplyUpdates(cds: T);

    // SQL.Text contains SQL Statement, Params contains all params and values
    procedure cdsOpen(cds: T; CreateFieldDefs: Boolean; LAppend: Boolean = False; LUseDP: Boolean = False);
    procedure cdsOpenExt(cds: T; CreateFieldDefs: Boolean;
            var DataSetState : TomControlState);

    procedure cdsExec(cds: T);

    procedure ExecSQL(SQL : string);

    procedure BeginTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    procedure ResetQuery(cds : T);
    procedure cdsParamGenerate(cds : T);

    function CloneDataProvider : ICommonDataProvider<T>;
    function GetUseCloneDataProvider: Boolean;
    procedure SetUseCloneDataProvider(Value : Boolean);
    property UseCloneDataProvider : Boolean read GetUseCloneDataProvider write SetUseCloneDataProvider;

    function CheckTableAndField(const s: String): Boolean;
  End;

  IFDMemTableProvider = ICommonDataProvider<TNGFDMemTable>;
  IClientDataSetProvider = ICommonDataProvider<TClientDataSet>;


  function EncryptPassword(s: string): string;
  function DecryptPassword(s: string): string;

  procedure CreateGlobalDataProvider(ClassIdent : string);
  procedure ReleaseGlobalDataProvider;
  function NewGUID: string;
  function UpdateStatusToInt(Value: TUpdateStatus): Integer;


var
  // GlobalDataProvider
  cdpGlobal : IFDMemTableProvider;


ResourceString
  RS_DatabaseNotConnected              = 'Database not connected';
  RS_LargeSizeDataSetErr = 'Returning dataset is too large. Please check the filters and try again.';


implementation


function EncryptPassword(s: string): string;
begin
  Result := s;
end;

function DecryptPassword(s: string): string;
begin
  Result := s;
end;


procedure CreateGlobalDataProvider(ClassIdent : string);
begin
  ReleaseGlobalDataProvider;
  cdpGlobal := ServiceLocator.GetService<IFDMemTableProvider>(ClassIdent);
end;

procedure ReleaseGlobalDataProvider;
begin
  if Assigned(cdpGlobal) then
  begin
    {$IFNDEF AUTOREFCOUNT}
    GlobalContainer.Release(cdpGlobal);
    {$ENDIF}
    cdpGlobal := nil;
  end;
end;


function NewGUID: string;
var
  GUID: TGUID;
begin
  System.SysUtils.CreateGUID(GUID);
  Result := GUID.ToString;
end;

function UpdateStatusToInt(Value: TUpdateStatus): Integer;
begin
  if Value = usInserted then
    Result := 1

  else if Value = usModified then
    Result := 2

  else if Value = usDeleted then
    Result := 3

  else
    Result := 0;
end;

initialization
  cdpGlobal := nil;

finalization
  ReleaseGlobalDataProvider;


end.
