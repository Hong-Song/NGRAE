unit CommonDataProvider.ADO;

interface
uses
  CommonConnection.Intf, DB, ADODB, DBClient, SysUtils, classes, Variants,
  Spring.Container, Spring.Services, UIRestore, NCADOConnection, DMBase,
  DataSetHelper, uGlobal, CommonConnectionClass, Provider, CommonResourceStrings;

type
  TNG_ADOConnection = class(TInterfacedObject, ICommonConnection)
  private
    FConnectionProperties : ICommonConnectionProperties;
    FServerType : TDatabaseTypeOption;
    FADOConnection, FDefaultConnection : TNCADOConnection;

    // IDBConnection
    function GetConnectionProperties : ICommonConnectionProperties;

    function GetServerType: TDatabaseTypeOption;
    procedure SetServerType(Value: TDatabaseTypeOption);

    // Connection
    function GetConnection : TObject;
    procedure SetConnection(Value: TObject);

    // Connected
    function GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);
    procedure SetConnectionString;

    //Events
    function GetAfterConnect: TNotifyEvent;
    procedure SetAfterConnect(Value: TNotifyEvent);

    function GetAfterDisconnect: TNotifyEvent;
    procedure SetAfterDisconnect(Value: TNotifyEvent);

    // Private
    procedure CreateDefaultConnection;
    procedure ReleaseDefaultConnection;
    procedure CreateConnectionProperties;
    procedure ReleaseConnectionProperties;

  public
    property Connection : TObject read GetConnection write SetConnection;
    property Connected : Boolean read GetConnected write SetConnected;
    constructor Create;
    destructor Destroy; override;
  end;

  TNG_ADODataProvider = class(TInterfacedObject, IClientDataSetProvider)
  private
    FDBConnection : ICommonConnection;
    FUseCloneDataProvider : Boolean;

    procedure CreateDBConnection;
    procedure ReleaseDBConnection;

    function GetDBConnection : ICommonConnection;

    // Private
    procedure CreateFieldInfoCDS(ds : TDataSet; cdsMapping: TClientDataSet);
    procedure AssignParamValues(qry : TADOQuery; cds, cdsMapping : TClientDataSet);
    procedure AssignFieldValues(qry : TADOQuery; cds : TClientDataSet);
    procedure AssignAutoIDValues(qry : TADOQuery; cds, cdsDelta : TClientDataSet);

    // ICommonDataProvider
    procedure SetConnectionString(const ServerType : TDatabaseTypeOption;
            const HostName, DatabaseName, PortNumber,
            LoginName, LoginPassword: string; OSAuthentication: Boolean);

    function GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);
    procedure GetExcelSheetNames(Sheets: TStringList);

    procedure CheckCDS(cds : TClientDataSet; cdsName: string; CheckActive : Boolean);
    function GetKeyFieldValue(cds: TClientDataSet; KeyFieldName: string) : Variant;
    function LocateKeyField(cds: TClientDataSet; KeyFieldName: string; KeyValue: Variant) : Boolean;
    function CanEditCDS(cds : TClientDataSet) : Boolean;
    function CanInsertCDS(cds: TClientDataSet): Boolean;
    function CanCancelCDS(cds: TClientDataSet): Boolean;
    function CanSaveCDS(cds: TClientDataSet): Boolean;
    function CanDeleteCDS(cds: TClientDataSet): Boolean;
    procedure PostCDS(cds : TClientDataSet);

    function GetServerDateTimeNow : TDateTime;
    function TextToListText(s : string) : string;
    function KeyFieldsToWhereAnd(TableAlias, KeyFields : string): string;
    function FieldToParam(FieldName: string): string;
    function AddFieldBrackets(FieldName: string): string;

    function GetQueryText(QryIndex: Integer; WhereAnd: string): string;
    function GetQueryTextTable(const TableName, TableAlias, WhereAnd, KeyFields, OrderBy : string) : string;

    procedure CreateParam(cds : TClientDataSet; ParamName: string;
            DataType : TFieldType; ParamValue : Variant);
    procedure CreateBlobParam(cds : TClientDataSet; ParamName: string;
            BlobType: TBlobType; Stream: TStream);
    procedure CreateDefaultParams(cds: TClientDataSet; PageSize, PageNum: Integer);

    procedure cdsOpenQryTextExt(QryText: string;
            cds : TClientDataSet; ExecQry, CreateFieldDefs : Boolean;
            LAppend: Boolean = False; LUseDP: Boolean = False);
    procedure cdsExecQryText(QryText: string; cds : TClientDataSet);
    procedure cdsOpenQryText(QryText: string;
            cds : TClientDataSet; CreateFieldDefs : Boolean; LAppend: Boolean = False; LUseDP: Boolean = False);

    procedure cdsExecQryIndex(QryIndex : Integer; WhereAnd: string;
            cds : TClientDataSet);
    procedure cdsOpenQryIndex(QryIndex : Integer; WhereAnd: string;
            cds : TClientDataSet; CreateFieldDefs : Boolean);

    procedure cdsOpenTable(const TableName, TableAlias, WhereAnd, OrderBy: string;
            cds : TClientDataSet; CreateFieldDefs : Boolean);
    procedure cdsOpenTableExt(const TableName, TableAlias, WhereAnd, OrderBy : string;
            cds : TClientDataSet; CreateFieldDefs : Boolean;
            var DataSetState : TomControlState);

    procedure cdsApplyUpdatesTable(cds : TClientDataSet;
            TableName, TableAlias, WhereAnd, KeyFields : string);
    procedure cdsApplyUpdates(cds : TClientDataSet);

    function DataSetQryOpen(cds: TClientDataSet): TDataSet;

    procedure cdsOpen(cds: TClientDataSet; CreateFieldDefs: Boolean; LAppend: Boolean = False; LUseDP: Boolean = False);
    procedure cdsOpenExt(cds: TClientDataSet; CreateFieldDefs: Boolean;
            var DataSetState : TomControlState);

    procedure cdsExec(cds: TClientDataSet);
    procedure ExecSQL(SQL : string);
    function  ExecSQLInt(SQL : string): Integer;
    function  ExecSQLStr(SQL : string): String;

    procedure BeginTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    procedure ResetQuery(cds : TClientDataSet);
    procedure cdsParamGenerate(cds : TClientDataSet);

    function CloneDataProvider: ICommonDataProvider<TClientDataSet>;
    function GetUseCloneDataProvider: Boolean;
    procedure SetUseCloneDataProvider(Value : Boolean);

    function CheckTableAndField(const s: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TNG_ADODataProvider }

function TNG_ADODataProvider.AddFieldBrackets(FieldName: string): string;
begin
  Result := FieldName;
  if FieldName <= ' ' then
    exit;

  if Result[1]  <> '[' then
    Result := '[' + Result;

  if Result[length(Result)] <> ']' then
    Result := Result + ']';
end;

procedure TNG_ADODataProvider.AssignAutoIDValues(qry: TADOQuery; cds, cdsDelta: TClientDataSet);
begin
  qry.ForEachField(
      function (Field: TField) : Boolean
      begin
        Result := (Field.DataType = ftAutoInc)
                and (cds.FindField(Field.FieldName) <> nil)
                and (not cdsDelta.FieldByName(Field.FieldName).IsNull);
      end,

      procedure (Field: TField)
      begin
        if cds.Locate(Field.FieldName, cdsDelta.FieldByName(Field.FieldName).Value, []) then
        begin
          cds.Edit;
          cds.FieldByName(Field.FieldName).Value := Field.Value;
          cds.Post;
        end;
      end,

      nil
  );
end;

procedure TNG_ADODataProvider.AssignFieldValues(qry: TADOQuery; cds: TClientDataSet);
begin
  CheckFalseFmt(Assigned(qry) and qry.Active, uGlobal.Err_InvalidPointerInCodeRef,
          ['qry', 'TNG_ADODataProvider.AssignFieldValues']);

  CheckFalseFmt(Assigned(cds), uGlobal.Err_InvalidPointerInCodeRef,
          ['cds', 'TNG_ADODataProvider.AssignFieldValues']);

  cds.ForEachField(
      function (Field: TField) : Boolean
      begin
        Result := (qry.FindField(Field.FieldName) <> nil)
                and ((not Field.IsNull) or (Field.NewValue <> Unassigned));
      end,
      procedure (Field: TField)
      begin
        if Field.IsBlob then
          TBlobField(qry.FieldByName(Field.FieldName)).Assign(Field)
        else if (Field.DataType <> ftAutoInc) and
                (qry.FieldByName(Field.FieldName).DataType <> ftAutoInc)  then
          qry.FieldByName(Field.FieldName).Value := Field.Value;
      end,

      nil
  );
end;

procedure TNG_ADODataProvider.AssignParamValues(qry: TADOQuery; cds, cdsMapping: TClientDataSet);
var
  i : Integer;
  QryParam : TParameter;
begin
  for i := 0 to qry.Parameters.Count - 1 do
  begin
    QryParam := qry.Parameters[i];
    if cdsMapping.Locate('ParamName', QryParam.Name, [loCaseInsensitive]) then
      QryParam.Value := cds.Fields[cdsMapping.FieldByName('FieldIndex').AsInteger].Value;
  end;
end;

procedure TNG_ADODataProvider.BeginTransaction;
begin
  uGlobal.CheckFalse(GetConnected, RS_DatabaseNotConnected);
  TNCADOConnection(FDBConnection.Connection).BeginTrans;
end;

function TNG_ADODataProvider.CanCancelCDS(cds: TClientDataSet): Boolean;
begin
  Result := TDataSet.CanCancelDataSet(cds);
end;

function TNG_ADODataProvider.CanDeleteCDS(cds: TClientDataSet): Boolean;
begin
  Result := TDataSet.CanDeleteDataSet(cds);
end;


function TNG_ADODataProvider.CanEditCDS(cds: TClientDataSet): Boolean;
begin
  Result := TDataSet.CanEditDataSet(cds);
end;


function TNG_ADODataProvider.CanInsertCDS(cds: TClientDataSet): Boolean;
begin
  Result := TDataSet.CanInsertDataSet(cds);
end;


function TNG_ADODataProvider.CanSaveCDS(cds: TClientDataSet): Boolean;
begin
  Result := TDataSet.CanSaveDataSet(cds);
end;


procedure TNG_ADODataProvider.cdsApplyUpdates(cds: TClientDataSet);
begin
//
end;

procedure TNG_ADODataProvider.cdsApplyUpdatesTable(cds: TClientDataSet;
        TableName, TableAlias, WhereAnd, KeyFields: string);
var
  qry : TADOQuery; iqry : IObjCleaner;
  cdsDelta, cdsDelta2 : TClientDataSet;
  icdsDelta, icdsDelta2 : IObjCleaner;

  cdsMapping : TClientDataSet; icdsMapping : IObjCleaner;
begin
  PostCDS(cds);
  uGlobal.CheckFalse(GetConnected, RS_DatabaseNotConnected);
  if not (assigned(cds) and cds.Active and (cds.ChangeCount > 0)) then
    exit;

  // Create vars
  qry := TNCADOConnection(FDBConnection.Connection).CreateQuery; iqry := CreateObjCleaner(qry);
  cdsDelta := TClientDataSet.Create(nil); icdsDelta := CreateObjCleaner(cdsDelta);
  cdsDelta2 := TClientDataSet.Create(nil); icdsDelta2 := CreateObjCleaner(cdsDelta2);
  cdsMapping := TClientDataSet.Create(nil); icdsMapping := CreateObjCleaner(cdsMapping);

  // Initialize vars
  cdsDelta.Data := cds.Delta;
  cdsDelta2.Data := cds.Delta;

  qry.SQL.Text := GetQueryTextTable(TableName, TableAlias, WhereAnd, KeyFields, '');
  CreateFieldInfoCDS(cds, cdsMapping);

  cdsDelta.ForEachRecord(nil,
      procedure (ds : TDataSet)
      begin
        qry.Close;
        if cdsDelta.UpdateStatus = TUpdateStatus.usModified then
        begin
          cdsDelta2.RecNo := cdsDelta.RecNo - 1;

          AssignParamValues(qry, cdsDelta2, cdsMapping);
          qry.Open;
          if not qry.EOF then
          begin
            qry.Edit;
            AssignFieldValues(qry, cdsDelta);
            qry.Post;
          end;
        end

        else if cdsDelta.UpdateStatus = TUpdateStatus.usInserted then
        begin
          AssignParamValues(qry, cdsDelta, cdsMapping);
          qry.Open;
          qry.Append;
          AssignFieldValues(qry, cdsDelta);
          qry.Post;
          AssignAutoIDValues(qry, cds, cdsDelta);
        end

        else if cdsDelta.UpdateStatus = TUpdateStatus.usDeleted then
        begin
          AssignParamValues(qry, cdsDelta, cdsMapping);
          qry.Open;
          while not qry.EOF do
            qry.Delete;
        end

      end
  );

  cds.MergeChangeLog;
end;

procedure TNG_ADODataProvider.cdsExec(cds: TClientDataSet);
begin
  try
    cdsExecQryText(cds.CommandText, cds);
  except
    on E:Exception do
      raise EGlobalException.Create(E.Message);
  end;
end;

procedure TNG_ADODataProvider.cdsExecQryIndex(QryIndex: Integer;
  WhereAnd: string; cds: TClientDataSet);
begin
  cdsExecQryText(GetQueryText(QryIndex, WhereAnd), cds);
end;

procedure TNG_ADODataProvider.cdsExecQryText(QryText: string;
  cds: TClientDataSet);
begin
  cdsOpenQryTextExt(QryText, cds, True, False);
end;

function TNG_ADODataProvider.DataSetQryOpen(cds: TClientDataSet) : TDataSet;
var
  qry : TADOQuery; //iqry : IObjCleaner;
  QryText: string;
  i : Integer;
  qryParam : TParameter;
  cdsParam : TParam;
begin
  uGlobal.CheckFalse(GetConnected, RS_DatabaseNotConnected);

  QryText := Trim(cds.CommandText);
  uGlobal.CheckFalseFmt(QryText > ' ', uGlobal.Err_InvalidPointerInCodeRef,
      ['QryText', 'TNG_ADODataProvider.DataSetQryOpen']);

  qry := (FDBConnection as TNG_ADOConnection).FADOConnection.CreateQuery; //iqry := CreateObjCleaner(qry);
  qry.SQL.Text := QryText;

  for i := 0 to qry.Parameters.Count - 1 do
  begin
    qryParam := qry.Parameters[i];
    cdsParam := cds.Params.FindParam(qryParam.Name);

    if Assigned(cdsParam) then
    begin
      if cdsParam.DataType = ftBlob then
        qryParam.Assign(cdsParam)
      else
        qryParam.Value := cdsParam.Value;
    end;
  end;


  qry.SQL.Text := StringReplace(qry.SQL.Text, '--WHERE--', ' ', [rfIgnoreCase]);
  qry.Open;

  Result := qry;

end;


procedure TNG_ADODataProvider.cdsOpen(cds: TClientDataSet; CreateFieldDefs : Boolean; LAppend: Boolean = False; LUseDP: Boolean = False);
begin
  cdsOpenQryText(cds.CommandText, cds, CreateFieldDefs, LAppend, LUseDP);
end;

procedure TNG_ADODataProvider.cdsOpenExt(cds: TClientDataSet;
        CreateFieldDefs: Boolean; var DataSetState: TomControlState);
begin
  DataSetState := ocsLoading;
  try
    cdsOpen(cds, CreateFieldDefs);
  finally
    DataSetState := ocsIdle;
  end;
end;

procedure TNG_ADODataProvider.cdsOpenQryIndex(QryIndex: Integer; WhereAnd: string;
        cds : TClientDataSet; CreateFieldDefs : Boolean);
begin
  cdsOpenQryText(GetQueryText(QryIndex, WhereAnd), cds, CreateFieldDefs);
end;

procedure TNG_ADODataProvider.cdsOpenQryText(QryText: string;
        cds: TClientDataSet; CreateFieldDefs : boolean; LAppend: Boolean = False; LUseDP: Boolean = False);
begin
  cdsOpenQryTextExt(QryText, cds, False, CreateFieldDefs, LAppend, LUseDP);
end;

procedure TNG_ADODataProvider.cdsOpenQryTextExt(QryText: string;
        cds: TClientDataSet; ExecQry, CreateFieldDefs: Boolean;
        LAppend: Boolean = False; LUseDP: Boolean = False);
  procedure CopyQryToCDS(q: TADOQuery; c: TClientDataSet);
  var
    DP: TDataSetProvider;  iDP: IObjCleaner;
  begin
    DP := TDataSetProvider.Create(nil);  iDP := CreateObjCleaner(DP);
    DP.DataSet := q;
    c.Data := DP.Data;
  end;
var
  qry : TADOQuery; iqry : IObjCleaner;
  cdsStructure : TClientDataSet; icdsStructure : IObjCleaner;
  i : Integer;
  qryParam : TParameter;
  cdsParam : TParam;
  cdsEvents : TClientDataSet; icdsEvents : IObjCleaner;
  sRowXml, str: string;
begin
  if CreateFieldDefs then
    LAppend := False;

  uGlobal.CheckFalse(GetConnected, RS_DatabaseNotConnected);

  uGlobal.CheckFalseFmt(Assigned(cds), uGlobal.Err_InvalidPointerInCodeRef,
      ['cds', 'TNG_ADODataProvider.cdsOpenQryTextExt']);

  QryText := Trim(QryText);
  uGlobal.CheckFalseFmt(QryText > ' ', uGlobal.Err_InvalidPointerInCodeRef,
      ['QryText', 'TNG_ADODataProvider.cdsOpenQryTextExt']);

  cdsStructure := TClientDataSet.Create(nil); icdsStructure := CreateObjCleaner(cdsStructure);
  qry := (FDBConnection as TNG_ADOConnection).FADOConnection.CreateQuery; iqry := CreateObjCleaner(qry);
  qry.SQL.Text := QryText;

  for i := 0 to qry.Parameters.Count - 1 do
  begin
    qryParam := qry.Parameters[i];
    cdsParam := cds.Params.FindParam(qryParam.Name);

    if Assigned(cdsParam) then
    begin
      if cdsParam.DataType = ftBlob then
        qryParam.Assign(cdsParam)
      else
        qryParam.Value := cdsParam.Value;
    end;
  end;
  qry.SQL.Text := StringReplace(qry.SQL.Text, '--WHERE--', ' ', [rfIgnoreCase]);

  if ExecQry then
  begin
    qry.ExecSQL;
    exit;
  end;
  qry.Open;

//  if qry.RecordCount > 10000 then
//  begin
//    str := 'SQL statement:' + #13#10 + #13#10 + qry.SQL.Text;
//    if Length(str) > 200 then
//      str := Copy(str, 1, 200) + #13#10 + '...';
//    raise EGlobalException.Create(RS_LargeSizeDataSetErr + #13#10 + #13#10 + Trim(str));
//  end;

  cdsEvents := TClientDataSet.Create(nil);   icdsEvents := CreateObjCleaner(cdsEvents);
  try
    cdsEvents.CopyDataSetEvents(cds, True);

    if not LAppend then
    begin
      cds.Close;
      if CreateFieldDefs then
        cds.CopyFieldDefs(qry)
      else
        cds.CreateDataSet;
    end;
    cdsStructure.CloneCursor(cds, True, False);

    // Backup ReadOnly property
    cds.ForEachField(
        function (Field: TField) : Boolean
        begin
          Result := Field.ReadOnly;
        end,

        procedure (Field: TField)
        begin
          Field.ReadOnly := False;
        end,

        nil
    );

    if LUseDP then
      CopyQryToCDS(qry, cds)
    else
      cds.CopyDataSet(qry);

    cds.MergeChangeLog;

    // Restore ReadOnly property
    cdsStructure.ForEachField(
        function (Field : TField): Boolean
        begin
          Result := Field.ReadOnly;
        end,

        procedure (Field: TField)
        begin
          cds.FieldByName(Field.FieldName).ReadOnly := True;
        end,

        nil
    );

  finally
    cds.CopyDataSetEvents(cdsEvents);
  end;

  if cds.RecNo > 1 then
    cds.First;
end;

procedure TNG_ADODataProvider.cdsOpenTable(const TableName, TableAlias, WhereAnd, OrderBy: string;
        cds: TClientDataSet; CreateFieldDefs : Boolean);
begin
  cdsOpenQryText(GetQueryTextTable(TableName, TableAlias, WhereAnd, '', OrderBy), cds, CreateFieldDefs);
end;

procedure TNG_ADODataProvider.cdsOpenTableExt(const TableName, TableAlias,
        WhereAnd, OrderBy: string; cds: TClientDataSet; CreateFieldDefs: Boolean;
        var DataSetState: TomControlState);
begin
  DataSetState := ocsLoading;
  try
    cdsOpenTable(TableName, TableAlias, WhereAnd, OrderBy,
            cds, CreateFieldDefs);
  finally
    DataSetState := ocsIdle;
  end;
end;

procedure TNG_ADODataProvider.cdsParamGenerate(cds: TClientDataSet);
var
  qry : TADOQuery; iqry : IObjCleaner;
  i : Integer;
  qryParam : TParameter;
  cdsParam : TParam;
begin
  if cds.CommandText = '' then
    Exit;
  cds.Params.Clear;

  qry := (FDBConnection as TNG_ADOConnection).FADOConnection.CreateQuery; iqry := CreateObjCleaner(qry);
  qry.SQL.Text := cds.CommandText;

  for i := 0 to qry.Parameters.Count - 1 do
  begin
    qryParam := qry.Parameters[i];
    cdsParam := cds.Params.FindParam(qryParam.Name);

    if not Assigned(cdsParam) then
    begin
      cdsParam := cds.Params.AddParameter;
      cdsParam.Name := qryParam.Name;
    end;
    cdsParam.DataType := qryParam.DataType;
  end;
end;

procedure TNG_ADODataProvider.ReleaseDBConnection;
begin
  GlobalContainer.Release(FDBConnection);
  FDBConnection := nil;
end;

procedure TNG_ADODataProvider.ResetQuery(cds: TClientDataSet);
begin
  if not Assigned(cds) then
    exit;

  cds.Close;
  cds.Params.Clear;
end;

procedure TNG_ADODataProvider.CheckCDS(cds: TClientDataSet; cdsName: string; CheckActive: Boolean);
begin
  TDataSet.CheckDataSet(cds, cdsName, CheckActive);
end;

function TNG_ADODataProvider.CheckTableAndField(const s: String): Boolean;
begin
  Result := TNCADOConnection(FDBConnection.Connection).CheckTableAndField(s);
end;

function TNG_ADODataProvider.CloneDataProvider: ICommonDataProvider<TClientDataSet>;
begin
  Result := ServiceLocator.GetService<ICommonDataProvider<TClientDataSet>>('ADOConnection');
  Result.DBConnection.ConnectionProperties.Assign(FDBConnection.ConnectionProperties);
  Result.DBConnection.SetConnectionString;
  Result.Connected := GetConnected;
end;

procedure TNG_ADODataProvider.CommitTransaction;
begin
  uGlobal.CheckFalse(GetConnected, RS_DatabaseNotConnected);
  if TNCADOConnection(FDBConnection.Connection).InTransaction then
    TNCADOConnection(FDBConnection.Connection).CommitTrans;
end;

procedure TNG_ADODataProvider.CreateFieldInfoCDS(ds: TDataSet; cdsMapping: TClientDataSet);
begin
  CheckFalseFmt(Assigned(ds) and ds.Active, uGlobal.Err_InvalidPointerInCodeRef,
          ['ds', 'TNG_ADODataProvider.CreateFieldInfoCDS']);
  CheckFalseFmt(Assigned(cdsMapping), uGlobal.Err_InvalidPointerInCodeRef,
          ['cdsMapping', 'TNG_ADODataProvider.CreateFieldInfoCDS']);

  cdsMapping.Close;
  cdsMapping.FieldDefs.Clear;
  cdsMapping.FieldDefs.Add('FieldName', ftWideString, 50, False);
  cdsMapping.FieldDefs.Add('DisplayName', ftWideString, 50, True);
  cdsMapping.FieldDefs.Add('ParamName', ftWideString, 50, True);
  cdsMapping.FieldDefs.Add('FieldIndex', ftInteger, 0, True);
  cdsMapping.CreateDataSet;

  // FieldName
  ds.ForEachField(nil,
      procedure (Field : TField)
      begin
        cdsMapping.Append;

        cdsMapping.FieldByName('FieldName').Value := Field.FieldName;
        cdsMapping.FieldByName('DisplayName').Value := Field.DisplayName;
        cdsMapping.FieldByName('ParamName').Value := FieldToParam(Field.FieldName);
        cdsMapping.FieldByName('FieldIndex').Value := Field.Index;

        cdsMapping.Post;
      end,
      nil
  );
end;

procedure TNG_ADODataProvider.CreateParam(cds: TClientDataSet;
        ParamName: string; DataType: TFieldType; ParamValue: Variant);
var
  p : TParam;
begin
  CheckFalseFmt(Assigned(cds), uGlobal.Err_FalseConditionInCodeRef, ['cds', 'TNG_ADODataProvider.CreateParam']);
  if cds.Params.FindParam(ParamName) <> nil then
    p := cds.Params.FindParam(ParamName)
  else
  begin
    p := cds.Params.AddParameter;
    p.Name := ParamName;
  end;

  p.DataType := DataType;
  p.Value := ParamValue;
end;

destructor TNG_ADODataProvider.Destroy;
begin
  ReleaseDBConnection;
  inherited;
end;

constructor TNG_ADODataProvider.Create;
begin
  inherited;
  CreateDBConnection;
end;

procedure TNG_ADODataProvider.CreateBlobParam(cds : TClientDataSet;
  ParamName: string; BlobType: TBlobType; Stream: TStream);
var
  p : TParam;
begin
  CheckFalseFmt(Assigned(cds), uGlobal.Err_FalseConditionInCodeRef, ['cds', 'TNG_ADODataProvider.CreateBlobParam']);
  if cds.Params.FindParam(ParamName) <> nil then
    p := cds.Params.FindParam(ParamName)
  else
  begin
    p := cds.Params.AddParameter;
    p.Name := ParamName;
  end;

  p.DataType := BlobType;
  P.LoadFromStream(Stream, BlobType);
end;

procedure TNG_ADODataProvider.CreateDBConnection;
begin
  FDBConnection := TNG_ADOConnection.Create;
end;

procedure TNG_ADODataProvider.CreateDefaultParams(cds: TClientDataSet; PageSize,
  PageNum: Integer);
begin
  CheckCDS(cds, '', False);
  CreateParam(cds, 'PageSize', ftInteger, PageSize);
  CreateParam(cds, 'PageNum', ftInteger, PageNum);
end;

procedure TNG_ADODataProvider.ExecSQL(SQL: string);
var
  cds : TClientDataSet; icds : IObjCleaner;
begin
  cds := TClientDataSet.Create(nil); icds := CreateObjCleaner(cds);
  cds.CommandText := SQL;
  cdsExec(cds);
end;

function TNG_ADODataProvider.ExecSQLInt(SQL: string): Integer;
var
  qry : TADOQuery; iqry : IObjCleaner;
begin
  Result := -1;
  try
    qry := (FDBConnection as TNG_ADOConnection).FADOConnection.CreateQuery; iqry := CreateObjCleaner(qry);
    qry.SQL.Text := SQL;
    qry.Open;
    Result := qry.Fields[0].AsInteger;
  except
    on E:Exception do
      raise EGlobalException.Create(E.Message);
  end;
end;

function TNG_ADODataProvider.ExecSQLStr(SQL: string): String;
var
  qry : TADOQuery; iqry : IObjCleaner;
begin
  Result := '';
  try
    qry := (FDBConnection as TNG_ADOConnection).FADOConnection.CreateQuery; iqry := CreateObjCleaner(qry);
    qry.SQL.Text := SQL;
    qry.Open;
    Result := qry.Fields[0].AsWideString;
  except
    on E:Exception do
      raise EGlobalException.Create(E.Message);
  end;
end;

function TNG_ADODataProvider.FieldToParam(FieldName: string): string;
begin
  Result := StringReplace(FieldName, ' ', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '?', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '_', [rfReplaceAll]);
end;

function TNG_ADODataProvider.GetConnected: Boolean;
begin
  Result := FDBConnection.Connected;
end;

function TNG_ADODataProvider.GetDBConnection: ICommonConnection;
begin
  Result := FDBConnection;
end;

procedure TNG_ADODataProvider.GetExcelSheetNames(Sheets: TStringList);
begin
  CheckFalseFmt(Assigned(Sheets), uGlobal.Err_InvalidPointerInCodeRef, ['Sheets', 'TNG_ADODataProvider.GetExcelSheetNames']);
  NCADOConnection.GetExcelSheetNames((FDBConnection as TNG_ADOConnection).FADOConnection, Sheets);
end;

function TNG_ADODataProvider.GetKeyFieldValue(cds: TClientDataSet; KeyFieldName: string): Variant;
begin
  Result := TDataSet(cds).GetKeyFieldValue(cds, KeyFieldName);
end;

function TNG_ADODataProvider.GetQueryText(QryIndex: Integer; WhereAnd: string): string;
begin
  Result := StringReplace(DataModuleBase.GetQueryText(QryIndex),
                  '--WHEREAND--', WhereAnd, [rfIgnoreCase, rfReplaceAll]);
end;


function TNG_ADODataProvider.GetQueryTextTable(const TableName, TableAlias, WhereAnd, KeyFields, OrderBy: string): string;
begin
  CheckFalseFmt(TableName > ' ', uGlobal.Err_InvalidPointerInCodeRef, ['TableName', 'TNG_ADODataProvider.GetQueryTextTable']);

  // select * from
  Result := Format('select * from [%s] ', [TableName]);

  if TableAlias > ' ' then
    Result := Result + ' ' + TableAlias;
  Result := Result + #13#10' where 1 = 1';

  if KeyFields > ' ' then
    Result := Result + #13#10 + KeyFieldsToWhereAnd(TableAlias, KeyFields);

  // WhereAnd
  if WhereAnd > ' ' then
  begin
    if LowerCase(copy(WhereAnd, 1, 3)) <> 'and' then
      Result := Result + #13#10'and ' + WhereAnd + ' '
    else
      Result := Result + #13#10 + WhereAnd + ' ';
  end;

  // OrderBy
  if OrderBy > ' ' then
    Result := Result + #13#10' order by ' + OrderBy;
end;

function TNG_ADODataProvider.GetServerDateTimeNow: TDateTime;
var
  cds : TClientDataSet; icds : IObjCleaner;
begin
  cds := TClientDataSet.Create(nil); icds := CreateObjCleaner(cds);
  cds.CommandText := 'select getDate()';
  cdsOpen(cds, True);
  Result := cds.Fields[0].AsDateTime;
end;

function TNG_ADODataProvider.GetUseCloneDataProvider: Boolean;
begin
  Result := FUseCloneDataProvider;
end;

function TNG_ADODataProvider.KeyFieldsToWhereAnd(TableAlias, KeyFields: string): string;
var
  lstKeyFields : TStringList; ilstKeyFields : IObjCleaner;
  i : Integer;
  Alias : string;
begin
  Result := '';
  if KeyFields <= ' ' then
    exit;

  lstKeyFields := TStringList.Create; ilstKeyFields := CreateObjCleaner(lstKeyFields);
  lstKeyFields.Text := TextToListText(KeyFields);

  if TableAlias > ' ' then
    Alias := TableAlias + '.'
  else
    Alias := '';

  for i := 0 to lstKeyFields.Count - 1 do
    Result := Result + #13#10' and ' + Alias + AddFieldBrackets(lstKeyFields[i]) +
                                ' = :' + FieldToParam(lstKeyFields[i]);
end;

function TNG_ADODataProvider.LocateKeyField(cds: TClientDataSet;
  KeyFieldName: string; KeyValue: Variant): Boolean;
begin
  Result := TDataSet.LocateKeyField(cds, KeyFieldName, KeyValue);
end;

procedure TNG_ADODataProvider.PostCDS(cds: TClientDataSet);
begin
  TDataSet.PostDataSet(cds);
end;


procedure TNG_ADODataProvider.RollbackTransaction;
begin
  uGlobal.CheckFalse(GetConnected, RS_DatabaseNotConnected);
  if TNCADOConnection(FDBConnection.Connection).InTransaction then
    TNCADOConnection(FDBConnection.Connection).RollbackTrans;
end;

procedure TNG_ADODataProvider.SetConnected(Value: Boolean);
begin
  FDBConnection.Connected := Value;
end;

procedure TNG_ADODataProvider.SetConnectionString(
        const ServerType: TDatabaseTypeOption; const HostName, DatabaseName,
        PortNumber, LoginName, LoginPassword: string; OSAuthentication: Boolean);
begin
  FDBConnection.ServerType := ServerType;
  FDBConnection.ConnectionProperties.ServerName := HostName;
  FDBConnection.ConnectionProperties.DatabaseName := DatabaseName;
  FDBConnection.ConnectionProperties.PortNumber := PortNumber;
  FDBConnection.ConnectionProperties.ServerLogin := LoginName;
  FDBConnection.ConnectionProperties.ServerPassword := LoginPassword;
  FDBConnection.ConnectionProperties.OSAuthentication := OSAuthentication;
  FDBConnection.SetConnectionString;
end;

procedure TNG_ADODataProvider.SetUseCloneDataProvider(Value: Boolean);
begin
  FUseCloneDataProvider := Value;
end;

function TNG_ADODataProvider.TextToListText(s: string): string;
begin
  Result := StringReplace(StringReplace(s, ', ', '', [rfReplaceAll]),
                ',', #13#10, [rfReplaceAll]);
end;

{ TNG_ADOConnection }

constructor TNG_ADOConnection.Create;
begin
  inherited;
  CreateDefaultConnection;
  CreateConnectionProperties;
end;

procedure TNG_ADOConnection.CreateConnectionProperties;
begin
  FConnectionProperties := ServiceLocator.GetService<ICommonConnectionProperties>;
end;

procedure TNG_ADOConnection.CreateDefaultConnection;
begin
  FDefaultConnection := TNCADOConnection.Create(nil);
  FDefaultConnection.LoginPrompt := False;

  FADOConnection := FDefaultConnection;
end;

destructor TNG_ADOConnection.Destroy;
begin
  ReleaseConnectionProperties;
  ReleaseDefaultConnection;
  inherited;
end;

function TNG_ADOConnection.GetAfterConnect: TNotifyEvent;
begin
  Result := FADOConnection.AfterConnect;
end;

function TNG_ADOConnection.GetAfterDisconnect: TNotifyEvent;
begin
  Result := FADOConnection.AfterDisconnect;
end;

function TNG_ADOConnection.GetConnected: Boolean;
begin
  Result := FADOConnection.Connected;
end;

function TNG_ADOConnection.GetConnection: TObject;
begin
  Result := FADOConnection;
end;


function TNG_ADOConnection.GetConnectionProperties: ICommonConnectionProperties;
begin
  Result := FConnectionProperties;
end;

function TNG_ADOConnection.GetServerType: TDatabaseTypeOption;
begin
  Result := FServerType
end;

procedure TNG_ADOConnection.ReleaseConnectionProperties;
begin
  if Assigned(FConnectionProperties) then
  begin
    {$IFNDEF AUTOREFCOUNT}
    GlobalContainer.Release(FConnectionProperties);
    {$ENDIF}
    FConnectionProperties := nil;
  end;
end;

procedure TNG_ADOConnection.ReleaseDefaultConnection;
begin
  FreeAndNil(FDefaultConnection);
end;

procedure TNG_ADOConnection.SetAfterConnect(Value: TNotifyEvent);
begin
  FADOConnection.AfterConnect := Value;
end;

procedure TNG_ADOConnection.SetAfterDisconnect(Value: TNotifyEvent);
begin
  FADOConnection.AfterDisconnect := Value;
end;

procedure TNG_ADOConnection.SetConnected(Value: Boolean);
begin
  if Value then
    FADOConnection.Open
  else
  begin
    if FADOConnection.Connected then
      FADOConnection.CloseConnection;
  end;
end;

procedure TNG_ADOConnection.SetConnection(Value: TObject);
begin
  CheckFalseFmt(Assigned(Value) and (Value is TNCADOConnection),
        Err_InvalidPointerInCodeRef, ['Value', 'TNG_ADOConnection.SetConnection']);
  ReleaseDefaultConnection;
  FADOConnection := TNCADOConnection(Value);
end;

procedure TNG_ADOConnection.SetConnectionString;
begin
  if FServerType = dtoSQLServer then
    (FADOConnection as ISQLServerConnection).SetSQLServerConnectionString(
            FConnectionProperties.ServerName,
            FConnectionProperties.DatabaseName,
            FConnectionProperties.ServerLogin,
            FConnectionProperties.ServerPassword,
            FConnectionProperties.OSAuthentication)
  else if FServerType = dtoExcel then
    (FADOConnection as IExcelConnection).SetExcelConnectionString(FConnectionProperties.DatabaseName);

  FConnectionProperties.ConnectionString := FADOConnection.ConnectionString;
end;

procedure TNG_ADOConnection.SetServerType(Value: TDatabaseTypeOption);
begin
  FServerType := Value;
end;

initialization
  GlobalContainer.RegisterType<TNG_ADODataProvider>.Implements<IClientDataSetProvider>(CDP_IDENT_ADO);
end.






