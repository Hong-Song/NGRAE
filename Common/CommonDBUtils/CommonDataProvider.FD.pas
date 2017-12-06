unit CommonDataProvider.FD;

interface

uses
  CommonConnection.Intf, Data.DB, FDConnectionHelper, uGlobal,
  UIRestore, DataSetHelper, NGFDMemTable, DMBase,
  Spring.Container, Spring.Services,
  System.SysUtils, classes, Variants, System.DateUtils,
  FireDAC.UI.Intf, FireDAC.Stan.Intf, FireDAC.Comp.UI,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Comp.Client,
  FireDAC.Stan.Param, FireDAC.DApt;

type
  TNG_FDConnection = class(TInterfacedObject, ICommonConnection)
    FConnectionProperties : ICommonConnectionProperties;
    FServerType : TDatabaseTypeOption;
    FFDConnection, FDefaultConnection : TFDConnection;

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


  TNG_FDDataProvider = class(TInterfacedObject, IFDMemTableProvider)
  private
    FDBConnection : ICommonConnection;
    FUseCloneDataProvider : Boolean;

    procedure CreateDBConnection;
    procedure ReleaseDBConnection;

    function GetDBConnection : ICommonConnection;

    // Private
    procedure MakeMultiRecordSetCDS(cds: TNGFDMemTable);
    procedure CreateFieldInfoCDS(ds : TDataSet; cdsMapping: TNGFDMemTable);
    procedure AssignParamValues(qry : TFDQuery; cds, cdsMapping : TNGFDMemTable);
    function ValidLogDataSets(cdsLog, cdsLogLink: TNGFDMemTable): Boolean;
    procedure AssignFieldValues(qry : TFDQuery;
            cds, cdsLog, cdsLogLink : TNGFDMemTable; TableName : string);
    procedure PrepareQuery(qry: TFDQuery; const SQL: string; Params: TParams);

    procedure DoCopyDataSet(qry : TFDQuery; cds : TNGFDMemTable);
    procedure DoCopyDataSetMulti(qry : TFDQuery; cds : TNGFDMemTable);

    // ICommonDataProvider
    procedure SetConnectionString(const ServerType : TDatabaseTypeOption;
            const HostName, DatabaseName, PortNumber,
            LoginName, LoginPassword: string; OSAuthentication: Boolean);

    function GetConnected: Boolean;
    procedure SetConnected(Value: Boolean);

    procedure CheckCDS(cds : TNGFDMemTable; cdsName: string; CheckActive : Boolean);
    function GetKeyFieldValue(cds: TNGFDMemTable; KeyFieldName: string) : Variant;
    function LocateKeyField(cds: TNGFDMemTable; KeyFieldName: string; KeyValue: Variant) : Boolean;
    function CanEditCDS(cds : TNGFDMemTable) : Boolean;
    function CanInsertCDS(cds: TNGFDMemTable): Boolean;
    function CanCancelCDS(cds: TNGFDMemTable): Boolean;
    function CanSaveCDS(cds: TNGFDMemTable): Boolean;
    function CanDeleteCDS(cds: TNGFDMemTable): Boolean;
    procedure PostCDS(cds : TNGFDMemTable);

    function GetServerDateTimeNow : TDateTime;
    function TextToListText(s : string) : string;
    function KeyFieldsToWhereAnd(TableAlias, KeyFields : string): string;
    function FieldToParam(FieldName: string): string;
    function AddFieldBrackets(FieldName: string): string;

    function GetQueryText(QryIndex: Integer; WhereAnd: string): string;
    function GetQueryTextTable(const TableName, TableAlias, WhereAnd, KeyFields, OrderBy : string) : string;

    procedure CreateParam(cds : TNGFDMemTable; ParamName: string;
            DataType : TFieldType; ParamValue : Variant);
    procedure CreateBlobParam(cds : TNGFDMemTable; ParamName: string;
            BlobType: TBlobType; Stream: TStream);
    procedure CreateDefaultParams(cds: TNGFDMemTable; PageSize, PageNum: Integer);


    procedure QryToCDS(qry : TFDQuery; cds: TNGFDMemTable; CreateFieldDefs: Boolean);

    procedure cdsOpenQryTextExt(QryText: string;
            cds : TNGFDMemTable; ExecQry, CreateFieldDefs : Boolean;
            LAppend: Boolean = False; LUseDP: Boolean = False);
    procedure cdsExecQryText(QryText: string; cds : TNGFDMemTable);
    procedure cdsOpenQryText(QryText: string;
            cds : TNGFDMemTable; CreateFieldDefs : Boolean; LAppend: Boolean = False; LUseDP: Boolean = False);

    procedure cdsExecQryIndex(QryIndex : Integer; WhereAnd: string;  cds : TNGFDMemTable);
    procedure cdsOpenQryIndex(QryIndex : Integer; WhereAnd: string;
            cds : TNGFDMemTable; CreateFieldDefs : Boolean);

    procedure cdsOpenTable(const TableName, TableAlias, WhereAnd, OrderBy: string;
            cds : TNGFDMemTable; CreateFieldDefs : Boolean);
    procedure cdsOpenTableExt(const TableName, TableAlias, WhereAnd, OrderBy : string;
            cds : TNGFDMemTable; CreateFieldDefs : Boolean;
            var DataSetState : TomControlState);

    procedure cdsApplyUpdatesTable(cds : TNGFDMemTable;
            TableName, TableAlias, WhereAnd, KeyFields : string);
    procedure cdsApplyUpdates(cds : TNGFDMemTable);

    procedure cdsOpen(cds: TNGFDMemTable; CreateFieldDefs: Boolean; LAppend: Boolean = False; LUseDP: Boolean = False);
    procedure cdsOpenExt(cds: TNGFDMemTable; CreateFieldDefs: Boolean;
            var DataSetState : TomControlState);

    procedure cdsExec(cds: TNGFDMemTable);

    procedure ExecSQL(SQL : string);

    procedure BeginTransaction;
    procedure CommitTransaction;
    procedure RollbackTransaction;
    procedure ResetQuery(cds : TNGFDMemTable);
    procedure cdsParamGenerate(cds : TNGFDMemTable);

    function CloneDataProvider: ICommonDataProvider<TNGFDMemTable>;
    function GetUseCloneDataProvider: Boolean;
    procedure SetUseCloneDataProvider(Value : Boolean);

    function CheckTableAndField(const s: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TNG_FDConnection }

constructor TNG_FDConnection.Create;
begin
  inherited;
  CreateDefaultConnection;
  CreateConnectionProperties;
end;

procedure TNG_FDConnection.CreateConnectionProperties;
begin
  FConnectionProperties := ServiceLocator.GetService<ICommonConnectionProperties>;
end;

procedure TNG_FDConnection.CreateDefaultConnection;
begin
  FDefaultConnection := TFDConnection.Create(nil);
  FDefaultConnection.LoginPrompt := False;

  FFDConnection := FDefaultConnection;
end;

destructor TNG_FDConnection.Destroy;
begin
  ReleaseConnectionProperties;
  ReleaseDefaultConnection;
  inherited;
end;

procedure TNG_FDConnection.ReleaseConnectionProperties;
begin
  if Assigned(FConnectionProperties) then
  begin
    {$IFNDEF AUTOREFCOUNT}
    GlobalContainer.Release(FConnectionProperties);
    {$ENDIF}
    FConnectionProperties := nil;
  end;
end;

procedure TNG_FDConnection.ReleaseDefaultConnection;
begin
  FreeAndNil(FDefaultConnection);
end;



function TNG_FDConnection.GetAfterConnect: TNotifyEvent;
begin
  Result := FFDConnection.AfterConnect;
end;

function TNG_FDConnection.GetAfterDisconnect: TNotifyEvent;
begin
  Result := FFDConnection.AfterDisconnect;
end;

function TNG_FDConnection.GetConnected: Boolean;
begin
  Result := FFDConnection.Connected;
end;

function TNG_FDConnection.GetConnection: TObject;
begin
  Result := FFDConnection;
end;

function TNG_FDConnection.GetConnectionProperties: ICommonConnectionProperties;
begin
  Result := FConnectionProperties;
end;

function TNG_FDConnection.GetServerType: TDatabaseTypeOption;
begin
  Result := FServerType
end;

procedure TNG_FDConnection.SetAfterConnect(Value: TNotifyEvent);
begin
  FFDConnection.AfterConnect := Value;
end;

procedure TNG_FDConnection.SetAfterDisconnect(Value: TNotifyEvent);
begin
  FFDConnection.AfterDisconnect := Value;
end;

procedure TNG_FDConnection.SetConnected(Value: Boolean);
begin
  if Value then
    FFDConnection.Open
  else
  begin
    if FFDConnection.Connected then
      FFDConnection.Close;
  end;
end;

procedure TNG_FDConnection.SetConnection(Value: TObject);
begin
  CheckFalseFmt(Assigned(Value) and (Value is TFDConnection),
        Err_InvalidPointerInCodeRef, ['Value', 'TNG_FDConnection.SetConnection']);
  ReleaseDefaultConnection;
  FFDConnection := TFDConnection(Value);
end;

procedure TNG_FDConnection.SetConnectionString;
begin
  if FServerType = dtoSQLite then
  begin
    FFDConnection.Params.Values['DriverID'] := 'SQLite';
    FFDConnection.Params.Values['Database'] := FConnectionProperties.DatabaseName;
  end

  else if FServerType = dtoSQLServer then
  begin
    FFDConnection.Params.Values['DriverID'] := 'MSSQL';
    FFDConnection.Params.Values['SERVER'] := FConnectionProperties.ServerName;
    FFDConnection.Params.Values['Database'] := FConnectionProperties.DatabaseName;
    FFDConnection.Params.Values['User_Name'] := FConnectionProperties.ServerLogin;
    FFDConnection.Params.Values['Password'] := FConnectionProperties.ServerPassword;
    if FConnectionProperties.OSAuthentication then
      FFDConnection.Params.Values['OSAuthent'] := 'YES'
    else
      FFDConnection.Params.Values['OSAuthent'] := 'NO';
  end
  else
  begin
    raise EGlobalException.Create('Not Support Database Type!');
  end;

  FConnectionProperties.ConnectionString := FFDConnection.ConnectionString;
end;

procedure TNG_FDConnection.SetServerType(Value: TDatabaseTypeOption);
begin
  FServerType := Value;
end;



{ TNG_FDDataProvider }

function TNG_FDDataProvider.AddFieldBrackets(FieldName: string): string;
begin
  Result := FieldName;
  if FieldName <= ' ' then
    exit;

  if Result[1]  <> '[' then
    Result := '[' + Result;

  if Result[length(Result)] <> ']' then
    Result := Result + ']';
end;

procedure TNG_FDDataProvider.AssignFieldValues(qry: TFDQuery;
        cds, cdsLog, cdsLogLink: TNGFDMemTable; TableName: string);
var
  ValidLogs : Boolean;
  ChangeLogID : Integer;

begin
  CheckFalseFmt(Assigned(qry) and qry.Active,
          Err_InvalidPointerInCodeRef,
          ['qry', 'TNG_FDDataProvider.AssignFieldValues']);

  CheckFalseFmt(Assigned(cds),
          Err_InvalidPointerInCodeRef,
          ['cds', 'TNG_FDDataProvider.AssignFieldValues']);

  ValidLogs := ValidLogDataSets(cdsLog, cdsLogLink);

  if ValidLogs then
  begin
    ChangeLogID := cdsLog.RecordCount + 1;

    cdsLog.Append;

    cdsLog.FieldByName('ChangeLogID').AsInteger := ChangeLogID;
    cdsLog.FieldByName('SystemGUID').AsWideString := NewGUID;
    cdsLog.FieldByName('TableName').AsWideString := TableName;
    cdsLog.FieldByName('ChangeTypeID').AsInteger := UpdateStatusToInt(cds.UpdateStatus);
    cdsLog.FieldByName('ChangeDate').AsDateTime := TTimeZone.Local.ToUniversalTime(Now);

    if (cds.FindField('SystemGUID') <> nil) and (not cds.FieldByName('SystemGUID').IsNull) then
      cdsLog.FieldByName('ItemGUID').asWideString := cds.FieldByName('SystemGUID').AsWideString
    else if (qry.FindField('SystemGUID') <> nil) and (not qry.FieldByName('SystemGUID').IsNull) then
      cdsLog.FieldByName('ItemGUID').asWideString := qry.FieldByName('SystemGUID').AsWideString;
  end;

  cds.ForEachField(
      function (Field: TField) : Boolean
      begin
        Result := (qry.FindField(Field.FieldName) <> nil)
                and ((not Field.IsNull) or (Field.NewValue <> Unassigned));
      end,

      procedure (Field: TField)
      begin
        if ValidLogs then
        begin
          cdsLogLink.Append;
          cdsLogLink.FieldByName('ChangeLogID').AsInteger := ChangeLogID;
          cdsLogLink.FieldByName('FieldName').AsWideString := Field.FieldName;
        end;

        if Field.IsBlob then
        begin
          if ValidLogs then
          begin
            if not qry.EOF then
              TBlobField(cdsLogLink.FieldByName('OriginalValue')).Assign(qry.FieldByName(Field.FieldName));

            TBlobField(cdsLogLink.FieldByName('NewValue')).Assign(Field);
          end;

          if (qry.State in [dsEdit, dsInsert]) then
            TBlobField(qry.FieldByName(Field.FieldName)).Assign(Field);
        end

        else if (Field.DataType <> ftAutoInc) and
                (qry.FieldByName(Field.FieldName).DataType <> ftAutoInc)  then
        begin
          if (qry.FieldByName(Field.FieldName).Value <> Field.Value) then
          begin
            if ValidLogs then
            begin
              if not qry.EOF then
                cdsLogLink.FieldByName('OriginalValue').Value := qry.FieldByName(Field.FieldName).Value;

              cdsLogLink.FieldByName('NewValue').Value := Field.Value;
            end;

            if (qry.State in [dsEdit, dsInsert]) then
              qry.FieldByName(Field.FieldName).Value := Field.Value;
          end;
        end;

        if ValidLogs and (cdsLogLink.State = dsInsert)
            and (cdsLogLink.FieldByName('OriginalValue').Value <> cdsLogLink.FieldByName('NewValue').Value) then
          cdsLogLink.Post;
      end,

      nil
  );

  if ValidLogs and (cdsLogLink.RecordCount > 0) then
  begin
    if cdsLog.State = dsInsert then
      cdsLog.Post;
  end;

end;

procedure TNG_FDDataProvider.AssignParamValues(qry: TFDQuery; cds, cdsMapping: TNGFDMemTable);
var
  i : Integer;
  QryParam : TFDParam;
begin
  for i := 0 to qry.Params.Count - 1 do
  begin
    QryParam := qry.Params[i];
    if cdsMapping.Locate('ParamName', QryParam.Name, [loCaseInsensitive]) then
      QryParam.Value := cds.Fields[cdsMapping.FieldByName('FieldIndex').AsInteger].Value;
  end;
end;

procedure TNG_FDDataProvider.BeginTransaction;
begin
  CheckFalse(GetConnected, RS_DatabaseNotConnected);
  TFDConnection(FDBConnection.Connection).Transaction.StartTransaction;
end;

function TNG_FDDataProvider.CanCancelCDS(cds: TNGFDMemTable): Boolean;
begin
  Result := TDataSet.CanCancelDataSet(cds);
end;

function TNG_FDDataProvider.CanDeleteCDS(cds: TNGFDMemTable): Boolean;
begin
  Result := TDataSet.CanDeleteDataSet(cds);
end;


function TNG_FDDataProvider.CanEditCDS(cds: TNGFDMemTable): Boolean;
begin
  Result := TDataSet.CanEditDataSet(cds);
end;


function TNG_FDDataProvider.CanInsertCDS(cds: TNGFDMemTable): Boolean;
begin
  Result := TDataSet.CanInsertDataSet(cds);
end;


function TNG_FDDataProvider.CanSaveCDS(cds: TNGFDMemTable): Boolean;
begin
  Result := TDataSet.CanSaveDataSet(cds) or (cds.ChangeCount > 0);
end;


procedure TNG_FDDataProvider.cdsApplyUpdates(cds: TNGFDMemTable);
var
  qry : TFDQuery; iqry : IObjCleaner;
  cdsDelta, cdsDelta2 : TNGFDMemTable;
  icdsDelta, icdsDelta2 : IObjCleaner;

  cdsMapping : TNGFDMemTable; icdsMapping : IObjCleaner;
  SearchRecord : Variant;
  SearchFields : string;
begin
  PostCDS(cds);
  CheckFalse(GetConnected, RS_DatabaseNotConnected);
  if not (assigned(cds) and cds.Active and (cds.ChangeCount > 0)) then
    exit;

  // Create vars
  qry := TFDConnection(FDBConnection.Connection).CreateQuery; iqry := CreateObjCleaner(qry);
  cdsDelta := TNGFDMemTable.Create(nil); icdsDelta := CreateObjCleaner(cdsDelta);
  cdsDelta2 := TNGFDMemTable.Create(nil); icdsDelta2 := CreateObjCleaner(cdsDelta2);
  cdsMapping := TNGFDMemTable.Create(nil); icdsMapping := CreateObjCleaner(cdsMapping);

  // Initialize vars
  cdsDelta.Data := cds.Delta;
  cdsDelta2.Data := cds.Delta;

  PrepareQuery(qry, cds.SQL.Text, cds.Params);
  qry.Open;

  CreateFieldInfoCDS(cds, cdsMapping);

  cdsDelta.ForEachRecord(nil,
      procedure (ds : TDataSet)
      begin
        if cdsDelta.UpdateStatus = TUpdateStatus.usModified then
        begin
          cdsDelta2.RecNo := cdsDelta.RecNo - 1;

          SearchFields := cdsDelta2.GetFieldNames(',', True);
          SearchRecord := cdsDelta2.KeyFieldsValue(SearchFields);

          SearchFields := StringReplace(SearchFields, ',', ';', [rfReplaceAll]);
          if qry.Locate(SearchFields, SearchRecord, [loPartialKey]) then
          begin
            qry.Edit;
            AssignFieldValues(qry, cdsDelta, nil, nil, '');
            qry.Post;
          end;
        end

        else if cdsDelta.UpdateStatus = TUpdateStatus.usInserted then
        begin
          AssignParamValues(qry, cdsDelta, cdsMapping);
          qry.Open;
          qry.Append;
          AssignFieldValues(qry, cdsDelta, nil, nil, '');
          qry.Post;
        end

        else if cdsDelta.UpdateStatus = TUpdateStatus.usDeleted then
        begin
          SearchFields := cdsDelta.GetFieldNames(',', True);
          SearchRecord := cdsDelta.KeyFieldsValue(SearchFields);

          while qry.Locate(SearchFields, SearchRecord, []) do
            qry.Delete;
        end

      end
  );

  cds.MergeChangeLog;
end;

procedure TNG_FDDataProvider.cdsApplyUpdatesTable(cds: TNGFDMemTable;
        TableName, TableAlias, WhereAnd, KeyFields: string);
var
  qry : TFDQuery; iqry : IObjCleaner;
  cdsDelta, cdsDelta2 : TNGFDMemTable;
  icdsDelta, icdsDelta2 : IObjCleaner;

  cdsMapping : TNGFDMemTable; icdsMapping : IObjCleaner;
begin
  PostCDS(cds);
  CheckFalse(GetConnected, RS_DatabaseNotConnected);
  if not (assigned(cds) and cds.Active and (cds.ChangeCount > 0)) then
    exit;

  // Create vars
  qry := TFDConnection(FDBConnection.Connection).CreateQuery; iqry := CreateObjCleaner(qry);
  cdsDelta := TNGFDMemTable.CreateDelta(nil, cds); icdsDelta := CreateObjCleaner(cdsDelta);
  cdsDelta2 := TNGFDMemTable.CreateDelta(nil, cds); icdsDelta2 := CreateObjCleaner(cdsDelta2);
  cdsMapping := TNGFDMemTable.Create(nil); icdsMapping := CreateObjCleaner(cdsMapping);

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
            AssignFieldValues(qry, cdsDelta, nil, nil, TableName);
            qry.Post;
          end;
        end

        else if cdsDelta.UpdateStatus = TUpdateStatus.usInserted then
        begin
          qry.SQL.Text := GetQueryTextTable(TableName, TableAlias, 'and 1 = 0', '', '');
          CreateFieldInfoCDS(cds, cdsMapping);

          AssignParamValues(qry, cdsDelta, cdsMapping);
          qry.Open;
          qry.Append;
          AssignFieldValues(qry, cdsDelta, nil, nil, TableName);
          qry.Post;
        end

        else if cdsDelta.UpdateStatus = TUpdateStatus.usDeleted then
        begin
          AssignParamValues(qry, cdsDelta, cdsMapping);
          qry.Open;

          AssignFieldValues(qry, cdsDelta, nil, nil, TableName);

          while not qry.EOF do
            qry.Delete;
        end
      end
  );

  cds.MergeChangeLog;
end;


procedure TNG_FDDataProvider.cdsExec(cds: TNGFDMemTable);
begin
  try
    cdsExecQryText(cds.SQL.Text, cds);
  except
    on E:Exception do
      raise EGlobalException.Create(E.Message);
  end;
end;

procedure TNG_FDDataProvider.cdsExecQryIndex(QryIndex : Integer; WhereAnd: string;  cds : TNGFDMemTable);
begin
  cdsExecQryText(GetQueryText(QryIndex, WhereAnd), cds);
end;

procedure TNG_FDDataProvider.cdsExecQryText(QryText: string;
  cds: TNGFDMemTable);
begin
  cdsOpenQryTextExt(QryText, cds, True, False);
end;

procedure TNG_FDDataProvider.cdsOpen(cds: TNGFDMemTable; CreateFieldDefs : Boolean;
     LAppend: Boolean = False; LUseDP: Boolean = False);
begin
  try
    cdsOpenQryText(cds.SQL.Text, cds, CreateFieldDefs);
  except
    on E:Exception do
      raise EGlobalException.Create(E.Message);
  end;
end;

procedure TNG_FDDataProvider.cdsOpenExt(cds: TNGFDMemTable;
        CreateFieldDefs: Boolean; var DataSetState: TomControlState);
begin
  DataSetState := ocsLoading;
  try
    cdsOpen(cds, CreateFieldDefs);
  finally
    DataSetState := ocsIdle;
  end;
end;

procedure TNG_FDDataProvider.cdsOpenQryIndex(QryIndex : Integer; WhereAnd: string;
        cds : TNGFDMemTable; CreateFieldDefs : Boolean);
begin
  cdsOpenQryText(GetQueryText(QryIndex, WhereAnd), cds, CreateFieldDefs);
end;

procedure TNG_FDDataProvider.cdsOpenQryText(QryText: string;
        cds: TNGFDMemTable; CreateFieldDefs : boolean; LAppend: Boolean = False; LUseDP: Boolean = False);
begin
  cdsOpenQryTextExt(QryText, cds, False, CreateFieldDefs);
end;

procedure TNG_FDDataProvider.cdsOpenQryTextExt(QryText: string;
        cds: TNGFDMemTable; ExecQry, CreateFieldDefs: Boolean;
        LAppend: Boolean = False; LUseDP: Boolean = False);
var
  qryAll : TFDQuery; iqryAll : IObjCleaner;
  cdsEvents : TNGFDMemTable; icdsEvents : IObjCleaner;
begin
  CheckFalse(GetConnected, RS_DatabaseNotConnected);

  CheckFalseFmt(Assigned(cds),
      Err_InvalidPointerInCodeRef,
      ['cds', 'TNG_FDDataProvider.cdsOpenQryTextExt']);

  QryText := Trim(QryText);
  CheckFalseFmt(QryText > ' ',
      Err_InvalidPointerInCodeRef,
      ['QryText', 'TNG_FDDataProvider.cdsOpenQryTextExt']);

  cdsEvents := TNGFDMemTable.Create(nil); icdsEvents := CreateObjCleaner(cdsEvents);
  qryAll := (FDBConnection as TNG_FDConnection).FFDConnection.CreateQuery; iqryAll := CreateObjCleaner(qryAll);

  PrepareQuery(qryAll, QryText, cds.Params);

  if ExecQry then
  begin
    qryAll.ExecSQL;
    exit;
  end;

  qryAll.Open;

  try
    cdsEvents.CopyDataSetEvents(cds, True);
    QryToCDS(qryAll, cds, CreateFieldDefs);
  finally
    cds.CopyDataSetEvents(cdsEvents);
  end;
end;

procedure TNG_FDDataProvider.cdsOpenTable(const TableName, TableAlias, WhereAnd, OrderBy: string;
        cds: TNGFDMemTable; CreateFieldDefs : Boolean);
begin
  cdsOpenQryText(GetQueryTextTable(TableName, TableAlias, WhereAnd, '', OrderBy), cds, CreateFieldDefs);
end;

procedure TNG_FDDataProvider.cdsOpenTableExt(const TableName, TableAlias,
        WhereAnd, OrderBy: string; cds: TNGFDMemTable; CreateFieldDefs: Boolean;
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

procedure TNG_FDDataProvider.cdsParamGenerate(cds: TNGFDMemTable);
var
  qry : TFDQuery; iqry : IObjCleaner;
  i : Integer;
  qryParam : TFDParam;
  cdsParam : TParam;
begin
  if cds.SQL.Text = '' then
    Exit;
  cds.Params.Clear;

  qry := (FDBConnection as TNG_FDConnection).FFDConnection.CreateQuery; iqry := CreateObjCleaner(qry);
  qry.SQL.Text := cds.SQL.Text;

  for i := 0 to qry.Params.Count - 1 do
  begin
    qryParam := qry.Params[i];
    cdsParam := cds.Params.FindParam(qryParam.Name);

    if not Assigned(cdsParam) then
    begin
      cdsParam := cds.Params.AddParameter;
      cdsParam.Name := qryParam.Name;
    end;
    cdsParam.DataType := qryParam.DataType;
  end;
end;

procedure TNG_FDDataProvider.ReleaseDBConnection;
begin
  {$IFNDEF AUTOREFCOUNT}
  GlobalContainer.Release(FDBConnection);
  {$ENDIF}
  FDBConnection := nil;
end;

procedure TNG_FDDataProvider.ResetQuery(cds: TNGFDMemTable);
begin
  if not Assigned(cds) then
    exit;

  cds.Close;
  cds.Params.Clear;
end;

procedure TNG_FDDataProvider.CheckCDS(cds: TNGFDMemTable; cdsName: string; CheckActive: Boolean);
begin
  TDataSet.CheckDataSet(cds, cdsName, CheckActive);
end;

function TNG_FDDataProvider.CheckTableAndField(const s: String): Boolean;
begin
  Result := TFDConnection(FDBConnection.Connection).CheckTableAndField(s);
end;

function TNG_FDDataProvider.CloneDataProvider: ICommonDataProvider<TNGFDMemTable>;
begin
  Result := ServiceLocator.GetService<ICommonDataProvider<TNGFDMemTable>>('FDConnection');
  Result.DBConnection.ConnectionProperties.Assign(FDBConnection.ConnectionProperties);
  Result.DBConnection.SetConnectionString;
  Result.Connected := GetConnected;
end;

procedure TNG_FDDataProvider.CommitTransaction;
begin
  CheckFalse(GetConnected, RS_DatabaseNotConnected);
  if TFDConnection(FDBConnection.Connection).InTransaction then
    TFDConnection(FDBConnection.Connection).Transaction.Commit;
end;

procedure TNG_FDDataProvider.CreateFieldInfoCDS(ds: TDataSet; cdsMapping: TNGFDMemTable);
begin
  CheckFalseFmt(Assigned(ds) and ds.Active,
          Err_InvalidPointerInCodeRef,
          ['ds', 'TNG_FDDataProvider.CreateFieldInfoCDS']);
  CheckFalseFmt(Assigned(cdsMapping),
          Err_InvalidPointerInCodeRef,
          ['cdsMapping', 'TNG_FDDataProvider.CreateFieldInfoCDS']);

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

procedure TNG_FDDataProvider.CreateParam(cds: TNGFDMemTable;
        ParamName: string; DataType: TFieldType; ParamValue: Variant);
var
  p : TParam;
begin
  CheckFalseFmt(Assigned(cds),
          Err_FalseConditionInCodeRef,
          ['cds', 'TNG_FDDataProvider.CreateParam']);
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

destructor TNG_FDDataProvider.Destroy;
begin
  ReleaseDBConnection;
  inherited;
end;

procedure TNG_FDDataProvider.DoCopyDataSet(qry: TFDQuery; cds: TNGFDMemTable);
var
  Lcds : TNGFDMemTable; icds : IObjCleaner;
  Lcds2 : TNGFDMemTable; icds2 : IObjCleaner;
begin
  checkFalseFmt(Assigned(Qry) and Qry.Active, Err_InvalidPointerInCodeRef, ['qry', 'TNG_FDDataProvider.DoCopyDataSet']);
  CheckCDS(cds, '', False);

  Lcds := TNGFDMemTable.Create(nil); icds := CreateObjCleaner(Lcds);
  Lcds2 := TNGFDMemTable.Create(nil); icds2 := CreateObjCleaner(Lcds2);

  MakeMultiRecordSetCDS(Lcds);
  DoCopyDataSetMulti(qry, cds);

  qry.NextRecordSet;
  if qry.Active then
  begin
    Lcds.append;
    TWideMemoField(Lcds.FieldByName('DataSet')).Value := cds.XMLData;
    Lcds.Post;

    while qry.Active do
    begin
      DoCopyDataSetMulti(qry, Lcds2);
      qry.NextRecordSet;
    end;

    cds.XMLData := Lcds.XMLData;
  end;

  cds.MergeChangeLog;
end;

procedure TNG_FDDataProvider.DoCopyDataSetMulti(qry: TFDQuery;
  cds: TNGFDMemTable);
var
  cdsStructure : TNGFDMemTable; icdsStructure : IObjCleaner;
begin
  checkFalseFmt(Assigned(Qry) and Qry.Active, Err_InvalidPointerInCodeRef, ['qry', 'TNG_FDDataProvider.DoCopyDataSet']);
  CheckCDS(cds, '', False);

  cdsStructure := TNGFDMemTable.Create(nil); icdsStructure := CreateObjCleaner(cdsStructure);
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

  cds.CopyDataSet(qry);

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

  cds.MergeChangeLog;
end;

constructor TNG_FDDataProvider.Create;
begin
  inherited;
  CreateDBConnection;
end;

procedure TNG_FDDataProvider.CreateBlobParam(cds : TNGFDMemTable;
  ParamName: string; BlobType: TBlobType; Stream: TStream);
var
  p : TParam;
begin
  CheckFalseFmt(Assigned(cds),
          Err_FalseConditionInCodeRef, ['cds', 'TNG_FDDataProvider.CreateBlobParam']);
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

procedure TNG_FDDataProvider.CreateDBConnection;
begin
  FDBConnection := TNG_FDConnection.Create;
end;

procedure TNG_FDDataProvider.CreateDefaultParams(cds: TNGFDMemTable; PageSize, PageNum: Integer);
begin
  CheckCDS(cds, '', False);
  CreateParam(cds, 'PageSize', ftInteger, PageSize);
  CreateParam(cds, 'PageNum', ftInteger, PageNum);
end;

procedure TNG_FDDataProvider.ExecSQL(SQL: string);
var
  cds : TNGFDMemTable; icds : IObjCleaner;
begin
  cds := TNGFDMemTable.Create(nil); icds := CreateObjCleaner(cds);
  cds.SQL.Text := SQL;
  cdsExec(cds);
end;

function TNG_FDDataProvider.FieldToParam(FieldName: string): string;
begin
  Result := StringReplace(FieldName, ' ', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '?', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '_', [rfReplaceAll]);
end;

function TNG_FDDataProvider.GetConnected: Boolean;
begin
  Result := FDBConnection.Connected;
end;

function TNG_FDDataProvider.GetDBConnection: ICommonConnection;
begin
  Result := FDBConnection;
end;

function TNG_FDDataProvider.GetKeyFieldValue(cds: TNGFDMemTable; KeyFieldName: string): Variant;
begin
  Result := TDataSet(cds).KeyFieldsValue(KeyFieldName);
end;

function TNG_FDDataProvider.GetQueryText(QryIndex: Integer; WhereAnd: string): string;
begin
  Result := StringReplace(DataModuleBase.GetQueryText(QryIndex),
                  '--WHEREAND--', WhereAnd, [rfIgnoreCase, rfReplaceAll]);
end;

function TNG_FDDataProvider.GetQueryTextTable(const TableName, TableAlias,
        WhereAnd, KeyFields, OrderBy: string): string;
begin
  Result := (FDBConnection as TNG_FDConnection).FFDConnection.GetQueryTextTable(
              TableName, TableAlias, WhereAnd, KeyFields, OrderBy);
end;

function TNG_FDDataProvider.GetServerDateTimeNow: TDateTime;
var
  cds : TNGFDMemTable; icds : IObjCleaner;
begin
  cds := TNGFDMemTable.Create(nil); icds := CreateObjCleaner(cds);
  cds.SQL.Text := 'select getDate()';
  cdsOpen(cds, True);
  Result := cds.Fields[0].AsDateTime;
end;

function TNG_FDDataProvider.GetUseCloneDataProvider: Boolean;
begin
  Result := FUseCloneDataProvider;
end;

function TNG_FDDataProvider.KeyFieldsToWhereAnd(TableAlias, KeyFields: string): string;
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

function TNG_FDDataProvider.LocateKeyField(cds: TNGFDMemTable;
  KeyFieldName: string; KeyValue: Variant): Boolean;
begin
  Result := cds.Locate(KeyFieldName, KeyValue, []);
end;

procedure TNG_FDDataProvider.MakeMultiRecordSetCDS(cds: TNGFDMemTable);
begin
  TDataSet.CheckDataSet(cds, 'cds', False);

  cds.Close;
  cds.FieldDefs.Clear;
  cds.FieldDefs.Add('DataSet', ftWideMemo, 0, True);
  cds.CreateDataSet;
end;

procedure TNG_FDDataProvider.PostCDS(cds: TNGFDMemTable);
begin
  if Assigned(cds) and cds.Active and (cds.State in [dsEdit, dsInsert]) then
    cds.post;
end;


procedure TNG_FDDataProvider.PrepareQuery(qry: TFDQuery; const SQL: string;
        Params: TParams);
var
  i: Integer;
  qryParam : TFDParam;
  cdsParam : TParam;
begin
  if not Assigned(qry) then
    exit;

  qry.SQL.Text := SQL;
  if not Assigned(params) then
    exit;

  for i := 0 to qry.Params.Count - 1 do
  begin
    qryParam := qry.Params[i];
    cdsParam := Params.FindParam(qryParam.Name);

    if Assigned(cdsParam) then
    begin
      if cdsParam.DataType = ftBlob then
        qryParam.Assign(cdsParam)
      else
        qryParam.Value := cdsParam.Value;
    end;
  end;
end;

procedure TNG_FDDataProvider.QryToCDS(qry: TFDQuery; cds: TNGFDMemTable;
        CreateFieldDefs: Boolean);
begin
  checkFalseFmt(Assigned(Qry) and Qry.Active,
          Err_InvalidPointerInCodeRef, ['qry', 'TNG_FDDataProvider.QryToCDS']);

  cds.Close;
  if CreateFieldDefs then
    cds.CopyFieldDefs(qry)
  else
    cds.CreateDataSet;

  DoCopyDataSet(qry, cds);

  if cds.RecNo > 1 then
    cds.First;
end;

procedure TNG_FDDataProvider.RollbackTransaction;
begin
  CheckFalse(GetConnected, RS_DatabaseNotConnected);
  if TFDConnection(FDBConnection.Connection).InTransaction then
    TFDConnection(FDBConnection.Connection).Transaction.Rollback;
end;

procedure TNG_FDDataProvider.SetConnected(Value: Boolean);
begin
  FDBConnection.Connected := Value;
end;

procedure TNG_FDDataProvider.SetConnectionString(
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

procedure TNG_FDDataProvider.SetUseCloneDataProvider(Value: Boolean);
begin
  FUseCloneDataProvider := Value;
end;

function TNG_FDDataProvider.TextToListText(s: string): string;
begin
  Result := StringReplace(StringReplace(s, ', ', '', [rfReplaceAll]),
                ',', #13#10, [rfReplaceAll]);
end;

function TNG_FDDataProvider.ValidLogDataSets(cdsLog,
  cdsLogLink: TNGFDMemTable): Boolean;
begin
  Result := False;

  if not (assigned(cdsLog) and assigned(cdsLogLink)) then
    exit;

  if not (cdsLog.Active and cdsLogLink.Active) then
    exit;

  if cdsLog.FindField('SystemGUID') = nil then
    exit;

  if cdsLog.FindField('ChangeLogID') = nil then
    exit;

  if cdsLogLink.FindField('ChangeLogID') = nil then
    exit;

  if cdsLogLink.FindField('FieldName') = nil then
    exit;

  if cdsLogLink.FindField('OriginalValue') = nil then
    exit;

  if cdsLogLink.FindField('NewValue') = nil then
    exit;

  Result := True;
end;



initialization
  GlobalContainer.RegisterType<TNG_FDDataProvider>.Implements<IFDMemTableProvider>(CDP_IDENT_FD);
end.
