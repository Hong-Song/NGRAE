unit FDConnectionHelper;

interface
uses
  classes, SysUtils, Data.DB, NGFDMemTable, DBXJSON, Variants,

  FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Comp.Client, FireDac.Stan.Param,
  FireDAC.UI.Intf, FireDAC.ConsoleUI.Wait, FireDAC.Comp.UI,

  {$IFDEF SQLServerDriver}
  FireDAC.Phys.MSSQLDef, FireDAC.Phys.ODBCBase, FireDAC.Phys.MSSQL,
  {$ENDIF}

  {$IFDEF SQLiteDriver}
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite,
  {$ENDIF}

  UIRestore, DataSetHelper, ParamsHelper, uGlobal;

ResourceString
  RS_InvalidNumberOfParams = 'Invalid number of parameters(%d). Expected(%d)';

type
  TFDConnectionHelper = class helper for TFDConnection
  private
  public
    procedure LogIt(lstLogs: TStringList; s : string);

    function CreateQuery : TFDQuery;
    function GetQueryTextTable(const TableName, TableAlias, WhereAnd, KeyFields, OrderBy : string) : string;

    procedure CreateFieldInfoCDS(ds: TDataSet; cdsMapping: TNGFDMemTable);
    function TextToListText(s: string): string;
    function KeyFieldsToWhereAnd(TableAlias, KeyFields : string): string;
    function AddFieldBrackets(FieldName: string): string;
    function FieldToParam(FieldName: string): string;

    procedure SetSQLServerConnectionString(const HostName, DBName,
            UserName, Password: string;
            OSAuthentication : Boolean);
    procedure SetAccessConnectionString(const UserName, Password, FileName: string);
    procedure SetExcelConnectionString(const FileName : string);
    procedure SetSQLiteConnectionString(const FileName : string);

    procedure AssignQueryParams(cds : TNGFDMemTable; qry : TFDQuery);
    procedure AssignParamValues(qry: TFDQuery; cds, cdsMapping: TNGFDMemTable);
    procedure AssignFieldValues(qry: TFDQuery; cds: TNGFDMemTable);

    procedure PrepareQuery(qry: TFDQuery; const SQL: string; Params: TParams);
    function DSOpenSQL(const SQL : string; Params : TParams) : TFDQuery;
    procedure ExecSQL(const SQL : string; Params : TParams);
    function OpenSQL(const SQL : string; Params : TParams;
            var XmlData: string) : Boolean;

    procedure ApplyUpdates(const XmlData, InsertSQL, UpdateSQL, DeleteSQL : string);
    procedure ApplyUpdatesByTable(const TableName, TableAlias,
            WhereAnd, KeyFields, XMLData : string);
    procedure ApplyUpdatesByQuery(const SQL, XMLData : string;
            Params : TParams; lstLogs : TStringList);

    function KeyFieldsValue(DataSet: TDataSet; KeyFields: string;
            lstLogs : TStringList): Variant;
    function GetFieldNames(DataSet: TDataSet; Separator: string;
            IgnoreNulls : Boolean): string;

    function CheckTableAndField(const s : string): Boolean;
  end;


implementation

var
  iCount : Integer;

function TFDConnectionHelper.DSOpenSQL(const SQL: string;
        Params: TParams): TFDQuery;
begin
  Result := CreateQuery;
  PrepareQuery(Result, SQL, Params);
  Result.Open;
end;

procedure TFDConnectionHelper.ExecSQL(const SQL: string; Params: TParams);
var
  qry : TFDQuery; iqry : IObjCleaner;
begin
  qry := TFDQuery.Create(nil); iqry := CreateObjCleaner(qry);

  PrepareQuery(qry, SQL, Params);
  qry.ExecSQL;
end;

function TFDConnectionHelper.FieldToParam(FieldName: string): string;
begin
  Result := StringReplace(FieldName, ' ', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '?', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '_', [rfReplaceAll]);
end;

function TFDConnectionHelper.GetFieldNames(DataSet: TDataSet; Separator: string;
        IgnoreNulls: Boolean): string;
var
  i : Integer;
begin
  Result := '';
  if not (Assigned(DataSet) and DataSet.Active) then
    exit;

  for i := 0 to DataSet.FieldCount - 1 do
  begin
    if IgnoreNulls and DataSet.Fields[i].isnull then
      continue;

    if Result > '' then
      Result := Result + Separator;

    Result := Result + DataSet.Fields[i].FieldName;
  end;
end;

function TFDConnectionHelper.GetQueryTextTable(const TableName, TableAlias,
  WhereAnd, KeyFields, OrderBy: string): string;
begin
  CheckFalseFmt(TableName > ' ', Err_InvalidPointerInCodeRef, ['TableName', 'TFDConnectionHelper.GetQueryTextTable']);

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

function TFDConnectionHelper.KeyFieldsToWhereAnd(TableAlias, KeyFields: string): string;
var
  lstKeyFields : IGObjCleaner<TStringList>;
  i : Integer;
  Alias : string;
begin
  Result := '';
  if KeyFields <= ' ' then
    exit;

  lstKeyFields := TGObjCleaner<TStringList>.Create(TStringList.Create);
  lstKeyFields.Obj.Text := TextToListText(KeyFields);

  if TableAlias > ' ' then
    Alias := TableAlias + '.'
  else
    Alias := '';

  for i := 0 to lstKeyFields.Obj.Count - 1 do
    Result := Result + #13#10' and ' + Alias + AddFieldBrackets(lstKeyFields.Obj[i]) +
                                ' = :' + FieldToParam(lstKeyFields.Obj[i]);
end;

function TFDConnectionHelper.KeyFieldsValue(DataSet: TDataSet; KeyFields: string;
        lstLogs : TStringList): Variant;
var
  lst : IGObjCleaner<TStringList>;
  i : Integer;
begin
  Result := Unassigned;
  if not (Assigned(DataSet) and DataSet.Active and (not DataSet.EOF)) then
    exit;

  if (pos(',', KeyFields) <= 0) then
  begin
    Result := DataSet.FieldByName(KeyFields).AsWideString;
  end
  else
  begin
    lst := TGObjCleaner<TStringList>.Create(TStringList.Create);

    lst.Obj.Text := StringReplace(KeyFields, ',', #13#10, [rfReplaceAll]);

    Result := VarArrayCreate([0, lst.Obj.Count-1], varVariant);
    for i := 0 to lst.Obj.count - 1 do
      Result[i] := DataSet.FieldByName(lst.Obj[i]).asWideString;
  end;
end;

procedure TFDConnectionHelper.LogIt(lstLogs : TStringList; s: string);
begin
  if not Assigned(lstLogs) then
    exit;

  lstLogs.add(s);
end;

function TFDConnectionHelper.OpenSQL(const SQL: string; Params: TParams;
        var XmlData : string): Boolean;
var
  cds : IGObjCleaner<TNGFDMemTable>;
  qry : IGObjCleaner<TFDQuery>;
begin
  qry := TGObjCleaner<TFDQuery>.Create(CreateQuery);
  cds := TGObjCleaner<TNGFDMemTable>.Create(TNGFDMemTable.Create(nil));

  PrepareQuery(qry.Obj, SQL, Params);
  qry.Obj.Open;

  cds.Obj.CopyFieldDefs(qry.Obj);
  cds.Obj.Open;

  cds.Obj.CopyDataSet(qry.Obj);
  cds.Obj.MergeChangeLog;
  XmlData := cds.Obj.XMLData;

  cds := nil;
  result := (XmlData <> '');
end;

procedure TFDConnectionHelper.PrepareQuery(qry: TFDQuery;
        const SQL: string; Params: TParams);
var
  i : Integer;
  p : TParam;
begin
  if not Assigned(qry) then
    exit;

  qry.SQL.Text := SQL;
  if not Assigned(params) then
    exit;

  for i := 0 to qry.Params.Count - 1 do
  begin
    p := Params.FindParam(qry.Params[i].Name);
    if not assigned(p) then
      continue;

    if p.DataType = ftBlob then
      TBlobField(qry.Params[i]).Assign(p)
    else
      qry.Params[i].Value := p.Value;
  end;
end;

procedure TFDConnectionHelper.SetAccessConnectionString(const UserName,
        Password, FileName: string);
begin

end;

procedure TFDConnectionHelper.SetExcelConnectionString(const FileName: string);
begin

end;

procedure TFDConnectionHelper.SetSQLiteConnectionString(const FileName: string);
begin
  Params.Values['DriverID'] := 'SQLite';
  Params.Values['Database'] := FileName;
end;

procedure TFDConnectionHelper.SetSQLServerConnectionString(const HostName,
        DBName, UserName, Password: string;
        OSAuthentication: Boolean);
begin
  Params.Values['DriverID'] := 'MSSQL';
  Params.Values['SERVER'] := HostName;
  Params.Values['Database'] := DBName;
  Params.Values['User_Name'] := UserName;
  Params.Values['Password'] := Password;
  if OSAuthentication then
    Params.Values['OSAuthent'] := 'YES'
  else
    Params.Values['OSAuthent'] := 'NO';
end;

function TFDConnectionHelper.TextToListText(s: string): string;
begin
  Result := StringReplace(StringReplace(s, ', ', '', [rfReplaceAll]),
                ',', #13#10, [rfReplaceAll]);
end;

{ TFDConnectionHelper }

function TFDConnectionHelper.AddFieldBrackets(FieldName: string): string;
begin
  Result := FieldName;
  if FieldName <= ' ' then
    exit;

  if Result[1]  <> '[' then
    Result := '[' + Result;

  if Result[length(Result)] <> ']' then
    Result := Result + ']';
end;


procedure TFDConnectionHelper.ApplyUpdates(const XmlData, InsertSQL,
        UpdateSQL, DeleteSQL: string);
var
  cds, cds2 : IGObjCleaner<TNGFDMemTable>;
  qry : IGObjCleaner<TFDQuery>;
  sFieldName : string;
  i : Integer;
begin
  cds := TGObjCleaner<TNGFDMemTable>.Create(TNGFDMemTable.Create(nil));
  cds2 := TGObjCleaner<TNGFDMemTable>.Create(TNGFDMemTable.Create(nil));

  qry := TGObjCleaner<TFDQuery>.Create(CreateQuery);
  qry.Obj.SQL.Text := InsertSQL;

  cds.Obj.XMLData := XmlData;
  cds.Obj.Open;
  cds2.Obj.CloneCursor(cds.Obj, True, True);

  cds.Obj.First;
  while not cds.Obj.Eof do
  begin
    if (cds.Obj.UpdateStatus = usInserted) then
    begin
      qry.Obj.SQL.Text := InsertSQL;
      qry.Obj.Open;
      qry.Obj.Insert;

      qry.Obj.CopyRecord(cds.Obj);
      qry.Obj.Post;
    end

    else if cds.Obj.UpdateStatus = usDeleted then
    begin
      qry.Obj.SQL.Text := DeleteSQL;
      AssignQueryParams(cds.Obj, qry.Obj);
      Qry.Obj.ExecSQL;
    end

    else if cds.Obj.UpdateStatus = usModified then
    begin
      cds2.Obj.RecNo := cds.Obj.RecNo-1;
      Qry.Obj.SQL.Text := UpdateSQL;
      AssignQueryParams(cds.Obj, qry.Obj);
      Qry.Obj.Open;

      if not qry.Obj.EOF then
      begin
        qry.Obj.Edit;

        for i := 0 to cds.Obj.FieldCount - 1 do
        begin
          if (cds.Obj.Fields[i].Value = cds2.Obj.Fields[i].Value) then
            continue;

          sFieldName := cds.Obj.Fields[i].FieldName;
          if qry.Obj.FindField(sFieldName) = nil then
            continue;

          if not cds.Obj.Fields[i].IsNull then
            qry.Obj.FieldByName(sFieldName).Value := cds.Obj.Fields[i].Value;
        end;

        qry.Obj.Post;
      end;
    end;

    cds.Obj.Next;
  end;
end;

procedure TFDConnectionHelper.ApplyUpdatesByQuery(const SQL, XMLData: string;
        Params: TParams; lstLogs : TStringList);
var
  qry : IGObjCleaner<TFDQuery>;
  cdsDelta, cdsDelta2 : IGObjCleaner<TNGFDMemTable>;
  SearchRecord : Variant;
  SearchFields : string;
begin
  // Create Vars
  qry := TGObjCleaner<TFDQuery>.Create(CreateQuery);
  cdsDelta := TGObjCleaner<TNGFDMemTable>.Create(TNGFDMemTable.Create(nil));
  cdsDelta2 := TGObjCleaner<TNGFDMemTable>.Create(TNGFDMemTable.Create(nil));

  PrepareQuery(qry.Obj, SQL, Params);
  qry.Obj.Open;

  cdsDelta.Obj.XMLData := XmlData;
  cdsDelta.Obj.Open;
  cdsDelta2.Obj.CloneCursor(cdsDelta.Obj, True, False);

  cdsDelta.Obj.First;
  while not cdsDelta.Obj.EOF do
  begin
      if cdsDelta.Obj.UpdateStatus = TUpdateStatus.usModified then
      begin
        LogIt(lstLogs, 'ApplyUpdatesByQuery: usModified');
        cdsDelta2.Obj.RecNo := cdsDelta.Obj.RecNo - 1;
        SearchFields := GetFieldNames(TDataSet(cdsDelta2.Obj), ',', True);
        SearchRecord := KeyFieldsValue(TDataSet(cdsDelta2.Obj), SearchFields, lstLogs);
        LogIt(lstLogs, 'ApplyUpdatesByQuery 5.11 - SearchField = ' + SearchFields);

        if qry.Obj.Locate(SearchFields, SearchRecord, []) then
        begin
          LogIt(lstLogs, 'ApplyUpdatesByQuery 5.121');
          qry.Obj.Edit;
          AssignFieldValues(qry.Obj, cdsDelta.Obj);
          qry.Obj.Post;
          LogIt(lstLogs, 'ApplyUpdatesByQuery 5.122');
        end;
        LogIt(lstLogs, 'ApplyUpdatesByQuery 5.13');
      end

      else if cdsDelta.Obj.UpdateStatus = TUpdateStatus.usInserted then
      begin
        LogIt(lstLogs, 'ApplyUpdatesByQuery: usInserted');
        exit;
        qry.Obj.Append;
        AssignFieldValues(qry.Obj, cdsDelta.Obj);
        qry.Obj.Post;
      end

      else if cdsDelta.Obj.UpdateStatus = TUpdateStatus.usDeleted then
      begin
        LogIt(lstLogs, 'ApplyUpdatesByQuery: usDeleted');
        exit;
        SearchFields := GetFieldNames(TDataSet(cdsDelta.Obj), ',', True);
        SearchRecord := KeyFieldsValue(TDataSet(cdsDelta.Obj), SearchFields, lstLogs);

        while qry.Obj.Locate(SearchFields, SearchRecord, []) do
          qry.Obj.Delete;
      end;
      LogIt(lstLogs, 'ApplyUpdatesByQuery: Next');

      cdsDelta.Obj.Next;
  end;
end;

procedure TFDConnectionHelper.ApplyUpdatesByTable(const TableName, TableAlias,
        WhereAnd, KeyFields, XMLData: string);
var
  qry : IGObjCleaner<TFDQuery>;
  cdsDelta, cdsDelta2, cdsMapping : IGObjCleaner<TNGFDMemTable>;
begin
  // Create Vars
  cdsDelta := TGObjCleaner<TNGFDMemTable>.Create(TNGFDMemTable.Create(nil));
  cdsDelta2 := TGObjCleaner<TNGFDMemTable>.Create(TNGFDMemTable.Create(nil));
  cdsMapping := TGObjCleaner<TNGFDMemTable>.Create(TNGFDMemTable.Create(nil));

  qry := TGObjCleaner<TFDQuery>.Create(CreateQuery);
  qry.Obj.SQL.Text := GetQueryTextTable(TableName, TableAlias, WhereAnd, KeyFields, '');

  cdsDelta.Obj.XMLData := XmlData;
  cdsDelta.Obj.Open;
  cdsDelta2.Obj.CloneCursor(cdsDelta.Obj, True, False);

  CreateFieldInfoCDS(cdsDelta.Obj, cdsMapping.Obj);

  cdsDelta.Obj.First;
  cdsDelta.Obj.ForEachRecord(nil,
      procedure (ds : TDataSet)
      begin
        if cdsDelta.Obj.UpdateStatus = TUpdateStatus.usModified then
        begin
          cdsDelta2.Obj.RecNo := cdsDelta.Obj.RecNo - 1;

          AssignParamValues(qry.Obj, cdsDelta2.Obj, cdsMapping.Obj);
          qry.Obj.Open;
          if not qry.Obj.EOF then
          begin
            qry.Obj.Edit;
            AssignFieldValues(qry.Obj, cdsDelta.Obj);
            qry.Obj.Post;
          end;
        end

        else if cdsDelta.Obj.UpdateStatus = TUpdateStatus.usInserted then
        begin
          AssignParamValues(qry.Obj, cdsDelta.Obj, cdsMapping.Obj);
          qry.Obj.Open;
          qry.Obj.Append;
          AssignFieldValues(qry.Obj, cdsDelta.Obj);
          qry.Obj.Post;
          //AssignAutoIDValues(qry.Obj, cds., cdsDelta);
        end

        else if cdsDelta.Obj.UpdateStatus = TUpdateStatus.usDeleted then
        begin
          AssignParamValues(qry.Obj, cdsDelta.Obj, cdsMapping.Obj);
          qry.Obj.Open;
          while not qry.Obj.EOF do
            qry.Obj.Delete;
        end

      end
  );

end;

procedure TFDConnectionHelper.AssignFieldValues(qry: TFDQuery; cds: TNGFDMemTable);
begin
  CheckFalseFmt(Assigned(qry) and qry.Active,
          Err_InvalidPointerInCodeRef,
          ['qry', 'TFDConnectionHelper.AssignFieldValues']);

  CheckFalseFmt(Assigned(cds),
          Err_InvalidPointerInCodeRef,
          ['cds', 'TFDConnectionHelper.AssignFieldValues']);

  cds.ForEachField(
      function (Field: TField) : Boolean
      begin
        Result := (qry.FindField(Field.FieldName) <> nil)
                and ((not Field.IsNull) or (Field.NewValue <> Unassigned))
                and (not qry.FieldByName(Field.FieldName).ReadOnly)
                and (qry.FieldByName(Field.FieldName).DataType <> ftAutoInc);
      end,
      procedure (Field: TField)
      begin
        if Field.IsBlob then
          TBlobField(qry.FieldByName(Field.FieldName)).Assign(Field)
        else if Field.DataType <> ftAutoInc then
          qry.FieldByName(Field.FieldName).Value := Field.Value;
      end,

      nil
  );
end;


procedure TFDConnectionHelper.AssignParamValues(qry: TFDQuery; cds, cdsMapping: TNGFDMemTable);
var
  i : Integer;
  QryParam : TFDParam;
begin
  for i := 0 to qry.ParamCount - 1 do
  begin
    QryParam := qry.Params[i];
    if cdsMapping.Locate('ParamName', QryParam.Name, [loCaseInsensitive]) then
      QryParam.Value := cds.Fields[cdsMapping.FieldByName('FieldIndex').AsInteger].Value;
  end;
end;


procedure TFDConnectionHelper.AssignQueryParams(cds: TNGFDMemTable;
        qry: TFDQuery);
var
  i : Integer;
  f : TField;
begin
  for i := 0 to qry.ParamCount - 1 do
  begin
    f := cds.FindField(qry.Params[i].Name);
    if f <> nil then
      qry.Params[i].Value := f.Value;
  end;
end;

function TFDConnectionHelper.CheckTableAndField(const s: string): Boolean;
var
  qry : TFDQuery;
  lst : TStringList;

  procedure PrepareStringItems;
  var
    s1, sCheck : String;
    i, n : Integer;
  begin
    s1 := StringReplace(StringReplace(s, '[', '', [rfReplaceAll]), ']', '', [rfReplaceAll]);
    n := 1;
    if Pos('DoIfTableExists', s) > 0 then
      sCheck := 'DoIfTableExists'
    else if Pos('DoIfTableNotExist', s) > 0 then
      sCheck := 'DoIfTableNotExist'
    else if Pos('DoIfFieldExists', s) > 0 then
    begin
      sCheck := 'DoIfFieldExists';
      n := 2;
    end
    else if Pos('DoIfFieldNotExist', s) > 0 then
    begin
      sCheck := 'DoIfFieldNotExist';
      n := 2;
    end;

    lst.Text := StringReplace(trim(StringReplace(s1, sCheck, '', [rfIgnoreCase])), ',', #13#10, [rfReplaceAll]);
    CheckFalse(lst.Count = n, Format(RS_InvalidNumberOfParams, [lst.Count, n]));

    lst[0] := StringReplace(lst[0], '.', '].[', [rfReplaceAll]);
    for i := 0 to lst.Count - 1 do
      lst[i] := format('[%s]', [trim(lst[i])]);
  end;

begin
  Result := False;
  if not ((Pos('DoIfTable', s) > 0) or (Pos('DoIfField', s) > 0)) then
    exit;

  lst := TStringList.Create;
  qry := CreateQuery;
  try
    PrepareStringItems;

    if (Pos('DoIfTableExists', s) > 0) then
    begin
      qry.SQL.Text := Format('select top 1 1 As CheckField from %s', [lst[0]]);
      try
        qry.Open;
        Result := qry.Active;
      except
      end;
    end
    else if (Pos('DoIfTableNotExist', s) > 0) then
    begin
      qry.SQL.Text := Format('select top 1 1 As CheckField from %s', [lst[0]]);
      try
        qry.Open;
      except
        Result := True;
      end;
    end
    else if (Pos('DoIfFieldExists', s) > 0) then
    begin
      qry.SQL.Text := Format('select top 1 %s As CheckField from %s', [lst[1], lst[0]]);
      try
        qry.Open;
        Result := qry.Active;
      except
      end;
    end
    else if (Pos('DoIfFieldNotExist', s) > 0) then
    begin
      qry.SQL.Text := Format('select top 1 %s As CheckField from %s', [lst[1], lst[0]]);
      try
        qry.Open;
      except
        Result := True;
      end;
    end
  finally
    lst.Free;
    qry.Free;
  end;
end;

procedure TFDConnectionHelper.CreateFieldInfoCDS(ds: TDataSet; cdsMapping: TNGFDMemTable);
begin
  CheckFalseFmt(Assigned(ds) and ds.Active,
          Err_InvalidPointerInCodeRef,
          ['ds', 'TFDConnectionHelpder.CreateFieldInfoCDS']);

  CheckFalseFmt(Assigned(cdsMapping),
          Err_InvalidPointerInCodeRef,
          ['cdsMapping', 'TFDConnectionHelpder.CreateFieldInfoCDS']);

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


function TFDConnectionHelper.CreateQuery: TFDQuery;
begin
  inc(iCount);

  Result := TFDQuery.Create(nil);
  Result.Name := 'FDQry' + IntToStr(iCount);
  Result.Connection := self;
end;

initialization
  iCount := 0;
end.
