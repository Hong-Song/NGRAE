unit ParamsHelper;

interface
uses
  Data.DB, System.Classes, System.SysUtils, UIRestore;

type
  TParamsHelper = class helper for TParams
    function AddParam(pParamName : string; pFieldType: TFieldType; pValue : Variant): TParam; overload;
    function AddParam(pField: TField) : TParam; overload;
  end;

function StrToParam(s : string) : string;

implementation

{ TParamsHelper }

function TParamsHelper.AddParam(pParamName: string; pFieldType: TFieldType;
  pValue: Variant): TParam;
begin
  Result := nil;
  if pParamName <> '' then
    Result := FindParam(pParamName);

  if not assigned(Result) then
    Result := AddParameter;

  if pParamName <> '' then
    Result.Name := pParamName;

  if pFieldType = ftString then
    Result.AsString := pValue

  else if pFieldType = ftWideString then
    Result.AsWideString := pValue

  else if pFieldType = ftFloat then
    Result.AsFloat := pValue

  else if pFieldType = ftDateTime then
    Result.AsDateTime := pValue

  else if pFieldType = ftInteger then
    Result.AsInteger := pValue

  else
    Result.Value := pValue;
end;

function TParamsHelper.AddParam(pField: TField): TParam;
var
  st: TStringStream; ist: IObjcleaner;
begin
  if pField.IsNull then
  begin
    if pField.DataType in [ftInteger, ftSmallInt] then
      Result := AddParam(pField.FieldName, ftInteger, 0)

    else if pField.DataType in [ftFloat, ftSingle] then
      Result := AddParam(pField.FieldName, ftFloat, 0)

    else if pField.DataType in [ftBoolean] then
      Result := AddParam(pField.FieldName, ftBoolean, False)

    else if pField.DataType in [ftDateTime] then
      Result := AddParam(pField.FieldName, ftDateTime, 0)

    else
      Result := AddParam(pField.FieldName, pField.DataType, '');
  end

  else if pField.DataType = ftBlob then
  begin
    st := TStringStream.Create; ist := CreateObjCleaner(st);
    TBlobField(pField).SaveToStream(st);

    Result := AddParameter;
    Result.Name := pField.FieldName;
    Result.DataType := ftBlob;
    Result.LoadFromStream(st, ftBlob);
  end

  else
    Result := AddParam(pField.FieldName, pField.DataType, pField.Value);
end;

// Global functions and procedures
function StrToParam(s: string): string;
begin
  Result := StringReplace(s, ' ', '', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

end.
