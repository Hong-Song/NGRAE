unit UIRestore;

interface
uses Classes, SysUtils, Controls, Forms;

ResourceString
  RS_UIRestoreErr_ObjNIL = 'The reference object cannot be nil';

type
  EObjCleaner = class(Exception);
  {
    This interface can be used as an alternative garbage collection in Delphi.
    The following shows an example of use.
    var
      iMyObj : IObjCleaner;
      ....
    begin
      // instantiate the object
      iMyObj := TObjCleaner.Create(TMyObject.Create..);

      // use it
      TMyObject(iMyObj.Obj)
      // no worries about freeing MyObject, as iMyObj does it automatically
      // as soon as the object goes out of scope
    end;
    // TL 22-12-2004
  }

  TPObjType = (potClass, potRecord);

  TPObj = class(TObject)
  private
    FObj: Pointer;
    FObjType: TPObjType;
    FAutoRelease: Boolean;
  public
    property PObj : Pointer read FObj write FObj;
    property ObjType : TPObjType read FObjType write FObjType;
    property AutoRelease : Boolean read FAutoRelease write FAutoRelease;
    constructor Create(Obj: Pointer; ObjType : TPObjType; AutoRelease : Boolean);
    destructor Destroy; override;
  end;

  IObjCleaner = Interface(IInterface)
  ['{1D25B099-BCC0-443F-9253-FFC270E10FD4}']
    function GetObj: TObject;
    procedure SetObj(const Value: TObject);
    property Obj : TObject read GetObj write SetObj;

    function GetPObj: Pointer;
    procedure SetPObj(const Value: Pointer);
    property PObj : Pointer read GetPObj write SetPObj;
  end;

  TObjCleaner = class(TInterfacedObject, IObjCleaner)
  private
    FObj : Pointer;
    FObjType : TPObjType;
    function GetObj: TObject;
    procedure SetObj(const Value: TObject);
    procedure FreeTheObj;

    function GetPObj: Pointer;
    procedure SetPObj(const Value: Pointer);
  public
    property Obj : TObject read GetObj write SetObj;
    property PObj : Pointer read GetPObj write SetPObj;
    constructor Create(Obj : TObject); overload;
    constructor Create(PObj : Pointer; ObjType : TPObjType); overload;
    destructor Destroy; override;
  end;


  IGObjCleaner<T> = Interface(IInterface)
  ['{8505DF90-F916-4CFC-A135-50A97177076F}']
    function GetObj: TObject;
    procedure SetObj(const Value: TObject);
    property Obj : TObject read GetObj write SetObj;

    function GetPObj: Pointer;
    procedure SetPObj(const Value: Pointer);
    property PObj : Pointer read GetPObj write SetPObj;

    function GetGenericObj : T;
    procedure SetGenericObj(const Value : T);
    property GObj : T read GetGenericObj write SetGenericObj;
  end;

  TGObjCleaner<T> = class(TInterfacedObject, IGObjCleaner<T>)
  private
    FObj : Pointer;
    FObjType : TPObjType;
    function GetObj: TObject;
    procedure SetObj(const Value: TObject);
    procedure FreeTheObj;

    function GetPObj: Pointer;
    procedure SetPObj(const Value: Pointer);

    function GetGenericObj : T;
    procedure SetGenericObj(const Value : T);
  public
    property GObj : T read GetGenericObj write SetGenericObj;

    constructor Create(Obj : TObject); overload;
    constructor Create(PObj : Pointer; ObjType : TPObjType); overload;
    destructor Destroy; override;
  end;




  TCursorRestore = class(TObject)
  private
    FCursor : TCursor;
  public
    constructor Create(ACursorValue : TCursor);
    destructor Destroy; override;
  end;

  function CreateObjCleaner(Obj : TObject) : TObjCleaner;
  function CreatePObjCleaner(PObj : Pointer; ObjType : TPObjType) : TObjCleaner;

implementation
uses Dialogs;

{ Other routines }
function CreateObjCleaner(Obj : TObject) : TObjCleaner;
begin
  if not assigned(Obj) or (Obj = nil) then
    raise EObjCleaner.Create(RS_UIRestoreErr_ObjNIL);
  Result := TObjCleaner.Create(Obj);
end;

function CreatePObjCleaner(PObj : Pointer; ObjType : TPObjType) : TObjCleaner;
begin
  if not assigned(PObj) or (PObj = nil) then
    raise EObjCleaner.Create(RS_UIRestoreErr_ObjNIL);
  Result := TObjCleaner.Create(PObj, ObjType);
end;

{ TLinkObj }
constructor TPObj.Create(Obj: Pointer; ObjType : TPObjType; AutoRelease : Boolean);
begin
  inherited Create;
  FObj := Obj;
  FObjType := ObjType;
  FAutoRelease := AutoRelease;
end;

destructor TPObj.Destroy;
var
  IObj : IObjCleaner;
begin
  if (FObjType = potClass) and FAutoRelease then
    IObj := CreateObjCleaner(FObj)
  else if (FObjType = potRecord) and FAutoRelease then
    Dispose(FObj);

  inherited;
end;

{ TObjCleaner }

constructor TObjCleaner.Create(Obj: TObject);
begin
  inherited Create;
  FObj := Obj;
  FObjType := potClass;
end;

constructor TObjCleaner.Create(PObj : Pointer; ObjType : TPObjType);
begin
  inherited Create;
  FObj := PObj;
  FObjType := ObjType;
end;

destructor TObjCleaner.Destroy;
begin
  FreeTheObj;
  inherited Destroy;
end;

procedure TObjCleaner.FreeTheObj;
begin
  try
    if Assigned(FObj) then
      if (FObjType = potClass) then
        FreeAndNil(FObj)
      else
        Dispose(Pointer(FObj));
  except
    // The object may already be destroyed so there is no need to raise
    // an exception here.
  end;
end;

function TObjCleaner.GetObj: TObject;
begin
  Result := TObject(GetPObj);
end;

procedure TObjCleaner.SetObj(const Value: TObject);
begin
  SetPObj(Value);
end;

function TObjCleaner.GetPObj: Pointer;
begin
  Result := FObj;
end;

procedure TObjCleaner.SetPObj(const Value: Pointer);
begin
  FreeTheObj;
  FObj := Value;
end;


{ TObjCleaner2 }

constructor TGObjCleaner<T>.Create(Obj: TObject);
begin
  inherited Create;
  FObj := Obj;
  FObjType := potClass;
end;

constructor TGObjCleaner<T>.Create(PObj : Pointer; ObjType : TPObjType);
begin
  inherited Create;
  FObj := PObj;
  FObjType := ObjType;
end;

destructor TGObjCleaner<T>.Destroy;
begin
  FreeTheObj;
  inherited Destroy;
end;

procedure TGObjCleaner<T>.FreeTheObj;
begin
  try
    if Assigned(FObj) then
      if (FObjType = potClass) then
        FreeAndNil(FObj)
      else
        Dispose(Pointer(FObj));
  except
    // The object may already be destroyed so there is no need to raise
    // an exception here.
  end;
end;

function TGObjCleaner<T>.GetGenericObj: T;
begin
  if Assigned(FObj) then
    Result := T(GetObj);
end;

function TGObjCleaner<T>.GetObj: TObject;
begin
  Result := TObject(GetPObj);
end;

procedure TGObjCleaner<T>.SetGenericObj(const Value: T);
begin
  SetObj(TObject(Value));
end;

procedure TGObjCleaner<T>.SetObj(const Value: TObject);
begin
  SetPObj(Value);
end;

function TGObjCleaner<T>.GetPObj: Pointer;
begin
  Result := FObj;
end;

procedure TGObjCleaner<T>.SetPObj(const Value: Pointer);
begin
  FreeTheObj;
  FObj := Value;
end;

{ TCursorRestore }

constructor TCursorRestore.Create(ACursorValue : TCursor);
begin
  inherited Create;
  FCursor := screen.cursor;
  Screen.Cursor := ACursorValue;
end;

destructor TCursorRestore.Destroy;
begin
  Screen.Cursor := FCursor;
  inherited Destroy;
end;

end.
