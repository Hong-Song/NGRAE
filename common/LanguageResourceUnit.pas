unit LanguageResourceUnit;

interface
uses
  classes, Controls, Graphics, TypInfo, Dialogs, StdCtrls, ActnList, Menus, Consts,
  Windows, Messages, SysUtils, Variants, Forms, Buttons, DMBase;

const
  C_Messages = 'Messages';
  C_MessageBox = 'Message Box';
  C_MessageType = 'Message Type';
  C_EditType = 'Edit Type';
  C_Entity = 'Entity';
  C_Common = 'Common';

  // Edit Type
  C_etEdit='Edit';
  C_etView='View';

  C_MinAppVersion = '5.1';
  C_ResourceStringSection = 'ResourceString';

type
  ILanguageForm = Interface
  ['{99b53aa9-6643-4dde-8b1c-35c1e39a797e}']
    procedure ApplyLanguageSettings;
  end;

  TLanguageResource = class(TComponent)
  private
    FLanguageFolderPath: string;
    lstMemLocRSMapping, lstItems : TStringList;
    lstSections : TStringList;
    FDefaultRSList : TStringList;
    FCurrReadingDir :string;
    FAppName, FAppVersion : string;
    FDrcFileName: string;
    cSection : string;
    FIsActive : boolean;

    function GetItem(sSection, sIdent: string): string;
    procedure SetItem(sSection, sIdent: string; const Value: string);
    function GetLanguageFolderPath: string;
    function GetAppName: string;
    function GetAppVersion: string;
    function GetItemList: TStringList;
    procedure SetLanguageFolderPath(const Value: string);
    procedure SetAppName(const Value: string);
    procedure SetAppVersion(const Value: string);
    function GetItemDefault(sSection, sIdent, sDefault: string): string;
    function GetRSItemDefault(sSection, sIdent, sDefault: string): string;
    procedure LoadDefaultRSList;
    procedure LoadRsIdMapping;
  public
    RSArray : array of string;
    property IsActive : boolean read FIsActive;
    property DrcFileName : string read FDrcFileName write FDrcFileName;
    property Item[sSection, sIdent : string] : string read GetItem write SetItem;
    property ItemDef[sSection, sIdent, sDefault : string] : string read GetItemDefault;
    property RSItemDef[sSection, sIdent, sDefault : string] : string read GetRSItemDefault;
    property Section: string read cSection write cSection;
    property DefaultRSList: TStringList read FDefaultRSList;
    property ItemList : TStringList read GetItemList;
    property CurrReadingDir : string read FCurrReadingDir write FCurrReadingDir;
    property LanguageFolderPath : string read GetLanguageFolderPath write SetLanguageFolderPath;
    property AppName: string read GetAppName write SetAppName;
    property AppVersion: string read GetAppVersion write SetAppVersion;
    procedure ReadFromFile(const sFileName : string);
    procedure ReadFromResDefLanguage;
    procedure SaveToFile(const sFileName : string; lst : TStringList);

    procedure ModifyFontsFor(ctrl: TWinControl; fontName : string);
    procedure ModifyReadingDir(Control:TComponent; readingDir: string);
    function IsValidLangFile(fileName : string) : boolean;
    procedure ReadLabelAndHint(Section, LabelIdent, HintIdent : string;
          ComponentArray : Array of TComponent);
    function MsgId(const Section, MsgIdent: string): string;

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function GetLangFileList : TStringList;
    procedure ApplyLanguageForAllChildFramesInForm(frm: TForm);
    procedure ApplyLanguageSetting(ctrl : TComponent);

  end;

var
  LanguageResource : TLanguageResource;

implementation
uses INIFiles, UIRestore, ActnMan, ActionToolBarCustom, JvCaptionButton, DBCtrls,
    JvDBComboBox, DBGrids, ExtCtrls, ComCtrls, JvToolEdit, JvCtrls, JvCaptionPanel,
    JvDBGrid, JvRollOut, JvLabel, DBActns, StdActns, VirtualTrees, JvStatusBar,
    JvDotNetControls, GridView, cxGridDBTableView, DBClient, JvRadioGroup, LibraryHookUnit;

type
  PStrData = ^TStrData;
  TStrData = record
    Ident: Integer;
    Str: String;
  end;

var
  HookLoadResString:THook;
  HookLoadStr:THook;
  HookFmtLoadStr:THook;

function LoadResString(ResStringRec: PResStringRec): String;
var
  Len: Integer;
  sResult : string;
begin
  if (ResStringRec=nil) or (ResStringRec.Identifier>=64*1024) then
    exit;

  if LanguageResource.IsActive then
  begin
    sResult := LanguageResource.RSArray[ResStringRec.Identifier];
  end;

  if sResult <> ''  then
  begin
    Result := sResult;
    exit;
  end;

  Result := '';
  Len := 0;
  While Length(Result)<=Len+1 do begin
    if Length(Result) = 0 then
      SetLength(Result, 1024)
    else
      SetLength(Result, Length(Result) * 2);
    Len := LoadStringW(FindResourceHInstance(ResStringRec.Module^),
      ResStringRec.Identifier, PWideChar(Result), Length(Result));
  end;
  SetLength(Result, Len);
end;

function SysUtilsEnumStringModules(Instance: {$IF CompilerVersion<=22}LongInt{$ELSE}NativeInt{$IFEND}; Data: Pointer): Boolean;
var
  Buffer: array [0..1023] of Char; // WideChar in Delphi 2008, AnsiChar before that
begin
  with PStrData(Data)^ do begin
    SetString(Str, Buffer,
      LoadString(Instance, Ident, @Buffer[0], sizeof(Buffer)));
    Result := Str = '';
  end;
end;

function SysUtilsFindStringResource(Ident: Integer): string;
var
  StrData: TStrData;
begin
  StrData.Ident := Ident;
  StrData.Str := '';
  EnumResourceModules(SysUtilsEnumStringModules, @StrData);
  Result := StrData.Str;
end;

function SysUtilsLoadStr(Ident: Integer): string;
begin
  if LanguageResource.IsActive then
  begin
    Result := LanguageResource.RSArray[Ident];
    if Result = '' then
      Result := SysUtilsFindStringResource(Ident);
  end
  else
    Result := SysUtilsFindStringResource(Ident);
end;

function SysUtilsFmtLoadStr(Ident: Integer; const Args: array of const): string;
begin
  FmtStr(Result, SysUtilsLoadStr(Ident),Args);
end;

{ TLanguageResource }
procedure TLanguageResource.LoadRsIdMapping;
var
  lstTemp : TStringList;
  iLstTemp : IObjCleaner;
  drcF : TextFile;
  line : string;
begin
  lstTemp := TStringList.Create;
  iLstTemp := CreateObjCleaner(lstTemp);

  if not FileExists(FDrcFileName) then
    exit;

  AssignFile(drcF, FDrcFileName);
  FileMode := fmOpenRead;
  Reset(drcF);

  lstMemLocRSMapping.Clear;
  while not Eof(drcF) do
  begin
    ReadLn(drcF, line);

    if  Pos('#define', line) =  1 then
    begin
      lstTemp.DelimitedText := line;
      lstTemp.Delimiter := ' ';
      lstMemLocRSMapping.Add(lstTemp[1] + '=' + lstTemp[2]);
    end;
  end;

  // Close the file for the last time
  CloseFile(drcF);
end;

procedure TLanguageResource.LoadDefaultRSList;
var
  drcF : TextFile;
  line, id, value : string;
  readnow : boolean;
  lstTemp : TStringList;
  iLstTemp : IObjCleaner;
begin
  lstTemp := TStringList.Create;
  iLstTemp := CreateObjCleaner(lstTemp);

  if not FileExists(FDrcFileName) then
    exit;

  AssignFile(drcF, FDrcFileName);
  FileMode := fmOpenRead;
  Reset(drcF);

  FDefaultRSList.Clear;

  readnow := false;
  while not Eof(drcF) do
  begin
    ReadLn(drcF, line);
    if line = 'END' then
      break;

    if readnow then
    begin
      lstTemp.DelimitedText := line;
      lstTemp.Delimiter := ',';
      lstTemp.QuoteChar := '"';
      id := lstTemp[0];
      value := lstTemp[1];

      FDefaultRSList.Add('ResourceString_' + id + '=' + value);
    end;

    if line = 'BEGIN' then
      readnow := true;
  end;

  // Close the file for the last time
  CloseFile(drcF);
end;

procedure TLanguageResource.ApplyLanguageSetting(ctrl : TComponent);
var
  i, j : integer;
begin
  {$IfDef UseLanguage}
  if cSection = '' then
    exit;

  // Form
  if (ctrl is TForm) then
  begin
    ModifyFontsFor(TWinControl(ctrl), ItemDef['LanguageInfo', 'Font', TForm(ctrl).Font.Name]);
    TForm(ctrl).Caption := ItemDef[cSection, 'Caption', TForm(ctrl).Caption];
    TForm(ctrl).Hint := ItemDef[cSection, TForm(ctrl).Name + '.Hint', TForm(ctrl).Hint];
  end

  // Frame
  else if (ctrl is TFrame) then
  begin
    ModifyFontsFor(TWinControl(ctrl), ItemDef['LanguageInfo', 'Font', TFrame(ctrl).Font.Name]);
    TFrame(ctrl).Hint := ItemDef[cSection, TFrame(ctrl).Name + '.Hint', TFrame(ctrl).Hint];
  end

  // Action
  else if (ctrl is TActionManager) then
  begin
    for I := 0 to TActionManager(ctrl).ActionBars.Count - 1 do
      for j := 0 to TActionManager(ctrl).ActionBars[i].Items.Count - 1 do
        TActionManager(ctrl).ActionBars[i].Items[j].Caption := ItemDef[cSection, TActionManager(ctrl).Name + '.ActionBars[' + intToStr(i) + '].Items[' + intToStr(j) + '].Caption', TActionManager(ctrl).ActionBars[i].Items[j].Caption];

  end
  else if (ctrl is TActionToolBarCustom) then
  begin
    TActionToolBarCustom(ctrl).Caption := ItemDef[cSection, TActionToolBarCustom(ctrl).Name + '.Caption', TActionToolBarCustom(ctrl).Caption];
  end

  else if (ctrl is TAction) then
  begin
    TAction(ctrl).Caption := ItemDef[cSection, TAction(ctrl).Name + '.Caption', TAction(ctrl).Caption];
    TAction(ctrl).Hint := ItemDef[cSection, TAction(ctrl).Name + '.Hint', TAction(ctrl).Hint];
  end

  else if (ctrl is TEditCopy) then
  begin
    TEditCopy(ctrl).Caption := ItemDef[cSection, TEditCopy(ctrl).Name + '.Caption', TEditCopy(ctrl).Caption];
    TEditCopy(ctrl).Hint := ItemDef[cSection, TEditCopy(ctrl).Name + '.Hint', TEditCopy(ctrl).Hint];
  end

  else if (ctrl is TEditCut) then
  begin
    TEditCut(ctrl).Caption := ItemDef[cSection, TEditCut(ctrl).Name + '.Caption', TEditCut(ctrl).Caption];
    TEditCut(ctrl).Hint := ItemDef[cSection, TEditCut(ctrl).Name + '.Hint', TEditCut(ctrl).Hint];
  end

  else if (ctrl is TEditPaste) then
  begin
    TEditPaste(ctrl).Caption := ItemDef[cSection, TEditPaste(ctrl).Name + '.Caption', TEditPaste(ctrl).Caption];
    TEditPaste(ctrl).Hint := ItemDef[cSection, TEditPaste(ctrl).Name + '.Hint', TEditPaste(ctrl).Hint];
  end

  else if (ctrl is TEditSelectAll) then
  begin
    TEditSelectAll(ctrl).Caption := ItemDef[cSection, TEditSelectAll(ctrl).Name + '.Caption', TEditSelectAll(ctrl).Caption];
    TEditSelectAll(ctrl).Hint := ItemDef[cSection, TEditSelectAll(ctrl).Name + '.Hint', TEditSelectAll(ctrl).Hint];
  end

  else if (ctrl is TFileSaveAs) then
  begin
    TFileSaveAs(ctrl).Caption := ItemDef[cSection, TFileSaveAs(ctrl).Name + '.Caption', TFileSaveAs(ctrl).Caption];
    TFileSaveAs(ctrl).Hint := ItemDef[cSection, TFileSaveAs(ctrl).Name + '.Hint', TFileSaveAs(ctrl).Hint];
  end

  // Panel
  else if (ctrl is TPanel) then
  begin
   TPanel(ctrl).Caption := ItemDef[cSection, TPanel(ctrl).Name + '.Caption', TPanel(ctrl).Caption];
   TPanel(ctrl).Hint := ItemDef[cSection, TPanel(ctrl).Name + '.Hint', TPanel(ctrl).Hint];
  end

  else if (ctrl is TJvCaptionPanel) then
  begin
   TJvCaptionPanel(ctrl).Caption := ItemDef[cSection, TJvCaptionPanel(ctrl).Name + '.Caption', TJvCaptionPanel(ctrl).Caption];
   TJvCaptionPanel(ctrl).Hint := ItemDef[cSection, TJvCaptionPanel(ctrl).Name + '.Hint', TJvCaptionPanel(ctrl).Hint];
  end

  else if (ctrl is TGridPanel) then
  begin
   TGridPanel(ctrl).Caption := ItemDef[cSection, TGridPanel(ctrl).Name + '.Caption', TGridPanel(ctrl).Caption];
   TGridPanel(ctrl).Hint := ItemDef[cSection, TGridPanel(ctrl).Name + '.Hint', TGridPanel(ctrl).Hint];
  end

  // GroupBox
  else if (ctrl is TGroupBox) then
  begin
    TGroupBox(ctrl).Caption := ItemDef[cSection, TGroupBox(ctrl).Name + '.Caption', TGroupBox(ctrl).Caption];
    TGroupBox(ctrl).Hint := ItemDef[cSection, TGroupBox(ctrl).Name + '.Hint', TGroupBox(ctrl).Hint];
  end

  // TabSheet
  else if (ctrl is TTabSheet) then
  begin
    TTabSheet(ctrl).Caption := ItemDef[cSection, TTabSheet(ctrl).Name + '.Caption', TTabSheet(ctrl).Caption];
    TTabSheet(ctrl).Hint := ItemDef[cSection, TTabSheet(ctrl).Name + '.Hint', TTabSheet(ctrl).Hint];
  end

  // Menu
  else if (ctrl is TMenuItem) then
  begin
    TMenuItem(ctrl).Caption := ItemDef[cSection, TMenuItem(ctrl).Name + '.Caption', TMenuItem(ctrl).Caption];
  end

  // CheckBox
  else if (ctrl is TCheckBox) then
  begin
    TCheckBox(ctrl).Caption := ItemDef[cSection, TCheckBox(ctrl).Name + '.Caption', TCheckBox(ctrl).Caption];
    TCheckBox(ctrl).Hint := ItemDef[cSection, TCheckBox(ctrl).Name + '.Hint', TCheckBox(ctrl).Hint];
  end

  else if (ctrl is TDBCheckBox) then
  begin
    TDBCheckBox(ctrl).Caption := ItemDef[cSection, TDBCheckBox(ctrl).Name + '.Caption', TDBCheckBox(ctrl).Caption];
    TDBCheckBox(ctrl).Hint := ItemDef[cSection, TDBCheckBox(ctrl).Name + '.Hint', TDBCheckBox(ctrl).Hint];
  end

  // CheckListBox
  else if (ctrl is TJvDotNetCheckListBox) then
  begin
    TJvDotNetCheckListBox(ctrl).Hint := ItemDef[cSection, TJvDotNetCheckListBox(ctrl).Name + '.Hint', TJvDotNetCheckListBox(ctrl).Hint];

    for i := 0 to TJvDotNetCheckListBox(ctrl).Items.Count - 1 do
      TJvDotNetCheckListBox(ctrl).Items.Strings[i] := LanguageResource.ItemDef[cSection, TJvDotNetCheckListBox(ctrl).Name + '.Items.Strings[' + intToStr(i) + ']', TJvDotNetCheckListBox(ctrl).Items.Strings[i]];
  end

  // RadioButton
  else if (ctrl is TRadioButton) then
  begin
    TRadioButton(ctrl).Caption := ItemDef[cSection, TRadioButton(ctrl).Name + '.Caption', TRadioButton(ctrl).Caption];
    TRadioButton(ctrl).Hint := ItemDef[cSection, TRadioButton(ctrl).Name + '.Hint', TRadioButton(ctrl).Hint];
  end

  // RadioGroup
  else if (ctrl is TRadioGroup) then
  begin
    TRadioGroup(ctrl).Caption := ItemDef[cSection, TRadioGroup(ctrl).Name + '.Caption', TRadioGroup(ctrl).Caption];
    TRadioGroup(ctrl).Hint := ItemDef[cSection, TRadioGroup(ctrl).Name + '.Hint', TRadioGroup(ctrl).Hint];

    for i := 0 to TRadioGroup(ctrl).Items.Count - 1 do
      TRadioGroup(ctrl).Items.Strings[i] := LanguageResource.ItemDef[cSection, TRadioGroup(ctrl).Name + '.Items.Strings[' + intToStr(i) + ']', TRadioGroup(ctrl).Items.Strings[i]];
  end

  else if (ctrl is TJvRadioGroup) then
  begin
    TJvRadioGroup(ctrl).Caption := ItemDef[cSection, TJvRadioGroup(ctrl).Name + '.Caption', TJvRadioGroup(ctrl).Caption];
    TJvRadioGroup(ctrl).Hint := ItemDef[cSection, TJvRadioGroup(ctrl).Name + '.Hint', TJvRadioGroup(ctrl).Hint];

    for i := 0 to TJvRadioGroup(ctrl).Items.Count - 1 do
      TJvRadioGroup(ctrl).Items.Strings[i] := LanguageResource.ItemDef[cSection, TJvRadioGroup(ctrl).Name + '.Items.Strings[' + intToStr(i) + ']', TJvRadioGroup(ctrl).Items.Strings[i]];
  end

  // Edit
  else if (ctrl is TEdit) then
  begin
    TEdit(ctrl).Text := ItemDef[cSection, TEdit(ctrl).Name + '.Text', TEdit(ctrl).Text];
    TEdit(ctrl).Hint := ItemDef[cSection, TEdit(ctrl).Name + '.Hint', TEdit(ctrl).Hint];
  end

  else if (ctrl is TDBEdit) then
  begin
    TDBEdit(ctrl).Hint := ItemDef[cSection, TDBEdit(ctrl).Name + '.Hint', TDBEdit(ctrl).Hint];
  end

  else if (ctrl is TJvComboEdit) then
  begin
    TJvComboEdit(ctrl).Text := ItemDef[cSection, TJvComboEdit(ctrl).Name + '.Text', TJvComboEdit(ctrl).Text];
    TJvComboEdit(ctrl).Hint := ItemDef[cSection, TJvComboEdit(ctrl).Name + '.Hint', TJvComboEdit(ctrl).Hint];
  end

  else if (ctrl is TJvFilenameEdit) then
  begin
    TJvFilenameEdit(ctrl).Text := ItemDef[cSection, TJvFilenameEdit(ctrl).Name + '.Text', TJvFilenameEdit(ctrl).Text];
    TJvFilenameEdit(ctrl).Hint := ItemDef[cSection, TJvFilenameEdit(ctrl).Name + '.Hint', TJvFilenameEdit(ctrl).Hint];
  end

  // Memo
  else if (ctrl is TMemo) then
  begin
    TMemo(ctrl).Hint := ItemDef[cSection, TMemo(ctrl).Name + '.Hint', TMemo(ctrl).Hint];

    for i := 0 to TMemo(ctrl).Lines.Count - 1 do
      TMemo(ctrl).Lines.Strings[i] := ItemDef[cSection, TMemo(ctrl).Name + '.Lines.Strings[' + intToStr(i) + ']', TMemo(ctrl).Lines.Strings[i]];
  end

  // Label
  else if (ctrl is TLabel) then
  begin
    TLabel(ctrl).Caption := ItemDef[cSection, TLabel(ctrl).Name + '.Caption', TLabel(ctrl).Caption];
    TLabel(ctrl).Hint := ItemDef[cSection, TLabel(ctrl).Name + '.Hint', TLabel(ctrl).Hint];
  end

  else if (ctrl is TJvLabel) then
  begin
    TJvLabel(ctrl).Caption := ItemDef[cSection, TJvLabel(ctrl).Name + '.Caption', TJvLabel(ctrl).Caption];
    TJvLabel(ctrl).Hint := ItemDef[cSection, TJvLabel(ctrl).Name + '.Hint', TJvLabel(ctrl).Hint];
  end

  // Buttons
  else if (ctrl is TJvCaptionButton) then
  begin
    TJvCaptionButton(ctrl).Caption := ItemDef[cSection, TJvCaptionButton(ctrl).Name + '.Caption', TJvCaptionButton(ctrl).Caption];
    TJvCaptionButton(ctrl).Hint := ItemDef[cSection, TJvCaptionButton(ctrl).Name + '.Hint', TJvCaptionButton(ctrl).Hint];
  end

  else if (ctrl is TButton) then
  begin
    TButton(ctrl).Caption := ItemDef[cSection, TButton(ctrl).Name + '.Caption', TButton(ctrl).Caption];
    TButton(ctrl).Hint := ItemDef[cSection, TButton(ctrl).Name + '.Hint', TButton(ctrl).Hint];
  end

  else if (ctrl is TBitBtn) then
  begin
    TBitBtn(ctrl).Caption := ItemDef[cSection, TBitBtn(ctrl).Name + '.Caption', TBitBtn(ctrl).Caption];
    TBitBtn(ctrl).Hint := ItemDef[cSection, TBitBtn(ctrl).Name + '.Hint', TBitBtn(ctrl).Hint];
  end

  else if (ctrl is TSpeedButton) then
  begin
    TSpeedButton(ctrl).Caption := ItemDef[cSection, TSpeedButton(ctrl).Name + '.Caption', TSpeedButton(ctrl).Caption];
    TSpeedButton(ctrl).Hint := ItemDef[cSection, TSpeedButton(ctrl).Name + '.Hint', TSpeedButton(ctrl).Hint];
  end

  else if (ctrl is TJvImgBtn) then
  begin
    TJvImgBtn(ctrl).Caption := ItemDef[cSection, TJvImgBtn(ctrl).Name + '.Caption', TJvImgBtn(ctrl).Caption];
    TJvImgBtn(ctrl).Hint := ItemDef[cSection, TJvImgBtn(ctrl).Name + '.Hint', TJvImgBtn(ctrl).Hint];
  end

  else if (ctrl is TDataSetCancel) then
  begin
    TDataSetCancel(ctrl).Caption := ItemDef[cSection, TDataSetCancel(ctrl).Name + '.Caption', TDataSetCancel(ctrl).Caption];
    TDataSetCancel(ctrl).Hint := ItemDef[cSection, TDataSetCancel(ctrl).Name + '.Hint', TDataSetCancel(ctrl).Hint];
  end

  else if (ctrl is TDataSetDelete) then
  begin
    TDataSetDelete(ctrl).Caption := ItemDef[cSection, TDataSetDelete(ctrl).Name + '.Caption', TDataSetDelete(ctrl).Caption];
    TDataSetDelete(ctrl).Hint := ItemDef[cSection, TDataSetDelete(ctrl).Name + '.Hint', TDataSetDelete(ctrl).Hint];
  end

  else if (ctrl is TDataSetInsert) then
  begin
    TDataSetInsert(ctrl).Caption := ItemDef[cSection, TDataSetInsert(ctrl).Name + '.Caption', TDataSetInsert(ctrl).Caption];
    TDataSetInsert(ctrl).Hint := ItemDef[cSection, TDataSetInsert(ctrl).Name + '.Hint', TDataSetInsert(ctrl).Hint];
  end

  else if (ctrl is TDataSetEdit) then
  begin
    TDataSetEdit(ctrl).Caption := ItemDef[cSection, TDataSetEdit(ctrl).Name + '.Caption', TDataSetEdit(ctrl).Caption];
    TDataSetEdit(ctrl).Hint := ItemDef[cSection, TDataSetEdit(ctrl).Name + '.Hint', TDataSetEdit(ctrl).Hint];
  end

  else if (ctrl is TDataSetPost) then
  begin
    TDataSetPost(ctrl).Caption := ItemDef[cSection, TDataSetPost(ctrl).Name + '.Caption', TDataSetPost(ctrl).Caption];
    TDataSetPost(ctrl).Hint := ItemDef[cSection, TDataSetPost(ctrl).Name + '.Hint', TDataSetPost(ctrl).Hint];
  end

  else if (ctrl is TDataSetRefresh) then
  begin
    TDataSetRefresh(ctrl).Caption := ItemDef[cSection, TDataSetRefresh(ctrl).Name + '.Caption', TDataSetRefresh(ctrl).Caption];
    TDataSetRefresh(ctrl).Hint := ItemDef[cSection, TDataSetRefresh(ctrl).Name + '.Hint', TDataSetRefresh(ctrl).Hint];
  end

  else if (ctrl is TDataSetFirst) then
  begin
    TDataSetFirst(ctrl).Caption := ItemDef[cSection, TDataSetFirst(ctrl).Name + '.Caption', TDataSetFirst(ctrl).Caption];
    TDataSetFirst(ctrl).Hint := ItemDef[cSection, TDataSetFirst(ctrl).Name + '.Hint', TDataSetFirst(ctrl).Hint];
  end

  else if (ctrl is TDataSetLast) then
  begin
    TDataSetLast(ctrl).Caption := ItemDef[cSection, TDataSetLast(ctrl).Name + '.Caption', TDataSetLast(ctrl).Caption];
    TDataSetLast(ctrl).Hint := ItemDef[cSection, TDataSetLast(ctrl).Name + '.Hint', TDataSetLast(ctrl).Hint];
  end

  else if (ctrl is TDataSetNext) then
  begin
    TDataSetNext(ctrl).Caption := ItemDef[cSection, TDataSetNext(ctrl).Name + '.Caption', TDataSetNext(ctrl).Caption];
    TDataSetNext(ctrl).Hint := ItemDef[cSection, TDataSetNext(ctrl).Name + '.Hint', TDataSetNext(ctrl).Hint];
  end

  else if (ctrl is TDataSetPrior) then
  begin
    TDataSetPrior(ctrl).Caption := ItemDef[cSection, TDataSetPrior(ctrl).Name + '.Caption', TDataSetPrior(ctrl).Caption];
    TDataSetPrior(ctrl).Hint := ItemDef[cSection, TDataSetPrior(ctrl).Name + '.Hint', TDataSetPrior(ctrl).Hint];
  end

  else if (ctrl is TEditCopy) then
  begin
    TEditCopy(ctrl).Caption := ItemDef[cSection, TEditCopy(ctrl).Name + '.Caption', TEditCopy(ctrl).Caption];
    TEditCopy(ctrl).Hint := ItemDef[cSection, TEditCopy(ctrl).Name + '.Hint', TEditCopy(ctrl).Hint];
  end

  else if (ctrl is TEditCut) then
  begin
    TEditCut(ctrl).Caption := ItemDef[cSection, TEditCut(ctrl).Name + '.Caption', TEditCut(ctrl).Caption];
    TEditCut(ctrl).Hint := ItemDef[cSection, TEditCut(ctrl).Name + '.Hint', TEditCut(ctrl).Hint];
  end

  else if (ctrl is TEditPaste) then
  begin
    TEditPaste(ctrl).Caption := ItemDef[cSection, TEditPaste(ctrl).Name + '.Caption', TEditPaste(ctrl).Caption];
    TEditPaste(ctrl).Hint := ItemDef[cSection, TEditPaste(ctrl).Name + '.Hint', TEditPaste(ctrl).Hint];
  end

  else if (ctrl is TEditSelectAll) then
  begin
    TEditSelectAll(ctrl).Caption := ItemDef[cSection, TEditSelectAll(ctrl).Name + '.Caption', TEditSelectAll(ctrl).Caption];
    TEditSelectAll(ctrl).Hint := ItemDef[cSection, TEditSelectAll(ctrl).Name + '.Hint', TEditSelectAll(ctrl).Hint];
  end

  else if (ctrl is TFileSaveAs) then
  begin
    TFileSaveAs(ctrl).Caption := ItemDef[cSection, TFileSaveAs(ctrl).Name + '.Caption', TFileSaveAs(ctrl).Caption];
    TFileSaveAs(ctrl).Hint := ItemDef[cSection, TFileSaveAs(ctrl).Name + '.Hint', TFileSaveAs(ctrl).Hint];
  end

  // RollOut
  else if (ctrl is TJvRollOut) then
  begin
    TJvRollOut(ctrl).Caption := ItemDef[cSection, TJvRollOut(ctrl).Name + '.Caption', TJvRollOut(ctrl).Caption];
    TJvRollOut(ctrl).Hint := ItemDef[cSection, TJvRollOut(ctrl).Name + '.Hint', TJvRollOut(ctrl).Hint];
  end

  // ComboBox
  else if (ctrl is TComboBox) then
  begin
    TComboBox(ctrl).Hint := ItemDef[cSection, TComboBox(ctrl).Name + '.Hint', TComboBox(ctrl).Hint];

    for i := 0 to TComboBox(ctrl).Items.Count - 1 do
      TComboBox(ctrl).Items.Strings[i] := ItemDef[cSection, TComboBox(ctrl).Name + '.Items.Strings[' + intToStr(i) + ']', TComboBox(ctrl).Items.Strings[i]];
  end

  else if (ctrl is TJvDBComboBox) then
  begin
    TJvDBComboBox(ctrl).Hint := ItemDef[cSection, TJvDBComboBox(ctrl).Name + '.Hint', TJvDBComboBox(ctrl).Hint];

    for i := 0 to TJvDBComboBox(ctrl).Items.Count - 1 do
      TJvDBComboBox(ctrl).Items.Strings[i] := ItemDef[cSection, TJvDBComboBox(ctrl).Name + '.Items.Strings[' + intToStr(i) + ']', TJvDBComboBox(ctrl).Items.Strings[i]];
  end

  // ClientDataSet
  else if (ctrl is TClientDataSet) then
  begin
    for i := 0 to TClientDataSet(ctrl).Fields.Count - 1 do
      TClientDataSet(ctrl).Fields[i].DisplayLabel := LanguageResource.ItemDef[cSection, TClientDataSet(ctrl).Name + '.Fields[' + intToStr(i) + '].DisplayLabel', TClientDataSet(ctrl).Fields[i].DisplayLabel];
  end

  // StatusBar
  else if (ctrl is TStatusBar) then
  begin
    TStatusBar(ctrl).Hint := ItemDef[cSection, TStatusBar(ctrl).Name + '.Hint', TStatusBar(ctrl).Hint];

    for i := 0 to TStatusBar(ctrl).Panels.Count - 1 do
      TStatusBar(ctrl).Panels[i].Text := LanguageResource.ItemDef[cSection, TStatusBar(ctrl).Name + '.Panels[' + intToStr(i) + '].Text', TStatusBar(ctrl).Panels[i].Text];
  end

  else if (ctrl is TJvStatusBar) then
  begin
    TJvStatusBar(ctrl).Hint := ItemDef[cSection, TJvStatusBar(ctrl).Name + '.Hint', TJvStatusBar(ctrl).Hint];

    for i := 0 to TJvStatusBar(ctrl).Panels.Count - 1 do
      TJvStatusBar(ctrl).Panels[i].Text := LanguageResource.ItemDef[cSection, TJvStatusBar(ctrl).Name + '.Panels[' + intToStr(i) + '].Text', TJvStatusBar(ctrl).Panels[i].Text];
  end

  // Grid
  else if (ctrl is TDBGrid) then
  begin
    TDBGrid(ctrl).Hint := ItemDef[cSection, TDBGrid(ctrl).Name + '.Hint', TDBGrid(ctrl).Hint];

    for i := 0 to TDBGrid(ctrl).Columns.Count - 1 do
      TDBGrid(ctrl).Columns[i].Title.Caption := LanguageResource.ItemDef[cSection, TDBGrid(ctrl).Name + '.Columns[' + intToStr(i) + '].Title.Caption', TDBGrid(ctrl).Columns[i].Title.Caption];
  end

  else if (ctrl is TJvDBGrid) then
  begin
    TJvDBGrid(ctrl).Hint := ItemDef[cSection, TJvDBGrid(ctrl).Name + '.Hint', TJvDBGrid(ctrl).Hint];

    for i := 0 to TJvDBGrid(ctrl).Columns.Count - 1 do
      TJvDBGrid(ctrl).Columns[i].Title.Caption := LanguageResource.ItemDef[cSection, TJvDBGrid(ctrl).Name + '.Columns[' + intToStr(i) + '].Title.Caption', TJvDBGrid(ctrl).Columns[i].Title.Caption];
  end

  else if (ctrl is TGridView) then
  begin
    TGridView(ctrl).Hint := ItemDef[cSection, TGridView(ctrl).Name + '.Hint', TGridView(ctrl).Hint];
  end

  else if (ctrl is TcxGridDBTableView) then
  begin
    for i := 0 to TcxGridDBTableView(ctrl).ColumnCount - 1 do
      TcxGridDBTableView(ctrl).Columns[i].Caption := LanguageResource.ItemDef[cSection, TcxGridDBTableView(ctrl).Name + '.Columns[' + intToStr(i) + '].Caption', TcxGridDBTableView(ctrl).Columns[i].Caption];
  end

  // VirtualStringTree
  else if (ctrl is TVirtualStringTree) then
  begin
    TVirtualStringTree(ctrl).Hint := ItemDef[cSection, TVirtualStringTree(ctrl).Name + '.Hint', TVirtualStringTree(ctrl).Hint];

    for i := 0 to TVirtualStringTree(ctrl).Header.Columns.Count - 1 do
      TVirtualStringTree(ctrl).Header.Columns[i].Text := LanguageResource.ItemDef[cSection, TcxGridDBTableView(ctrl).Name + '.Header.Columns[' + intToStr(i) + '].Text', TVirtualStringTree(ctrl).Header.Columns[i].Text];
  end;

  if (ctrl is TForm) or (ctrl is TFrame) then
    for i := 0 to ctrl.ComponentCount - 1 do
      ApplyLanguageSetting(ctrl.Components[i]);

  {$Endif}
end;

procedure TLanguageResource.ApplyLanguageForAllChildFramesInForm(frm: TForm);
  procedure ApplyLanguageForAllChildFrames(ctrl:TControl);
    var
       i: Integer;
    begin
      if ctrl is TWinControl then
      begin
        if supports(ctrl, ILanguageForm) then
          (ctrl as ILanguageForm).ApplyLanguageSettings;

        for i := 0 to TWincontrol(ctrl).controlcount - 1 do
          ApplyLanguageForAllChildFrames(TWincontrol(ctrl).Controls[i]);
      end;

  end;
var
  j :integer;
begin
  for j := 0 to frm.ControlCount - 1 do
      if frm.Controls[j] is TWinControl then
        ApplyLanguageForAllChildFrames(frm.Controls[j]);
end;



constructor TLanguageResource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetLength(RSArray, 64 * 1024 + 1);
  lstMemLocRSMapping := TStringList.Create;
  lstItems := TStringList.Create;
  lstItems.CaseSensitive := False;

  lstSections := TStringList.Create;
  lstSections.CaseSensitive := False;

  FDefaultRSList := TStringList.Create;

  FLanguageFolderPath := ExtractFilePath(Paramstr(0)) + 'Language\';
  FAppName := 'PMORA';
  FAppVersion := C_MinAppVersion;

  FCurrReadingDir := 'LeftToRight';
  FDrcFileName := ChangeFileExt(Paramstr(0), '.drc');
  FIsActive := true;

  LoadRsIdMapping;
  LoadDefaultRSList;
end;

destructor TLanguageResource.Destroy;
begin
  lstMemLocRSMapping.Free;
  lstItems.Free;
  lstSections.Free;
  FDefaultRSList.Free;
  inherited;
end;

function TLanguageResource.GetLangFileList : TStringList;
var
  FileList, lSec, lSecValue :TStringList;
  IniFile : TMemIniFile;
  SR: TSearchRec;
  i : integer;
  s: string;
begin
  {$IfDef UseLanguage}

  FileList := TStringList.Create;
  FileList.add('English-English-Default=');
  if FindFirst(LanguageResource.LanguageFolderPath + '*.lang', faAnyFile, SR) = 0 then
  repeat
    IniFile := TMemIniFile.Create(LanguageResource.LanguageFolderPath + SR.Name, TEncoding.UTF8);

    lSec := TStringList.Create;

    IniFile.ReadSections(lSec);
    for i := 0 to lSec.Count - 1 do
    begin
      lSecValue := TStringList.Create;
      IniFile.ReadSectionValues(lSec[i], lSecValue);

      if lSec[i] = 'LanguageInfo' then
      begin
        s := lSecValue.Values['Language'] + '-' + lSecValue.Values['LanguageLocal'];

        if lSecValue.Values['UserDetails'] <> '' then
          s := s + '-' + lSecValue.Values['UserDetails'];

        FileList.add(s + '=' + SR.Name);
        lSecValue.free;
        break;
      end;

      lSecValue.free;
    end;
    lSec.Free;
    IniFile.Free;
  until FindNext(SR) <> 0;
  FindClose(SR);

  Result := FileList;
  {$EndIf}
end;

function TLanguageResource.GetItem(sSection, sIdent: string): string;
begin
  Result := lstItems.Values[sSection + '_' + sIdent];
end;

function TLanguageResource.GetItemDefault(sSection, sIdent, sDefault: string): string;
begin
 Result := lstItems.Values[sSection + '_' + sIdent];
 if Result = '' then
  Result := sDefault;
end;

function TLanguageResource.GetRSItemDefault(sSection, sIdent, sDefault: string): string;
begin
  Result := lstItems.Values[sSection + '_' + sIdent];
  if Result = '' then
    Result := sDefault;

  Result := StringReplace(Result, '\r', #13, [rfReplaceAll]);
  Result := StringReplace(Result, '\n', #10, [rfReplaceAll]);
end;

procedure TLanguageResource.ReadFromFile(const sFileName : String);
{$IfDef UseLanguage}
var
  IniFile : TMemIniFile; iIniFile : IObjCleaner;
  i, j, index : Integer;
  sIndex, value: string;
  lst : TStringList; ilst : IObjCleaner;
{$EndIf}
begin
  {$IfDef UseLanguage}
  lstItems.Clear;
  lstSections.Clear;
  SetLength(RSArray, 64 * 1024 + 1);

  if not FileExists(sFileName) then
  begin
    FIsActive := false;
    exit;
  end
  else
    FIsActive := true;

  IniFile := TMemIniFile.Create(sFileName, TEncoding.UTF8); iIniFile := CreateObjCleaner(IniFile);
  lst := TStringList.Create; ilst := CreateObjCleaner(lst);

  IniFile.ReadSections(lstSections);

  for i := 0 to lstSections.Count - 1 do
  begin
    lst.Clear;
    IniFile.ReadSectionValues(lstSections[i], lst);
    for j := 0 to lst.Count - 1 do
    begin
      if lstSections[i] = 'ResourceString' then
      begin
        sIndex := lstMemLocRSMapping.values[lst.Names[j]];
        if sIndex <> '' then
        begin
          index := strToInt(sIndex);
          value := lst.ValueFromIndex[j];
          value := StringReplace(value, '\r', #13, [rfReplaceAll]);
          value := StringReplace(value, '\n', #10, [rfReplaceAll]);
          RSArray[index] := value;
        end;
      end;

      lstItems.Add(lstSections[i] + '_' + lst.Names[j] + '=' + lst.ValueFromIndex[j]);
    end;

  end;


  {$EndIf}

end;

procedure TLanguageResource.ReadFromResDefLanguage;
{$IfDef UseLanguage}
var
  IniFile : TMemIniFile; iIniFile : IObjCleaner;
  i, j : Integer;
  lst : TStringList; ilst : IObjCleaner;
  tmpDefLang : TextFile;
  tmpDefLangFilePath : string;
{$EndIf}
begin
  {$IfDef UseLanguage}
  lstItems.Clear;
  lstSections.Clear;

  tmpDefLangFilePath := GetEnvironmentVariable('TEMP') + 'tmpDefault.lang';
  if FileExists(tmpDefLangFilePath) then
    DeleteFile(tmpDefLangFilePath);

  AssignFile(tmpDefLang, tmpDefLangFilePath);
  ReWrite(tmpDefLang);
  Write(tmpDefLang, DataModuleBase.GetLanguageFile(DefaultLangFile));
  CloseFile(tmpDefLang);

  IniFile := TMemIniFile.Create(tmpDefLangFilePath);
  iIniFile := CreateObjCleaner(IniFile);
  lst := TStringList.Create; ilst := CreateObjCleaner(lst);

  IniFile.ReadSections(lstSections);
  for i := 0 to lstSections.Count - 1 do
  begin
    lst.Clear;
    IniFile.ReadSectionValues(lstSections[i], lst);
    for j := 0 to lst.Count - 1 do
    begin
      lstItems.Add(lstSections[i] + '_' + lst.Names[j] + '=' + lst.ValueFromIndex[j]);
    end;

  end;

  DeleteFile(tmpDefLangFilePath);
  {$EndIf}
end;

procedure TLanguageResource.ReadLabelAndHint(Section, LabelIdent,
  HintIdent: string; ComponentArray: array of TComponent);
{$IfDef UseLanguage}
var
  sLabel, sHint : String;
  i : Integer;
  aComponent : TComponent;
{$EndIf}
begin
  {$IfDef UseLanguage}
  sLabel := Item[Section, LabelIdent];
  sHint := Item[Section, HintIdent];

  for i := low(ComponentArray) to high(ComponentArray) do
  begin
    aComponent := ComponentArray[i];

    if aComponent is TAction then
    begin
      if sLabel <> '' then
        TAction(aComponent).Caption := sLabel;

      if sHint <> '' then
        TAction(aComponent).Hint := sHint;
    end

    else if aComponent is TButton then
    begin
      if sLabel <> '' then
        TButton(aComponent).Caption := sLabel;

      if sHint <> '' then
        TButton(aComponent).Hint := sHint;
    end

    else if aComponent is TCheckBox then
    begin
      if sLabel <> '' then
        TCheckBox(aComponent).Caption := sLabel;

      if sHint <> '' then
        TCheckBox(aComponent).Hint := sHint;
    end

    else if aComponent is TCustomLabel then
    begin
      if sLabel <> '' then
        TCustomLabel(aComponent).Caption := sLabel;

      if sHint <> '' then
        TCustomLabel(aComponent).Hint := sHint;
    end

    else if aComponent is TBitBtn then
    begin
      if sLabel <> '' then
        TBitBtn(aComponent).Caption := sLabel;

      if sHint <> '' then
        TBitBtn(aComponent).Hint := sHint;
    end

    else if aComponent is TMenuItem then
    begin
      if sLabel <> '' then
        TMenuItem(aComponent).Caption := sLabel;

      if sHint <> '' then
        TMenuItem(aComponent).Hint := sHint;
    end;
  end;
  {$EndIf}
end;

procedure TLanguageResource.SaveToFile(const sFileName : string; lst : TStringList);
var
  IniFile : TMemIniFile; iIniFile : IObjCleaner;
  i : Integer;
  TempSL : TStringList;
  tempLangFileName, langFolder, tmpStr: string;
begin
  {$IfDef UseLanguage}
  langFolder := ExtractFilePath(sFileName);
  tempLangFileName := langFolder + 'TempLangFile';
  if FileExists(tempLangFileName) then
    DeleteFile(tempLangFileName);

  IniFile := TMemIniFile.Create(tempLangFileName, TEncoding.UTF8);
  iIniFile := CreateObjCleaner(IniFile);

  if lst = nil then
    exit;

  for i := 0 to lst.Count - 1 do
  begin
    TempSL := TStringList.Create;
    TempSL.Delimiter := '_';
    tmpStr := lst.Names[i];
    TempSL.DelimitedText := tmpStr;
    IniFile.WriteString(TempSL[0], Copy(tmpStr, length(TempSL[0]) + 2, length(tmpStr) - (length(TempSL[0]) + 1)), lst.ValueFromIndex[i]);
    IniFile.UpdateFile;
    TempSL.Free;
  end;

  if FileExists(sFileName) then
    DeleteFile(sFileName);

  RenameFile(tempLangFileName, sFileName);
  {$EndIf}
end;

procedure TLanguageResource.SetAppName(const Value: string);
begin
  FAppName := Value;
end;

procedure TLanguageResource.SetAppVersion(const Value: string);
begin
  FAppVersion := Value;
end;

procedure TLanguageResource.SetItem(sSection, sIdent: string; const Value: string);
begin
  lstItems.Values[sSection + '_' + sIdent] := Value;
end;

procedure TLanguageResource.SetLanguageFolderPath(const Value: string);
begin
  FLanguageFolderPath := Value;
end;

function TLanguageResource.GetLanguageFolderPath: string;
begin
  Result := FLanguageFolderPath;
end;

function TLanguageResource.GetAppName: string;
begin
  Result := FAppName;
end;

function TLanguageResource.GetAppVersion: string;
begin
  Result := FAppVersion;
end;

function TLanguageResource.GetItemList: TStringList;
begin
  Result := lstItems;
end;

function TLanguageResource.IsValidLangFile(fileName : string) : boolean;
var
  TempLR : TLanguageResource;
  iTempLR :IObjCleaner;
begin
  Result:= true;

  TempLR := TLanguageResource.Create(nil); iTempLR := CreateObjCleaner(TempLR);
  TempLR.ReadFromFile(fileName);

  if (TempLR.ItemDef['AppInfo', 'ApplicationName', ''] = '') or
     (TempLR.ItemDef['AppInfo', 'AppVersion', ''] = '') or
     (TempLR.ItemDef['LanguageInfo', 'Language', ''] = '') or
     (TempLR.ItemDef['LanguageInfo', 'LanguageLocal', ''] = '') or
     (TempLR.ItemDef['LanguageInfo', 'ReadingDirection', ''] = '') or
     (TempLR.ItemDef['LanguageInfo', 'Font', ''] = '') then
    Result:= false;

end;


procedure TLanguageResource.ModifyFontsFor(ctrl: TWinControl; fontName : string);
  procedure ModifyFont(ctrl: TControl; fontName : string);
  var
    f: TFont;
  begin
    if IsPublishedProp(ctrl, 'Parentfont')
      and (GetOrdProp(ctrl, 'Parentfont') = Ord(false))
      and IsPublishedProp(ctrl, 'font')
    then begin
      f := TFont(GetObjectProp(ctrl, 'font', TFont));
      f.Name := fontName;
    end;
  end;
var
  i: Integer;
begin
  ModifyFont(ctrl, fontName);
  for i := 0 to ctrl.controlcount - 1 do
    if ctrl.controls[i] is Twincontrol then
      ModifyFontsfor(TWincontrol(ctrl.controls[i]), fontName)
    else
      Modifyfont(ctrl.controls[i], fontName);
end;

procedure TLanguageResource.ModifyReadingDir(Control:TComponent; readingDir: string);
  procedure ModifyReadingDirFor(Control:TComponent; bidiMode: TBiDiMode);
  var
    i:Integer;
  begin

    if not (Control is TWinControl) then
      exit;

    TWinControl(Control).BiDiMode := bidiMode;

    for i := 0 to Control.ComponentCount - 1 do
    begin
      ModifyReadingDirFor(Control.Components[i], bidiMode);

      if (Control.Components[i] is TWinControl)  then
        TWinControl(Control.Components[i]).Left :=
          TWinControl(Control).Width -
          (TWinControl(Control.Components[i]).Left + TWinControl(Control.Components[i]).Width);
    end;

  end;
begin
  {$IfDef UseLanguage}

  if FCurrReadingDir <> readingDir then
  begin
    if readingDir = 'LeftToRight' then
      ModifyReadingDirFor(Control, bdLeftToRight)
    else
      ModifyReadingDirFor(Control, bdRightToLeft);
  end;

  {$EndIf}
end;

function TLanguageResource.MsgId(const Section, MsgIdent: string): string;
begin
  Result := Section + '_' + MsgIdent;
end;

initialization
  LanguageResource := TLanguageResource.Create(nil);

  {$IfDef UseLanguage}
  // replace Borlands LoadResString with gettext enabled version:
  HookLoadResString:=THook.Create (@system.LoadResString, @LoadResString);
  HookLoadStr:=THook.Create (@sysutils.LoadStr, @SysUtilsLoadStr);
  HookFmtLoadStr:=THook.Create (@sysutils.FmtLoadStr, @SysUtilsFmtLoadStr);

  HookLoadResString.Enable;
  HookLoadStr.Enable;
  HookFmtLoadStr.Enable;
  {$EndIf}


finalization
  {$IfDef UseLanguage}
  FreeAndNil (HookFmtLoadStr);
  FreeAndNil (HookLoadStr);
  FreeAndNil (HookLoadResString);
  {$EndIf}
  LanguageResource.Free;
end.
