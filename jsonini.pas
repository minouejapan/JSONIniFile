(*
  JSONデータをTIniFilesと同様に気軽に扱うためのラッパークラス
  Lazarus 3.6/FPC3.2.2以降

  1.00 2025/07/13 初版

*)
unit JSONIni;

{$mode delphi}{$H+}{$M+}

interface

uses
  Classes, SysUtils, Forms, dialogs, Windows, ShlObj, ActiveX, fpJSON, lazUTF8;

type  TFolderKind = (APP_FOLDER, USER_FOLDER);
      TJSONIni = class
      private
        FIniFolder,
        FFileName,
        FJText: string;
        FJData: TJSONData;
        FJObj: TJSONObject;
        procedure JSONInit;
        procedure SetIniFolder(Value: string);
        procedure SetFileName(Value: string);
      public
        constructor Create; overload;
   		  constructor Create(Value: string); overload;
   		  Destructor Destroy;override;
        procedure Update;

    		function ReadBool(const Section, Item: string; Default: boolean): boolean;
    		procedure WriteBool(const Section, Item: string; Value: boolean);
    		function ReadInt(const Section, Item: string; Default: integer): integer;
    		procedure WriteInt(const Section, Item: string; Value: integer);
    		function ReadStr(const Section, Item, Default: string): string;
    		procedure WriteStr(const Section, Item, Value: string);

    		function ReadFloat(const Section, Item: string; Default: Extended): Extended;
    		procedure WriteFloat(const Section, Item:string; Value: Extended);

    		procedure ReadWinPos(const Section, Item: string; var Value: TForm);
    		procedure WriteWinPos(const Section, Item: string; Value: TForm);
    		procedure ReadWinSize(const Section, Item: string; var Value: TForm);
    		procedure WriteWinSize(const Section, Item: string; Value: TForm);
    		procedure ReadWin(const Section, Item: string; var Value: TForm);
    		procedure WriteWin(const Section, Item: string; Value: TForm);
      published
        property IniFolder: string read FIniFolder;
    		property FileName: string read FFileName;
      end;

implementation

// Windowsの特殊フォルダを取得する
function GetSpecialFolder(const iFolder: DWORD): String;
var
  IDL: PItemIDList;
  r2: bool;
  sf: string;
begin
  sf := '';
  SHGetSpecialFolderLocation(Application.Handle, iFolder, IDL);
  try
    sf := StringOfChar(#0, MAX_PATH);
    r2 := SHGetPathFromIDList(IDL, PChar(sf));
    if (IDL <> nil) and r2 then
    sf := Trim(sf);
  finally
    CoTaskMemFree(IDL);
  end;
  GetSpecialFolder := sf;
end;

// JSONObjectを準備する
procedure TJSONIni.JSONInit;
var
  js: TStringList;
  fld, fnm, fpath: string;
  attr: integer;
begin
  // JSONファイル名を指定せずにCreateした場合はファイル名を準備する
  // 実行ファイルがあるフォルダが書き込み可能であればそのフォルダを
  // 書き込み不可(C:\Program Files等)であればAppData\Roamingフォルダ
  // を使用する
  if FFileName = '' then
  begin
    fld := ExtractFileDir(Application.ExeName);
    fnm := ChangeFileExt(ExtractFileName(Application.ExeName), '.json');
    attr := GetFileAttributes(PChar(fld));
    if (attr and 1) = 1 then  // フォルダがReadOnlyであればAPPDATAフォルダを取得する
      fld := GetSpecialFolder(CSIDL_APPDATA);
    if fld[UTF8Length(fld)] <> '\' then
      fld := fld + '\';
    fpath := fld + fnm;
  // JSONファイルを指定してCreateした
  end else
    fpath := FFileName;
  // JSONファイルが存在していれば読み込む
  if FileExists(fpath) then
  begin
    js := TStringList.Create;
    try
      js.LoadFromFile(fpath, TEncoding.UTF8);
      FJText := js.Text;
    finally
      js.Free;
    end;
  // JSONファイルがなければルートデータを準備する
  end else begin
    FJText := '{'#13#10
           +  '    "JSONIni" : "' + fnm + '"'#13#10
           +  '}'#13#10;
  end;
  // JSONObject/JSONDataを準備する
  FJObj := TJSONObject(GetJSON(FJText));//TJSONObject.Create;
  FJData := FJObj;
  SetIniFolder(fld);
  SetFileName(fpath);
end;

// JSONファイルの指定がない場合は実行ファイル名でJSONファイルを準備する
constructor TJSONIni.Create; overload;
begin
  inherited Create;

  JSONInit;
end;

// JSONファイルを指定した場合
constructor TJSONIni.Create(Value: string); overload;
begin
  inherited Create;

  if FileExists(Value) then
  begin
    SetIniFolder(ExtractFileDir(Value));
    SetFileName(Value);
    JSONInit;
  end else begin
    SetIniFolder('');
    SetFileName('');
  end;

  JSONInit;
end;

// 終了処理
destructor TJSONIni.Destroy;
begin
  Update;

  // FJObjはFJDataに紐づけられているのでFJDataを開放すれば
  // 動的生成したFJObj.Objectsと共に自動的に開放される模様
  if Assigned(FJData) then
    FJData.Free;

  inherited Destroy;
end;

procedure TJSONIni.SetIniFolder(Value: string);
begin
  FIniFolder := Value;
end;

procedure TJSONIni.SetFileName(Value: string);
begin
  FFileName := Value;
end;

// 現在のFJDataをファイルに書き出す
procedure TJSONIni.Update;
var
  js: TStringList;
begin
  js := TStringList.Create;
  try
    // FormatJSONでインデント整形されたテキストを取得する
    js.Text := FJData.FormatJSON;
    js.SaveToFile(FFileName, TEncoding.UTF8);
  finally
    js.Free;
  end;
end;

function TJSONIni.ReadBool(const Section, Item: string; Default: boolean): boolean;
var
  v, r: string;
begin
  // True/Falseを文字列で扱う
  if Default then v := 'TRUE' else v := 'FALSE';
  // SectonがなければSection名のノードを追加する
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  try
    // Itemが存在していればその値を取得する
    r := FJObj.Objects[Section].Strings[Item];
    if r = 'TRUE' then Result := True else Result := False;
  except
    // Itemがない場合はエラーとなるため新たにItemと値を追加しておく
    FJObj.Objects[Section].Add(Item, v);
    Result := Default;
  end;
end;

procedure TJSONIni.WriteBool(const Section, Item: string; Value: boolean);
var
  v: string;
begin
  // True/Falseを文字列で扱う
  if Value then v := 'TRUE' else v := 'FALSE';
  // SectonがなければSection名のノードを追加する
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  // ItemがなければSectionに追加する
  if FJObj.Objects[Section].Find(Item) = nil then
    FJObj.Objects[Section].Add(Item, v)
  // すでに存在している場合は値を書き換える
  else
    FJObj.Objects[Section].Strings[Item] := v;
end;

function TJSONIni.ReadInt(const Section, Item: string; Default: integer): integer;
begin
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  try
    Result := FJObj.Objects[Section].Integers[Item];
  except
    FJObj.Objects[Section].Add(Item, Default);
    Result := Default;
  end;
end;

procedure TJSONIni.WriteInt(const Section, Item: string; Value: integer);
begin
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  if FJObj.Objects[Section].Find(Item) = nil then
    FJObj.Objects[Section].Add(Item, Value)
  else
    FJObj.Objects[Section].Integers[Item] := Value;
end;

function TJSONIni.ReadStr(const Section, Item, Default: string): string;
begin
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  try
    Result := FJObj.Objects[Section].Strings[Item];
  except
    FJObj.Objects[Section].Add(Item, Default);
    Result := Default;
  end;
end;

procedure TJSONIni.WriteStr(const Section, Item, Value: string);
begin
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  if FJObj.Objects[Section].Find(Item) = nil then
    FJObj.Objects[Section].Add(Item, Value)
  else
    FJObj.Objects[Section].Strings[Item] := Value;
end;

function TJSONIni.ReadFloat(const Section, Item: string; Default: Extended): Extended;
begin
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  try
    Result := FJObj.Objects[Section].Floats[Item];
  except
    FJObj.Objects[Section].Add(Item, Default);
    Result := Default;
  end;
end;

procedure TJSONIni.WriteFloat(const Section, Item: string; Value: Extended);
begin
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  if FJObj.Objects[Section].Find(Item) = nil then
    FJObj.Objects[Section].Add(Item, Value)
  else
    FJObj.Objects[Section].Floats[Item] := Value;
end;

procedure TJSONIni.ReadWinPos(const Section, Item: string; var Value: TForm);
begin
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  try
    Value.Left := FJObj.Objects[Section].Integers[Item + '.Left'];
    Value.Top  := FJObj.Objects[Section].Integers[Item + '.Top'];
  except
    FJObj.Objects[Section].Add(Item + '.Left', Value.Left);
    FJObj.Objects[Section].Add(Item + '.Top',  Value.Top);
  end;
end;

procedure TJSONIni.WriteWinPos(const Section, Item: string; Value: TForm);
begin
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  if FJObj.Objects[Section].Find(Item + '.Left') = nil then
  begin
    FJObj.Objects[Section].Add(Item + '.Left', Value.Left);
    FJObj.Objects[Section].Add(Item + '.Top',  Value.Top);
  end else begin
    FJObj.Objects[Section].Integers[Item + '.Left'] := Value.Left;
    FJObj.Objects[Section].Integers[Item + '.Top']  := Value.Top;
  end;
end;

procedure TJSONIni.ReadWinSize(const Section, Item: string; var Value: TForm);
begin
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  try
    Value.Width  := FJObj.Objects[Section].Integers[Item + '.Width'];
    Value.Height := FJObj.Objects[Section].Integers[Item + '.Height'];
  except
    FJObj.Objects[Section].Add(Item + '.Width',  Value.Width);
    FJObj.Objects[Section].Add(Item + '.Height', Value.Height);
  end;
end;

procedure TJSONIni.WriteWinSize(const Section, Item: string; Value: TForm);
begin
  if FJObj.Find(Section) = nil then
    FJObj.Add(Section, TJSONObject.Create);
  if FJObj.Objects[Section].Find(Item + '.Width') = nil then
  begin
    FJObj.Objects[Section].Add(Item + '.Width',  Value.Width);
    FJObj.Objects[Section].Add(Item + '.Height', Value.Height);
  end else begin
    FJObj.Objects[Section].Integers[Item + '.Width']  := Value.Width;
    FJObj.Objects[Section].Integers[Item + '.Height'] := Value.Height;
  end;
end;

procedure TJSONIni.ReadWin(const Section, Item: string; var Value: TForm);
begin
  ReadWinPos(Section, Item, TForm(Value));
  ReadWinSize(Section, Item, TForm(Value));
end;

procedure TJSONIni.WriteWin(const Section, Item: string; Value: TForm);
begin
  WriteWinPos(Section, Item, Value);
  WriteWinSize(Section, Item, Value);
end;


end.

