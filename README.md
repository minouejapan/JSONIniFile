# JSONIniFile for Lazarus

TIniFilesと同じ使い勝手でデータをJSONファイルに保存・読み込みするためのクラスです。


### 使用方法

TestSampleプロジェクトを参考にしてください。

#### 使用出来る手続き・関数

　ReadBool(Section, Item, Default)　　論理値(True/False)を読み込む

　WriteBool(Section, Item, Value)　　論理値(True/False)を保存する

　ReadInt(Section, Item, Default)　　整数値を読み込む

　WriteInt(Section, Item, Value)   製図内を保存する

　ReadStr(Section, Item, Default)　　文字列を読み込む

　WriteStr(Section, Item, Value)　　文字列を保存する

　ReadFloat(Section, Item, Default)　　実数値を読み込む

　WriteFloat(Section, Item, Value)　　実数値を保存する

　ReadWinPos(Section, Item, TForm)　　 ウィンドウの位置をTFormに読み込む

　WriteWinPos(Section, Item, TForm)　　TFormの位置を保存する

　ReadWinSize(Section, Item, TForm)　　ウィンドウのサイズをTFormに読み込む

　WriteWinSize(Section, Item, TForm)　　TFormのサイズを保存する

　ReadWin(Section, Item, TForm)　　ウィンドウの位置とサイズをTFormに読み込む

　WriteWin(Section, Item, TForm)　　TFormの位置とサイズを保存する

#### jsonファイルの例(TestSampleプロジェクト)

```
{
  "JSONIni" : "TestSample.json",
  "Window" : {
    "Main.Left" : 748,
    "Main.Top" : 286,
    "Main.Width" : 433,
    "Main.Height" : 281
  },
  "Options" : {
    "Edit1" : "これはテストです",
    "CheckBox1" : "TRUE",
    "RadioButton1" : "TRUE",
    "RadioGroup1" : 1,
    "SpinEdit1" : 500,
    "FloatSpinEdit1" : 1.2500000000000000E+000
  }
}
```

### ライセンス

MIT
