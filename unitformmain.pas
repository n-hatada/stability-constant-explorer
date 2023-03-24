{ Stability Constant Explorer ver.1.0.3

 The source code of this program is in the public domain.
 Date: Oct. 26, 2022
 Author: Naoyuki Hatada

 The accompanying database file (NIST_SRD_46_ported.db) is in the SQLite format
 and contains data ported from "SRD 46 SQL.zip" which is distributed at the
 following website.

 Donald R. Burgess (2004), NIST SRD 46. Critically Selected Stability Constants
 of Metal Complexes: Version 8.0 for Windows, National Institute of Standards
 and Technology, https://doi.org/10.18434/M32154

}

unit unitformmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, SQLite3Conn, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Grids, Clipbrd, ExtCtrls, ComCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonCopyTable: TButton;
    EditSearchResult: TEdit;
    GroupBoxLigand: TGroupBox;
    GroupBoxMetal: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBoxMetal: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    RadioButtonAllMetals: TRadioButton;
    RadioButtonAllLigands: TRadioButton;
    RadioButtonSelectMetals: TRadioButton;
    RadioButtonSelectLigands: TRadioButton;
    Splitter1: TSplitter;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StringGridLiterature: TStringGrid;
    StringGridSearchResults: TStringGrid;
    TreeViewLigand: TTreeView;
    procedure ButtonCopyTableClick(Sender: TObject);
    procedure CheckListBoxLigandItemClick(Sender: TObject; Index: integer);
    procedure CheckListBoxMetalItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxMetalSelectionChange(Sender: TObject; User: boolean);
    procedure RadioButtonAllLigandsChange(Sender: TObject);
    procedure RadioButtonAllMetalsChange(Sender: TObject);
    procedure RadioButtonSelectLigandsChange(Sender: TObject);
    procedure RadioButtonSelectMetalsChange(Sender: TObject);
    procedure StringGridSearchResultsSelection(Sender: TObject;
      aCol, aRow: integer);
    procedure TreeViewLigandSelectionChanged(Sender: TObject);
  private
    procedure DoSearchAndShowResults();
  public

  end;


  { TMetal }

  TMetal = class(TObject)
  private
    FMetalName: string;
    FFormattedMetalName: string;
    procedure FormatMetalName;
    procedure SetMetalName(AValue: string);
  public
    property MetalName: string read FMetalName write SetMetalName;
    property FormattedMetalName: string read FFormattedMetalName;
    constructor Create(AMetalName: string);
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TMetal }

procedure TMetal.FormatMetalName;
var
  i, StartPos, EndPos: integer;
  OriginalStr, FormattedStr: string;
begin
  //Apply subscript
  OriginalStr := FMetalName;
  FormattedStr := '';
  i := 1;
  repeat
    StartPos := Pos('<sub>', OriginalStr, i);
    if StartPos > 0 then
    begin
      //when '<sub>' is found
      FormattedStr := FormattedStr + Copy(OriginalStr, i, StartPos - i);
      i := StartPos;
      Delete(OriginalStr, StartPos, 5);
      EndPos := Pos('</sub>', OriginalStr, i);
      if EndPos > 0 then Delete(OriginalStr, EndPos, 6);
      while (i <= Length(OriginalStr)) and (i <> EndPos) do
      begin
        case OriginalStr[i] of
          '0': FormattedStr := FormattedStr + #$E2#$82#$80;
          '1': FormattedStr := FormattedStr + #$E2#$82#$81;
          '2': FormattedStr := FormattedStr + #$E2#$82#$82;
          '3': FormattedStr := FormattedStr + #$E2#$82#$83;
          '4': FormattedStr := FormattedStr + #$E2#$82#$84;
          '5': FormattedStr := FormattedStr + #$E2#$82#$85;
          '6': FormattedStr := FormattedStr + #$E2#$82#$86;
          '7': FormattedStr := FormattedStr + #$E2#$82#$87;
          '8': FormattedStr := FormattedStr + #$E2#$82#$88;
          '9': FormattedStr := FormattedStr + #$E2#$82#$89;
          '+': FormattedStr := FormattedStr + #$E2#$82#$8A;
          '-': FormattedStr := FormattedStr + #$E2#$82#$8B;
          else
            FormattedStr := FormattedStr + OriginalStr[i]
        end;
        Inc(i);
      end;
    end
    else
    begin
      //when '<sub>' is not found
      FormattedStr := FormattedStr + Copy(OriginalStr, i, Length(OriginalStr) - i + 1);
      Break;
    end;
  until False;
  //Apply superscript
  OriginalStr := FormattedStr;
  FormattedStr := '';
  i := 1;
  repeat
    StartPos := Pos('<sup>', OriginalStr, i);
    if StartPos > 0 then
    begin
      //when '<sup>' is found
      FormattedStr := FormattedStr + Copy(OriginalStr, i, StartPos - i);
      i := StartPos;
      Delete(OriginalStr, StartPos, 5);
      EndPos := Pos('</sup>', OriginalStr, i);
      if EndPos > 0 then Delete(OriginalStr, EndPos, 6);
      while (i <= Length(OriginalStr)) and (i <> EndPos) do
      begin
        case OriginalStr[i] of
          '0': FormattedStr := FormattedStr + #$E2#$81#$B0;
          '1': FormattedStr := FormattedStr + #$C2#$B9;
          '2': FormattedStr := FormattedStr + #$C2#$B2;
          '3': FormattedStr := FormattedStr + #$C2#$B3;
          '4': FormattedStr := FormattedStr + #$E2#$81#$B4;
          '5': FormattedStr := FormattedStr + #$E2#$81#$B5;
          '6': FormattedStr := FormattedStr + #$E2#$81#$B6;
          '7': FormattedStr := FormattedStr + #$E2#$81#$B7;
          '8': FormattedStr := FormattedStr + #$E2#$81#$B8;
          '9': FormattedStr := FormattedStr + #$E2#$81#$B9;
          '+': FormattedStr := FormattedStr + #$E2#$81#$BA;
          '-': FormattedStr := FormattedStr + #$E2#$81#$BB;
          else
            FormattedStr := FormattedStr + OriginalStr[i]
        end;
        Inc(i);
      end;
    end
    else
    begin
      //when '<sup>' is not found
      FormattedStr := FormattedStr + Copy(OriginalStr, i, Length(OriginalStr) - i + 1);
      Break;
    end;
  until False;
  FFormattedMetalName:=FormattedStr;
end;


procedure TMetal.SetMetalName(AValue: string);
begin
  if FMetalName = AValue then Exit;
  FMetalName := AValue;
  FormatMetalName;
end;

constructor TMetal.Create(AMetalName: string);
begin
  MetalName := AMetalName;
end;

destructor TMetal.Destroy;
begin
  inherited Destroy;
end;

{ TFormMain }

//When the form is created, metals and ligands are listed in the listbox and treeview.
procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
  CurrentLigandClassStr: string;
  CurrentMetal: TMetal;
begin
  SQLite3Connection1.DatabaseName :=
    ExtractFilePath(Application.ExeName) + 'NIST_SRD_46_ported.db';
  ListBoxMetal.Clear;
  TreeViewLigand.Items.Clear;
  //metals are listed.
  SQLQuery1.SQL.Text := 'SELECT name_metal FROM metal;';
  SQLQuery1.Open;
  while (not SQLQuery1.EOF) do
  begin
    CurrentMetal := TMetal.Create(SQLQuery1.FieldByName('name_metal').AsString);
    ListBoxMetal.AddItem(CurrentMetal.FormattedMetalName, CurrentMetal);
    SQLQuery1.Next;
  end;
  //ligand classes are listed.
  SQLQuery1.Close;
  SQLQuery1.SQL.Text := 'SELECT name_ligandclass FROM ligand_class;';
  SQLQuery1.Open;
  while (not SQLQuery1.EOF) do
  begin
    TreeViewLigand.Items.Add(nil, SQLQuery1.FieldByName('name_ligandclass').AsString);
    SQLQuery1.Next;
  end;
  //ligands are appended to each ligand class.
  for i := 0 to Pred(TreeViewLigand.Items.TopLvlCount) do
  begin
    CurrentLigandClassStr := TreeViewLigand.Items.TopLvlItems[i].Text;
    SQLQuery1.Close;
    SQLQuery1.SQL.Text := 'SELECT name_ligand FROM liganden ' +
      'INNER JOIN ligand_class on liganden.ligand_classNr=ligand_class.ligand_classID '
      +
      'WHERE ligand_class.name_ligandclass="' + CurrentLigandClassStr + '";';
    SQLQuery1.Open;
    while (not SQLQuery1.EOF) do
    begin
      TreeViewLigand.Items.AddChild(TreeViewLigand.Items.TopLvlItems[i],
        SQLQuery1.FieldByName('name_ligand').AsString);
      SQLQuery1.Next;
    end;
  end;
end;

//When a metal(s) is selected on the listbox, the right radio button is checked and search is performed.
procedure TFormMain.ListBoxMetalSelectionChange(Sender: TObject; User: boolean);
begin
  if RadioButtonSelectMetals.Checked = False then
    RadioButtonSelectMetals.Checked := True
  else
    DoSearchAndShowResults();
end;

procedure TFormMain.RadioButtonAllLigandsChange(Sender: TObject);
begin
  DoSearchAndShowResults();
end;

procedure TFormMain.RadioButtonAllMetalsChange(Sender: TObject);
begin
  DoSearchAndShowResults();
end;

procedure TFormMain.RadioButtonSelectLigandsChange(Sender: TObject);
begin
  DoSearchAndShowResults();
end;

procedure TFormMain.RadioButtonSelectMetalsChange(Sender: TObject);
begin
  DoSearchAndShowResults();
end;


//When a cell in the search result stringgrid is selected. -> References for the line is shown.
procedure TFormMain.StringGridSearchResultsSelection(Sender: TObject;
  aCol, aRow: integer);
var
  i, j: integer;
begin
  SQLQuery1.Close;
  SQLQuery1.SQL.Text :=
    'SELECT Distinct literature_shortcut as Code, literature_alt as Literature ' +
    'FROM verkn_ligand_metal vlm ' +
    'INNER JOIN liganden lig on vlm.ligandenNr=lig.ligandenID ' +
    'INNER JOIN metal met on vlm.metalNr=met.metalID ' +
    'INNER JOIN verkn_ligand_metal_literature ' +
    'on vlm.ligandenNr=verkn_ligand_metal_literature.ligandenNr ' +
    'AND vlm.metalNr=verkn_ligand_metal_literature.metalNr ' +
    'INNER JOIN literature_alt on verkn_ligand_metal_literature.literature_altNr=literature_alt.literature_altID '
    + 'WHERE name_metal="' + StringGridSearchResults.Cells[0, aRow] +
    '" AND ' + 'name_ligand="' + StringGridSearchResults.Cells[1, aRow] + '";';
  SQLQuery1.Open;
  SQLQuery1.Last;
  StringGridLiterature.RowCount := SQLQuery1.RecordCount + 1;
  SQLQuery1.First;
  i := 1;
  while (not SQLQuery1.EOF) do
  begin
    for j := 0 to Pred(SQLQuery1.FieldCount) do
    begin
      StringGridLiterature.Cells[j, i] := SQLQuery1.Fields.Fields[j].AsString;
    end;
    Inc(i);
    SQLQuery1.Next;
  end;
end;

//When a ligand(s) is selected on the treeview, the right radio button is checked and search is performed.
procedure TFormMain.TreeViewLigandSelectionChanged(Sender: TObject);
begin
  if RadioButtonSelectLigands.Checked = False then
    RadioButtonSelectLigands.Checked := True
  else
    DoSearchAndShowResults();
end;

procedure TFormMain.CheckListBoxLigandItemClick(Sender: TObject; Index: integer);
begin
  DoSearchAndShowResults();
end;

procedure TFormMain.CheckListBoxMetalItemClick(Sender: TObject; Index: integer);
begin
  DoSearchAndShowResults();
end;

//Copy the search result table to clipboard.
procedure TFormMain.ButtonCopyTableClick(Sender: TObject);
var
  SGRow, SGRows: TStringList;
  i: integer;
begin
  SGRow := TStringList.Create;
  SGRow.StrictDelimiter := True;
  SGRow.Delimiter := #9;//Tab
  SGRows := TStringList.Create;
  try
    for i := 0 to StringGridSearchResults.RowCount - 1 do
    begin
      SGRow.Clear;
      SGRow.AddStrings(StringGridSearchResults.Rows[i]);
      SGRows.Add(SGRow.DelimitedText);
    end;
    Clipboard.AsText := SGRows.Text;
  finally
    SGRow.Free;
    SGRows.Free;
  end;
end;

//Search stability constants and show them in a stringgrid.
procedure TFormMain.DoSearchAndShowResults();
var
  WhereText, WhereTextMetal, WhereTextLigand: string;
  i, j: integer;
begin
  WhereText := '';
  WhereTextMetal := '';
  WhereTextLigand := '';
  if RadioButtonSelectMetals.Checked then
  begin
    for i := 0 to Pred(ListBoxMetal.Count) do
    begin
      if ListBoxMetal.Selected[i] then
      begin
        if Length(WhereTextMetal) > 0 then
          WhereTextMetal := WhereTextMetal + ' OR ';
        WhereTextMetal := WhereTextMetal + 'name_metal="' +
          TMetal(ListBoxMetal.Items.Objects[i]).MetalName + '"';
      end;
    end;
  end;
  if RadioButtonSelectLigands.Checked then
  begin
    for i := 0 to Pred(TreeViewLigand.Items.TopLvlCount) do
    begin
      if TreeViewLigand.Items.TopLvlItems[i].Selected then
      begin
        //If a ligand class is selected,
        //it will be a search target.
        if Length(WhereTextLigand) > 0 then
          WhereTextLigand := WhereTextLigand + ' OR ';
        WhereTextLigand := WhereTextLigand + 'name_ligandclass="' +
          TreeViewLigand.Items.TopLvlItems[i].Text + '"';
      end;
    end;
    for i := 0 to Pred(TreeViewLigand.Items.Count) do
    begin
      if TreeViewLigand.Items.Item[i].Level = 1 then
      begin
        //If a ligand is selected, and the ligand class to which a ligand belongs is not selected,
        //the ligand will be a search target.
        if TreeViewLigand.Items.Item[i].Selected and not
          (TreeViewLigand.Items.Item[i].Parent.Selected) then
        begin
          if Length(WhereTextLigand) > 0 then
            WhereTextLigand := WhereTextLigand + ' OR ';
          WhereTextLigand := WhereTextLigand + 'name_ligand="' +
            TreeViewLigand.Items.Item[i].Text + '"';
        end;
      end;
    end;
  end;
  if (Length(WhereTextMetal) > 0) and (Length(WhereTextLigand) > 0) then
    WhereText := '(' + WhereTextMetal + ') AND (' + WhereTextLigand + ')'
  else
    WhereText := WhereTextMetal + WhereTextLigand;
  //If no metal or ligand is selected, the whole result is not shown because it will take a long time.
  if Length(WhereText) = 0 then
  begin
    StringGridSearchResults.Clear;
    EditSearchResult.Text := 'Either metal ion(s) or ligand(s) must be specified.';
    Exit;
  end;
  //Search result is shown in the string grid.
  SQLQuery1.Close;
  SQLQuery1.SQL.Text :=
    'SELECT met.name_metal as ''Metal ion'', ' + 'lig.name_ligand as ''Ligand'', ' +
    'lig.formula as ''Formula'', ' +
    'ligand_class.name_ligandclass as ''Ligand class'', ' +
    'beta.name_beta_definition as ''Equilibrium'', ' +
    'temperature as ''Temperature (C)'', ' + 'ionicstrength as ''Ionic strength'', ' +
    'case ' + 'when constanttyp.name_constanttyp = ''K'' then ''Log K'' ' +
    'when constanttyp.name_constanttyp=''H'' then ''DH (kJ/mol)'' ' +
    'when constanttyp.name_constanttyp=''S'' then ''DS (J/mol.K)'' ' +
    'end as ''Value type'', ' + 'constant as ''Value'', ' +
    'footnote.name_footnote as ''Note'' ' + 'FROM verkn_ligand_metal vlm ' +
    'LEFT OUTER JOIN liganden lig on vlm.ligandenNr=lig.ligandenID ' +
    'LEFT OUTER JOIN metal met on vlm.metalNr=met.metalID ' +
    'LEFT OUTER JOIN beta_definition beta on vlm.beta_definitionNr=beta.beta_definitionID '
    + 'LEFT OUTER JOIN constanttyp on vlm.constanttypNr=constanttyp.constanttypID ' +
    'LEFT OUTER JOIN footnote on vlm.footnoteNr=footnote.footnoteID ' +
    'LEFT OUTER JOIN solvent on vlm.solventNr=solvent.solventID ' +
    'LEFT OUTER JOIN ligand_class on lig.ligand_classNr=ligand_class.ligand_classID ';
  if Length(WhereText) > 0 then
    SQLQuery1.SQL.Text := SQLQuery1.SQL.Text + 'WHERE ' + WhereText;
  SQLQuery1.SQL.Text := SQLQuery1.SQL.Text + ';';
  SQLQuery1.Open;
  SQLQuery1.Last;
  StringGridSearchResults.ColCount := SQLQuery1.FieldCount;
  StringGridSearchResults.RowCount := SQLQuery1.RecordCount + 1;
  //The record count is shown in the editbox.
  EditSearchResult.Text := IntToStr(SQLQuery1.RecordCount) + ' data found.';
  //The header line is prepared.
  for i := 0 to Pred(SQLQuery1.FieldCount) do
  begin
    StringGridSearchResults.Cells[i, 0] := SQLQuery1.FieldDefs.Items[i].Name;
  end;
  StringGridSearchResults.FixedRows := 1;
  //Data lines are appended.
  SQLQuery1.First;
  i := 1;
  while (not SQLQuery1.EOF) and (i < StringGridSearchResults.RowCount) do
  begin
    for j := 0 to Pred(SQLQuery1.FieldCount) do
    begin
      StringGridSearchResults.Cells[j, i] := SQLQuery1.Fields.Fields[j].AsString;
    end;
    Inc(i);
    SQLQuery1.Next;
  end;
end;

end.
