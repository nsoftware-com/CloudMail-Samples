(*
 * Cloud Mail 2024 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of Cloud Mail in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/cloudmail
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit office365f;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  cmcore, cmtypes, cmoffice365, cmoauth, StdCtrls, ExtCtrls, ComCtrls;

type
  GetMode = ( HEADERS_, TEXT_ );
  TFormOffice365 = class(TForm)
    ListViewMessages: TListView;
    ListBoxMessage: TListBox;
    TreeViewMailboxes: TTreeView;
    ButtonCompose: TButton;
    ButtonLogin: TButton;
    Label5: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    office365: TcmOffice365;
    EditClientID: TEdit;
    EditClientSecret: TEdit;
    LabelClientID: TLabel;
    LabelClientSecret: TLabel;
    procedure ButtonLoginClick(Sender: TObject);
    procedure TreeViewMailboxesClick(Sender: TObject);
    procedure ListViewMessagesClick(Sender: TObject);
    procedure ButtonComposeClick(Sender: TObject);

  private
    { Private declarations }
    head_: TTreeNode;
    textpart: String;
  public
    { Public declarations }
    constructor Create( Owner: TComponent );  override;
    procedure AddChildFolders(Parent: TTreeNode; id: String);
    procedure SetData(node: TTreeNode; str: String);
  end;

var
  FormOffice365: TFormOffice365;

implementation

uses composef;

{$R *.DFM}

constructor TFormOffice365.Create( Owner: TComponent );
begin
   inherited Create(Owner);
   head_ := TreeViewMailboxes.Items.Add( nil, 'Folders');
   textpart := '0';
end;


procedure TFormOffice365.ButtonLoginClick(Sender: TObject);
var folder: TcmOLFolder;
folderList: TcmOLFolderList;
i: integer;
newFolder: TTreeNode;
begin
// Login / Logout
  Screen.Cursor := crHourGlass;
  
  // clear old folders
  for i := TreeViewMailboxes.Items.Count - 1 downto 1 do
    TreeViewMailboxes.Items[i].Delete();

  office365.OAuth.ClientId := EditClientID.Text;
  office365.OAuth.ClientSecret := EditClientSecret.Text;
  office365.OAuth.ServerAuthURL := 'https://login.microsoftonline.com/common/oauth2/v2.0/authorize';
  office365.OAuth.ServerTokenURL := 'https://login.microsoftonline.com/common/oauth2/v2.0/token';
  office365.OAuth.AuthorizationScope := 'user.read mail.readwrite mail.send mailboxsettings.readwrite';
  office365.OAuth.GrantType := TcmOAuthSettingsGrantTypes.cogtAuthorizationCode;

  office365.Authorize();

  office365.ListFolders('');
  folderList := office365.Folders.Clone();

  for i := 0 to folderList.Count - 1 do
  begin
    folder := folderList[i];
    newFolder := TreeViewMailboxes.Items.AddChild(TreeViewMailboxes.Items[0], folder.DisplayName);

    SetData(newFolder, folder.Id);

    if (folder.ChildFolderCount > 0)
    then
    begin
      AddChildFolders(newFolder, folder.Id);
    end;
  end;

  ButtonCompose.Enabled := True;
  Screen.Cursor := crDefault;
end;


procedure TFormOffice365.AddChildFolders(Parent: TTreeNode; id: String);
var folder: TcmOLFolder;
folderList: TcmOLFolderList;
i: integer;
newFolder: TTreeNode;
begin
  office365.ListFolders(id);
  folderList := office365.Folders.Clone();

  for i := 0 to folderList.Count - 1 do
  begin
    folder := folderList[i];
    newFolder := TreeViewMailboxes.Items.AddChild(Parent, folder.DisplayName);

    SetData(newFolder, folder.Id);

    if (folder.ChildFolderCount > 0)
    then
    begin
      AddChildFolders(newFolder, folder.Id);
    end;
  end;

end;

procedure TFormOffice365.TreeViewMailboxesClick(Sender: TObject);
var i: integer;
mail: TListItem;
begin
// Click the tree
   Screen.Cursor := crHourGlass;
   try
      if TreeViewMailboxes.Selected.Level > 0 then
      begin
        office365.ListMessages(string(TreeViewMailboxes.Selected.Data^), '');
        for i := 0 to office365.MessageInfoCount - 1 do
        begin
          mail := ListViewMessages.Items.Add;
          mail.SubItems.Add(office365.MessageInfoSubject[i]);
          mail.SubItems.Add(office365.MessageInfoFrom[i]);
          mail.SubItems.Add(office365.MessageInfoReceivedDate[i]);
        end;
      end;
   except on E: ECloudMail do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormOffice365.SetData(node: TTreeNode; str: String);
var
pStr : ^String;
begin
  New(pStr);
  pStr^ := str;
  node.Data := pStr;
end;

procedure TFormOffice365.ListViewMessagesClick(Sender: TObject);
begin
// Select a message
   Screen.Cursor := crHourGlass;
   try
      ListBoxMessage.Items.Text := office365.MessageInfoBodyContent[ListViewMessages.Selected.Index];

      Label3.Caption := ListViewMessages.Selected.SubItems.Strings[1];
      Label4.Caption := ListViewMessages.Selected.SubItems.Strings[0];
   except on E: ECloudMail do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;

procedure TFormOffice365.ButtonComposeClick(Sender: TObject);
var
   i: integer;
begin
// Compose
   Screen.Cursor := crHourGlass;
   if FormCompose.ShowModal() = mrOk then
   begin
      office365.MessageFrom := FormCompose.EditFrom.Text;
      office365.MessageTo:= FormCompose.EditTo.Text;
      office365.MessageCc := FormCompose.EditCc.Text;
      office365.MessageSubject := FormCompose.EditSubject.Text;
      office365.MessageBodyContentType := 'TEXT';
      office365.MessageBodyContent := FormCompose.MemoMessage.Text;

      if FormCompose.ComboBoxAttachments.Items.Count <> 0 then
      begin
         //add attachments
         for i := 1 to FormCompose.ComboBoxAttachments.Items.Count do
          office365.MessageAttachments.Add(TcmOLAttachment.Create(FormCompose.ComboBoxAttachments.Items.Strings[i-1]));
      end;

      office365.SendMail(True);

      FormCompose.EditCc.Text := '';
      FormCompose.EditTo.Text := '';
      FormCompose.EditSubject.Text := '';
      FormCompose.ComboBoxAttachments.Items.Clear();
      FormCompose.MemoMessage.Lines.Clear();
   end;
   Screen.Cursor := crDefault;
end;

end.



//---------------------------------------------------------------------------






