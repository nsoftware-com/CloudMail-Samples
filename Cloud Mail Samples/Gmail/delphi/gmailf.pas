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
unit gmailf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, cmcore,
  cmtypes, cmgmail, cmoauth;

type
  TFormGMail = class(TForm)
    gmail: TcmGmail;
    bLogin: TButton;
    PageControl1: TPageControl;
    ListMessages: TTabSheet;
    Compose: TTabSheet;
    btn_next100: TButton;
    rawMessage: TMemo;
    emailsNumber: TLabel;
    Label2: TLabel;
    bClear: TButton;
    emails: TListBox;
    sendToLabel: TLabel;
    ccLabel: TLabel;
    bccLabel: TLabel;
    SubjectLabel: TLabel;
    contentType: TComboBox;
    BodyContentLabel: TLabel;
    subject: TEdit;
    bcc: TEdit;
    cc: TEdit;
    sendTo: TEdit;
    emailBody: TMemo;
    AttachmentsLabel: TLabel;
    attachments: TListBox;
    addAttachment: TButton;
    removeAttachment: TButton;
    sendEmail: TButton;
    OpenDialog1: TOpenDialog;
    btn_refresh: TButton;
    procedure bLoginClick(Sender: TObject);
    procedure gmailMessageInfo(Sender: TObject; const Id, ThreadId, HistoryId,
      InternalDate, Subject, Labels, From, SentTo, Cc, Bcc, Snippet: string;
      Size: Integer);
    procedure btn_next100Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bClearClick(Sender: TObject);
    procedure emailsClick(Sender: TObject);
    procedure sendEmailClick(Sender: TObject);
    procedure addAttachmentClick(Sender: TObject);
    procedure removeAttachmentClick(Sender: TObject);
    procedure btn_refreshClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGMail: TFormGMail;
  isListingEmails: boolean;
  count: integer;
  emailsChanged: boolean;

const
  Header = 'Message ID';

implementation

{$R *.dfm}

procedure TFormGMail.addAttachmentClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    if FileExists(OpenDialog1.FileName) then
      attachments.Items.Add(OpenDialog1.FileName)
    else
      raise Exception.Create('File does not exist.');
end;

procedure TFormGMail.bClearClick(Sender: TObject);
begin
  rawMessage.Clear;
end;

procedure TFormGMail.bLoginClick(Sender: TObject);
begin
  try
    if bLogin.Caption = 'Login' then
    begin
      gmail.OAuth.ClientId := '922417066392-f9fomlr35hffi9gkq5te61nr47rtk00q.apps.googleusercontent.com';
      gmail.OAuth.ClientSecret := 'o8tCrbErRHjAh42whuvXGW0R';
      gmail.OAuth.ServerAuthURL := 'https://accounts.google.com/o/oauth2/auth';
      gmail.OAuth.ServerTokenURL := 'https://accounts.google.com/o/oauth2/token';
      gmail.OAuth.AuthorizationScope := 'https://mail.google.com/';
      gmail.OAuth.GrantType := TcmOAuthSettingsGrantTypes.cogtAuthorizationCode;
      gmail.Authorize();

      bLogin.Caption := 'Logout';
      PageControl1.Enabled := true;
      emails.Items.Clear;
      gmail.Config('ResponseType=4');
      isListingEmails := true;
      gmail.ListMessages('', '');
      btn_refresh.Enabled := true;
      if gmail.NextPageToken.Length > 0 then
      begin
        emailsNumber.Caption := Concat(Header, inttostr((count - 1) * 100), '-', inttostr(count * 100));
        btn_next100.Enabled := true;
      end
      else
      begin
        emailsNumber.Caption := Concat(Header, inttostr((count - 1) * 100), '-', inttostr(gmail.MessageInfo.Count));
        btn_next100.Enabled := false;
      end;

    end
    else
    begin
      gmail.Reset();
      bLogin.Caption := 'Login';
      PageControl1.Enabled := false;
    end;
  finally

  end;
end;

procedure TFormGMail.btn_next100Click(Sender: TObject);
begin
  emailsChanged := false;
  count := count + 1;
  isListingEmails := true;
  emails.Items.Clear;
  emailsChanged := false;
  rawMessage.Clear;
  gmail.Config('ResponseType=4');
  gmail.ListMessages('', '');
  if gmail.NextPageToken.Length > 0 then
  begin
    emailsNumber.Caption := Concat(Header, inttostr((count - 1) * 100), '-', inttostr(count * 100));
  end
  else
  begin
    emailsNumber.Caption := Concat(Header, inttostr((count - 1) * 100), '-', inttostr(gmail.MessageInfo.Count));
    btn_next100.Enabled := false;
  end;
end;

procedure TFormGMail.btn_refreshClick(Sender: TObject);
begin
  emailsChanged := false;
  count := 1;
  isListingEmails := true;
  emails.Items.Clear;
  emailsChanged := false;
  rawMessage.Clear;
  gmail.Config('ResponseType=4');
  gmail.NextPageToken := '';
  gmail.ListMessages('', '');
  if gmail.NextPageToken.Length > 0 then
  begin
    emailsNumber.Caption := Concat(Header, inttostr((count - 1) * 100), '-', inttostr(count * 100));
    btn_next100.Enabled := true;
  end
  else
  begin
    emailsNumber.Caption := Concat(Header, inttostr((count - 1) * 100), '-', inttostr(gmail.MessageInfo.Count));
    btn_next100.Enabled := false;
  end;
end;

procedure TFormGMail.emailsClick(Sender: TObject);
var
  id: string;
begin
    if emails.ItemIndex > -1 then
    begin
      id := emails.Items[emails.ItemIndex];
      isListingEmails := false;
      gmail.RetrieveMessageRaw(id);
      rawMessage.Text := gmail.Message;
    end;
end;

procedure TFormGMail.FormCreate(Sender: TObject);
begin
  count := 1;
  emailsChanged := true;
end;

procedure TFormGMail.gmailMessageInfo(Sender: TObject; const Id, ThreadId,
  HistoryId, InternalDate, Subject, Labels, From, SentTo, Cc, Bcc,
  Snippet: string; Size: Integer);
begin
  if isListingEmails then
  begin
    emails.Items.Add(Id);
  end;
end;

procedure TFormGMail.removeAttachmentClick(Sender: TObject);
begin
  if attachments.ItemIndex >= 0 then
    attachments.Items.Delete(attachments.ItemIndex);
end;

procedure TFormGMail.sendEmailClick(Sender: TObject);
var
  i: integer;
  allAttachments: string;
begin
  if not(sendTo.Text = null) then
  begin
    gmail.MessageTo := sendTo.Text;
    gmail.MessageCc := cc.Text;
    gmail.MessageBcc := bcc.Text;
    gmail.MessageSubject := subject.Text;
    gmail.MessageBodyContentType := contentType.Text;
    gmail.MessageBodyContent := emailBody.Text;
    allAttachments := '';
    for i := 0 to attachments.Count - 1 do
    begin
      allAttachments := allAttachments + attachments.Items[i] + ';';
    end;
    gmail.MessageAttachments := allAttachments;
    gmail.SendMail();
    ShowMessage('Message sent!');

  end;
end;

end.



