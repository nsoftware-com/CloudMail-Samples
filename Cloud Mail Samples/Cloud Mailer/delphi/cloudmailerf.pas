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
unit cloudmailerf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, cmoauth, cmcore, cmtypes,
  cmcloudmailer;

type
  TFormCloudmailer = class(TForm)
    lblHeading: TLabel;
    lblServiceProvider: TLabel;
    rbtnAmazonSES: TRadioButton;
    rbtnGMail: TRadioButton;
    rbtnOffice365: TRadioButton;
    cmCloudMailer1: TcmCloudMailer;
    OpenDialog1: TOpenDialog;
    lblAccessID: TLabel;
    lblSecretClient: TLabel;
    lblSendTo: TLabel;
    lblFrom: TLabel;
    lblSubject: TLabel;
    lblAuthScope: TLabel;
    lblHTML: TLabel;
    lblAttachments: TLabel;
    txtAccessID: TEdit;
    txtSecretClient: TEdit;
    txtSendTo: TEdit;
    txtFrom: TEdit;
    txtSubject: TEdit;
    txtAuthScope: TEdit;
    txtHTML: TMemo;
    lstAttachments: TListBox;
    btnAdd: TButton;
    btnRemove: TButton;
    btnSend: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure rbtnAmazonSESClick(Sender: TObject);
    procedure rbtnGMailClick(Sender: TObject);
    procedure rbtnOffice365Click(Sender: TObject);
    procedure cmCloudMailer1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCloudmailer: TFormCloudmailer;

implementation

{$R *.dfm}

procedure TFormCloudmailer.btnAddClick(Sender: TObject);
begin
  if OpenDialog1.Execute then lstAttachments.Items.Add(OpenDialog1.FileName);
end;

procedure TFormCloudmailer.btnRemoveClick(Sender: TObject);
var I: Integer;
begin
    for I := lstAttachments.Items.Count - 1 downto 0 do
    if lstAttachments.Selected[I] then lstAttachments.Items.Delete(I);
end;

procedure TFormCloudmailer.btnSendClick(Sender: TObject);
var I: Integer;
begin
  Screen.Cursor := crHourGlass;

  if (cmCloudMailer1.ServiceProvider = spGMail) or (cmCloudMailer1.ServiceProvider = spOffice365) then
  begin
    cmCloudMailer1.OAuth.ClientId := txtAccessID.Text;
    cmCloudMailer1.OAuth.ClientSecret := txtSecretClient.Text;
    cmCloudMailer1.OAuth.AuthorizationScope := txtAuthScope.Text;
    cmCloudMailer1.Authorize();
  end
  else
  begin
    cmCloudMailer1.Account.AmazonAccessKey := txtAccessID.Text;
    cmCloudMailer1.Account.AmazonSecretKey := txtSecretClient.Text;
  end;

  cmCloudMailer1.SendTo := txtSendTo.Text;
  cmCloudMailer1.From := txtFrom.Text;
  cmCloudMailer1.Subject := txtSubject.Text;
  cmCloudMailer1.MessageHTML := txtHTML.Text;
  cmCloudMailer1.AttachmentCount := 0;
  for I := 0 to lstAttachments.Items.Count - 1 do
  begin
    cmCloudMailer1.AddAttachment(lstAttachments.Items[I]);
  end;

  cmCloudMailer1.Send();
  ShowMessage('Message sent.');
  Screen.Cursor := crDefault;
end;

procedure TFormCloudmailer.cmCloudMailer1SSLServerAuthentication(
  Sender: TObject; CertEncoded: string; CertEncodedB: TArray<System.Byte>;
  const CertSubject, CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept := true;
end;

procedure TFormCloudmailer.rbtnAmazonSESClick(Sender: TObject);
begin
  cmCloudmailer1.Reset();

  if rbtnAmazonSES.Checked then
  begin
    rbtnGMail.Checked := false;
    rbtnOffice365.Checked := false;
    lblAccessID.Caption := 'Access Key';
    lblSecretClient.Caption := 'Secret Key';
    txtAccessID.Text := '';
    txtSecretClient.Text := '';
    txtAuthScope.Text := '';
    txtAuthScope.Enabled := false;
    cmCloudMailer1.ServiceProvider := spAmazonSES;
  end;
end;

procedure TFormCloudmailer.rbtnGMailClick(Sender: TObject);
begin
  cmCloudmailer1.Reset();

  if rbtnGMail.Checked then
  begin
    rbtnAmazonSES.Checked := false;
    rbtnOffice365.Checked := false;
    lblAccessID.Caption := 'Client ID';
    lblSecretClient.Caption := 'Client Secret';
    txtAccessID.Text := '';
    txtSecretClient.Text := '';
    txtAuthScope.Text := '';
    txtAuthScope.Enabled := true;
    cmCloudMailer1.ServiceProvider := spGMail;
    cmCloudMailer1.OAuth.ServerAuthURL := 'https://accounts.google.com/o/oauth2/auth';
    cmCloudMailer1.OAuth.ServerTokenURL := 'https://accounts.google.com/o/oauth2/token';
  end;
end;

procedure TFormCloudmailer.rbtnOffice365Click(Sender: TObject);
begin
  cmCloudmailer1.Reset();

  if rbtnOffice365.Checked then
  begin
    rbtnAmazonSES.Checked := false;
    rbtnGMail.Checked := false;
    lblAccessID.Caption := 'Client ID';
    lblSecretClient.Caption := 'Client Secret';
    txtAccessID.Text := '';
    txtSecretClient.Text := '';
    txtAuthScope.Text := '';
    txtAuthScope.Enabled := true;
    cmCloudMailer1.ServiceProvider := spOffice365;
    cmCloudMailer1.OAuth.ServerAuthURL := 'https://login.microsoftonline.com/common/oauth2/v2.0/authorize';
    cmCloudMailer1.OAuth.ServerTokenURL := 'https://login.microsoftonline.com/common/oauth2/v2.0/token';
  end;
end;

end.


