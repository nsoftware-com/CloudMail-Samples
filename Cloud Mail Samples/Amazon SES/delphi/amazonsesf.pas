(*
 * Cloud Mail 2022 Delphi Edition - Sample Project
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
unit amazonsesf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cmcore, cmamazonses;

type
  TFormAmazonses = class(TForm)
    lblHeading: TLabel;
    lblMailServer: TLabel;
    txtSendTo: TEdit;
    lblSendTo: TLabel;
    txtFrom: TEdit;
    lblFrom: TLabel;
    txtSubject: TEdit;
    lblSubject: TLabel;
    lblHTML: TLabel;
    txtHTML: TMemo;
    lstAttachments: TListBox;
    lblAttachments: TLabel;
    btnAdd: TButton;
    btnRemove: TButton;
    btnSend: TButton;
    mailAmazonSES1: TcmAmazonSES;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    txtAccessKey: TEdit;
    txtSecretKey: TEdit;
    procedure btnSendClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAmazonses: TFormAmazonses;

implementation

{$R *.dfm}

procedure TFormAmazonses.btnAddClick(Sender: TObject);
begin
  if OpenDialog1.Execute then lstAttachments.Items.Add(OpenDialog1.FileName);
end;

procedure TFormAmazonses.btnRemoveClick(Sender: TObject);
var I: Integer;
begin
    for I := lstAttachments.Items.Count - 1 downto 0 do
    if lstAttachments.Selected[I] then lstAttachments.Items.Delete(I) ;
end;

procedure TFormAmazonses.btnSendClick(Sender: TObject);
var I: Integer;
begin
  Screen.Cursor := crHourGlass;

  try
    mailAmazonSES1.AccessKey := txtAccessKey.Text;
    mailAmazonSES1.SecretKey := txtSecretKey.Text;
    mailAmazonSES1.SendTo := txtSendTo.Text;
    mailAmazonSES1.Subject := txtSubject.Text;
    mailAmazonSES1.From := txtFrom.Text;

    mailAmazonSES1.MessageHTML := txtHtml.Text;
    mailAmazonSES1.AttachmentCount := 0;

    for I := 0 to lstAttachments.Items.Count - 1 do
    begin
      mailAmazonSES1.AddAttachment (lstAttachments.Items[I]);
    end;

    mailAmazonSES1.Send;
    ShowMessage('Message Sent');
  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;
  Screen.Cursor := crDefault;
end;

end.

