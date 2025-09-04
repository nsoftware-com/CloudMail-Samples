object FormCloudmailer: TFormCloudmailer
  Left = 0
  Top = 0
  Caption = 'CloudMailer Demo'
  ClientHeight = 474
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 15
  object lblHeading: TLabel
    Left = 8
    Top = 8
    Width = 605
    Height = 26
    Caption = 
      'The CloudMailer component can be used to send emails with html p' +
      'arts as well as file attachments.  It will handle all necessary ' +
      'encoding automatically.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblServiceProvider: TLabel
    Left = 8
    Top = 50
    Width = 84
    Height = 15
    Caption = 'Service Provider'
  end
  object lblAccessID: TLabel
    Left = 8
    Top = 82
    Width = 58
    Height = 15
    Caption = 'Access Key'
  end
  object lblSecretClient: TLabel
    Left = 310
    Top = 82
    Width = 54
    Height = 15
    Caption = 'Secret Key'
  end
  object lblSendTo: TLabel
    Left = 8
    Top = 149
    Width = 41
    Height = 15
    Caption = 'Send To'
  end
  object lblFrom: TLabel
    Left = 320
    Top = 149
    Width = 28
    Height = 15
    Caption = 'From'
  end
  object lblSubject: TLabel
    Left = 8
    Top = 184
    Width = 39
    Height = 15
    Caption = 'Subject'
  end
  object lblAuthScope: TLabel
    Left = 8
    Top = 114
    Width = 112
    Height = 15
    Caption = 'Authorization Scopes'
  end
  object lblHTML: TLabel
    Left = 8
    Top = 216
    Width = 105
    Height = 15
    Caption = 'HTML Message Text'
  end
  object lblAttachments: TLabel
    Left = 8
    Top = 412
    Width = 68
    Height = 15
    Caption = 'Attachments'
  end
  object rbtnAmazonSES: TRadioButton
    Left = 136
    Top = 50
    Width = 89
    Height = 17
    Caption = 'Amazon SES'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = rbtnAmazonSESClick
  end
  object rbtnGMail: TRadioButton
    Left = 312
    Top = 50
    Width = 49
    Height = 17
    Caption = 'Gmail'
    TabOrder = 1
    OnClick = rbtnGMailClick
  end
  object rbtnOffice365: TRadioButton
    Left = 456
    Top = 50
    Width = 73
    Height = 17
    Caption = 'Office 365'
    TabOrder = 2
    OnClick = rbtnOffice365Click
  end
  object txtAccessID: TEdit
    Left = 72
    Top = 79
    Width = 233
    Height = 23
    TabOrder = 3
  end
  object txtSecretClient: TEdit
    Left = 377
    Top = 79
    Width = 239
    Height = 23
    TabOrder = 4
  end
  object txtSendTo: TEdit
    Left = 74
    Top = 146
    Width = 231
    Height = 23
    TabOrder = 6
  end
  object txtFrom: TEdit
    Left = 377
    Top = 146
    Width = 239
    Height = 23
    TabOrder = 7
  end
  object txtSubject: TEdit
    Left = 74
    Top = 181
    Width = 542
    Height = 23
    TabOrder = 8
    Text = 'CloudMailer Demo Test Message'
  end
  object txtAuthScope: TEdit
    Left = 126
    Top = 111
    Width = 490
    Height = 23
    Enabled = False
    TabOrder = 5
  end
  object txtHTML: TMemo
    Left = 8
    Top = 237
    Width = 608
    Height = 169
    Lines.Strings = (
      'This message was sent with the /n software CloudMailer Control!'
      
        '<a href="http://www.nsoftware.com">www.nsoftware.com</a> is our ' +
        'web site address.'
      
        'Or, you can email us at <a href="mailto:support@nsoftware.com">s' +
        'upport@nsoftware.com</a>.'
      'Thank you for using our products!'
      '<p>'
      
        'You can include your HTML links and images, just be sure that th' +
        'e paths are relative to the directory'
      'where you are running the program.')
    TabOrder = 9
  end
  object lstAttachments: TListBox
    Left = 82
    Top = 412
    Width = 286
    Height = 56
    ItemHeight = 15
    TabOrder = 10
  end
  object btnAdd: TButton
    Left = 374
    Top = 412
    Width = 66
    Height = 25
    Caption = 'Add'
    TabOrder = 11
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 374
    Top = 443
    Width = 66
    Height = 25
    Caption = 'Remove'
    TabOrder = 12
    OnClick = btnRemoveClick
  end
  object btnSend: TButton
    Left = 446
    Top = 441
    Width = 66
    Height = 25
    Caption = 'Send'
    TabOrder = 13
    OnClick = btnSendClick
  end
  object cmCloudMailer1: TcmCloudMailer
    SSLCertStore = 'MY'
    OnSSLServerAuthentication = cmCloudMailer1SSLServerAuthentication
    Left = 552
    Top = 440
  end
  object OpenDialog1: TOpenDialog
    Left = 584
    Top = 440
  end
end


