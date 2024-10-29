object FormOffice365: TFormOffice365
  Left = 281
  Top = 159
  Caption = 'Office365 Demo'
  ClientHeight = 529
  ClientWidth = 754
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 728
    Height = 26
    Caption = 
      'Use this demo to connect to a Microsoft Outlook server and check' +
      ' email.  Enter your OAuth client ID and secret, ensure you allow' +
      ' the necessary permissions, then click the '#39'Login'#39' button to dow' +
      'nload the folder list from the server.  Then select a mailbox fr' +
      'om the Tree and the List Box will be populated.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label4: TLabel
    Left = 244
    Top = 276
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 244
    Top = 260
    Width = 3
    Height = 13
  end
  object Label1: TLabel
    Left = 184
    Top = 260
    Width = 32
    Height = 13
    Caption = 'From:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 184
    Top = 276
    Width = 48
    Height = 13
    Caption = 'Subject:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelClientID: TLabel
    Left = 119
    Top = 53
    Width = 46
    Height = 13
    Caption = 'Client ID: '
  end
  object LabelClientSecret: TLabel
    Left = 384
    Top = 53
    Width = 66
    Height = 13
    Caption = 'Client Secret: '
  end
  object ListViewMessages: TListView
    Left = 175
    Top = 88
    Width = 571
    Height = 169
    Columns = <
      item
        Caption = '#'
        Width = 0
      end
      item
        Caption = 'Subject'
        Width = 200
      end
      item
        Caption = 'From'
        Width = 160
      end
      item
        Caption = 'Date'
        Width = 120
      end>
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = ListViewMessagesClick
  end
  object ListBoxMessage: TListBox
    Left = 175
    Top = 296
    Width = 571
    Height = 225
    ItemHeight = 13
    TabOrder = 1
  end
  object TreeViewMailboxes: TTreeView
    Left = 8
    Top = 88
    Width = 157
    Height = 433
    Indent = 19
    ReadOnly = True
    TabOrder = 2
    OnClick = TreeViewMailboxesClick
  end
  object ButtonCompose: TButton
    Left = 671
    Top = 53
    Width = 75
    Height = 23
    Caption = 'Compose'
    Enabled = False
    TabOrder = 3
    OnClick = ButtonComposeClick
  end
  object ButtonLogin: TButton
    Left = 8
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Login'
    TabOrder = 4
    OnClick = ButtonLoginClick
  end
  object EditClientID: TEdit
    Left = 171
    Top = 53
    Width = 210
    Height = 21
    TabOrder = 5
  end
  object EditClientSecret: TEdit
    Left = 456
    Top = 53
    Width = 209
    Height = 21
    TabOrder = 6
  end
  object office365: TcmOffice365
    SSLAcceptServerCertStore = 'MY'
    SSLCertStore = 'MY'
    Left = 720
    Top = 264
  end
end


