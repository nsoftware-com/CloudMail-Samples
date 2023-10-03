object FormGMail: TFormGMail
  Left = 0
  Top = 0
  Caption = 'Gmail'
  ClientHeight = 608
  ClientWidth = 815
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 15
  object bLogin: TButton
    Left = 8
    Top = 0
    Width = 75
    Height = 25
    Caption = 'Login'
    TabOrder = 0
    OnClick = bLoginClick
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 31
    Width = 799
    Height = 576
    ActivePage = ListMessages
    Enabled = False
    TabOrder = 1
    object ListMessages: TTabSheet
      Caption = 'ListMessages'
      object emailsNumber: TLabel
        Left = 3
        Top = 54
        Width = 22
        Height = 20
        Caption = '0-0'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object Label2: TLabel
        Left = 207
        Top = 58
        Width = 93
        Height = 20
        Caption = 'Raw Message:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object btn_next100: TButton
        Left = 11
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Next 100'
        Enabled = False
        TabOrder = 0
        OnClick = btn_next100Click
      end
      object rawMessage: TMemo
        Left = 207
        Top = 80
        Width = 581
        Height = 463
        ScrollBars = ssVertical
        TabOrder = 1
      end
      object bClear: TButton
        Left = 704
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Clear'
        TabOrder = 2
        OnClick = bClearClick
      end
      object emails: TListBox
        Left = 3
        Top = 80
        Width = 198
        Height = 463
        ItemHeight = 15
        TabOrder = 3
        OnClick = emailsClick
      end
      object btn_refresh: TButton
        Left = 92
        Top = 16
        Width = 75
        Height = 25
        Caption = 'Refresh'
        TabOrder = 4
        OnClick = btn_refreshClick
      end
    end
    object Compose: TTabSheet
      Caption = 'Compose'
      ImageIndex = 1
      object sendToLabel: TLabel
        Left = 16
        Top = 32
        Width = 56
        Height = 20
        Caption = 'Send To:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object ccLabel: TLabel
        Left = 16
        Top = 80
        Width = 21
        Height = 20
        Caption = 'CC:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object bccLabel: TLabel
        Left = 16
        Top = 120
        Width = 30
        Height = 20
        Caption = 'BCC:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object SubjectLabel: TLabel
        Left = 16
        Top = 160
        Width = 52
        Height = 20
        Caption = 'Subject:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object BodyContentLabel: TLabel
        Left = 16
        Top = 203
        Width = 128
        Height = 20
        Caption = 'Body Content Type:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object AttachmentsLabel: TLabel
        Left = 3
        Top = 432
        Width = 86
        Height = 20
        Caption = 'Attachments:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object contentType: TComboBox
        Left = 166
        Top = 200
        Width = 195
        Height = 23
        ItemIndex = 0
        TabOrder = 0
        Text = 'text'
        Items.Strings = (
          'text'
          'html')
      end
      object subject: TEdit
        Left = 166
        Top = 157
        Width = 385
        Height = 23
        TabOrder = 1
      end
      object bcc: TEdit
        Left = 166
        Top = 117
        Width = 385
        Height = 23
        TabOrder = 2
      end
      object cc: TEdit
        Left = 166
        Top = 77
        Width = 385
        Height = 23
        TabOrder = 3
      end
      object sendTo: TEdit
        Left = 166
        Top = 29
        Width = 385
        Height = 23
        TabOrder = 4
      end
      object emailBody: TMemo
        Left = 3
        Top = 229
        Width = 785
        Height = 172
        Lines.Strings = (
          'emailBody')
        TabOrder = 5
      end
      object attachments: TListBox
        Left = 95
        Top = 431
        Width = 522
        Height = 62
        ItemHeight = 15
        TabOrder = 6
      end
      object addAttachment: TButton
        Left = 95
        Top = 510
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 7
        OnClick = addAttachmentClick
      end
      object removeAttachment: TButton
        Left = 176
        Top = 510
        Width = 75
        Height = 25
        Caption = 'Remove'
        TabOrder = 8
        OnClick = removeAttachmentClick
      end
      object sendEmail: TButton
        Left = 640
        Top = 432
        Width = 89
        Height = 89
        Caption = 'Send'
        TabOrder = 9
        OnClick = sendEmailClick
      end
    end
  end
  object gmail: TcmGMail
    SSLCertStore = 'MY'
    OnMessageInfo = gmailMessageInfo
    Left = 784
  end
  object OpenDialog1: TOpenDialog
    Left = 752
  end
end


