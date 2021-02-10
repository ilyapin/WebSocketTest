object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 290
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 8
    Top = 70
    Width = 425
    Height = 75
    Lines.Strings = (
      'Json to send')
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 8
    Top = 158
    Width = 425
    Height = 124
    TabOrder = 1
  end
  object btnRun: TButton
    Left = 448
    Top = 8
    Width = 75
    Height = 25
    Action = actRun
    TabOrder = 2
  end
  object cbUrl: TComboBox
    Left = 8
    Top = 8
    Width = 361
    Height = 21
    TabOrder = 3
    Text = 'wss://ct.ma.ru:9872'
    Items.Strings = (
      'wss://echo.websocket.org'
      'wss://cw.ma.ru:9872'
      'wss://ct.ma.ru:9872')
  end
  object btnAuthorize: TButton
    Left = 8
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Authorize'
    TabOrder = 4
    OnClick = btnAuthorizeClick
  end
  object btnStop: TButton
    Left = 448
    Top = 39
    Width = 75
    Height = 25
    Action = actStop
    TabOrder = 5
  end
  object btnSend: TButton
    Left = 448
    Top = 70
    Width = 75
    Height = 25
    Action = actSend
    TabOrder = 6
  end
  object btnStatus: TButton
    Left = 89
    Top = 35
    Width = 75
    Height = 25
    Caption = 'Status'
    TabOrder = 7
    OnClick = btnStatusClick
  end
  object ActionList1: TActionList
    OnUpdate = ActionList1Update
    Left = 472
    Top = 208
    object actRun: TAction
      Caption = 'Run'
      OnExecute = actRunExecute
    end
    object actStop: TAction
      Caption = 'Stop'
      OnExecute = actStopExecute
    end
    object actSend: TAction
      Caption = 'Send'
      OnExecute = actSendExecute
    end
  end
end
