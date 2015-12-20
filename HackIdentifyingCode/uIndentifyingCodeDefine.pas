unit uIndentifyingCodeDefine;

interface

uses
  System.Classes;

type
  TGetCodeByURLCallBack = procedure (const AURL: PAnsiChar; AURLLength: Integer;
    const ACode: PAnsiChar; ACodeLength: Integer; ASuccess: Boolean) of object;


  PHackResultRecord = ^THackResultRecord;
  THackResultRecord = record
    Rates: array [0..3,0..9] of Integer;
  end;

implementation

end.
