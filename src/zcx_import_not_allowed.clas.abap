class ZCX_IMPORT_NOT_ALLOWED definition
  public
  inheriting from ZCX_IMPORT_ERROR
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_IMPORT_NOT_ALLOWED,
      msgid type symsgid value 'ZEXPORT',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_IMPORT_NOT_ALLOWED .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_IMPORT_NOT_ALLOWED IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_IMPORT_NOT_ALLOWED .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
