class ZCX_IMPORT_OBJECT_NOT_EXISTS definition
  public
  inheriting from ZCX_IMPORT_ERROR
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_IMPORT_OBJECT_NOT_EXISTS,
      msgid type symsgid value 'ZEXPORT',
      msgno type symsgno value '012',
      attr1 type scx_attrname value 'TESTCASE_ID',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_IMPORT_OBJECT_NOT_EXISTS .
  data TESTCASE_ID type W3OBJID .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !TESTCASE_ID type W3OBJID .
protected section.
private section.
ENDCLASS.



CLASS ZCX_IMPORT_OBJECT_NOT_EXISTS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TESTCASE_ID = TESTCASE_ID .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_IMPORT_OBJECT_NOT_EXISTS .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
