class ZCX_IMPORT_OBJECT_NOT_EXISTS definition
  public
  inheriting from zcx_import_error
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .
  CONSTANTS: BEGIN OF zcx_export_object_exists,
    msgid TYPE symsgid VALUE 'ZEXPORT',
    msgno TYPE symsgno VALUE '001',
    attr1 TYPE scx_attrname VALUE 'TESTCASE_ID',
    attr2 TYPE scx_attrname VALUE '',
    attr3 TYPE scx_attrname VALUE '',
    attr4 TYPE scx_attrname VALUE '',
  END OF zcx_export_object_exists.
  DATA testcase_id TYPE w3objid.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      testcase_id TYPE w3objid.
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
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
