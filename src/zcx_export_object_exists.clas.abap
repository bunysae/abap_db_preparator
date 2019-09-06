class ZCX_EXPORT_OBJECT_EXISTS definition
  public
  inheriting from zcx_export_error
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .
  CONSTANTS: BEGIN OF zcx_export_object_exists,
    msgid TYPE symsgid VALUE 'ZEXPORT',
    msgno TYPE symsgno VALUE '000',
    attr1 TYPE scx_attrname VALUE 'TESTCASE_ID',
    attr2 TYPE scx_attrname VALUE '',
    attr3 TYPE scx_attrname VALUE '',
    attr4 TYPE scx_attrname VALUE '',
  END OF zcx_export_object_exists.
  CONSTANTS: BEGIN OF tdc_exists,
    msgid TYPE symsgid VALUE 'ZEXPORT',
    msgno TYPE symsgno VALUE '007',
    attr1 TYPE scx_attrname VALUE 'TDC_NAME',
    attr2 TYPE scx_attrname VALUE '',
    attr3 TYPE scx_attrname VALUE '',
    attr4 TYPE scx_attrname VALUE '',
  END OF tdc_exists.
  DATA testcase_id TYPE w3objid.
  DATA tdc_name TYPE etobj_name.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      testcase_id TYPE w3objid OPTIONAL
      tdc_name TYPE etobj_name OPTIONAL.
protected section.
private section.
ENDCLASS.



CLASS ZCX_EXPORT_OBJECT_EXISTS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TESTCASE_ID = TESTCASE_ID .
me->TDC_NAME = TDC_NAME .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_EXPORT_OBJECT_EXISTS .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
