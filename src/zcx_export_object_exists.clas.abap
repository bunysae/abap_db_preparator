class ZCX_EXPORT_OBJECT_EXISTS definition
  public
  inheriting from ZCX_EXPORT_ERROR
  final
  create public .

public section.

  constants:
    BEGIN OF zcx_export_object_exists,
    msgid TYPE symsgid VALUE 'ZEXPORT',
    msgno TYPE symsgno VALUE '000',
    attr1 TYPE scx_attrname VALUE 'TESTCASE_ID',
    attr2 TYPE scx_attrname VALUE '',
    attr3 TYPE scx_attrname VALUE '',
    attr4 TYPE scx_attrname VALUE '',
  END OF zcx_export_object_exists .
  constants:
    BEGIN OF tdc_exists,
    msgid TYPE symsgid VALUE 'ZEXPORT',
    msgno TYPE symsgno VALUE '007',
    attr1 TYPE scx_attrname VALUE 'TDC_NAME',
    attr2 TYPE scx_attrname VALUE '',
    attr3 TYPE scx_attrname VALUE '',
    attr4 TYPE scx_attrname VALUE '',
  END OF tdc_exists .
  data TESTCASE_ID type W3OBJID .
  data TDC_NAME type ETOBJ_NAME .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional
      !TESTCASE_ID type W3OBJID optional
      !TDC_NAME type ETOBJ_NAME optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_EXPORT_OBJECT_EXISTS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
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
