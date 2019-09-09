class ZCX_EXPORT_TR_ORDER definition
  public
  inheriting from ZCX_EXPORT_ERROR
  final
  create public .

public section.

  CONSTANTS: BEGIN OF zcx_export_tr_order,
    msgid TYPE symsgid VALUE 'ZEXPORT',
    msgno TYPE symsgno VALUE '009',
    attr1 TYPE scx_attrname VALUE 'TR_ORDER',
    attr2 TYPE scx_attrname VALUE '',
    attr3 TYPE scx_attrname VALUE '',
    attr4 TYPE scx_attrname VALUE '',
  END OF zcx_export_tr_order.
  DATA tr_order TYPE e070-trkorr.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional
      tr_order TYPE e070-trkorr.
protected section.
private section.
ENDCLASS.



CLASS ZCX_EXPORT_TR_ORDER IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
.
me->TR_ORDER = TR_ORDER .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_EXPORT_TR_ORDER .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
