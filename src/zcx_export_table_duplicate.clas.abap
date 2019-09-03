class ZCX_EXPORT_TABLE_DUPLICATE definition
  public
  inheriting from ZCX_EXPORT_ERROR
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .
  CONSTANTS: BEGIN OF zcx_export_table_duplicate,
    msgid TYPE symsgid VALUE 'ZEXPORT',
    msgno TYPE symsgno VALUE '003',
    attr1 TYPE scx_attrname VALUE 'TABLE',
    attr2 TYPE scx_attrname VALUE '',
    attr3 TYPE scx_attrname VALUE '',
    attr4 TYPE scx_attrname VALUE '',
  END OF zcx_export_table_duplicate.
  DATA table TYPE tabname.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      table TYPE tabname.
protected section.
private section.
ENDCLASS.



CLASS ZCX_EXPORT_TABLE_DUPLICATE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TABLE = TABLE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_EXPORT_TABLE_DUPLICATE .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
