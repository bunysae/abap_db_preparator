class ZCX_EXPORT_WHERE_CLAUSE_INVALI definition
  public
  inheriting from ZCX_EXPORT_ERROR
  final
  create public .

public section.

  CONSTANTS: BEGIN OF ZCX_EXPORT_WHERE_CLAUSE_INVALI,
    msgid TYPE symsgid VALUE 'ZEXPORT',
    msgno TYPE symsgno VALUE '008',
    attr1 TYPE scx_attrname VALUE 'TABLE',
    attr2 TYPE scx_attrname VALUE 'WHERE_CLAUSE',
    attr3 TYPE scx_attrname VALUE '',
    attr4 TYPE scx_attrname VALUE '',
  END OF ZCX_EXPORT_WHERE_CLAUSE_INVALI.
  interfaces IF_T100_MESSAGE .
  DATA table TYPE tabname.
  DATA where_clause TYPE string.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      table TYPE tabname
      where_clause TYPE string.
protected section.
private section.
ENDCLASS.



CLASS ZCX_EXPORT_WHERE_CLAUSE_INVALI IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TABLE = TABLE .
me->WHERE_CLAUSE = WHERE_CLAUSE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_EXPORT_WHERE_CLAUSE_INVALI .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
