class ZCX_EXPORT_WHERE_CLAUSE_INVALI definition
  public
  inheriting from ZCX_EXPORT_ERROR
  final
  create public .

public section.

  constants:
    BEGIN OF ZCX_EXPORT_WHERE_CLAUSE_INVALI,
    msgid TYPE symsgid VALUE 'ZEXPORT',
    msgno TYPE symsgno VALUE '008',
    attr1 TYPE scx_attrname VALUE 'TABLE',
    attr2 TYPE scx_attrname VALUE 'WHERE_CLAUSE',
    attr3 TYPE scx_attrname VALUE 'FAILURE_DESCRIPTION',
    attr4 TYPE scx_attrname VALUE '',
  END OF ZCX_EXPORT_WHERE_CLAUSE_INVALI .
  data TABLE type TABNAME .
  data WHERE_CLAUSE type STRING .
  data failure_description type STRING.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional
      !TABLE type TABNAME
      !WHERE_CLAUSE type STRING
      failure_description type STRING.
protected section.
private section.
ENDCLASS.



CLASS ZCX_EXPORT_WHERE_CLAUSE_INVALI IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
.
me->TABLE = TABLE .
me->WHERE_CLAUSE = WHERE_CLAUSE .
me->FAILURE_DESCRIPTION = FAILURE_DESCRIPTION .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_EXPORT_WHERE_CLAUSE_INVALI .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
