class ZCX_EXPORT_NAME_CHANGED definition
  public
  inheriting from ZCX_EXPORT_ERROR
  create public .

public section.

  constants: begin of zcx_export_name_changed,
    msgid type symsgid value 'ZEXPORT',
    msgno type symsgno value '011',
    attr1 type scx_attrname value 'TABLE_NAME',
    attr2 type scx_attrname value '',
    attr3 type scx_attrname value '',
    attr4 type scx_attrname value '',
  end of zcx_export_name_changed.
  data table_name type tabname.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      msgv1 TYPE symsgv
      msgv2 TYPE symsgv
      msgv3 type symsgv
      msgv4 type symsgv
      table_name type tabname.
protected section.
private section.
ENDCLASS.



CLASS ZCX_EXPORT_NAME_CHANGED IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
.
me->TABLE_NAME = TABLE_NAME .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_EXPORT_NAME_CHANGED .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
