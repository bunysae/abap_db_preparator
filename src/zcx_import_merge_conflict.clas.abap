CLASS zcx_import_merge_conflict DEFINITION
  PUBLIC
  INHERITING FROM zcx_import_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .

    CONSTANTS: BEGIN OF zcx_import_merge_conflict,
                 msgid TYPE symsgid VALUE 'ZEXPORT',
                 msgno TYPE symsgno VALUE '003',
                 attr1 TYPE scx_attrname VALUE 'TABLE',
                 attr2 TYPE scx_attrname VALUE '',
                 attr3 TYPE scx_attrname VALUE '',
                 attr4 TYPE scx_attrname VALUE '',
               END OF zcx_import_merge_conflict.
    DATA: table TYPE tabname.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        table     TYPE tabname.
protected section.
private section.
ENDCLASS.



CLASS ZCX_IMPORT_MERGE_CONFLICT IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->TABLE = TABLE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_IMPORT_MERGE_CONFLICT .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
