CLASS zcx_export_invalid_name DEFINITION
  PUBLIC
  INHERITING FROM zcx_export_error
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF zcx_export_invalid_name,
        msgid TYPE symsgid VALUE 'ZEXPORT',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_export_invalid_name .
    DATA name TYPE tabname.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE symsgv OPTIONAL
        !msgv2    TYPE symsgv OPTIONAL
        !msgv3    TYPE symsgv OPTIONAL
        !msgv4    TYPE symsgv OPTIONAL
        !name     TYPE tabname.
protected section.
private section.
ENDCLASS.



CLASS ZCX_EXPORT_INVALID_NAME IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MSGV1 = MSGV1
MSGV2 = MSGV2
MSGV3 = MSGV3
MSGV4 = MSGV4
.
me->NAME = NAME .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_EXPORT_INVALID_NAME .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
