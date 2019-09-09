class ZCX_EXPORT_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  data MSGV1 type SYMSGV .
  data MSGV2 type SYMSGV .
  data MSGV3 type SYMSGV .
  data MSGV4 type SYMSGV .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional .
  class-methods WRAP_ECATT_FAILURE
    importing
      !ECATT_FAILURE type ref to CX_ECATT_TDC_ACCESS
    raising
      ZCX_EXPORT_ERROR .
  class-methods WRAP_T100_MESSAGE
    RAISING
      zcx_export_error.

  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_EXPORT_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  method IF_MESSAGE~GET_LONGTEXT.

    IF previous IS BOUND.
      result = previous->if_message~get_longtext( preserve_newlines ).
    ELSE.
      result = super->if_message~get_longtext( preserve_newlines ).
    ENDIF.

  endmethod.


  method IF_MESSAGE~GET_TEXT.

    IF previous IS BOUND.
      result = previous->if_message~get_text( ).
    ELSE.
      result = super->if_message~get_text( ).
    ENDIF.

  endmethod.


  method WRAP_ECATT_FAILURE.

    RAISE EXCEPTION TYPE zcx_export_error
      EXPORTING
        previous = ecatt_failure.

  endmethod.


  method WRAP_T100_MESSAGE.
    DATA: textid TYPE scx_t100key.

    textid = VALUE #( msgid = sy-msgid msgno = sy-msgno
      attr1 = 'MSGV1' attr2 = 'MSGV2' attr3 = 'MSGV3'
      attr4 = 'MSGV4' ).

    RAISE EXCEPTION TYPE zcx_export_error
      EXPORTING
        textid = textid
        msgv1 = sy-msgv1
        msgv2 = sy-msgv2
        msgv3 = sy-msgv3
        msgv4 = sy-msgv4.

  endmethod.
ENDCLASS.
