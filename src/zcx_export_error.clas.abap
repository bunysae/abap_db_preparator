class ZCX_EXPORT_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
  class-methods WRAP_ECATT_FAILURE
    importing
      !ECATT_FAILURE type ref to CX_ECATT_TDC_ACCESS
    raising
      ZCX_EXPORT_ERROR .

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
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
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
ENDCLASS.
