CLASS zcx_import_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !textid   LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL .
    CLASS-METHODS wrap_ecatt_failure
      IMPORTING
        ecatt_failure TYPE REF TO cx_ecatt_tdc_access
      RAISING
        zcx_import_error.

    METHODS if_message~get_text REDEFINITION.

    METHODS if_message~get_longtext REDEFINITION.
protected section.
private section.
ENDCLASS.



CLASS ZCX_IMPORT_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
  endmethod.


  METHOD if_message~get_longtext.

    IF previous IS BOUND.
      result = previous->if_message~get_longtext( preserve_newlines ).
    ELSE.
      result = super->if_message~get_longtext( preserve_newlines ).
    ENDIF.

  ENDMETHOD.


  method IF_MESSAGE~GET_TEXT.

    IF previous IS BOUND.
      result = previous->if_message~get_text( ).
    ELSE.
      result = super->if_message~get_text( ).
    ENDIF.

  endmethod.


  method WRAP_ECATT_FAILURE.

    RAISE EXCEPTION TYPE zcx_import_error
      EXPORTING
        previous = ecatt_failure.

  endmethod.
ENDCLASS.
