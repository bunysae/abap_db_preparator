class ZCX_EXPORT_TO_FILE definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create private .

public section.

  CONSTANTS: BEGIN OF zcx_export_to_file,
    msgid TYPE symsgid VALUE 'ZEXPORT',
    msgno TYPE symsgno VALUE '006',
    attr1 TYPE scx_attrname VALUE 'FILENAME',
    attr2 TYPE scx_attrname VALUE 'REASON',
    attr3 TYPE scx_attrname VALUE '',
    attr4 TYPE scx_attrname VALUE '',
  END OF zcx_export_to_file.

  INTERFACES if_t100_message.
  DATA: filename TYPE string,
        reason TYPE string.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      filename TYPE string
      reason TYPE string.
  class-methods DOWNLOAD_FAILED
    importing
      !FILENAME type STRING
      VALUE(SUBRC) type SYST-SUBRC
    RAISING
      zcx_export_to_file.
protected section.
private section.

  TYPES: BEGIN OF _return_code_map,
    subrc TYPE i,
    reason TYPE string,
  END OF _return_code_map.
  CLASS-DATA: return_code_map TYPE STANDARD TABLE OF _return_code_map.

  CLASS-METHODS initialize.
ENDCLASS.



CLASS ZCX_EXPORT_TO_FILE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->FILENAME = FILENAME .
me->REASON = REASON .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_EXPORT_TO_FILE .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  method DOWNLOAD_FAILED.
    DATA: reason TYPE string.

    initialize( ).

    READ TABLE return_code_map REFERENCE INTO DATA(ret)
      WITH KEY subrc = subrc.
    IF sy-subrc = 0.
      reason = ret->*-reason.
    ELSE.
      reason = subrc.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_export_to_file
      EXPORTING
        filename = filename
        reason = reason.

  endmethod.


  method INITIALIZE.

    return_code_map = VALUE #(
      ( subrc = 2 reason = text-002 )
      ( subrc = 4 reason = text-004 )
      ( subrc = 6 reason = text-006 )
      ( subrc = 8 reason = text-008 )
      ( subrc = 10 reason = text-010 )
    ).

  endmethod.
ENDCLASS.
