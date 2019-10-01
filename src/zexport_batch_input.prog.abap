CLASS batch_input DEFINITION.

  PUBLIC SECTION.

    METHODS dynpro_field
      IMPORTING
        name   TYPE clike
        fvalue TYPE any.

    METHODS dynpro_head
      IMPORTING
        report TYPE syst-repid
        dynpro TYPE syst-dynnr.

    METHODS call_transaction
      IMPORTING code    TYPE tcode
                VALUE(options) TYPE ctu_params OPTIONAL.

    METHODS command
      IMPORTING command TYPE syst-ucomm.

    METHODS subscreen
      IMPORTING subscreen TYPE bdc_fval.

  PROTECTED SECTION.
    DATA: input_map TYPE STANDARD TABLE OF bdcdata.

ENDCLASS.

CLASS batch_input IMPLEMENTATION.

  METHOD dynpro_field.
    DATA: conv_exit TYPE tfdir-funcname.

    APPEND INITIAL LINE TO input_map ASSIGNING FIELD-SYMBOL(<inp>).
    <inp>-fnam = name.

    DESCRIBE FIELD fvalue EDIT MASK conv_exit.

    IF conv_exit <> space.
      WRITE fvalue TO <inp>-fval USING EDIT MASK conv_exit.
    ELSE.
      ##WRITE_MOVE
      WRITE fvalue TO <inp>-fval.
    ENDIF.

  ENDMETHOD.

  METHOD dynpro_head.

    APPEND INITIAL LINE TO input_map ASSIGNING FIELD-SYMBOL(<inp>).

    <inp>-program = report.
    <inp>-dynpro = dynpro.
    <inp>-dynbegin = abap_true.

  ENDMETHOD.

  METHOD command.

    APPEND INITIAL LINE TO input_map ASSIGNING FIELD-SYMBOL(<inp>).
    <inp>-fnam = 'BDC_OKCODE'.
    <inp>-fval = command.

  ENDMETHOD.

  METHOD subscreen.

    APPEND INITIAL LINE TO input_map ASSIGNING FIELD-SYMBOL(<inp>).
    <inp>-fnam = 'BDC_SUBSCR'.
    <inp>-fval = subscreen.

  ENDMETHOD.

  METHOD call_transaction.

    CALL TRANSACTION code WITH AUTHORITY-CHECK USING input_map
      OPTIONS FROM options.

  ENDMETHOD.

ENDCLASS.
