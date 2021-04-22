CLASS zexport_bundle DEFINITION
  PUBLIC
  CREATE PUBLIC
  ABSTRACT.

  PUBLIC SECTION.
  PROTECTED SECTION.
    DATA table_list TYPE STANDARD TABLE OF zexport_table_list .

    METHODS select
      IMPORTING
        table_conjunction TYPE zexport_table_list
      EXPORTING
        content           TYPE STANDARD TABLE
      RAISING
        zcx_export_error
        cx_sy_dynamic_osql_syntax.

    METHODS get_exported_content ABSTRACT
      IMPORTING
        table_conjunction     TYPE zexport_table_list
      CHANGING
        table_for_all_entries TYPE REF TO data
      RAISING
        zcx_export_error.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZEXPORT_BUNDLE IMPLEMENTATION.


  METHOD select.
    DATA: table_for_all_entries TYPE REF TO data.

    zexport_utils=>get_table_for_all_entries( EXPORTING
      table_conjunction = table_conjunction
      RECEIVING table_name = DATA(table_name)
      EXCEPTIONS not_for_all_entries_cond = 4 ).
    IF sy-subrc = 0.

      READ TABLE table_list REFERENCE INTO DATA(foe_conjunction)
        WITH KEY source_table = table_name+1.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_export_invalid_name
          EXPORTING
            name = table_name.
      ENDIF.

      CREATE DATA table_for_all_entries TYPE STANDARD TABLE OF (table_name+1).
      get_exported_content( EXPORTING table_conjunction = foe_conjunction->*
        CHANGING table_for_all_entries = table_for_all_entries ).
      ASSIGN table_for_all_entries->* TO FIELD-SYMBOL(<table_for_all_entries>).
      zexport_utils=>select( EXPORTING table_for_all_entries = <table_for_all_entries>
        table_conjunction = table_conjunction table_name = table_name
        IMPORTING result = content ).

    ELSE.

      SELECT * FROM (table_conjunction-source_table) INTO TABLE @content
        WHERE (table_conjunction-where_restriction).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
