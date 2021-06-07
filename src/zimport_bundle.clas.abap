CLASS zimport_bundle DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      index_table TYPE STANDARD TABLE OF i .

    DATA:
      table_list TYPE STANDARD TABLE OF zexport_table_list READ-ONLY .

    METHODS replace_content_all_tables
          ABSTRACT
      RAISING
        zcx_import_error .
    METHODS replace_content_completly
          ABSTRACT
      RAISING
        zcx_import_error .
    METHODS add_content_all_tables
          ABSTRACT
      RAISING
        zcx_import_error .
    "! 7.51 feature, for backwards-compatibility uses dynamic method calls
    METHODS activate_osql_replacement
      RAISING
        zcx_import_error .
    "! Indicies relate to the attribute "table_list"
    METHODS get_changed_source_tables
      EXPORTING
        !indicies TYPE index_table
      RAISING
        zcx_import_error .
    METHODS source_table_has_changed
      IMPORTING
        !table_conjunction TYPE zexport_table_list
      RETURNING
        VALUE(has_changed) TYPE abap_bool
      RAISING
        zcx_import_error .
    METHODS prevent_commit_work .
  PROTECTED SECTION.

    TYPES:
      whitelist TYPE RANGE OF tabname .

    METHODS get_exported_content
          ABSTRACT
      IMPORTING
        !table_conjunction TYPE zexport_table_list
      EXPORTING
        !content           TYPE REF TO data
      RAISING
        zcx_import_error .
    METHODS permission_is_granted
      RAISING
        zcx_import_not_allowed .
    METHODS get_whitelist
      RETURNING
        VALUE(result) TYPE whitelist .
    METHODS delete
      IMPORTING
        table_conjunction TYPE zexport_table_list
      RAISING
        zcx_import_error.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF replaced_table,
        source TYPE tabname,
        target TYPE string,
      END OF replaced_table .
    TYPES:
      replaced_tables TYPE SORTED TABLE OF replaced_table WITH UNIQUE KEY source .

    METHODS compare
      IMPORTING
                lhs            TYPE STANDARD TABLE
                rhs            TYPE STANDARD TABLE
      RETURNING VALUE(differs) TYPE abap_bool.
    METHODS select
      IMPORTING
        table_conjunction TYPE zexport_table_list
        from_fake         TYPE sap_bool OPTIONAL
      EXPORTING
        content           TYPE STANDARD TABLE
      RAISING
        zcx_import_error.
    METHODS select_from_fake
      IMPORTING
        table_conjunction TYPE zexport_table_list
      EXPORTING
        content           TYPE STANDARD TABLE
      RAISING
        zcx_import_error.
ENDCLASS.



CLASS ZIMPORT_BUNDLE IMPLEMENTATION.


  METHOD activate_osql_replacement.
    DATA: replaced_tables  TYPE replaced_tables,
          _replaced_tables TYPE REF TO data.
    FIELD-SYMBOLS: <replaced_tables> TYPE ANY TABLE.

    CREATE DATA _replaced_tables TYPE ('CL_OSQL_REPLACE=>TT_REPLACEMENT').
    ASSIGN _replaced_tables->* TO <replaced_tables>.

    LOOP AT table_list REFERENCE INTO DATA(pair).
      IF pair->*-source_table <> pair->*-fake_table.
        INSERT VALUE #( source = pair->*-source_table target = pair->*-fake_table )
          INTO TABLE replaced_tables.
      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING replaced_tables TO <replaced_tables>.
    CALL METHOD ('CL_OSQL_REPLACE')=>activate_replacement
      EXPORTING
        replacement_table = <replaced_tables>.

  ENDMETHOD.


  METHOD compare.
    FIELD-SYMBOLS: <lhs> TYPE any,
                   <rhs> TYPE any.

    IF lines( lhs ) <> lines( rhs ).
      differs = abap_true.
      RETURN.
    ENDIF.

    " "lhs" and "rhs" may have different sort order
    LOOP AT lhs ASSIGNING <lhs>.
      READ TABLE rhs ASSIGNING <rhs> FROM <lhs>.
      IF sy-subrc = 0.
        IF <lhs> <> <rhs>.
          differs = abap_true.
          RETURN.
        ENDIF.
      ELSE.
        differs = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD delete.
    DATA: content TYPE REF TO data.
    FIELD-SYMBOLS: <content> TYPE STANDARD TABLE.

    CREATE DATA content TYPE STANDARD TABLE OF (table_conjunction-fake_table).
    ASSIGN content->* TO <content>.

    select_from_fake( EXPORTING table_conjunction = table_conjunction
      IMPORTING content = <content> ).
    DELETE (table_conjunction-fake_table) FROM TABLE <content>.

  ENDMETHOD.


  METHOD get_changed_source_tables.
    DATA: actual_content   TYPE REF TO data,
          exported_content TYPE REF TO data.
    FIELD-SYMBOLS: <actual_content>   TYPE STANDARD TABLE,
                   <exported_content> TYPE STANDARD TABLE.

    CLEAR indicies.
    LOOP AT table_list REFERENCE INTO DATA(table_conjunction).

      DATA(idx) = sy-tabix.

      CREATE DATA:
        actual_content TYPE STANDARD TABLE OF (table_conjunction->*-source_table),
        exported_content TYPE STANDARD TABLE OF (table_conjunction->*-source_table).
      ASSIGN actual_content->* TO <actual_content>.

      get_exported_content( EXPORTING table_conjunction = table_conjunction->*
        IMPORTING content = exported_content ).
      ASSIGN exported_content->* TO <exported_content>.
      select( EXPORTING table_conjunction = table_conjunction->*
        IMPORTING content = <actual_content> ).

      IF compare( lhs = <exported_content> rhs = <actual_content> ) = abap_true.
        APPEND idx TO indicies.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_whitelist.

    SELECT sign, opti AS option, low, high FROM tvarvc
      INTO CORRESPONDING FIELDS OF TABLE @result
      WHERE name = 'ZIMPORT_REPLACE_WHITELIST' AND type = 'S'.

  ENDMETHOD.


  METHOD permission_is_granted.

    DATA(aunit_setup) = cl_aunit_customizing=>get_setup( ).
    IF aunit_setup-client-max_risk_level >= if_aunit_attribute_enums=>c_risk_level-dangerous
      AND aunit_setup-client-deny_execution = abap_false.

      RETURN.

    ENDIF.

    RAISE EXCEPTION TYPE zcx_import_not_allowed.

  ENDMETHOD.


  METHOD prevent_commit_work.

    SET UPDATE TASK LOCAL.
    CALL FUNCTION 'ZIMPORT_PREVENT_COMMIT_WORK'
      IN UPDATE TASK.

  ENDMETHOD.


  METHOD select.
    DATA: table_for_all_entries TYPE REF TO data.

    TRY.
        zexport_utils=>get_table_for_all_entries( EXPORTING
          table_conjunction = table_conjunction
          RECEIVING table_name = DATA(table_name)
          EXCEPTIONS not_for_all_entries_cond = 4 ).
        IF sy-subrc = 0.

          READ TABLE table_list REFERENCE INTO DATA(foe_conjunction)
            WITH KEY fake_table = table_name+1.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_export_invalid_name
              EXPORTING
                name = table_name.
          ENDIF.

          CREATE DATA table_for_all_entries TYPE STANDARD TABLE OF (table_name+1).
          get_exported_content( EXPORTING table_conjunction = foe_conjunction->*
            IMPORTING content = table_for_all_entries ).
          ASSIGN table_for_all_entries->* TO FIELD-SYMBOL(<table_for_all_entries>).
          zexport_utils=>select( EXPORTING table_for_all_entries = <table_for_all_entries>
            table_conjunction = table_conjunction table_name = table_name
            select_from_fake = from_fake
            IMPORTING result = content ).

        ELSE.

          DATA(data_source) = COND tabname( WHEN from_fake = abap_true
            THEN table_conjunction-fake_table ELSE table_conjunction-source_table ).
          SELECT * FROM (data_source) INTO TABLE @content
            WHERE (table_conjunction-where_restriction).

        ENDIF.
      CATCH zcx_export_error INTO DATA(exc).
        RAISE EXCEPTION TYPE zcx_import_error
          EXPORTING
            previous = exc.
    ENDTRY.

  ENDMETHOD.


  METHOD select_from_fake.

    select( EXPORTING table_conjunction = table_conjunction
      from_fake = abap_true
      IMPORTING content = content ).

  ENDMETHOD.


  METHOD source_table_has_changed.
    DATA: actual_content   TYPE REF TO data,
          exported_content TYPE REF TO data.
    FIELD-SYMBOLS: <actual_content>   TYPE STANDARD TABLE,
                   <exported_content> TYPE STANDARD TABLE.

    CREATE DATA:
        actual_content TYPE STANDARD TABLE OF (table_conjunction-source_table),
        exported_content TYPE STANDARD TABLE OF (table_conjunction-source_table).
    ASSIGN actual_content->* TO <actual_content>.

    get_exported_content( EXPORTING table_conjunction = table_conjunction
        IMPORTING content = exported_content ).
    ASSIGN exported_content->* TO <exported_content>.
    select( EXPORTING table_conjunction = table_conjunction
      IMPORTING content = <actual_content> ).

    has_changed = compare( lhs = <exported_content> rhs = <actual_content> ).

  ENDMETHOD.
ENDCLASS.
