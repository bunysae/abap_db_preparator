class ZIMPORT_BUNDLE definition
  public
  abstract
  create public .

public section.

  data:
    TABLE_LIST type standard table of ZEXPORT_TABLE_LIST read-only .
  types:
    index_table type standard table of i.

  methods REPLACE_CONTENT_ALL_TABLES
  abstract
    raising
      ZCX_IMPORT_ERROR .
  methods REPLACE_CONTENT_COMPLETLY
  abstract
    raising
      ZCX_IMPORT_ERROR .
  methods ADD_CONTENT_ALL_TABLES
  abstract
    raising
      ZCX_IMPORT_ERROR .
    "! 7.51 feature, for backwards-compatibility uses dynamic method calls
  methods ACTIVATE_OSQL_REPLACEMENT
    raising
      ZCX_IMPORT_ERROR .
  "! Indicies relate to the attribute "table_list"
  methods GET_CHANGED_SOURCE_TABLES
    exporting
      INDICIES type INDEX_TABLE
    raising
      ZCX_IMPORT_ERROR .
protected section.

  types:
    WHITELIST type range of TABNAME .

  methods GET_EXPORTED_CONTENT
        abstract
    importing
      !TABLE_CONJUNCTION type ZEXPORT_TABLE_LIST
    exporting
      !CONTENT           type ref to DATA
    raising
      ZCX_IMPORT_ERROR .
  methods PERMISSION_IS_GRANTED
    raising
      ZCX_IMPORT_NOT_ALLOWED .
  methods GET_WHITELIST
    returning
      value(RESULT) type WHITELIST .
private section.

  types:
    begin of REPLACED_TABLE,
      SOURCE type TABNAME,
      TARGET type STRING,
    end of REPLACED_TABLE .
  types:
    REPLACED_TABLES type sorted table of REPLACED_TABLE with unique key SOURCE .

  methods COMPARE
    importing
      LHS type standard table
      RHS type standard table
    returning value(differs) type abap_bool.
ENDCLASS.



CLASS ZIMPORT_BUNDLE IMPLEMENTATION.


METHOD activate_osql_replacement.
  DATA: replaced_tables TYPE replaced_tables,
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


  method COMPARE.
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

  endmethod.


  METHOD get_changed_source_tables.
    DATA: actual_content TYPE REF TO data,
          exported_content TYPE REF TO data.
    FIELD-SYMBOLS: <actual_content> TYPE STANDARD TABLE,
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
      SELECT * FROM (table_conjunction->*-source_table)
        INTO TABLE @<actual_content>
        WHERE (table_conjunction->*-where_restriction).

      IF compare( lhs = <exported_content> rhs = <actual_content> ) = abap_true.
        APPEND idx TO indicies.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  method GET_WHITELIST.

    SELECT sign, opti AS option, low, high FROM tvarvc
      INTO CORRESPONDING FIELDS OF TABLE @result
      WHERE name = 'ZIMPORT_REPLACE_WHITELIST' AND type = 'S'.

  endmethod.


  method PERMISSION_IS_GRANTED.

    DATA(aunit_setup) = cl_aunit_customizing=>get_setup( ).
    IF aunit_setup-client-max_risk_level >= if_aunit_attribute_enums=>c_risk_level-dangerous
      AND aunit_setup-client-deny_execution = abap_false.

      RETURN.

    ENDIF.

    RAISE EXCEPTION TYPE zcx_import_not_allowed.

  endmethod.
ENDCLASS.
