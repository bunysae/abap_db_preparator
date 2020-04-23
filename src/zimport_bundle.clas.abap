class ZIMPORT_BUNDLE definition
  public
  abstract
  create public .

public section.

  data:
    table_list TYPE STANDARD TABLE OF zexport_table_list read-only .

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
protected section.

  types:
    whitelist type range of tabname .

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
