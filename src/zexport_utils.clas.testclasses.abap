CLASS test_utils DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    METHODS get_table_for_all_entries FOR TESTING.

    METHODS select_lower_case FOR TESTING
      RAISING
        cx_static_check.

    METHODS select_upper_case FOR TESTING
      RAISING
        cx_static_check.

ENDCLASS.

CLASS test_utils IMPLEMENTATION.

  METHOD get_table_for_all_entries.

    zexport_utils=>get_table_for_all_entries( EXPORTING
      table_conjunction = VALUE #( where_restriction = `A = 'A'` )
      EXCEPTIONS not_for_all_entries_cond = 4 ).
    cl_abap_unit_assert=>assert_subrc( exp = 4
      msg = 'not for all entries condition' ).

    " lower case
    zexport_utils=>get_table_for_all_entries( EXPORTING
      table_conjunction = VALUE #( where_restriction = `for all entries in mseg where a = 'A'` )
      RECEIVING table_name = DATA(act_table_name)
      EXCEPTIONS not_for_all_entries_cond = 4 ).
    cl_abap_unit_assert=>assert_equals( exp = 'MSEG'
      act = act_table_name ).

    " upper case
    zexport_utils=>get_table_for_all_entries( EXPORTING
      table_conjunction = VALUE #( where_restriction = `FOR ALL ENTRIES IN mseg WHERE A = 'A'` )
      RECEIVING table_name = act_table_name
      EXCEPTIONS not_for_all_entries_cond = 4 ).
    cl_abap_unit_assert=>assert_equals( exp = 'MSEG'
      act = act_table_name ).

  ENDMETHOD.

  METHOD select_lower_case.
    DATA: keys TYPE STANDARD TABLE OF zexport_ut3,
          mockup TYPE STANDARD TABLE OF zexport_ut1,
          act_result TYPE STANDARD TABLE OF zexport_ut1.

    DELETE FROM: zimport_ut1, zexport_ut1.
    keys = VALUE #(
      ( primary_key = 'AA' )
      ( primary_key = 'BB' ) ).
    mockup = VALUE #(
      ( client = sy-mandt primary_key = 'AA' content = 'A' )
      ( client = sy-mandt primary_key = 'AB' content = 'AB' )
      ( client = sy-mandt primary_key = 'BB' content = 'B' ) ).
    INSERT zexport_ut1 FROM TABLE mockup.

    DATA(table_conjunction) = VALUE zexport_table_list(
      source_table = 'ZEXPORT_UT1' fake_table = 'ZIMPORT_UT1'
      where_restriction = 'for all entries in keys where primary_key = keys-primary_key' ).

    zexport_utils=>select( EXPORTING
      table_for_all_entries = keys
      table_conjunction = table_conjunction
      table_name = 'KEYS'
      IMPORTING
        result = act_result ).

    DELETE mockup WHERE primary_key = 'AB'.
    cl_abap_unit_assert=>assert_equals( exp = mockup
      act = act_result ).

  ENDMETHOD.

  METHOD select_upper_case.
    DATA: keys TYPE STANDARD TABLE OF zexport_ut3,
          mockup TYPE STANDARD TABLE OF zexport_ut1,
          act_result TYPE STANDARD TABLE OF zexport_ut1.

    DELETE FROM: zimport_ut1, zexport_ut1.
    keys = VALUE #(
      ( primary_key = 'AA' )
      ( primary_key = 'BB' ) ).
    mockup = VALUE #(
      ( client = sy-mandt primary_key = 'AA' content = 'A' )
      ( client = sy-mandt primary_key = 'AB' content = 'AB' )
      ( client = sy-mandt primary_key = 'BB' content = 'B' ) ).
    INSERT zexport_ut1 FROM TABLE mockup.

    DATA(table_conjunction) = VALUE zexport_table_list(
      source_table = 'ZEXPORT_UT1' fake_table = 'ZIMPORT_UT1'
      where_restriction = 'FOR ALL ENTRIES IN keys WHERE primary_key = keys-primary_key' ).

    zexport_utils=>select( EXPORTING
      table_for_all_entries = keys
      table_conjunction = table_conjunction
      table_name = 'KEYS'
      IMPORTING
        result = act_result ).

    DELETE mockup WHERE primary_key = 'AB'.
    cl_abap_unit_assert=>assert_equals( exp = mockup
      act = act_result ).

  ENDMETHOD.

ENDCLASS.
