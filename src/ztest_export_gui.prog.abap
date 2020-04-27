*&---------------------------------------------------------------------*
*&  Include           ZTEST_EXPORT_GUI
*&---------------------------------------------------------------------*
CLASS test_export_tdc DEFINITION FOR TESTING DURATION SHORT
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    CONSTANTS: tdc_name TYPE etobj_name VALUE 'ZEXPORT_UNIT_TEST'.
    TYPES: _export_ut1 TYPE STANDARD TABLE OF zexport_ut1,
           _export_ut2 TYPE STANDARD TABLE OF zexport_ut2.

    METHODS setup
      RAISING cx_static_check.

    METHODS setup_tables.

    METHODS change_tables.

    METHODS export_in_new_tdc FOR TESTING
      RAISING cx_static_check.

    METHODS reexport_in_existing_tdc FOR TESTING
      RAISING cx_static_check.

    METHODS tdc_params_equals
      IMPORTING
        exp_table_list TYPE _table_list
        exp_export_ut1 TYPE _export_ut1
        exp_export_ut2 TYPE _export_ut2
      RAISING
        cx_static_check.

ENDCLASS.

CLASS test_export_tdc IMPLEMENTATION.

  METHOD setup.

    TRY.
        cl_apl_ecatt_tdc_api=>delete_tdc( i_name = tdc_name ).
        ##NO_HANDLER
      CATCH cx_ecatt_tdc_access.
    ENDTRY.
    header_tdc = VALUE #( name = tdc_name version = 1 variant = 'ECATTDEFAULT'
      package = '$TMP' overwrite = abap_true ).
    bundle = VALUE #(
      ( name = 'ZEXPORT_UT1' overwrite = overwrite_option-yes )
      ( name = 'ZEXPORT_UT2' overwrite = overwrite_option-yes )
    ).
    setup_tables( ).
    PERFORM export_screen_0002.

  ENDMETHOD.

  METHOD setup_tables.
    DATA: export_ut1 TYPE zexport_ut1,
          export_ut2 TYPE zexport_ut2.

    DELETE FROM: zexport_ut1, zexport_ut2.

    export_ut1 = VALUE #( primary_key = 'AAA' content = 'char' ).
    ##LITERAL
    export_ut2 = VALUE #( primary_key = 'AAA' content = '130' ).

    INSERT zexport_ut1 FROM export_ut1.
    INSERT zexport_ut2 FROM export_ut2.

  ENDMETHOD.

  METHOD change_tables.
    DATA: export_ut1 TYPE zexport_ut1,
          export_ut2 TYPE zexport_ut2.

    DELETE FROM: zexport_ut1, zexport_ut2.

    export_ut1 = VALUE #( primary_key = 'TTT' content = 'char' ).
    ##LITERAL
    export_ut2 = VALUE #( primary_key = 'TTT' content = '130' ).

    INSERT zexport_ut1 FROM export_ut1.
    INSERT zexport_ut2 FROM export_ut2.

  ENDMETHOD.

  METHOD export_in_new_tdc.
    DATA: exp_table_list TYPE _table_list,
          exp_export_ut1 TYPE _export_ut1,
          exp_export_ut2 TYPE _export_ut2.

    exp_table_list = VALUE #(
      ( source_table = 'ZEXPORT_UT1' fake_table = 'ZEXPORT_UT1'
        tdc_parameter_name = 'ZEXPORT_UT1' )
      ( source_table = 'ZEXPORT_UT2' fake_table = 'ZEXPORT_UT2'
        tdc_parameter_name = 'ZEXPORT_UT2' )
    ).
    exp_export_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
    ).
    exp_export_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 130 )
    ).

    " when: is done in setup
    " then
    tdc_params_equals( exp_table_list = exp_table_list
      exp_export_ut1 = exp_export_ut1 exp_export_ut2 = exp_export_ut2 ).

  ENDMETHOD.

  METHOD reexport_in_existing_tdc.
    DATA: exp_table_list TYPE _table_list,
          exp_export_ut1 TYPE _export_ut1,
          exp_export_ut2 TYPE _export_ut2.

    exp_table_list = VALUE #(
      ( source_table = 'ZEXPORT_UT1' fake_table = 'ZEXPORT_UT1'
        tdc_parameter_name = 'ZEXPORT_UT1' )
      ( source_table = 'ZEXPORT_UT2' fake_table = 'ZEXPORT_UT2'
        tdc_parameter_name = 'ZEXPORT_UT2' )
    ).
    exp_export_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'TTT' content = 'char' )
    ).
    exp_export_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 130 )
    ).

    " given
    change_tables( ).
    bundle = VALUE #(
      ( name = 'ZEXPORT_UT1' overwrite = overwrite_option-yes )
      ( name = 'ZEXPORT_UT2' overwrite = overwrite_option-no )
    ).

    " when
    PERFORM export_screen_0002.

    " then
    tdc_params_equals( exp_table_list = exp_table_list
      exp_export_ut1 = exp_export_ut1 exp_export_ut2 = exp_export_ut2 ).

  ENDMETHOD.

  METHOD tdc_params_equals.
    DATA: act_table_list TYPE _table_list,
          act_export_ut1 TYPE _export_ut1,
          act_export_ut2 TYPE _export_ut2.

    DATA(tdc) = cl_apl_ecatt_tdc_api=>get_instance( EXPORTING
      i_testdatacontainer = tdc_name
      i_testdatacontainer_version = 1 ).

    tdc->get_value( EXPORTING i_param_name = 'ZEXPORT_TABLE_LIST'
      i_variant_name = 'ECATTDEFAULT'
      CHANGING e_param_value = act_table_list ).
    cl_abap_unit_assert=>assert_equals( exp = exp_table_list
      act = act_table_list ).

    tdc->get_value( EXPORTING i_param_name = 'ZEXPORT_UT1'
      i_variant_name = 'ECATTDEFAULT'
      CHANGING e_param_value = act_export_ut1 ).
    cl_abap_unit_assert=>assert_equals( exp = exp_export_ut1
      act = act_export_ut1 ).

    tdc->get_value( EXPORTING i_param_name = 'ZEXPORT_UT2'
      i_variant_name = 'ECATTDEFAULT'
      CHANGING e_param_value = act_export_ut2 ).
    cl_abap_unit_assert=>assert_equals( exp = exp_export_ut2
      act = act_export_ut2 ).

  ENDMETHOD.

ENDCLASS.

CLASS test_export_cluster DEFINITION FOR TESTING DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS: testcase_id TYPE w3objid VALUE 'ZBUNDLE_UNIT_TEST'.
    TYPES: _export_ut1 TYPE STANDARD TABLE OF zexport_ut1,
           _export_ut2 TYPE STANDARD TABLE OF zexport_ut2.

    METHODS setup.

    METHODS setup_mime.

    METHODS setup_tables.

    METHODS change_tables.

    METHODS export_in_new_cluster FOR TESTING
      RAISING cx_static_check.

    METHODS reexport_in_cluster FOR TESTING
      RAISING cx_static_check.

    METHODS cluster_equals
      IMPORTING
        exp_table_list TYPE _table_list
        exp_export_ut1 TYPE _export_ut1
        exp_export_ut2 TYPE _export_ut2
      RAISING
        cx_static_check.

ENDCLASS.

CLASS test_export_cluster IMPLEMENTATION.

  METHOD setup.

    setup_mime( ).
    header_cluster = VALUE #( testcase_id = testcase_id package = '$TMP'
      overwrite = abap_true ).
    bundle = VALUE #(
      ( name = 'ZEXPORT_UT1' overwrite = overwrite_option-yes )
      ( name = 'ZEXPORT_UT2' overwrite = overwrite_option-yes )
    ).
    setup_tables( ).
    PERFORM export_screen_0001.

  ENDMETHOD.

  METHOD setup_mime.
    DATA: mime_key TYPE wwwdatatab.

    mime_key-relid = 'MI'.
    mime_key-objid = testcase_id.

    CALL FUNCTION 'WWWDATA_DELETE'
      EXPORTING
        key = mime_key
      EXCEPTIONS
        OTHERS = 0.

  ENDMETHOD.

  METHOD setup_tables.
    DATA: export_ut1 TYPE zexport_ut1,
          export_ut2 TYPE zexport_ut2.

    DELETE FROM: zexport_ut1, zexport_ut2.

    export_ut1 = VALUE #( primary_key = 'AAA' content = 'char' ).
    ##LITERAL
    export_ut2 = VALUE #( primary_key = 'AAA' content = '130' ).

    INSERT zexport_ut1 FROM export_ut1.
    INSERT zexport_ut2 FROM export_ut2.

  ENDMETHOD.

  METHOD change_tables.
    DATA: export_ut1 TYPE zexport_ut1,
          export_ut2 TYPE zexport_ut2.

    DELETE FROM: zexport_ut1, zexport_ut2.

    export_ut1 = VALUE #( primary_key = 'TTT' content = 'char' ).
    ##LITERAL
    export_ut2 = VALUE #( primary_key = 'TTT' content = '130' ).

    INSERT zexport_ut1 FROM export_ut1.
    INSERT zexport_ut2 FROM export_ut2.

  ENDMETHOD.

  METHOD export_in_new_cluster.
    DATA: exp_table_list TYPE _table_list,
          exp_export_ut1 TYPE _export_ut1,
          exp_export_ut2 TYPE _export_ut2.

    exp_table_list = VALUE #(
      ( source_table = 'ZEXPORT_UT1' fake_table = 'ZEXPORT_UT1' )
      ( source_table = 'ZEXPORT_UT2' fake_table = 'ZEXPORT_UT2' )
    ).
    exp_export_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
    ).
    exp_export_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 130 )
    ).

    " when: is done in setup
    " then
    cluster_equals( exp_table_list = exp_table_list exp_export_ut1 = exp_export_ut1
      exp_export_ut2 = exp_export_ut2 ).

  ENDMETHOD.

  METHOD reexport_in_cluster.
    DATA: exp_table_list TYPE _table_list,
          exp_export_ut1 TYPE _export_ut1,
          exp_export_ut2 TYPE _export_ut2.

    exp_table_list = VALUE #(
      ( source_table = 'ZEXPORT_UT1' fake_table = 'ZEXPORT_UT1' )
      ( source_table = 'ZEXPORT_UT2' fake_table = 'ZEXPORT_UT2' )
    ).
    exp_export_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'TTT' content = 'char' )
    ).
    exp_export_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 130 )
    ).

    " given
    change_tables( ).
    bundle = VALUE #(
      ( name = 'ZEXPORT_UT1' overwrite = overwrite_option-yes )
      ( name = 'ZEXPORT_UT2' overwrite = overwrite_option-no )
    ).

    " when
    PERFORM export_screen_0001.

    " then
    cluster_equals( exp_table_list = exp_table_list exp_export_ut1 = exp_export_ut1
      exp_export_ut2 = exp_export_ut2 ).

  ENDMETHOD.

  METHOD cluster_equals.
    DATA: act_export_ut1 TYPE REF TO data,
          act_export_ut2 TYPE REF TO data.
    FIELD-SYMBOLS: <act_export_ut1> TYPE _export_ut1,
                   <act_export_ut2> TYPE _export_ut2.

    DATA(importer) = NEW zimport_bundle_from_cluster( testcase_id = testcase_id ).

    CREATE DATA: act_export_ut1 TYPE _export_ut1,
      act_export_ut2 TYPE _export_ut2.

    cl_abap_unit_assert=>assert_equals( exp = exp_table_list
      act = importer->table_list ).

    importer->get_exported_content_for_table( EXPORTING source_table = 'ZEXPORT_UT1'
      IMPORTING content = act_export_ut1 ).
    ASSIGN act_export_ut1->* TO <act_export_ut1>.
    cl_abap_unit_assert=>assert_equals( exp = exp_export_ut1
      act = <act_export_ut1> ).

    importer->get_exported_content_for_table( EXPORTING source_table = 'ZEXPORT_UT2'
      IMPORTING content = act_export_ut2 ).
    ASSIGN act_export_ut2->* TO <act_export_ut2>.
    cl_abap_unit_assert=>assert_equals( exp = exp_export_ut2
      act = <act_export_ut2> ).

  ENDMETHOD.

ENDCLASS.
