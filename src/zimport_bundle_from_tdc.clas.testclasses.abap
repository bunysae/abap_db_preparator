CLASS test_export_import DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    DATA: tdc TYPE REF TO cl_apl_ecatt_tdc_api.
    CONSTANTS: tdc_name TYPE etobj_name VALUE 'ZEXPORT_UNIT_TEST'.

    METHODS setup
      RAISING
        cx_static_check.

    METHODS create_cut
      RAISING
        cx_static_check.

    METHODS export
      RAISING
        cx_static_check.

    METHODS setup_tables.

    METHODS import_and_replace FOR TESTING
      RAISING
        cx_static_check.

    METHODS import_and_replace_all FOR TESTING
      RAISING
        cx_static_check.

    METHODS import_and_add FOR TESTING
      RAISING
        cx_static_check.

    METHODS activate_osql_replacement FOR TESTING
      RAISING
        cx_static_check.

ENDCLASS.

CLASS test_export_import IMPLEMENTATION.

  METHOD setup.

    create_cut( ).
    setup_tables( ).
    COMMIT WORK AND WAIT.
    export( ).
    COMMIT WORK AND WAIT.

    DELETE FROM zexport_ut3.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD create_cut.

    TRY.
        cl_apl_ecatt_tdc_api=>delete_tdc( i_name = tdc_name ).
        ##NO_HANDLER
      CATCH cx_ecatt_tdc_access.
    ENDTRY.

    cl_apl_ecatt_tdc_api=>create_tdc( EXPORTING
      i_name = 'ZEXPORT_UNIT_TEST' i_tadir_devclass = '$TMP' i_write_access = abap_true
      IMPORTING e_tdc_ref = tdc ).

  ENDMETHOD.

  METHOD setup_tables.
    DATA: export_ut1 TYPE zexport_ut1,
          export_ut2 TYPE zexport_ut2,
          import_ut1 TYPE zimport_ut1,
          import_ut2 TYPE zimport_ut2,
          export_ut3 TYPE zexport_ut3.

    " setup the tables in this package
    DELETE FROM: zexport_ut1, zimport_ut1,
      zexport_ut2, zexport_ut3, zimport_ut2.

    export_ut1 = VALUE #( primary_key = 'AAA' content = 'char' ).
    ##LITERAL
    export_ut2 = VALUE #( primary_key = 'AAA' content = '130' ).
    export_ut3 = VALUE #( primary_key = 'ADA' content = '9999' ).

    import_ut1 = VALUE #( primary_key = 'CCC' content = 'imp' ).
    ##LITERAL
    import_ut2 = VALUE #( primary_key = 'CCC' content = '30' ).

    INSERT zexport_ut1 FROM export_ut1.
    INSERT zexport_ut2 FROM export_ut2.
    INSERT zexport_ut3 FROM export_ut3.

    INSERT zimport_ut1 FROM import_ut1.
    INSERT zimport_ut2 FROM import_ut2.

  ENDMETHOD.

  METHOD export.

    DATA(exporter) = NEW zexport_bundle_in_tdc( tdc = tdc
      variant = 'ECATTDEFAULT' ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT1' fake_table = 'ZIMPORT_UT1' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT2' fake_table = 'ZIMPORT_UT2' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT3' ) ).
    exporter->export( transport_request = space ).

  ENDMETHOD.

  METHOD import_and_replace.
    DATA: act_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          act_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2,
          act_cont_export_ut3 TYPE STANDARD TABLE OF zexport_ut3,
          exp_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          exp_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2,
          exp_cont_export_ut3 TYPE STANDARD TABLE OF zexport_ut3.

    exp_cont_import_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
    ).
    exp_cont_import_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = '130' )
    ).
    exp_cont_export_ut3 = VALUE #(
      ( client = sy-mandt primary_key = 'ADA' content = '9999' )
    ).

    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'ECATTDEFAULT' ).
    cut->replace_content_all_tables( ).
    COMMIT WORK AND WAIT.

    SELECT * FROM zimport_ut1 INTO TABLE act_cont_import_ut1.
    SELECT * FROM zimport_ut2 INTO TABLE act_cont_import_ut2.
    SELECT * FROM zexport_ut3 INTO TABLE act_cont_export_ut3.

    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut1
      act = act_cont_import_ut1
      msg = 'content imported from table zimport_ut1' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut2
      act = act_cont_import_ut2
      msg = 'content imported from table zimport_ut2' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_export_ut3
      act = act_cont_export_ut3
      msg = 'content imported from table zexport_ut3 (no fake-table)' ).

  ENDMETHOD.

  METHOD import_and_replace_all.
    DATA: act_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          act_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2,
          act_cont_export_ut3 TYPE STANDARD TABLE OF zexport_ut3,
          exp_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          exp_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2,
          exp_cont_export_ut3 TYPE STANDARD TABLE OF zexport_ut3.

    exp_cont_import_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
    ).
    exp_cont_import_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = '130' )
    ).
    exp_cont_export_ut3 = VALUE #(
      ( client = sy-mandt primary_key = 'ADA' content = '9999' )
    ).

    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'ECATTDEFAULT' ).
    cut->replace_content_completly( ).
    COMMIT WORK AND WAIT.

    SELECT * FROM zimport_ut1 INTO TABLE act_cont_import_ut1.
    SELECT * FROM zimport_ut2 INTO TABLE act_cont_import_ut2.
    SELECT * FROM zexport_ut3 INTO TABLE act_cont_export_ut3.

    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut1
      act = act_cont_import_ut1
      msg = 'content imported from table zimport_ut1' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut2
      act = act_cont_import_ut2
      msg = 'content imported from table zimport_ut2' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_export_ut3
      act = act_cont_export_ut3
      msg = 'content imported from table zexport_ut3 (no fake-table)' ).

  ENDMETHOD.

  METHOD import_and_add.
    DATA: act_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          act_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2,
          act_cont_export_ut3 TYPE STANDARD TABLE OF zexport_ut3,
          exp_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          exp_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2,
          exp_cont_export_ut3 TYPE STANDARD TABLE OF zexport_ut3.

    exp_cont_import_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
      ( client = sy-mandt primary_key = 'CCC' content = 'imp' )
    ).
    exp_cont_import_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = '130' )
      ( client = sy-mandt primary_key = 'CCC' content = '30' )
    ).
    exp_cont_export_ut3 = VALUE #(
      ( client = sy-mandt primary_key = 'ADA' content = '9999' )
    ).

    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'ECATTDEFAULT' ).
    cut->add_content_all_tables( ).
    COMMIT WORK AND WAIT.

    SELECT * FROM zimport_ut1 INTO TABLE act_cont_import_ut1
      ORDER BY PRIMARY KEY.
    SELECT * FROM zimport_ut2 INTO TABLE act_cont_import_ut2
      ORDER BY PRIMARY KEY.
    SELECT * FROM zexport_ut3 INTO TABLE act_cont_export_ut3.

    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut1
      act = act_cont_import_ut1
      msg = 'content imported from table zimport_ut1' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut2
      act = act_cont_import_ut2
      msg = 'content imported from table zimport_ut2' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_export_ut3
      act = act_cont_export_ut3
      msg = 'content imported from table zexport_ut3 (no fake-table)' ).

  ENDMETHOD.

  METHOD activate_osql_replacement.
    DATA: act_cont_export_ut1 TYPE STANDARD TABLE OF zexport_ut1,
          exp_cont_export_ut1 TYPE STANDARD TABLE OF zexport_ut1.

    exp_cont_export_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
    ).

    " when
    DELETE FROM zexport_ut1.
    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'ECATTDEFAULT' ).
    cut->replace_content_completly( ).
    cut->activate_osql_replacement( ).
    COMMIT WORK AND WAIT.

    " then
    SELECT * FROM zexport_ut1 INTO TABLE act_cont_export_ut1.
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_export_ut1
      act = act_cont_export_ut1 ).

    " teardown
    CALL METHOD ('CL_OSQL_REPLACE')=>activate_replacement.

  ENDMETHOD.

ENDCLASS.
