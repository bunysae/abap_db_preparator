CLASS test_export_import DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL DANGEROUS.

  PRIVATE SECTION.
    CONSTANTS: tdc_name TYPE etobj_name VALUE 'ZEXPORT_UNIT_TEST'.

    METHODS setup
      RAISING
        cx_static_check.

    METHODS create_tdc
      RAISING
        cx_static_check.

    METHODS export_in_ecattdefault_variant
      RAISING
        cx_static_check.

    METHODS export_in_secondary_variant
      RAISING
        cx_static_check.

    METHODS setup_tables_ecattdefault_vari.

    METHODS setup_tables_secondary_variant.

    METHODS setup_import_tables.

    "! Import from default variant
    METHODS import_and_replace_dv FOR TESTING
      RAISING
        cx_static_check.

    "! Import from secondary variant.
    "! Tables 'zimport_ut2' and 'zexport_ut3' should stay empty
    METHODS import_and_replace_sv FOR TESTING
      RAISING
        cx_static_check.

    METHODS import_and_replace_all_dv FOR TESTING
      RAISING
        cx_static_check.

    "! Import from secondary variant.
    "! Tables 'zimport_ut2' and 'zexport_ut3' should stay empty
    METHODS import_and_replace_all_sv FOR TESTING
      RAISING
        cx_static_check.

    METHODS import_and_add_dv FOR TESTING
      RAISING
        cx_static_check.

    "! Import from secondary variant.
    "! Tables 'zimport_ut2' and 'zexport_ut3' should stay empty
    METHODS import_and_add_sv FOR TESTING
      RAISING
        cx_static_check.

    METHODS no_import_for_blacklist FOR TESTING
      RAISING
        cx_static_check.

    METHODS activate_tvarvc_replacement.

    METHODS set_replacement_whitelist.

    METHODS activate_osql_replacement FOR TESTING
      RAISING
        cx_static_check.

    METHODS get_changed_source_tables FOR TESTING
      RAISING
        cx_static_check.

    METHODS source_table_has_changed FOR TESTING
      RAISING
        cx_static_check.

    METHODS teardown.

ENDCLASS.

CLASS test_export_import IMPLEMENTATION.

  METHOD setup.

    create_tdc( ).
    setup_tables_ecattdefault_vari( ).
    COMMIT WORK AND WAIT.
    export_in_ecattdefault_variant( ).
    COMMIT WORK AND WAIT.
    setup_tables_secondary_variant( ).
    COMMIT WORK AND WAIT.
    export_in_secondary_variant( ).
    COMMIT WORK AND WAIT.

    setup_import_tables( ).
    activate_tvarvc_replacement( ).
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD teardown.

    IF sy-saprl < 751.
      RETURN.
    ENDIF.
    CALL METHOD ('CL_OSQL_REPLACE')=>activate_replacement.

  ENDMETHOD.

  METHOD create_tdc.

    TRY.
        cl_apl_ecatt_tdc_api=>delete_tdc( i_name = tdc_name ).
        ##NO_HANDLER
      CATCH cx_ecatt_tdc_access.
    ENDTRY.

    cl_apl_ecatt_tdc_api=>create_tdc( EXPORTING
      i_name = tdc_name i_tadir_devclass = '$TMP' i_write_access = abap_true
      IMPORTING e_tdc_ref = DATA(tdc_accessor) ).
    tdc_accessor->commit_changes( i_release_lock = abap_true
      i_commit_mode = abap_false ).

  ENDMETHOD.

  METHOD setup_tables_ecattdefault_vari.
    DATA: export_ut1 TYPE zexport_ut1,
          export_ut2 TYPE zexport_ut2,
          export_ut3 TYPE zexport_ut3.

    " setup the tables in this package
    DELETE FROM: zexport_ut1, zexport_ut2, zexport_ut3.

    export_ut1 = VALUE #( primary_key = 'AAA' content = 'char' ).
    ##LITERAL
    export_ut2 = VALUE #( primary_key = 'AAA' content = '130' ).
    export_ut3 = VALUE #( primary_key = 'ADA' content = '9999' ).

    INSERT zexport_ut1 FROM export_ut1.
    INSERT zexport_ut2 FROM export_ut2.
    INSERT zexport_ut3 FROM export_ut3.

  ENDMETHOD.

  METHOD setup_tables_secondary_variant.
    DATA: export_ut1 TYPE zexport_ut1.

    DELETE FROM: zexport_ut1, zexport_ut2, zexport_ut3.

    export_ut1 = VALUE #( primary_key = 'AAA' content = 'char' ).

    INSERT zexport_ut1 FROM export_ut1.

  ENDMETHOD.

  METHOD setup_import_tables.
    DATA: import_ut1 TYPE zimport_ut1,
          import_ut2 TYPE zimport_ut2.

    DELETE FROM: zimport_ut1, zimport_ut2, zexport_ut3.

    import_ut1 = VALUE #( primary_key = 'CCC' content = 'imp' ).
    ##LITERAL
    import_ut2 = VALUE #( primary_key = 'CCC' content = '30' ).

    INSERT zimport_ut1 FROM import_ut1.
    INSERT zimport_ut2 FROM import_ut2.

  ENDMETHOD.

  METHOD export_in_ecattdefault_variant.

    DATA(tdc_accessor) = cl_apl_ecatt_tdc_api=>get_instance( EXPORTING i_testdatacontainer = tdc_name
      i_testdatacontainer_version = 1 i_write_access = abap_true ).
    DATA(exporter) = NEW zexport_bundle_in_tdc( tdc = tdc_accessor
      variant = 'ECATTDEFAULT' ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT1' fake_table = 'ZIMPORT_UT1' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT2' fake_table = 'ZIMPORT_UT2' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT3' ) ).
    exporter->export( transport_request = space ).

  ENDMETHOD.

  METHOD export_in_secondary_variant.

    DATA(tdc_accessor) = cl_apl_ecatt_tdc_api=>get_instance( EXPORTING i_testdatacontainer = tdc_name
      i_testdatacontainer_version = 1 i_write_access = abap_true ).
    DATA(exporter) = NEW zexport_bundle_in_tdc( tdc = tdc_accessor
      variant = 'SECONDARY' ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT1' fake_table = 'ZIMPORT_UT1' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT2' fake_table = 'ZIMPORT_UT2' ) ).
    exporter->export( transport_request = space ).

  ENDMETHOD.

  METHOD import_and_replace_dv.
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

    " when
    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'ECATTDEFAULT' ).
    cut->replace_content_all_tables( ).
    COMMIT WORK AND WAIT.

    " then
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

  METHOD import_and_replace_sv.
    DATA: act_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          exp_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1.

    exp_cont_import_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
    ).

    " when
    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'SECONDARY' ).
    cut->replace_content_all_tables( ).
    COMMIT WORK AND WAIT.

    " then
    SELECT * FROM zimport_ut1 INTO TABLE act_cont_import_ut1.
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut1
      act = act_cont_import_ut1
      msg = 'content imported from table zimport_ut1' ).

    SELECT COUNT(*) FROM zimport_ut2.
    cl_abap_unit_assert=>assert_subrc( exp = 4
      msg = 'exported content for table zexport_ut2 was initial' ).

  ENDMETHOD.

  METHOD import_and_replace_all_dv.
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

    " when
    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'ECATTDEFAULT' ).
    cut->replace_content_completly( ).
    COMMIT WORK AND WAIT.

    " then
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

  METHOD import_and_replace_all_sv.
    DATA: act_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          exp_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1.

    exp_cont_import_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
    ).

    " when
    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'SECONDARY' ).
    cut->replace_content_completly( ).
    COMMIT WORK AND WAIT.

    " then
    SELECT * FROM zimport_ut1 INTO TABLE act_cont_import_ut1.
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut1
      act = act_cont_import_ut1
      msg = 'content imported from table zimport_ut1' ).

    SELECT COUNT(*) FROM zimport_ut2.
    cl_abap_unit_assert=>assert_subrc( exp = 4
      msg = 'exported content for table zexport_ut2 was initial' ).

  ENDMETHOD.

  METHOD import_and_add_dv.
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

    " when
    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'ECATTDEFAULT' ).
    cut->add_content_all_tables( ).
    COMMIT WORK AND WAIT.

    " then
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

  METHOD import_and_add_sv.
    DATA: act_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          exp_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          act_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2,
          exp_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2.

    exp_cont_import_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
      ( client = sy-mandt primary_key = 'CCC' content = 'imp' )
    ).
    exp_cont_import_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'CCC' content = '30' )
    ).

    " when
    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'SECONDARY' ).
    cut->add_content_all_tables( ).
    COMMIT WORK AND WAIT.

    " then
    SELECT * FROM zimport_ut1 INTO TABLE act_cont_import_ut1.
    SELECT * FROM zimport_ut2 INTO TABLE act_cont_import_ut2.

    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut1
      act = act_cont_import_ut1
      msg = 'content imported from table zimport_ut1' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut2
      act = act_cont_import_ut2
      msg = 'content from table zimport_ut2 should not change' ).

  ENDMETHOD.

  METHOD no_import_for_blacklist.
    DATA: act_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          exp_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          act_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2,
          exp_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2.

    " given
    set_replacement_whitelist( ).

    exp_cont_import_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'CCC' content = 'imp' )
    ).
    exp_cont_import_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = '130' )
    ).

    " when
    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'ECATTDEFAULT' ).
    cut->replace_content_completly( ).
    COMMIT WORK AND WAIT.

    " then
    SELECT * FROM zimport_ut1 INTO TABLE act_cont_import_ut1.
    SELECT * FROM zimport_ut2 INTO TABLE act_cont_import_ut2.

    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut1
      act = act_cont_import_ut1
      msg = 'table zimport_ut1 is excluded from the whitelist' ).
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut2
      act = act_cont_import_ut2
      msg = 'content from table zimport_ut2' ).

  ENDMETHOD.

  METHOD activate_tvarvc_replacement.
    TYPES: BEGIN OF replacement,
            source TYPE tabname,
            target TYPE tabname,
          END OF replacement.
    DATA: replaced_tables TYPE STANDARD TABLE OF replacement,
          _replaced_tables TYPE REF TO data.
    FIELD-SYMBOLS: <replaced_tables> TYPE ANY TABLE.

    DELETE FROM zimport_tvarvc.

    IF sy-saprl < 751.
      RETURN.
    ENDIF.

    CREATE DATA _replaced_tables TYPE ('CL_OSQL_REPLACE=>TT_REPLACEMENT').
    ASSIGN _replaced_tables->* TO <replaced_tables>.

    replaced_tables = VALUE #( ( source = 'TVARVC' target = 'ZIMPORT_TVARVC' ) ).
    MOVE-CORRESPONDING replaced_tables TO <replaced_tables>.
    CALL METHOD ('CL_OSQL_REPLACE')=>activate_replacement
      EXPORTING
        replacement_table = <replaced_tables>.

  ENDMETHOD.

  METHOD set_replacement_whitelist.
    DATA: whitelist TYPE STANDARD TABLE OF zimport_tvarvc.

    whitelist = VALUE #(
      ( name = 'OTHER' type = 'S' sign = 'E' opti = 'EQ' low = 'Z*' )
      ( name = 'ZIMPORT_REPLACE_WHITELIST' type = 'S' sign = 'E' opti = 'EQ' low = 'ZIMPORT_UT1' )
    ).
    INSERT zimport_tvarvc FROM TABLE whitelist.
    COMMIT WORK AND WAIT.

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

  METHOD get_changed_source_tables.
    DATA: exp_indicies TYPE zimport_bundle=>index_table,
          export_ut2 TYPE zexport_ut2.

    " given
    export_ut2 = VALUE #( primary_key = 'BBB' content = '130' ).
    INSERT zexport_ut2 FROM export_ut2.
    COMMIT WORK AND WAIT.

    " when
    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'ECATTDEFAULT' ).
    cut->get_changed_source_tables(
      IMPORTING indicies = DATA(act_indicies) ).

    " then
    APPEND: 2 TO exp_indicies, 3 TO exp_indicies.
    cl_abap_unit_assert=>assert_equals( exp = exp_indicies
      act = act_indicies ).

  ENDMETHOD.

  METHOD source_table_has_changed.

    " given
    DATA(export_ut2) = VALUE zexport_ut2( primary_key = 'BBB' content = '130' ).
    INSERT zexport_ut2 FROM export_ut2.
    COMMIT WORK AND WAIT.

    " when
    DATA(cut) = NEW zimport_bundle_from_tdc( tdc = tdc_name
      variant = 'ECATTDEFAULT' ).

    " then
    cl_abap_unit_assert=>assert_true( act = cut->source_table_has_changed(
      table_conjunction = VALUE #( source_table = 'ZEXPORT_UT2' fake_table = 'ZIMPORT_UT2'
        tdc_parameter_name = 'ZIMPORT_UT2' ) ) ).
    cl_abap_unit_assert=>assert_false( act = cut->source_table_has_changed(
      table_conjunction = VALUE #( source_table = 'ZEXPORT_UT1' fake_table = 'ZIMPORT_UT1'
        tdc_parameter_name = 'ZIMPORT_UT1' ) ) ).

  ENDMETHOD.

ENDCLASS.
