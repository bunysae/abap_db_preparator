CONSTANTS: testcase_id TYPE w3objid VALUE 'ZBUNDLE_UNIT_TEST'.

CLASS test_export_import DEFINITION FOR TESTING DURATION SHORT
  RISK LEVEL DANGEROUS.

  PRIVATE SECTION.

    METHODS setup
      RAISING
        zcx_export_error.

    METHODS setup_mime.

    METHODS setup_tables.

    METHODS export
      RAISING
        zcx_export_error.

    METHODS import_and_replace FOR TESTING
      RAISING
        zcx_import_error.

    METHODS import_and_add FOR TESTING
      RAISING
        zcx_import_error.

    METHODS activate_osql_replacement FOR TESTING
      RAISING
        zcx_import_error.

    METHODS get_changed_source_tables FOR TESTING
      RAISING
        cx_static_check.

    METHODS get_exported_content FOR TESTING
      RAISING
        cx_static_check.

    METHODS reexport
      RAISING
        cx_static_check.

    METHODS reexport_subset FOR TESTING
      RAISING
        cx_static_check.

ENDCLASS.

CLASS test_export_import IMPLEMENTATION.

  METHOD setup.

    setup_mime( ).
    setup_tables( ).
    COMMIT WORK AND WAIT.
    export( ).
    COMMIT WORK AND WAIT.

    DELETE FROM zexport_ut3.
    COMMIT WORK AND WAIT.

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

    " commit work and wait necessary, because uncommitted read
    " on oracle and hana databases not possible.
    " If the MIME-Object exists, export will throw exception.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD setup_tables.
    DATA: export_ut1 TYPE zexport_ut1,
          export_ut2 TYPE zexport_ut2,
          export_ut3 TYPE zexport_ut3,
          import_ut1 TYPE zimport_ut1,
          import_ut2 TYPE zimport_ut2.

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

    INSERT zimport_ut1 FROM import_ut1.
    INSERT zimport_ut2 FROM import_ut2.

    INSERT zexport_ut3 FROM export_ut3.

  ENDMETHOD.

  METHOD export.
    DATA: dev_package TYPE devclass.

    SELECT SINGLE devclass FROM tadir INTO dev_package
      WHERE pgmid = 'R3TR' AND object = 'CLAS' AND obj_name = 'ZIMPORT_BUNDLE_FROM_CLUSTER'.

    DATA(exporter) = NEW zexport_bundle_in_cluster( testcase_id = testcase_id
      dev_package = dev_package title = 'Unit-Test ABAP DB preparator' ).

    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT1' fake_table = 'ZIMPORT_UT1' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT2' fake_table = 'ZIMPORT_UT2' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT3' ) ).

    exporter->export( ).

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

    DATA(cut) = NEW zimport_bundle_from_cluster( testcase_id ).

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

    DATA(cut) = NEW zimport_bundle_from_cluster( testcase_id ).

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
    DATA(cut) = NEW zimport_bundle_from_cluster( testcase_id ).
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
    DATA(cut) = NEW zimport_bundle_from_cluster( testcase_id ).
    cut->get_changed_source_tables(
      IMPORTING indicies = DATA(act_indicies) ).

    " then
    APPEND: 2 TO exp_indicies, 3 TO exp_indicies.
    cl_abap_unit_assert=>assert_equals( exp = exp_indicies
      act = act_indicies ).

  ENDMETHOD.

  METHOD get_exported_content.
    DATA: act_content TYPE REF TO data,
          exp_content TYPE STANDARD TABLE OF zexport_ut1.
    FIELD-SYMBOLS: <act_content> LIKE exp_content.

    exp_content = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
    ).

    " when
    CREATE DATA act_content LIKE exp_content.
    DATA(cut) = NEW zimport_bundle_from_cluster( testcase_id ).
    cut->get_exported_content_for_table( EXPORTING source_table = 'ZEXPORT_UT1'
      IMPORTING content = act_content ).

    " then
    ASSIGN act_content->* TO <act_content>.
    cl_abap_unit_assert=>assert_equals( exp = exp_content
      act = <act_content> ).

  ENDMETHOD.

  METHOD reexport.
    DATA: prior_content_ut1 TYPE REF TO data,
          dev_package TYPE devclass.

    SELECT SINGLE devclass FROM tadir INTO dev_package
      WHERE pgmid = 'R3TR' AND object = 'CLAS' AND obj_name = 'ZIMPORT_BUNDLE_FROM_CLUSTER'.

    DATA(exporter) = NEW zexport_bundle_in_cluster( testcase_id = testcase_id
      dev_package = dev_package title = 'Unit-Test ABAP DB preparator'
      force_overwrite = abap_true ).
    DATA(importer) = NEW zimport_bundle_from_cluster( testcase_id = testcase_id ).

    importer->get_exported_content_for_table( EXPORTING source_table = 'ZEXPORT_UT1'
      IMPORTING content = prior_content_ut1 ).
    exporter->add_prior_content( _table = VALUE #(
      source_table = 'ZEXPORT_UT1' fake_table = 'ZIMPORT_UT1' )
      content = prior_content_ut1 ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT2' fake_table = 'ZIMPORT_UT2' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT3' ) ).

    exporter->export( ).

  ENDMETHOD.

  METHOD reexport_subset.
    DATA: act_content_ut1 TYPE REF TO data,
          exp_content_ut1 TYPE STANDARD TABLE OF zexport_ut1.
    FIELD-SYMBOLS: <act_content_ut1> LIKE exp_content_ut1.

    exp_content_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
    ).

    " when
    reexport( ).
    COMMIT WORK AND WAIT.

    " then
    DATA(importer) = NEW zimport_bundle_from_cluster( testcase_id = testcase_id ).
    importer->get_exported_content_for_table( EXPORTING source_table = 'ZEXPORT_UT1'
      IMPORTING content = act_content_ut1 ).
    ASSIGN act_content_ut1->* TO <act_content_ut1>.

    cl_abap_unit_assert=>assert_equals( exp = exp_content_ut1
      act = <act_content_ut1> ).

  ENDMETHOD.

ENDCLASS.

CLASS test_for_all_entries DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS setup
      RAISING
        cx_static_check.

    METHODS setup_mime
      RAISING
        cx_static_check.

    METHODS setup_tables.

    METHODS export
      RAISING
        cx_static_check.

    METHODS setup_import_tables.

    METHODS import_and_replace FOR TESTING
      RAISING
        cx_static_check.

    METHODS import_and_add FOR TESTING
      RAISING
        cx_static_check.

    METHODS import_and_replace_all FOR TESTING
      RAISING
        cx_static_check.

ENDCLASS.

CLASS test_for_all_entries IMPLEMENTATION.

  METHOD setup.

    setup_mime( ).
    setup_tables( ).
    export( ).
    setup_import_tables( ).
    COMMIT WORK AND WAIT.

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
    DATA: export_ut1 TYPE STANDARD TABLE OF zexport_ut1,
          export_ut2 TYPE STANDARD TABLE OF zexport_ut2,
          export_ut3 TYPE zexport_ut3.

    " setup the tables in this package
    DELETE FROM: zexport_ut1, zexport_ut2, zexport_ut3.

    export_ut1 = VALUE #(
      ( primary_key = 'AAA' content = 'char' )
      ( primary_key = 'AAB' content = 'num' )
      ( primary_key = 'AAC' content = 'int' )
    ).
    ##LITERAL
    export_ut2 = VALUE #(
      ( primary_key = 'AAA' content = '130' )
      ( primary_key = 'AAB' content = '140' )
      ( primary_key = 'DDD' content = '150' )
    ).
    export_ut3 = VALUE #( primary_key = 'ADA' content = '9999' ).

    INSERT zexport_ut1 FROM TABLE export_ut1.
    INSERT zexport_ut2 FROM TABLE export_ut2.
    INSERT zexport_ut3 FROM export_ut3.

  ENDMETHOD.

  METHOD export.
    DATA: dev_package TYPE devclass.

    SELECT SINGLE devclass FROM tadir INTO dev_package
      WHERE pgmid = 'R3TR' AND object = 'CLAS' AND obj_name = 'ZIMPORT_BUNDLE_FROM_CLUSTER'.

    DATA(exporter) = NEW zexport_bundle_in_cluster( testcase_id = testcase_id
      dev_package = dev_package title = 'Unit-Test ABAP DB preparator' ).

    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT1' fake_table = 'ZIMPORT_UT1' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT2' fake_table = 'ZIMPORT_UT2'
      where_restriction = 'FOR ALL ENTRIES IN xzexport_ut1 WHERE primary_key = xzexport_ut1-primary_key' ) ).
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = 'ZEXPORT_UT3' ) ).

    exporter->export( ).

  ENDMETHOD.

  METHOD setup_import_tables.
    DATA: import_ut2 TYPE zimport_ut2.

    DELETE FROM: zimport_ut1, zimport_ut2, zexport_ut3.
    import_ut2 = VALUE #( primary_key = 'CCC' content = '400' ).
    INSERT zimport_ut2 FROM import_ut2.

  ENDMETHOD.

  METHOD import_and_replace.
    DATA: act_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          exp_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          act_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2,
          exp_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2.

    exp_cont_import_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
      ( client = sy-mandt primary_key = 'AAB' content = 'num' )
      ( client = sy-mandt primary_key = 'AAC' content = 'int' )
    ).
    exp_cont_import_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = '130' )
      ( client = sy-mandt primary_key = 'AAB' content = '140' )
      ( client = sy-mandt primary_key = 'CCC' content = '400' )
    ).

    " when
    DATA(cut) = NEW zimport_bundle_from_cluster( testcase_id ).
    cut->replace_content_all_tables( ).
    SELECT * FROM zimport_ut1 INTO TABLE act_cont_import_ut1
      ORDER BY PRIMARY KEY.
    SELECT * FROM zimport_ut2 INTO TABLE act_cont_import_ut2
      ORDER BY PRIMARY KEY.

    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut1
      act = act_cont_import_ut1 ).
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut2
      act = act_cont_import_ut2 ).

  ENDMETHOD.

  METHOD import_and_add.
    DATA: act_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          exp_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          act_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2,
          exp_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2.

    exp_cont_import_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
      ( client = sy-mandt primary_key = 'AAB' content = 'num' )
      ( client = sy-mandt primary_key = 'AAC' content = 'int' )
    ).
    exp_cont_import_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = '130' )
      ( client = sy-mandt primary_key = 'AAB' content = '140' )
      ( client = sy-mandt primary_key = 'CCC' content = '400' )
    ).

    " when
    DATA(cut) = NEW zimport_bundle_from_cluster( testcase_id ).
    cut->add_content_all_tables( ).
    SELECT * FROM zimport_ut1 INTO TABLE act_cont_import_ut1
      ORDER BY PRIMARY KEY.
    SELECT * FROM zimport_ut2 INTO TABLE act_cont_import_ut2
      ORDER BY PRIMARY KEY.

    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut1
      act = act_cont_import_ut1 ).
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut2
      act = act_cont_import_ut2 ).

  ENDMETHOD.

  METHOD import_and_replace_all.
    DATA: act_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          exp_cont_import_ut1 TYPE STANDARD TABLE OF zimport_ut1,
          act_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2,
          exp_cont_import_ut2 TYPE STANDARD TABLE OF zimport_ut2.

    exp_cont_import_ut1 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = 'char' )
      ( client = sy-mandt primary_key = 'AAB' content = 'num' )
      ( client = sy-mandt primary_key = 'AAC' content = 'int' )
    ).
    exp_cont_import_ut2 = VALUE #(
      ( client = sy-mandt primary_key = 'AAA' content = '130' )
      ( client = sy-mandt primary_key = 'AAB' content = '140' )
    ).

    " when
    DATA(cut) = NEW zimport_bundle_from_cluster( testcase_id ).
    cut->replace_content_completly( ).
    SELECT * FROM zimport_ut1 INTO TABLE act_cont_import_ut1
      ORDER BY PRIMARY KEY.
    SELECT * FROM zimport_ut2 INTO TABLE act_cont_import_ut2
      ORDER BY PRIMARY KEY.

    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut1
      act = act_cont_import_ut1 ).
    cl_abap_unit_assert=>assert_equals( exp = exp_cont_import_ut2
      act = act_cont_import_ut2 ).

  ENDMETHOD.

ENDCLASS.
