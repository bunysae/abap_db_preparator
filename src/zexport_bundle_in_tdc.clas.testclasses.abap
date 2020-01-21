CLASS test_export_bundle DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    " TDC is temporary created
    CONSTANTS: tdc_name TYPE etobj_name VALUE 'ZEXPORT_UNIT_TEST'.
    CLASS-DATA: cut TYPE REF TO zexport_bundle_in_tdc.

    CLASS-METHODS class_setup
      RAISING cx_static_check.

    " Testcases for method get_parameter_name:
    " <ol>
    "   <li>should replace invalid characters</li>
    "   <li>should not replace valid characters</li>
    METHODS verify_parameter_name FOR TESTING.

    CLASS-METHODS class_teardown.

ENDCLASS.

CLASS zexport_bundle_in_tdc DEFINITION LOCAL FRIENDS test_export_bundle.

CLASS test_export_bundle IMPLEMENTATION.

  METHOD class_setup.

    TRY.
        cl_apl_ecatt_tdc_api=>delete_tdc( i_name = tdc_name ).
        ##NO_HANDLER
      CATCH cx_ecatt_tdc_access.
    ENDTRY.

    cl_apl_ecatt_tdc_api=>create_tdc( EXPORTING
      i_name = 'ZEXPORT_UNIT_TEST' i_tadir_devclass = '$TMP' i_write_access = abap_true
      IMPORTING e_tdc_ref = DATA(tdc) ).

    cut = NEW zexport_bundle_in_tdc( tdc = tdc variant = 'ECATTDEFAULT' ).

  ENDMETHOD.

  METHOD verify_parameter_name.

    cl_abap_unit_assert=>assert_equals( exp = '_namespace_table1'
      act = cut->get_parameter_name( '/namespace/table1' )
      msg = `invalid character '\' should be replaced` ).

    cl_abap_unit_assert=>assert_equals( exp = 'some_table1'
      act = cut->get_parameter_name( 'some_table1' )
      msg = `no characters should be changed. All are valid` ).

  ENDMETHOD.

  METHOD class_teardown.

    TRY.
        cl_apl_ecatt_tdc_api=>delete_tdc( i_name = tdc_name ).
        ##NO_HANDLER
      CATCH cx_ecatt_tdc_access.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
