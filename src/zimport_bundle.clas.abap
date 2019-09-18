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
protected section.

  "! Called either in abap unit-test
  "! or if the method is called outside of an unit-test, at least dangerous
  "! unit-tests must be enabled
  "! @raising zcx_import_not_allowed | Of the above conditions are not met
  methods CALLED_INSIDE_UNIT_TEST
    RAISING
      zcx_import_not_allowed.
private section.
ENDCLASS.



CLASS ZIMPORT_BUNDLE IMPLEMENTATION.


  method CALLED_INSIDE_UNIT_TEST.

    DATA: call_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack.

    call_stack = cl_abap_get_call_stack=>format_call_stack_with_struct(
      cl_abap_get_call_stack=>get_call_stack( )
    ).

    " Assumes, that testclasses are executed from the following stack:
    " kind=METHOD
    " progname=CL_AUNIT_TEST_CLASS===========CP
    " event: a method of CL_AUNIT_TEST_CLASS=>if_aunit_test_class_handle

    LOOP AT call_stack TRANSPORTING NO FIELDS
      WHERE kind = 'METHOD' AND progname = 'CL_AUNIT_TEST_CLASS===========CP'
      AND event CS 'CL_AUNIT_TEST_CLASS=>IF_AUNIT_TEST_CLASS_HANDLE'.

      RETURN.

    ENDLOOP.

    DATA(aunit_setup) = cl_aunit_customizing=>get_setup( ).
    IF aunit_setup-client-max_risk_level >= if_Aunit_Attribute_Enums=>c_Risk_Level-dangerous
      AND aunit_setup-client-deny_execution = abap_false.

      RETURN.

    ENDIF.

    RAISE EXCEPTION TYPE zcx_import_not_allowed.

  endmethod.
ENDCLASS.
