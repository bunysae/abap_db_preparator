class ZIMPORT_BUNDLE_FROM_TDC definition
  public
  inheriting from ZIMPORT_BUNDLE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TDC type ETOBJ_NAME
      !TDC_VERSION type ETOBJ_VER OPTIONAL
      !VARIANT type ETVAR_ID
    raising
      ZCX_IMPORT_ERROR .

  methods ADD_CONTENT_ALL_TABLES
    redefinition .
  methods REPLACE_CONTENT_ALL_TABLES
    redefinition .
protected section.
private section.
  DATA tdc TYPE REF TO cl_apl_ecatt_tdc_api.
  DATA variant TYPE etvar_id.
ENDCLASS.



CLASS ZIMPORT_BUNDLE_FROM_TDC IMPLEMENTATION.


  METHOD add_content_all_tables.
    DATA: content TYPE REF TO data.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    TRY.
        " it's assumed, that all params of the tdc are transparent tables
        " and the name of the parameter is equal to the name of transparent table
        LOOP AT tdc->get_param_list( ) REFERENCE INTO DATA(param).

          CREATE DATA content TYPE STANDARD TABLE OF (param->*).
          ASSIGN content->* TO <con>.
          tdc->get_value( EXPORTING i_param_name = param->* i_variant_name = variant
            CHANGING e_param_value = <con> ).

          MODIFY (param->*) FROM TABLE <con>.

        ENDLOOP.
      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_import_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

  ENDMETHOD.


  method CONSTRUCTOR.

    super->constructor( ).
    called_inside_unit_test( ).

    TRY.
      me->tdc = cl_apl_ecatt_tdc_api=>get_instance( i_testdatacontainer = tdc
        i_testdatacontainer_version = tdc_version ).
      me->variant = variant.

      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_import_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

  endmethod.


  METHOD replace_content_all_tables.
    DATA: content TYPE REF TO data.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    TRY.
        " it's assumed, that all params of the tdc are transparent tables
        " and the name of the parameter is equal to the name of transparent table
        LOOP AT tdc->get_param_list( ) REFERENCE INTO DATA(param).

          CREATE DATA content TYPE STANDARD TABLE OF (param->*).
          ASSIGN content->* TO <con>.
          tdc->get_value( EXPORTING i_param_name = param->* i_variant_name = variant
            CHANGING e_param_value = <con> ).

          DELETE FROM (param->*).
          INSERT (param->*) FROM TABLE <con>.

        ENDLOOP.
      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_import_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
