class ZEXPORT_BUNDLE_IN_TDC definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TDC type ref to CL_APL_ECATT_TDC_API
      !VARIANT type ETVAR_ID
    RAISING
      cx_ecatt_tdc_access.
  "! Add the content of the table to the bundle (uses the builder-pattern).
  "! @parameter _table |
  "! <ul>
  "! <li> Component "source_table":
  "!   Database table (transparent table), where the content lies.
  "! </li>
  "! <li> Component "fake_table":
  "! The fake table, which is used in the unit-test.
  "! The import-class <em>zimport_bundle_from_cluster</em>
  "! overwrites the content of the fake-table.
  "! This parameter can be used, if working with class <em>cl_osq_replace</em>
  "! to replace table contents.
  "! If this parameter is omitted, parameter <em>table</em> is used.
  "! </li>
  "! <li> Component "where_restriction":
  "! An valid sql-where-clause for an restriction of the exported rows.
  "! </li>
  "! </ul>
  "! @raising zcx_export_table_duplicate | If a table name is
  "! used more than one time.
  methods ADD_TABLE_TO_BUNDLE
    importing
      VALUE(_TABLE) type zexport_table_list
    returning
      value(INSTANCE) type ref to ZEXPORT_BUNDLE_IN_TDC
    raising
      CX_ECATT_TDC_ACCESS .
  methods EXPORT
    IMPORTING
      transport_request TYPE e070-trkorr
    RAISING
      cx_ecatt_tdc_access.
protected section.
private section.

  data TDC type ref to CL_APL_ECATT_TDC_API .
  data VARIANT type ETVAR_ID .
  DATA table_list TYPE STANDARD TABLE OF zexport_table_list.

  methods CREATE_PARAMETER
    importing
      !TABLE type TABNAME
    returning
      value(NAME) type ETP_NAME
    raising
      CX_ECATT_TDC_ACCESS .
  methods SET_PARAMETER_VALUE
    importing
      !CONTENT type STANDARD TABLE
      !NAME type ETP_NAME
    RAISING
      cx_ecatt_tdc_access.
ENDCLASS.



CLASS ZEXPORT_BUNDLE_IN_TDC IMPLEMENTATION.


  method ADD_TABLE_TO_BUNDLE.
    DATA: content TYPE REF TO data,
          param_name TYPE etp_name.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    IF _table-fake_table IS INITIAL.
      _table-fake_table = _table-source_table.
    ENDIF.
    param_name = create_parameter( _table-fake_table ).

    INSERT _table INTO TABLE table_list.

    CREATE DATA content TYPE STANDARD TABLE OF (_table-source_table).
    ASSIGN content->* TO <con>.

    SELECT * FROM (_table-source_table) INTO TABLE <con>
      WHERE (_table-where_restriction).

    set_parameter_value( content = <con> name = param_name ).

  endmethod.


  method CONSTRUCTOR.

    me->tdc = tdc.
    me->variant = variant.

    " create variant, if not exists
    TRY.
      tdc->create_variant( i_variant_name = variant ).
      CATCH cx_ecatt_tdc_access INTO DATA(failure).
        IF failure->textid <> cx_ecatt_tdc_access=>variant_exists.
          RAISE EXCEPTION failure.
        ENDIF.
    ENDTRY.

  endmethod.


  METHOD create_parameter.
    DATA: type_definition TYPE string.

    CONCATENATE 'STANDARD TABLE OF' table INTO type_definition
      SEPARATED BY space.

    TRY.
      tdc->create_parameter( i_param_name = table
        i_param_def = type_definition ).
      CATCH cx_ecatt_tdc_access INTO DATA(failure).
        IF failure->textid <> cx_ecatt_tdc_access=>parameter_exists.
          RAISE EXCEPTION failure.
        ENDIF.
    ENDTRY.

    name = table.

  ENDMETHOD.


  method EXPORT.

    DATA(param_for_list) = create_parameter( table = 'ZEXPORT_TABLE_LIST' ).
    set_parameter_value( content = table_list name = param_for_list ).

    tdc->commit_changes( i_commit_mode = abap_false
      i_tr_order = transport_request ).

  endmethod.


  method SET_PARAMETER_VALUE.

    tdc->set_value( i_param_name = name i_param_value = content
      i_variant_name = variant ).

  endmethod.
ENDCLASS.
