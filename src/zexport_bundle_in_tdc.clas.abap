class ZEXPORT_BUNDLE_IN_TDC definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TDC type ref to CL_APL_ECATT_TDC_API
      !VARIANT type ETVAR_ID
    raising
      CX_ECATT_TDC_ACCESS .
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
  "! @raising zcx_export_error | If a table name is
  "! used more than one time or,
  "! if the "where_restriction" is invalid.
  methods ADD_TABLE_TO_BUNDLE
    importing
      value(_TABLE) type ZEXPORT_TABLE_LIST
    returning
      value(INSTANCE) type ref to ZEXPORT_BUNDLE_IN_TDC
    raising
      ZCX_EXPORT_ERROR .
  methods EXPORT
    importing
      !TRANSPORT_REQUEST type E070-TRKORR
    raising
      ZCX_EXPORT_ERROR .
  methods ADD_PRIOR_CONTENT
    importing
      table_conjunction type zexport_table_list.
protected section.
private section.

  data TDC type ref to CL_APL_ECATT_TDC_API .
  data VARIANT type ETVAR_ID .
  data:
    table_list TYPE STANDARD TABLE OF zexport_table_list .

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
    raising
      CX_ECATT_TDC_ACCESS .
  "! Invalid characters for ecatt parameters are replaced by '_'.
  methods GET_PARAMETER_NAME
    importing
      !TABLE_NAME type TABNAME
    returning
      value(RESULT) type ETP_NAME .
ENDCLASS.



CLASS ZEXPORT_BUNDLE_IN_TDC IMPLEMENTATION.


  METHOD add_prior_content.

    IF line_exists( table_list[ fake_table = table_conjunction-fake_table ] ).
      RAISE EXCEPTION TYPE zcx_export_table_duplicate
        EXPORTING
          table = table_conjunction-fake_table.
    ENDIF.

    INSERT table_conjunction INTO TABLE table_list.

  ENDMETHOD.


  METHOD add_table_to_bundle.
    DATA: content    TYPE REF TO data,
          param_name TYPE etp_name.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    IF _table-fake_table IS INITIAL.
      _table-fake_table = _table-source_table.
    ENDIF.
    IF line_exists( table_list[ fake_table = _table-fake_table ] ).
      RAISE EXCEPTION TYPE zcx_export_table_duplicate
        EXPORTING
          table = _table-fake_table.
    ENDIF.

    TRY.
        _table-tdc_parameter_name = create_parameter( _table-fake_table ).

        CREATE DATA content TYPE STANDARD TABLE OF (_table-source_table).
        ASSIGN content->* TO <con>.

        SELECT * FROM (_table-source_table) INTO TABLE @<con>
          WHERE (_table-where_restriction).
        _table-is_initial = xsdbool( <con> IS INITIAL ).

        set_parameter_value( content = <con> name = _table-tdc_parameter_name ).

        INSERT _table INTO TABLE table_list.

      CATCH cx_sy_dynamic_osql_error INTO DATA(osql_syntax_error).
        RAISE EXCEPTION TYPE zcx_export_where_clause_invali
          EXPORTING
            table        = _table-source_table
            where_clause = _table-where_restriction
            failure_description = osql_syntax_error->msgtext.
      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_export_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

  ENDMETHOD.


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
    name = get_parameter_name( table ).

    TRY.
      tdc->create_parameter( i_param_name = name
        i_param_def = type_definition ).
      CATCH cx_ecatt_tdc_access INTO DATA(failure).
        IF failure->textid = cx_ecatt_tdc_access=>parameter_exists.
          IF tdc->get_param_definition( name ) <> type_definition.
            tdc->change_parameter( EXPORTING i_param_name = name
              i_param_def = type_definition ).
          ENDIF.
        ELSE.
          RAISE EXCEPTION failure.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  method EXPORT.

    TRY.
      DATA(param_for_list) = create_parameter( table = 'ZEXPORT_TABLE_LIST' ).
      set_parameter_value( content = table_list name = param_for_list ).

      tdc->commit_changes( i_commit_mode = abap_false
        i_tr_order = transport_request i_release_lock = abap_true ).

      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_export_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

  endmethod.


  method GET_PARAMETER_NAME.

    result = table_name.
    REPLACE ALL OCCURRENCES OF REGEX '[^A-Za-z0-9_\s]'
      IN result WITH '_'.

  endmethod.


  method SET_PARAMETER_VALUE.

    tdc->set_value( i_param_name = name i_param_value = content
      i_variant_name = variant ).

  endmethod.
ENDCLASS.
