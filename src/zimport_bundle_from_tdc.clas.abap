CLASS zimport_bundle_from_tdc DEFINITION
  PUBLIC
  INHERITING FROM zimport_bundle
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !tdc         TYPE etobj_name
        !tdc_version TYPE etobj_ver OPTIONAL
        !variant     TYPE etvar_id
      RAISING
        zcx_import_error .

    METHODS add_content_all_tables
         REDEFINITION .
    METHODS replace_content_all_tables
         REDEFINITION .
    METHODS replace_content_completly
         REDEFINITION .
  PROTECTED SECTION.

    METHODS get_exported_content
         REDEFINITION .
  PRIVATE SECTION.

    DATA tdc TYPE REF TO cl_apl_ecatt_tdc_api .
    DATA variant TYPE etvar_id .

    METHODS get_tdc_parameter_name
      IMPORTING
                table         TYPE zexport_table_list
      RETURNING VALUE(result) TYPE etp_name.
ENDCLASS.



CLASS ZIMPORT_BUNDLE_FROM_TDC IMPLEMENTATION.


  METHOD add_content_all_tables.
    DATA: content TYPE REF TO data.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    permission_is_granted( ).
    TRY.

        LOOP AT table_list REFERENCE INTO DATA(table).

          IF zexport_utils=>is_cds_view_entity( table->*-source_table ) = abap_true.
            CONTINUE.
          ENDIF.

          CREATE DATA content TYPE STANDARD TABLE OF (table->*-fake_table).
          ASSIGN content->* TO <con>.
          tdc->get_value( EXPORTING i_param_name = get_tdc_parameter_name( table->* )
            i_variant_name = variant
            CHANGING e_param_value = <con> ).

          " If no content is exported in other than the "ECATTDEFAULT"-variant,
          " <con> contains the parameter value of the "ECATTDEFAULT"-variant.
          " Therefore INSERT-Statement is only executed, when the exported content is not initial.
          IF table->*-is_initial = abap_false.
            MODIFY (table->*-fake_table) FROM TABLE <con>.
          ENDIF.

        ENDLOOP.
      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_import_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    TRY.
        me->tdc = cl_apl_ecatt_tdc_api=>get_instance( i_testdatacontainer = tdc
          i_testdatacontainer_version = tdc_version ).
        me->variant = variant.

      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_import_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

    TRY.
        " The tables are read from the parameter "ZEXPORT_TABLE_LIST".
        " The parameter-list is not used, because different variants can use
        " different parameters and some parameters may be not database-tables.
        me->tdc->get_value( EXPORTING i_param_name = 'ZEXPORT_TABLE_LIST' i_variant_name = variant
          CHANGING e_param_value = table_list ).
        ##NO_HANDLER
      CATCH cx_ecatt_tdc_access.
    ENDTRY.


  ENDMETHOD.


  METHOD get_exported_content.

    TRY.
        tdc->get_value_ref( EXPORTING i_param_name = get_tdc_parameter_name( table_conjunction )
          i_variant_name = variant
          CHANGING e_param_ref = content ).

      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_import_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_tdc_parameter_name.

    " backwards-compatibility: until commit
    " 550688c21ca119ffaecd4e1c11a68ab504fc53ee
    " tdc-parameter-name was the fake-table name.
    " So 'table-tdc_parameter_name' can be empty.
    IF table-tdc_parameter_name IS INITIAL.
      result = table-fake_table.
    ELSE.
      result = table-tdc_parameter_name.
    ENDIF.

  ENDMETHOD.


  METHOD replace_content_all_tables.
    DATA: content TYPE REF TO data.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    permission_is_granted( ).
    TRY.

        LOOP AT table_list REFERENCE INTO DATA(table).

          IF zexport_utils=>is_cds_view_entity( table->*-source_table ) = abap_true.
            CONTINUE.
          ENDIF.

          CREATE DATA content TYPE STANDARD TABLE OF (table->*-fake_table).
          ASSIGN content->* TO <con>.
          tdc->get_value( EXPORTING i_param_name = get_tdc_parameter_name( table->* )
            i_variant_name = variant
            CHANGING e_param_value = <con> ).

          delete( table->* ).
          " If no content is exported in other than the "ECATTDEFAULT"-variant,
          " <con> contains the parameter value of the "ECATTDEFAULT"-variant.
          " Therefore INSERT-Statement is only executed, when the exported content is not initial.
          IF table->*-is_initial = abap_false.
            MODIFY (table->*-fake_table) FROM TABLE <con>.
          ENDIF.

        ENDLOOP.

      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_import_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

  ENDMETHOD.


  METHOD replace_content_completly.
    DATA: content TYPE REF TO data.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    permission_is_granted( ).
    TRY.

        LOOP AT table_list REFERENCE INTO DATA(table).

          IF zexport_utils=>is_cds_view_entity( table->*-source_table ) = abap_true
              OR table->*-fake_table NOT IN get_whitelist( ).
            CONTINUE.
          ENDIF.

          CREATE DATA content TYPE STANDARD TABLE OF (table->*-fake_table).
          ASSIGN content->* TO <con>.
          tdc->get_value( EXPORTING i_param_name = get_tdc_parameter_name( table->* )
            i_variant_name = variant
            CHANGING e_param_value = <con> ).

          DELETE FROM (table->*-fake_table).
          " If no content is exported in other than the "ECATTDEFAULT"-variant,
          " <con> contains the parameter value of the "ECATTDEFAULT"-variant.
          " Therefore INSERT-Statement is only executed, when the exported content is not initial.
          IF table->*-is_initial = abap_false.
            INSERT (table->*-fake_table) FROM TABLE <con>.
          ENDIF.

        ENDLOOP.

      CATCH cx_ecatt_tdc_access INTO DATA(ecatt_failure).
        zcx_import_error=>wrap_ecatt_failure( ecatt_failure ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
