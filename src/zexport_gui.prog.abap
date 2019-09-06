*&---------------------------------------------------------------------*
*& Report  ZEXPORT_GUI
*& Export the bundle in an cluster or in test data container
*&---------------------------------------------------------------------*
REPORT zexport_gui MESSAGE-ID zexport.

TYPES: BEGIN OF _table,
         name              TYPE tabname,
         fake              TYPE tabname,
         where_restriction TYPE string,
         marked            TYPE abap_bool,
       END OF _table.

CONSTANTS: transparent_table TYPE dd02v-tabclass VALUE 'TRANSP'.

CONTROLS: bundle_cluster TYPE TABLEVIEW USING SCREEN '0001',
          bundle_tdc     TYPE TABLEVIEW USING SCREEN '0002',
          main_tabstrip  TYPE TABSTRIP.
DATA: active_screen_no_ts TYPE sy-dynnr VALUE '0001'.

DATA: BEGIN OF header_cluster,
        testcase_id TYPE w3objid,
        package     TYPE devclass,
        overwrite   TYPE abap_bool,
      END OF header_cluster,
      BEGIN OF header_tdc,
        name      TYPE etobj_name,
        version   TYPE etobj_ver,
        variant   TYPE etvar_id,
        tr_order  TYPE e070-trkorr,
        package   TYPE devclass,
        overwrite TYPE abap_bool,
      END OF header_tdc.
DATA: table       TYPE _table,
      bundle      TYPE STANDARD TABLE OF _table,
      is_changed  TYPE abap_bool,
      marked_rows TYPE STANDARD TABLE OF i.

START-OF-SELECTION.

  CALL SCREEN '9000'.

MODULE check_table_names_0001 INPUT.

  is_changed = abap_true.
  PERFORM check_table_names.
  MODIFY bundle FROM table INDEX bundle_cluster-current_line.

ENDMODULE.

MODULE check_table_names_0002 INPUT.

  is_changed = abap_true.
  PERFORM check_table_names.
  MODIFY bundle FROM table INDEX bundle_tdc-current_line.

ENDMODULE.

MODULE status OUTPUT.

  SET PF-STATUS 'MODIFY'.

ENDMODULE.

MODULE initialize OUTPUT.
  CLEAR marked_rows.
ENDMODULE.

MODULE exit_command_9000 INPUT.

  PERFORM exit_command_9000.

ENDMODULE.

FORM exit_command_9000.
  DATA: procedure TYPE char30.

  procedure = 'EXIT_COMMAND_' && active_screen_no_ts.
  PERFORM (procedure) IN PROGRAM (sy-repid).

ENDFORM.

FORM user_command_9000.
  DATA: procedure TYPE char30.

  procedure = 'USER_COMMAND_' && active_screen_no_ts.
  PERFORM (procedure) IN PROGRAM (sy-repid).

ENDFORM.

MODULE user_command_9000 INPUT.

  CASE sy-ucomm.
    WHEN 'CLUSTER'.
      active_screen_no_ts = '0001'.
      main_tabstrip-activetab = 'CLUSTER'.
    WHEN 'TDC'.
      active_screen_no_ts = '0002'.
      main_tabstrip-activetab = 'TDC'.
    WHEN OTHERS.
      PERFORM user_command_9000.
  ENDCASE.

ENDMODULE.

MODULE exit_command_0001 INPUT.
  PERFORM exit_command_0001.
ENDMODULE.

FORM exit_command_0001.
  DATA: answer TYPE char1.

  TRY.
      PERFORM save_cancel_or_discard USING 'EXPORT_SCREEN_0001'
        CHANGING answer.

      IF answer = 'A'.
        RETURN.
      ENDIF.
      LEAVE PROGRAM.
    CATCH zcx_export_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

MODULE exit_command_0002 INPUT.
  PERFORM exit_command_0002.
ENDMODULE.

FORM exit_command_0002.
  DATA: answer TYPE char1.

  TRY.
      PERFORM save_cancel_or_discard USING 'EXPORT_SCREEN_0002'
        CHANGING answer.

      IF answer = 'A'.
        RETURN.
      ENDIF.
      LEAVE PROGRAM.
    CATCH cx_ecatt_tdc_access INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

MODULE delete_row_0001 INPUT.

  IF table-marked = abap_true.
    INSERT bundle_cluster-current_line INTO TABLE marked_rows.
  ENDIF.

ENDMODULE.

MODULE delete_row_0002 INPUT.

  IF table-marked = abap_true.
    INSERT bundle_tdc-current_line INTO TABLE marked_rows.
  ENDIF.

ENDMODULE.

MODULE user_commmand_0001 INPUT.
  PERFORM user_command_0001.
ENDMODULE.

FORM user_command_0001.

  TRY.
      CASE sy-ucomm.
        WHEN 'READ'.
          PERFORM read_bundle_cluster.
        WHEN 'SAVE'.
          PERFORM export_screen_0001.
        WHEN 'REFRESH'.
          CLEAR bundle.
          REFRESH CONTROL 'BUNDLE_CLUSTER' FROM SCREEN '0001'.
        WHEN 'ADD_ROW'.
          APPEND INITIAL LINE TO bundle.
          REFRESH CONTROL 'BUNDLE_CLUSTER' FROM SCREEN '0001'.
        WHEN 'DELETE_ROW'.
          LOOP AT marked_rows INTO DATA(marked).
            DELETE bundle INDEX marked.
          ENDLOOP.
          REFRESH CONTROL 'BUNDLE_CLUSTER' FROM SCREEN '0001'.
      ENDCASE.
    CATCH zcx_export_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

MODULE user_commmand_0002 INPUT.
  PERFORM user_command_0002.
ENDMODULE.

FORM user_command_0002.

  TRY.
      CASE sy-ucomm.
        WHEN 'READ'.
          PERFORM read_bundle_tdc.
        WHEN 'SAVE'.
          PERFORM export_screen_0002.
        WHEN 'REFRESH'.
          CLEAR bundle.
          REFRESH CONTROL 'BUNDLE_TDC' FROM SCREEN '0002'.
        WHEN 'ADD_ROW'.
          APPEND INITIAL LINE TO bundle.
          REFRESH CONTROL 'BUNDLE_TDC' FROM SCREEN '0002'.
        WHEN 'DELETE_ROW'.
          LOOP AT marked_rows INTO DATA(marked).
            DELETE bundle INDEX marked.
          ENDLOOP.
          REFRESH CONTROL 'BUNDLE_TDC' FROM SCREEN '0002'.
      ENDCASE.
    CATCH zcx_export_error INTO DATA(export_error).
      MESSAGE export_error TYPE 'S' DISPLAY LIKE 'E'.
    CATCH cx_ecatt_tdc_access INTO DATA(tdc_error).
      MESSAGE tdc_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

DEFINE check_name.

  CALL FUNCTION 'DDIF_NAMETAB_GET'
    EXPORTING
      tabname   = &1
    IMPORTING
      ddobjtype = object_type
    EXCEPTIONS
      not_found = 4.
  IF sy-subrc <> 0.
    MESSAGE e004 WITH &1.
  ENDIF.
  IF object_type <> transparent_table.
    MESSAGE e005 WITH &1.
  ENDIF.

END-OF-DEFINITION.

FORM read_bundle_cluster.

  TRY.
      DATA(importer) = NEW zimport_bundle_from_cluster( header_cluster-testcase_id ).

      CLEAR: bundle.
      LOOP AT importer->table_list REFERENCE INTO DATA(table).
        APPEND VALUE #( name = table->*-source_table fake = table->*-fake_table
          where_restriction = table->*-where_restriction ) TO bundle.
      ENDLOOP.
    CATCH zcx_import_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

FORM read_bundle_tdc.

  TRY.
      DATA(importer) = NEW zimport_bundle_from_tdc(
        tdc = header_tdc-name tdc_version = header_tdc-version
        variant = header_tdc-variant ).

      CLEAR: bundle.
      LOOP AT importer->table_list REFERENCE INTO DATA(table).
        APPEND VALUE #( name = table->*-source_table fake = table->*-fake_table
          where_restriction = table->*-where_restriction ) TO bundle.
      ENDLOOP.
    CATCH zcx_import_error INTO DATA(error).
      MESSAGE error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

FORM check_table_names.
  DATA: object_type TYPE dd02v-tabclass.

  check_name table-name.
  IF table-fake IS NOT INITIAL.
    check_name table-fake.
  ENDIF.

ENDFORM.

FORM create_cluster_exporter
  CHANGING exporter TYPE REF TO zexport_bundle_in_cluster
  RAISING zcx_export_error.

  exporter = NEW zexport_bundle_in_cluster(
    testcase_id = header_cluster-testcase_id
    force_overwrite = header_cluster-overwrite
    dev_package = header_cluster-package ).

ENDFORM.

FORM create_tdc_exporter
  CHANGING exporter TYPE REF TO zexport_bundle_in_tdc
  RAISING cx_ecatt_tdc_access zcx_export_object_exists.
  DATA: tdc TYPE REF TO cl_apl_ecatt_tdc_api.

  " check for version again, because empty version causes
  " the exception cx_ecatt_tdc_access. The exception cx_ecatt_tdc_access
  " should only been thrown, if the tdc doesn't exists
  IF header_tdc-version IS INITIAL.
    RAISE EXCEPTION TYPE cx_ecatt_tdc_access
      EXPORTING
        textid     = cx_ecatt_tdc_access=>version_not_found
        last_obj_ver = space
        last_obj_type = conv string( cl_apl_ecatt_const=>obj_type_test_data )
        last_obj_name = header_tdc-name.
  ENDIF.

  TRY.
      tdc = cl_apl_ecatt_tdc_api=>get_instance( EXPORTING
        i_testdatacontainer = header_tdc-name
        i_testdatacontainer_version = header_tdc-version
        i_write_access = abap_true ).

      IF header_tdc-overwrite = abap_false.
        " container exists and shouldn't be overwritten
        RAISE EXCEPTION TYPE zcx_export_object_exists
          EXPORTING
            textid   = zcx_export_object_exists=>tdc_exists
            tdc_name = header_tdc-name.
      ENDIF.

    CATCH cx_ecatt_tdc_access.

      cl_apl_ecatt_tdc_api=>create_tdc( EXPORTING i_tr_order = header_tdc-tr_order
        i_name = header_tdc-name i_version = header_tdc-version
        i_tadir_devclass = header_tdc-package i_write_access = abap_true
        IMPORTING e_tdc_ref = tdc ).

  ENDTRY.

  " create variant, if not exists
  DATA(variant_list) = tdc->get_variant_list( ).
  IF NOT line_exists( variant_list[ table_line = header_tdc-variant ] ).
    tdc->create_variant( i_variant_name = header_tdc-variant ).
  ENDIF.

  exporter = NEW zexport_bundle_in_tdc( tdc = tdc variant = header_tdc-variant ).

ENDFORM.

FORM export_screen_0001 RAISING zcx_export_error.
  DATA: exporter TYPE REF TO zexport_bundle_in_cluster.

  IF bundle IS INITIAL.
    MESSAGE s002.
    RETURN.
  ENDIF.
  PERFORM create_cluster_exporter CHANGING exporter.

  LOOP AT bundle INTO table.
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = table-name fake_table = table-fake
      where_restriction = table-where_restriction ) ).
  ENDLOOP.
  exporter->export( ).
  exporter->attach_to_wb_order( ).

  is_changed = abap_false.
  COMMIT WORK.

ENDFORM.

FORM export_screen_0002 RAISING cx_ecatt_tdc_access zcx_export_object_exists.
  DATA exporter TYPE REF TO zexport_bundle_in_tdc.

  IF bundle IS INITIAL.
    MESSAGE s002.
    RETURN.
  ENDIF.
  PERFORM create_tdc_exporter CHANGING exporter.

  LOOP AT bundle INTO table.
    exporter->add_table_to_bundle( _table = VALUE #(
      source_table = table-name fake_table = table-fake
      where_restriction = table-where_restriction ) ).
  ENDLOOP.
  exporter->export( transport_request = header_tdc-tr_order ).

  is_changed = abap_false.
  COMMIT WORK.

ENDFORM.

FORM save_cancel_or_discard USING export_procedure TYPE char30
  CHANGING answer TYPE char1
  RAISING cx_static_check.

  IF is_changed = abap_false.
    RETURN.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar      = text-sav
      text_question = text-sav
    IMPORTING
      answer        = answer.

  CASE answer.
    WHEN 'A'.
      RETURN.
    WHEN '1'.
      PERFORM (export_procedure) IN PROGRAM (sy-repid).
  ENDCASE.

ENDFORM.
