*&---------------------------------------------------------------------*
*& Report  ZEXPORT_GUI
*& Export the bundle in an cluster or in test data container
*&---------------------------------------------------------------------*
REPORT zexport_gui MESSAGE-ID zexport.

INCLUDE: rddkorri, zexport_batch_input.

TYPES: BEGIN OF _table,
         name              TYPE tabname,
         fake              TYPE tabname,
         where_restriction TYPE string,
         marked            TYPE abap_bool,
       END OF _table.

CONSTANTS: transparent_table TYPE dd02v-tabclass VALUE 'TRANSP',
  cluster_table TYPE dd02v-tabclass VALUE 'CLUSTER',
  pool_table TYPE dd02v-tabclass VALUE 'POOL'.

CONTROLS: bundle_cluster TYPE TABLEVIEW USING SCREEN '0001',
          bundle_tdc     TYPE TABLEVIEW USING SCREEN '0002',
          main_tabstrip  TYPE TABSTRIP.
DATA: active_screen_no_ts TYPE sy-dynnr VALUE '0001'.

DATA: BEGIN OF header_cluster,
        testcase_id TYPE w3objid,
        package     TYPE devclass,
        title       TYPE w3_text,
        overwrite   TYPE abap_bool,
      END OF header_cluster,
      BEGIN OF header_tdc,
        name      TYPE etobj_name,
        version   TYPE etobj_ver,
        variant   TYPE etvar_id,
        title     TYPE twb_title,
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
  IF sy-subrc <> 0.
    APPEND table TO bundle.
  ENDIF.

ENDMODULE.

MODULE check_table_names_0002 INPUT.

  is_changed = abap_true.
  PERFORM check_table_names.
  MODIFY bundle FROM table INDEX bundle_tdc-current_line.
  IF sy-subrc <> 0.
    APPEND table TO bundle.
  ENDIF.

ENDMODULE.

MODULE status OUTPUT.

  SET PF-STATUS 'MODIFY'.
  SET TITLEBAR 'EXPORT'.

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
  CLEAR sy-ucomm.

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
    CATCH cx_static_check INTO DATA(error).
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
    CATCH cx_static_check INTO DATA(error).
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
          PERFORM: read_bundle_cluster, read_title_cluster.
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
          PERFORM: read_bundle_tdc, read_title_tdc.
        WHEN 'SAVE'.
          PERFORM: check_header_tdc, export_screen_0002.
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
        WHEN 'DISPLAY'.
          PERFORM display_tdc_content.
      ENDCASE.
    CATCH zcx_export_error INTO DATA(export_error).
      MESSAGE export_error TYPE 'S' DISPLAY LIKE 'E'.
    CATCH cx_ecatt_tdc_access INTO DATA(tdc_error).
      MESSAGE tdc_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

"! check package existence and if transport order is mandatory
FORM check_header_tdc RAISING zcx_export_error.

  cl_package_helper=>check_package_existence(
    EXPORTING i_package_name = header_tdc-package
    IMPORTING e_package_exists = DATA(exists) ).
  IF exists = abap_false.
    zcx_export_error=>wrap_t100_message( ).
  ENDIF.

  cl_package_helper=>check_package_name(
     EXPORTING i_package_name = header_tdc-package
     IMPORTING e_package_type = DATA(package_type)
     EXCEPTIONS OTHERS = 4 ).
  IF sy-subrc <> 0.
    zcx_export_error=>wrap_t100_message( ).
  ENDIF.

  IF package_type <> '$'.
    " for non-local packages transport order must be specified
    SELECT COUNT(*) FROM e070
      WHERE trkorr = header_tdc-tr_order AND trfunction = 'K'
      AND trstatus = 'D'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_export_tr_order
        EXPORTING
          tr_order = header_tdc-tr_order.
    ENDIF.
  ENDIF.

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
  IF NOT ( object_type = transparent_table OR object_type = cluster_table
    OR object_type = pool_table ).
    MESSAGE e005 WITH &1.
  ENDIF.

END-OF-DEFINITION.

FORM read_bundle_cluster.

  TRY.
      PERFORM read_package_bundle_cluster.
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

FORM read_package_bundle_cluster.

  SELECT SINGLE devclass INTO header_cluster-package FROM tadir
    WHERE pgmid = 'R3TR' AND object = 'W3MI' AND obj_name = header_cluster-testcase_id.

ENDFORM.

FORM read_title_cluster.

  SELECT text UP TO 1 ROWS FROM wwwdata INTO header_cluster-title
    WHERE relid = 'MI' AND objid = header_cluster-testcase_id.
  ENDSELECT.

ENDFORM.

FORM read_bundle_tdc.

  TRY.
      PERFORM read_package_bundle_tdc.

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

"! reads the package and the open transport order for this tdc
FORM read_package_bundle_tdc.
  DATA: task_contains_tdc TYPE e070-trkorr.

  SELECT SINGLE devclass INTO header_tdc-package FROM tadir
    WHERE pgmid = 'R3TR' AND object = cl_apl_ecatt_const=>obj_type_test_data
    AND obj_name = header_tdc-name.

  SELECT trkorr UP TO 1 ROWS INTO task_contains_tdc FROM e071
    WHERE pgmid = 'R3TR' AND object = cl_apl_ecatt_const=>obj_type_test_data
    AND obj_name = header_tdc-name AND lockflag = abap_true.

    SELECT SINGLE strkorr INTO header_tdc-tr_order FROM e070
      WHERE trkorr = task_contains_tdc.

  ENDSELECT.

ENDFORM.

FORM read_title_tdc
  RAISING cx_ecatt_tdc_access.

  CLEAR header_tdc-title.
  DATA(tdc_accessor) = cl_apl_ecatt_tdc_api=>get_instance(
    i_testdatacontainer = header_tdc-name
    i_testdatacontainer_version = header_tdc-version
    i_write_access = abap_true ).

  tdc_accessor->get_tdc_attributes( IMPORTING e_version_dependant_attribs
    = DATA(attributes) ).
  header_tdc-title = attributes-twb_title.

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
    dev_package = header_cluster-package
    title = header_cluster-title ).

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

  PERFORM create_tdc_variant USING tdc.
  PERFORM set_tdc_title USING tdc.

  exporter = NEW zexport_bundle_in_tdc( tdc = tdc variant = header_tdc-variant ).

ENDFORM.

" create variant, if not exists
FORM create_tdc_variant USING VALUE(tdc) TYPE REF TO cl_apl_ecatt_tdc_api
  RAISING cx_ecatt_tdc_access.

  DATA(variant_list) = tdc->get_variant_list( ).
  IF NOT line_exists( variant_list[ table_line = header_tdc-variant ] ).
    tdc->create_variant( i_variant_name = header_tdc-variant ).
  ENDIF.

ENDFORM.

FORM set_tdc_title USING VALUE(tdc) TYPE REF TO cl_apl_ecatt_tdc_api
  RAISING cx_ecatt_tdc_access.

  tdc->set_tdc_attributes( EXPORTING
    i_version_dependant_attribs = VALUE #( twb_title = header_tdc-title )
  ).

ENDFORM.

FORM export_screen_0001 RAISING zcx_export_error.
  DATA: exporter TYPE REF TO zexport_bundle_in_cluster.

  DELETE bundle WHERE name IS INITIAL.
  IF bundle IS INITIAL.
    MESSAGE s002.
    RETURN.
  ENDIF.

  TRY.
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
  CATCH zcx_export_error INTO DATA(failure).
    " rollback necessary, if user canceled the attachment to workbench order
    ROLLBACK WORK.
    RAISE EXCEPTION failure.
  ENDTRY.

ENDFORM.

FORM export_screen_0002 RAISING cx_ecatt_tdc_access zcx_export_error.
  DATA exporter TYPE REF TO zexport_bundle_in_tdc.

  DELETE bundle WHERE name IS INITIAL.
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

FORM show_own_orders.
  DATA: selection         TYPE trwbo_selection,
        new_request_props TYPE trwbo_new_req_props,
        organizer         TYPE trwbo_calling_organizer,
        request_header    TYPE trwbo_request_header,
        title             TYPE trwbo_title.

  selection-client        = sy-mandt.
  "selection-tarsystem     = target_system
  selection-taskfunctions = wbtasktype.
  organizer               = trwbo_wbo.

  selection-reqfunctions  = trco.
  selection-reqstatus     = notrel.
  selection-taskstatus    = notrel.
  selection-connect_req_task_conditions = abap_true.

* parameters for new request
  PERFORM get_new_req_props CHANGING new_request_props.

  title = text-enw.

  CALL FUNCTION 'TR_PRESENT_REQUESTS_SEL_POPUP'
       EXPORTING
            iv_organizer_type    = organizer
            iv_username          = sy-uname
            is_selection         = selection
            iv_title             = title
            is_new_request_props = new_request_props
       IMPORTING
            es_selected_request  = request_header.

  header_tdc-tr_order = request_header-trkorr.

ENDFORM.

FORM get_new_req_props   CHANGING properties TYPE trwbo_new_req_props.

  properties-trfunctions = trco.
  "properties-tarsystem   = target_system.

* task type
  properties-taskfunc = tcol.

* source client
  properties = sy-mandt.

ENDFORM.

FORM show_tdcs.
  DATA: program TYPE progname.

  program = sy-repid.
  cl_gui_ecatt_object_usage=>select_object_f4(
      im_object_type           = 'ECTD'
      im_dfield_obj_name       = 'HEADER_TDC-NAME'
      im_dfield_obj_version    = 'HEADER_TDC-VERSION'
      im_progname              = program
      im_dynnr                 = sy-dynnr
      im_without_personal_list = abap_true ).

ENDFORM.

FORM show_tdc_versions.
  DATA: program TYPE progname.

  program = sy-repid.
  TRY.
    cl_gui_ecatt_object_usage=>select_version_f4(
      im_obj_type           = 'ECTD'
      im_dfield_obj_name       = 'HEADER_TDC-NAME'
      im_dfield_obj_version    = 'HEADER_TDC-VERSION'
      im_progname              = program
      im_dynnr = sy-dynnr ).
    CATCH cx_ecatt_apl INTO DATA(failure).
      MESSAGE failure TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

"! Display the tdc via batch-input
FORM display_tdc_content.

  DATA(bi_handler) = NEW batch_input( ).
  bi_handler->dynpro_head( report = 'SAPLECATT_MAIN' dynpro = '0100' ).
  bi_handler->dynpro_field( name = 'RB_TEST_DATA' fvalue = abap_true ).
  bi_handler->dynpro_field( name = 'ECTD_VER-NAME' fvalue = header_tdc-name ).
  bi_handler->dynpro_field( name = 'ECTD_VER-VERSION' fvalue = header_tdc-version ).

  bi_handler->call_transaction( code = 'SECATT' ).

ENDFORM.

MODULE show_transport_orders INPUT.
  PERFORM show_own_orders.
ENDMODULE.

MODULE show_tdcs INPUT.
  PERFORM show_tdcs.
ENDMODULE.

MODULE show_tdc_versions INPUT.
  PERFORM show_tdc_versions.
ENDMODULE.
