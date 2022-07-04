CLASS zexport_bundle_in_cluster DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM zexport_bundle.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !testcase_id     TYPE w3objid
        !force_overwrite TYPE abap_bool DEFAULT abap_false
        !dev_package     TYPE devclass
        !title           TYPE w3_text
      RAISING
        zcx_export_object_exists .
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
    "! used more than one time,
    "! or the "where_restriction" is invalid.
    METHODS add_table_to_bundle
      IMPORTING
        VALUE(_table)   TYPE zexport_table_list
      RETURNING
        VALUE(instance) TYPE REF TO zexport_bundle_in_cluster
      RAISING
        zcx_export_error .
    METHODS export .
    "! Attach the MIME-Object to an workbench order for transportation pruposes
    METHODS attach_to_wb_order
      RAISING
        zcx_export_error .
    METHODS add_prior_content
      IMPORTING
                VALUE(_table)   TYPE zexport_table_list
                content         TYPE REF TO data
      RETURNING VALUE(instance) TYPE REF TO zexport_bundle_in_cluster
      RAISING
                zcx_export_error.
  PROTECTED SECTION.
    METHODS get_exported_content REDEFINITION.
  PRIVATE SECTION.

    DATA cluster_objects TYPE abap_trans_srcbind_tab .
    DATA mime_key TYPE wwwdatatab .

    METHODS serialize
      RETURNING
        VALUE(result) TYPE REF TO cl_sxml_string_writer .
    METHODS create_mime_object
      IMPORTING
        !content TYPE REF TO cl_sxml_string_writer .
    METHODS set_filesize
      IMPORTING
        !size TYPE i .
ENDCLASS.



CLASS ZEXPORT_BUNDLE_IN_CLUSTER IMPLEMENTATION.


  METHOD add_prior_content.

    IF _table-fake_table IS INITIAL.
      _table-fake_table = _table-source_table.
    ENDIF.
    IF line_exists( cluster_objects[ name = _table-fake_table ] ).
      RAISE EXCEPTION TYPE zcx_export_table_duplicate
        EXPORTING
          table = _table-fake_table.
    ENDIF.

    INSERT _table INTO TABLE table_list.

    APPEND INITIAL LINE TO cluster_objects
      ASSIGNING FIELD-SYMBOL(<content>).
    <content>-name = _table-fake_table.
    <content>-value = content.

    instance = me.

  ENDMETHOD.


  METHOD add_table_to_bundle.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    IF _table-fake_table IS INITIAL.
      _table-fake_table = _table-source_table.
    ENDIF.
    IF line_exists( cluster_objects[ name = _table-fake_table ] ).
      RAISE EXCEPTION TYPE zcx_export_table_duplicate
        EXPORTING
          table = _table-fake_table.
    ENDIF.

    INSERT _table INTO TABLE table_list.

    APPEND INITIAL LINE TO cluster_objects
      ASSIGNING FIELD-SYMBOL(<content>).
    <content>-name = _table-fake_table.

    CREATE DATA <content>-value TYPE STANDARD TABLE OF
      (_table-source_table).
    ASSIGN <content>-value->* TO <con>.

    select( EXPORTING table_conjunction = _table
      IMPORTING content = <con> ).

    instance = me.

  ENDMETHOD.


  METHOD attach_to_wb_order.
    DATA: header TYPE STANDARD TABLE OF ko200,
          items  TYPE STANDARD TABLE OF e071k.

    header = VALUE #(
      ( pgmid = 'R3TR' object = 'W3' && mime_key-relid obj_name = mime_key-objid
        masterlang = sy-langu devclass = mime_key-devclass ) ).

    CALL FUNCTION 'TR_OBJECTS_CHECK'
      TABLES
        wt_ko200                = header
        wt_e071k                = items
      EXCEPTIONS
        cancel_edit_other_error = 2
        show_only_other_error   = 4.
    IF sy-subrc <> 0.
      zcx_export_error=>wrap_t100_message( ).
    ENDIF.

    CALL FUNCTION 'TR_OBJECTS_INSERT'
      TABLES
        wt_ko200                = header
        wt_e071k                = items
      EXCEPTIONS
        cancel_edit_other_error = 2
        show_only_other_error   = 4.
    IF sy-subrc <> 0.
      zcx_export_error=>wrap_t100_message( ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    mime_key-relid = 'MI'.
    mime_key-objid = testcase_id.
    mime_key-devclass = dev_package.
    mime_key-text = title.

    IF force_overwrite = abap_false.
      SELECT COUNT(*) FROM wwwdata WHERE relid = @mime_key-relid
        AND objid = @mime_key-objid.
      IF sy-subrc = 0.
        RAISE EXCEPTION TYPE zcx_export_object_exists
          EXPORTING
            testcase_id = testcase_id.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD create_mime_object.
    DATA: mime_content   TYPE STANDARD TABLE OF w3mime,
          binary_content TYPE xstring,
          length         TYPE i.

    DATA(binary_table_content) = content->get_output( ).

    " exporting the table list is necessary for deserialization
    EXPORT content = binary_table_content table_list = table_list
      TO DATA BUFFER binary_content.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = binary_content
      IMPORTING
        output_length = length
      TABLES
        binary_tab    = mime_content.

    CALL FUNCTION 'WWWDATA_EXPORT'
      EXPORTING
        key  = mime_key
      TABLES
        mime = mime_content.

    set_filesize( length ).

  ENDMETHOD.


  METHOD export.

    create_mime_object( serialize( ) ).

  ENDMETHOD.


  METHOD get_exported_content.

    READ TABLE cluster_objects ASSIGNING FIELD-SYMBOL(<content>)
      WITH KEY name = table_conjunction-fake_table.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_export_invalid_name
        EXPORTING
          name = table_conjunction-fake_table.
    ENDIF.
    table_for_all_entries = <content>-value.

  ENDMETHOD.


  METHOD serialize.

    result = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).

    CALL TRANSFORMATION id
      SOURCE (cluster_objects)
      RESULT XML result.

  ENDMETHOD.


  METHOD set_filesize.
    DATA: filesize_param TYPE wwwparams.

    filesize_param-relid = mime_key-relid.
    filesize_param-objid = mime_key-objid.
    filesize_param-name = 'filesize'.
    filesize_param-value = size.
    MODIFY wwwparams FROM filesize_param.

  ENDMETHOD.
ENDCLASS.
