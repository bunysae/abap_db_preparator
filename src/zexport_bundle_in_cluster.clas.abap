class ZEXPORT_BUNDLE_IN_CLUSTER definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TESTCASE_ID type W3OBJID
      !FORCE_OVERWRITE type ABAP_BOOL default ABAP_FALSE
      dev_package TYPE devclass
      title type w3_text
    raising
      ZCX_EXPORT_OBJECT_EXISTS .
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
  methods ADD_TABLE_TO_BUNDLE
    importing
      VALUE(_table) TYPE zexport_table_list
    returning
      value(INSTANCE) type ref to ZEXPORT_BUNDLE_IN_CLUSTER
    RAISING
      zcx_export_error.
  methods EXPORT .
  "! Attach the MIME-Object to an workbench order for transportation pruposes
  methods ATTACH_TO_WB_ORDER
    RAISING
      zcx_export_error.
protected section.
private section.

  data CLUSTER_OBJECTS type ABAP_TRANS_SRCBIND_TAB .
  DATA table_list TYPE STANDARD TABLE OF zexport_table_list.
  data MIME_KEY type WWWDATATAB .

  methods SERIALIZE
    returning
      value(RESULT) type ref to CL_SXML_STRING_WRITER .
  methods CREATE_MIME_OBJECT
    importing
      !CONTENT type ref to CL_SXML_STRING_WRITER .
  methods SET_FILESIZE
    importing
      !SIZE type I .
ENDCLASS.



CLASS ZEXPORT_BUNDLE_IN_CLUSTER IMPLEMENTATION.


  method ADD_TABLE_TO_BUNDLE.
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

    TRY.
      SELECT * FROM (_table-source_table) INTO TABLE @<con>
        WHERE (_table-where_restriction).
      CATCH cx_sy_dynamic_osql_error INTO DATA(osql_syntax_error).
        RAISE EXCEPTION TYPE zcx_export_where_clause_invali
          EXPORTING
            table = _table-source_table
            where_clause = _table-where_restriction
            failure_description = osql_syntax_error->msgtext.
    ENDTRY.

    instance = me.

  endmethod.


  METHOD attach_to_wb_order.
    DATA: header TYPE STANDARD TABLE OF ko200,
          items TYPE STANDARD TABLE OF e071k.

    header = VALUE #(
      ( pgmid = 'R3TR' object = 'W3' && mime_key-relid obj_name = mime_key-objid
        masterlang = sy-langu devclass = mime_key-devclass ) ).

    CALL FUNCTION 'TR_OBJECTS_CHECK'
      TABLES
        wt_ko200 = header
        wt_e071k = items
      EXCEPTIONS
        cancel_edit_other_error = 2
        show_only_other_error = 4.
    IF sy-subrc <> 0.
      zcx_export_error=>wrap_t100_message( ).
    ENDIF.

    CALL FUNCTION 'TR_OBJECTS_INSERT'
      TABLES
        wt_ko200 = header
        wt_e071k = items
      EXCEPTIONS
        cancel_edit_other_error = 2
        show_only_other_error = 4.
    IF sy-subrc <> 0.
      zcx_export_error=>wrap_t100_message( ).
    ENDIF.

  ENDMETHOD.


  method CONSTRUCTOR.

    mime_key-relid = 'MI'.
    mime_key-objid = testcase_id.
    mime_key-devclass = dev_package.
    mime_key-text = title.

    IF force_overwrite = abap_false.
      SELECT COUNT(*) FROM wwwdata WHERE relid = mime_key-relid
        AND objid = mime_key-objid.
      IF sy-subrc = 0.
        RAISE EXCEPTION TYPE zcx_export_object_exists
          EXPORTING
            testcase_id = testcase_id.
      ENDIF.
    ENDIF.

  endmethod.


  method CREATE_MIME_OBJECT.
    DATA: mime_content TYPE STANDARD TABLE OF w3mime,
          binary_content TYPE xstring,
          length TYPE i.

    DATA(binary_table_content) = content->get_output( ).

    " exporting the table list is necessary for deserialization
    EXPORT content = binary_table_content table_list = table_list
      TO DATA BUFFER binary_content.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer = binary_content
      IMPORTING
        output_length = length
      TABLES
        binary_tab = mime_content.

    CALL FUNCTION 'WWWDATA_EXPORT'
      EXPORTING
        key = mime_key
      TABLES
        mime = mime_content.

    set_filesize( length ).

  endmethod.


  method EXPORT.

    create_mime_object( serialize( ) ).

  endmethod.


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
