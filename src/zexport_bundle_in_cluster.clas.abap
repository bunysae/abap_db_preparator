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
    raising
      ZCX_EXPORT_OBJECT_EXISTS .
  "! Add the content of the table to the bundle (uses the builder-pattern).
  "! @parameter table | Database table (transparent table),
  "! where the content lies.
  "! @parameter fake_table | The fake table, which is used in
  "! the unit-test. The import-class <em>zimport_bundle_from_cluster</em>
  "! overwrites the content of the fake-table.
  "! This parameter can be used, if working with class <em>cl_osq_replace</em>
  "! to replace table contents.
  "! If this parameter is omitted, parameter <em>table</em> is used.
  "! @parameter where_restriction | An valid sql-where-clause
  "! for an restriction of the exported rows.
  "! @raising zcx_export_table_duplicate | If a table name is
  "! used more than one time.
  methods ADD_TABLE_TO_BUNDLE
    importing
      !TABLE type TABNAME
      !FAKE_TABLE type TABNAME optional
      !WHERE_RESTRICTION type STRING OPTIONAL
    returning
      value(INSTANCE) type ref to ZEXPORT_BUNDLE_IN_CLUSTER
    RAISING
      zcx_export_table_duplicate.
  methods EXPORT .
  "! Attach the MIME-Object to an workbench order for transportation pruposes
  methods ATTACH_TO_WB_ORDER .
protected section.
private section.

  types:
    _tables TYPE STANDARD TABLE OF tabname .

  data CLUSTER_OBJECTS type ABAP_TRANS_SRCBIND_TAB .
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
  methods GET_TABLE_LIST
    exporting
      !RESULT type _TABLES .
ENDCLASS.



CLASS ZEXPORT_BUNDLE_IN_CLUSTER IMPLEMENTATION.


  method ADD_TABLE_TO_BUNDLE.
    DATA: name TYPE tabname.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    IF fake_table IS NOT INITIAL.
      name = fake_table.
    ELSE.
      name = table.
    ENDIF.
    IF line_exists( cluster_objects[ name = name ] ).
      RAISE EXCEPTION TYPE zcx_export_table_duplicate
        EXPORTING
          table = name.
    ENDIF.

    APPEND INITIAL LINE TO cluster_objects
      ASSIGNING FIELD-SYMBOL(<content>).
    <content>-name = name.

    CREATE DATA <content>-value TYPE STANDARD TABLE OF
      (table).
    ASSIGN <content>-value->* TO <con>.

    SELECT * FROM (table) INTO TABLE <con>
      WHERE (where_restriction).

    instance = me.

  endmethod.


  METHOD attach_to_wb_order.
    DATA: header TYPE STANDARD TABLE OF ko200,
          items TYPE STANDARD TABLE OF e071k.

    header = VALUE #(
      ( pgmid = 'R3TR' object = 'W3' && mime_key-relid obj_name = mime_key-objid
        masterlang = sy-langu ) ).

    CALL FUNCTION 'TR_OBJECTS_CHECK'
      TABLES
        wt_ko200 = header
        wt_e071k = items.

    CALL FUNCTION 'TR_OBJECTS_INSERT'
      TABLES
        wt_ko200 = header
        wt_e071k = items.
  ENDMETHOD.


  method CONSTRUCTOR.

    mime_key-relid = 'MI'.
    mime_key-objid = testcase_id.
    mime_key-devclass = dev_package.

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
    get_table_list( IMPORTING result = DATA(table_list) ).
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


  method GET_TABLE_LIST.

    CLEAR result.

    LOOP AT cluster_objects REFERENCE INTO DATA(object).
      APPEND object->*-name TO result.
    ENDLOOP.

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
