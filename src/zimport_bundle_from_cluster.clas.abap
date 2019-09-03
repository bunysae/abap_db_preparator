class ZIMPORT_BUNDLE_FROM_CLUSTER definition
  public
  final
  create public INHERITING FROM zimport_bundle.

public section.

  methods CONSTRUCTOR
    importing
      !TESTCASE_ID type W3OBJID
    raising
      ZCX_IMPORT_ERROR .
  methods REPLACE_CONTENT_ALL_TABLES REDEFINITION.
  methods ADD_CONTENT_ALL_TABLES REDEFINITION.
protected section.
private section.

  types:
    _tables TYPE STANDARD TABLE OF tabname .

  data CLUSTER_OBJECTS type ABAP_TRANS_SRCBIND_TAB .
  data MIME_KEY type WWWDATATAB .

  methods IMPORT_MIME_OBJECT
    raising
      ZCX_IMPORT_OBJECT_NOT_EXISTS .
  methods GET_FILESIZE
    returning
      value(SIZE) type I .
  methods DESERIALIZE
    importing
      !BINARY_CONTENT type XSTRING .
ENDCLASS.



CLASS ZIMPORT_BUNDLE_FROM_CLUSTER IMPLEMENTATION.


  method ADD_CONTENT_ALL_TABLES.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    LOOP AT cluster_objects REFERENCE INTO DATA(object).

      ASSIGN object->*-value->* TO <con>.

      MODIFY (object->*-name) FROM TABLE <con>.

    ENDLOOP.

  endmethod.


  method CONSTRUCTOR.

    super->constructor( ).
    called_inside_unit_test( ).

    mime_key-relid = 'MI'.
    mime_key-objid = testcase_id.

    import_mime_object( ).

  endmethod.


  method DESERIALIZE.
    DATA: binary_table_content TYPE xstring,
          table_list TYPE _tables.

    IMPORT content = binary_table_content table_list = table_list
      FROM DATA BUFFER binary_content.

    LOOP AT table_list INTO DATA(table).
      APPEND INITIAL LINE TO cluster_objects ASSIGNING FIELD-SYMBOL(<object>).
      <object>-name = table.
      CREATE DATA <object>-value TYPE STANDARD TABLE OF (table).
    ENDLOOP.

    CALL TRANSFORMATION id
      SOURCE XML binary_table_content
      RESULT (cluster_objects).

  endmethod.


  METHOD get_filesize.
    DATA: c_size TYPE wwwparams-value.

    SELECT SINGLE value FROM wwwparams INTO c_size
      WHERE relid = mime_key-relid AND objid = mime_key-objid
      AND name = 'filesize'.

    size = c_size.

  ENDMETHOD.


  method IMPORT_MIME_OBJECT.
    DATA: mime_content TYPE STANDARD TABLE OF w3mime,
          binary_content TYPE xstring.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key = mime_key
      TABLES
        mime = mime_content
      EXCEPTIONS
        import_error = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_import_object_not_exists
        EXPORTING
          testcase_id = mime_key-objid.
    ENDIF.

    DATA(length) = get_filesize( ).

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = length
      IMPORTING
        buffer = binary_content
      TABLES
        binary_tab = mime_content.

    deserialize( binary_content ).

  endmethod.


  method REPLACE_CONTENT_ALL_TABLES.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    LOOP AT cluster_objects REFERENCE INTO DATA(object).

      ASSIGN object->*-value->* TO <con>.

      DELETE FROM (object->*-name).
      INSERT (object->*-name) FROM TABLE <con>.

    ENDLOOP.

  endmethod.
ENDCLASS.
