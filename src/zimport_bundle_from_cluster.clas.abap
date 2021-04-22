CLASS zimport_bundle_from_cluster DEFINITION
  PUBLIC
  INHERITING FROM zimport_bundle
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !testcase_id TYPE w3objid
      RAISING
        zcx_import_error .
    METHODS get_exported_content_for_table
      IMPORTING
        source_table      TYPE tabname
      EXPORTING
        content           TYPE REF TO data
        table_conjunction TYPE zexport_table_list
      RAISING
        zcx_import_error.

    METHODS add_content_all_tables
         REDEFINITION .
    METHODS replace_content_all_tables
         REDEFINITION .
    METHODS replace_content_completly
         REDEFINITION .
  PROTECTED SECTION.

    METHODS get_exported_content REDEFINITION.
  PRIVATE SECTION.

    DATA cluster_objects TYPE abap_trans_srcbind_tab .
    DATA mime_key TYPE wwwdatatab .

    METHODS import_mime_object
      RAISING
        zcx_import_object_not_exists .
    METHODS get_filesize
      RETURNING
        VALUE(size) TYPE i .
    METHODS deserialize
      IMPORTING
        !binary_content TYPE xstring .
ENDCLASS.



CLASS ZIMPORT_BUNDLE_FROM_CLUSTER IMPLEMENTATION.


  METHOD add_content_all_tables.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    permission_is_granted( ).
    LOOP AT cluster_objects REFERENCE INTO DATA(object).

      ASSIGN object->*-value->* TO <con>.

      MODIFY (object->*-name) FROM TABLE <con>.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mime_key-relid = 'MI'.
    mime_key-objid = testcase_id.

    import_mime_object( ).

  ENDMETHOD.


  METHOD deserialize.
    DATA: binary_table_content TYPE xstring.

    IMPORT content = binary_table_content table_list = table_list
      FROM DATA BUFFER binary_content.

    LOOP AT table_list REFERENCE INTO DATA(_table).
      APPEND INITIAL LINE TO cluster_objects ASSIGNING FIELD-SYMBOL(<object>).
      <object>-name = _table->*-fake_table.
      CREATE DATA <object>-value TYPE STANDARD TABLE OF (_table->*-fake_table).
    ENDLOOP.

    CALL TRANSFORMATION id
      SOURCE XML binary_table_content
      RESULT (cluster_objects).

  ENDMETHOD.


  METHOD get_exported_content.

    READ TABLE cluster_objects REFERENCE INTO DATA(object)
      WITH KEY name = table_conjunction-fake_table.
    content = object->*-value.

  ENDMETHOD.


  METHOD get_exported_content_for_table.

    READ TABLE table_list INTO table_conjunction
      WITH KEY source_table = source_table.

    get_exported_content( EXPORTING table_conjunction = table_conjunction
      IMPORTING content = content ).

  ENDMETHOD.


  METHOD get_filesize.
    DATA: c_size TYPE wwwparams-value.

    SELECT SINGLE value FROM wwwparams INTO c_size
      WHERE relid = mime_key-relid AND objid = mime_key-objid
      AND name = 'filesize'.

    size = c_size.

  ENDMETHOD.


  METHOD import_mime_object.
    DATA: mime_content   TYPE STANDARD TABLE OF w3mime,
          binary_content TYPE xstring.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key          = mime_key
      TABLES
        mime         = mime_content
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
        buffer       = binary_content
      TABLES
        binary_tab   = mime_content.

    deserialize( binary_content ).

  ENDMETHOD.


  METHOD replace_content_all_tables.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    permission_is_granted( ).
    LOOP AT cluster_objects REFERENCE INTO DATA(object).

      ASSIGN object->*-value->* TO <con>.

      READ TABLE table_list REFERENCE INTO DATA(_table)
        WITH KEY fake_table = CONV tabname( object->*-name ).
      ASSERT FIELDS object->*-name CONDITION sy-subrc = 0.

      delete( _table->* ).
      INSERT (object->*-name) FROM TABLE <con>.

    ENDLOOP.

  ENDMETHOD.


  METHOD replace_content_completly.
    FIELD-SYMBOLS: <con> TYPE STANDARD TABLE.

    permission_is_granted( ).
    LOOP AT cluster_objects REFERENCE INTO DATA(object).

      ASSIGN object->*-value->* TO <con>.

      DELETE FROM (object->*-name).
      INSERT (object->*-name) FROM TABLE <con>.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
