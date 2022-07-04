CLASS zexport_file_mime_to_frontend DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS export_to_sapgui_work_dir
      IMPORTING
        !mime_key TYPE wwwdatatab
      RAISING
        zcx_export_to_file
        zcx_import_object_not_exists .
    CLASS-METHODS get_filename
      IMPORTING
        !key            TYPE wwwdatatab
      RETURNING
        VALUE(filename) TYPE string .
    CLASS-METHODS delete_from_sapgui_work
      IMPORTING
        !key TYPE wwwdatatab.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      _mime_content TYPE STANDARD TABLE OF w3mime .

    CLASS-METHODS get_mime_object
      IMPORTING
        !key     TYPE wwwdatatab
      EXPORTING
        !content TYPE _mime_content
        size     TYPE i
      RAISING
        zcx_import_object_not_exists .
ENDCLASS.



CLASS ZEXPORT_FILE_MIME_TO_FRONTEND IMPLEMENTATION.


  METHOD delete_from_sapgui_work.
    DATA:
          ##NEEDED
          return_code TYPE i.

    cl_gui_frontend_services=>file_delete( EXPORTING
      filename = get_filename( key )
      CHANGING rc = return_code EXCEPTIONS OTHERS = 0 ).

  ENDMETHOD.


  METHOD export_to_sapgui_work_dir.
    DATA subrc TYPE sy-subrc.

    get_mime_object( EXPORTING key = mime_key
      IMPORTING content = DATA(content)
        size = DATA(binary_filesize) ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING bin_filesize = binary_filesize
        filename = get_filename( mime_key ) filetype = 'BIN'
      CHANGING data_tab = content
      EXCEPTIONS
        file_write_error = 2 gui_refuse_filetransfer = 4
        no_authority = 6 access_denied = 8 OTHERS = 10 ).
    subrc = sy-subrc.
    IF sy-subrc <> 0.
      zcx_export_to_file=>download_failed(
        subrc = subrc filename = get_filename( mime_key ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_filename.
    DATA: file_separator(1) TYPE c,
          file_extension TYPE wwwparams-value.

    cl_gui_frontend_services=>get_sapgui_workdir( CHANGING sapworkdir = filename ).
    cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = file_separator ).

    SELECT SINGLE value FROM wwwparams INTO @file_extension
      WHERE relid = @key-relid AND objid = @key-objid
      AND name = 'fileextension'.

    filename = filename && file_separator && key-objid && file_extension.

  ENDMETHOD.


  METHOD get_mime_object.
    DATA: c_size TYPE wwwparams-value.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key          = key
      TABLES
        mime         = content
      EXCEPTIONS
        import_error = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_import_object_not_exists
        EXPORTING
          testcase_id = key-objid.
    ENDIF.

    SELECT SINGLE value FROM wwwparams INTO @c_size
      WHERE relid = @key-relid AND objid = @key-objid
      AND name = 'filesize'.

    size = c_size.

  ENDMETHOD.
ENDCLASS.
