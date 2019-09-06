class ZEXPORT_FILE_MIME_TO_FRONTEND definition
  public
  final
  create public .

public section.

  class-methods EXPORT_TO_SAPGUI_WORK_DIR
    importing
      !MIME_KEY type WWWDATATAB
    raising
      ZCX_EXPORT_TO_FILE
      ZCX_IMPORT_OBJECT_NOT_EXISTS .
  class-methods GET_FILENAME
    importing
      !KEY type WWWDATATAB
    returning
      value(FILENAME) type STRING .
  class-methods DELETE_FROM_SAPGUI_WORK
    importing
      !KEY type WWWDATATAB.
protected section.
private section.

  types:
    _mime_content TYPE STANDARD TABLE OF w3mime .

  class-methods GET_MIME_OBJECT
    importing
      !KEY type WWWDATATAB
    exporting
      !CONTENT type _MIME_CONTENT
      size TYPE i
    raising
      ZCX_IMPORT_OBJECT_NOT_EXISTS .
ENDCLASS.



CLASS ZEXPORT_FILE_MIME_TO_FRONTEND IMPLEMENTATION.


  method DELETE_FROM_SAPGUI_WORK.
    DATA:
          ##NEEDED
          return_code TYPE i.

    cl_gui_frontend_services=>file_delete( EXPORTING
      filename = get_filename( key )
      CHANGING rc = return_code EXCEPTIONS OTHERS = 0 ).

  endmethod.


  method EXPORT_TO_SAPGUI_WORK_DIR.
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

  endmethod.


  method GET_FILENAME.
    DATA: file_separator(1),
          file_extension TYPE wwwparams-value.

    cl_gui_frontend_services=>get_sapgui_workdir( CHANGING sapworkdir = filename ).
    cl_gui_frontend_services=>get_file_separator( CHANGING file_separator = file_separator ).

    SELECT SINGLE value FROM wwwparams INTO file_extension
      WHERE relid = key-relid AND objid = key-objid
      AND name = 'fileextension'.

    filename = filename && file_separator && key-objid && file_extension.

  endmethod.


  method GET_MIME_OBJECT.
    DATA: c_size TYPE wwwparams-value.

    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key = key
      TABLES
        mime = content
      EXCEPTIONS
        import_error = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_import_object_not_exists
        EXPORTING
          testcase_id = key-objid.
    ENDIF.

    SELECT SINGLE value FROM wwwparams INTO c_size
      WHERE relid = key-relid AND objid = key-objid
      AND name = 'filesize'.

    size = c_size.

  endmethod.
ENDCLASS.
