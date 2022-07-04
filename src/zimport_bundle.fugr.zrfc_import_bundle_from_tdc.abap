FUNCTION zrfc_import_bundle_from_tdc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TDC) TYPE  ETOBJ_NAME
*"     VALUE(TDC_VERSION) TYPE  ETOBJ_VER
*"     VALUE(VARIANT) TYPE  ETVAR_ID
*"     VALUE(CONTENT_HANDLER) TYPE  SEOCMPNAME
*"     VALUE(DO_COMMIT) TYPE  SAP_BOOL DEFAULT ABAP_TRUE
*"  EXCEPTIONS
*"      IMPORT_ERROR
*"      INVALID_CONTENT_HANDLER
*"----------------------------------------------------------------------

  TRY.

      CALL FUNCTION 'ZIMPORT_BUNDLE_FROM_TDC'
        EXPORTING
          tdc             = tdc
          tdc_version     = tdc_version
          variant         = variant
          content_handler = content_handler
          do_commit       = do_commit.

    CATCH zcx_import_error INTO DATA(error).
      MESSAGE error TYPE 'E' RAISING import_error.
    CATCH cx_sy_dyn_call_illegal_method.
      MESSAGE e010(zexport) WITH content_handler RAISING invalid_content_handler.
  ENDTRY.

ENDFUNCTION.
