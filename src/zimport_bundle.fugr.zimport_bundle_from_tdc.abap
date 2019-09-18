FUNCTION zimport_bundle_from_tdc .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(TDC) TYPE  ETOBJ_NAME
*"     REFERENCE(TDC_VERSION) TYPE  ETOBJ_VER
*"     REFERENCE(VARIANT) TYPE  ETVAR_ID
*"     REFERENCE(CONTENT_HANDLER) TYPE  SEOCMPNAME
*"     REFERENCE(DO_COMMIT) TYPE  SAP_BOOL DEFAULT ABAP_TRUE
*"  RAISING
*"      ZCX_IMPORT_ERROR
*"      CX_SY_DYN_CALL_ILLEGAL_METHOD
*"----------------------------------------------------------------------

  DATA(importer) = NEW zimport_bundle_from_tdc(
    tdc = tdc tdc_version = tdc_version variant = variant ).

  CALL METHOD importer->(content_handler).

  IF do_commit = abap_true.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFUNCTION.
