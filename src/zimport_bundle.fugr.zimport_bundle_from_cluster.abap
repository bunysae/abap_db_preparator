FUNCTION ZIMPORT_BUNDLE_FROM_CLUSTER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(TESTCASE_ID) TYPE  W3OBJID
*"     REFERENCE(CONTENT_HANDLER) TYPE  SEOCMPNAME
*"     REFERENCE(DO_COMMIT) TYPE  ABAP_BOOL
*"  RAISING
*"      ZCX_IMPORT_ERROR
*"      CX_SY_DYN_CALL_ILLEGAL_METHOD
*"----------------------------------------------------------------------

  DATA(importer) = NEW zimport_bundle_from_cluster( testcase_id ).

  CALL METHOD importer->(content_handler).

  IF do_commit = abap_true.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFUNCTION.
