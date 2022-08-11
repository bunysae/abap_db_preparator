CLASS zexport_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      _conditions TYPE STANDARD TABLE OF string .

    CLASS-METHODS get_table_for_all_entries
      IMPORTING
        !table_conjunction TYPE zexport_table_list
      RETURNING
        VALUE(table_name)  TYPE tabname
      EXCEPTIONS
        not_for_all_entries_cond .
    "! Perform select-statement
    "! @parameter table_name | Dictionary-Name of table_for_all_entries
    CLASS-METHODS select
      IMPORTING
        !table_for_all_entries TYPE STANDARD TABLE
        !table_conjunction     TYPE zexport_table_list
        !table_name            TYPE tabname
        select_from_fake       TYPE abap_bool DEFAULT abap_false
      EXPORTING
        result                 TYPE STANDARD TABLE
      RAISING
        zcx_export_error.
    CLASS-METHODS is_cds_view_entity
      IMPORTING
        table_name    TYPE tabname
      RETURNING
        VALUE(result) TYPE sap_bool.
    CLASS-METHODS get_cds_view_name
      IMPORTING
        entity_name    TYPE tabname
      RETURNING
        VALUE(result) TYPE ddstrucobjname.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZEXPORT_UTILS IMPLEMENTATION.


  METHOD get_cds_view_name.

    cl_dd_ddl_handler_factory=>create( )->get_viewname_from_entityname(
      EXPORTING
        ddnames = VALUE #( ( name = entity_name ) )
      IMPORTING
        view_of_entity = DATA(entities) ).
    result = entities[ 1 ]-viewname.

  ENDMETHOD.


  METHOD get_table_for_all_entries.
    CONSTANTS: expression TYPE string VALUE 'FOR ALL ENTRIES IN'.

    DATA(where_restriction) = table_conjunction-where_restriction.
    TRANSLATE where_restriction TO UPPER CASE.
    DATA(length) = strlen( expression ).
    IF strlen( where_restriction ) < length
        OR where_restriction+0(length) <> expression.
      RAISE not_for_all_entries_cond.
    ENDIF.

    SHIFT where_restriction BY length PLACES LEFT.
    FIND FIRST OCCURRENCE OF 'WHERE' IN where_restriction
      MATCH OFFSET length
      IGNORING CASE.
    table_name = where_restriction+0(length).
    CONDENSE table_name NO-GAPS.

  ENDMETHOD.


  METHOD is_cds_view_entity.
    DATA: object_type TYPE dd02v-tabclass.

    CALL FUNCTION 'DDIF_NAMETAB_GET'
      EXPORTING
        tabname = table_name
      IMPORTING
        ddobjtype = object_type
      EXCEPTIONS
        not_found = 4.
    ASSERT sy-subrc = 0.
    result = xsdbool( object_type = 'STOB' ).

  ENDMETHOD.


  METHOD select.
    DATA: offset TYPE i,
          length TYPE i.

    IF table_for_all_entries IS INITIAL.
      RAISE EXCEPTION TYPE zcx_export_empty
        EXPORTING
          table_name = table_name.
    ENDIF.

    CLEAR result.

    FIND FIRST OCCURRENCE OF 'WHERE' IN table_conjunction-where_restriction
      MATCH OFFSET offset
      MATCH LENGTH length
      IGNORING CASE.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_export_where_clause_invali
        EXPORTING
          table               = table_conjunction-source_table
          where_clause        = table_conjunction-where_restriction
          failure_description = CONV string( TEXT-001 ).
    ENDIF.
    offset = offset + length.
    DATA(where_restriction) = table_conjunction-where_restriction+offset.
    REPLACE ALL OCCURRENCES OF table_name IN where_restriction
      WITH 'table_for_all_entries'
      IGNORING CASE.

    DATA(select_table) = COND tabname( WHEN select_from_fake = abap_true
      THEN table_conjunction-fake_table
      ELSE table_conjunction-source_table ).
    TRY.
        SELECT * FROM (select_table)
          INTO CORRESPONDING FIELDS OF TABLE result
          FOR ALL ENTRIES IN table_for_all_entries
          WHERE (where_restriction).
      CATCH cx_sy_dynamic_osql_error INTO DATA(osql_syntax_error).
        RAISE EXCEPTION TYPE zcx_export_where_clause_invali
          EXPORTING
            table               = table_conjunction-source_table
            where_clause        = table_conjunction-where_restriction
            failure_description = osql_syntax_error->msgtext.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
