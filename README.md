# ABAP Database preparator #
Database preparation for unit testing made simple. Store snapshots 
(bundles) of your database records for later use in unit tests.

## No more breaking unit tests ##
The snapshots should avoid breaking unit tests after system copies and 
other database manipulations. This is achieved by providing a wrapper
for the OpenSQL replacement service in class `cl_osql_replace`, that 
makes huge setups unnecessary.

### Good bye huge setups ###
Replace this huge setup method
```ABAP
METHOD setup.
  DATA: airlines TYPE STANDARD TABLE OF zcarr_fake,
        replaced_tables TYPE cl_osql_replace=>tt_replacement.

  DELETE FROM airlines.
  airlines = VALUE #(
    ( carrid = 'TG' carrname = 'Thai Airways' )
  ).
  INSERT zcarr_fake FROM airlines.

  replaced_tables = VALUE #(
    ( source = 'SCARR' target = 'ZCARR_FAKE' )
  ).
  cl_osql_replace=>activate_replacement( EXPORTING
    replacement_table = replaced_tables ).

ENDMETHOD.
```
by just two method calls:
```ABAP
METHOD setup.

  DATA(db_preparator) = NEW zimport_bundle_from_tdc( tdc = 'ZAIRLINES'
    tdc_version = 1 variant = 'ECATTDEFAULT' ).
  db_preparator->replace_content_completly( ).
  db_preparator->activate_osql_replacement( ).

ENDMETHOD.
```

## How it works ##
Bundles are stored either in an ECATT test data container
or in a cluster (binary MIME-object in transaction smw0).

### Export step ###
In the first step we can choose the database records,
which should be exported into the bundle.
The OpenSQL replacement service needs a so called fake table with the same
structure as the original table. From this fake table the database records are
read or they are written back in this fake table. In the export step we link
original and fake table as shown in the picture below.
![program zexport_gui](img/export_scarr.png)
*Figure 1 Export step for table scarr in program `zexport_gui`*
In figure 1 the original table is named `scarr`, the fake table `zcarr_fake`.

Fake tables can be left empty, if the OpenSQL replacement service isn't
installed.

### Import step ###
In the ABAP unit-testclass the database records exported in previous step
can be imported in the fake tables or in the original tables,
if the corresponding fake table was left empty.
The API for the import step is located in class `zimport_bundle_from_cluster`
for clusters or in class `zimport_bundle_from_tdc` for
ECATT test data container.

For testing Rfc-Client- or Rfc-Serverprograms or programs,
which interact through the [JSON Adapter for ABAP Function Modules](https://github.com/cesar-sap/abap_fm_json/)
a API is located in function-group `zimport_bundle`.

A example can be found in the listing below.
```ABAP
CLASS test_airlines DEFINITION FOR TESTING DURATION SHORT
  RISK LEVEL HARMLESS.
  
  PRIVATE SECTION.

    " activate replacement service	  
    METHODS setup.

    METHODS thai_should_be_found FOR TESTING.

ENDCLASS.

CLASS test_airlines IMPLEMENTATION.

  METHOD setup.

    DATA(db_preparator) = NEW zimport_bundle_from_tdc( tdc = 'ZAIRLINES'
      tdc_version = 1 variant = 'ECATTDEFAULT' ).
    db_preparator->replace_content_completly( ).
    db_preparator->activate_osql_replacement( ).

  ENDMETHOD.

  METHOD thai_should_be_found.
    DATA: exp_airlines TYPE STANDARD TABLE OF scarr.

    exp_airlines = VALUE #( ( carrid = 'TG' carrname = 'Thai airways'
                              currcode = 'THB' url = 'https://thaiairways.com' ) ).

    SELECT * FROM scarr INTO TABLE @DATA(act_airlines).

    cl_abap_unit_assert=>assert_equals( exp = exp_airlines
      act = act_airlines ).

  ENDMETHOD.

ENDCLASS.
```

### Authorization ###
The APIs in class `zimport_bundle_from_cluster` or 
in class `zimport_bundle_from_tdc` can only be used in 
development systems, where 
dangerous ABAP unit-testclasses are enabled.

### Whitelist-check ###
Content of fake tables listed in the whitelist can be completely
overriden. The whitelist is maintained in parameter
`ZIMPORT_REPLACE_WHITELIST` (transaction `stvarv`, table `tvarvc`).

## Terms ##

* Bundle: a collection of database records, which are stored outside
  of the database tables.
* Cluster: a MIME-object in transaction `smw0`, which contains the bundle
* TDC: abbreviation for ECATT test data container

## Cloning this repository ##
Cloning can be done with [abapGit](https://github.com/larshp/abapgit).
The code is backwards compatible until release 7.40. The replacement services
need higher releases (SAP NetWeaver 7.51 or 7.52).

## Further links ##
[Blog entry on SAP community network](https://blogs.sap.com/?p=1049057)
