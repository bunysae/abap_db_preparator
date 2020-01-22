# Contributing #
Contributions are welcome. Please open a issue before submitting a bigger pull-request.

## Styleguide ##
Development should be sustainable (clean-code).
Please stick to [clean-code styleguides](https://github.com/sap/styleguides).
Refactorings are always welcome!

* Please use the Pretty Printer with keywords in upper-case.
* Code should be compatible with release 7.40 or higher.
* Development objects should begin with `zexport` or with `zimport`.

## Testing ##
The classes `zimport_bundle_from_cluster` and `zimport_bundle_from_tdc` 
contains automated tests. The automated tests overwrite the MIME-object `ZBUNDLE_UNIT_TEST` and the ECATT test data container `ZBUNDLE_UNIT_TEST`. Please make sure,
these objects are not used otherwise.