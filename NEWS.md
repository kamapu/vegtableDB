vegtable2 0.2.1
===============

### New Features

* All the package have been reshaped for new database structure

vegtable2 0.2.0
===============

### New Features

* Roxygenized version

### Improvements

* Functions adapted to new database structure.

vegtable2 0.1.2
===============

### New Features

* New function `report_communities()`
* New function `import_sudamerica()`
* New function `version_svg()`
* New function `get_description()`

### Improvements

* In function `postgres2vegtable()` import of header using `pgGetGeom()` from package `rpostgis`.

### Bug Fixes

* Function `get_precision()` provided wrong information for short strings with NAs

vegtable2 0.1.1
===============

### New Features

* New function `get_precision()` for getting length and precision of numeric vectors to be used in definitions of tables for relational databases

### Improvements

* Function `import_swea()` Adapted to a **PostgreSQL** version of [**SWEA-Dataveg**](http://www.givd.info/ID/AF-00-006)

vegtable2 0.1.0
===============

### New Features

* Very first release of `vegtable2`.
