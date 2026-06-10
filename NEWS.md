# collar 0.0.7

* `ats_logout()` no longer errors on Windows when the logout request fails with a schannel `SEC_E_CONTEXT_EXPIRED` error under newer libcurl (bundled with R >= 4.5); the transport error is now caught and the local session is cleared by resetting the connection handle (curl#18029).

# collar 0.0.4

* updates ATS functionality to work with redesigned website

# collar 0.0.3

* adds improved functionality to make_gpx
* minor update to data-download vignette

# collar 0.0.2

* adds Lotek API functions and documentation
* replaces tidyr drop_na with dplyr filter to match stated dependencies

# collar 0.0.1

* Updates Vectronic's URL
* Updates syntax to allow the use of a renaming function within any fetch_* call
* Updated tests to align with new error messages in dependencies

# collar 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* Added cllr_fetch_csv skeleton to show proof of concept and setup documentation and testing
