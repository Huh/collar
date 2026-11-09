# collar 0.1.0

* Documentation now uses the manufacturer's correct name, Vectronic, rather than "Vectronics" (#63). Function names are unchanged.
* Lotek and Track Tag functions now report the server's actual error message when a request fails, instead of `bad error message` (#80). Lotek's public demo account no longer accepts logins, so examples and the Lotek vignette now use placeholder credentials.
* `add_lotek_status()` is new: it decodes Lotek's `RxStatus` (and, for Swift fixes, `FixType`) into readable fix status and satellite-count columns for Litetrack standard, Litetrack Swift, and Iridium collars (#87, thanks @ericnewkirk and @JessKLockhart).
* `fetch_ats_positions()` and `fetch_ats_transmissions()` now default to `chunk_size = 20`, because the ATS website stopped returning all data for batches larger than about 20 collars (#89).
* `fetch_trktg_devices()` and `fetch_trktg_positions()` no longer use stringr, so collar no longer depends on it; unmatched device IDs and coordinates still come back as `NA`.
* `fetch_vectronics()` now passes `rename_fun` through instead of ignoring it, returns the collar ID and data type (not the full request URL, which contained the collar key) when `count = TRUE`, and prints a readable elapsed-time message.

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
