// RUN: %empty-directory(%t)
// RUN: %{python} %S/../gen-output-file-map.py -o %t %S/Inputs -r %t.resp
// RUN: cd %t && %target-swiftc_driver -c -output-file-map %t/output.json -incremental -module-name main -enable-direct-intramodule-dependencies -verify-incremental-dependencies @%t.resp
// RUN: cd %t && %target-swiftc_driver -c -output-file-map %t/output.json -incremental -enable-batch-mode -module-name main -enable-direct-intramodule-dependencies -verify-incremental-dependencies @%t.resp

// N.B. These tests are meant to continue to expand to more and more input files
// as more kinds of cross-type dependencies are discovered. This will naturally
// increase the chance that input ordering bugs will be surfaced by batch mode.
