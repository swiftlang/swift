// Each format uses only its own output file map entry.

// RUN: %empty-directory(%t)
// RUN: echo "{\"%/s\": {\"sarif-diagnostics\": \"%/t/main-from-map.sarif\", \"diagnostics\": \"%/t/main-from-map.dia\"}}" > %t/ofm.json

// RUN: %swiftc_driver -typecheck -driver-print-jobs -disallow-use-new-driver -serialize-diagnostics=sarif -output-file-map %t/ofm.json %s 2>&1 | %FileCheck %s -check-prefix=SARIF --implicit-check-not=-from-map.dia

// SARIF: -primary-file {{[^ ]*}}sarif-diagnostics-output-file-map.swift
// SARIF-SAME: -serialize-diagnostics-path {{[^ ]*}}main-from-map.sarif
// SARIF-SAME: -serialize-diagnostics=sarif

// RUN: %swiftc_driver -typecheck -driver-print-jobs -disallow-use-new-driver -serialize-diagnostics -output-file-map %t/ofm.json %s 2>&1 | %FileCheck %s -check-prefix=DIA --implicit-check-not=-serialize-diagnostics= --implicit-check-not=.sarif

// DIA: -primary-file {{[^ ]*}}sarif-diagnostics-output-file-map.swift
// DIA-SAME: -serialize-diagnostics-path {{[^ ]*}}main-from-map.dia

// REQUIRES: cplusplus_driver

let y = x
