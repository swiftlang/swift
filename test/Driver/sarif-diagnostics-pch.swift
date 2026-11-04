// PCH jobs serialize diagnostics in the requested format, like other jobs.

// RUN: %empty-directory(%t)

// A persistent PCH writes its log inside the PCH output directory.
// RUN: %swiftc_driver -typecheck -driver-print-jobs -disallow-use-new-driver -serialize-diagnostics=sarif -emit-module-path %t/main.swiftmodule -pch-output-dir %t/pch -import-objc-header %S/Inputs/bridging-header.h %s 2>&1 | %FileCheck %s -check-prefix=PERSISTENT

// PERSISTENT: -serialize-diagnostics-path {{[^ ]*}}pch{{/|\\\\}}bridging-header-{{[^ ]*}}.sarif
// PERSISTENT-SAME: -serialize-diagnostics=sarif
// PERSISTENT-SAME: -emit-pch -pch-output-dir

// The compile job serializes to SARIF too.
// PERSISTENT: -primary-file {{[^ ]*}}sarif-diagnostics-pch.swift
// PERSISTENT-SAME: -serialize-diagnostics-path {{[^ ]*}}sarif-diagnostics-pch.sarif
// PERSISTENT-SAME: -serialize-diagnostics=sarif

// Later jobs, such as emit-module, get neither a path nor the format.
// PERSISTENT-NOT: -serialize-diagnostics

// A temporary PCH does the same.
// RUN: %swiftc_driver -typecheck -driver-print-jobs -disallow-use-new-driver -serialize-diagnostics=sarif -import-objc-header %S/Inputs/bridging-header.h %s 2>&1 | %FileCheck %s -check-prefix=TEMPORARY

// TEMPORARY: -serialize-diagnostics-path {{[^ ]*}}.sarif
// TEMPORARY-SAME: -serialize-diagnostics=sarif
// TEMPORARY-SAME: -emit-pch -o {{[^ ]*}}bridging-header-{{[^ ]*}}.pch

// TEMPORARY: -primary-file {{[^ ]*}}sarif-diagnostics-pch.swift
// TEMPORARY-SAME: -serialize-diagnostics-path {{[^ ]*}}sarif-diagnostics-pch.sarif
// TEMPORARY-SAME: -serialize-diagnostics=sarif

// Without a module path, a persistent PCH's log is a temporary .sarif file.
// RUN: %swiftc_driver -typecheck -driver-print-jobs -disallow-use-new-driver -serialize-diagnostics=sarif -pch-output-dir %t/pch -import-objc-header %S/Inputs/bridging-header.h %s 2>&1 | %FileCheck %s -check-prefix=NO-MODULE-PATH

// NO-MODULE-PATH: -serialize-diagnostics-path {{[^ ]*}}bridging-header-{{[^ ]*}}.sarif
// NO-MODULE-PATH-SAME: -serialize-diagnostics=sarif
// NO-MODULE-PATH-SAME: -emit-pch -pch-output-dir

// Without a format, the PCH log stays binary and no job gets a format flag.
// RUN: %swiftc_driver -typecheck -driver-print-jobs -disallow-use-new-driver -serialize-diagnostics -emit-module-path %t/main.swiftmodule -pch-output-dir %t/pch -import-objc-header %S/Inputs/bridging-header.h %s 2>&1 | %FileCheck %s -check-prefix=DIA --implicit-check-not=-serialize-diagnostics=

// DIA: -serialize-diagnostics-path {{[^ ]*}}pch{{/|\\\\}}bridging-header-{{[^ ]*}}.dia
// DIA-SAME: -emit-pch -pch-output-dir
// DIA: -primary-file {{[^ ]*}}sarif-diagnostics-pch.swift
// DIA-SAME: -serialize-diagnostics-path {{[^ ]*}}sarif-diagnostics-pch.dia

// A persistent PCH's log ignores the output file map.
// RUN: echo "{\"%/s\": {\"sarif-diagnostics\": \"%/t/main-from-map.sarif\"}, \"%/S/Inputs/bridging-header.h\": {\"sarif-diagnostics\": \"%/t/header-from-map.sarif\"}}" > %t/ofm.json
// RUN: %swiftc_driver -typecheck -driver-print-jobs -disallow-use-new-driver -serialize-diagnostics=sarif -output-file-map %t/ofm.json -pch-output-dir %t/pch -import-objc-header %S/Inputs/bridging-header.h %s 2>&1 | %FileCheck %s -check-prefix=MAP --implicit-check-not=header-from-map

// MAP: -serialize-diagnostics-path {{[^ ]*}}bridging-header-{{[^ ]*}}.sarif
// MAP-SAME: -emit-pch

// REQUIRES: cplusplus_driver

let y = x
