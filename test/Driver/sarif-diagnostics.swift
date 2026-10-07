// Each input gets its own SARIF log, and the format travels with the path.

// RUN: %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics=sarif -disallow-use-new-driver %s %S/Inputs/main.swift 2>&1 | %FileCheck %s

// Without a format, or with 'dia', logs stay binary and get no format flag.
// RUN: %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics -disallow-use-new-driver %s %S/Inputs/main.swift 2>&1 | %FileCheck %s -check-prefix=DIA --implicit-check-not=-serialize-diagnostics=
// RUN: %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics=dia -disallow-use-new-driver %s %S/Inputs/main.swift 2>&1 | %FileCheck %s -check-prefix=DIA --implicit-check-not=-serialize-diagnostics=

// An unknown format is rejected wherever it appears, before planning.
// RUN: not %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics=bogus -disallow-use-new-driver %s %S/Inputs/main.swift 2>&1 | %FileCheck %s -check-prefix=UNKNOWN --implicit-check-not=-frontend
// RUN: not %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics=sarif -serialize-diagnostics=bogus -disallow-use-new-driver %s %S/Inputs/main.swift 2>&1 | %FileCheck %s -check-prefix=UNKNOWN --implicit-check-not=-frontend
// RUN: not %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics=bogus -serialize-diagnostics=sarif -disallow-use-new-driver %s %S/Inputs/main.swift 2>&1 | %FileCheck %s -check-prefix=UNKNOWN --implicit-check-not=-frontend
// RUN: not %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics=bogus -serialize-diagnostics -disallow-use-new-driver %s %S/Inputs/main.swift 2>&1 | %FileCheck %s -check-prefix=UNKNOWN --implicit-check-not=-frontend
// UNKNOWN: error: unsupported argument 'bogus' to option '-serialize-diagnostics='

// The last of the bare flag, which means 'dia', and a format wins.
// RUN: %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics=sarif -serialize-diagnostics=dia -disallow-use-new-driver %s %S/Inputs/main.swift 2>&1 | %FileCheck %s -check-prefix=DIA --implicit-check-not=-serialize-diagnostics=
// RUN: %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics=dia -serialize-diagnostics=sarif -disallow-use-new-driver %s %S/Inputs/main.swift 2>&1 | %FileCheck %s
// RUN: %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics -serialize-diagnostics=sarif -disallow-use-new-driver %s %S/Inputs/main.swift 2>&1 | %FileCheck %s
// RUN: %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics=sarif -serialize-diagnostics -disallow-use-new-driver %s %S/Inputs/main.swift 2>&1 | %FileCheck %s -check-prefix=DIA --implicit-check-not=-serialize-diagnostics=

// A supplementary output file map can't carry the format, so it's passed too.
// RUN: %swiftc_driver -driver-print-jobs -typecheck -serialize-diagnostics=sarif -disallow-use-new-driver -driver-filelist-threshold=0 %s %S/Inputs/main.swift 2>&1 | %FileCheck %s -check-prefix=FILEMAP

// swiftc chooses log paths itself, so it rejects an explicit one either way.
// RUN: not %swiftc_driver -typecheck -serialize-diagnostics=sarif -serialize-diagnostics-path %t.sarif -disallow-use-new-driver %s 2>&1 | %FileCheck %s -check-prefix=EXPLICIT-PATH
// RUN: not %swiftc_driver -typecheck -serialize-diagnostics -serialize-diagnostics-path %t.dia -disallow-use-new-driver %s 2>&1 | %FileCheck %s -check-prefix=EXPLICIT-PATH
// EXPLICIT-PATH: error: option '-serialize-diagnostics-path' is not supported by 'swiftc'

// REQUIRES: cplusplus_driver

// CHECK: -serialize-diagnostics-path {{[^ ]*}}sarif-diagnostics.sarif
// CHECK-SAME: -serialize-diagnostics=sarif
// CHECK: -serialize-diagnostics-path {{[^ ]*}}main.sarif
// CHECK-SAME: -serialize-diagnostics=sarif

// DIA: -serialize-diagnostics-path {{[^ ]*}}sarif-diagnostics.dia
// DIA: -serialize-diagnostics-path {{[^ ]*}}main.dia

// FILEMAP: -supplementary-output-file-map {{[^ ]+}}
// FILEMAP-SAME: -serialize-diagnostics=sarif
// FILEMAP: -supplementary-output-file-map {{[^ ]+}}
// FILEMAP-SAME: -serialize-diagnostics=sarif
