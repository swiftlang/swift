// Under an explicit module build the frontend records this module's binary
// swiftmodule in debug info via -debug-module-path, which supersedes
// -add_ast_path.

// RUN: %swiftc_driver -disallow-use-new-driver -driver-print-jobs -target x86_64-apple-macosx10.15 -g %s 2>&1 | %FileCheck -check-prefix=IMPLICIT %s
// RUN: %swiftc_driver -disallow-use-new-driver -driver-print-jobs -target x86_64-apple-macosx10.15 -g -explicit-module-build %s 2>&1 | %FileCheck -check-prefix=EXPLICIT %s

// IMPLICIT: bin{{/|\\\\}}ld{{"? }}
// IMPLICIT-SAME: -add_ast_path {{.*(/|\\\\)[^/]+}}.swiftmodule

// EXPLICIT: -debug-module-path {{.*(/|\\\\)[^/]+}}.swiftmodule
// EXPLICIT: bin{{/|\\\\}}ld{{"? }}
// EXPLICIT-NOT: -add_ast_path

// REQUIRES: OS=macosx
// REQUIRES: cplusplus_driver
