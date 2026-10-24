// The driver removes only a stale log of the requested format before running.

// RUN: %empty-directory(%t)
// RUN: echo "{\"%/s\": {\"sarif-diagnostics\": \"%/t/main.sarif\", \"diagnostics\": \"%/t/main.dia\"}}" > %t/ofm.json

// RUN: touch %t/main.sarif %t/main.dia
// RUN: %swiftc_driver -typecheck -driver-print-jobs -disallow-use-new-driver -serialize-diagnostics=sarif -output-file-map %t/ofm.json %s
// RUN: test ! -e %t/main.sarif
// RUN: test -e %t/main.dia

// RUN: touch %t/main.sarif %t/main.dia
// RUN: %swiftc_driver -typecheck -driver-print-jobs -disallow-use-new-driver -serialize-diagnostics -output-file-map %t/ofm.json %s
// RUN: test -e %t/main.sarif
// RUN: test ! -e %t/main.dia

// REQUIRES: cplusplus_driver

let x = 1
