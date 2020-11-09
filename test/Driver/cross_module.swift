// RUN: %empty-directory(%t/sub)
// RUN: echo "{\"\": {\"swift-dependencies\": \"cross_module.swiftdeps\"}}" > %t/ofm.json
// RUN: %swiftc_driver -incremental -emit-module -module-name cross_module -output-file-map=%/t/ofm.json -driver-print-jobs -target x86_64-apple-macosx10.9 %s -enable-experimental-cross-module-incremental-build 2>^1 | %FileCheck -check-prefix ENABLE %s

// ENABLE: {{bin(/|\\\\)swift(-frontend|c)?(\.exe)?"?}} -frontend -merge-modules
// ENABLE-SAME: -enable-experimental-cross-module-incremental-build
