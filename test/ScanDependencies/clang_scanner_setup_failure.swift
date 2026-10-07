// If Clang cannot set up its compiler instance, the failure is reported once,
// and the scan stops before any module is looked up. Every worker sets up its
// compiler instance from the same command line, so the scanner checks the
// setup once, before scanning.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: split-file %s %t

// An empty -fdepscan-log-path= makes the compiler instance setup fail.
// RUN: not %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib %t/client.swift -I %t/inc -o %t/deps_default.json -Xcc -fdepscan-log-path= &> %t/out_default.txt
// RUN: not %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib %t/client.swift -I %t/inc -o %t/deps_serial.json -Xcc -fdepscan-log-path= -no-parallel-scan &> %t/out_serial.txt
// RUN: not %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib %t/client.swift -I %t/inc -o %t/deps_no_sharing.json -Xcc -fdepscan-log-path= -no-clang-scanner-instance-sharing &> %t/out_no_sharing.txt

// Exactly one failure report, and no module lookups, in every mode.
// RUN: %FileCheck %s --implicit-check-not='clang dependency scanning failure' --implicit-check-not='unable to resolve module dependency' < %t/out_default.txt
// RUN: %FileCheck %s --implicit-check-not='clang dependency scanning failure' --implicit-check-not='unable to resolve module dependency' < %t/out_serial.txt
// RUN: %FileCheck %s --implicit-check-not='clang dependency scanning failure' --implicit-check-not='unable to resolve module dependency' < %t/out_no_sharing.txt

// CHECK: error: clang dependency scanning failure: error: '-fdepscan-log-path=' requires a non-empty file path

//--- client.swift
import A
import B
import C

//--- inc/module.modulemap
module A { header "A.h" export * }
module B { header "B.h" export * }
module C { header "C.h" export * }

//--- inc/A.h
void a(void);

//--- inc/B.h
void b(void);

//--- inc/C.h
void c(void);
