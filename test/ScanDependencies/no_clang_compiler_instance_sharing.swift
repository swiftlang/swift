// Test that the dependency scanning results are identical with or without
// compiler instance sharing.
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs)
// RUN: %empty-directory(%t/stats_shared)
// RUN: %empty-directory(%t/stats_no_sharing)
// RUN: %empty-directory(%t/stats_shared_serial)
// RUN: split-file %s %t

// Run with default (shared instance)
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -stats-output-dir %t/stats_shared %t/test.swift -o %t/deps_shared.json -I %t/inputs

// Run with sharing disabled
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -stats-output-dir %t/stats_no_sharing %t/test.swift -o %t/deps_no_sharing.json -I %t/inputs -no-clang-scanner-instance-sharing

// Run with the default shared instance but a single worker, so one Clang
// compiler instance queries both C and D.
// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -stats-output-dir %t/stats_shared_serial %t/test.swift -o %t/deps_shared_serial.json -I %t/inputs -no-parallel-scan

// Verify identical results
// RUN: diff %t/deps_shared.json %t/deps_no_sharing.json
// RUN: diff %t/deps_shared_serial.json %t/deps_no_sharing.json

// Also verify the output is actually valid (not empty/broken)
// RUN: %validate-json %t/deps_no_sharing.json | %FileCheck %s

// Lookups are counted once per module name, so all modes count the same.
// RUN: %{python} %utils/process-stats-dir.py \
// RUN:   --evaluate-delta 'NumDepScanFilesystemLookups == 0' \
// RUN:   %t/stats_no_sharing %t/stats_shared
// RUN: %{python} %utils/process-stats-dir.py \
// RUN:   --evaluate-delta 'NumDepScanFilesystemLookups == 0' \
// RUN:   %t/stats_no_sharing %t/stats_shared_serial

// CHECK: "mainModuleName": "Test"
// CHECK-DAG: "clang": "C"
// CHECK-DAG: "clang": "D"
// CHECK-DAG: "swift": "A"
// CHECK-DAG: "swift": "B"

//--- test.swift
import A
import B
public func test() {}

//--- inputs/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
import C
public func a() {}

//--- inputs/B.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name B -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
import D
public func b() {}

//--- inputs/C.h
void c(void);

//--- inputs/D.h
void d(void);

//--- inputs/module.modulemap
module C {
  header "C.h"
  export *
}
module D {
  header "D.h"
  export *
}

