// When one Clang compiler instance queries several module names
// (-no-parallel-scan with instance sharing), each failure is reported for its
// own name, failures do not affect the other names, and the results match
// using one compiler instance per name.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/stats_shared_serial)
// RUN: %empty-directory(%t/stats_no_sharing)
// RUN: split-file %s %t

// The scans report errors, so they exit with a failure status.

// Scanning all names with a shared clang compiler instance.
// RUN: not %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib %t/client.swift -I %t/inc -o %t/deps_shared_serial.json -no-parallel-scan -stats-output-dir %t/stats_shared_serial &> %t/out_shared_serial.txt

// Scanning each name separately with a unique compiler instance.
// RUN: not %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/module-cache -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib %t/client.swift -I %t/inc -o %t/deps_no_sharing.json -no-clang-scanner-instance-sharing -stats-output-dir %t/stats_no_sharing &> %t/out_no_sharing.txt

// Diagnostic order and positions in Clang's placeholder input differ between
// the modes, so check each output instead of diffing them.
// RUN: %FileCheck %s < %t/out_shared_serial.txt
// RUN: %FileCheck %s < %t/out_no_sharing.txt
// RUN: %FileCheck %s --check-prefix=NEG < %t/out_shared_serial.txt
// RUN: %FileCheck %s --check-prefix=NEG < %t/out_no_sharing.txt

// The resolved modules and the lookup counts are identical.
// RUN: diff %t/deps_shared_serial.json %t/deps_no_sharing.json
// RUN: %validate-json %t/deps_shared_serial.json | %FileCheck %s --check-prefix=JSON
// RUN: %{python} %utils/process-stats-dir.py --evaluate-delta 'NumDepScanFilesystemLookups == 0' %t/stats_no_sharing %t/stats_shared_serial

// A missing transitive dependency is reported for each module that imports it.
// CHECK-DAG: error: clang dependency scanning failure: While building module 'X'
// CHECK-DAG: X.h:1:{{[0-9]+}}: fatal error: module 'CFoo' not found
// CHECK-DAG: error: clang dependency scanning failure: While building module 'Y'
// CHECK-DAG: Y.h:1:{{[0-9]+}}: fatal error: module 'CFoo' not found
// Other Clang errors are reported for the module that has them.
// CHECK-DAG: error: clang dependency scanning failure: While building module 'Broken'
// CHECK-DAG: Broken.h:1:{{[0-9]+}}: fatal error: 'does_not_exist.h' file not found
// A queried module that does not exist is reported by Swift only.
// CHECK-DAG: error: unable to resolve module dependency: 'Missing'

// NEG-NOT: module 'Missing' not found
// NEG-NOT: unable to resolve module dependency: 'Z'

// JSON: "clang": "Z"

//--- client.swift
import X
import Y
import Z
import Missing
import Broken

//--- inc/module.modulemap
module X { header "X.h" export * }
module Y { header "Y.h" export * }
module Z { header "Z.h" export * }
module Broken { header "Broken.h" export * }

//--- inc/X.h
#pragma clang module import CFoo
void x(void);

//--- inc/Y.h
#pragma clang module import CFoo
void y(void);

//--- inc/Z.h
void z(void);

//--- inc/Broken.h
#include "does_not_exist.h"
void broken(void);
