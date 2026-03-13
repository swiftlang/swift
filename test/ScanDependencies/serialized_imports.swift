// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// Run the scanner once, emitting the serialized scanner cache
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -Rdependency-scan-cache -serialize-dependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -enable-cross-import-overlays -module-name deps 2>&1 | %FileCheck %s -check-prefix CHECK-REMARK-SAVE
// RUN: llvm-bcanalyzer --dump %t/cache.moddepcache > %t/cache.moddepcache.initial.dump.txt
// RUN: %validate-json %t/deps.json | %FileCheck %s -check-prefix CHECK-IMPORTS

// Run the scanner again, but now re-using previously-serialized cache
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -Rdependency-scan-cache -serialize-dependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %s -o %t/deps_incremental.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -enable-cross-import-overlays -module-name deps 2>&1 | %FileCheck %s -check-prefix CHECK-REMARK-LOAD
// RUN: llvm-bcanalyzer --dump %t/cache.moddepcache > %t/cache.moddepcache.dump.txt
// RUN: %validate-json %t/deps_incremental.json | %FileCheck %s -check-prefix CHECK-IMPORTS

// Ensure that the initial scan, and the secondary scan which just re-used the initial scan's results report
// the same number of import statement nodes, ensuring that serialization-deserialization did not affect
// the import sets
//
// RUN: grep "IMPORT_STATEMENT_NODE" %t/cache.moddepcache.initial.dump.txt | wc -l > %t/initial_scan_import_count.txt
// RUN: grep "IMPORT_STATEMENT_NODE" %t/cache.moddepcache.dump.txt | wc -l > %t/secondary_scan_import_count.txt
// RUN: diff %t/initial_scan_import_count.txt %t/secondary_scan_import_count.txt

// CHECK-REMARK-SAVE: remark: Incremental module scan: Failed to load module scanning dependency cache from:
// CHECK-REMARK-SAVE: remark: Incremental module scan: Serializing module scanning dependency cache to:
// CHECK-REMARK-LOAD: remark: Incremental module scan: Re-using serialized module scanning dependency cache from:

// CHECK-IMPORTS:      "modulePath": "deps.swiftmodule",
// CHECK-IMPORTS:      "imports": [
// CHECK-IMPORTS-NEXT:        {
// CHECK-IMPORTS-NEXT:          "identifier": "Swift",
// CHECK-IMPORTS-NEXT:          "accessLevel": "public"
// CHECK-IMPORTS-NEXT:        },
// CHECK-IMPORTS-NEXT:        {
// CHECK-IMPORTS-NEXT:          "identifier": "SwiftOnoneSupport",
// CHECK-IMPORTS-NEXT:          "accessLevel": "public"
// CHECK-IMPORTS-NEXT:        },
// CHECK-IMPORTS-NEXT:        {
// CHECK-IMPORTS-NEXT:          "identifier": "E",
// CHECK-IMPORTS-NEXT:          "accessLevel": "public",
// CHECK-IMPORTS-NEXT:          "importLocations": [
// CHECK-IMPORTS-NEXT:            {
// CHECK-IMPORTS-NEXT:              "bufferIdentifier": "{{.*}}serialized_imports.swift",
// CHECK-IMPORTS-NEXT:              "linuNumber": 54,
// CHECK-IMPORTS-NEXT:              "columnNumber": 8
// CHECK-IMPORTS-NEXT:            }

// CHECK-IMPORTS:          "optionalImports": [
// CHECK-IMPORTS-NEXT:        {
// CHECK-IMPORTS-NEXT:          "identifier": "E_Private",
// CHECK-IMPORTS-NEXT:          "accessLevel": "public"
// CHECK-IMPORTS-NEXT:        }
// CHECK-IMPORTS-NEXT:      ],

import E
import E.Private
