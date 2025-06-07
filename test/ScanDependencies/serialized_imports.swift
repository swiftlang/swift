// REQUIRES: objc_interop
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// Run the scanner once, emitting the serialized scanner cache
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -Rdependency-scan-cache -serialize-dependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -enable-cross-import-overlays 2>&1 | %FileCheck %s -check-prefix CHECK-REMARK-SAVE
// RUN: llvm-bcanalyzer --dump %t/cache.moddepcache > %t/cache.moddepcache.initial.dump.txt

// Run the scanner again, but now re-using previously-serialized cache
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-interface -Rdependency-scan-cache -serialize-dependency-scan-cache -load-dependency-scan-cache -dependency-scan-cache-path %t/cache.moddepcache -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -enable-cross-import-overlays 2>&1 | %FileCheck %s -check-prefix CHECK-REMARK-LOAD
// RUN: llvm-bcanalyzer --dump %t/cache.moddepcache > %t/cache.moddepcache.dump.txt

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

import E




