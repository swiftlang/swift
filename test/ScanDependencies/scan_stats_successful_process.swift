// Verify that an in-process dependency scan records its exit status in
// '-stats-output-dir'.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/stats)
// RUN: %swift-scan-test -action scan_dependency -- \
// RUN:   %target-swift-frontend -scan-dependencies \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -I %S/Inputs/CHeaders -module-name Test \
// RUN:   -module-cache-path %t/module-cache -stats-output-dir %t/stats %s \
// RUN:   > %t/deps.json
// RUN: %{python} %utils/process-stats-dir.py --set-csv-baseline %t/stats.csv %t/stats
// RUN: %FileCheck -input-file %t/stats.csv %s

// CHECK: {{"AST\.NumDepScanFilesystemLookups"}}
// CHECK-NOT: {{"Frontend.NumProcessFailures"	[1-9]+}}

import B
