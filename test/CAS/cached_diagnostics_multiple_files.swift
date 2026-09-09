// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/test.swift %t/foo.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-parse-stdlib\"" >> %t/MyApp.cmd

/// First pass: both inputs are cache misses, each produces its own warning.
// RUN: %target-swift-frontend-plain -cache-compile-job -Rcache-compile-job %t/test.swift %t/foo.swift -emit-module -o %t/Test.swiftmodule \
// RUN:  -module-name Test -cas-path %t/cas @%t/MyApp.cmd 2>&1 | %FileCheck --check-prefix=CACHE-MISS %s

/// Second pass: both inputs are cache hits, and both cached warnings must
/// replay with their original file and location.
// RUN: %target-swift-frontend-plain -cache-compile-job -Rcache-compile-job %t/test.swift %t/foo.swift -emit-module -o %t/Test.swiftmodule \
// RUN:  -module-name Test -cas-path %t/cas @%t/MyApp.cmd 2>&1 | %FileCheck --check-prefix=CACHE-HIT %s

//--- test.swift
func testFunc() {}
#warning("warning from test.swift")

//--- foo.swift
func foo() {}
#warning("warning from foo.swift")

// CACHE-MISS: remark: cache miss for input
// CACHE-MISS-NOT: remark: replay output file
// CACHE-MISS-DAG: test.swift:2:{{.*}}: warning: warning from test.swift
// CACHE-MISS-DAG: foo.swift:2:{{.*}}: warning: warning from foo.swift

// CACHE-HIT: remark: replay output file
// CACHE-HIT-NOT: remark: cache miss for input
// CACHE-HIT-DAG: test.swift:2:{{.*}}: warning: warning from test.swift
// CACHE-HIT-DAG: foo.swift:2:{{.*}}: warning: warning from foo.swift
