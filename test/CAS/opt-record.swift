// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %s -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: %target-swift-frontend -c -cache-compile-job -cas-path %t/cas -O \
// RUN:   -save-optimization-record -save-optimization-record-path %t/record.yaml \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   @%t/MyApp.cmd %s -o %t/test.o -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-MISS

// RUN: %target-swift-frontend -c -cache-compile-job -cas-path %t/cas -O \
// RUN:   -save-optimization-record -save-optimization-record-path %t/record-1.yaml \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   @%t/MyApp.cmd %s -o %t/test.o -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-HIT

// RUN: %FileCheck %s --check-prefix=YAML --input-file=%t/record.yaml

/// FIXME: Remarks are not produced when cache hit.
// RUN: not %FileCheck %s --check-prefix=YAML --input-file=%t/record-1.yaml

// CACHE-MISS: remark: cache miss for input
// CACHE-HIT: remark: replay output file '<cached-diagnostics>': key 'llvmcas://{{.*}}'
// CACHE-HIT: remark: replay output file '{{.*}}{{/|\\}}test.o': key 'llvmcas://{{.*}}'
// YAML: ---

var a: Int = 1

func foo() {
  a = 2
}

public func bar() {
  foo()
}
