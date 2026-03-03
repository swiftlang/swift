// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %s -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

// RUN: %target-swift-frontend-plain -c -cache-compile-job -cas-path %t/cas -O \
// RUN:   -save-optimization-record -save-optimization-record-path %t/record.yaml \
// RUN:   -swift-version 5 -module-name Test \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   @%t/MyApp.cmd %s -o %t/test.o -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-MISS

// RUN: %target-swift-frontend-plain -c -cache-compile-job -cas-path %t/cas -O \
// RUN:   -save-optimization-record -save-optimization-record-path %t/record-1.yaml \
// RUN:   -swift-version 5 -module-name Test \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
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
