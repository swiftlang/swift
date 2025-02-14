// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -parse-stdlib -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include \
// RUN:   -emit-symbol-graph -emit-symbol-graph-dir %t/symbol-graph1

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/Test.cmd
// RUN: %target-swift-frontend -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -parse-stdlib -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/Test.swiftmodule -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include \
// RUN:   -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -emit-symbol-graph -emit-symbol-graph-dir %t/symbol-graph1 \
// RUN:   -emit-module @%t/Test.cmd -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-MISS

// CACHE-MISS: remark: cache miss for input

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/Test.cmd
// RUN: %target-swift-frontend -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -parse-stdlib -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/Test.swiftmodule -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include \
// RUN:   -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -emit-symbol-graph -emit-symbol-graph-dir %t/symbol-graph2 \
// RUN:   -emit-module @%t/Test.cmd -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-HIT

// CACHE-HIT: remark: replay output file '{{.*}}{{/|\\}}symbol-graph2{{/|\\}}Test.symbols.json': key 'llvmcas://{{.*}}'
// CACHE-HIT: remark: replay output file '{{.*}}{{/|\\}}symbol-graph2{{/|\\}}Test@A.symbols.json': key 'llvmcas://{{.*}}'

// RUN: diff -r -u %t/symbol-graph1 %t/symbol-graph2

/// Test replay from driver interface
// RUN: %swift-scan-test -action compute_cache_key_from_index -cas-path %t/cas -input 0 -- \
// RUN:   %target-swift-frontend -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -parse-stdlib -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/Test.swiftmodule -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include \
// RUN:   -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -emit-symbol-graph -emit-symbol-graph-dir %t/symbol-graph3 \
// RUN:   -emit-module @%t/Test.cmd -Rcache-compile-job > %t/key.casid

// RUN: %swift-scan-test -action replay_result -cas-path %t/cas -id @%t/key.casid -- \
// RUN:   %target-swift-frontend -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -parse-stdlib -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/Test.swiftmodule -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include \
// RUN:   -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -emit-symbol-graph -emit-symbol-graph-dir %t/symbol-graph3 \
// RUN:   -emit-module @%t/Test.cmd -Rcache-compile-job

// RUN: diff -r -u %t/symbol-graph1 %t/symbol-graph3

//--- include/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0

public struct A {}

//--- main.swift
import A

public struct Foo {}

extension A {
  public func bar() {}
}
