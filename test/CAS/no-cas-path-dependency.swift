// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-MISS
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json B > %t/B.cmd
// RUN: %swift_frontend_plain @%t/B.cmd -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-MISS

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: %target-swift-frontend \
// RUN:   -c -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/main.swift @%t/MyApp.cmd -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-MISS

// CACHE-MISS: remark: cache miss for input
// CACHE-HIT: remark: replay output file
// CACHE-HIT-NOT: remark: cache miss for input

// RUN: mv %t/cas %t/cas-2

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/main.swift -o %t/deps-2.json -swift-version 5 -cache-compile-job -cas-path %t/cas-2 -I %t/include

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-2.json clang:A > %t/A-2.cmd
// RUN: %swift_frontend_plain @%t/A-2.cmd -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-HIT
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-2.json B > %t/B-2.cmd
// RUN: %swift_frontend_plain @%t/B-2.cmd -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-HIT

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps-2.json > %t/map-2.json
// RUN: llvm-cas --cas %t/cas-2 --make-blob --data %t/map-2.json > %t/map-2.casid
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps-2.json Test > %t/MyApp-2.cmd

// RUN: %target-swift-frontend \
// RUN:   -c -cache-compile-job -cas-path %t/cas-2 \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map-2.casid \
// RUN:   %t/main.swift @%t/MyApp-2.cmd -Rcache-compile-job 2>&1 | %FileCheck %s --check-prefix=CACHE-HIT

//--- main.swift
import A
import B

//--- include/A.h
void a(void);

//--- include/module.modulemap
module A {
  header "A.h"
  export *
}

//--- include/B.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name B -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func c() { }
