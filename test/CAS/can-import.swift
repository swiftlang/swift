// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -I %t/include

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:B > %t/B.cmd
// RUN: %swift_frontend_plain @%t/B.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json C > %t/C.cmd
// RUN: %swift_frontend_plain @%t/C.cmd

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/main.swift @%t/MyApp.cmd

//--- main.swift
#if canImport(A.Sub)
func a() {}
#endif

#if canImport(A.Missing)
import A.Missing
#endif

#if canImport(B)
func b() {}
#endif

#if canImport(C, _version: 1.0)
import C
#endif

#if canImport(C, _version: 2.0)
import Missing
#endif

func useA() {
  a()
  b()
  c()
}

//--- include/module.modulemap
module A {
  module Sub {
    header "sub.h"
    export *
  }
}

module B {
  header "B.h"
  export *
}

//--- include/sub.h
void notused(void);

//--- include/B.h
void notused2(void);

//--- include/C.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name C -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func c() { }
