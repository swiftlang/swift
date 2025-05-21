// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -module-load-mode prefer-serialized \
// RUN:   %t/test.swift -o %t/deps.json -I %t -import-objc-header %t/Bridging.h \
// RUN:   -enable-deterministic-check -cache-compile-job -cas-path %t/cas 2>&1 | %FileCheck %s

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json A > %t/A.cmd
// RUN: %FileCheck %s --check-prefix=CMD --input-file=%t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd 2>&1 | %FileCheck %s
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:B > %t/B.cmd
// RUN: %FileCheck %s --check-prefix=CMD --input-file=%t/B.cmd
// RUN: %swift_frontend_plain @%t/B.cmd 2>&1 | %FileCheck %s

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Test bridgingHeader | %FileCheck %s --check-prefix=CMD

// CHECK: remark: produced matching output file
// CMD: -enable-deterministic-check
// CMD: -always-compile-output-files
// CMD: -cache-disable-replay

//--- test.swift
import A
import B
public func test() {}

//--- A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func a() { }

//--- b.h
void b(void);

//--- module.modulemap
module B {
  header "b.h"
  export *
}

//--- Bridging.h
void bridge(void);
