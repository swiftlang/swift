// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O -module-cache-path %t/clang-module-cache \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -import-objc-header %t/Bridging.h -scanner-output-dir %t -auto-bridging-header-chaining -scanner-debug-write-output \
// RUN:   %t/test.swift %t/foo.swift -I %t/include -o %t/deps.json -cache-compile-job -cas-path %t/cas

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:Dummy > %t/dummy.cmd
// RUN: %swift_frontend_plain @%t/dummy.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Simple > %t/simple.cmd
// RUN: %swift_frontend_plain @%t/simple.cmd

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json bridgingHeader > %t/header.cmd
// RUN: %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/bridging.pch
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-output-keys -- \
// RUN:   %target-swift-frontend @%t/header.cmd -disable-implicit-swift-modules -O -o %t/bridging.pch > %t/keys.json
// RUN: %{python} %S/Inputs/ExtractOutputKey.py %t/keys.json > %t/key

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-objc-header\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/Bridging.h\"" >> %t/MyApp.cmd
// RUN: echo "\"-import-pch\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/bridging.pch\"" >> %t/MyApp.cmd
// RUN: echo "\"-bridging-header-pch-key\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/key\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd
// RUN: echo "\"@%t/map.casid\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend %t/test.swift %t/foo.swift -O -emit-module -emit-module-path %t/Test.swiftmodule -c \
// RUN:  -module-name Test -o %t/test.o -cache-compile-job -cas-path %t/cas @%t/MyApp.cmd -gen-reproducer -gen-reproducer-dir %t/crash

// RUN: %FileCheck %s --input-file=%t/crash/reproduce.sh
// CHECK: -cache-compile-job
// CHECK-NOT: -gen-reproducer

/// Delete some inputs from original compilation and run the reproducer.
// RUN: rm -rf %t/include
// RUN: %swift_frontend_plain @%t/crash/reproduce.sh

/// Also test module jobs.
// RUN: %swift_frontend_plain @%t/dummy.cmd -gen-reproducer -gen-reproducer-dir %t/crash-2
// RUN: %FileCheck %s --input-file=%t/crash-2/reproduce.sh
// RUN: %swift_frontend_plain @%t/crash-2/reproduce.sh
// RUN: %swift_frontend_plain @%t/simple.cmd -gen-reproducer -gen-reproducer-dir %t/crash-3
// RUN: %FileCheck %s --input-file=%t/crash-3/reproduce.sh
// RUN: %swift_frontend_plain @%t/crash-3/reproduce.sh

//--- test.swift
import Dummy
import Simple
public func testFunc() {
  foo()
  bridge()
  simple()
}

//--- foo.swift
public func foo() {}

//--- Bridging.h
void bridge(void);

//--- include/module.modulemap
module Dummy {
 umbrella header "Dummy.h"
}

//--- include/Dummy.h
void dummy(void);

//--- include/Simple.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name Simple -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import
import Swift
import Dummy
public func simple() { }
