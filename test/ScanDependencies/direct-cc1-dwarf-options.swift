// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: RC_DEBUG_OPTIONS=1 %target-swift-frontend -scan-dependencies -o %t/deps.json -I %t \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -g -file-compilation-dir %t -Xcc -ferror-limit=1 \
// RUN:   %t/test.swift -module-name Test -swift-version 5 -experimental-clang-importer-direct-cc1-scan
// RUN: %FileCheck %s --check-prefix CHECK-DIRECT-CC1-SCAN --input-file=%t/deps.json

// CHECK-DIRECT-CC1-SCAN-NOT: -dwarf-debug-flags

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd

// RUN: %{python} %S/../CAS/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-string-processing-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-concurrency-module-import\"" >> %t/MyApp.cmd
// RUN: echo "\"-disable-implicit-swift-modules\"" >> %t/MyApp.cmd
// RUN: echo "\"-explicit-swift-module-map-file\"" >> %t/MyApp.cmd
// RUN: echo "\"%t/map.json\"" >> %t/MyApp.cmd

// RUN: %target-swift-frontend -module-name Test -O @%t/MyApp.cmd %t/test.swift -parse-stdlib \
// RUN:   -g -c -o %t/test.o -debug-info-store-invocation

// RUN: %llvm-dwarfdump %t/test.o --recurse-depth=0 | %FileCheck %s --check-prefix=FLAGS
// FLAGS: DW_AT_APPLE_flags
// FLAGS-SAME: -target

//--- A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib -user-module-version 1.0
public func a() { }

//--- test.swift
import A
func test() {}
