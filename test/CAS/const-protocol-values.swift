// UNSUPPORTED: OS=windows-msvc
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/main.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -const-gather-protocols-file %t/protocols.json \
// RUN:   -scanner-prefix-map-paths %t /^tmp -I %t/include

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json A casFSRootID > %t/A.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/A.casid | %FileCheck %s --check-prefix=A-FS
// A-FS-NOT: protocols.json

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Test casFSRootID > %t/test.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/test.casid | %FileCheck %s --check-prefix=TEST-FS
// TEST-FS: protocols.json

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

// RUN: %target-swift-frontend \
// RUN:   -typecheck -cache-compile-job -cas-path %t/cas \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name Test -cache-replay-prefix-map /^tmp %t -const-gather-protocols-file /^tmp/protocols.json \
// RUN:   /^tmp/main.swift @%t/MyApp.cmd -emit-const-values-path %t/main.module.swiftconstvalues

//--- include/A.swiftinterface
// swift-interface-format-version: 1.0
// swift-module-flags: -module-name A -O -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib

//--- main.swift
import A
print("Hello")

//--- protocols.json
["Foo"]
