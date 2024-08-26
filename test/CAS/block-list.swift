// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -blocklist-file %t/blocklist.yml -blocklist-file %t/empty.yml \
// RUN:   %t/main.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas 

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json Test casFSRootID > %t/fs.casid
// DISABLE: llvm-cas --cas %t/cas --ls-tree-recursive @%t/fs.casid | %FileCheck %s -check-prefix FS

// FS-DAG: blocklist.yml
// FS-DAG: empty.yml

// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json Test > %t/MyApp.cmd

// RUN: %target-swift-frontend \
// RUN:   -emit-ir -o - -cache-compile-job -cas-path %t/cas -O \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name Test -explicit-swift-module-map-file @%t/map.casid \
// RUN:   -blocklist-file %t/blocklist.yml -blocklist-file %t/empty.yml \
// RUN:   -enable-layout-string-value-witnesses -enable-layout-string-value-witnesses-instantiation \
// RUN:   -enable-experimental-feature LayoutStringValueWitnesses -enable-experimental-feature LayoutStringValueWitnessesInstantiation \
// RUN:   %t/main.swift @%t/MyApp.cmd 2>&1 | %FileCheck %s --check-prefix CHECK-BLOCKED

// CHECK-BLOCKED: note: Layout string value witnesses have been disabled for module 'Test' through block list entry
// CHECK-BLOCKED-NOT: type_layout_string

//--- main.swift
public struct Bar {
    let x: Int
    let y: AnyObject
}

public enum Foo {
    case a(AnyObject)
    case b(Int, AnyObject)
    case c
}

//--- blocklist.yml
---
ShouldUseLayoutStringValueWitnesses:
    ModuleName:
        - Test
//--- empty.yml
---
PlaceHolder:
    ModuleName:
        - A
