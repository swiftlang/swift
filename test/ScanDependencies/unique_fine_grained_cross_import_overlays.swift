// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: split-file %s %t

// Run a dependency scan on a source file that imports both constituents of a cross-import overlay to ensure the cross-import overlay dependency is registered
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/A.swift %t/B.swift -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -I %S/Inputs/CHeaders/ExtraCModules -module-name FineGrainedCrossImportTestModule -enable-cross-import-overlays

// Check the contents of the JSON output
// RUN: %validate-json %t/deps.json | %FileCheck %s

//--- A.swift
import E
import SubE

//--- B.swift
import E
import SubE

// CHECK: "swift": "_cross_import_E"
// CHECK-COUNT-1: -swift-module-cross-import
