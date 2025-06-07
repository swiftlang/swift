// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name ModuleA -emit-module-path %t/ModuleA.swiftmodule -primary-file %S/Inputs/TestablePrintASTLocationsModule.swift -enable-testing
// RUN: %target-swift-frontend -emit-module -module-name ModuleB -emit-module-path %t/ModuleB.swiftmodule -primary-file %S/Inputs/TestablePrintASTLocationsModule.swift -enable-testing
// RUN: not %target-swift-frontend -typecheck -I %t %s 2>&1 | %FileCheck %s

@testable import ModuleA
@testable import ModuleB

ambiguous()

// CHECK: testable-printast-locations.swift:[[@LINE-2]]:1: error: ambiguous use of 'ambiguous()'
// CHECK: ModuleA.ambiguous:1:15: note: found this candidate in module 'ModuleA'
// CHECK: ModuleB.ambiguous:1:15: note: found this candidate in module 'ModuleB'
