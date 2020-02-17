// REQUIRES: asserts

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %empty-directory(%t/Build)
// RUN: %empty-directory(%t/PrebuiltCache)

// 1. Create a module for our nested type
// RUN: echo 'public protocol Nest { associatedtype Egg }' > %t/TestModule.swift

// 2. Create an interface for it
// RUN: %target-swift-frontend -typecheck %t/TestModule.swift -emit-module-interface-path %t/Build/TestModule.swiftinterface -swift-version 5

// 3. Build a .swiftmodule from the .swiftinterface and put it in the module cache
// RUN: %target-swift-frontend -compile-module-from-interface %t/Build/TestModule.swiftinterface -o %t/PrebuiltCache/TestModule.swiftmodule

// 4. Import the module in a different module that extends the nested type.
// RUN: echo 'import TestModule; extension Nest where Egg == Int { public func robin() -> Egg { return 3 } }' > %t/NestModule.swift

// 5. Create an interface for it
// RUN: %target-swift-frontend -typecheck %t/NestModule.swift -I %t/PrebuiltCache -sdk %t -prebuilt-module-cache-path %t/PrebuiltCache -module-cache-path %t/ModuleCache -emit-module-interface-path %t/Build/NestModule.swiftinterface -swift-version 5

// 6. Build a .swiftmodule from the .swiftinterface and put it in the module cache
// RUN: %target-swift-frontend -compile-module-from-interface -I %t/PrebuiltCache -sdk %t %t/Build/NestModule.swiftinterface -o %t/PrebuiltCache/NestModule.swiftmodule

// 7. Ensure we resolve the cross-ref to the nested type properly.
// RUN: %target-swift-frontend -typecheck %s -I %t/PrebuiltCache -sdk %t -prebuilt-module-cache-path %t/PrebuiltCache -module-cache-path %t/ModuleCache -print-stats 2>&1 | %FileCheck %s

import TestModule
import NestModule

// CHECK: Statistics
// CHECK: 1 Serialization - # of nested types resolved without full lookup

func tweet<Location: Nest>(from place: Location) where Location.Egg == Int {
  _ = place.robin()
}
