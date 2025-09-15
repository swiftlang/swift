// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -I %t -o %t/MyModuleA.swiftmodule %t/MyModuleA.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -emit-module -I %t -o %t/MyModuleB.swiftmodule %t/MyModuleB.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -emit-module -I %t -o %t/MyModuleC.swiftmodule %t/MyModuleC.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift -enable-experimental-feature Embedded -parse-as-library | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// Dependencies look like this:
//
//              ┌───  ModuleB  ◀─┐
//  ModuleA  ◀──┤                ├───   Main
//              └───  ModuleC  ◀─┘

//--- MyModuleA.swift

public var global = 0

public func foo() { global += 1 }

//--- MyModuleB.swift

import MyModuleA

public func foo() { global += 1 }

//--- MyModuleC.swift

import MyModuleA

public func foo() { global += 1 }

//--- Main.swift

import MyModuleB
import MyModuleC

public func main() {
  MyModuleB.foo()
  MyModuleC.foo()
}

// CHECK: @"$e9MyModuleA6globalSivp" = {{.*}}global %TSi zeroinitializer
