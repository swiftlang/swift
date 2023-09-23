// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -I %t -o %t/MyModuleA.swiftmodule %t/MyModuleA.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -emit-module -I %t -o %t/MyModuleB.swiftmodule %t/MyModuleB.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -emit-module -I %t -o %t/MyModuleC.swiftmodule %t/MyModuleC.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -emit-ir -I %t %t/Main.swift -enable-experimental-feature Embedded -parse-as-library | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

// Dependencies look like this:
//
//              ┌───  ModuleB  ◀─┐
//  ModuleA  ◀──┤                ├───   Main
//              └───  ModuleC  ◀─┘

// BEGIN MyModuleA.swift

public var global = 0

public func foo() { global += 1 }

// BEGIN MyModuleB.swift

import MyModuleA

public func foo() { global += 1 }

// BEGIN MyModuleC.swift

import MyModuleA

public func foo() { global += 1 }

// BEGIN Main.swift

import MyModuleB
import MyModuleC

public func main() {
  MyModuleB.foo()
  MyModuleC.foo()
}

// CHECK: @"$s9MyModuleA6globalSivp" = global %TSi zeroinitializer
