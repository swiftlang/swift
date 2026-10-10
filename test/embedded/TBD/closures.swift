// A closure is emitted wherever the code that contains it is. So a closure in
// generic code is emitted into each client that uses it, and isn't strongly
// defined in its module, even when serialized code refers to it.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -o %t/Lib.ll %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib -validate-tbd-against-ir=all
// RUN: %FileCheck -check-prefix LIBRARY-IR %s < %t/Lib.ll
// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib-O.tbd -tbd-install_name Lib -validate-tbd-against-ir=all -O

// Clients emit their own copies of the closure.
// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

//--- Lib.swift
@usableFromInline func helper(_ x: Int) -> Int { x &+ 1 }

// LIBRARY-IR-DAG: define linkonce_odr {{.*}}@"$e3Lib18genericWithClosureySixlFS2icfU_"(
public func genericWithClosure<T>(_ t: T) -> Int {
  let f = { (x: Int) -> Int in helper(x) }
  return f(MemoryLayout<T>.size)
}

// A closure in code with a unique definition doesn't need a symbol.
// LIBRARY-IR-DAG: define internal {{.*}}@"$e3Lib21nonGenericWithClosureSiyFS2icfU_"(
public func nonGenericWithClosure() -> Int {
  let f = { (x: Int) -> Int in helper(x) &* 2 }
  return f(1)
}

//--- Client.swift
import Lib

@main
struct Main {
  static func main() {
    print(genericWithClosure(1))
    print(nonGenericWithClosure())
  }
}

// CHECK: 9
// CHECK-NEXT: 4
