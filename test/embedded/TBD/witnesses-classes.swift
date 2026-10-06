// A witness thunk for a class's conformance dispatches to a non-final method
// through the vtable, unless the optimizer devirtualizes the call. Either way,
// the method is public, so the TBD file can list it.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -emit-tbd-path %t/Lib.tbd -tbd-install_name Lib -validate-tbd-against-ir=all
// RUN: %FileCheck %s < %t/Lib.tbd
// RUN: %target-swift-frontend -emit-ir -o /dev/null %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -validate-tbd-against-ir=all -O

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck -check-prefix OUTPUT %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: VENDOR=apple

//--- Lib.swift
public protocol Describe {
  func describe() -> Int
  var value: Int { get }
  static func kind() -> Int
}

// Never subclassed, so the optimizer can devirtualize the thunk's calls.
// CHECK-DAG: "_$e3Lib12NoSubclassesC8describeSiyF"
// CHECK-DAG: "_$e3Lib12NoSubclassesC5valueSivg"
// CHECK-DAG: "_$e3Lib12NoSubclassesC4kindSiyFZ"
class NoSubclasses: Describe {
  init() {}
  func describe() -> Int { 1 }
  var value: Int { 10 }
  class func kind() -> Int { 100 }
}

// Overridden, so the thunk's calls stay dynamically dispatched.
// CHECK-DAG: "_$e3Lib4BaseC8describeSiyF"
// CHECK-DAG: "_$e3Lib4BaseC5valueSivg"
// CHECK-DAG: "_$e3Lib4BaseC4kindSiyFZ"
class Base: Describe {
  init() {}
  func describe() -> Int { 2 }
  var value: Int { 20 }
  class func kind() -> Int { 200 }
}

class Derived: Base {
  override func describe() -> Int { 3 }
  override var value: Int { 30 }
  override class func kind() -> Int { 300 }
}

// A conformance whose witnesses are inherited from the superclass.
class Inheriting: NonConforming, Describe {}

class NonConforming {
  init() {}
  // CHECK-DAG: "_$e3Lib13NonConformingC8describeSiyF"
  func describe() -> Int { 4 }
  // CHECK-DAG: "_$e3Lib13NonConformingC5valueSivg"
  var value: Int { 40 }
  // CHECK-DAG: "_$e3Lib13NonConformingC4kindSiyFZ"
  class func kind() -> Int { 400 }
}

@inline(never)
func all() -> [any Describe] {
  [NoSubclasses(), Base(), Derived(), Inheriting()]
}

@inline(never)
func kindOf<T: Describe>(_ t: T) -> Int { T.kind() }

public func total() -> Int {
  var result = 0
  for d in all() { result += d.describe() + d.value }
  return result + kindOf(NoSubclasses()) + kindOf(Base()) + kindOf(Derived()) +
    kindOf(Inheriting())
}

//--- Client.swift
import Lib

@main
struct Main {
  static func main() {
    print(total())
  }
}

// OUTPUT: 1110
