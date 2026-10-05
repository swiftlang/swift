// Code with the "interface" code generation model has a unique definition in
// its module, so cross-module optimization doesn't serialize it, or anything
// that only it uses. Clients still get what they need to specialize generics.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %target-sil-opt %t/Lib.swiftmodule -enable-experimental-feature Embedded -emit-sorted-sil | %FileCheck -check-prefix INTERFACE %s

// An explicit @export(interface) has the same effect in the default model.
// RUN: %target-swift-frontend -emit-module -o %t/LibDefault.swiftmodule %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded
// RUN: %target-sil-opt %t/LibDefault.swiftmodule -enable-experimental-feature Embedded -emit-sorted-sil | %FileCheck -check-prefix DEFAULT %s

// Clients link and run.
// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck -check-prefix OUTPUT %s

// RUN: %target-swift-frontend -c -emit-module -o %t/Lib.o %t/Lib.swift -parse-as-library -module-name Lib -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-swift-frontend -c -I %t -o %t/Client.o %t/Client.swift -parse-as-library -module-name Client -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface -O
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Lib.o %t/Client.o -o %t/Client
// RUN: %target-run %t/Client | %FileCheck -check-prefix OUTPUT %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

//--- Lib.swift

// Referenced from a generic function, so it is serialized in the default
// model but has a unique definition in the "interface" model.
// INTERFACE: sil [export_interface] {{.*}}@$e3Lib14internalHelperyS2iF : {{[^{]*$}}
// DEFAULT: sil [serialized] {{.*}}@$e3Lib14internalHelperyS2iF : {{.*}} {
func internalHelper(_ x: Int) -> Int { x &+ 1 }

// INTERFACE: sil [serialized] [export_implementation] {{.*}}@$e3Lib7genericySixlF : {{.*}} {
public func generic<T>(_ t: T) -> Int { internalHelper(MemoryLayout<T>.size) }

// Neither the body of an @export(interface) function nor its closures are
// serialized.
// INTERFACE-NOT: @$e3Lib5ifaceSiyF{{.*}} {
// DEFAULT-NOT: @$e3Lib5ifaceSiyF{{.*}} {
@export(interface)
public func iface() -> Int {
  let f = { (x: Int) -> Int in internalHelper(x) }
  return f(1)
}

// Used from generic code, but its metadata is unique, so clients don't need
// its vtable.
// INTERFACE-NOT: sil_vtable {{.*}}InternalClass
final class InternalClass {
  var v: Int
  init(_ v: Int) { self.v = v }
}

public func usesInternalClass<T>(_ t: T) -> Int {
  InternalClass(MemoryLayout<T>.size).v
}

// The witness tables of conformances are serialized, so that clients can
// devirtualize calls through them.
// INTERFACE: sil_witness_table [serialized] S: P module Lib {
public protocol P { func p() -> Int }

public struct S: P {
  public init() {}
  public func p() -> Int { 20 }
}

open class C {
  public init() {}
  open func m() -> Int { 30 }
}

//--- Client.swift
import Lib

func callP<T: P>(_ t: T) -> Int { t.p() }

final class Mine: C { override func m() -> Int { 33 } }

@main
struct Main {
  static func main() {
    print(generic(1))
    print(iface())
    print(usesInternalClass(1))
    print(callP(S()))
    let e: any P = S()
    print(e.p())
    let objects: [C] = [C(), Mine()]
    for o in objects { print(o.m()) }
  }
}

// OUTPUT: 9
// OUTPUT-NEXT: 2
// OUTPUT-NEXT: 8
// OUTPUT-NEXT: 20
// OUTPUT-NEXT: 20
// OUTPUT-NEXT: 30
// OUTPUT-NEXT: 33
