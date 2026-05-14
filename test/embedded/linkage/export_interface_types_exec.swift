// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the library module (defines @export(interface) types) and the
// application, then link and run the resulting binary. This ensures that
// type metadata for @export(interface) struct/class types has the right
// cross-module linkage to form existentials from the importing module.

// RUN: %target-swift-frontend -c -emit-module -o %t/Library.o %t/Library.swift -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/Application.o %t/Application.swift -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library
// RUN: %target-clang %target-clang-resource-dir-opt %t/Library.o %t/Application.o %target-embedded-posix-shim -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

//--- Library.swift

public protocol Greeter {
  func greet()
}

@export(interface)
public struct ExportedStruct: Greeter {
  public var name: StaticString
  public init(name: StaticString) { self.name = name }
  public func greet() { print("struct greeting from \(name)") }
}

@export(interface)
public class ExportedClass: Greeter {
  public var name: StaticString
  public init(name: StaticString) { self.name = name }
  public func greet() { print("class greeting from \(name)") }
}

//--- Application.swift
import Library

func callThroughExistential(_ g: any Greeter) {
  g.greet()
}

@main
struct Main {
  static func main() {
    // Wrap imported @export(interface) values in `any Greeter` existentials.
    // This requires both the type metadata AND the protocol witness table
    // for ExportedStruct/ExportedClass to be available in this module.

    // CHECK: struct greeting from S
    callThroughExistential(ExportedStruct(name: "S"))
    // CHECK-NEXT: class greeting from C
    callThroughExistential(ExportedClass(name: "C"))

    // Also check casting out of Any still works end-to-end.
    let anys: [Any] = [ExportedStruct(name: "S2"), ExportedClass(name: "C2")]
    // CHECK-NEXT: struct greeting from S2
    (anys[0] as! ExportedStruct).greet()
    // CHECK-NEXT: class greeting from C2
    (anys[1] as! ExportedClass).greet()

    // CHECK-NEXT: DONE
    print("DONE")
  }
}
