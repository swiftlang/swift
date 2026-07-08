// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the library module (defines @export(interface) types) and the
// application, then link and run the resulting binary. This ensures that
// type metadata for @export(interface) struct/class types has the right
// cross-module linkage to form existentials from the importing module.

// RUN: %target-swift-frontend -c -emit-module -o %t/Library.o %t/Library.swift -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library
// RUN: %target-swift-frontend -c -I %t -emit-module -o %t/Application.o %t/Application.swift -enable-experimental-feature Embedded -enable-experimental-feature Extern -parse-as-library
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Library.o %t/Application.o %target-embedded-posix-shim -o %t/Application
// RUN: %target-run %t/Application | %FileCheck %s

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

@export(interface)
public enum ExportedEnum: Greeter, Error, Equatable {
  case red
  case green
  case blue
  public func greet() {
    switch self {
    case .red:   print("enum greeting from red")
    case .green: print("enum greeting from green")
    case .blue:  print("enum greeting from blue")
    }
  }
}

@export(interface)
public func throwExportedEnum() throws {
  throw ExportedEnum.red
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
    // CHECK-NEXT: enum greeting from green
    callThroughExistential(ExportedEnum.green)

    // Also check casting out of Any still works end-to-end.
    let anys: [Any] = [
      ExportedStruct(name: "S2"),
      ExportedClass(name: "C2"),
      ExportedEnum.blue,
    ]
    // CHECK-NEXT: struct greeting from S2
    (anys[0] as! ExportedStruct).greet()
    // CHECK-NEXT: class greeting from C2
    (anys[1] as! ExportedClass).greet()
    // CHECK-NEXT: enum greeting from blue
    (anys[2] as! ExportedEnum).greet()

    // Throw the @export(interface) enum across the module boundary and
    // catch it as the concrete type in the application module.
    do {
      try throwExportedEnum()
    } catch let e as ExportedEnum {
      // CHECK-NEXT: caught enum greeting from red
      print("caught", terminator: " ")
      e.greet()
      // Exercise the cross-module Equatable conformance on the caught value.
      // CHECK-NEXT: true
      print(e == .red)
      // CHECK-NEXT: false
      print(e == .green)
    } catch {
      print("WRONG ERROR")
    }

    // CHECK-NEXT: DONE
    print("DONE")
  }
}
