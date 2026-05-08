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

@export(interface)
public struct ExportedStruct {
  public var x: Int
  public init(x: Int) { self.x = x }
  public func describe() { print("ExportedStruct(x: \(x))") }
}

@export(interface)
public class ExportedClass {
  public var y: Int
  public init(y: Int) { self.y = y }
  public func describe() { print("ExportedClass(y: \(y))") }
}

//--- Application.swift
import Library

@main
struct Main {
  static func main() {
    // Wrap imported @export(interface) values in Any existentials, which
    // requires the type metadata for ExportedStruct / ExportedClass to be
    // visible across the module boundary.
    let values: [Any] = [
      ExportedStruct(x: 42),
      ExportedClass(y: 99),
    ]

    // CHECK: ExportedStruct(x: 42)
    (values[0] as! ExportedStruct).describe()
    // CHECK-NEXT: ExportedClass(y: 99)
    (values[1] as! ExportedClass).describe()
    // CHECK-NEXT: DONE
    print("DONE")
  }
}
