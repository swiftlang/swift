// Emit the Swift module.
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -module-name MyModule -o %t/MyModule.swiftmodule %s

// Invoke the frontend with the module on the import path.
// RUN: %target-swift-synthesize-interface -module-name MyModule -I %t -o - | %FileCheck %s

public struct MyStruct {
// CHECK:     public struct MyStruct {
  public private(set) var value: Int
  // CHECK-DAG:     public private(set) var value: Int { get }

  public init(value: Int = 0) {
  // CHECK-DAG:     public init(value: Int = 0)
    self.value = value
  }

  public func printValue() {
  // CHECK-DAG:     public func printValue()
    print(self.value)
  }
}
// CHECK-DAG: }
