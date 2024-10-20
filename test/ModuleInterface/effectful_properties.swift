// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name EffProps -target %target-swift-5.1-abi-triple
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name EffProps -target %target-swift-5.1-abi-triple
// RUN: %FileCheck %s < %t.swiftinterface

public struct MyStruct {}

// CHECK-NOT:    #if compiler(>=5.3) && $EffectfulProp
// CHECK:        public var status: Swift.Bool {
// CHECK:          get async throws
// CHECK:        }

public extension MyStruct {
  struct InnerStruct {
      public var status: Bool { get async throws { false } }
    }
}

// CHECK-NOT:    #if compiler(>=5.3) && $EffectfulProp
// CHECK:        public var hello: Swift.Int {
// CHECK:            get async
// CHECK:          }


// CHECK-NOT:    #if compiler(>=5.3) && $EffectfulProp
// CHECK:        public subscript(x: Swift.Int) -> Swift.Void {
// CHECK:            get async throws
// CHECK:          }

public class C {
  public var hello: Int { get async { 0 } }

  public subscript(_ x: Int) -> Void {
    get async throws { }
  }
}

// CHECK-NOT:    #if compiler(>=5.3) && $EffectfulProp
// CHECK:        public var world: Swift.Int {
// CHECK:          get throws
// CHECK:        }

public enum E {
  public var world: Int { get throws { 0 } }
}

// CHECK-NOT:    #if compiler(>=5.3) && $EffectfulProp
// CHECK:        var books: Swift.Int { get async }


// CHECK-NOT:    #if compiler(>=5.3) && $EffectfulProp
// CHECK:        subscript(x: Swift.Int) -> Swift.Int { get throws }

public protocol P {
  var books: Int { get async }
  subscript(_ x: Int) -> Int { get throws }
}
