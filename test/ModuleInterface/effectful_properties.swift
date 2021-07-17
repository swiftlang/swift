// RUN: %target-swift-frontend -typecheck -swift-version 5 -enable-library-evolution -emit-module-interface-path %t.swiftinterface %s -module-name EffProps -disable-availability-checking
// RUN: %FileCheck %s < %t.swiftinterface

public struct MyStruct {}

// CHECK-LABEL:  #if compiler(>=5.3) && $EffectfulProp
// CHECK:        public var status: Swift.Bool {
// CHECK:          get async throws
// CHECK:        }
// CHECK:        #endif

public extension MyStruct {
  struct InnerStruct {
      public var status: Bool { get async throws { false } }
    }
}

// CHECK-LABEL:  #if compiler(>=5.3) && $EffectfulProp
// CHECK:        public var hello: Swift.Int {
// CHECK:            get async
// CHECK:          }
// CHECK:        #endif


// CHECK-LABEL:  #if compiler(>=5.3) && $EffectfulProp
// CHECK:        public subscript(x: Swift.Int) -> Swift.Void {
// CHECK:            get async throws
// CHECK:          }
// CHECK:        #endif

public class C {
  public var hello: Int { get async { 0 } }

  public subscript(_ x: Int) -> Void {
    get async throws { }
  }
}

// CHECK-LABEL:  #if compiler(>=5.3) && $EffectfulProp
// CHECK:        public var world: Swift.Int {
// CHECK:          get throws
// CHECK:        }
// CHECK:        #endif

public enum E {
  public var world: Int { get throws { 0 } }
}

// CHECK-LABEL:  #if compiler(>=5.3) && $EffectfulProp
// CHECK:        var books: Swift.Int { get async }
// CHECK:        #endif


// CHECK-LABEL:  #if compiler(>=5.3) && $EffectfulProp
// CHECK:        subscript(x: Swift.Int) -> Swift.Int { get throws }
// CHECK:        #endif

public protocol P {
  var books: Int { get async }
  subscript(_ x: Int) -> Int { get throws }
}
