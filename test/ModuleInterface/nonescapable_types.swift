// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test -enable-experimental-feature NonescapableTypes
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: #if compiler(>=5.3) && $NonescapableTypes
// CHECK: public protocol P : ~Escapable {
// CHECK:   associatedtype A
// CHECK: }
// CHECK: #else
// CHECK: public protocol P {
// CHECK:   associatedtype A
// CHECK: }
// CHECK: #endif
public protocol P: ~Escapable {
  associatedtype A
}

// CHECK: #if compiler(>=5.3) && $NonescapableTypes
// CHECK: public struct X<T> : ~Swift.Escapable where T : ~Escapable {
// CHECK: }
// CHECK: #else
// CHECK: public struct X<T> {
// CHECK: }
// CHECK: #endif
public struct X<T: ~Escapable>: ~Escapable { }

// CHECK: #if compiler(>=5.3) && $NonescapableTypes
// CHECK:      extension Test.X {
// CHECK-NEXT:   func f()
// CHECK:      }
// CHECK: #else
// CHECK:      extension Test.X {
// CHECK-NEXT:   func f()
// CHECK:      }
extension X where T: Escapable {
  public func f() { }
}

// CHECK: #if compiler(>=5.3) && $NonescapableTypes
// CHECK: extension Test.X where T : ~Escapable {
// CHECK:   public func g(other: borrowing T)
// CHECK: }
// CHECK: #else
// CHECK: extension Test.X {
// CHECK:   public func g(other: borrowing T)
// CHECK: }
// CHECK: #endif
extension X where T: ~Escapable {
  public func g(other: borrowing T) { }
}

// CHECK: #if compiler(>=5.3) && $NonescapableTypes
// CHECK: public enum Y<T> : ~Swift.Escapable where T : ~Escapable {
// CHECK:   case none
// CHECK:   case some(T)
// CHECK: }
// CHECK: #else
// CHECK: public enum Y<T> {
// CHECK:   case none
// CHECK:   case some(T)
// CHECK: }
public enum Y<T: ~Escapable>: ~Escapable {
  case none
  case some(T)
}

extension Y: Escapable where T: Escapable { }

// CHECK: #if compiler(>=5.3) && $NonescapableTypes
// CHECK: @lifetime(y)
// CHECK: public func derive<T>(_ y: Test.Y<T>) -> Test.Y<T> where T : ~Escapable
// CHECK: #else
// CHECK: public func derive<T>(_ y: Test.Y<T>) -> Test.Y<T>
// CHECK: #endif
@lifetime(y)
public func derive<T : ~Escapable>(_ y: Y<T>) -> Y<T> {
  y
}

// CHECK: #if compiler(>=5.3) && $NonescapableTypes
// CHECK: @lifetime(x)
// CHECK: public func derive<T>(_ x: Test.X<T>) -> Test.X<T> where T : ~Escapable
// CHECK: #else
// CHECK: public func derive<T>(_ x: Test.X<T>) -> Test.X<T>
// CHECK: #endif
@lifetime(x)
public func derive<T : ~Escapable>(_ x: X<T>) -> X<T> {
  x
}
