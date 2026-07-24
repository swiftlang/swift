// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) \
// RUN:     %s \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -module-name Rock
// RUN: %FileCheck %s < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Rock

// REQUIRES: swift_feature_CoroutineAccessors

var _i: Int = 0

// CHECK:      #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT: public var i: Swift::Int {
// CHECK-NEXT:   yielding borrow
// CHECK-NEXT:   yielding mutate
// CHECK-NEXT: }
// CHECK-NEXT: #else
// CHECK-NEXT: public var i: Swift::Int {
// CHECK-NEXT:   _read
// CHECK-NEXT:   _modify
// CHECK-NEXT: }
// CHECK-NEXT: #endif
public var i: Int {
  yielding borrow {
    yield _i
  }
  yielding mutate {
    yield &_i
  }
}

// A protocol requirement with coroutine accessors.  The textual form of a
// requirement is the same with or without the feature (`{ get set }`), so the
// availability guard's two branches print identically; the subscript
// requirement is printed without a guard.  This locks in that printing and
// verifies the interface round-trips.
// CHECK:      public protocol P {
// CHECK:        #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT:   @_borrowed var value: Swift::Int { get set }
// CHECK-NEXT:   #else
// CHECK-NEXT:   @_borrowed var value: Swift::Int { get set }
// CHECK-NEXT:   #endif
// CHECK:        @_borrowed subscript(i: Swift::Int) -> Swift::Int { get set }
// CHECK:      }
public protocol P {
  @_borrowed var value: Int { get set }
  @_borrowed subscript(i: Int) -> Int { get set }
}
