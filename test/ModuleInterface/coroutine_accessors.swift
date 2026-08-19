// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) \
// RUN:     %s \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -module-name Rock
// RUN: %FileCheck %s --check-prefixes=CHECK,CHECK-%target-abi-stability < %t.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Rock

// REQUIRES: swift_feature_CoroutineAccessors

// === A global public computed property ===

var _i: Int = 0

public var i: Int {
  yielding borrow {
    yield _i
  }
  yielding mutate {
    yield &_i
  }
}

// New consumers (who understand CoroutineAccessors) will see
// the new form.

// CHECK:      #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT: public var i: Swift::Int {
// CHECK-NEXT:   yielding borrow
// CHECK-NEXT:   yielding mutate
// CHECK-NEXT: }
// CHECK-NEXT: #else

// Old consumers will get the legacy form, which in this case
// implies the legacy ABI.

// CHECK-stable-NEXT: public var i: Swift::Int {
// CHECK-stable-NEXT:   _read
// CHECK-stable-NEXT:   _modify
// CHECK-stable-NEXT: }

// On non-ABI-stable platforms, we've stopped emitting the legacy ABI,
// so this property has no ABI on such platforms.

// CHECK-unstable-NEXT: public var i: Swift::Int {
// CHECK-unstable-NEXT: }
// CHECK-NEXT: #endif


// === A protocol requirement with (implied) coroutine accessors ===

public protocol P1 {
  @_borrowed var value: Int { get set }
  @_borrowed subscript(i: Int) -> Int { get set }
}

// The requirement is written identically with or without the feature, so the
// availability guard's two branches print identically -- for both the
// property and the subscript.

// CHECK:      public protocol P1 {
// CHECK:        #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT:   @_borrowed var value: Swift::Int { get set }
// CHECK-NEXT:   #else
// CHECK-NEXT:   @_borrowed var value: Swift::Int { get set }
// CHECK-NEXT:   #endif
// CHECK-NEXT:   #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT:   @_borrowed subscript(i: Swift::Int) -> Swift::Int { get set }
// CHECK-NEXT:   #else
// CHECK-NEXT:   @_borrowed subscript(i: Swift::Int) -> Swift::Int { get set }
// CHECK-NEXT:   #endif
// CHECK:      }


// === A concrete struct type ===

public struct S {
    public var value: Int {
        yielding borrow { var t = 0; yield t }
        yielding mutate { var t = 0; yield &t; print(t) }
    }
    public subscript(i: Int) -> Int {
        yielding borrow { var t = i; yield t }
        yielding mutate { var t = i; yield &t; print(t) }
    }
}

// The property mirrors `var i` above: same guard, same stable/unstable
// split for the legacy branch.

// CHECK:      public struct S {
// CHECK:        #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT:   public var value: Swift::Int {
// CHECK-NEXT:     yielding borrow
// CHECK-NEXT:     yielding mutate
// CHECK-NEXT:   }
// CHECK-NEXT:   #else
// CHECK-stable-NEXT: public var value: Swift::Int {
// CHECK-stable-NEXT:   _read
// CHECK-stable-NEXT:   _modify
// CHECK-stable-NEXT: }
// CHECK-unstable-NEXT: public var value: Swift::Int {
// CHECK-unstable-NEXT: }
// CHECK-NEXT:   #endif

// The subscript now mirrors the property: same guard, same stable/unstable
// split for the legacy branch.

// CHECK-NEXT:   #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT:   public subscript(i: Swift::Int) -> Swift::Int {
// CHECK-NEXT:     yielding borrow
// CHECK-NEXT:     yielding mutate
// CHECK-NEXT:   }
// CHECK-NEXT:   #else
// CHECK-stable-NEXT: public subscript(i: Swift::Int) -> Swift::Int {
// CHECK-stable-NEXT:   _read
// CHECK-stable-NEXT:   _modify
// CHECK-stable-NEXT: }
// CHECK-unstable-NEXT: public subscript(i: Swift::Int) -> Swift::Int {
// CHECK-unstable-NEXT: }
// CHECK-NEXT:   #endif
// CHECK:      }

