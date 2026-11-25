// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) \
// RUN:     %s \
// RUN:     -enable-experimental-feature CoroutineAccessors \
// RUN:     -module-name Rock
// RUN: %FileCheck %s < %t.swiftinterface
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

// Old consumers get the same accessors under the legacy keywords.  (On
// non-ABI-stable platforms the legacy-ABI entry points aren't emitted, so old
// consumers can parse this but can't link against it; that's expected, since
// such platforms have no cross-compiler binary compatibility.)

// CHECK-NEXT: public var i: Swift::Int {
// CHECK-NEXT:   _read
// CHECK-NEXT:   _modify
// CHECK-NEXT: }
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

// The property mirrors `var i` above.

// CHECK:      public struct S {
// CHECK:        #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT:   public var value: Swift::Int {
// CHECK-NEXT:     yielding borrow
// CHECK-NEXT:     yielding mutate
// CHECK-NEXT:   }
// CHECK-NEXT:   #else
// CHECK-NEXT:   public var value: Swift::Int {
// CHECK-NEXT:     _read
// CHECK-NEXT:     _modify
// CHECK-NEXT:   }
// CHECK-NEXT:   #endif

// The subscript mirrors the property.

// CHECK-NEXT:   #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT:   public subscript(i: Swift::Int) -> Swift::Int {
// CHECK-NEXT:     yielding borrow
// CHECK-NEXT:     yielding mutate
// CHECK-NEXT:   }
// CHECK-NEXT:   #else
// CHECK-NEXT:   public subscript(i: Swift::Int) -> Swift::Int {
// CHECK-NEXT:     _read
// CHECK-NEXT:     _modify
// CHECK-NEXT:   }
// CHECK-NEXT:   #endif
// CHECK:      }



// === Inlinable coroutine accessors ===

public struct Inlinable {
  @usableFromInline var x = 0
  public init() {}
  @inlinable public var value: Int {
    get { x }
    yielding mutate { yield &x; x += 1 }
  }
  @inlinable public var view: Int {
    yielding borrow { yield x }
  }
}

// Old consumers must get the accessor bodies too: the legacy branch prints the
// `yielding` accessors under the legacy keywords, not the synthesized legacy
// accessors that forward to them (which have no body to print).

// CHECK:      public struct Inlinable {
// CHECK:        #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT:   @inlinable public var value: Swift::Int {
// CHECK-NEXT:     get { x }
// CHECK-NEXT:     yielding mutate { yield &x; x += 1 }
// CHECK-NEXT:   }
// CHECK-NEXT:   #else
// CHECK-NEXT:   @inlinable public var value: Swift::Int {
// CHECK-NEXT:     get { x }
// CHECK-NEXT:     _modify { yield &x; x += 1 }
// CHECK-NEXT:   }
// CHECK-NEXT:   #endif
// CHECK-NEXT:   #if compiler(>=5.3) && $CoroutineAccessors
// CHECK-NEXT:   @inlinable public var view: Swift::Int {
// CHECK-NEXT:     yielding borrow { yield x }
// CHECK-NEXT:   }
// CHECK-NEXT:   #else
// CHECK-NEXT:   @inlinable public var view: Swift::Int {
// CHECK-NEXT:     _read { yield x }
// CHECK-NEXT:   }
// CHECK-NEXT:   #endif
// CHECK:      }
