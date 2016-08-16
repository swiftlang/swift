// RUN: %target-parse-verify-swift
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename=%s -disable-objc-attr-requires-foundation-module | %FileCheck %s

struct S<T> {}

// Specialize freestanding functions with the correct number of concrete types.
// ----------------------------------------------------------------------------

// CHECK: @_specialize(Int)
@_specialize(Int)
// CHECK: @_specialize(S<Int>)
@_specialize(S<Int>)
@_specialize(Int, Int) // expected-error{{generic type 'oneGenericParam' specialized with too many type parameters (got 2, but expected 1)}},
@_specialize(T) // expected-error{{use of undeclared type 'T'}}
public func oneGenericParam<T>(_ t: T) -> T {
  return t
}

// CHECK: @_specialize(Int, Int)
@_specialize(Int, Int)
@_specialize(Int) // expected-error{{generic type 'twoGenericParams' specialized with too few type parameters (got 1, but expected 2)}},
public func twoGenericParams<T, U>(_ t: T, u: U) -> (T, U) {
  return (t, u)
}

// Specialize contextual types.
// ----------------------------

class G<T> {
  // CHECK: @_specialize(Int)
  @_specialize(Int)
  @_specialize(T) // expected-error{{cannot partially specialize a generic function}}
  @_specialize(S<T>) // expected-error{{cannot partially specialize a generic function}}
  @_specialize(Int, Int) // expected-error{{generic type 'noGenericParams' specialized with too many type parameters (got 2, but expected 1)}}
  func noGenericParams() {}

  // CHECK: @_specialize(Int, Float)
  @_specialize(Int, Float)
  // CHECK: @_specialize(Int, S<Int>)
  @_specialize(Int, S<Int>)
  @_specialize(Int) // expected-error{{generic type 'oneGenericParam' specialized with too few type parameters (got 1, but expected 2)}},
  func oneGenericParam<U>(_ t: T, u: U) -> (U, T) {
    return (u, t)
  }
}

// Specialize with requirements.
// -----------------------------

protocol Thing {}

struct AThing : Thing {}

// CHECK: @_specialize(AThing)
@_specialize(AThing)
@_specialize(Int) // expected-error{{argument type 'Int' does not conform to expected type 'Thing'}}
func oneRequirement<T : Thing>(_ t: T) {}

protocol HasElt {
  associatedtype Element
}
struct IntElement : HasElt {
  typealias Element = Int
}
struct FloatElement : HasElt {
  typealias Element = Float
}
@_specialize(FloatElement)
@_specialize(IntElement) // expected-error{{'<T : HasElt where T.Element == Float> (T) -> ()' requires the types 'IntElement.Element' (aka 'Int') and 'Float' be equivalent}}
func sameTypeRequirement<T : HasElt>(_ t: T) where T.Element == Float {}

class Base {}
class Sub : Base {}
class NonSub {}
@_specialize(Sub)
@_specialize(NonSub) // expected-error{{'<T : Base> (T) -> ()' requires that 'NonSub' inherit from 'Base'}}
func superTypeRequirement<T : Base>(_ t: T) {}
