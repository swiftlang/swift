// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename=%s -disable-objc-attr-requires-foundation-module | %FileCheck %s

struct S<T> {}

public protocol P {
}
extension Int: P {
}

public protocol ProtocolWithDep {
  associatedtype Element
}

public class C1 {
}

class Base {}
class Sub : Base {}
class NonSub {}

// Specialize freestanding functions with the correct number of concrete types.
// ----------------------------------------------------------------------------

// CHECK: @_specialize(exported: false, kind: full, where T == Int)
@_specialize(where T == Int)
// CHECK: @_specialize(exported: false, kind: full, where T == S<Int>)
@_specialize(where T == S<Int>)
@_specialize(where T == Int, U == Int) // expected-error{{use of undeclared type 'U'}},
@_specialize(where T == T1) // expected-error{{use of undeclared type 'T1'}}
public func oneGenericParam<T>(_ t: T) -> T {
  return t
}

// CHECK: @_specialize(exported: false, kind: full, where T == Int, U == Int)
@_specialize(where T == Int, U == Int)
@_specialize(where T == Int) // expected-error{{too few type parameters are specified in '_specialize' attribute (got 1, but expected 2)}} expected-error{{Missing constraint for 'U' in '_specialize' attribute}}
public func twoGenericParams<T, U>(_ t: T, u: U) -> (T, U) {
  return (t, u)
}

@_specialize(where T == Int) // expected-error{{trailing 'where' clause in '_specialize' attribute of non-generic function 'nonGenericParam'}}
func nonGenericParam(x: Int) {}

// Specialize contextual types.
// ----------------------------

class G<T> {
  // CHECK: @_specialize(exported: false, kind: full, where T == Int)
  @_specialize(where T == Int)
  @_specialize(where T == T) // expected-error{{Only concrete type same-type requirements are supported by '_specialize' attribute}}
  @_specialize(where T == S<T>) // expected-error{{Only concrete type same-type requirements are supported by '_specialize' attribute}}
  @_specialize(where T == Int, U == Int) // expected-error{{use of undeclared type 'U'}}
  func noGenericParams() {}

  // CHECK: @_specialize(exported: false, kind: full, where T == Int, U == Float)
  @_specialize(where T == Int, U == Float)
  // CHECK: @_specialize(exported: false, kind: full, where T == Int, U == S<Int>)
  @_specialize(where T == Int, U == S<Int>)
  @_specialize(where T == Int) // expected-error{{too few type parameters are specified in '_specialize' attribute (got 1, but expected 2)}} expected-error {{Missing constraint for 'U' in '_specialize' attribute}}
  func oneGenericParam<U>(_ t: T, u: U) -> (U, T) {
    return (u, t)
  }
}

// Specialize with requirements.
// -----------------------------

protocol Thing {}

struct AThing : Thing {}

// CHECK: @_specialize(exported: false, kind: full, where T == AThing)
@_specialize(where T == AThing)
@_specialize(where T == Int) // expected-error{{same-type constraint type 'Int' does not conform to required protocol 'Thing'}}
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
@_specialize(where T == FloatElement)
@_specialize(where T == IntElement) // expected-error{{generic parameter 'T.Element' cannot be equal to both 'Float' and 'Int'}}
func sameTypeRequirement<T : HasElt>(_ t: T) where T.Element == Float {}

@_specialize(where T == Sub)
@_specialize(where T == NonSub) // expected-error{{'T' requires that 'NonSub' inherit from 'Base'}}
func superTypeRequirement<T : Base>(_ t: T) {}

@_specialize(where X:_Trivial(8), Y == Int) // expected-error{{trailing 'where' clause in '_specialize' attribute of non-generic function 'requirementOnNonGenericFunction'}}
public func requirementOnNonGenericFunction(x: Int, y: Int) {
}

@_specialize(where Y == Int) // expected-error{{too few type parameters are specified in '_specialize' attribute (got 1, but expected 2)}} expected-error{{Missing constraint for 'X' in '_specialize' attribute}}
public func missingRequirement<X:P, Y>(x: X, y: Y) {
}

@_specialize(where) // expected-error{{expected identifier for type name}}
@_specialize() // expected-error{{expected a parameter label or a where clause in '_specialize' attribute}} expected-error{{expected declaration}}
public func funcWithEmptySpecializeAttr<X: P, Y>(x: X, y: Y) {
}


@_specialize(where X:_Trivial(8), Y:_Trivial(32), Z == Int) // expected-error{{use of undeclared type 'Z'}}
@_specialize(where X:_Trivial(8), Y:_Trivial(32, 4))
@_specialize(where X == Int) // expected-error{{too few type parameters are specified in '_specialize' attribute (got 1, but expected 2)}} expected-error{{Missing constraint for 'Y' in '_specialize' attribute}}
@_specialize(where Y:_Trivial(32)) // expected-error {{too few type parameters are specified in '_specialize' attribute (got 1, but expected 2)}} expected-error{{Missing constraint for 'X' in '_specialize' attribute}}
@_specialize(where Y: P) // expected-error{{Only same-type and layout requirements are supported by '_specialize' attribute}} expected-error{{too few type parameters are specified in '_specialize' attribute (got 1, but expected 2)}} expected-error{{Missing constraint for 'X' in '_specialize' attribute}}
@_specialize(where Y: MyClass) // expected-error{{use of undeclared type 'MyClass'}} expected-error{{too few type parameters are specified in '_specialize' attribute (got 1, but expected 2)}} expected-error{{Missing constraint for 'X' in '_specialize' attribute}}
@_specialize(where X:_Trivial(8), Y == Int)
@_specialize(where X == Int, Y == Int)
@_specialize(where X == Int, X == Int) // expected-error{{too few type parameters are specified in '_specialize' attribute (got 1, but expected 2)}} expected-error{{Missing constraint for 'Y' in '_specialize' attribute}}
@_specialize(where Y:_Trivial(32), X == Float)
@_specialize(where X1 == Int, Y1 == Int) // expected-error{{use of undeclared type 'X1'}} expected-error{{use of undeclared type 'Y1'}} expected-error{{too few type parameters are specified in '_specialize' attribute (got 0, but expected 2)}} expected-error{{Missing constraint for 'X' in '_specialize' attribute}} expected-error{{Missing constraint for 'Y' in '_specialize' attribute}}
public func funcWithTwoGenericParameters<X, Y>(x: X, y: Y) {
}

@_specialize(where X == Int, Y == Int)
@_specialize(exported: true, where X == Int, Y == Int)
@_specialize(exported: false, where X == Int, Y == Int)
@_specialize(exported: false where X == Int, Y == Int) // expected-error{{missing ',' in '_specialize' attribute}}
@_specialize(exported: yes, where X == Int, Y == Int) // expected-error{{expected a boolean true or false value in '_specialize' attribute}}
@_specialize(exported: , where X == Int, Y == Int) // expected-error{{expected a boolean true or false value in '_specialize' attribute}}

@_specialize(kind: partial, where X == Int, Y == Int)
@_specialize(kind: partial, where X == Int)
@_specialize(kind: full, where X == Int, Y == Int)
@_specialize(kind: any, where X == Int, Y == Int) // expected-error{{expected 'partial' or 'full' as values of the 'kind' parameter in '_specialize' attribute}}
@_specialize(kind: false, where X == Int, Y == Int) // expected-error{{expected 'partial' or 'full' as values of the 'kind' parameter in '_specialize' attribute}}
@_specialize(kind: partial where X == Int, Y == Int) // expected-error{{missing ',' in '_specialize' attribute}}
@_specialize(kind: partial, where X == Int, Y == Int)
@_specialize(kind: , where X == Int, Y == Int)

@_specialize(exported: true, kind: partial, where X == Int, Y == Int)
@_specialize(exported: true, exported: true, where X == Int, Y == Int) // expected-error{{parameter 'exported' was already defined in '_specialize' attribute}}
@_specialize(kind: partial, exported: true, where X == Int, Y == Int)
@_specialize(kind: partial, kind: partial, where X == Int, Y == Int) // expected-error{{parameter 'kind' was already defined in '_specialize' attribute}}

@_specialize(where X == Int, Y == Int, exported: true, kind: partial) // expected-error{{use of undeclared type 'exported'}} expected-error{{use of undeclared type 'kind'}} expected-error{{use of undeclared type 'partial'}} expected-error{{expected identifier for type name}}
public func anotherFuncWithTwoGenericParameters<X: P, Y>(x: X, y: Y) {
}

@_specialize(where T: P) // expected-error{{Only same-type and layout requirements are supported by '_specialize' attribute}}
@_specialize(where T: Int) // expected-error{{Only conformances to protocol types are supported by '_specialize' attribute}} expected-error{{Only same-type and layout requirements are supported by '_specialize' attribute}}

@_specialize(where T: S1) // expected-error{{Only conformances to protocol types are supported by '_specialize' attribute}} expected-error{{Only same-type and layout requirements are supported by '_specialize' attribute}}
@_specialize(where T: C1) // expected-error{{Only conformances to protocol types are supported by '_specialize' attribute}} expected-error{{Only same-type and layout requirements are supported by '_specialize' attribute}}
@_specialize(where Int: P) // expected-error{{type 'Int' in conformance requirement does not refer to a generic parameter or associated type}} expected-error{{Only same-type and layout requirements are supported by '_specialize' attribute}} expected-error{{too few type parameters are specified in '_specialize' attribute (got 0, but expected 1)}} expected-error{{Missing constraint for 'T' in '_specialize' attribute}}
func funcWithForbiddenSpecializeRequirement<T>(_ t: T) {
}

@_specialize(where T: _Trivial(32), T: _Trivial(64), T: _Trivial, T: _RefCountedObject) // expected-error{{multiple layout constraints cannot be used at the same time: '_Trivial(64)' and '_Trivial(32)'}} expected-note{{previous layout constraint declaration '_Trivial(32)' was here}} expected-error{{multiple layout constraints cannot be used at the same time: '_Trivial' and '_Trivial(32)'}} expected-note{{previous layout constraint declaration '_Trivial(32)' was here}} expected-error{{multiple layout constraints cannot be used at the same time: '_RefCountedObject' and '_Trivial(32)'}} expected-note{{previous layout constraint declaration '_Trivial(32)' was here}}
@_specialize(where Array<T> == Int) // expected-error{{Only requirements on generic parameters are supported by '_specialize' attribute}}
// expected-error@-1{{generic signature requires types 'Array<T>' and 'Int' to be the same}}
@_specialize(where T.Element == Int) // expected-error{{Only requirements on generic parameters are supported by '_specialize' attribute}}
public func funcWithComplexSpecializeRequirements<T: ProtocolWithDep>(t: T) -> Int {
  return 55555
}

public struct S1 {
}

@_specialize(exported: false, where T == Int64)
public func simpleGeneric<T>(t: T) -> T {
  return t
}


@_specialize(exported: true, where S: _Trivial(64))
// Check that any bitsize size is OK, not only powers of 8.
@_specialize(where S: _Trivial(60))
@_specialize(exported: true, where S: _RefCountedObject)
@inline(never)
public func copyValue<S>(_ t: S, s: inout S) -> Int64 where S: P{
  return 1
}

@_specialize(exported: true, where S: _Trivial)
@_specialize(exported: true, where S: _Trivial(64))
@_specialize(exported: true, where S: _Trivial(32))
@_specialize(exported: true, where S: _RefCountedObject)
@inline(never)
public func copyValueAndReturn<S>(_ t: S, s: inout S) -> S where S: P{
  return s
}

struct OuterStruct<S> {
  struct MyStruct<T> {
    @_specialize(where T == Int, U == Float) // expected-error{{too few type parameters are specified in '_specialize' attribute (got 2, but expected 3)}} expected-error{{Missing constraint for 'S' in '_specialize' attribute}}
    public func foo<U>(u : U) {
    }

    @_specialize(where T == Int, U == Float, S == Int)
    public func bar<U>(u : U) {
    }
  }
}

// Check _TrivialAtMostN constraints.
@_specialize(exported: true, where S: _TrivialAtMost(64))
@inline(never)
public func copy2<S>(_ t: S, s: inout S) -> S where S: P{
  return s
}

// Check missing alignment.
@_specialize(where S: _Trivial(64, )) // expected-error{{expected non-negative alignment to be specified in layout constraint}}
// Check non-numeric size.
@_specialize(where S: _Trivial(Int)) // expected-error{{expected non-negative size to be specified in layout constraint}}
// Check non-numeric alignment.
@_specialize(where S: _Trivial(64, X)) // expected-error{{expected non-negative alignment to be specified in layout constraint}}
@inline(never)
public func copy3<S>(_ s: S) -> S {
  return s
}

public func funcWithWhereClause<T>(t: T) where T:P, T: _Trivial(64) { // expected-error{{layout constraints are only allowed inside '_specialize' attributes}}
}

// rdar://problem/29333056
public protocol P1 {
  associatedtype DP1
  associatedtype DP11
}

public protocol P2 {
  associatedtype DP2 : P1
}

public struct H<T> {
}

public struct MyStruct3 : P1 {
  public typealias DP1 = Int
  public typealias DP11 = H<Int>
}

public struct MyStruct4 : P2 {
  public typealias DP2 = MyStruct3
}

@_specialize(where T==MyStruct4)
public func foo<T: P2>(_ t: T) where T.DP2.DP11 == H<T.DP2.DP1> {
}
