// RUN: %target-typecheck-verify-swift -enable-experimental-structural-opaque-types -disable-availability-checking

// Tests for experimental extensions to opaque return type support.

protocol P { func paul() }
protocol Q {}

extension Int: P, Q { func paul() {} }
extension String: P, Q { func paul() {} }

class C {}
class D: P, Q { func paul() {}; func d() {} }


// TODO: cases that we should support, but don't yet:
//
// WARNING: using '!' is not allowed here; treating this as '?' instead
// func asUnwrappedOptionalBase() -> (some P)! { return 1 }
//
// ERROR: generic parameter 'τ_0_0' could not be inferred
// func asHOFRetRet() -> () -> some P { return { 1 } }
//
// ERROR: function declares an opaque return type, but has no return statements in its body from which to infer an underlying type
// func asHOFRetArg() -> (some P) -> () { return { (x: Int) -> () in } }
//
// ERROR: 'some' types are only implemented for the declared type of properties and subscripts and the return type of functions
// let x = { () -> some P in return 1 }

func twoOpaqueTypes() -> (some P, some P) { return (1, 2) } // expected-error{{only one 'opaque' type is supported}}
func asTupleElemBad() -> (P, some Q) { return (1, C()) } // expected-note{{opaque return type declared here}} expected-error{{requires that 'C' conform to 'Q'}}

func asTupleElem() -> (P, some Q) { return (1, 2) }
func asArrayElem() -> [some P] { return [1] }
func asOptionalBase() -> (some P)? { return 1 }

let asTupleElemLet: (P, some Q) = (1, 2)
let asArrayElemLet: [some P] = [1]
let asOptionalBaseLet: (some P)? = 1

struct S1<T> {
  var x: T
}
struct S2<T, U> {
  var x: T
  var y: U
}
struct R1<T: P> {
  var x: T
}
struct R2<T: P, U: Q> {
  var x: T
  var y: U
}

func asUnconstrainedGeneric1() -> S1<some P> { return S1(x: 1) }
func asUnconstrainedGeneric2() -> S2<P, some Q> { return S2(x: 1, y: 2) }
func asConstrainedGeneric1() -> R1<some P> { return R1(x: 1) }
func asConstrainedGeneric2() -> R2<Int, some Q> { return R2(x: 1, y: 2) }
func asNestedGenericDirect() -> S1<S1<some P>> { return S1(x: S1(x: 1)) }
func asNestedGenericIndirect() -> S1<S1<(Int, some P)>> { return S1(x: S1(x: (1, 2))) }

let asUnconstrainedGeneric2Let: S2<P, some Q> = S2(x: 1, y: 2)
let asNestedGenericIndirectLet: S1<S1<(Int, some P)>> = S1(x: S1(x: (1, 2)))

// Tests an interesting SILGen case. For the underlying opaque type, we have to
// use the generic calling convention for closures.
func funcToAnyOpaqueCoercion() -> S1<some Any> {
  let f: () -> () = {}
  return S1(x: f)
}

// TODO: We should give better error messages here. The opaque types have
// underlying types 'Int' and 'String', but the return statments have underlying
// types '(Int, Int)' and '(String, Int)'.
func structuralMismatchedReturnTypes(_ x: Bool, _ y: Int, _ z: String) -> (some P, Int) { // expected-error{{do not have matching underlying types}}
  if x {
    return (y, 1) // expected-note{{return statement has underlying type 'Int'}}
  } else {
    return (z, 1) // expected-note{{return statement has underlying type 'String'}}
  }
}

func structuralMemberLookupBad() {
  var tup: (some P, Int) = (D(), 1)
  tup.0.paul();
  tup.0.d(); // expected-error{{value of type 'some P' has no member 'd'}}
}
