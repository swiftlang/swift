// RUN: %target-parse-verify-swift -enable-experimental-nested-generic-types

struct MyType<TyA, TyB> { // expected-note {{declared here}}
  var a : TyA, b : TyB
}

//
// Type aliases that reference unbound generic types -- not really generic,
// but they behave as such, in the sense that you can apply generic
// arguments to them.
//

// FIXME: This should work?
typealias OurType = MyType  // expected-error {{reference to generic type 'MyType' requires arguments in <...>}}

typealias YourType = Swift.Optional

struct Container {
  typealias YourType = Swift.Optional
}

let _ : YourType<Int>
let _ : Container.YourType<Int>

//
// Bona-fide generic type aliases
//

typealias DS<T> = MyType<String, T>

typealias BadA<T : Int> = MyType<String, T>  // expected-error {{inheritance from non-protocol, non-class type 'Int'}}

typealias BadB<T where T == Int> = MyType<String, T>  // expected-error {{associated types may not have a generic parameter list}}
// expected-error @-1 {{same-type requirement makes generic parameter 'T' non-generic}}

typealias BadC<T,T> = MyType<String, T>  // expected-error {{definition conflicts with previous value}}
// expected-note @-1 {{previous definition of 'T' is here}}

typealias Tuple2<T1, T2> = (T1, T2)

typealias Tuple3<T1> = (T1, T1) where T1 : Hashable


let _ : Tuple2<Int, String> = (1, "foo")
let _ : Tuple2 = (1, "foo")
let _ : Tuple2<Int, String> = ("bar",  // expected-error {{cannot convert value of type 'String' to specified type 'Int'}}
                               "foo")

func f() {
  typealias Tuple2b<T1, T2> = (T1, T2)
  let _ : Tuple2b = (1, "foo")
  
}


typealias A<T1, T2> = MyType<T2, T1>  // expected-note {{generic type 'A' declared here}}

typealias B<T1> = MyType<T1, T1>  // expected-note {{'T1' declared as parameter to type 'B'}}

typealias C<T> = MyType<String, T>

// Type aliases with unused generic params.
typealias D<T1, T2, T3> = MyType<T2, T1>  // expected-note {{'T3' declared as parameter to type 'D'}}

typealias E<T1, T2> = Int  // expected-note {{generic type 'E' declared here}}

typealias F<T1, T2> = (T1) -> T2

// Type alias of type alias.
typealias G<S1, S2> = A<S1, S2>

let _ : E<Int, Float> = 42
let _ : E<Float> = 42   // expected-error {{generic type 'E' specialized with too few type parameters (got 1, but expected 2)}}
let _ : E = 42   // expected-error {{cannot convert value of type 'Int' to specified type 'E'}}
let _ : D = D(a: 1, b: 2)  // expected-error {{cannot convert value of type 'MyType<Int, Int>' to specified type 'D'}}

let _ : D<Int, Int, Float> = D<Int, Int, Float>(a: 1, b: 2)

// expected-error @+1 {{cannot convert value of type 'MyType<Int, Int>' to specified type 'D'}}
let _ : D = D<Int, Int, Float>(a: 1, b: 2)


// expected-error @+1 {{generic parameter 'T3' could not be inferred}}
let _ : D<Int, Int, Float> = D(a: 1, b: 2)

let _ : F = { (a : Int) -> Int in a }  // Infer the types of F

// TODO QoI: Cannot infer T1/T2.
let _ : F = { a in a }  // expected-error {{cannot convert value of type '(_) -> _' to specified type 'F'}}

_ = MyType(a: "foo", b: 42)
_ = A(a: "foo", b: 42)
_ = A<Int, String>(a: "foo", b: 42)
_ = A<String, Int>(a: "foo", // expected-error {{'String' is not convertible to 'Int'}}
  b: 42)
_ = B(a: 12, b: 42)
_ = B(a: 12, b: 42 as Float)
_ = B(a: "foo", b: 42)     // expected-error {{generic parameter 'T1' could not be inferred}}
_ = C(a: "foo", b: 42)
_ = C(a: 42,        // expected-error {{'Int' is not convertible to 'String'}}
  b: 42)

_ = G(a: "foo", b: 42)
_ = G<Int, String>(a: "foo", b: 42)


struct MyTypeWithHashable<TyA, TyB : Hashable> {
}

typealias MTWHInt<HT : Hashable> = MyTypeWithHashable<Int, HT>
typealias MTWHInt2<HT> = MyTypeWithHashable<Int, HT> // expected-error {{type 'HT' does not conform to protocol 'Hashable'}}

func f(a : MyTypeWithHashable<Int, Int>) {
  f(a: MyTypeWithHashable<Int, Int>())
  f(a: MTWHInt<Int>())
}


// Unqualified lookup of generic typealiases nested inside generic contexts
class GenericClass<T> {
  typealias TA<U> = MyType<T, U>
  typealias TAI<U> = MyType<Int, U>
  
  func testCapture<S>(s: S, t: T) -> TA<S> {
    return TA<S>(a: t, b: s)
  }

  func testCaptureUnbound<S>(s: S, t: T) -> TA<S> {
    return TA(a: t, b: s)
  }

  func testConcrete1(s: Int, t: T) -> TA<Int> {
    return TA<Int>(a: t, b: s)
  }

  func testConcreteUnbound1(s: Int, t: T) -> TA<Int> {
    return TA(a: t, b: s)
  }

  func testConcrete2(s: Float, t: Int) -> TAI<Float> {
    return TAI<Float>(a: t, b: s)
  }

  func testConcreteUnbound2(s: Float, t: Int) -> TAI<Float> {
    return TAI(a: t, b: s)
  }

  // FIXME: Would be nice to preserve sugar here

  func testCaptureInvalid1<S>(s: S, t: T) -> TA<Int> {
    return TA<S>(a: t, b: s) // expected-error {{cannot convert return expression of type 'MyType<T, S>' to return type 'MyType<T, Int>'}}
  }

  func testCaptureInvalid2<S>(s: Int, t: T) -> TA<S> {
    return TA(a: t, b: s) // expected-error {{cannot convert return expression of type 'MyType<T, Int>' to return type 'MyType<T, S>'}}
  }

  struct NestedStruct<U> {
    typealias TA<V> = MyType<(T, V), (U, V)>

    func testCapture<S>(x: (T, S), y: (U, S)) -> TA<S> {
      return TA(a: x, b: y)
    }
  }
}

let gc = GenericClass<Double>()
let fn: MyType<Double, Int> = gc.testCapture(s: 1, t: 1.0)

func use<T>(_ t: T) {}
use(fn)

// Make sure we apply base substitutions to the interface type of the typealias
class ConcreteClass : GenericClass<String> {
  func testSubstitutedCapture1<S>(s: S, t: String) -> TA<S> {
    return TA<S>(a: t, b: s)
  }

  func testSubstitutedCapture2<S>(s: S, t: String) -> TA<S> {
    return TA(a: t, b: s)
  }

  func testSubstitutedCapture3(s: Int, t: String) -> TA<Int> {
    return TA<Int>(a: t, b: s)
  }

  func testSubstitutedCapture4(s: Int, t: String) -> TA<Int> {
    return TA(a: t, b: s)
  }

  func testSubstitutedCapture5(s: Float, t: Int) -> TAI<Float> {
    return TAI<Float>(a: t, b: s)
  }

  func testSubstitutedCapture6(s: Float, t: Int) -> TAI<Float> {
    return TAI(a: t, b: s)
  }
}

// Qualified lookup of generic typealiases nested inside concrete contexts
struct ConcreteStruct {
  typealias O<T> = Optional<T>
}

func takesUnsugaredType1(m: MyType<String, Float>) {}
func takesSugaredType1(m: ConcreteClass.TA<Float>) {
  takesUnsugaredType1(m: m)
}

// FIXME: Something is wrong with SpecializeExpr here
let _ = ConcreteStruct.O<Int>(123) // expected-error {{cannot invoke value of type 'Optional<Int>.Type' with argument list '(Int)'}}

// Qualified lookup of generic typealiases nested inside generic contexts

// FIXME: Something is wrong with SpecializeExpr here
let _ = GenericClass<Int>.TA<Float>(a: 4.0, b: 1)  // expected-error {{'Int' is not convertible to 'Float'}}
let _ = GenericClass<Int>.TA<Float>(a: 1, b: 4.0)  // expected-error {{'Int' is not convertible to 'Float'}}

func takesUnsugaredType2(m: MyType<Int, Float>) {}
func takesSugaredType2(m: GenericClass<Int>.TA<Float>) {
  takesUnsugaredType2(m: m)
}



extension A {}  // expected-error {{non-nominal type 'A' cannot be extended}}
extension A<T> {}  // expected-error {{generic type 'A' specialized with too few type parameters (got 1, but expected 2)}}
extension A<Float,Int> {}  // expected-error {{constrained extension must be declared on the unspecialized generic type 'MyType' with constraints specified by a 'where' clause}}
extension C<T> {}  // expected-error {{use of undeclared type 'T'}}
extension C<Int> {}  // expected-error {{constrained extension must be declared on the unspecialized generic type 'MyType' with constraints specified by a 'where' clause}}


