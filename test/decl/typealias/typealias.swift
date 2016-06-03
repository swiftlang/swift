// RUN: %target-parse-verify-swift

typealias rgb = Int32 // expected-note {{declared here}}
var rgb : rgb? // expected-error {{invalid redeclaration of 'rgb'}}


struct Color {
    var rgba : rgba? { // expected-error {{'rgba' used within its own type}}
        return nil
    }

    typealias rgba = Int32
}

struct Color2 {
    let rgba : rgba? // expected-error {{'rgba' used within its own type}}

    struct rgba {}
}






struct MyType<TyA, TyB> {
  var a : TyA, b : TyB
}

protocol P {
  associatedtype X<T>  // expected-error {{associated types may not have a generic parameter list}}
  
  typealias Y<T>       // expected-error {{expected '=' in typealias declaration}}
}

typealias basicTypealias = Int

typealias DSI = MyType<String, Int>
typealias DS<T> = MyType<String, T>

typealias BadA<T : Int> = MyType<String, T>  // expected-error {{inheritance from non-protocol, non-class type 'Int'}}

typealias BadB<T where T == Int> = MyType<String, T>  // expected-error {{associated types may not have a generic parameter list}}
// expected-error @-1 {{same-type requirement makes generic parameter 'T' non-generic}}

typealias BadC<T,T> = MyType<String, T>  // expected-error {{definition conflicts with previous value}}
// expected-note @-1 {{previous definition of 'T' is here}}

typealias Tuple2<T1, T2> = (T1, T2)

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




// FIXME: Nested generic typealiases aren't working yet.
struct GenericStruct<T> {
  typealias TA<U> = MyType<T, U>
  
  func testCapture<S>(s : S, t : T) -> TA<S> {  // expected-error {{cannot specialize non-generic type 'MyType<T, U>'}}
    return TA<S>(a: t, b : s)
  }
}

let _ = GenericStruct<Int>.TA<Float>(a: 4.0, b: 1)  // expected-error {{'Int' is not convertible to 'Float'}}

let _ : GenericStruct<Int>.TA<Float>  // expected-error {{cannot specialize non-generic type 'MyType<Int, U>'}}



extension A {}  // expected-error {{non-nominal type 'A' cannot be extended}}
extension A<T> {}  // expected-error {{generic type 'A' specialized with too few type parameters (got 1, but expected 2)}}
extension A<Float,Int> {}  // expected-error {{constrained extension must be declared on the unspecialized generic type 'MyType' with constraints specified by a 'where' clause}}
extension C<T> {}  // expected-error {{use of undeclared type 'T'}}
extension C<Int> {}  // expected-error {{constrained extension must be declared on the unspecialized generic type 'MyType' with constraints specified by a 'where' clause}}


// Allow typealias inside protocol, but don't allow it in where clauses (at least not yet)
protocol Col {
  associatedtype Elem
  var elem: Elem { get }
}

protocol CB {
  associatedtype C : Col
  typealias E = C.Elem
  
  func setIt(_ element: E)
}

func go1<T : CB, U : Col where U.Elem == T.E>(_ col: U, builder: T) { // OK
  builder.setIt(col.elem)
}
func go2<T : CB, U : Col where U.Elem == T.C.Elem>(_ col: U, builder: T) { // OK
  builder.setIt(col.elem)
}

// Test for same type requirement with typealias == concrete
func go3<T : CB where T.E == Int>(_ builder: T) {
  builder.setIt(1)
}

// Test for conformance to protocol with associatedtype and another with typealias with same name.
protocol MyIterator {
  associatedtype Elem
  
  func next() -> Elem?
}

protocol MySeq {
  associatedtype I : MyIterator
  typealias Elem = Self.I.Elem
  
  func makeIterator() -> I
  func getIndex(_ i: Int) -> Elem
  func first() -> Elem
}

extension MySeq where Self : MyIterator {
  func makeIterator() -> Self {
    return self
  }
}

func plusOne<S: MySeq where S.Elem == Int>(_ s: S, i: Int) -> Int {
  return s.getIndex(i) + 1
}

struct OneIntSeq: MySeq, MyIterator {
  let e : Float
  
  func next() -> Float? {
    return e
  }
  
  func getIndex(_ i: Int) -> Float {
    return e
  }
}

// test for conformance correctness using typealias in extension
extension MySeq {
  func first() -> Elem {
    return getIndex(0)
  }
}

// Specific diagnosis for trying to use complex typealiases in generic constraints
protocol P1 {
    associatedtype A
    typealias F = (A) -> ()
}

protocol P2 {
    associatedtype B
}

func go3<T : P1, U : P2 where T.F == U.B>(_ x: T) -> U { // expected-error {{typealias 'F' is too complex to be used as a generic constraint; use an associatedtype instead}} expected-error {{'F' is not a member type of 'T'}}
}

// Specific diagnosis for things that look like Swift 2.x typealiases
protocol P3 {
  typealias T // expected-error {{typealias is missing an assigned type; use 'associatedtype' to define an associated type requirement}}
  typealias U : P2 // expected-error {{typealias is missing an assigned type; use 'associatedtype' to define an associated type requirement}}
  
  associatedtype V : P2 = // expected-error {{expected type in associatedtype declaration}}
}

// Test for not crashing on self and recursive aliases
protocol P4 {
  typealias X = Self
  typealias Y = Self.Y // expected-error {{type alias 'Y' circularly references itself}}
  
  func getSelf() -> X
}


