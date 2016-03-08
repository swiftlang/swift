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






// FIXME: BAD LOC: expected-note @+2 {{'T1' declared as parameter to type 'MyType'}}
// FIXME: BAD LOC: expected-note @+1 {{'T3' declared as parameter to type 'MyType'}}
struct MyType<TyA, TyB> {
  var a : TyA, b : TyB
}

protocol P {
  associatedtype X<T>  // expected-error {{associated types may not have a generic parameter list}}
  
  // expected-error @+1 {{associated types may not have a generic parameter list}}
  typealias Y<T>       // expected-warning {{use of 'typealias' to declare associated types is deprecated; use 'associatedtype' instead}}
}

typealias basicTypealias = Int

typealias DSI = MyType<String, Int>
typealias DS<T> = MyType<String, T>

typealias BadA<T : Int> = MyType<String, T>  // expected-error {{type parameters may not be constrained in typealias argument list}}

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
typealias B<T1> = MyType<T1, T1>
typealias C<T> = MyType<String, T>

// Type aliases with unused generic params.
typealias D<T1, T2, T3> = MyType<T2, T1>
typealias E<T1, T2> = Int  // expected-note {{generic type 'E' declared here}}

typealias F<T1, T2> = T1->T2

// Type alias of type alias.
typealias G<S1, S2> = A<S1, S2>

let _ : E<Int, Float> = 42
let _ : E<Float> = 42   // expected-error {{generic type 'E' specialized with too few type parameters (got 1, but expected 2)}}
let _ : E = 42   // expected-error {{cannot convert value of type 'Int' to specified type 'E'}}
let _ : D = D(a: 1, b: 2)  // expected-error {{cannot convert value of type 'MyType<Int, Int>' to specified type 'D'}}

let _ : D<Int, Int, Float> = D<Int, Int, Float>(a: 1, b: 2)

// BOGUS FIXME: expected-error @+1 {{cannot convert value of type 'MyType<Int, Int>' to specified type 'D'}}
let _ : D = D<Int, Int, Float>(a: 1, b: 2)


// FIXME: This produces bad loc info on T3 archetype notes.
// BOGUS FIXME: expected-error @+1 {{generic parameter 'T3' could not be inferred}}
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



extension A {}  // expected-error {{non-nominal type 'A' cannot be extended}}
extension A<T> {}  // expected-error {{generic type 'A' specialized with too few type parameters (got 1, but expected 2)}}
extension A<Float,Int> {}  // expected-error {{constrained extension must be declared on the unspecialized generic type 'MyType' with constraints specified by a 'where' clause}}
extension C<T> {}  // expected-error {{use of undeclared type 'T'}}
extension C<Int> {}  // expected-error {{constrained extension must be declared on the unspecialized generic type 'MyType' with constraints specified by a 'where' clause}}

