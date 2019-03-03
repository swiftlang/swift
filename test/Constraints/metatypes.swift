// RUN: %target-typecheck-verify-swift

class A {}
class B : A {}

// Deprecated .self
let test0_self : A.Type = A.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                                 // expected-note@-1 {{remove '.self' to silence this warning}}
let test1_self : A.Type = B.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                                 // expected-note@-1 {{remove '.self' to silence this warning}}
let test2_self : B.Type = A.self // expected-error {{cannot convert value of type 'A.Type' to specified type 'B.Type'}}
let test3_self : AnyClass = A.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                                   // expected-note@-1 {{remove '.self' to silence this warning}}
let test4_self : AnyClass = B.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                                   // expected-note@-1 {{remove '.self' to silence this warning}}

let test0 : A.Type = A
let test1 : A.Type = B
let test2 : B.Type = A // expected-error {{cannot convert value of type 'A.Type' to specified type 'B.Type'}}
let test3 : AnyClass = A
let test4 : AnyClass = B

struct S {}

let test5 : S.Type = S.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                            // expected-note@-1 {{remove '.self' to silence this warning}}
let test6 : AnyClass = S.self // expected-error {{cannot convert value of type 'S.Type' to specified type 'AnyClass' (aka 'AnyObject.Type')}}

func acceptMeta<T>(_ meta: T.Type) { }
acceptMeta(A) // ok

acceptMeta((A) -> Void) // ok

// Post removed .self on metatypes

// Variables
let simple = Int // ok

let arraySugar0 = [Int] // expected-error {{type of expression is ambiguous without more context}}
let arraySugar1: [Int].Type = [Int] // ok
let arraySugar2: [Int.Type] = [Int] // ok

let dict0 = [Int: String] // expected-error {{type of expression is ambiguous without more context}}
let dict1: [Int: String].Type = [Int: String] // ok
let dict2: [Int.Type: String.Type] = [Int: String] // expected-error {{type 'Int.Type' does not conform to protocol 'Hashable'}}

let tuple0 = (Int, Int) // expected-error {{type of expression is ambiguous without more context}}
let tuple1: (Int, Int).Type = (Int, Int) // ok
let tuple2: (Int.Type, Int.Type) = (Int, Int) // ok
let tuple3 = () // expected-warning {{constant 'tuple3' inferred to have type '()', which may be unexpected}}
                // expected-note@-1 {{add an explicit type annotation to silence this warning}}
let tuple4 = Void // ok

let generic0 = Array<Int> // ok
let generic1 = Array<Int.Type> // ok
let generic2 = Array // expected-error {{generic parameter 'Element' could not be inferred}}
                     // expected-note@-1 {{explicitly specify the generic arguments to fix this issue}}

