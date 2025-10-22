// Constant globals using @section
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -verify

// integer literals
@section("mysection") let intLiteral1 = 42 // ok
@section("mysection") let intLiteral2: Int8 = 127 // ok
@section("mysection") let intLiteral3: Int16 = 32767 // ok
@section("mysection") let intLiteral4: Int32 = 2147483647 // ok
@section("mysection") let intLiteral5: Int64 = 9223372036854775807 // ok
@section("mysection") let intLiteral6: UInt = 42 // ok
@section("mysection") let intLiteral7: UInt8 = 255 // ok
@section("mysection") let intLiteral8: UInt16 = 65535 // ok
@section("mysection") let intLiteral9: UInt32 = 4294967295 // ok
@section("mysection") let intLiteral10: UInt64 = 18446744073709551615 // ok

// floating-point literals
@section("mysection") let floatLiteral1: Float = 3.14 // ok
@section("mysection") let floatLiteral2: Double = 2.718 // ok

// boolean literals
@section("mysection") let boolLiteral1 = true // ok
@section("mysection") let boolLiteral2 = false // ok

// operators (should be rejected)
@section("mysection") let invalidOperator1 = 1 + 1
// expected-error@-1{{unsupported operator in a '@const' expression}}
@section("mysection") let invalidOperator2 = 3.14 * 2.0
// expected-error@-1{{unsupported operator in a '@const' expression}}
@section("mysection") let invalidOperator3: Int = -(1)
// expected-error@-1{{unsupported operator in a '@const' expression}}

// non-literal expressions (should be rejected)
@section("mysection") let invalidNonLiteral1 = Int.max
// expected-error@-1{{unable to resolve variable reference in a '@const' expression}}
@section("mysection") let invalidNonLiteral2 = UInt8(42)
// expected-error@-1{{not supported in a '@const' expression}}

func foo() -> Int { return 42 }
func bar(x: Int) -> String { return "test" }

// function references
@section("mysection") let funcRef1 = foo // ok
@section("mysection") let funcRef2 = bar // ok

// invalid function references (should be rejected)
@section("mysection") let invalidFuncRef1 = foo()
// expected-error@-1{{not supported in a '@const' expression}}
@section("mysection") let invalidFuncRef2 = Bool.self.random
// expected-error@-1{{closures not supported in a '@const' expression}}
@section("mysection") let invalidFuncRef3 = (Bool.self as Bool.Type).random
// expected-error@-1{{not supported in a '@const' expression}}

// generic function references (should be rejected)
@section("mysection") let invalidGenericFunc = [Int].randomElement
// expected-error@-1{{not supported in a '@const' expression}}

// closures (should be rejected)
@section("mysection") let invalidClosure1 = { }
// expected-error@-1{{closures not supported in a '@const' expression}}
@section("mysection") let invalidClosure2 = { return 42 }
// expected-error@-1{{closures not supported in a '@const' expression}}

struct S { }
enum E { case a }

// metatypes - TODO
@section("mysection") let metatype1 = Int.self
// expected-error@-1{{type expressions not supported in a '@const' expression}}

// invalid metatype references
@section("mysection") let invalidMetatype1 = Int.self.self
// expected-error@-1{{type expressions not supported in a '@const' expression}}

// tuples
@section("mysection") let tuple1 = (1, 2, 3, 2.718, true) // ok
@section("mysection") let tuple2: (Int, Float, Bool) = (42, 3.14, false) // ok
@section("mysection") let tuple3 = (foo, bar) // ok (function references in tuple)

// invalid tuples (should be rejected)
@section("mysection") let invalidTuple1 = (1, 2, Int.max)
// expected-error@-1{{unable to resolve variable reference in a '@const' expression}}
@section("mysection") let invalidTuple2 = (1 + 1, 2)
// expected-error@-1{{unsupported operator in a '@const' expression}}

let someVar = 42

// variables (should be rejected)
@section("mysection") let invalidVarRef = someVar
// expected-error@-1{{unable to resolve variable reference in a '@const' expression}}

struct MyCustomExpressibleByIntegerLiteral: ExpressibleByIntegerLiteral {
    init(integerLiteral value: Int) {}
}

// custom types (should be rejected)
@section("mysection") let invalidCustomType1: MyCustomExpressibleByIntegerLiteral = 42
// expected-error@-1{{unsupported type in a '@const' expression}}
@section("mysection") let invalidCustomType2: E = E.a
// expected-error@-1{{not supported in a '@const' expression}}
@section("mysection") let invalidCustomType3: S = S()
// expected-error@-1{{not supported in a '@const' expression}}

// other standard types (should be rejected)
@section("mysection") let invalidString = "hello"
// expected-error@-1{{unsupported type in a '@const' expression}}
@section("mysection") let invalidArray = [1, 2, 3]
// expected-error@-1{{unsupported type in a '@const' expression}}
@section("mysection") let invalidDict = ["key": "value"]
// expected-error@-1{{not supported in a '@const' expression}}

// inline array
@available(SwiftStdlib 6.2, *)
@section("mysection") let inlineArray: InlineArray = [1, 2, 3] // ok

// invalid inline array (should be rejected)
@available(SwiftStdlib 6.2, *)
@section("mysection") let invalidInlineArray: InlineArray = [1, 2, 3, Int.max]
// expected-error@-1{{unable to resolve variable reference in a '@const' expression}}

// optionals (should be rejected)
@section("mysection") let optional1: Int? = 42
// expected-error@-1{{unsupported type in a '@const' expression}}
@section("mysection") let optional2: Int? = nil
// expected-error@-1{{unsupported type in a '@const' expression}}
@section("mysection") let optional3: Double? = 3.14
// expected-error@-1{{unsupported type in a '@const' expression}}
@section("mysection") let optional4: Bool? = true
// expected-error@-1{{unsupported type in a '@const' expression}}
@section("mysection") let optional5: Bool? = false
// expected-error@-1{{unsupported type in a '@const' expression}}
@section("mysection") let optional6: Int? = 1 + 1
// expected-error@-1{{unsupported type in a '@const' expression}}
@section("mysection") let optional7: Int? = Int.max
// expected-error@-1{{unsupported type in a '@const' expression}}
