// RUN: %target-parse-verify-swift

enum Exception : ErrorType { case A }

// Basic syntax ///////////////////////////////////////////////////////////////
func bar() throws -> Int { return 0 }
func foo() -> Int { return 0 }


// Currying ///////////////////////////////////////////////////////////////////
func curry1() {

}

func curry1Throws() throws {

}

func curry2() -> () -> () {
	return curry1
}

func curry2Throws() throws -> () -> () {
	return curry1
}

func curry3() -> () throws -> () {
	return curry1Throws
}

func curry3Throws() throws -> () throws -> () {
	return curry1Throws
}

var a : () -> () -> () = curry2
var b : () throws -> () -> () = curry2Throws
var c : () -> () throws -> () = curry3
var d : () throws -> () throws -> () = curry3Throws

// Partial application ////////////////////////////////////////////////////////

func partialApply1(a: Int)(b: Int)(c: Int) throws { }

func partialApply2<T>(a: T)(b: Int)(c: Int) throws { }

protocol Parallelogram {
  static func partialApply3(a: Int) throws
}

let f11 = partialApply1
let f12 = partialApply1(1)
let f13 = partialApply1(1)(b: 2)

let f22 = partialApply2(1)
let f23 = partialApply2(1)(b: 2)

func partialApply4<T: Parallelogram>(t: T) {
  let f = T.partialApply3
}

// Overload resolution/////////////////////////////////////////////////////////
func barG<T>(t : T) throws -> T { return t } // expected-note{{}}
func fooG<T>(t : T) -> T { return t }

var bGE: (i: Int) -> Int = barG // expected-error{{invalid conversion from throwing function of type '(T) throws -> T' to non-throwing function type '(i: Int) -> Int'}}
var bg: (i: Int) throws -> Int = barG
var fG: (i: Int) throws -> Int = fooG

func fred(callback: (UInt8) throws -> ()) throws { }

func rachel() -> Int { return 12 }
func donna(generator: () throws -> Int) -> Int { return generator() } // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}}

donna(rachel)

func barT() throws -> Int { return 0 } // expected-note{{}}
func barT() -> Int { return 0 } // expected-error{{invalid redeclaration of 'barT()'}}

func fooT(callback: () throws -> Bool) {} //OK
func fooT(callback: () -> Bool) {}

func jerry(i: Int)(j: Int) throws -> Int { return 0 } // (Int) -> (Int) throws -> Int

// Throwing and non-throwing types are not equivalent.
struct X<T> { }
func specializedOnFuncType1(x: X<String throws -> Int>) { }
func specializedOnFuncType2(x: X<String -> Int>) { }
func testSpecializedOnFuncType(xThrows: X<String throws -> Int>,
                               xNonThrows: X<String -> Int>) {
  specializedOnFuncType1(xThrows) // ok
  specializedOnFuncType1(xNonThrows) // expected-error{{invalid conversion from non-throwing function of type 'String -> Int' to throwing function type 'String throws -> Int'}}
  specializedOnFuncType2(xThrows)  // expected-error{{invalid conversion from throwing function of type 'String throws -> Int' to non-throwing function type 'String -> Int'}}
  specializedOnFuncType2(xNonThrows) // ok
}

// Subtyping
func subtypeResult1(x: String -> (Int -> String)) { }
func testSubtypeResult1(x1: String -> (Int throws -> String),
                        x2: String -> (Int -> String)) {
  subtypeResult1(x1) // expected-error{{invalid conversion from throwing function of type 'Int throws -> String' to non-throwing function type 'Int -> String'}}
  subtypeResult1(x2)
}

func subtypeResult2(x: String -> (Int throws -> String)) { }
func testSubtypeResult2(x1: String -> (Int throws -> String),
                        x2: String -> (Int -> String)) {
  subtypeResult2(x1)
  subtypeResult2(x2)
}

func subtypeArgument1(x: (fn: (String -> Int)) -> Int) { }
func testSubtypeArgument1(x1: (fn: (String -> Int)) -> Int,
                          x2: (fn: (String throws -> Int)) -> Int) {
  subtypeArgument1(x1)
  subtypeArgument1(x2)
}

func subtypeArgument2(x: (fn: (String throws -> Int)) -> Int) { }
func testSubtypeArgument2(x1: (fn: (String -> Int)) -> Int,
                          x2: (fn: (String throws -> Int)) -> Int) {
  subtypeArgument2(x1) // expected-error{{cannot invoke 'subtypeArgument2' with an argument list of type '((fn: (String -> Int)) -> Int)'}}
  // expected-note@-1{{expected an argument list of type '((fn: (String throws -> Int)) -> Int)'}}
  subtypeArgument2(x2)
}

// Closures
var c1 = {() throws -> Int in 0}
var c2 : () throws -> Int = c1 // ok
var c3 : () -> Int = c1 // expected-error{{invalid conversion from throwing function of type '() throws -> Int' to non-throwing function type '() -> Int'}}
var c4 : () -> Int = {() throws -> Int in 0} // expected-error{{invalid conversion from throwing function of type '() throws -> Int' to non-throwing function type '() -> Int'}}
var c5 : () -> Int = { try c2() } // expected-error{{invalid conversion from throwing function of type '() throws -> Int' to non-throwing function type '() -> Int'}}
var c6 : () throws -> Int = { do { try c2() } ; return 0 }
var c7 : () -> Int = { do { try c2() } ; return 0 } // expected-error{{invalid conversion from throwing function of type '() throws -> Int' to non-throwing function type '() -> Int'}}
var c8 : () -> Int = { do { try c2()  } catch _ { var x = 0 } ; return 0 }
var c9 : () -> Int = { do { try c2()  } catch Exception.A { var x = 0 } ; return 0 }// expected-error{{invalid conversion from throwing function of type '() throws -> Int' to non-throwing function type '() -> Int'}}
var c10 : () -> Int = { throw Exception.A; return 0 } // expected-error{{invalid conversion from throwing function of type '() throws -> Int' to non-throwing function type '() -> Int'}}

// Initializers
struct A {
    init(doomed: ()) throws {}
}

func fi1() throws {
    A(doomed: ()) // expected-error {{call can throw but is not marked with 'try'}}
}

struct B {
 init() throws {}
 init(foo: Int) {}
}

B(foo: 0)
