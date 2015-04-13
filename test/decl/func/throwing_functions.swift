// RUN: %target-parse-verify-swift

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


// Overload resolution/////////////////////////////////////////////////////////
func barG<T>(t : T) throws -> T { return t } // expected-note{{}}
func fooG<T>(t : T) -> T { return t }

var bGE: (i: Int) -> Int = barG // expected-error{{invalid conversion from throwing function of type '(T) throws -> T' to non-throwing function type '(i: Int) -> Int'}}
var bg: (i: Int) throws -> Int = barG
var fG: (i: Int) throws -> Int = fooG

func fred(callback: (UInt8) throws -> ()) throws { }

func rachel() -> Int { return 12 }
func donna(generator: () throws -> Int) -> Int { return generator() }

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
