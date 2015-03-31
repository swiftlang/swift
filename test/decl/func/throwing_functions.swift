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

var bGE: (i: Int) -> Int = barG // expected-error{{argument for generic parameter 'T' could not be inferred}} expected-error{{'(T) throws -> T' is not convertible to '(i: Int) -> Int'}}
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