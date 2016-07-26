// RUN: %target-parse-verify-swift

infix operator ====
infix operator <<<<
infix operator <><>

// <rdar://problem/13782566>
// Check that func op<T>() parses without a space between the name and the
// generic parameter list.
func ====<T>(x: T, y: T) {}
func <<<<<T>(x: T, y: T) {}
func <><><T>(x: T, y: T) {}

//===--- Check that we recover when the parameter tuple is missing.

func recover_missing_parameter_tuple_1 { // expected-error {{expected '(' in argument list of function declaration}}
}

func recover_missing_parameter_tuple_1a // expected-error {{expected '(' in argument list of function declaration}}
{
}

func recover_missing_parameter_tuple_2<T> { // expected-error {{expected '(' in argument list of function declaration}} expected-error {{generic parameter 'T' is not used in function signature}}
}

func recover_missing_parameter_tuple_3 -> Int { // expected-error {{expected '(' in argument list of function declaration}}
}

func recover_missing_parameter_tuple_4<T> -> Int { // expected-error {{expected '(' in argument list of function declaration}} expected-error {{generic parameter 'T' is not used in function signature}}
}

//===--- Check that we recover when the function return type is missing.

// Note: Don't move braces to a different line here.
func recover_missing_return_type_1() -> // expected-error {{expected type for function result}}
{
}

func recover_missing_return_type_2() -> // expected-error {{expected type for function result}} expected-error{{expected '{' in body of function declaration}}

// Note: Don't move braces to a different line here.
func recover_missing_return_type_3 -> // expected-error {{expected '(' in argument list of function declaration}} expected-error {{expected type for function result}}
{
}

//===--- Check that we recover if ':' was used instead of '->' to specify the return type.

func recover_colon_arrow_1() : Int { } // expected-error {{expected '->' after function parameter tuple}} {{30-31=->}}
func recover_colon_arrow_2() : { }     // expected-error {{expected '->' after function parameter tuple}} {{30-31=->}} expected-error {{expected type for function result}}
func recover_colon_arrow_3 : Int { }   // expected-error {{expected '->' after function parameter tuple}} {{28-29=->}} expected-error {{expected '(' in argument list of function declaration}}
func recover_colon_arrow_4 : { }       // expected-error {{expected '->' after function parameter tuple}} {{28-29=->}} expected-error {{expected '(' in argument list of function declaration}} expected-error {{expected type for function result}}

//===--- Check that we recover if the function does not have a body, but the
//===--- context requires the function to have a body.

func recover_missing_body_1() // expected-error {{expected '{' in body of function declaration}}
func recover_missing_body_2() // expected-error {{expected '{' in body of function declaration}}
    -> Int 

// Ensure that we don't skip over the 'func g' over to the right paren in
// function g, while recovering from parse error in f() parameter tuple.  We
// should produce the error about missing right paren.
//
// FIXME: The errors are awful.  We should produce just the error about paren.
func f_recover_missing_tuple_paren(_ a: Int // expected-note {{to match this opening '('}} expected-error{{expected '{' in body of function declaration}} expected-error {{expected ')' in parameter}} 
func g_recover_missing_tuple_paren(_ b: Int) {
}

//===--- Parse errors.

func parseError1a(_ a: ) {} // expected-error {{expected parameter type following ':'}}

func parseError1b(_ a: // expected-error {{expected parameter type following ':'}}
                  ) {}

func parseError2(_ a: Int, b: ) {} // expected-error {{expected parameter type following ':'}}

func parseError3(_ a: unknown_type, b: ) {} // expected-error {{use of undeclared type 'unknown_type'}}  expected-error {{expected parameter type following ':'}}

func parseError4(_ a: , b: ) {} // expected-error 2{{expected parameter type following ':'}}

func parseError5(_ a: b: ) {} // expected-error {{use of undeclared type 'b'}}  expected-error 2 {{expected ',' separator}} {{24-24=,}} {{24-24=,}} expected-error {{expected parameter name followed by ':'}}

func parseError6(_ a: unknown_type, b: ) {} // expected-error {{use of undeclared type 'unknown_type'}}  expected-error {{expected parameter type following ':'}}

func parseError7(_ a: Int, goo b: unknown_type) {} // expected-error {{use of undeclared type 'unknown_type'}}

public func foo(_ a: Bool = true) -> (b: Bar, c: Bar) {} // expected-error {{use of undeclared type 'Bar'}}

func parenPatternInArg((a): Int) -> Int { // expected-error {{expected parameter name followed by ':'}} expected-error {{expected ',' separator}}
  return a  // expected-error {{use of unresolved identifier 'a'}}
}
parenPatternInArg(0)  // expected-error {{argument passed to call that takes no arguments}}

var nullaryClosure: (Int) -> Int = {_ in 0}
_ = nullaryClosure(0)


// rdar://16737322 - This argument is an unnamed argument that has a labeled
// tuple type as the type.  Because the labels are in the type, they are not
// parameter labels, and they are thus not in scope in the body of the function.
// expected-error@+1{{unnamed parameters must be written}} {{27-27=_: }}
func destructureArgument( (result: Int, error: Bool) ) -> Int {
  return result  // expected-error {{use of unresolved identifier 'result'}}
}

// The former is the same as this:
func destructureArgument2(_ a: (result: Int, error: Bool) ) -> Int {
  return result  // expected-error {{use of unresolved identifier 'result'}}
}


class ClassWithObjCMethod {
  @objc
  func someMethod(_ x : Int) {}
}

func testObjCMethodCurry(_ a : ClassWithObjCMethod) -> (Int) -> () {
  return a.someMethod
}

// We used to crash on this.
func rdar16786220(inout let c: Int) -> () { // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{25-29=}}
// expected-error @-1 {{'inout' before a parameter name is not allowed, place it before the parameter type instead}}{{19-24=}}{{32-32=inout }}

  c = 42
}


// <rdar://problem/17763388> ambiguous operator emits same candidate multiple times
infix operator !!!

func !!!<T>(lhs: Array<T>, rhs: Array<T>) -> Bool { return false }
func !!!<T>(lhs: UnsafePointer<T>, rhs: UnsafePointer<T>) -> Bool { return false }
_ = [1] !!! [1]   // unambiguously picking the array overload.


// <rdar://problem/16786168> Functions currently permit 'var inout' parameters
func var_inout_error(inout var x : Int) {} // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}} {{28-32=}}
// expected-error @-1 {{'inout' before a parameter name is not allowed, place it before the parameter type instead}} {{22-27=}}{{36-36=inout }}

// Unnamed parameters require the name "_":
func unnamed(Int) { } // expected-error{{unnamed parameters must be written with the empty name '_'}}{{14-14=_: }}

// Test fixits on curried functions.
func testCurryFixits() {
  func f1(_ x: Int)(y: Int) {} // expected-error{{curried function declaration syntax has been removed; use a single parameter list}} {{19-21=, }}
  func f1a(_ x: Int, y: Int) {}
  func f2(_ x: Int)(y: Int)(z: Int) {} // expected-error{{curried function declaration syntax has been removed; use a single parameter list}} {{19-21=, }} {{27-29=, }}
  func f2a(_ x: Int, y: Int, z: Int) {}
  func f3(_ x: Int)() {} // expected-error{{curried function declaration syntax has been removed; use a single parameter list}} {{19-21=}}
  func f3a(_ x: Int) {}
  func f4()(x: Int) {} // expected-error{{curried function declaration syntax has been removed; use a single parameter list}} {{11-13=}}
  func f4a(_ x: Int) {}
  func f5(_ x: Int)()(y: Int) {} // expected-error{{curried function declaration syntax has been removed; use a single parameter list}} {{19-21=}} {{21-23=, }}
  func f5a(_ x: Int, y: Int) {}
}
