// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated -enable-objc-interop

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

func recover_missing_parameter_tuple_1 { // expected-cparser-error {{expected '(' in argument list of function declaration}} {{39-39=()}} expected-astgen-error {{expected parameter clause in function signature}}
}

func recover_missing_parameter_tuple_1a // expected-cparser-error {{expected '(' in argument list of function declaration}} {{40-40=()}} expected-astgen-error {{expected parameter clause in function signature}}
{
}

func recover_missing_parameter_tuple_2<T> { // expected-cparser-error {{expected '(' in argument list of function declaration}} {{42-42=()}} expected-astgen-error {{expected parameter clause in function signature}} expected-error {{generic parameter 'T' is not used in function signature}}
}

func recover_missing_parameter_tuple_3 -> Int { // expected-cparser-error {{expected '(' in argument list of function declaration}} {{39-39=()}} expected-astgen-error {{expected parameter clause in function signature}}
}

func recover_missing_parameter_tuple_4<T> -> Int { // expected-cparser-error {{expected '(' in argument list of function declaration}} {{42-42=()}} expected-astgen-error {{expected parameter clause in function signature}} expected-error {{generic parameter 'T' is not used in function signature}}
}

//===--- Check that we recover when the function return type is missing.

// Note: Don't move braces to a different line here.
func recover_missing_return_type_1() -> // expected-cparser-error {{expected type for function result}} expected-astgen-error {{expected return type in function signature}}
{
}

func recover_missing_return_type_2() -> // expected-cparser-error {{expected type for function result}} expected-cparser-error{{expected '{' in body of function declaration}} expected-astgen-error {{expected return type in function signature}}

// Note: Don't move braces to a different line here.
func recover_missing_return_type_3 -> // expected-cparser-error {{expected '(' in argument list of function declaration}} {{35-35=()}} expected-astgen-error {{expected return type in function signature}} expected-cparser-error {{expected type for function result}} expected-astgen-error {{expected parameter clause in function signature}}
{
}

//===--- Check that we recover if ':' was used instead of '->' to specify the return type.

func recover_colon_arrow_1() : Int { } // expected-cparser-error {{expected '->' after function parameter tuple}} {{30-31=->}} expected-astgen-error {{unexpected code ': Int' in function}}
func recover_colon_arrow_2() : { }     // expected-cparser-error {{expected '->' after function parameter tuple}} {{30-31=->}} expected-cparser-error {{expected type for function result}} expected-astgen-error {{unexpected code ':' in function}}
func recover_colon_arrow_3 : Int { }   // expected-cparser-error {{expected '->' after function parameter tuple}} {{28-29=->}} expected-astgen-error {{unexpected code ': Int' in function}} expected-cparser-error {{expected '(' in argument list of function declaration}} expected-astgen-error {{expected parameter clause in function signature}}
func recover_colon_arrow_4 : { }       // expected-cparser-error {{expected '->' after function parameter tuple}} {{28-29=->}} expected-cparser-error {{expected '(' in argument list of function declaration}} expected-astgen-error {{unexpected code ':' in function}} expected-cparser-error {{expected type for function result}} expected-astgen-error {{expected parameter clause in function signature}}
func recover_colon_arrow_5():Int { }   // expected-cparser-error {{expected '->' after function parameter tuple}} {{29-30= -> }} expected-astgen-error {{unexpected code ':Int' in function}}
func recover_colon_arrow_6(): Int { }  // expected-cparser-error {{expected '->' after function parameter tuple}} {{29-30= ->}} expected-astgen-error {{unexpected code ': Int' in function}}
func recover_colon_arrow_7() :Int { }  // expected-cparser-error {{expected '->' after function parameter tuple}} {{30-31=-> }} expected-astgen-error {{unexpected code ':Int' in function}}

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
func f_recover_missing_tuple_paren(_ a: Int // expected-note {{to match this opening '('}} expected-error{{expected '{' in body of function declaration}} expected-cparser-error {{expected ')' in parameter}} expected-astgen-error {{expected ')' to end parameter clause}}
func g_recover_missing_tuple_paren(_ b: Int) {
}

//===--- Parse errors.

func parseError1a(_ a: ) {} // expected-cparser-error {{expected parameter type following ':'}} {{23-23= <#type#>}} expected-astgen-error {{expected type in parameter}}

func parseError1b(_ a: // expected-cparser-error {{expected parameter type following ':'}} {{23-23= <#type#>}} expected-astgen-error {{expected type in parameter}}
                  ) {}

func parseError2(_ a: Int, b: ) {} // expected-cparser-error {{expected parameter type following ':'}} {{30-30= <#type#>}} expected-astgen-error {{expected type in parameter}}

func parseError3(_ a: unknown_type, b: ) {} // expected-error {{cannot find type 'unknown_type' in scope}}  expected-cparser-error {{expected parameter type following ':'}} {{39-39= <#type#>}} expected-astgen-error {{expected type in parameter}}

func parseError4(_ a: , b: ) {} // expected-cparser-error 2{{expected parameter type following ':'}} expected-astgen-error 2 {{expected type in parameter}}

func parseError5(_ a: b: ) {} // expected-error {{cannot find type 'b' in scope}}  expected-cparser-error {{expected ',' separator}} {{24-24=,}} expected-cparser-error {{expected parameter name followed by ':'}} expected-astgen-error {{unexpected code ':' in parameter clause}}

func parseError6(_ a: unknown_type, b: ) {} // expected-error {{cannot find type 'unknown_type' in scope}}  expected-cparser-error {{expected parameter type following ':'}} {{39-39= <#type#>}} expected-astgen-error {{expected type in parameter}}

func parseError7(_ a: Int, goo b: unknown_type) {} // expected-error {{cannot find type 'unknown_type' in scope}}

public func foo(_ a: Bool = true) -> (b: Bar, c: Bar) {} // expected-error 2{{cannot find type 'Bar' in scope}}

func parenPatternInArg((a): Int) -> Int { // expected-cparser-error {{expected parameter name followed by ':'}} expected-astgen-error {{expected identifier in parameter}} expected-astgen-error {{unexpected code '(a)' before parameter}}
  return a  // expected-error {{cannot find 'a' in scope}}
}
parenPatternInArg(0)  // expected-cparser-error {{argument passed to call that takes no arguments}} expected-astgen-error {{missing argument label ':' in call}}

var nullaryClosure: (Int) -> Int = {_ in 0}
_ = nullaryClosure(0)


// rdar://16737322 - This argument is an unnamed argument that has a labeled
// tuple type as the type.  Because the labels are in the type, they are not
// parameter labels, and they are thus not in scope in the body of the function.
// expected-cparser-error@+1{{unnamed parameters must be written}} {{27-27=_: }} expected-astgen-error@+1 {{expected identifier and ':' in parameter}}
func destructureArgument( (result: Int, error: Bool) ) -> Int {
  return result  // expected-error {{cannot find 'result' in scope}}
}

// The former is the same as this:
func destructureArgument2(_ a: (result: Int, error: Bool) ) -> Int {
  return result  // expected-error {{cannot find 'result' in scope}}
}


class ClassWithObjCMethod {
  @objc
  func someMethod(_ x : Int) {}
}

func testObjCMethodCurry(_ a : ClassWithObjCMethod) -> (Int) -> () {
  return a.someMethod
}

// We used to crash on this.
func rdar16786220(inout let c: Int) -> () { // expected-warning {{'let' in this position is interpreted as an argument label}} {{25-28=`let`}}
// expected-cparser-error @-1 {{'inout' before a parameter name is not allowed, place it before the parameter type instead}}{{19-24=}}{{32-32=inout }} expected-astgen-error@-1 {{'inout' before a parameter name is not allowed}}

  c = 42
}

func multipleSpecifiers(a: inout __owned Int) {} // expected-cparser-error {{parameter may have at most one of the 'inout', 'borrowing', or 'consuming' specifiers}} {{28-34=}} expected-astgen-error {{parameter may have at most one of the 'inout', 'borrowing', or 'consuming' specifiers}}{{28-33=}}

// <rdar://problem/17763388> ambiguous operator emits same candidate multiple times
infix operator !!!

func !!!<T>(lhs: Array<T>, rhs: Array<T>) -> Bool { return false }
func !!!<T>(lhs: UnsafePointer<T>, rhs: UnsafePointer<T>) -> Bool { return false }
_ = [1] !!! [1]   // unambiguously picking the array overload.


// <rdar://problem/16786168> Functions currently permit 'var inout' parameters
func inout_error(inout var x : Int) {} // expected-warning {{'var' in this position is interpreted as an argument label}} {{24-27=`var`}}
// expected-cparser-error @-1 {{'inout' before a parameter name is not allowed, place it before the parameter type instead}} {{18-23=}}{{32-32=inout }} expected-astgen-error@-1 {{'inout' before a parameter name is not allowed}}

// Unnamed parameters require the name "_":
func unnamed(Int) { } // expected-cparser-error{{unnamed parameters must be written with the empty name '_'}}{{14-14=_: }} expected-astgen-error {{expected identifier and ':' in parameter}}

func typeAttrBeforeParamDecl(@convention(c) _: () -> Void) {} // expected-cparser-error{{attribute can only be applied to types, not declarations}} expected-astgen-error {{unknown attribute 'convention'}}

// FIXME: Bad diagnostics
func bareTypeWithAttr(@convention(c) () -> Void) {} // expected-cparser-error{{attribute can only be applied to types, not declarations}} expected-astgen-error {{unknown attribute 'convention'}}
// expected-cparser-error @-1 {{unnamed parameters must be written with the empty name '_'}} {{38-38=_: }} expected-astgen-error@-1 {{}}

// Test fixits on curried functions.
func testCurryFixits() {
  func f1(_ x: Int)(y: Int) {} // expected-cparser-error{{cannot have more than one parameter list}} expected-astgen-error {{unexpected code '(y: Int)' in function}}
  func f1a(_ x: Int, y: Int) {}
  func f2(_ x: Int)(y: Int)(z: Int) {} // expected-cparser-error{{cannot have more than one parameter list}} expected-astgen-error {{unexpected code '(y: Int)(z: Int)' in function}}
  func f2a(_ x: Int, y: Int, z: Int) {}
  func f3(_ x: Int)() {} // expected-cparser-error{{cannot have more than one parameter list}} expected-astgen-error {{unexpected code '()' in function}}
  func f3a(_ x: Int) {}
  func f4()(x: Int) {} // expected-cparser-error{{cannot have more than one parameter list}} expected-astgen-error {{unexpected code '(x: Int)' in function}}
  func f4a(_ x: Int) {}
  func f5(_ x: Int)()(y: Int) {} // expected-cparser-error{{cannot have more than one parameter list}} expected-astgen-error {{unexpected code '()(y: Int)' in function}}
  func f5a(_ x: Int, y: Int) {}
}

// Bogus diagnostic talking about a 'var' where there is none
func invalidInOutParam(x: inout XYZ) {}
// expected-error@-1{{cannot find type 'XYZ' in scope}}

// Parens around the 'inout'
func parentheticalInout(_ x: ((inout Int))) {}

var value = 0
parentheticalInout(&value)

func parentheticalInout2(_ fn: (((inout Int)), Int) -> ()) {
  var value = 0
  fn(&value, 0)
}

// https://github.com/apple/swift/issues/54133
// FIXME: None of these diagnostics is particularly good.
func bogusDestructuring() {
  struct Bar {}

  struct Foo {
    func registerCallback(_ callback: @escaping ([Bar]) -> Void) {} // expected-astgen-note {{found this candidate }}
    func registerCallback(_ callback: @escaping ([String: Bar]) -> Void) {} // expected-astgen-note {{found this candidate }}
    func registerCallback(_ callback: @escaping (Bar?) -> Void) {} // expected-astgen-note {{found this candidate }}
  }

  Foo().registerCallback { ([Bar]) in } // expected-cparser-warning {{unnamed parameters must be written with the empty name '_'; this is an error in the Swift 6 language mode}} {{29-29=_: }} expected-astgen-error {{expected identifier and ':' in parameter}}
  Foo().registerCallback { ([String: Bar]) in }// expected-cparser-warning {{unnamed parameters must be written with the empty name '_'; this is an error in the Swift 6 language mode}} {{29-29=_: }} expected-astgen-error {{expected identifier and ':' in parameter}}
  Foo().registerCallback { (Bar?) in } // expected-cparser-error {{unnamed parameters must be written with the empty name '_'}} expected-astgen-error {{unexpected code '?' in parameter clause}} expected-astgen-error {{ambiguous use of 'registerCallback'}}

}
