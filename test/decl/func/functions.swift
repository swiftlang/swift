// RUN: %swift %s -verify

// FIXME: Ban prefix & when checking operator decl.
operator prefix & {}

@prefix def & (x : Int) {} // expected-error {{cannot declare a custom unary '&' operator}}

def fn_redef() {} // expected-note {{fn_redef' previously declared here}}
def fn_redef() {} // expected-error {{invalid redeclaration}}

operator infix ==== {}
operator infix <<<< {}
operator infix <><> {}

// <rdar://problem/13782566>
// Check that def op<T>() parses without a space between the name and the
// generic parameter list.
def ====<T>(x: T, y: T) {}
def <<<<<T>(x: T, y: T) {}
def <><><T>(x: T, y: T) {}

//===--- Check that we recover when the parameter tuple is missing.

def recover_missing_parameter_tuple_1 { // expected-error {{expected '(' in argument list of function declaration}}
}

def recover_missing_parameter_tuple_1a // expected-error {{expected '(' in argument list of function declaration}}
{
}

def recover_missing_parameter_tuple_2<T> { // expected-error {{expected '(' in argument list of function declaration}}
}

def recover_missing_parameter_tuple_3 -> Int { // expected-error {{expected '(' in argument list of function declaration}}
}

def recover_missing_parameter_tuple_4<T> -> Int { // expected-error {{expected '(' in argument list of function declaration}}
}

//===--- Check that we recover when the function return type is missing.

// Note: Don't move braces to a different line here.
def recover_missing_return_type_1() -> // expected-error {{expected type for function result}}
{
}

def recover_missing_return_type_2() -> // expected-error {{expected type for function result}}

// Note: Don't move braces to a different line here.
def recover_missing_return_type_3 -> // expected-error {{expected '(' in argument list of function declaration}} expected-error {{expected type for function result}}
{
}

//===--- Check that we recover if ':' was used instead of '->' to specify the return type.

def recover_colon_arrow_1() : Int { } // expected-error {{expected '->' after function parameter tuple}} {{29-30=->}}
def recover_colon_arrow_2() : { }     // expected-error {{expected '->' after function parameter tuple}} {{29-30=->}} expected-error {{expected type for function result}}
def recover_colon_arrow_3 : Int { }   // expected-error {{expected '->' after function parameter tuple}} {{27-28=->}} expected-error {{expected '(' in argument list of function declaration}}
def recover_colon_arrow_4 : { }       // expected-error {{expected '->' after function parameter tuple}} {{27-28=->}} expected-error {{expected '(' in argument list of function declaration}} expected-error {{expected type for function result}}

//===--- Check that we recover if the function does not have a body, but the
//===--- context requires the function to have a body.

def recover_missing_body_1() // expected-error {{expected '{' in body of function declaration}}
def recover_missing_body_2()
    -> Int // expected-error {{expected '{' in body of function declaration}}

// Ensure that we don't skip over the 'func g' over to the right paren in
// function g, while recovering from parse error in f() parameter tuple.  We
// should produce the error about missing right paren.
//
// FIXME: The errors are awful.  We should produce just the error about paren.
def f_recover_missing_tuple_paren(a: Int // expected-note {{to match this opening '('}}
def g_recover_missing_tuple_paren(b: Int) { // expected-error 2{{expected ',' separator}} expected-error {{expected pattern}} expected-error {{expected ')' at end of tuple pattern}}
}

//===--- Functions with selector-style arguments.

def murderInRoom(room: Int) {}
def murderInRoom(room: Int) withWeapon(weapon: Int) {} // expected-note {{'murderInRoom' previously declared here}}
def murderInRoom(room: Int) withWeapon(weapon: Int)
    framingSuspect(suspect: Int) {}

def murderInRoom(room: Int) withWeapon(w: Int) {} // expected-error {{invalid redeclaration}}

def blah(a: Int) blah(b: Int) {}
def blah(a: Int) blah(b: Int) blah(c: Int) {} // expected-error {{definition conflicts with previous value}} expected-note {{previous definition of 'blah' is here}}
def blah(a: Int) blah(b: Int) bluh(b: Int) {} // expected-error {{definition conflicts with previous value}} expected-note {{previous definition of 'b' is here}}
def blah(a: Int) blah(b: Int) bloh(c: Int) {}

def blah(a: Int) _(b: Int) _(c: Int) {}

def zero() sel(a: Int) {} // expected-error {{selector-style function arguments may only be used with one-argument patterns}}
def two(a: Int, b: Int) sel(c:Int) {} // expected-error {{selector-style function arguments may only be used with one-argument patterns}}
def second_zero(a: Int) zero() {} // expected-error {{selector-style function arguments may only be used with one-argument patterns}}
def second_tuple(a: Int) two(b: Int, c: Int) {} // expected-error {{selector-style function arguments may only be used with one-argument patterns}}

def curry(a: Int) andSel(b: Int)(c: Int) {} // expected-error {{funcs with selector-style arguments may not be curried}}

def missing_selector_pattern(a: Int) b // expected-error {{expected '(' after identifier in argument list of selector-style function declaration}}
{
}

//===--- Parse errors.

def parseError1a(a: ) {} // expected-error {{expected type}}

def parseError1b(a: // expected-error {{expected type}}
                  ) {}

def parseError2(a: Int, b: ) {} // expected-error {{expected type}}

def parseError3(a: unknown_type, b: ) {} // expected-error {{expected type}} expected-error {{use of undeclared type 'unknown_type'}}

def parseError4(a: , b: ) {} // expected-error 2{{expected type}}

def parseError5(a: b: ) {} // expected-error {{expected type}} expected-error {{use of undeclared type 'b'}} expected-error {{expected ',' separator}} expected-error {{expected pattern}}

def parseError6(a: unknown_type)(b: ) {} // expected-error {{expected type}} expected-error {{use of undeclared type 'unknown_type'}}

def parenPatternInArg((a): Int) -> Int {
  return a
}
parenPatternInArg(0)

var nullaryClosure: Int -> Int = {_ in 0}
nullaryClosure(0)
