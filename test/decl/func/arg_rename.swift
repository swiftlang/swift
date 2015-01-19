// RUN: %target-parse-verify-swift

// Renaming of arguments.
func foo(a x: Int, b y: Int) { }
foo(a: 5, b: 7)

func bar<T>(a x: T, b y: T) { }
bar(a: 5, b: 7)

// Renaming of arguments in initializers.
struct S {
  init(a x: Int, b y: Int) { }
}

S(a: 5, b: 7)

struct GS {
  init<T>(a x: T, b y: T) { }
}
GS(a: 5, b: 7)

// Using the backtick to make a name API.
func f1(#a: Int, #b: Int) { }
f1(a: 1, b: 2)

func f2(#`class`: Int) { }
f2(`class`: 5)


// # diagnostics.
func g1(#a x: Int, #b y: Int) { } 
// expected-warning@-1{{extraneous '#' in parameter}}{{9-10=}}
// expected-warning@-2{{extraneous '#' in parameter}}{{20-21=}}

func g2(a a: Int) { }
// expected-warning@-1{{'a a' can be expressed more succinctly as '#a'}}{{9-9=#}}{{10-12=}}

func g3(#:Int) { }
// expected-error@-1{{expected parameter name after '#'}}

func g4(#_:Int) { }
// expected-error@-1{{expected non-empty parameter name after '#'}}{{9-10=}}

func g5(_ a: Int) { }
  // expected-warning@-1{{extraneous '_' in parameter: 'a' has no keyword argument name}}{{9-11=}}

class X {
  init(#a: Int) { } // expected-warning{{'#' in parameter: 'a' is already the keyword argument name}}{{8-9=}}
  func f1(#a: Int, b: Int) { }
  func f2(a: Int, #b: Int) { } // expected-warning{{extraneous '#' in parameter: 'b' is already the keyword argument name}}{{19-20=}}

  func f3(_ a: Int, b: Int) { }
  // expected-warning@-1{{extraneous '_' in parameter: 'a' has no keyword argument name}}{{11-13=}}
}

// Operators never have keyword arguments.
infix operator +++ { }
func +++(#lhs: Int, // expected-error{{operator cannot have keyword arguments}}{{10-11=}}
         rhs x: Int) -> Int { // expected-error{{operator cannot have keyword arguments}}{{10-14=}}
  return lhs + x 
}


