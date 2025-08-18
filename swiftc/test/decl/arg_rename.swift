// RUN: %target-typecheck-verify-swift

// Renaming of arguments.
func foo(a x: Int, b y: Int) { }
foo(a: 5, b: 7)

func bar<T>(a x: T, b y: T) { }
bar(a: 5, b: 7)

// Renaming of arguments in initializers.
struct S {
  init(a x: Int, b y: Int) { }
}

S(a: 5, b: 7) // expected-warning{{unused}}

struct GS {
  init<T>(a x: T, b y: T) { }
}
GS(a: 5, b: 7) // expected-warning{{unused}}

func f1(a: Int, b: Int) { }
f1(a: 1, b: 2)

func f2(class cls: Int) { }
f2(class: 5)

func f3(var a: Int) {} // expected-warning {{'var' in this position is interpreted as an argument label}} {{9-12=`var`}}
f3(var: 5)

func f4(let a: Int) {} // expected-warning {{'let' in this position is interpreted as an argument label}} {{9-12=`let`}}
f4(let: 5)

func f5(a var: Int) {}
f5(a: 5)

func f6(a let: Int) {}
f6(a: 5)

func f7(var let: Int) { // expected-warning {{'var' in this position is interpreted as an argument label}} {{9-12=`var`}}
  let _ = `let`
} 
f7(var: 5)

func f8(let var: Int) { // expected-warning {{'let' in this position is interpreted as an argument label}} {{9-12=`let`}}
  let _ = `var`
}
f8(let: 5)


func g1(a a: Int) { } // expected-warning{{extraneous duplicate parameter name; 'a' already has an argument label}}{{9-11=}}

func g2(_ a: Int) { }

func g3(var var: Int) {} // expected-warning {{'var' in this position is interpreted as an argument label}} {{9-12=`var`}}
// expected-warning @-1 {{extraneous duplicate parameter name; 'var' already has an argument label}}{{9-13=}}

func g4(let let: Int) {} // expected-warning {{'let' in this position is interpreted as an argument label}} {{9-12=`let`}}
// expected-warning @-1 {{extraneous duplicate parameter name; 'let' already has an argument label}}{{9-13=}}

class X {
  init(a a: Int) { } // expected-warning{{extraneous duplicate parameter name; 'a' already has an argument label}}{{8-10=}}
  func f1(a a: Int, b: Int) { } // expected-warning{{extraneous duplicate parameter name; 'a' already has an argument label}}{{11-13=}}
  func f2(a: Int, b b: Int) { } // expected-warning{{extraneous duplicate parameter name; 'b' already has an argument label}}{{19-21=}}

  func f3(_ a: Int, b: Int) { }
}

// Operators never have keyword arguments.
infix operator +++
func +++(lhs lhs: Int, // expected-error{{operator cannot have keyword arguments}}{{10-14=}}
         rhs x: Int) -> Int { // expected-error{{operator cannot have keyword arguments}}{{10-14=}}
  return lhs + x 
}


