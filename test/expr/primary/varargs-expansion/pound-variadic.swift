// RUN: %target-typecheck-verify-swift

func variadic1(x: Int...) {}
func variadic2(y: Int..., z: String) {}

func parseErrors1() {
  variadic2(y: #variadic([1,2,3], z: "hello") // expected-error {{expected ')' to complete '#variadic' expression}}
  // expected-note@-1 {{to match this opening '('}}
}

func parseErrors2() {
  variadic1(x: #variadic() ) // expected-error {{expected expression within '#variadic(...)'}}
}

func parseErrors3() {
  variadic1(x: #variadic[1,2,3])) // expected-error {{expected '(' following '#variadic'}}
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2 {{expected expression}}
}

func parseErrors4() {
  variadic2(y: #variadic(]), z: "hello") // expected-error {{expected expression within '#variadic(...)'}}
}

func f(_ x: Int...) {}
func f2(x: String, y: Int..., z: String) {}
func f3(_ x: String...) {}
struct HasVariadicSubscript {
  subscript(bar: Int...) -> Int {
    get { 42 }
  }
}
let closure: ((Int...) -> Void) = {
  (args: Int...) in
}

f(1,2,3)
f(#variadic([1,2,3]))
closure(#variadic([1,2,3]))
let x: [Int] = [1,2,3] + [4]
f(#variadic(x))
f(#variadic(x.map { $0 + 1 }))
f(#variadic([]))

f2(x: "Hello,", y: 1,2,3, z: "world!")
f2(x: "Hello again,", y: #variadic([1,2,3]), z: "world!")
f2(x: "Hello yet again,", y: #variadic(x), z: "world!")

f3(#variadic(["\(1)", "\(2)"]))

func overloaded(x: [Int]) {}
func overloaded(x: Int...) {}
overloaded(x: #variadic(x))

func takesArray(x: Int...) {}
takesArray(x: #variadic([1, 2, 3]))


f(#variadic([1,2,3]), 3, 4) // expected-error {{#variadic cannot be used alongside additional variadic arguments}}
f(1, 2, #variadic([1,2,3])) // expected-error {{#variadic cannot be used alongside additional variadic arguments}}
f(1, 2, #variadic([1,2,3]), 3, 4) // expected-error {{#variadic cannot be used alongside additional variadic arguments}}
f(#variadic([1,2,3]), 3, 4, #variadic([1,2,3])) // expected-error {{#variadic cannot be used alongside additional variadic arguments}}
f(#variadic([1,2,3]), #variadic([1,2,3])) // expected-error {{#variadic cannot be used alongside additional variadic arguments}}

f(#variadic(1)) // expected-error {{expression type '[_]' is ambiguous without more context}}

class A {}
class B: A {}
protocol P {}
struct S: P {}
struct S2 {}
func takesA(_ x: A...) {}
func takesP(_ x: P...) {}
takesA(#variadic([A(), A()]))
takesA(#variadic([B(), B()]))
takesA(#variadic([S2()])) // expected-error {{cannot convert value of type 'S2' to expected element type 'A'}}
takesP(#variadic([S(), S(), S()]))
takesP(#variadic([S2()])) // expected-error {{argument type '[S2]' does not conform to expected type 'P'}}

f(#variadic(#variadic([1,2,3]))) // expected-error {{#variadic can only be used in an argument position}}
let y = #variadic([1,2,3]) // expected-error {{#variadic can only be used in an argument position}}
