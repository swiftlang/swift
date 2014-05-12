// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Generic function declarations
//===----------------------------------------------------------------------===//

func f0<T>(x: Int, y: Int) { }
func f1<T : Any>(x: Int, y: Int) { }
func f2<T : protocol<Generator,Any>>(x: Int, y: Int) { }
func f3<T : () -> ()>(x: Int, y: Int) { } // expected-error{{expected a type name or protocol composition restricting 'T'}}
func f4<T>(x: T, y: T) { }

// Name lookup within local classes.
func f5<T, U>(x: T, y: U) {
  struct Local {
    func f() {
      var t : T = 17 // expected-error{{cannot convert the expression's type 'Int' to type 'T'}}
      var u : U = 17 // okay: refers to 'U' declared within the local class
    }
    typealias U = Int
  }
}

// Non-protocol type constraints.
func f6<T : Wonka>(x: T) {} // expected-error{{use of undeclared type 'Wonka'}}

// FIXME: The term 'inherit' is unfortunate here.
func f7<T : Int>(x: T) {} // expected-error{{inheritance from non-protocol, non-class type 'Int'}}
