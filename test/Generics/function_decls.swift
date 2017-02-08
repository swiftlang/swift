// RUN: %target-typecheck-verify-swift

//===----------------------------------------------------------------------===//
// Generic function declarations
//===----------------------------------------------------------------------===//

func f0<T>(x: Int, y: Int, t: T) { }
func f1<T : Any>(x: Int, y: Int, t: T) { }
func f2<T : IteratorProtocol>(x: Int, y: Int, t: T) { }
func f3<T : () -> ()>(x: Int, y: Int, t: T) { } // expected-error{{expected a type name or protocol composition restricting 'T'}}
func f4<T>(x: T, y: T) { }

// Non-protocol type constraints.
func f6<T : Wonka>(x: T) {} // expected-error{{use of undeclared type 'Wonka'}}

// FIXME: The term 'inherit' is unfortunate here.
func f7<T : Int>(x: T) {} // expected-error{{inheritance from non-protocol, non-class type 'Int'}}

func f8<T> (x: Int) {} //expected-error{{generic parameter 'T' is not used in function signature}}

public class A<X> {
  init<T>(){} //expected-error{{generic parameter 'T' is not used in function signature}}
  public func f9<T, U>(x: T, y: X) {} //expected-error{{generic parameter 'U' is not used in function signature}}
  public func f10(x: Int) {}
  public func f11<T, U>(x: X, y: T) {} //expected-error{{generic parameter 'U' is not used in function signature}}
}

protocol P { associatedtype A }

func f12<T : P>(x: T) -> T.A<Int> {} //expected-error{{cannot specialize non-generic type 'T.A'}}{{29-34=}}
