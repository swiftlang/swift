// RUN: %target-typecheck-verify-swift -swift-version 4

// rdar://problem/31969605

class Base {}
class Derived : Base {}

protocol Refined {}
protocol Proto : Refined {}
extension Base : Refined {}

func baseFn(_: Base) {}

func superclassConversion(fn: @escaping (Base) -> ()) {
  let _: (Derived) -> () = fn
}

func existentialConversion(fn: @escaping (Refined) -> ()) {
  let _: (Proto) -> () = fn
  let _: (Base) -> () = fn
  let _: (Derived) -> () = fn
}

// rdar://problem/31725325

func a<b>(_: [(String, (b) -> () -> Void)]) {}
func a<b>(_: [(String, (b) -> () throws -> Void)]) {}

class c {
  func e() {}
  static var d = [("", e)]
}
a(c.d)

func b<T>(_: (T) -> () -> ()) {}

b(c.e)

func bar(_: () -> ()) {}
func bar(_: () throws -> ()) {}
func bar_empty() {}

bar(bar_empty)

func consumeNoEscape(_ f: (Int) -> Int) {}
func consumeEscaping(_ f: @escaping (Int) -> Int) {}
func takesAny(_ f: Any)  {}

func twoFns(_ f: (Int) -> Int, _ g: @escaping (Int) -> Int) {
  // expected-note@-1 {{parameter 'f' is implicitly non-escaping}}
  takesAny(f) // expected-error {{converting non-escaping value to 'Any' may allow it to escape}}
  takesAny(g)
  var h = g
  h = f // expected-error {{assigning non-escaping parameter 'f' to an @escaping closure}}
}

takesAny(consumeNoEscape)
takesAny(consumeEscaping)

var noEscapeParam: ((Int) -> Int) -> () = consumeNoEscape
var escapingParam: (@escaping (Int) -> Int) -> () = consumeEscaping
noEscapeParam = escapingParam // expected-error {{cannot assign value of type '(@escaping (Int) -> Int) -> ()' to type '((Int) -> Int) -> ()}}

escapingParam = takesAny
noEscapeParam = takesAny // expected-error {{converting non-escaping value to 'Any' may allow it to escape}}
