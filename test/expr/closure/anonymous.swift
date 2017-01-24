// RUN: %target-typecheck-verify-swift

func takeIntToInt(_ f: (Int) -> Int) { }
func takeIntIntToInt(_ f: (Int, Int) -> Int) { }

// Simple closures with anonymous arguments
func simple() {
  takeIntToInt({return $0 + 1})
  takeIntIntToInt({return $0 + $1 + 1})
}

func takesIntArray(_: [Int]) { }
func takesVariadicInt(_: (Int...) -> ()) { }
func takesVariadicIntInt(_: (Int, Int...) -> ()) { }

func takesVariadicGeneric<T>(_ f: (T...) -> ()) { }
// expected-note@-1 {{in call to function 'takesVariadicGeneric'}}

func variadic() {
  // These work
  takesVariadicInt({let _ = $0})
  takesVariadicInt({let _: [Int] = $0})
  let _: (Int...) -> () = {let _: [Int] = $0}

  // FIXME: Make the rest work
  takesVariadicInt({takesIntArray($0)})
  // expected-error@-1 {{cannot convert value of type '([Int]) -> ()' to expected argument type '(Int...) -> ()'}}

  let _: (Int...) -> () = {takesIntArray($0)}
  // expected-error@-1 {{cannot convert value of type '([Int]) -> ()' to specified type '(Int...) -> ()'}}

  takesVariadicGeneric({takesIntArray($0)})
  // expected-error@-1 {{cannot convert value of type '[_]' to expected argument type '[Int]'}}

  takesVariadicGeneric({let _: [Int] = $0})
  // expected-error@-1 {{generic parameter 'T' could not be inferred}}

  takesVariadicIntInt({_ = $0; takesIntArray($1)})
  // expected-error@-1 {{cannot convert value of type '(_, _) -> ()' to expected argument type '(Int, Int...) -> ()'}}

  takesVariadicIntInt({_ = $0; let _: [Int] = $1})
  // expected-error@-1 {{cannot convert value of type '(_, _) -> ()' to expected argument type '(Int, Int...) -> ()'}}
}
