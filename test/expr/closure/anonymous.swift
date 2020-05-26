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

func variadic() {
  // These work

  takesVariadicInt({let _ = $0})
  takesVariadicInt({let _: [Int] = $0})

  let _: (Int...) -> () = {let _: [Int] = $0}

  takesVariadicInt({takesIntArray($0)})

  let _: (Int...) -> () = {takesIntArray($0)}

  takesVariadicGeneric({takesIntArray($0)})

  // FIXME: Problem here is related to multi-statement closure body not being type-checked together with
  // enclosing context. We could have inferred `$0` to be `[Int]` if `let` was a part of constraint system.
  takesVariadicGeneric({let _: [Int] = $0})
  // expected-error@-1 {{unable to infer type of a closure parameter $0 in the current context}}

  takesVariadicIntInt({_ = $0; takesIntArray($1)})
  takesVariadicIntInt({_ = $0; let _: [Int] = $1})
}
