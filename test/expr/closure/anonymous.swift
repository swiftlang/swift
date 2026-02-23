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

  takesVariadicGeneric({let _: [Int] = $0})
  takesVariadicIntInt({_ = $0; takesIntArray($1)})
  takesVariadicIntInt({_ = $0; let _: [Int] = $1})
}

// https://github.com/swiftlang/swift/issues/54030
if $0 {} // expected-error {{anonymous closure argument not contained in a closure}}

#if false
if $0 {}
#endif
