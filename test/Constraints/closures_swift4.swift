// RUN: %target-typecheck-verify-swift -swift-version 4

// <rdar://problem/20371273> Type errors inside anonymous functions don't provide enough information
func f20371273() {
  let x: [Int] = [1, 2, 3, 4]
  let y: UInt = 4
  _ = x.filter { ($0 + y)  > 42 } // expected-error {{'+' is unavailable}}
}

// rdar://problem/32432145 - compiler should emit fixit to remove "_ in" in closures if 0 parameters is expected

func r32432145(_ a: () -> ()) {}

r32432145 { _ in let _ = 42 }
// expected-error@-1 {{contextual closure type '() -> ()' expects 0 arguments, but 1 was used in closure body}} {{13-17=}}

r32432145 { _ in
  // expected-error@-1 {{contextual closure type '() -> ()' expects 0 arguments, but 1 was used in closure body}} {{13-17=}}
  print("answer is 42")
}
