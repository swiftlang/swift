// RUN: %target-typecheck-verify-swift -swift-version 4

// rdar://problem/32432145 - compiler should emit fixit to remove "_ in" in closures if 0 parameters is expected

func r32432145(_ a: () -> ()) {}

r32432145 { _ in let _ = 42 }
// expected-error@-1 {{contextual closure type '() -> ()' expects 0 arguments, but 1 was used in closure body}} {{13-17=}}

r32432145 { _ in
  // expected-error@-1 {{contextual closure type '() -> ()' expects 0 arguments, but 1 was used in closure body}} {{13-17=}}
  print("answer is 42")
}

r32432145 { _,_ in
  // expected-error@-1 {{contextual closure type '() -> ()' expects 0 arguments, but 2 were used in closure body}} {{13-19=}}
  print("answer is 42")
}
