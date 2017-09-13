// RUN: %target-typecheck-verify-swift -swift-version 3

var _: () -> Int = {a in 0}

var _: (Int, Int) -> Int = {a in 0}

// <rdar://problem/20371273> Type errors inside anonymous functions don't provide enough information
func f20371273() {
  let x: [Int] = [1, 2, 3, 4]
  let y: UInt = 4
  _ = x.filter { ($0 + y)  > 42 }  // expected-warning {{deprecated}}
}

// rdar://problem/32432145 - compiler should emit fixit to remove "_ in" in closures if 0 parameters is expected

func r32432145(_ a: () -> ()) {}
r32432145 { _ in let _ = 42 } // Ok in Swift 3
r32432145 { _ in // Ok in Swift 3
  print("answer is 42")
}
