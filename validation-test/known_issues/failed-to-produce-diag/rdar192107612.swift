// rdar://192107612
// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

struct S<each T> {
  var x: (repeat each T)
}

func f<each T>(_ t: repeat (each T).Type) -> S<repeat each T>? { nil }

let _ = f(Int.self)?.x.bitWidth // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
