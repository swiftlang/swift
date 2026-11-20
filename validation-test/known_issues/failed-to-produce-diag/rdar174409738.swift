// rdar://174409738
// RUN: %target-typecheck-verify-swift

func f(numbers: inout [Float32]) {
  numbers[0] = .random(in: ) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
