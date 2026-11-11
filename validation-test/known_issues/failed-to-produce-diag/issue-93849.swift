// rdar://191490858 (https://github.com/swiftlang/swift/issues/93849)
// RUN: %target-typecheck-verify-swift

let a: [Int?] = ["a"]
// expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
let b: [String: Int?] = ["k": "a"]
// expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
let c: Set<Int?> = ["a"]
// expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
