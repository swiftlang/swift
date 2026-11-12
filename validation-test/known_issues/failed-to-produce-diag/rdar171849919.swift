// rdar://171849919
// RUN: %target-typecheck-verify-swift -target %target-swift-6.2-abi-triple

let foo = [8 of Int](repeating: 3)
try [11 of UInt] { i throws(Never) in // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
    guard let a = .some(foo[i]) else { throw Never() }
}
