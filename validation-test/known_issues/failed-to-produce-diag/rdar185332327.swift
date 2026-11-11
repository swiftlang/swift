// rdar://185332327
// RUN: %target-typecheck-verify-swift

protocol ContiguousBytes {
  func withUnsafeBytes<R>(_ body: (UnsafeRawBufferPointer) throws -> R) rethrows -> R
}

extension Array: ContiguousBytes where Element == UInt8 {}

func f<C: ContiguousBytes>(data: C) {
  data.withUnsafeBytes { _ = $0.baseAddress }
}

var a = [Int]()
f(data: a) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
