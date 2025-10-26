// RUN: %target-typecheck-verify-swift

protocol P {
  typealias A = () -> Void
  func f(_: Int) -> (Int, A)
  // expected-note@-1 {{protocol requires function 'f' with type '(Int) -> (Int, Self.A)' (aka '(Int) -> (Int, () -> ())')}}
}

class C: P {
  // expected-error@-1 {{type 'C' does not conform to protocol 'P'}}
  // expected-note@-2 {{add stubs for conformance}}
  func f(_: Int) -> Int { fatalError() }
  // expected-note@-1 {{candidate has non-matching type '(Int) -> Int'}}
}
