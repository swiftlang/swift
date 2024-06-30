// RUN: %target-typecheck-verify-swift -swift-version 6

// rdar://85263844 - initializer 'init(_:)' requires the types be equivalent
func rdar85263844(arr: [(q: String, a: Int)]) -> AnySequence<(question: String, answer: Int)> {
  AnySequence(arr.map { $0 }) // expected-note {{arguments to generic parameter 'Element' ('(q: String, a: Int)' and '(question: String, answer: Int)') are expected to be equal}}
  // expected-error@-1 {{cannot convert value of type '[(q: String, a: Int)]' to expected argument type '[(question: String, answer: Int)]'}}
}

// Another case for rdar://85263844
protocol P {
  associatedtype Element
}
extension Array : P {}

public struct S4<T> {
  init<S : P>(_ x: S) where S.Element == T {}
  init(_ x: Int) {}
}

extension S4 where T == (outer: Int, y: Int) {
  init(arr: [Int]) {
    // FIXME: This ideally shouldn't compile either, but because of the way we
    // generate constraints for it, it continues to compile. We should fix
    // tuple subtyping for Swift 6 mode to not accept label mismatches.
    self.init(arr.map { (inner: $0, y: $0) })
    // expected-warning@-1 {{tuple conversion from '(inner: Int, y: Int)' to '(outer: Int, y: Int)' mismatches labels}}
  }
}

public func rdar85263844_2(_ x: [Int]) -> S4<(outer: Int, y: Int)> {
  S4(x.map { (inner: $0, y: $0) }) // expected-error {{cannot convert value of type '[(inner: Int, y: Int)]' to expected argument type '[(outer: Int, y: Int)]'}}
  // expected-note@-1 {{arguments to generic parameter 'Element' ('(inner: Int, y: Int)' and '(outer: Int, y: Int)') are expected to be equal}}
}
