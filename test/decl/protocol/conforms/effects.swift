// RUN: %target-typecheck-verify-swift -verify-ignore-unrelated

// https://github.com/swiftlang/swift/issues/86105
protocol P {
    func perform(_ body: () -> Void) -> Void
    // expected-note@-1 {{protocol requires function 'perform' with type '(() -> Void) -> Void'}}
}

struct S {
    func perform<E: Error>(_ body: () throws(E) -> Void) throws(E) -> Void {
    // expected-note@-1 {{candidate throws, but protocol does not allow it}}
        try body()
    }
}

extension S: P {}
// expected-error@-1 {{type 'S' does not conform to protocol 'P'}}
// expected-note@-2 {{add stubs for conformance}}



// https://github.com/swiftlang/swift/issues/73479
protocol P1: AsyncIteratorProtocol {
    mutating func next() async -> Element?
}
struct S2: P1 {
    mutating func next() async -> Int? {
        return nil
    }
}


public protocol Functor<A> {
  associatedtype FA: Functor = Self
  associatedtype A
  func map<B>(_ f: (A) -> B) -> FA where FA.A == B  // expected-note {{protocol requires function 'map' with type}}
}
extension Optional : Functor {
  // expected-error@-1 {{type 'Optional<Wrapped>' does not conform to protocol 'Functor'}}
  // expected-note@-2 {{add stubs for conformance}}
  public typealias A = Wrapped
}
