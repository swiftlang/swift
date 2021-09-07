// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
}
struct Straint<C: P> where C.A : P {
 typealias X = Any 
}
protocol Q : Straint<Self>.X {} // expected-error {{type 'Self' does not conform to protocol 'P'}}
