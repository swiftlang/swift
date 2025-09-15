// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/49119

protocol P {
    associatedtype A: P // expected-note {{protocol requires nested type 'A'}} 
}

struct Type<Param> {}
extension Type: P where Param: P, Param.A == Type<Param> {
  // expected-error@-1 {{extension of generic struct 'Type' has self-referential generic requirements}}
  // expected-note@-2 2 {{through reference here}}
  // expected-error@-3 {{type 'Type<Param>' does not conform to protocol 'P'}}
  // expected-note@-4 {{add stubs for conformance}}
  typealias A = Param
  // expected-note@-1 {{possibly intended match 'Type<Param>.A' (aka 'Param') does not conform to 'P'}}
}
