// RUN: %target-typecheck-verify-swift

protocol P {
    associatedtype A: P // expected-note {{protocol requires nested type 'A'; do you want to add it?}}
}

struct Type<Param> {}
extension Type: P where Param: P, Param.A == Type<Param> {
  // expected-error@-1 {{circular reference}}
  // expected-note@-2 {{through reference here}}
  // expected-error@-3 {{circular reference}}
  // expected-note@-4 {{through reference here}}
  // expected-error@-5 {{circular reference}}
  // expected-note@-6 {{through reference here}}
  // expected-error@-7 {{type 'Type<Param>' does not conform to protocol 'P'}}
  typealias A = Param
  // expected-note@-1 {{through reference here}}
  // expected-note@-2 {{through reference here}}
}
