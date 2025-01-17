protocol PHelper {
}

protocol P {
  associatedtype Assoc: PHelper // expected-note {{protocol requires nested type 'Assoc'}}
}

struct A : P { // expected-error {{type 'A' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  typealias Assoc = Int // expected-note {{possibly intended match 'A.Assoc' (aka 'Int') does not conform to 'PHelper'}}
}
