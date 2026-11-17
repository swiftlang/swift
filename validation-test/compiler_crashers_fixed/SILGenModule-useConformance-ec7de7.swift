// RUN: %target-swift-frontend -emit-silgen %s -verify
// https://github.com/swiftlang/swift/issues/89520

// FIXME: this sort of constraint on Self in a protocol declaration is
// supposed to be invalid, but this is not a good diagnostic.


struct a<b>: c {
  // expected-error@-1 {{type 'a<b>' does not conform to protocol 'c'}}
  // expected-note@-2 {{add stubs for conformance}}
  typealias d = b
}
protocol c where Self == a<d> {
  associatedtype d
  // expected-note@-1 {{protocol requires nested type 'd'}}
}
