// RUN: %target-swift-frontend -emit-ir -verify %s -parse-stdlib -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler

public protocol Player {}
struct Concrete: Player {}

public func test() -> any Player {
  Concrete() // expected-error {{cannot use a value of protocol type 'any Player' in embedded Swift}}
  // expected-note@-1 {{called from here}}
}
