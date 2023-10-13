// RUN: %target-swift-frontend -emit-ir -verify %s -parse-stdlib -enable-experimental-feature Embedded -wmo

public protocol Player {}
struct Concrete: Player {}

public func test() -> any Player {
  Concrete() // expected-error {{using values of protocol type 'any Player' is not allowed in embedded Swift}}
  // expected-note@-1 {{called from here}}
}
