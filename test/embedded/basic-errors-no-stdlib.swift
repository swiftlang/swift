// RUN: %target-swift-frontend -emit-ir -verify %s -parse-stdlib -enable-experimental-feature Embedded -wmo

public protocol Player {}
struct Concrete: Player {}

public func test() -> any Player {
  Concrete() // expected-error {{Existential type 'any Player' is unavailable in embedded Swift}}
  // expected-note@-1 {{called from here}}
}
