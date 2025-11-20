// RUN: %target-swift-frontend -emit-ir -verify %s -parse-stdlib -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

public protocol Player {}
struct Concrete: Player {}

public func test() -> any Player {
  Concrete() // expected-error {{cannot use a value of protocol type 'any Player' in embedded Swift}}
}
