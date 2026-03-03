// RUN: %target-swift-frontend -emit-ir -verify %s -parse-stdlib -enable-experimental-feature Embedded -wmo -disable-experimental-feature EmbeddedExistentials

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// This is only an error if we disable support for existentials in embedded.

public protocol Player {}
struct Concrete: Player {}

public func test() -> any Player {
  Concrete() // expected-error {{cannot use a value of protocol type 'any Player' in embedded Swift}}
}
