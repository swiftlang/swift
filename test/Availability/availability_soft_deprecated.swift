// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -warn-soft-deprecated -verify-additional-prefix soft-deprecated-

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos || OS=xros

@available(*, deprecated)
func alwaysDeprecated() {}

@available(macOS, deprecated: 1.0)
@available(iOS, deprecated: 1.0)
@available(tvOS, deprecated: 1.0)
@available(watchOS, deprecated: 1.0)
@available(visionOS, deprecated: 1.0)
func deprecatedEarly() {}

@available(macOS, deprecated: 10000)
@available(iOS, deprecated: 10000)
@available(tvOS, deprecated: 10000)
@available(watchOS, deprecated: 10000)
@available(visionOS, deprecated: 10000)
func deprecatedFarFuture() {}

protocol Proto {}
struct HasSoftDeprecatedConformanceToProto {}

@available(macOS, deprecated: 10000)
@available(iOS, deprecated: 10000)
@available(tvOS, deprecated: 10000)
@available(watchOS, deprecated: 10000)
@available(visionOS, deprecated: 10000)
extension HasSoftDeprecatedConformanceToProto: Proto {}

func test() {
  alwaysDeprecated() // expected-warning {{'alwaysDeprecated()' is deprecated}}
  deprecatedEarly() // expected-warning {{'deprecatedEarly()' was deprecated in}}
  deprecatedFarFuture() // expected-soft-deprecated-warning {{'deprecatedFarFuture()' was deprecated in}}
  let _: any Proto = HasSoftDeprecatedConformanceToProto() // expected-soft-deprecated-warning {{conformance of 'HasSoftDeprecatedConformanceToProto' to 'Proto' was deprecated in}}
}

@available(*, deprecated)
func testDeprecated() {
  alwaysDeprecated()
  deprecatedEarly()
  deprecatedFarFuture()
  let _: any Proto = HasSoftDeprecatedConformanceToProto()
}
