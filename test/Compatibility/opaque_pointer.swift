// RUN: %target-swift-frontend -typecheck -parse-as-library %s -verify -swift-version 5

func testConversionToMutable(ptr: UnsafeRawPointer) {
  _ = UnsafeMutableRawPointer(ptr) // expected-warning{{'init' is deprecated: replaced by 'init(mutating:)'}}
  // expected-note@-1{{use 'init(mutating:)' instead}}{{31-31=mutating: }}
}
