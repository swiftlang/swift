// RUN: %target-typecheck-verify-swift -swift-version 3
// Generate a swift 3 compatible warning if final is specified in an extension

protocol P {}

extension P {
  final func inExtension() {} // expected-warning{{functions in a protocol extension do not need to be marked with 'final'}} {{3-9=}}
}
