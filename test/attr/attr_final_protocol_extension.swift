// RUN: %target-typecheck-verify-swift -swift-version 4

protocol P {}

extension P {
  final func inExtension() {} // expected-error {{only classes and class members may be marked with 'final'}} {{3-9=}}
}
