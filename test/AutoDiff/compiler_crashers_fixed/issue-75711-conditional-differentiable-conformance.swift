// RUN: %target-swift-frontend -emit-sil -verify %s

// https://github.com/swiftlang/swift/issues/75711

// Ensure we propagate T : Differentiable conditional conformance

import _Differentiation

struct Wrapper<T> {
  func read(_ t: T) -> T {
    return t
  }
}

protocol P {
  associatedtype T: Differentiable

  @differentiable(reverse)
  func read(_: T) -> T
}

extension Wrapper: P where T: Differentiable {}
