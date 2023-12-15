// RUN: %target-typecheck-verify-swift -parse-stdlib -module-name Swift -enable-experimental-feature BuiltinModule -enable-experimental-feature NoncopyableGenerics -enable-experimental-feature NonescapableTypes

// REQUIRES: asserts

/// This test specifically covers constructs that are only valid in the stdlib.

import Builtin

@_marker public protocol Copyable: ~Escapable {}
@_marker public protocol Escapable: ~Copyable {}

struct NC: ~Copyable {}

@frozen public struct UnsafePointer<T: ~Copyable>: Copyable {
  var value: Builtin.RawPointer
}

@frozen
public enum Optional<T: ~Copyable> {
  case some(T)
  case none
}

public func wrapping<T: ~Copyable>(_ t: consuming T) -> T? {
  return .some(t)
}

// No ownership required.
func checkCopyability(_ t: UnsafePointer<NC>) {}
