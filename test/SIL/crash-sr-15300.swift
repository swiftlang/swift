// RUN: %target-swift-frontend -Osize -parse-stdlib -enable-ossa-modules -emit-sil %s

// https://bugs.swift.org/browse/SR-15300
// XFAIL: *

import Swift

public struct A {
  public init(_ pairs: KeyValuePairs<Any, Any>) {
    Builtin.unreachable()
  }
}
