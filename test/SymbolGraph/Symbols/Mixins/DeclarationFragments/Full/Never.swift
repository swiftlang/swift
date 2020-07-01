// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Never -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Never -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Never.symbols.json --match-full-lines --strict-whitespace

// REQUIRES: OS=macosx

// Attributes that should never appear anywhere in the symbol graph.

@available(macOS, deprecated)
public func deprecated() {}

@inlinable
public func inlinable() {}

@inline(never)
public func inline() {}

public struct S {
  public static prefix func ..<(s: S) -> S {
    return s
  }
  public static func +(a: S, b: S) -> S {
    return a
  }
  public static postfix func ++(s: S) -> S {
    return s
  }
}

// CHECK-NOT: @available
// CHECK-NOT: @inlinable
// CHECK-NOT: @inline
// CHECK-NOT: "spelling": "prefix"
// CHECK-NOT: "spelling": "postfix"
// CHECK-NOT: "spelling": "infix"

