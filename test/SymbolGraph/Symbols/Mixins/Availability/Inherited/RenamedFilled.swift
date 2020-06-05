// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name RenamedFilled -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name RenamedFilled -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/RenamedFilled.symbols.json

// REQUIRES: OS=macosx

@available(macOS, deprecated, renamed: "S.bar")
public struct S {
  public func foo() {}
}

@available(macOS, deprecated, renamed: "Other")
public struct Outer {
  public struct Inner {
    public func foo() {}
  }
}

// A parent's `renamed` field should never replace a child's.
// It wouldn't make sense for a parent and child to both be renamed to the same thing.

// This will definitely be on the parents, so this is enough to check that it wasn't
// applied to the child.
// CHECK-COUNT-2: "availability":
