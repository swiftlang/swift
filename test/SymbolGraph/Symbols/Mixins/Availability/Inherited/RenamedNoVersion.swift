// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name RenamedFilled -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name RenamedFilled -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/RenamedFilled.symbols.json

// REQUIRES: OS=macosx

@available(macOS, renamed: "S.bar")
public struct S {
  public func foo() {}
}

// There is no version information or unconditional deprecation here,
// so we will not expect to see an empty availability mix-in with
// just the `renamed` field, even though there is a domain listed.
// CHECK-NOT: "availability":
