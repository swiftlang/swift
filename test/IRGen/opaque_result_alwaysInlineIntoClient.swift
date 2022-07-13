// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -parse-as-library -emit-library -emit-module-path %t/AlwaysInlineIntoWithOpaque.swiftmodule -module-name AlwaysInlineIntoWithOpaque %S/Inputs/AlwaysInlineIntoWithOpaque.swift -o %t/%target-library-name(AlwaysInlineIntoWithOpaque)
// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -lAlwaysInlineIntoWithOpaque -module-name main -I %t -L %t %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// RUN: %target-build-swift -target %target-cpu-apple-macosx10.15 -parse-as-library -emit-library -emit-module-path %t/AlwaysInlineIntoWithOpaque.swiftmodule -module-name AlwaysInlineIntoWithOpaque %S/Inputs/AlwaysInlineIntoWithOpaqueReplacement.swift -o %t/%target-library-name(AlwaysInlineIntoWithOpaque)
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: OS=macosx && (CPU=x86_64 || CPU=arm64)
// REQUIRES: executable_test

import AlwaysInlineIntoWithOpaque

public func test() {
  let p = testInlineWithOpaque()
  print(p)
}

test()
// CHECK: 1
