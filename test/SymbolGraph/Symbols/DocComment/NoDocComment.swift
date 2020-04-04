// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name NoDocComment -emit-module-path %t/NoDocComment.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name NoDocComment -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/NoDocComment.symbols.json

public struct S {}

// CHECK-NOT: docComment
