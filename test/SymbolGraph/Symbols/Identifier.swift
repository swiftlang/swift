// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Identifier -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Identifier -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Identifier.symbols.json

public struct MyStruct {}

// CHECK: "precise": "s:10Identifier8MyStructV",
// CHECK-NEXT: "interfaceLanguage": "swift"
