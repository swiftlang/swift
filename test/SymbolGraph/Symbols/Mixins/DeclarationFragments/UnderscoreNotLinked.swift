// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name UnderscoreNotLinked -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name UnderscoreNotLinked -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/UnderscoreNotLinked.symbols.json

public protocol _ShouldntBeLinked {}
public protocol ShouldBeLinked : _ShouldntBeLinked {}
public struct MyStruct : ShouldBeLinked {}

// CHECK: "spelling": "_ShouldntBeLinked"
// CHECK-NOT: "preciseIdentifier": "s:19UnderscoreNotLinked011_ShouldntBeC0P"
