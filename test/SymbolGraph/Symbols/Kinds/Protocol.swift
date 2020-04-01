// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Protocol -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Protocol -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Protocol.symbols.json

// CHECK: "identifier": "swift.protocol"
// CHECK-NEXT: "displayName": "Protocol"
// CHECK: pathComponents
// CHECK-NEXT: "P"
public protocol P {}
