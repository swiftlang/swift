// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Extension -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Extension -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Extension@Swift.symbols.json

public extension String {
  /// Return something.
  var something: String {
    return "something"
  }
}

// CHECK: module
// CHECK-NEXT: "name": "Extension"

// CHECK: "precise": "s:SS9ExtensionE9somethingSSvp"

// CHECK: "kind": "memberOf"
// CHECK-NEXT: "source": "s:SS9ExtensionE9somethingSSvp"
// CHECK-NEXT: "target": "s:SS"

// Extending `String` creates a memberOf relationship above.
// However, it should not be included as a node because `String`
// is owned by the Swift module.
// rdar://58876107
// CHECK-NOT: "precise": "s:SS"
