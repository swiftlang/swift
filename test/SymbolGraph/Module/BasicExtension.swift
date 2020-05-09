// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name BasicExtension -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name BasicExtension -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/BasicExtension@Swift.symbols.json

extension String {
  /// Return something.
  public var something: String {
    return "something"
  }
}

// CHECK: module
// CHECK-NEXT: "name": "BasicExtension"

// CHECK: "precise": "s:SS14BasicExtensionE9somethingSSvp"

// CHECK: "kind": "memberOf"
// CHECK-NEXT: "source": "s:SS14BasicExtensionE9somethingSSvp"
// CHECK-NEXT: "target": "s:SS"

// Extending `String` creates a memberOf relationship above.
// However, it should not be included as a node because `String`
// is owned by the Swift module.
// rdar://58876107
// CHECK-NOT: "precise": "s:SS"
