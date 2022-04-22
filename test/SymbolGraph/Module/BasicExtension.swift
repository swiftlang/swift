// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name BasicExtension -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name BasicExtension -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/BasicExtension@Swift.symbols.json --check-prefix EXTRACT

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name BasicExtension -emit-module -emit-module-path %t/ -emit-symbol-graph -emit-symbol-graph-dir %t
// RUN: %FileCheck %s --input-file %t/BasicExtension@Swift.symbols.json --check-prefix BUILD

extension String {
  /// Return something.
  public var something: String {
    return "something"
  }
}

// EXTRACT: module
// EXTRACT-NEXT: "name": "BasicExtension"

// BUILD: module
// BUILD: "name":"BasicExtension"

// EXTRACT: "precise": "s:SS14BasicExtensionE9somethingSSvp"

// BUILD: "precise":"s:SS14BasicExtensionE9somethingSSvp"

// EXTRACT: "kind": "memberOf"
// EXTRACT-NEXT: "source": "s:SS14BasicExtensionE9somethingSSvp"
// EXTRACT-NEXT: "target": "s:SS"

// BUILD: "kind":"memberOf"
// BUILD: "source":"s:SS14BasicExtensionE9somethingSSvp"
// BUILD: "target":"s:SS"

// Extending `String` creates a memberOf relationship above.
// However, it should not be included as a node because `String`
// is owned by the Swift module.
// rdar://58876107
// EXTRACT-NOT: "precise": "s:SS"

// BUILD-NOT: "precise":"s:SS"
