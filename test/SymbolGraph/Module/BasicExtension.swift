// Testing with Extension Block Symbols Off:

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name BasicExtension -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name BasicExtension -I %t -pretty-print -output-dir %t -omit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/BasicExtension@Swift.symbols.json --check-prefixes ALL,EXTRACT,EBSOff,EBSOff_EXTRACT

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name BasicExtension -emit-module -emit-module-path %t/ -emit-symbol-graph -emit-symbol-graph-dir %t -omit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/BasicExtension@Swift.symbols.json --check-prefixes ALL,BUILD,EBSOff,EBSOff_BUILD


// Testing with Extension Block Symbols On:

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name BasicExtension -emit-module -emit-module-path  %t/
// RUN: %target-swift-symbolgraph-extract -module-name BasicExtension -I %t -pretty-print -output-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/BasicExtension@Swift.symbols.json --check-prefixes ALL,EXTRACT,EBSOn,EBSOn_EXTRACT

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name BasicExtension -emit-module -emit-module-path %t/ -emit-symbol-graph -emit-symbol-graph-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/BasicExtension@Swift.symbols.json --check-prefixes ALL,BUILD,EBSOn,EBSOn_BUILD

/// We add some useless capabilities to ``Swift/String``.
extension String {
  /// Return something.
  public var something: String {
    return "something"
  }
}


// Check for module name:

// ALL-LABEL: module

// ALL: {{"name": ?"BasicExtension"}}


// Check for Symbols and Documentation Strings:

// Symbols and relationships are not ordered. Therefore, we have to use
// the `-DAG` variant if we have more than one symbol/relationship.

// ALL-LABEL: symbols

// ALL-DAG: {{"precise": ?"s:SS14BasicExtensionE9somethingSSvp"}}
// EBSOn-DAG: {{"precise": ?"s:e:s:SS14BasicExtensionE9somethingSSvp"}}

// BUILD-DAG: Return something.
// EBSOn_BUILD-DAG: We add some useless capabilities to ``Swift/String``.

// EBSOn-DAG: {{"swiftExtension": ?{[[:space:]]*"extendedModule": ?"Swift",[[:space:]]*"typeKind": ?"swift.struct"[[:space:]]*}}}

// Check for Relationships:

// ALL-LABEL: relationships

// EBSOff: {{"kind": ?"memberOf",[[:space:]]*"source": ?"s:SS14BasicExtensionE9somethingSSvp",[[:space:]]*"target": ?"s:SS"}}

// EBSOn-DAG: {{"kind": ?"memberOf",[[:space:]]*"source": ?"s:SS14BasicExtensionE9somethingSSvp",[[:space:]]*"target": ?"s:e:s:SS14BasicExtensionE9somethingSSvp"}}
// EBSOn-DAG: {{"kind": ?"extensionTo",[[:space:]]*"source": ?"s:e:s:SS14BasicExtensionE9somethingSSvp",[[:space:]]*"target": ?"s:SS"}}


// Check for Symbols that should NOT be included:

// Extending `String` creates a memberOf relationship above.
// However, it should not be included as a node because `String`
// is owned by the Swift module.
// rdar://58876107
// ALL-NOT: {{"precise": ?"s:SS"}}
