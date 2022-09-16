// FYI: The lit commands and FileCheck statements are at the bottom of the file, to be resilient
// against changes to the doc comment format.

/// location points here
///           v
public struct MyStruct {}

/// location points here
///    v
public extension String {
    func foo() { }
}

// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Location -emit-module-path %t/Location.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name Location -I %t -pretty-print -output-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/Location.symbols.json
// RUN: %FileCheck %s --input-file %t/Location@Swift.symbols.json --check-prefix EXTENSION


// CHECK: "location"
// CHECK-NEXT: "uri"
// CHECK-NEXT: "position"
// CHECK-NEXT:   "line": 5
// CHECK-NEXT:   "character": 14 

// EXTENSION-LABEL: "swift.extension"
// EXTENSION: "location"
// EXTENSION-NEXT: "uri"
// EXTENSION-NEXT: "position"
// EXTENSION-NEXT:   "line": 9
// EXTENSION-NEXT:   "character": 7
