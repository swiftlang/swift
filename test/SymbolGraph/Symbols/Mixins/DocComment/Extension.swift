// FYI: The lit commands and FileCheck statements are at the bottom of the file, to be resilient
// against changes to the doc comment format.

/// This should be captured
extension String: CustomDebugStringConvertible {
   var debugDescription: String {
      return ""
   }
}


// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Extension -emit-module-path %t/Extension.swiftmodule
// RUN: %target-swift-symbolgraph-extract -module-name Extension -I %t -pretty-print -output-dir %t -emit-extension-block-symbols
// RUN: %FileCheck %s --input-file %t/Extension@Swift.symbols.json

// CHECK: This should be captured
