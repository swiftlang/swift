// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -target x86_64-apple-macosx10.50 -verify

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -swift-version 4 %s -target x86_64-apple-macosx10.50 -dump-ast > %t.ast
// RUN: %FileCheck %s < %t.ast

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Foundation

// Nested classes that aren't available in our deployment target.
@available(OSX 10.51, *)
class CodingI : NSObject, NSCoding {
  required init(coder: NSCoder) { }
  func encode(coder: NSCoder) { }
}

@available(OSX 10.51, *)
class OuterCodingJ {
  // CHECK-NOT: class_decl{{.*}}"NestedJ"{{.*}}@_staticInitializeObjCMetadata
  class NestedJ : CodingI { }
}
