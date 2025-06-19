// REQUIRES: objc_interop, OS=macosx
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: split-file %s %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) %t/objc-implement.swift -typecheck -parse-as-library -emit-module-interface-path %t/ObjcImplement.swiftinterface -enable-library-evolution -module-name ObjcImplement -import-underlying-module -swift-version 5 -emit-api-descriptor-path %t/api.json
// RUN: %validate-json %t/api.json | %FileCheck %s

//--- objc-implement.swift
import Foundation

// API record for ObjCClass shouldn't be emitted from swift.
@objc @implementation
extension ObjCClass {
  init?(char: CChar) {}
}

// API record for ObjCClass and ObjCCategory shouldn't be emitted from swift.
@objc(ObjCCategory) @implementation
extension ObjCClass {
  convenience init?(int: Int32) {}
}

// This is an actual class declaration that should be included in the API
// descriptor.
@objc
public class SwiftObjCClass: NSObject {}

//--- ObjcImplement.h
@interface Root
@end

@interface ObjCClass : Root
- (instancetype) initWithChar:(char)c;
@end

@interface ObjCClass (ObjCCategory)
- (instancetype) initWithInt:(int)i;
@end

//--- module.modulemap
module ObjcImplement {
  header "ObjcImplement.h"
  export *
}


// CHECK-NOT: "file": "{{.*}}.h"

// CHECK:      "interfaces": [
// CHECK-NEXT:    {
// CHECK-NEXT:      "name": "_TtC13ObjcImplement14SwiftObjCClass",
// CHECK-NEXT:      "access": "public",
// CHECK-NEXT:      "file": "{{.*}}/objc-implement.swift",
// CHECK-NEXT:      "linkage": "exported",
// CHECK-NEXT:      "super": "NSObject",
// CHECK-NEXT:      "instanceMethods": [
// CHECK-NEXT:        {
// CHECK-NEXT:          "name": "init",
// CHECK-NEXT:          "access": "public",
// CHECK-NEXT:          "file": "{{.*}}/objc-implement.swift"
// CHECK-NEXT:        }
// CHECK-NEXT:      ],
// CHECK-NEXT:      "classMethods": []
// CHECK-NEXT:    }
// CHECK-NEXT: ],
// CHECK-NEXT: "categories": [],
// CHECK-NEXT: "version": "1.0"
