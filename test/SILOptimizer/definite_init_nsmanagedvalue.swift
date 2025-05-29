// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/../ClangImporter/Inputs/custom-modules %s -emit-sil -g | %FileCheck %s

// REQUIRES: objc_interop

// Make sure if we have an NSManagedObject without stored properties that DI
// does not crash or emit an error.

import Foundation
import CoreData

class Person : NSManagedObject {
  enum MyError : Error {
  case error
  }

  static func myThrow() throws {}
  static func myBool() -> Bool { return false }
  public required init(_ x: Int) throws {
    if Person.myBool() {
      throw MyError.error
    }
    super.init()
    try Person.myThrow()
  }
}

extension Person {
  @NSManaged var name: String
}

// Verify that the DI instructions share the scope of the adjacent instructions.
// CHECK: sil {{.*}}$s28definite_init_nsmanagedvalue6PersonCyACSiKcfc
// CHECK: integer_literal $Builtin.Int2, {{.*}}, scope [[SCOPE:[0-9]+]]
// CHECK-NEXT: debug_value
// CHECK-NEXT: debug_value
// CHECK-NEXT: store {{.*}}, scope [[SCOPE]]
