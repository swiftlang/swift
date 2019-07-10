// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/../ClangImporter/Inputs/custom-modules %s -emit-sil

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
