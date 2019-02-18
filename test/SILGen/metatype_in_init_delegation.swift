// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -verify %s -sdk %S/Inputs -enable-objc-interop

// REQUIRES: objc_interop

import Foundation
import CoreData

@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
class Foo : NSManagedObject {
  convenience init(context: NSManagedObjectContext, value: Int) {
    self.init(entity: type(of: self).entity(), insertInto: context)
  }
}
