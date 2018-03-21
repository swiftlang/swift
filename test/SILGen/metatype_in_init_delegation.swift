// RUN: %target-swift-frontend -emit-silgen -verify %s
// REQUIRES: objc_interop

import Foundation
import CoreData

@available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
class Foo : NSManagedObject {
  convenience init(context: NSManagedObjectContext, value: Int) {
    self.init(entity: type(of: self).entity(), insertInto: context)
  }
}
