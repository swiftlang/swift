// RUN: %target-swift-frontend %s -emit-silgen

// REQUIRES: objc_interop

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

import CoreData
class A : NSManagedObjectContext {
    // Error-handling mutilates this test a lot.
    override func isEqual(other: AnyObject?) -> Bool {
        self.hasChanges || super.isEqual(other)
    }
}
