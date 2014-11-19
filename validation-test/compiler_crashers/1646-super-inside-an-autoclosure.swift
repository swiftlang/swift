// RUN: rm -rf %t/clang-module-cache
// RUN: not --crash %swift %s -sdk %sdk -module-cache-path %t/clang-module-cache -emit-silgen

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)

import CoreData
class A : NSManagedObjectContext {
    override func save(a: NSErrorPointer) -> Bool {
        return self.hasChanges || super.save(a)
    }
}
