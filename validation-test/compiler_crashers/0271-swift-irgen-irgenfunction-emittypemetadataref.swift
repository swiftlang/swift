// RUN: %target-swift-frontend %s -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// https://devforums.apple.com/message/1051132

import CoreData
class A : NSManagedObject {
    func b<T: A>() -> [T] {
        return [T]()
    }
}
