// RUN: rm -rf %t/clang-module-cache
// RUN: %target-swift-frontend %s -module-cache-path %t/clang-module-cache -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// https://devforums.apple.com/message/1051132

import CoreData
class A : NSManagedObject {
    func b<T: A>() -> [T] {
        return [T]()
    }
}
