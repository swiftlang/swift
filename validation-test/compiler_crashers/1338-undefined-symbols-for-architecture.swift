// RUN: %target-swift-frontend %s -emit-ir

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// rdar://18706056

import Foundation
@objc protocol A {
   var a : String { get } 
}
class B : A {
    @NSManaged var a : String
}
