// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

import Foundation
class A {
    class func a() -> String {
        return ""
    }
    class func b() {
        struct c {
            static let d: String = {
                return self  }
}
func b(c) -> <d>(() -> d) {
}
struct d<f(b: c) {
        self.b = b
   }
}
var f = 1
var e: Int -> Int = {
    return $0
}
let d: Int =  { c, b in
}(f, e)
import Foundation
extension NSSet {
    convenience init<T>(array: Array<T>) {
        self.init()
    }
}
