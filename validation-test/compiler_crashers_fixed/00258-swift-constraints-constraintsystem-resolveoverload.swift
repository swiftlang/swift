// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
