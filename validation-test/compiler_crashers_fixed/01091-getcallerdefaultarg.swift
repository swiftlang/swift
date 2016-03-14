// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
class A : NSManagedObject {
func b<T: A>() -> [T] {
}
var e: Int -> Int = {
return $0 Foundation
func a(b: Int = 0) {
}
let c = a
c()
