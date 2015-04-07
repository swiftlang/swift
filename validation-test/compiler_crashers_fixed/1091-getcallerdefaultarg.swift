// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class A : NSManagedObject {
func b<T: A>() -> [T] {
}
var e: Int -> Int = {
return $0 Foundation
func a(b: Int = 0) {
}
let c = a
c()
