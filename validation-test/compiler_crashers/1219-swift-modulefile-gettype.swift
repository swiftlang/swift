// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func f<T>() -> T -> T {
return { x in x 1 {
}
}
class A {
class func a() -> String {
let d: String = {
return self.a()
}()
}
}
s}
func b<T: A>() -> [T] {
return [T]()
