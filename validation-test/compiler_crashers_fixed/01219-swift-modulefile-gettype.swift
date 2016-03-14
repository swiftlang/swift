// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
