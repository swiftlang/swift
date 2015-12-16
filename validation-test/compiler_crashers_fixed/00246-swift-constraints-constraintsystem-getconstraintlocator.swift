// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol A {
    typealias E
}
struct B<T : A> {
    let h: T
    let i: T.E
}
protocol C {
    typealias F
    func g<T where T.E == F>(f: B<T>)
}
struct D : C {on NSSet {
    convenience init(array: Array) {
        self.init()
    }
}
var f = 1
var e: Int -> Int = {
    return $0
}
let d: Int =  { c, b in
}(f, e)
enum S<T> : P {
    func f<T>() -> T -> T {
   }
}
protocol P {
    func f<T>()(T) -> T
}
import Foundation
class d<c>: NSObject {
    var b: c
    init(b: c) {
        self.b = b
   }
}
import lf.c = c
    }
}
func b(c) -> <d>(() -> d) {
}
func a(b: Int = 0) {
}
let c = a
c()
