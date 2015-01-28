// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: no_asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

struct c<d : SequenceType> {
    var b:  [c<d>] {
    return []
}
protocol a {
    class func c()
}
class b: a {
    class func c() { }
}
(b() as a).dynamicType.c()
func f<T : BooleanType>(b: T) {
}
f(true as BooleanType)
func a(x: Any, y: Any) -> (((Any, Any) -> Any) -> A  var d: b.Type
    func e() {
        d.e()
    }
}
b
protocol c : b { func b
otocol A {
  E == F>(f: B<T>)
}
struct  }
}
