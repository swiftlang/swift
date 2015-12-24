// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

protocol A {
    typealias B
    func b(B)
}
struct X<Y> : A {
    func b(b: X.Type) {
    }
}
struct Abject {
  a {
}
class b<h, i> {
}
protocol c {
    typealias g
}
protocol a {
    class func c()
}
class b:s   return f(x)
}(x1, f1)
let crashes: Int = { x, f in
    A.B == D>(e: A.B) {
    }
}
protocol a : a {
}
class a {
    typealias b = b
}
func prefix(with: String) -> <T>(() -> T) -> String {
  return { g in "\(with): \(g }
}
