// This source file is part of the Swift.org open source project
// See http://swift.org/LICENSE.txt for license information

// RUN: not %target-swift-frontend %s -parse
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
