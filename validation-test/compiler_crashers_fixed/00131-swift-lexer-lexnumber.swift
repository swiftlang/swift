// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

({})
func f() {
    ({})
}
class a {
    typealias b = b
}
class A: A {
}
class B : C {
}
typealias C = B
func a<T>() {
    enum b {
        case c
    }
}
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
struct D : C {
    typealias F = Int
    func g<T where T.E == F>(f: B<T>) {
    }
}
protocol b {
    class func e()
}
struct c {
    var d: b.Type
    func e() {
        d.e()
    }
}
func c<d {
    enum c {
        func e
        var _ = e
    }
}
protocol A {
    typealias B
}
class C<D> {
    init <A: A where A.B == D>(e: A.B) {
 c {
    func b((Any, c))(a: (Any, AnyObject)) {
        b(a)
    }
}
protocol A {
}
struct B : A {
}
struct C<D, E: A where D.C == E> {
}
protocol a {
    class func c()
}
class b: a {
    class func c() { }
}
(b() as a).dynamicType.c()
struct A<T> {
    let a: [(T, () -> ())] = []
}
func a<T>() -> (T, T -> T) -> T {
    var b: ((T, T -> T) -> T)!
    return b
}
class A<T : A> {
}
struct c<d : SequenceType> {
    var b: d
}
func a<d>() -> [c<d>] {
    return []
}
f
e)
func f<g>() -> (g, g -> g) -> g {
   d j d.i = {
}
 {
   g) {
        h  }
}
protocol f {
   class func i()
}
class d: f{  class func i {}
func i(c: () -> ()) {
}
class a {
    var _ = i() {
    }
}
import Foundation
class Foo<T>: NSObject {
    var foo: T
    init(foo: T) {
        self.foo = foo
        super.init()
    }
}
func some<S: SequenceType, T where Optional<T> == S.Generator.Element>(xs : S) -> T? {
    for (mx : T?) in xs {
        if let x = mx {
            return x
        }
    }
    return nil
}
let xs : [Int?] = [nil, 4, nil]
print(some(xs))
a=1 as a=1
