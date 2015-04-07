// RUN: not %target-swift-frontend %s -parse

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

func b(c) -> <d>(() -> d) {
}
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
    typealias h
}
struct c<d : SequenceType> {
    var b: d
}
func a<d>() -> [c<d>] {
    return []
}
func i(c: () -> ()) {
}
class a {
    var _ = i() {
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
protocol A {
    func c() -> String
}
class B {
    func d() -> String {
        return ""
   B) {
    }
}
func ^(a: BooleanType, Bool) -> Bool {
    return !(a)
}
func f<T : BooleanType>(b: T) {
}
f(true as BooleanType)
var f = 1
var e: Int -> Int = {
    return $0
}
let d: Int =  { c, b in
}(f, e)
func d<b: SequenceType, e where Optional<e> == b.Generator.Element>(c : b) -> e? {
    for (mx : e?) in c {
    }
}
protocol a {
}
protocol b : a {
}
protocol c : a {
}
protocol d {
  typealias f = a
}
struct e : d {
  typealias f = b
}
func i<j : b, k : d where k.f == j> (n: k) {
}
func i<l : d where l.f == c> (n: l) {
}
i(e(ass func c()
}
class b: a {
    class func c() { }
}
(b() as a).dynamicType.c()
class a<f : b, g : b where f.d == g> {
}
protocol b {
    typealias d
    typealias e
}
struct c<h : b> : b {
    typealias d = h
    typealias e = a<c<h>, d>
}
class a {
    typealias b = b
}
func a(b: Int = 0) {
}
let c = a
c()
enum S<T> {
    case C(T, () -> ())
}
func f() {
    ({})
}
enum S<T> : P {
    func f<T>() -> T -> T {
        return { x in x }
    }
}
protocol P {
    func f<T>()(T) -> T
}
protocol A {
    typealias B
    func b(B)
}
struct X<Y> : A {
    func b(b: X.Type) {
    }
}
class A<T : A> {
}
protocol a {
  typealias d
  typealias e = d
  typealias f = d
}
class b<h : c, i : c where h.g == i> : a {
}
class b<h, i> {
}
protocol c {
    typealias g
}
struct A<T> {
    let a: [(T, () -> ())] = []
}
struct c<d, e: b where d.c == e> {
}
func a(x: Any, y: Any) -> (((Any, Any) -> Any) -> Any) {
    return {
        (m: (Any, Any) -> Any) -> Any in
        return m(x, y)
    }
}
func b(z: (((Any, Any) -> Any) -> Any)) -> Any {
    return z({
        (p: Any, q:Any) -> Any in
        return p
    })
}
b(a(1, a(2, 3)))
func a<T>() -> (T, T -> T) -> T {
    var b: ((T, T -> T) -> T)!
    return b
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
import Foundation
class d<c>: NSObject {
    var b: c
    init(b: c) {
        self.b = b
   }
}
func a<T>() {
    enum b {
        case c
    }
}
protocol a : a {
}
class A : A {
}
class B : C {
}
typealias C = B
