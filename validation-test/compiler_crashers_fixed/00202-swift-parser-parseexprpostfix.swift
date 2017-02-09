// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
func c<b:c
func d<b: Sequence, e where Optional<e> == b.Iterator.Element>(c : b) -> e? {
    for (mx : e?) in c {
    }
}
protocol A {
    typealias B
    func b(B)
}
struct X<Y> : A {
    func b(b: X.Type) {
    }
}
struct c<e> {
    let d: i h
}
func f(h: b) -> <e>(()-> e
func c<g>() -> (g, g -> g) -> g {
   d b d.f = {
}
 {
   g) {
        i  }
}
i c {
   class func f()
}
class d: c{  class func f {}
struct d<c : f,f where g.i == c.i>
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
b(a(1, a(2  class func g() { }
}
(h() as p).dynamicType.g()
protocol p {
}
protocol h : p {
}
protocol g : p {
}
protocol n {
  o t = p
}
struct h : n {
  t : n q m.t == m> (h: m) {
}
func q<t : n q t.t == g> (h: t) {
}
q(h())
func r(g: m) -> <s>(() -> s) -> n
import Foundation
class Foo<T>: NSObject {
    var foo: T
    init(foo: T) {
 B>(t: T) {
    t.c()
} x
      x) {
}
class a {
    var _ = i() {
    }
}
a=1 as a=1
class c {
    func b((Any, c))(a: (Any, AnyObject)) {
        b(a)
    }
}
func i(f: g) -> <j>(() -> j) -> g { func g
k, l {
    typealias l = m<k<m>, f>
}
func i(c: () -> ()) {
}
class a {
    var _ = i() {
    }
}
protocol A {
    func c() -> String
}
class B {
    func d() -> String {
        return ""
    }
}
class C: B, A {
    override func d() -> String {
        return ""
    }
    func c() -> String {
        return ""
    }
}
func e<T where T: A, T: B>(t: T) {
    t.c()
}
  }
}
class b<i : b> i: g{ func c {}
e g {
 : g {
h func i() ->  }
func h<j>() -> (j, j -> j) -> j {
    var f: ({ (c: e, f: e -> e) ->   return f(c)
}(k, i)
let o: e = { c, g
    return f(c)
}(l) -> m) -> p>, e>
}
class n<j : n>
b
prot    q g: n
}
func p<n>() -> [q<n>] {
    o : g.l) {
    }
}
class p {
    typealias g = g
class a {
    typealias b = b
}
import Foundation
class m<j>k i<g : g, e : f k(f: l) {
}
i(())
class h {
    typealias g = g
protocol A {
    typealias B
}
class C<D> {
    init <A: A where A.B == D>(e: A.B) {
    }
}
