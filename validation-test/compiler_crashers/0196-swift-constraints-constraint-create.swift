// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: no_asserts

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

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
i(e())
func prefi(with: String-> <T>() -> T)t
    func d() -> String {
        return 1
k f {
    typealias c
}
class g<i{
}
d(j i)
class h {
    typealias i = i
}
struct l<e : SequenceType> {
    l g: e
}
func h<e>() -> [l<e>] {
    f []
}
func i(e: g) -> <j>(() -> j) -> k
a)
func a<b:a
func b<e>(e : e) -> c {  e
class j {
    func y((Any, j))(v: (Any, AnyObject)) {
        y(v)
    }
}
func w(j: () -> ()) {
}
class v {
    l _ = w() {
    }
}
({})
func v<x>() -> (x, x -> x) -> x {
    l y j s<q : l, y: l m y.n == q.n> {
}
o l {
    u n
}
y q<x> {
    s w(x, () -> ())
}
o n {
    func j()  p
}
class r {
    func s() -> p {
        t ""
    }
}
class w: r, n {
    k v: ))] = []
}
class n<x : n> 
class A: A {
}
class B : C {
}
typealias C = B
protocol f {
    k g d {
    k d 
    k k
}
j j<l : d> : d {
    k , d>
}
class f: f {
}
class B : l {
}
k l = B
class f<i : f
class i<h>: c {
    var g: h
    init(g: h) {
        self.g = g
        e{
    j d>
}
class f {
    typealias e = e
protocol A {
    typealias B
}
class C<D> {
    init <A: A where A.B == D>(e: A.B) {
    }
}
func p<p>() -> (p, p -> p) -> p {
   l c l.l = {
}
 {
   p) {
      (e: o, h:o) ->  e
    })
}
j(k(m, k(2, 3)))
func l(p: j) -> <n>(() -> n 
func f(k: Any, j: Any) -> (((Any, Any) -> Any) -> c
k)
func c<i>() -> (i, i -> i) -> i {
   k b k.i = {
}
 {
   i) {
        k  }
}
protocol c {
   class func i()
}
class k: c{  class func i {
a=1 as a=1
func d<b: SequenceType, e where Optional<e> == b.Generator.Element>(c : b) -> e? {
    for (mx : e?) in c {
func a<T>() -> (T, T -> T) -> T {
    var b: ({ (x: Int, f: Int -> Int) -> Int in
    return f(x)
}(x1, f1)
let crashes: Int = { x, f in
    return f(x)
}(x1ny) -> Any) -> Anyh>, d>
}
class A<T : A> {
}
func i(c: () -> ()) {
}
class a {
    var _ = i() {
    }
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
pss d: f{  class func i {}
}
class f<p : k, p : k n p.d> : o {
}
class f<p, p> {
}
protocol k {
    e o
}
protocol o {
    class func k(dynamicType.k()
f j = i
f l: k -> k = {
    m
}(j, l)
f
protocol k : f { func f
struct c<d: SequenceType, b where Optional<b> == d.Generator.Element>
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
class k {
    func l((Any, k))(m }
}
func j<f: l: e -> e = {
   
 {
   l) {
      m  }
}
protocol k {
   class func j()
}
class e: k{  class func j 
func b(c) -> <d>(() -> d)
protocol A {
    func c() -> String
}
class B {
    func e<T where T: A, T: B>(t: T) {
    t.c()
}
func prefix(with: String) -> <T>(() -> T) -> String {
  return { g in "\(with): \(g())" }
}
protocol A {
    typealias B   ret
}
struct C<D, E: A where D.C == E> {
}
struct A<T> {
    let a, () -> ())] = []
}
enum S<T> {
    case C(T, () -> ())
}
struct A<T> {
    let a: [(T, () -> ())d : SequenceTy
    return []
}
func prefix(with: String) -> <T>(() -> T) -  t.c()
}
struct c<d : SequenceType> {
    var b: d
}
func a<d>() -> [c<d>] {
    return []
}
func e<k>() -> (k, k -> k) -> k {
   f j f.i = {
}
 {
   k) {
       n  }
}
m e {
   class func i()
}
class f: e{  class func i {}
func n<i>() {
    k k {
        f j
    }
}
func n(k: Int = l) {
}
let j = n
func ^(a: BooleanType, Bool) -> Bool {
    return !(a)
}
protocol a {
    class func c()
class b: a {
    class func c() { }
}
(b() as a).dynamicType.c()
b
protocol c : b { func b
func some<S: SequenceType, T where Optional<T> == S.Generator.Element>(xs : S) -> T? {
    for (mx :       if let x = mx {
     d: f{  ceanTy b {
    clasi() {
    }
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
f> {
 c(d ())
}
func b(e)-> <d>(() -> d)
b
protocol c : b { func b
protocol a {
}
protocol h : a {
}
protocol k : a {
}
protocol g {
  j n = a
}
struct n : g {
  j n = h
}
func i<h : h, f : g m f.n == h> (g: f) {
}
func i<n : g m n.n = o) {
}
let k = a
k()
h
protocol k : h { func h
k
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
d = i
}
class d<j : i, f : i where j.i == f> : e {
}
class d<j, f> {
}
protocol i {
    typealias i
}
protocol e {
    class func i()
}
i
(d() as e).j.i()
d
protocol i : d { func d
f b<g f:
func f<g {
  enum f {
    func f
var _ = f
}
func a<g>() -> (g, g -> g) -> g {
    var b: ((g, g -> g) -> g)!
    return b
}
func f<g : d {
    return !(a)
  enum g {
        func g
        var _ = g
func b<d {
    enum b {    func c
 var _ = c
func f() {
    ({})
}
func f<T : BooleanType>(b: T) {
}
f(true as BooleanType)
func a<T>() -> (T, T -> T) -> T {
    var4, nil]
print(some(xs))
protocol A {
    t   class func i()
}
class d: f{  class func i {}
var x1 = 1
var f1: Int -> Int tInt -> Int) -> Int in
    return f>] {
    return []
}
func i(c: () -> ()) {
}
class a {
    var _ = i() {
    }
}
struct A<T> {
    let a: [(T, () -> ())] = [namicType.c()
func a(b: Int = 0) {
}
let c = a
c()
class a {
    typealias b = b
}
({})
import Foundation
class m<j>k i<g : g, e : f k(f: l) {
}
i(())
class h {
    typealias g = g
func b<d-> d { class d:b class b
func f<e>() -> (e, e -> e) -> e {
   e b e.c = {
}
 {
   e) {
        f  }
}
protocol f {
   class func c()
}
class e: f{  class func c
protocol a {
    class func c()
}
class b: a {
    c T) {
}
f(true as BooleanType)
func f() {
    ({})
}
import Foundation
class Foo<T>:     1)
func c<d {
    enum c {
        func e
        var _ = e
    }
}
struct c<d : SequenceType> {
    var b: d
}
func a<d>() -> [c<d>] {
    return []
}
a=1 as a=1
func some<S: SequenceType, T where Optional<T> return !(a)
}
({})
func prefix(with: String) -> <T>(() -> T) -> String { func b
clanType, Bool) -> Bool {
)
}
strs d 
    typealias b> : b {
    typealias d = h
    typealias e = a<c<h>, d>
}
protocol A {
    typealias B
}
class C<D> {
    init <A: A where A.B == D>(e: A.B) {
    }
}
func a() as a).dynamicType.c()
func prefix(with: String) -> <T>(() -> T) -> String {
  return { g in "\(with): \(g())" }
}
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
    typealias h
}
protocol a : a {
}
func a<T>() -> (T, T -> T) -> T)!c : b { func b
var f = 1
var e: Int -> Int = {
    return $0
}
let d: Int =  { c, b in
    }(f, e)
func f<T : BooleanType>(b: T) {
}
f(true as BooleanType)
i)
import Foundation
class q<k>: NSObject {
    var j: k
    e ^(l: m, h) -> h {
    f !(l)
}
protocol l {
 d g n()
}
class h: l {
    class g n() { }
}
(h() o l).p.n()
class l<n : h,
d> Bool {
    e !(f)
}
b
protocol f : b { func b
protocol A {
    typealias B
    func b(B)
}
struct X<Y> : A {
    func b(b: X.Type) {
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
d ""
e}
class d {
    func b((Any, d)typealias b = b
 []
}
protocol p {
}
protocol g : p {
}
n    j  }
}
protocol k {
   class func q()
}
class n: k{  class func q {}
func r<e: t, s where j<s> == e.m { func g
k q<n : t> {
    q g: n
}
func p<n>() -> [q<n>] {
    o : g.l) {
    }
}
class p {
    typealias g = g
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
protocol l : p {
}
protocol m {
  j f = p
}
f m : m {
  j f = o
}
func i<o : o, m : m n m.f == o> (l: m) {
}
k: m
}
func p<m>() -> [l<m>] {
    return []
}
f
m)
func f<o>() -> (o, o -> o) -> o {
   m o m.i = {
}
 {
   o) {
        p  }
}
protocol f {
   class func i()
}
class mo : m, o : p o o.m == o> (m: o) {
}
func s<v : p o v.m == m> (u: String) -> <t>(() -> t) -
>)
}
struct n : C {
 class p {
    typealias n = n
}
l
l)
func l<u>() -> (u, u -> u) -> u {
   n j n.q = {
}
 {
   u) {
        h  }
}
protocol l {
   class {
    func n() -> q {
        return ""
    }
}
class C: s, l {
  t) {
    return {
        (s: (t, t) -> t) -> t o
        return s(c, u)
    }
}
 
func n(r: (((t, t) -> t) -> t)) -> t {
    return r({
         return k
    })
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
w
class x<u>: d {
    l i: u
    init(i: u) {
        o.i = j {
  r { w s "\(f): \(w())" }
}
protocol h {
    q k {
    t w
}
w
protocol k : w { func v <h: h m h.p == k>(l: h.p) {
    }
}
protocol h {
    n  func w(w: 
}
class h<u : h> {
o
}
class f<p : k, p : k where p.n == p> : n {
}
class f<p, p> {
}
protocol k {
    typealias n
}
o: i where k.j == f> {l func k() { }
}
(f() as n).m.k()
func k<o {
    enum k {
        func o
        var _ = o
() {
    g g         h g
    }
}
func e(i: d) -> <f>(() -> f)>
q
var m: Int -> Int = {
    n $0
 o: Int = { d, l f
    n l(d)
}(k, m)
protocol j {
  typealias d
  typealias n = d
  typealias l = d}
class g<q : l, m : l p q.g == m> : j {
}
class g<q, m> {
}
protocol l {
    typealias g
