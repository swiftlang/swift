// RUN: not --crash %target-swift-frontend %s -parse
// XFAIL: asan

// Distributed under the terms of the MIT license
// Test case submitted to project by https://github.com/practicalswift (practicalswift)
// Test case found by fuzzing

class a {
type b, g : b where f.d == g> {
}
protocol b {
typealias d
typealias e
}
struct c<h : b> : b {
typealias d = h
typealias e = a<c<h>, d>
}
func prefix(with: String) -> <T>(() -> T) -> String {
return { g in "\(with): \(g())" }
}
func f<T : BooleanType>(b: T) {
}
f(true as BooleanType)
func a<T>() -> (T, T -> T) -> T {
var b: ((T, T -> T) -> T)!
return b
}
func ^(a: BooleanType, Bool) -> Bool {
return !(a)
}
protocol A {
}
struct B : A {
}
struct C<D, E: A where D.C == E> {
}
struct c<d : SequenceType> {
var b: d
}
func a<d>() -> [c<d>] {
return []
}
class A: A {
}
class B : C {
}
typealias C = B
func i(c: () -> ()) {
}
class a {
var _ = i() {
}
}
func c<d {
enum c {
func e
var _ = e
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
func some<S: SequenceType, T where Optional<T> == S.Generator.Element>(xs : S) -> T? {
for (mx : T?) in xs {
if let x? = mx {
return x
}
}
return nil
}
let xs : [Int?] = [nil, 4, nil]
println(some(xs))
protocol a {
typealias d
typealias e = d
typealias f = d
}
class b<h : c, i : c where h.g == i> : a {
}
