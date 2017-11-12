// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
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
func f<T : Boolean>(b: T) {
}
f(true as Boolean)
func a<T>() -> (T, T -> T) -> T {
var b: ((T, T -> T) -> T)!
return b
}
func ^(a: Boolean, Bool) -> Bool {
return !(a)
}
protocol A {
}
struct B : A {
}
struct C<D, E: A where D.C == E> {
}
struct c<d : Sequence> {
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
func some<S: Sequence, T where Optional<T> == S.Iterator.Element>(xs : S) -> T? {
for (mx : T?) in xs {
if let x = mx {
return x
}
}
return nil
}
let xs : [Int?] = [nil, 4, nil]
print(some(xs))
protocol a {
typealias d
typealias e = d
typealias f = d
}
class b<h : c, i : c where h.g == i> : a {
}
