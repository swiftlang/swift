// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
func a(x: Any, y: Any) -> (((Any, Any) -> Any) -> Any) {
return {
}
struct X<Y> : A {
func b(b: X.Type) {
}
}
class d<c>: NSObject {
init(b: c) {
}
}
protocol a {
}
class b<h : c, i : c where h.g == i> : a {
}
class b<h, i> {
}
protocol c {
}
protocol a : a {
}
class A : A {
}
class B : C {
}
class c {
func b((Any, c))(a: (Any, AnyObject)) {
}
}
protocol b {
}
struct c {
func e() {
}
}
func d<b: Sequence, e where Optional<e> == b.Generat<d>(() -> d) {
}
protocol A {
}
class B {
func d() -> String {
}
}
class C: B, A {
override func d() -> String {
}
func c() -> String {
}
}
func e<T where T: A, T: B>(t: T) {
}
enum S<T> : P {
func f<T>() -> T -> T {
}
}
protocol P {
}
func a(b: Int = 0) {
}
struct c<d, e: b where d.c == e
