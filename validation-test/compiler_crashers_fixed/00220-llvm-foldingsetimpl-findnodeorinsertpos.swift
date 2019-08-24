// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
protocol A {
    typealias B
    func b(B)
}
struct X<Y> : A {
    func b(b: X.Type) {
    }
}
func a<T>() -> (T, T -> T) -> T {
    var b: ((T, T -> T) -> T)!
    return b
}
func f<T : Boolean>(b: T) {
}
f(true as Boolean)
protocol a {
    class func c()
}
class b: a {
    class funcol c : a {
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
class A<T : A> {
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
struct c<d : Sequence> {
    var b: d
}
func a<d>() -> [c<d>] {
    return []
}
func d<b: Sequence, e where Optional<e> == b.Iterator.Element>(c : b) -> e? {
    for (mx : e?) in c {
    }
}
protocol b {
    class fol C {
    typealias F
    func g<T whe   case c
    }
}
func b(c) -> <d>(() -> d) {
}
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
import Foundation
class d<c>: NSObject 
}
