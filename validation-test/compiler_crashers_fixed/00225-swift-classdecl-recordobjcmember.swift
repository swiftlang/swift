// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
import Foundation
class d<c>: NSObject {
    var b: c
    init(b: c) {
        self.b = b
   }
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
protocol b {
    class func e()
}
struct c {
    var d: b.Type
    func e() {
        d.e()
    }
}
class c {
    func b((Any, c))(a: (Any, AnyObject)) {
        b(a)
    }
}
struct c<d, e: b where d.c == e> {
}
func f<T : Boolean>(b: T) {
}
f(true as Boolean)
enum S<T> {
    case C(T, () -> ())
}
var f = 1
var e: Int -> Int = {
    return $0
}
let d: Int =  { c, b in
}(f, e)
struct d<f : e, g: e where g.h == f.h> {
}
protocol e {
    typealias h
}
func a(x: Any, y: Any) -> (((Any, Any) -> Any) -> Any) {
    return {
        (m: (Any, Any) -> Any) -> Any in
        return m(t>(c : b) -> e? {
    for (mx : e?) in c {
    }
}
struct c<d : Sequence> {
    var b: d
}
func a<d>() -> [c<d>] {
    return []
}
