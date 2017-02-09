// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
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
class A<T : A> {
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
struct c<d : Sequence> {
    var b: d
}
func a<d>() -> [c<d>] {
    return []
}
func some<S: Sequence, T where Optional<T> == S.Iterator.Element>(xs : S) -> T? {
    for (mx : T?) in xs {
       func a(b:T -> T) -> T {
    var b: ((T, T -> T) -> T)!
    return b
}
struct A<T> {
    let a: [(T, () -> ())] = []
}
func prefix(with: String) -> <T>(() -> T) -> String {
  return { g in "\(with): \(g())" }
}
