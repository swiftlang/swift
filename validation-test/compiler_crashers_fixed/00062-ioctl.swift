// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
protocol A {
    typealias  override func d() -> String {
        return ""
    }
    func c() -> String {
        return ""
    }
}
func e<T where T: A, T: B>(t: T) {
    t.c()
}
protocol b {
    claealias f = d
}
class b<h : c, i : c where h.g == i> : a {
}
class b<h, i> {
}
protocol c {
    typealias g
}
protocol a {
    class func c(dynamicType.c()
var x1 = 1
var f1: Int -> Int = {
    return $0
}
let succeeds: Int = { (x: Int, f: Int -> Int) -> Int in
    return f(x)
}(x1, f1)
let crashes: Int = { x, f in
    return f(x)
}(x1, f1)
b
protocol c : b { func b
import Foundation
class Foo<T>: NSObject {
    var foo: T
    init(foo: T) {
        self.foo = foo
        su
    var b: ((T,}
}
