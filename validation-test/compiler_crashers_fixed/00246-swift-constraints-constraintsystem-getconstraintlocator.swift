// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
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
struct D : C {on NSSet {
    convenience init(array: Array) {
        self.init()
    }
}
var f = 1
var e: Int -> Int = {
    return $0
}
let d: Int =  { c, b in
}(f, e)
enum S<T> : P {
    func f<T>() -> T -> T {
   }
}
protocol P {
    func f<T>()(T) -> T
}
import Foundation
class d<c>: NSObject {
    var b: c
    init(b: c) {
        self.b = b
   }
}
import lf.c = c
    }
}
func b(c) -> <d>(() -> d) {
}
func a(b: Int = 0) {
}
let c = a
c()
