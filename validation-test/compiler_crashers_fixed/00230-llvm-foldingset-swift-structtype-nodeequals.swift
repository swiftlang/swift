// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
func ^(a: Boolean, Bool) -> Bool {
    return !(a)
}
func i(c: () -> ()) {
}
class a protocol A {
       typealias F = Int
    func g<T where T.E == F>(f: B<T>) {
    }
}
protocol a : a {
}
class A : A {
}
class B : C {
}
typealias C = B
class A<T : A> {
}
class c {
    f   var : 
