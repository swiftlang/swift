// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
protocol A {
    typealias B
}
class C<D> {
    init <A: A where A.B == D>(e: A.B) {
    }
}
import Founda
    typealias F
    func g<T where T.E == F>(f: B<T>)
}
struct D : C {
    ias F = Int
    func g<T where T.E == F>(f: B<T>) {
    }
}
func b(c) -> <d>(() -> d) {S<T> : P  convenience init<T>(array: Array<T>}
