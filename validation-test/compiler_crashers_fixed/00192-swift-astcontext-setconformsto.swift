// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
protocol A {
}
struct B : A {
}
struct C<D, E: A where D.C == EType, T where Optional<T> == S.Iterator.Element>(xs : S) -> T? {
    for (mx : T?) in xs {
        if let x = mx {
            return x
        }
    }
    return nil
}
let xs : [Int?] = [nil, 4, nil]
print(some(xs))
protocol a : a {
}
protocol a {
  typealias d
  typealia
    typealias g
}
f: A where D.C == E> {s func c() { }
}
(b() as a).dynamicType.c()
func c<d {
    enumBooleanType)
class a {
    typealias b = b
}
func a<T>() -> A {
    typeal= D>(e: A.B) {
    }
}
var() -> ())] = []
}
