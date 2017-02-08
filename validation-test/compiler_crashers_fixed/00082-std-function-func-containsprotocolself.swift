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
struct Abject {
  a {
}
class b<h, i> {
}
protocol c {
    typealias g
}
protocol a {
    class func c()
}
class b:s   return f(x)
}(x1, f1)
let crashes: Int = { x, f in
    A.B == D>(e: A.B) {
    }
}
protocol a : a {
}
class a {
    typealias b = b
}
func prefix(with: String) -> <T>(() -> T) -> String {
  return { g in "\(with): \(g }
}
