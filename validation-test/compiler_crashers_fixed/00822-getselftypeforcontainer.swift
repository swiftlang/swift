// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
protocol B : B<T where A, B<h: A<d = {
protocol a {
func c(Any, d.E == nil
return b<d : a {
}
return { x in c {
switch x }
}
typealias f : c: e(f.d {
class B.d = a("))
}
protocol A {
}
func b(a
