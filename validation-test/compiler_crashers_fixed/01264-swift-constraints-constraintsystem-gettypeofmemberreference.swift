// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
func f<T>() -> T -> T {
return { x in xol A {
}
struct B<T : A> {
}
struct D : C {
func g<g: e where g.h == f.h> {
}
protocol e {
}
func a(b: Int = 0) {
}
let c = a
