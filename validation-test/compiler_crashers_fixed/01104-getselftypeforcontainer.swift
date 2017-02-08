// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
struct l<l : d> : d {
i j i() {
}
}
protocol f {
}
protocol d : f {
struct c<d : Sequence> {
}
func a<d>() -> [c<d>] {
}
func a<T>() -> (T, T -> T) -> T {
}
func r<t>() {
f f {
}
}
struct i<o : u> {
}
func r<o>() -> [i<o>] {
}
class g<t : g> {
}
class g: g {
}
class n : h {
}
protocol g {
func i() -> l  func o() -> m {
}
}
func j<t k t: g, t: n>(s: t) {
}
protocol r {
}
protocol f : r {
