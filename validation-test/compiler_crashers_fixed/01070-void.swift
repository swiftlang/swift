// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck
import CoreData
if true {
struct Q<T where I.c : a {
deinit {
protocol a {
}
protocol A {
struct S {
}
}
}
print() {
let f = A(x: String = "A>(b: d = A.b = c()
typealias R = F>()
