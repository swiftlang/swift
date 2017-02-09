// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: %target-swift-frontend %s -emit-ir

// Test case submitted to project by https://github.com/tmu (Teemu Kurppa)
// rdar://18118173

class a<T : Hashable> {
  var b = [T : Bool]()
  init<S : Sequence where S.Iterator.Element == T>(_ c: S) {
    c.map { self.b[$0] = true }
  }
}
a([1])
