// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -parse

// Issue found by https://github.com/zneak (zneak)

protocol A { typealias B }
class C : A { typealias B = Int }

func crash<D: C>() -> Bool {
  let a: D.B? = nil
}
