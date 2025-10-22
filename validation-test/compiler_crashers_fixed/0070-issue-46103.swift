// This source file is part of the Swift.org open source project
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

// RUN: not %target-swift-frontend %s -typecheck

// https://github.com/apple/swift/issues/46103

protocol Element
{
  typealias ItemType = (Int, Int)
}

if let i = [(1, 2), (3, 4)].index{ (G : Element.ItemType) in
  G.1 == 1
}

