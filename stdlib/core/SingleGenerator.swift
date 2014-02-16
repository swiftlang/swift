//===--- SingleGenerator.swift - A Generator of up to one element ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Used by T? and SequenceOfOne<T>
//
//===----------------------------------------------------------------------===//
struct SingleGenerator<T> : Generator {
//  func generate() -> SingleGenerator {
//    return self
//  }
  
  mutating func next() -> T? {
    let result = elements
    elements = .None
    return result
  }
  
  var elements: T?
}

