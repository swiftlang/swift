//===----------------------------------------------------------------------===//
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

/// Wrapper for a contiguous array of T.  UnsafeArray is both a
/// Collection—which is multi-pass if you use indices or call
/// generate() on it—and a Generator, which can only be assumed to be
/// single-pass.  It's not clear how well this combination will work
/// out, or whether all Collections should also be Streams; consider
/// this an experiment.
struct UnsafeArray<T> : Collection, Generator {

  var startIndex: Int {
    return 0
  }

  var endIndex: Int {
    return _end - _position
  }

  subscript(i: Int) -> T {
    assert(i >= 0)
    assert(i < endIndex)
    return (_position + i).get()
  }
  
  init(start: UnsafePointer<T>, length: Int) {
    _position = start
    _end = start + length
  }

  mutating func next() -> T? {
    if _position == _end {
      return .None
    }
    return .Some((_position++).get())
  }

  func generate() -> UnsafeArray {
    return self
  }
  
  var _position, _end: UnsafePointer<T>
}
