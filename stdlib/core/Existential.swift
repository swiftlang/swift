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

//////////////////////////////////////////
// FIXME: Workaround for inability to create existentials of protocols
// with associated types <rdar://problem/11689181>

// This file contains "existentials" for the protocols defined in
// Policy.swift.  Similar components should usually be defined next to
// their respective protocols.
struct GeneratorOf<T> : Generator, Sequence {
  init(_ next: ()->T?) {
    self._next = next
  }
  
  init<G: Generator where G.Element == T>(var _ self_: G) {
    self._next = { self_.next() }
  }
  
  mutating func next() -> T? {
    return _next()
  }

  func generate() -> GeneratorOf {
    return self
  }
  let _next: ()->T?
}

struct SequenceOf<T> : Sequence {
  init<G: Generator where G.Element == T>(_ generate: ()->G) {
    _generate = { GeneratorOf(generate()) }
  }
  init<S: Sequence where S.GeneratorType.Element == T>(_ self_: S) {
    self = SequenceOf({ self_.generate() })
  }

  func generate() -> GeneratorOf<T> {
    return _generate()
  }
  
  let _generate: ()->GeneratorOf<T>
}

struct SinkOf<T> : Sink {
  init(_ put: (T)->()) {
    _put = put
  }

  init<S: Sink where S.Element == T>(var _ base: S) {
    _put = { base.put($0) }
  }
  func put(x: T) {
    _put(x)
  }
  let _put: (T)->()
}

