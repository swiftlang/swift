//===--- Map.swift - Lazily map the elements of a Collection --------------===//
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

struct _MapGenerator<Base: Generator, T> : Generator, Sequence {
  mutating func next() -> T? {
    return base.next().map(transform)
  }
  
  func generate() -> _MapGenerator {
    return self
  }

  init(_ base: Base, _ transform: (Base.Element)->T) {
    self.base = base
    self.transform = transform
  }

  var base: Base
  var transform: (Base.Element)->T
}

struct _Map<Base: Collection, T> : Collection {
  var startIndex: Base.IndexType {
    return base.startIndex
  }
  
  var endIndex: Base.IndexType {
    return base.endIndex
  }

  subscript(index: Base.IndexType) -> T {
    return transform(base[index])
  }

  func generate() -> _MapGenerator<Base.GeneratorType, T> {
    return _MapGenerator(base.generate(), transform)
  }

  init(_ base: Base, transform: (Base.GeneratorType.Element)->T) {
    self.base = base
    self.transform = transform
  }
  
  var base: Base
  var transform: (Base.GeneratorType.Element)->T
}

