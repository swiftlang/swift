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
struct MapGenerator<Base: Generator, T> : Generator, Sequence {
  mutating func next() -> T? {
    return base.next().map(transform)
  }
  
  func generate() -> MapGenerator {
    return self
  }

  var base: Base
  var transform: (Base.Element)->T
}

struct Map<Base: Collection, T> : Collection {
  var startIndex: Base.IndexType {
    return base.startIndex
  }
  
  var endIndex: Base.IndexType {
    return base.endIndex
  }

  subscript(index: Base.IndexType) -> T {
    return transform(base[index])
  }

  func generate() -> MapGenerator<Base.GeneratorType, T> {
    return MapGenerator(base.generate(), transform)
  }
  
  var base: Base
  var transform: (Base.GeneratorType.Element)->T
}

