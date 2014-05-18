//===--- Map.swift - Lazily map the elements of a Sequence ---------------===//
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

struct MapSequenceView<Base: Generator, T> : Generator, Sequence {
  mutating func next() -> T? {
    return base.next().map(transform)
  }
  
  func generate() -> MapSequenceView {
    return self
  }

  init(_ base: Base, _ transform: (Base.Element)->T) {
    self.base = base
    self.transform = transform
  }

  var base: Base
  var transform: (Base.Element)->T
}

struct MapCollectionView<Base: Collection, T> : Collection {
  var startIndex: Base.IndexType {
    return base.startIndex
  }
  
  var endIndex: Base.IndexType {
    return base.endIndex
  }

  subscript(index: Base.IndexType) -> T {
    return transform(base[index])
  }

  func generate() -> MapSequenceView<Base.GeneratorType, T> {
    return MapSequenceView(base.generate(), transform)
  }

  init(_ base: Base, transform: (Base.GeneratorType.Element)->T) {
    self.base = base
    self.transform = transform
  }
  
  var base: Base
  var transform: (Base.GeneratorType.Element)->T
}

func map<S:Sequence, T>(
  source: S, transform: (S.GeneratorType.Element)->T
) -> MapSequenceView<S.GeneratorType, T> {
  return MapSequenceView(source.generate(), transform)
}

func map<C:Collection, T>(
  source: C, transform: (C.GeneratorType.Element)->T
) -> MapCollectionView<C, T> {
  return MapCollectionView(source, transform)
}
