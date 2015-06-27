//===--- InstrumentedWrappers.swift ---------------------------------------===//
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
public var countNext = TypeIndexed(0)

public struct InstrumentedGenerator<Base: GeneratorType> : GeneratorType {
  public init(_ base: Base) {
    self.base = base
  }
  
  public mutating func next() -> Base.Element? {
    ++countNext[self.dynamicType]
    return base.next()
  }
  
  public var base: Base
}

public var countGenerate = TypeIndexed(0)
public var countUnderestimateCount = TypeIndexed(0)
public var count_customContainsEquatableElement = TypeIndexed(0)
public var count_preprocessingPass = TypeIndexed(0)
public var count_copyToNativeArrayBuffer = TypeIndexed(0)
public var count_initializeTo = TypeIndexed(0)

public struct InstrumentedSequence<Base: SequenceType> : SequenceType {
  public init(_ base: Base) {
    self.base = base
  }
  
  public func generate() -> InstrumentedGenerator<Base.Generator> {
    ++countGenerate[self.dynamicType]
    return InstrumentedGenerator(base.generate())
  }

  public func underestimateCount() -> Int {
    ++countUnderestimateCount[self.dynamicType]
    return base.underestimateCount()
  }

  public func _customContainsEquatableElement(
    element: Base.Generator.Element
  ) -> Bool? {
    ++count_customContainsEquatableElement[self.dynamicType]
    return base._customContainsEquatableElement(element)
  }
  
  /// If `self` is multi-pass (i.e., a `CollectionType`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    preprocess: (InstrumentedSequence)->R
  ) -> R? {
    ++count_preprocessingPass[self.dynamicType]
    return base._preprocessingPass { _ in preprocess(self) }
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToNativeArrayBuffer()
    -> _ContiguousArrayBuffer<Base.Generator.Element> {
    ++count_copyToNativeArrayBuffer[self.dynamicType]
    return base._copyToNativeArrayBuffer()
  }

  /// Copy a Sequence into an array.
  public func _initializeTo(ptr: UnsafeMutablePointer<Base.Generator.Element>) {
    ++count_initializeTo[self.dynamicType]
    return base._initializeTo(ptr)
  }
  
  public var base: Base
}
