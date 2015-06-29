//===--- LoggingWrappers.swift ---------------------------------------===//
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

public protocol WrapperType {
  typealias Base
  init(_: Base)
  var base: Base {get set}
}

public protocol LoggingType : WrapperType {
  typealias Log : AnyObject
}

extension LoggingType {
  public var log: Log.Type {
    return Log.self
  }
  
  public var selfType: Any.Type {
    return self.dynamicType
  }
}

public class GeneratorLog {
  public static func dispatchTester<G: GeneratorType>(
    g: G
  ) -> LoggingGenerator<LoggingGenerator<G>> {
    return LoggingGenerator(LoggingGenerator(g))
  }
  public static var next = TypeIndexed(0)
}

public struct LoggingGenerator<Base: GeneratorType>
  : GeneratorType, LoggingType {

  typealias Log = GeneratorLog
  
  public init(_ base: Base) {
    self.base = base
  }
  
  public mutating func next() -> Base.Element? {
    ++Log.next[selfType]
    return base.next()
  }
  
  public var base: Base
}

public class SequenceLog {
  public static func dispatchTester<S: SequenceType>(
    s: S
  ) -> LoggingSequence<LoggingSequence<S>> {
    return LoggingSequence(LoggingSequence(s))
  }
  public static var generate = TypeIndexed(0)
  public static var underestimateCount = TypeIndexed(0)
  public static var map = TypeIndexed(0)
  public static var filter = TypeIndexed(0)
  public static var _customContainsEquatableElement = TypeIndexed(0)
  public static var _preprocessingPass = TypeIndexed(0)
  public static var _copyToNativeArrayBuffer = TypeIndexed(0)
  public static var _initializeTo = TypeIndexed(0)
}

public struct LoggingSequence<Base: SequenceType> : SequenceType, LoggingType {

  typealias Log = SequenceLog
  
  public init(_ base: Base) {
    self.base = base
  }
  
  public func generate() -> LoggingGenerator<Base.Generator> {
    ++Log.generate[selfType]
    return LoggingGenerator(base.generate())
  }

  public func underestimateCount() -> Int {
    ++Log.underestimateCount[selfType]
    return base.underestimateCount()
  }

  public func map<T>(
    @noescape transform: (Base.Generator.Element) -> T
  ) -> [T] {
    ++Log.map[selfType]
    return base.map(transform)
  }

  public func filter(
    @noescape includeElement: (Base.Generator.Element) -> Bool
  ) -> [Base.Generator.Element] {
    ++Log.filter[selfType]
    return base.filter(includeElement)
  }
  
  public func _customContainsEquatableElement(
    element: Base.Generator.Element
  ) -> Bool? {
    ++Log._customContainsEquatableElement[selfType]
    return base._customContainsEquatableElement(element)
  }
  
  /// If `self` is multi-pass (i.e., a `CollectionType`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    preprocess: (LoggingSequence)->R
  ) -> R? {
    ++Log._preprocessingPass[selfType]
    return base._preprocessingPass { _ in preprocess(self) }
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToNativeArrayBuffer()
    -> _ContiguousArrayBuffer<Base.Generator.Element> {
    ++Log._copyToNativeArrayBuffer[selfType]
    return base._copyToNativeArrayBuffer()
  }

  /// Copy a Sequence into an array.
  public func _initializeTo(ptr: UnsafeMutablePointer<Base.Generator.Element>) {
    ++Log._initializeTo[selfType]
    return base._initializeTo(ptr)
  }
  
  public var base: Base
}

public func expectCustomizable<
  T : WrapperType where
  T : LoggingType,
  T.Base : WrapperType, T.Base : LoggingType,
  T.Log == T.Base.Log
>(_: T, _ counters: TypeIndexed<Int>,
  stackTrace: SourceLocStack? = nil,
  file: String = __FILE__, line: UWord = __LINE__,
  collectMoreInfo: (()->String)? = nil
) {
  expectNotEqual(
    0, counters[T.self], stackTrace: stackTrace,
    file: file, line: line, collectMoreInfo: collectMoreInfo)
  
  expectEqual(
    counters[T.self], counters[T.Base.self], stackTrace: stackTrace,
    file: file, line: line, collectMoreInfo: collectMoreInfo)
}

public func expectNotCustomizable<
  T : WrapperType where
  T : LoggingType,
  T.Base : WrapperType, T.Base : LoggingType,
  T.Log == T.Base.Log
>(_: T, _ counters: TypeIndexed<Int>,
  stackTrace: SourceLocStack? = nil,
  file: String = __FILE__, line: UWord = __LINE__,
  collectMoreInfo: (()->String)? = nil
) {
  expectNotEqual(
    0, counters[T.self], stackTrace: stackTrace,
    file: file, line: line, collectMoreInfo: collectMoreInfo)
  
  expectEqual(
    0, counters[T.Base.self], stackTrace: stackTrace,
    file: file, line: line, collectMoreInfo: collectMoreInfo)
}
