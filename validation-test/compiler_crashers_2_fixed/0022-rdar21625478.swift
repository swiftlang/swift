// RUN: %target-swift-frontend %s -emit-silgen

import StdlibUnittest

protocol Resettable : AnyObject {
  func reset()
}

internal var _allResettables: [Resettable] = []

public class TypeIndexed<Value> : Resettable {
  public init(_ value: Value) {
    self.defaultValue = value
    _allResettables.append(self)
  }
  
  public subscript(t: Any.Type) -> Value {
    get {
      return byType[ObjectIdentifier(t)] ?? defaultValue
    }
    set {
      byType[ObjectIdentifier(t)] = newValue
    }
  }

  public func reset() { byType = [:] }

  internal var byType: [ObjectIdentifier:Value] = [:]
  internal var defaultValue: Value
}

//===--- LoggingWrappers.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public protocol Wrapper {
  typealias Base
  init(_: Base)
  var base: Base {get set}
}

public protocol LoggingType : Wrapper {
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

public class IteratorLog {
  public static func dispatchTester<G: IteratorProtocol>(
    g: G
  ) -> LoggingIterator<LoggingIterator<G>> {
    return LoggingIterator(LoggingIterator(g))
  }
  public static var next = TypeIndexed(0)
}

public struct LoggingIterator<Base: IteratorProtocol>
  : IteratorProtocol, LoggingType {

  public typealias Log = IteratorLog
  
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
  public static func dispatchTester<S: Sequence>(
    s: S
  ) -> LoggingSequence<LoggingSequence<S>> {
    return LoggingSequence(LoggingSequence(s))
  }
  public static var iterator = TypeIndexed(0)
  public static var underestimatedCount = TypeIndexed(0)
  public static var map = TypeIndexed(0)
  public static var filter = TypeIndexed(0)
  public static var _customContainsEquatableElement = TypeIndexed(0)
  public static var _preprocessingPass = TypeIndexed(0)
  public static var _copyToNativeArrayBuffer = TypeIndexed(0)
  public static var _initializeTo = TypeIndexed(0)
}

public protocol LoggingSequenceType  : Sequence, LoggingType {
  typealias Base : Sequence
  typealias Log : AnyObject = SequenceLog
  typealias Iterator : IteratorProtocol = LoggingIterator<Base.Iterator>
}

extension LoggingSequenceType {
  public var underestimatedCount: Int {
    ++SequenceLog.underestimatedCount[selfType]
    return base.underestimatedCount
  }
}

extension LoggingSequenceType
  where Log == SequenceLog, Iterator == LoggingIterator<Base.Iterator> {
  public func makeIterator() -> LoggingIterator<Base.Iterator> {
    ++Log.iterator[selfType]
    return LoggingIterator(base.makeIterator())
  }

  public func map<T>(
    @noescape transform: (Base.Iterator.Element) -> T
  ) -> [T] {
    ++Log.map[selfType]
    return base.map(transform)
  }

  public func filter(
    @noescape includeElement: (Base.Iterator.Element) -> Bool
  ) -> [Base.Iterator.Element] {
    ++Log.filter[selfType]
    return base.filter(includeElement)
  }
  
  public func _customContainsEquatableElement(
    element: Base.Iterator.Element
  ) -> Bool? {
    ++Log._customContainsEquatableElement[selfType]
    return base._customContainsEquatableElement(element)
  }
  
  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    @noescape preprocess: (Self) -> R
  ) -> R? {
    ++Log._preprocessingPass[selfType]
    return base._preprocessingPass { _ in preprocess(self) }
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToNativeArrayBuffer()
    -> _ContiguousArrayBuffer<Base.Iterator.Element> {
    ++Log._copyToNativeArrayBuffer[selfType]
    return base._copyToNativeArrayBuffer()
  }

  /// Copy a Sequence into an array.
  public func _initializeTo(ptr: UnsafeMutablePointer<Base.Iterator.Element>)
    -> UnsafeMutablePointer<Base.Iterator.Element> {
    ++Log._initializeTo[selfType]
    return base._initializeTo(ptr)
  }
}

public struct LoggingSequence<
  Base_: Sequence
> : LoggingSequenceType, Sequence {
  public typealias Log = SequenceLog
  public typealias Base = Base_

  public init(_ base: Base_) {
    self.base = base
  }
  
  public var base: Base_
}

public class CollectionLog : SequenceLog {
  public class func dispatchTester<C: Collection>(
    c: C
  ) -> LoggingCollection<LoggingCollection<C>> {
    return LoggingCollection(LoggingCollection(c))
  }
  static var startIndex = TypeIndexed(0)
  static var endIndex = TypeIndexed(0)
  static var subscriptIndex = TypeIndexed(0)
  static var subscriptRange = TypeIndexed(0)
  static var isEmpty = TypeIndexed(0)
  static var count = TypeIndexed(0)
  static var _customIndexOfEquatableElement = TypeIndexed(0)
  static var first = TypeIndexed(0)
}

public protocol LoggingCollectionType : LoggingSequenceType, Collection {
  typealias Base : Collection
  typealias Index : ForwardIndex = Base.Index
}

extension LoggingCollectionType
where Index == Base.Index {
  public var startIndex: Base.Index {
    ++CollectionLog.startIndex[selfType]
    return base.startIndex
  }
  
  public var endIndex: Base.Index {
    ++CollectionLog.endIndex[selfType]
    return base.endIndex
  }
  public subscript(position: Base.Index) -> Base.Iterator.Element {
    ++CollectionLog.subscriptIndex[selfType]
    return base[position]
  }
  
  public subscript(_prext_bounds: Range<Base.Index>) -> Base.SubSequence {
    ++CollectionLog.subscriptRange[selfType]
    return base[_prext_bounds]
  }    

  public var isEmpty: Bool {
    ++CollectionLog.isEmpty[selfType]
    return base.isEmpty
  }

  public var count: Base.Index.Distance {
    ++CollectionLog.count[selfType]
    return base.count
  }
  
  public func _customIndexOfEquatableElement(element: Base.Iterator.Element) -> Base.Index?? {
    ++CollectionLog._customIndexOfEquatableElement[selfType]
    return base._customIndexOfEquatableElement(element)
  }

  public var first: Base.Iterator.Element? {
    ++CollectionLog.first[selfType]
    return base.first
  }
}

public struct LoggingCollection<Base_: Collection> : LoggingCollectionType {
  public typealias Iterator = LoggingIterator<Base.Iterator>
  public typealias Log = CollectionLog
  public typealias Base = Base_
  public typealias SubSequence = Base.SubSequence

  public func makeIterator() -> Iterator {
    return Iterator(base.makeIterator())
  }
  public init(_ base: Base_) {
    self.base = base
  }
  
  public var base: Base_
}

public func expectCustomizable<
  T : Wrapper where
  T : LoggingType,
  T.Base : Wrapper, T.Base : LoggingType,
  T.Log == T.Base.Log
>(_: T, _ counters: TypeIndexed<Int>,
  stackTrace: SourceLocStack? = nil,
  file: String = #file, line: UInt = #line,
  collectMoreInfo: (()->String)? = nil
) {
  expectNotEqual(
    0, counters[T.self], collectMoreInfo?() ?? "",
    stackTrace: stackTrace ?? SourceLocStack(), file: file, line: line)
  
  expectEqual(
    counters[T.self], counters[T.Base.self], collectMoreInfo?() ?? "",
    stackTrace: stackTrace ?? SourceLocStack(), file: file, line: line)
}

public func expectNotCustomizable<
  T : Wrapper where
  T : LoggingType,
  T.Base : Wrapper, T.Base : LoggingType,
  T.Log == T.Base.Log
>(_: T, _ counters: TypeIndexed<Int>,
  stackTrace: SourceLocStack? = nil,
  file: String = #file, line: UInt = #line,
  collectMoreInfo: (()->String)? = nil
) {
  expectNotEqual(
    0, counters[T.self], collectMoreInfo?() ?? "",
    stackTrace: stackTrace ?? SourceLocStack(), file: file, line: line)
  
  expectEqual(
    0, counters[T.Base.self], collectMoreInfo?() ?? "",
    stackTrace: stackTrace ?? SourceLocStack(), file: file, line: line)
}
