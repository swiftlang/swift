// RUN: not %target-swift-frontend %s -typecheck

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
  public static func dispatchTester<G : IteratorProtocol>(
    g: G
  ) -> LoggingIterator<LoggingIterator<G>> {
    return LoggingIterator(LoggingIterator(g))
  }
  public static var next = TypeIndexed(0)
}

public struct LoggingIterator<Base : IteratorProtocol>
  : IteratorProtocol, LoggingType {

  typealias Log = IteratorLog
  
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
  public class func dispatchTester<S: Sequence>(
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
  public static var _copyContents = TypeIndexed(0)
}

public protocol LoggingSequenceType  : Sequence, LoggingType {
  typealias Base : Sequence
}

extension LoggingSequenceType {
  public init(_ base: Base) {
    self.base = base
  }
  
  public func makeIterator() -> LoggingIterator<Base.Iterator> {
    ++SequenceLog.iterator[selfType]
    return LoggingIterator(base.makeIterator())
  }

  public func underestimatedCount() -> Int {
    ++SequenceLog.underestimatedCount[selfType]
    return base.underestimatedCount()
  }

  public func map<T>(
    transform: (Base.Iterator.Element) -> T
  ) -> [T] {
    ++SequenceLog.map[selfType]
    return base.map(transform)
  }

  public func filter(
    isIncluded: (Base.Iterator.Element) -> Bool
  ) -> [Base.Iterator.Element] {
    ++SequenceLog.filter[selfType]
    return base.filter(isIncluded)
  }
  
  public func _customContainsEquatableElement(
    element: Base.Iterator.Element
  ) -> Bool? {
    ++SequenceLog._customContainsEquatableElement[selfType]
    return base._customContainsEquatableElement(element)
  }
  
  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: (Self) -> R
  ) -> R? {
    ++SequenceLog._preprocessingPass[selfType]
    return base._preprocessingPass { _ in preprocess(self) }
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToNativeArrayBuffer()
    -> _ContiguousArrayBuffer<Base.Iterator.Element> {
    ++SequenceLog._copyToNativeArrayBuffer[selfType]
    return base._copyToNativeArrayBuffer()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing ptr: UnsafeMutablePointer<Base.Iterator.Element>
  ) {
    ++SequenceLog._copyContents[selfType]
    return base._copyContents(initializing: ptr)
  }
}

public struct LoggingSequence<Base_: Sequence> : LoggingSequenceType {
  typealias Log = SequenceLog
  typealias Base = Base_
  
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
  static var subscriptIndex = TypeIndexed(0)
  static var subscriptRange = TypeIndexed(0)
  static var isEmpty = TypeIndexed(0)
  static var count = TypeIndexed(0)
  static var _customIndexOfEquatableElement = TypeIndexed(0)
  static var first = TypeIndexed(0)
}

public protocol LoggingCollectionType : Collection, LoggingSequenceType {
  typealias Base : Collection
}

extension LoggingCollectionType {
  subscript(position: Base.Index) -> Base.Iterator.Element {
    ++CollectionLog.subscriptIndex[selfType]
    return base[position]
  }
  
  subscript(_prext_bounds: Range<Base.Index>) -> Base._prext_SubSequence {
    ++CollectionLog.subscriptRange[selfType]
    return base[_prext_bounds]
  }    

  var isEmpty: Bool {
    ++CollectionLog.isEmpty[selfType]
    return base.isEmpty
  }

  var count: Base.Index.Distance {
    ++CollectionLog.count[selfType]
    return base.count
  }
  
  func _customIndexOfEquatableElement(element: Iterator.Element) -> Base.Index?? {
    ++CollectionLog._customIndexOfEquatableElement[selfType]
    return base._customIndexOfEquatableElement(element)
  }

  var first: Iterator.Element? {
    ++CollectionLog.first[selfType]
    return base.first
  }
}

struct LoggingCollection<Base_: Collection> : LoggingCollectionType {}
