// RUN: not %target-swift-frontend %s -parse

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
  public class func dispatchTester<S: SequenceType>(
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

public protocol LoggingSequenceType  : SequenceType, LoggingType {
  typealias Base : SequenceType
}

extension LoggingSequenceType {
  public init(_ base: Base) {
    self.base = base
  }
  
  public func generate() -> LoggingGenerator<Base.Generator> {
    ++SequenceLog.generate[selfType]
    return LoggingGenerator(base.generate())
  }

  public func underestimateCount() -> Int {
    ++SequenceLog.underestimateCount[selfType]
    return base.underestimateCount()
  }

  public func map<T>(
    @noescape transform: (Base.Generator.Element) -> T
  ) -> [T] {
    ++SequenceLog.map[selfType]
    return base.map(transform)
  }

  public func filter(
    @noescape includeElement: (Base.Generator.Element) -> Bool
  ) -> [Base.Generator.Element] {
    ++SequenceLog.filter[selfType]
    return base.filter(includeElement)
  }
  
  public func _customContainsEquatableElement(
    element: Base.Generator.Element
  ) -> Bool? {
    ++SequenceLog._customContainsEquatableElement[selfType]
    return base._customContainsEquatableElement(element)
  }
  
  /// If `self` is multi-pass (i.e., a `CollectionType`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    preprocess: (Self)->R
  ) -> R? {
    ++SequenceLog._preprocessingPass[selfType]
    return base._preprocessingPass { _ in preprocess(self) }
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToNativeArrayBuffer()
    -> _ContiguousArrayBuffer<Base.Generator.Element> {
    ++SequenceLog._copyToNativeArrayBuffer[selfType]
    return base._copyToNativeArrayBuffer()
  }

  /// Copy a Sequence into an array.
  public func _initializeTo(ptr: UnsafeMutablePointer<Base.Generator.Element>) {
    ++SequenceLog._initializeTo[selfType]
    return base._initializeTo(ptr)
  }
}

public struct LoggingSequence<Base_: SequenceType> : LoggingSequenceType {
  typealias Log = SequenceLog
  typealias Base = Base_
  
  public init(_ base: Base_) {
    self.base = base
  }
  
  public var base: Base_
}

public class CollectionLog : SequenceLog {
  public class func dispatchTester<C: CollectionType>(
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

public protocol LoggingCollectionType : CollectionType, LoggingSequenceType {
  typealias Base : CollectionType
}

extension LoggingCollectionType {
  subscript(position: Base.Index) -> Base.Generator.Element {
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
  
  func _customIndexOfEquatableElement(element: Generator.Element) -> Base.Index?? {
    ++CollectionLog._customIndexOfEquatableElement[selfType]
    return base._customIndexOfEquatableElement(element)
  }

  var first: Generator.Element? {
    ++CollectionLog.first[selfType]
    return base.first
  }
}

struct LoggingCollection<Base_: CollectionType> : LoggingCollectionType {}
