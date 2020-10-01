import StdlibUnittest

fileprivate var COWLoggingArray_CopyCount = 0

public func expectNoCopyOnWrite<T>(
  _ elements: [T], 
  _ message: @autoclosure () -> String = "",
  stackTrace: SourceLocStack = SourceLocStack(),
  showFrame: Bool = true,
  file: String = #file,
  line: UInt = #line,
  _ body: (inout COWLoggingArray<T>) -> Void
) {
  let copyCountBeforeBody = COWLoggingArray_CopyCount
  var loggingArray = COWLoggingArray(elements)
  body(&loggingArray)
  expectEqual(copyCountBeforeBody, COWLoggingArray_CopyCount, message(),
    stackTrace: stackTrace.pushIf(showFrame, file: file, line: line),
    showFrame: false)
}

public struct COWLoggingArray<Element> {
  var storage: Storage
  
  class Storage {
    var buffer: UnsafeMutableBufferPointer<Element>
    var count: Int
    var capacity: Int {
      buffer.count
    }
    
    init(capacity: Int) {
      self.buffer = .allocate(capacity: capacity)
      self.count = 0
    }
    
    deinit {
      buffer.baseAddress!.deinitialize(count: count)
      buffer.deallocate()
    }
    
    func cloned(capacity: Int? = nil) -> Storage {
      let newCapacity = Swift.max(capacity ?? self.capacity, self.capacity)
      let newStorage = Storage(capacity: newCapacity)
      newStorage.buffer.baseAddress!
        .initialize(from: buffer.baseAddress!, count: count)
      newStorage.count = count
      return newStorage
    }
  }
  
  mutating func _makeUnique() {
    if !isKnownUniquelyReferenced(&storage) {
      storage = storage.cloned()
      COWLoggingArray_CopyCount += 1
    }
  }
}

extension COWLoggingArray: RandomAccessCollection, RangeReplaceableCollection,
  MutableCollection, ExpressibleByArrayLiteral
{
  public var count: Int { storage.count }
  public var startIndex: Int { 0 }
  public var endIndex: Int { count }
  
  public subscript(i: Int) -> Element {
    get {
      storage.buffer[i]
    }
    set {
      _makeUnique()
      storage.buffer[i] = newValue
    }
  }
  
  public init() {
    storage = Storage(capacity: 10)
  }
  
  public mutating func reserveCapacity(_ n: Int) {
    if !isKnownUniquelyReferenced(&storage) {
      COWLoggingArray_CopyCount += 1
      storage = storage.cloned(capacity: n)
    } else if count < n {
      storage = storage.cloned(capacity: n)
    }
  }
  
  public mutating func replaceSubrange<C>(_ subrange: Range<Int>, with newElements: C)
    where C : Collection, Element == C.Element
  {
    _makeUnique()
    let newCount = (count - subrange.count) + newElements.count
    if newCount > storage.capacity {
      storage = storage.cloned(capacity: newCount)
    }
    
    let startOfSubrange = storage.buffer.baseAddress! + subrange.lowerBound
    let endOfSubrange = startOfSubrange + subrange.count
    let endOfNewElements = startOfSubrange + newElements.count
    let countAfterSubrange = count - subrange.upperBound
    
    // clear out old elements
    startOfSubrange.deinitialize(count: subrange.count)
    
    // move elements above subrange
    endOfNewElements.moveInitialize(from: endOfSubrange, count: countAfterSubrange)
    
    // assign new elements
    for (pointer, element) in zip(startOfSubrange..., newElements) {
      pointer.initialize(to: element)
    }
    
    // update count
    storage.count = newCount
  }
  
  public init(arrayLiteral elements: Element...) {
    storage = Storage(capacity: elements.count)
    replaceSubrange(0..<0, with: elements)
  }
}
