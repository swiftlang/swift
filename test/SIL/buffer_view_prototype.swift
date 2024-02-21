// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -disable-experimental-parser-round-trip \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -enable-experimental-feature NoncopyableGenerics \
// RUN:   -enable-experimental-lifetime-dependence-inference \ 
// RUN:   -Xllvm -enable-lifetime-dependence-diagnostics

// REQUIRES: asserts
// REQUIRES: swift_in_compiler
// REQUIRES: noncopyable_generics

// Incrementally testing https://github.pie.apple.com/guillaume-l/BufferView with lifetime dependence

// TODO: Use real Range
public struct FakeRange<Bound> {
  public let lowerBound: Bound
  public let upperBound: Bound
}

// TODO: Use real Optional
public enum FakeOptional<T> {
  case none
  case some(T)
}

public struct BufferViewIndex<Element> {
  let _rawValue: UnsafeRawPointer

  internal init(rawValue: UnsafeRawPointer) {
    _rawValue = rawValue
  }

  var isAligned: Bool {
    (Int(bitPattern: _rawValue) & (MemoryLayout<Element>.alignment-1)) == 0
  }
}

extension BufferViewIndex: Equatable {}

extension BufferViewIndex: Hashable {}

extension BufferViewIndex: Strideable {
  public typealias Stride = Int

  public func distance(to other: BufferViewIndex) -> Int {
    let bytes = _rawValue.distance(to: other._rawValue)
    let (q, r) = bytes.quotientAndRemainder(dividingBy: MemoryLayout<Element>.stride)
    precondition(r == 0)
    return q
  }

  public func advanced(by n: Int) -> BufferViewIndex {
    .init(rawValue: _rawValue.advanced(by: n &* MemoryLayout<Element>.stride))
  }
}

extension BufferViewIndex: Comparable {
  public static func <(lhs: BufferViewIndex, rhs: BufferViewIndex) -> Bool {
    lhs._rawValue < rhs._rawValue
  }
}

public struct BufferView<Element> : ~Escapable {
  let start: BufferViewIndex<Element>
  public let count: Int
  private var baseAddress: UnsafeRawPointer { start._rawValue }
  
  public init<Owner>(
      baseAddress: UnsafeRawPointer,
      count: Int,
      dependsOn owner: borrowing Owner
    ) {
      self.init(
        start: .init(rawValue: baseAddress), count: count, dependsOn: owner
      )
  }
  init<Owner>(
    start index: BufferViewIndex<Element>,
    count: Int,
    dependsOn owner: borrowing Owner
  ) {
    precondition(count >= 0, "Count must not be negative")
    if !_isPOD(Element.self) {
      precondition(
        index.isAligned,
        "baseAddress must be properly aligned for \(Element.self)"
      )
    }
    self.start = index
    self.count = count
  }
  @_unsafeNonescapableResult
  init(start index: BufferViewIndex<Element>,
    count: Int) {
    precondition(count >= 0, "Count must not be negative")
    if !_isPOD(Element.self) {
      precondition(
        index.isAligned,
        "baseAddress must be properly aligned for \(Element.self)"
      )
    }
    self.start = index
    self.count = count
  }
}
// TODO: extend  Collection, BidirectionalCollection, RandomAccessCollection {
extension BufferView {
  public typealias Index = BufferViewIndex<Element>
  public typealias SubSequence = Self

  public var startIndex: Index { start }
  public var endIndex: Index { start.advanced(by: count) }  
 
  public subscript(position: Index) -> Element {
    get {
      if _isPOD(Element.self) {
        return position._rawValue.loadUnaligned(as: Element.self)
      }
      else {
        return position._rawValue.load(as: Element.self)
      }
    }
  }
 
  public subscript(bounds: FakeRange<BufferViewIndex<Element>>) -> Self {
    get {
      BufferView(
        start: bounds.lowerBound,
        count: bounds.upperBound.distance(to:bounds.lowerBound) / MemoryLayout<Element>.stride
      )
    }
  }
}

extension Array {
  // var view: BufferView<Element> {
  //   withUnsafeBufferPointer {
  //     return BufferView(unsafeBuffer: $0, storage: self)
  //   }
  // }
  // TODO: Implementation of getter should not need a temporary
  // rdar://123071321
  var view: BufferView<Element> {
    var _view : BufferView<Element>? 
    withUnsafePointer(to:self) {
      _view = BufferView(baseAddress: $0, count: self.count, dependsOn: self)
    }
    return _view!
  }
}

public func array_view_element(a: [Int] , i: BufferViewIndex<Int>) -> Int {
  a.view[i]
}

public func array_view_slice_element(a: [Int] , sliceIdx: FakeRange<BufferViewIndex<Int>>, Idx: BufferViewIndex<Int>) -> Int {
  a.view[sliceIdx][Idx]
}
