// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -enable-experimental-feature NonescapableTypes \
// RUN:   -enable-experimental-feature NoncopyableGenerics

// REQUIRES: asserts
// REQUIRES: swift_in_compiler

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
// TODO: Enable diagnostics once this initializer's store to temporary is handled  
// CHECK: sil @$s31lifetime_dependence_scope_fixup10BufferViewV11baseAddress5count9dependsOnACyxGSVYls_Siqd__htclufC : $@convention(method) <Element><Owner> (UnsafeRawPointer, Int, @in_guaranteed Owner, @thin BufferView<Element>.Type) -> _scope(1) @owned BufferView<Element> {
  public init<Owner: ~Copyable & ~Escapable>(
      baseAddress: UnsafeRawPointer,
      count: Int,
      dependsOn owner: borrowing Owner
    ) {
      self.init(
        start: .init(rawValue: baseAddress), count: count, dependsOn: owner
      )
  }
// CHECK: sil hidden @$s31lifetime_dependence_scope_fixup10BufferViewV5start5count9dependsOnACyxGAA0eF5IndexVyxGYls_Siqd__htclufC : $@convention(method) <Element><Owner> (BufferViewIndex<Element>, Int, @in_guaranteed Owner, @thin BufferView<Element>.Type) -> _scope(1) @owned BufferView<Element> {
  init<Owner: ~Copyable & ~Escapable>(
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

  @inlinable @inline(__always) 
  public func distance(from start: Index, to end: Index) -> Int {
    start.distance(to: end)
  }
 
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
 
// CHECK: sil @$s31lifetime_dependence_scope_fixup10BufferViewVyACyxGAA9FakeRangeVyAA0eF5IndexVyxGGcig : $@convention(method) <Element> (FakeRange<BufferViewIndex<Element>>, @guaranteed BufferView<Element>) -> _scope(0) @owned BufferView<Element> {
  public subscript(bounds: FakeRange<BufferViewIndex<Element>>) -> Self {
    get {
      BufferView(
        start: bounds.lowerBound,
        count: bounds.upperBound.distance(to:bounds.lowerBound) / MemoryLayout<Element>.stride
      )
    }
  }

  borrowing public func prefix(upTo index: BufferViewIndex<Element>) -> dependsOn(self) Self {
    index == startIndex
    ? Self(start: start, count: 0, dependsOn: copy self)
    : prefix(through: index.advanced(by: -1))
  }

  borrowing public func prefix(through index: Index) -> dependsOn(self) Self {
    let nc = distance(from: startIndex, to: index) &+ 1
    return Self(start: start, count: nc, dependsOn: copy self)
  }

  consuming public func prefix(_ maxLength: Int) -> dependsOn(self) Self {
    precondition(maxLength >= 0, "Can't have a prefix of negative length.")
    let nc = maxLength < count ? maxLength : count
    return Self(start: start, count: nc, dependsOn: self)
  }
}

extension ContiguousArray {
  public var view: BufferView<Element> {
    borrowing _read {
      yield BufferView(
        baseAddress: _baseAddressIfContiguous!, count: count, dependsOn: self
      )
    }
  }
}

public func array_view_element(a: ContiguousArray<Int> , i: BufferViewIndex<Int>) -> Int {
  a.view[i]
}

public func array_view_slice_element(a: ContiguousArray<Int> , sliceIdx: FakeRange<BufferViewIndex<Int>>, Idx: BufferViewIndex<Int>) -> Int {
  a.view[sliceIdx][Idx]
}
