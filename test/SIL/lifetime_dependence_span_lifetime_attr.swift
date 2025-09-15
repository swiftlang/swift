// RUN: %target-swift-frontend %s -emit-sil \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN: | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Lifetimes

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

public struct SpanIndex<Element> {
  let _rawValue: UnsafeRawPointer

  internal init(rawValue: UnsafeRawPointer) {
    _rawValue = rawValue
  }

  var isAligned: Bool {
    (Int(bitPattern: _rawValue) & (MemoryLayout<Element>.alignment-1)) == 0
  }
}

extension SpanIndex: Equatable {}

extension SpanIndex: Hashable {}

extension SpanIndex: Strideable {
  public typealias Stride = Int

  public func distance(to other: SpanIndex) -> Int {
    let bytes = _rawValue.distance(to: other._rawValue)
    let (q, r) = bytes.quotientAndRemainder(dividingBy: MemoryLayout<Element>.stride)
    precondition(r == 0)
    return q
  }

  public func advanced(by n: Int) -> SpanIndex {
    .init(rawValue: _rawValue.advanced(by: n &* MemoryLayout<Element>.stride))
  }
}

extension SpanIndex: Comparable {
  public static func <(lhs: SpanIndex, rhs: SpanIndex) -> Bool {
    lhs._rawValue < rhs._rawValue
  }
}

public struct Span<Element> : ~Escapable {
  let start: SpanIndex<Element>
  public let count: Int
  private var baseAddress: UnsafeRawPointer { start._rawValue }
// CHECK-LABEL: sil @$s025lifetime_dependence_span_A5_attr4SpanV11baseAddress5count9dependsOnACyxGSV_Siqd__htcRi_d__Ri0_d__lufC : $@convention(method) <Element><Owner where Owner : ~Copyable, Owner : ~Escapable> (UnsafeRawPointer, Int, @in_guaranteed Owner, @thin Span<Element>.Type) -> @lifetime(copy 2) @owned Span<Element> {
  @_lifetime(copy owner)
  public init<Owner: ~Copyable & ~Escapable>(
      baseAddress: UnsafeRawPointer,
      count: Int,
      dependsOn owner: borrowing Owner
    ) {
      self.init(
        start: .init(rawValue: baseAddress), count: count, dependsOn: owner
      )
  }
// CHECK-LABEL: sil hidden @$s025lifetime_dependence_span_A5_attr4SpanV5start5count9dependsOnACyxGAA0E5IndexVyxG_Siqd__htcRi_d__Ri0_d__lufC : $@convention(method) <Element><Owner where Owner : ~Copyable, Owner : ~Escapable> (SpanIndex<Element>, Int, @in_guaranteed Owner, @thin Span<Element>.Type) -> @lifetime(copy 2) @owned Span<Element> {
  @_lifetime(copy owner)
  init<Owner: ~Copyable & ~Escapable>(
    start index: SpanIndex<Element>,
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
  init(start index: SpanIndex<Element>,
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

extension Span {
  public typealias Index = SpanIndex<Element>
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
 
// CHECK-LABEL: sil @$s025lifetime_dependence_span_A5_attr4SpanVyACyxGAA9FakeRangeVyAA0E5IndexVyxGGcig : $@convention(method) <Element> (FakeRange<SpanIndex<Element>>, @guaranteed Span<Element>) -> @lifetime(copy 1) @owned Span<Element> {
  public subscript(bounds: FakeRange<SpanIndex<Element>>) -> Self {
  @_lifetime(copy self)
    get {
      let span = Span(
        start: bounds.lowerBound,
        count: bounds.upperBound.distance(to:bounds.lowerBound) / MemoryLayout<Element>.stride
      )
      return _overrideLifetime(span, copying: self)
    }
  }

// CHECK-LABEL: sil @$s025lifetime_dependence_span_A5_attr4SpanV6prefix4upToACyxGAA0E5IndexVyxG_tF : $@convention(method) <Element> (SpanIndex<Element>, @guaranteed Span<Element>) -> @lifetime(copy 1) @owned Span<Element> {
  @_lifetime(copy self)
  borrowing public func prefix(upTo index: SpanIndex<Element>) -> Self {
    index == startIndex
    ? Self(start: start, count: 0, dependsOn: copy self)
    : prefix(through: index.advanced(by: -1))
  }

// CHECK-LABEL: sil @$s025lifetime_dependence_span_A5_attr4SpanV6prefix7throughACyxGAA0E5IndexVyxG_tF : $@convention(method) <Element> (SpanIndex<Element>, @guaranteed Span<Element>) -> @lifetime(copy 1) @owned Span<Element> {
  @_lifetime(copy self)
  borrowing public func prefix(through index: Index) -> Self {
    let nc = distance(from: startIndex, to: index) &+ 1
    return Self(start: start, count: nc, dependsOn: copy self)
  }

// CHECK-LABEL: sil @$s025lifetime_dependence_span_A5_attr4SpanV6prefixyACyxGSiF : $@convention(method) <Element> (Int, @owned Span<Element>) -> @lifetime(copy 1) @owned Span<Element> {
  @_lifetime(copy self)
  consuming public func prefix(_ maxLength: Int) -> Self {
    precondition(maxLength >= 0, "Can't have a prefix of negative length.")
    let nc = maxLength < count ? maxLength : count
    return Self(start: start, count: nc, dependsOn: self)
  }
}

extension ContiguousArray {
  public var view: Span<Element> {
    @_lifetime(borrow self)
    borrowing _read {
      yield Span(
        baseAddress: _baseAddressIfContiguous!, count: count, dependsOn: self
      )
    }
  }
}

public func array_view_element(a: ContiguousArray<Int> , i: SpanIndex<Int>) -> Int {
  a.view[i]
}

public func array_view_slice_element(a: ContiguousArray<Int> , sliceIdx: FakeRange<SpanIndex<Int>>, Idx: SpanIndex<Int>) -> Int {
  a.view[sliceIdx][Idx]
}

