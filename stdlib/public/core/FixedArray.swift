//===--- FixedArray.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A collection containing a single element of type `Element`.
///
/// Like CollectionOfOne but bypasses all the error checking we can find
public struct _FastCollectionOfOne<Element>
  : MutableCollection, RandomAccessCollection {

  /// Creates an instance containing just `element`.
  public init(_ element: Element) {
    self._element = element
  }

  public typealias Index = Int

  /// The position of the first element.
  public var startIndex: Int {
    return 0
  }

  /// The "past the end" position---that is, the position one greater than the
  /// last valid subscript argument.
  ///
  /// In a `CollectionOfOne` instance, `endIndex` is always identical to
  /// `index(after: startIndex)`.
  public var endIndex: Int {
    return 1
  }
  
  /// Always returns `endIndex`.
  @inline(__always)
  public func index(after i: Int) -> Int {
    return endIndex
  }

  /// Always returns `startIndex`.
  @inline(__always)
  public func index(before i: Int) -> Int {
    return startIndex
  }

  public typealias Indices = CountableRange<Int>

  /// Returns an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  @inline(__always)
  public func makeIterator() -> IteratorOverOne<Element> {
    return IteratorOverOne(_elements: _element)
  }

  /// Accesses the element at `position`.
  ///
  /// - Precondition: `position == 0`.
  public subscript(position: Int) -> Element {
    @inline(__always)
    get {
      return _element
    }
    @inline(__always)
    set {
      _element = newValue
    }
  }

  public subscript(bounds: Range<Int>)
    -> MutableRandomAccessSlice<_FastCollectionOfOne<Element>> {
    get {
      return MutableRandomAccessSlice<_FastCollectionOfOne<Element>>(
        base: self, bounds: bounds)
    }
    set {
      if let newElement = newValue.first {
        _element = newElement
      }
    }
  }

  /// The number of elements (always one).
  public var count: Int {
    @inline(__always)
    get {
      return 1
    }
  }

  @inline(__always)
  func index(i: Int, offsetBy offset: Int) -> Int {
    return i &+ offset
  }
  
  public var _element: Element
}

public protocol _FixedSizeCollection : Collection {
  init(repeating: Iterator.Element)
  
/*  init<C: Collection>(_ x: C)
  where C.Iterator.Element == Iterator.Element,
  C.SubSequence.Iterator.Element == Iterator.Element
  */
}

/// A concatenation of B0 and B1 into a single Collection
public struct _Joined<B0 : MutableCollection, B1: MutableCollection>
  : MutableCollection, RandomAccessCollection, _FixedSizeCollection
where B0.IndexDistance == Int, 
  B1.IndexDistance == Int,
  B0.Iterator.Element == B1.Iterator.Element,
  B0 : _FixedSizeCollection & RandomAccessCollection, 
  B1 : _FixedSizeCollection & RandomAccessCollection
{
  var b0: B0, b1: B1

  public typealias Indices = CountableRange<Int>
  public typealias Element = B0.Iterator.Element
  
  public var startIndex : Int { return 0 }
  public var endIndex : Int { return b0.count &+ b1.count }

  public subscript(i: Int) -> B0.Iterator.Element {
    get {
      var s = self
      let d = distance(from: startIndex, to: i)
      let c = numericCast(count) as Int
      let i2 = Int(truncatingBitPattern: d.toIntMax())
      
      return withUnsafePointer(to: &s) {
        $0.withMemoryRebound(to: Element.self, capacity: c) {
          $0[i2]
        }
      }
    }
    set {
      let d = distance(from: startIndex, to: i)
      let c = numericCast(count) as Int
      let i2 = Int(truncatingBitPattern: d.toIntMax())
      withUnsafeMutablePointer(to: &self) {
        $0.withMemoryRebound(to: Element.self, capacity: c) {
          $0[i2] = newValue
        }
      }
    }
  }

  public subscript(r: Range<Int>) -> MutableRandomAccessSlice<_Joined> {
    get {
      return MutableRandomAccessSlice(
        base: self, bounds: r)
    }
    set {
      for (i, x) in zip(CountableRange(r), newValue) {
        self[i] = x
      }
    }
  }
  
  public func index(after i: Int) -> Int {
    return i &+ 1
  }
  public func index(before i: Int) -> Int {
    return i &- 1
  }
  public func index(i: Int, offsetBy offset: Int) -> Int {
    return i &+ offset
  }
  public init(repeating x: B0.Iterator.Element) {
    b0 = B0(repeating: x)
    b1 = B1(repeating: x)
  }
}

extension _FastCollectionOfOne : _FixedSizeCollection {
  public init(repeating x: Element) {
    self = _FastCollectionOfOne(x)
  }
}

extension CollectionOfOne : _FixedSizeCollection {
  public init(repeating x: Element) {
    self = CollectionOfOne(x)
  }
}

public typealias _DoubleLength<
  C : MutableCollection & RandomAccessCollection & _FixedSizeCollection
> = _Joined<C,C> where C.IndexDistance == Int
public typealias _Array2<T> = _DoubleLength<_FastCollectionOfOne<T>>
public typealias _Array4<T> = _DoubleLength<_Array2<T>>
public typealias _Array8<T> = _DoubleLength<_Array4<T>>
public typealias _Array16<T> = _DoubleLength<_Array8<T>>
public typealias _Array32<T> = _DoubleLength<_Array16<T>>
