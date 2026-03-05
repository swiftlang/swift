//===--- BorrowingSequence.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-stdlib-swift(-enable-experimental-feature SuppressedAssociatedTypesWithDefaults -enable-experimental-feature BorrowInout -enable-experimental-feature BorrowingSequence -enable-experimental-feature Lifetimes -Xfrontend -disable-availability-checking -enable-experimental-feature AddressableParameters -enable-experimental-feature AddressableTypes)

// REQUIRES: executable_test
// REQUIRES: swift_feature_SuppressedAssociatedTypesWithDefaults
// REQUIRES: swift_feature_BorrowingSequence

import StdlibUnittest

var suite = TestSuite("BorrowingZequence Prototype")
defer { runAllTests() }

//===------------------------------------------------------------===//
// Protocols
//===------------------------------------------------------------===//

public protocol ConsumingZequence<Element>: ~Copyable {
  associatedtype Element: ~Copyable
  associatedtype Yterator: YteratorProtocol<Element> & ~Copyable

  consuming func makeYterator() -> Yterator

  // Other things like 'estimatedCount', 'isEmpty', etc.
}

public protocol BorrowingZequence<BorrowedElement>: ~Copyable, ~Escapable {
  associatedtype BorrowedElement: ~Copyable & ~Escapable
  associatedtype BorrowingYterator: YteratorProtocol<BorrowedElement> & ~Copyable & ~Escapable

  @_lifetime(borrow self)
  /*borrowing*/ func makeBorrowingYterator() -> BorrowingYterator

  // Other things like 'estimatedCount', 'isEmpty', etc.
}

public protocol MutatingZequence<MutableElement>: ~Copyable, ~Escapable {
  associatedtype MutableElement: ~Copyable & ~Escapable
  associatedtype MutatingYterator: YteratorProtocol<MutableElement> & ~Copyable & ~Escapable

  @_lifetime(&self)
  mutating func makeMutatingYterator() -> MutatingYterator

  // Other things like 'estimatedCount', 'isEmpty', etc.
}

public protocol YteratorProtocol<Element>: ~Copyable & ~Escapable {
  associatedtype Element: ~Copyable & ~Escapable

  @_lifetime(copy self)
  mutating func next() -> Element?
}

extension Span where Element: ~Copyable {
  @_lifetime(copy self)
  public func borrowElement(at index: Index) -> Borrow<Element> {
    _checkIndex(index)
    let address = unsafe _unsafeAddressOfElement(unchecked: index)
    let borrow = unsafe Borrow<Element>(unsafeAddress: address, borrowing: ())
    return unsafe _overrideLifetime(borrow, copying: self)
  }
}

extension MutableSpan where Element: ~Copyable {
  @_lifetime(&self)
  public mutating func mutateElement(at index: Index) -> Inout<Element> {
    _checkIndex(index)
    let address = unsafe _unsafeAddressOfElement(unchecked: index)
    var void = ()
    let io = unsafe Inout<Element>(unsafeAddress: address, mutating: &void)
    return unsafe _overrideLifetime(io, mutating: &self)
  }
}

struct SpanYterator<Element>: YteratorProtocol, ~Copyable, ~Escapable where Element: ~Copyable {
  var span: Span<Element>
  var index = 0
  
  @_lifetime(copy span)
  init(_ span: Span<Element>) {
    self.span = span
  }
  
  @_lifetime(copy self)
  mutating func next() -> Borrow<Element>? {
    guard index < span.count else { return nil }
    defer { index &+= 1 }
    let b = span.borrowElement(at: index)
    return unsafe _overrideLifetime(b, copying: self)
  }
}

struct MutableSpanYterator<Element>: YteratorProtocol, ~Copyable, ~Escapable where Element: ~Copyable {
  var span: MutableSpan<Element>
  var index = 0
  
  @_lifetime(&span)
  init(_ span: inout MutableSpan<Element>) {
    self.span = span
  }
  
  @_lifetime(copy self)
  mutating func next() -> Inout<Element>? {
    guard index < span.count else { return nil }
    defer { index &+= 1 }
    let io = span.mutateElement(at: index)
    return unsafe _overrideLifetime(io, mutating: &self)
  }
}

extension Span: BorrowingZequence where Element: ~Copyable {
  @_lifetime(borrow self)
  public func makeBorrowingYterator() -> SpanYterator<Element> {
    SpanYterator(self)
  }
}

//extension Span where Element: ~Copyable {
//  @_lifetime(borrow self)
//  func lifetimeBorrowingGet() -> Span {
//    self
//  }
//  
//  borrowing func keywordBorrowingGet() -> Span {
//    self // error: 'self' is borrowed and cannot be consumed
//  }
//  
//  @_lifetime(borrow self)
//  borrowing func doubleBorrowingGet() -> Span {
//    self // error: 'self' is borrowed and cannot be consumed
//  }
//}

//===------------------------------------------------------------===//
// UniqueArray conformances
//===------------------------------------------------------------===//

public struct UniqueArray<Element: ~Copyable>: ~Copyable {
  // ...

  var span: Span<Element> {
    fatalError()
  }
}

extension UniqueArray: BorrowingZequence where Element: ~Copyable {
  @_lifetime(borrow self)
  public borrowing func makeBorrowingYterator() -> SpanYterator<Element> {
    
    SpanYterator(span)
  }
}

struct UniqueArrayIterator<Element>: YteratorProtocol & ~Copyable & ~Escapable where Element: ~Copyable {
  var array: Borrow<UniqueArray<Element>>
}

//===------------------------------------------------------------===//
// UniqueDictonary conformance
//===------------------------------------------------------------===//

public struct UniqueDictionary<Key: ~Copyable, Value: ~Copyable>: ~Copyable {
  // ...
}

extension UniqueDictionary where Key: ~Copyable, Value: ~Copyable {
  public struct BorrowingYterator: ~Escapable {
    let dict: Borrow<UniqueDictionary>

    @_lifetime(borrow dict)
    init(_ dict: borrowing @_addressable UniqueDictionary) {
      self.dict = Borrow(dict)
    }
  }
}

extension UniqueDictionary.BorrowingYterator: YteratorProtocol where Key: ~Copyable, Value: ~Copyable {
  @_lifetime(copy self)
  public mutating func next() -> (Borrow<Key>, Borrow<Value>)? {
    fatalError()
  }
}

//extension UniqueDictionary: BorrowingZequence where Key: ~Copyable, Value: ~Copyable {
//  @_lifetime(borrow self)
//  public borrowing func makeBorrowingYterator() -> BorrowingYterator {
//    return BorrowingYterator(self)
//  }
//}

//===------------------------------------------------------------===//
// Lazy map conformance
//===------------------------------------------------------------===//

public struct LazyMapIter<
  Iter: YteratorProtocol & ~Copyable & ~Escapable,
  Element: ~Copyable & ~Escapable
>: ~Copyable, ~Escapable {
  var iter: Iter
  let fn: (consuming Iter.Element) -> Element

  @_lifetime(copy iter)
  init(_ iter: consuming Iter, _ fn: @escaping (consuming Iter.Element) -> Element) {
    self.iter = iter
    self.fn = fn
  }
}

extension LazyMapIter: Copyable where Iter: Copyable & ~Escapable, Element: ~Copyable & ~Escapable {}
extension LazyMapIter: Escapable where Iter: Escapable & ~Copyable, Element: ~Copyable & ~Escapable {}

// FIXME: Remove element escapable conformance
extension LazyMapIter: YteratorProtocol where Iter: ~Copyable & ~Escapable, Iter.Element: Escapable {
  public mutating func next() -> Element? {
    // FIXME: '_consumingMap' requires escapable result
    iter.next()._consumingMap(fn)
  }
}

extension YteratorProtocol where Self: ~Copyable & ~Escapable {
  @_lifetime(copy self)
  public consuming func map<NewElement>(
    _ fn: @escaping (consuming Element) -> NewElement
  ) -> LazyMapIter<Self, NewElement> {
    LazyMapIter(self, fn)
  }
}








@available(SwiftStdlib 6.4, *)
extension YteratorProtocol where Self: ~Copyable & ~Escapable, Element: ~Copyable & ~Escapable {
  consuming func reduce<T: ~Copyable>(
    _ initial: consuming T,
    _ nextPartialResult: @escaping (consuming T, borrowing Element) -> T
  ) -> T {
    var result = initial
    while let el = next() {
      result = nextPartialResult(result, el)
    }
    return result
  }
  
  consuming func reduce<T: ~Copyable>(
    into initial: consuming T,
    _ nextPartialResult: (inout T, borrowing Element) -> Void
  ) -> T {
    var result = initial
    while let el = next() {
      nextPartialResult(&result, el)
    }
    return result
  }
}
//
//@available(SwiftStdlib 6.4, *)
//extension BorrowingZequence where Self: ~Copyable & ~Escapable, Element: Copyable {
//  func collectViaBorrowing() -> [Element] {
//    var iter = makeBorrowingYterator()
//    var result: [Element] = []
//    while let borrow = iter.next() {
//      result.append(borrow.value)
//      }
//    }
//    return result
//  }
//}
//
//struct NoncopyableInt: ~Copyable, Equatable {
//  var value: Int
//
//  static func ==(lhs: borrowing Self, rhs: borrowing Self) -> Bool {
//    lhs.value == rhs.value
//  }
//}
//
//@available(SwiftStdlib 6.4, *)
//extension BorrowingZequence where Self: ~Escapable & ~Copyable, Element: Equatable & ~Copyable {
//  func elementsEqual<S: BorrowingZequence<Element>>(
//    _ rhs: borrowing S
//  ) -> Bool
//    where S: ~Escapable & ~Copyable, S.Element: ~Copyable
//  {
//    var iter1 = makeBorrowingYterator()
//    var iter2 = rhs.makeBorrowingYterator()
//    
//    while let lhs = iter1.next() {
//      guard let rhs = iter2.next() else {
//        return false // iter2 ended first
//      }
//      guard lhs == rhs else { return false }
//    }
//    
//    guard iter2.next() == nil else {
//      return false // iter1 ended first
//    }
//    
//    return true
//  }
//}
//
//suite.test("BORROWING")
//.require(.stdlib_6_4).code {
//  guard #available(SwiftStdlib 6.4, *) else {
//    expectTrue(false)
//    return
//  }
//
//  let array = [1, 2, 3, 4, 5, 6, 7, 8]
//
//  let span = array.span
//  let spanCollected = span.collectViaBorrowing()
//  expectTrue(span.elementsEqual(span))
////  expectTrue(span.elementsEqual(spanCollected))
////  expectTrue(spanCollected.elementsEqual(span))
//  expectTrue(spanCollected.elementsEqual(spanCollected))
//  expectEqual(array.reduce(0, +), span.reduce(0, +))
//  expectEqual(array.reduce(into: 0, +=), span.reduce(into: 0, +=))
//
//  let inline: [8 of Int] = [1, 2, 3, 4, 5, 6, 7, 8]
//  let inlineCollected = inline.collectViaBorrowing()
//  expectTrue(inline.elementsEqual(inline))
////  expectTrue(inline.elementsEqual(inlineCollected))
////  expectTrue(inlineCollected.elementsEqual(inline))
//  expectTrue(inlineCollected.elementsEqual(inlineCollected))
//
//  let nocopyInline: [8 of NoncopyableInt] = InlineArray(NoncopyableInt.init(value:))
//  let nocopyBuffer = UnsafeMutableBufferPointer<NoncopyableInt>.allocate(capacity: 8)
//  for i in 0..<8 {
//    nocopyBuffer.initializeElement(at: i, to: NoncopyableInt(value: i))
//  }
//  expectTrue(nocopyInline.elementsEqual(nocopyInline))
////  expectTrue(nocopyInline.elementsEqual(nocopyBuffer))
////  expectTrue(nocopyBuffer.elementsEqual(nocopyBuffer))
////  expectTrue(nocopyBuffer.elementsEqual(nocopyInline))
//}
//
//
//
//




