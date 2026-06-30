//===--- SequenceUtilities.swift ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Basic

/// Types conforming to `HasShortDescription` will be displayed by their name (instead
/// of the full object) in collection descriptions.
///
/// This is useful to make collections, e.g. of BasicBlocks or Functions, readable.
public protocol HasShortDescription {
  var shortDescription: String { get }
}

private struct CustomMirrorChild : CustomStringConvertible, NoReflectionChildren {
  public var description: String
  
  public init(description: String) { self.description = description }
}

/// Makes a Sequence's `description` and `customMirror` formatted like Array, e.g. [a, b, c].
public protocol FormattedLikeArray : Sequence, CustomStringConvertible, CustomReflectable {
}

extension FormattedLikeArray {
  /// Display a Sequence in an array like format, e.g. [a, b, c]
  public var description: String {
    "[" + map {
      if let named = $0 as? HasShortDescription {
        return named.shortDescription
      }
      return String(describing: $0)
    }.joined(separator: ", ") + "]"
  }
  
  /// The mirror which adds the children of a Sequence, similar to `Array`.
  public var customMirror: Mirror {
    // If the one-line description is not too large, print that instead of the
    // children in separate lines.
    if description.count <= 80 {
      return Mirror(self, children: [])
    }
    let c: [Mirror.Child] = map {
      let val: Any
      if let named = $0 as? HasShortDescription {
        val = CustomMirrorChild(description: named.shortDescription)
      } else {
        val = $0
      }
      return (label: nil, value: val)
    }
    return Mirror(self, children: c, displayStyle: .collection)
  }
}

/// A Sequence which is not consuming and therefore behaves like a Collection.
///
/// Many sequences in SIL and the optimizer should be collections but cannot
/// because their Index cannot conform to Comparable. Those sequences conform
/// to CollectionLikeSequence.
///
/// For convenience it also inherits from FormattedLikeArray.
public protocol CollectionLikeSequence : FormattedLikeArray {
}

public extension Sequence {
  var isEmpty: Bool { !contains(where: { _ in true }) }

  var singleElement: Element? {
    var singleElement: Element? = nil
    for e in self {
      if singleElement != nil {
        return nil
      }
      singleElement = e
    }
    return singleElement
  }

  var first: Element? { first(where: { _ in true }) }

  func countExceeds(_ n: Int) -> Bool {
    for (idx, _) in self.enumerated() {
      if idx > n {
        return true
      }
    }
    return false
  }
}

// Also make the lazy sequences a CollectionLikeSequence if the underlying sequence is one.

extension LazySequence : /*@retroactive*/ SIL.CollectionLikeSequence,
                         /*@retroactive*/ SIL.FormattedLikeArray,
                         /*@retroactive*/ Swift.CustomStringConvertible,
                         /*@retroactive*/ Swift.CustomReflectable
                         where Base: CollectionLikeSequence {}

extension FlattenSequence : /*@retroactive*/ SIL.CollectionLikeSequence,
                            /*@retroactive*/ SIL.FormattedLikeArray,
                            /*@retroactive*/ Swift.CustomStringConvertible,
                            /*@retroactive*/ Swift.CustomReflectable
                            where Base: CollectionLikeSequence {}

extension LazyMapSequence : /*@retroactive*/ SIL.CollectionLikeSequence,
                            /*@retroactive*/ SIL.FormattedLikeArray,
                            /*@retroactive*/ Swift.CustomStringConvertible,
                            /*@retroactive*/ Swift.CustomReflectable
                            where Base: CollectionLikeSequence {}

extension LazyFilterSequence : /*@retroactive*/ SIL.CollectionLikeSequence,
                               /*@retroactive*/ SIL.FormattedLikeArray,
                               /*@retroactive*/ Swift.CustomStringConvertible,
                               /*@retroactive*/ Swift.CustomReflectable
                               where Base: CollectionLikeSequence {}

//===----------------------------------------------------------------------===//
//                      Single-Element Inline Array
//===----------------------------------------------------------------------===//

public struct SingleInlineArray<Element>: RandomAccessCollection, FormattedLikeArray {
  enum Storage {
    case empty
    case singleElement(Element)
    case multipleElements([Element])
  }
  var storage: Storage

  public init() {
    self.storage = .empty
  }

  public init(element: Element) {
    self.storage = .singleElement(element)
  }

  public var startIndex: Int { 0 }
  public var endIndex: Int {
    switch storage {
      case .empty:                       return 0
      case .singleElement:               return 1
      case .multipleElements(let array): return array.count
    }
  }

  public subscript(_ index: Int) -> Element {
    _read {
      switch storage {
      case .empty:
        fatalError("index out of bounds")
      case .singleElement(let element):
        precondition(index == 0, "index out of bounds")
        yield element
      case .multipleElements(let array):
        yield array[index]
      }
    }
    _modify {
      switch storage {
      case .empty:
        fatalError("index out of bounds")
      case .singleElement(var element):
        precondition(index == 0, "index out of bounds")
        yield &element
        storage = .singleElement(element)
      case .multipleElements(var array):
        storage = .empty
        yield &array[index]
        storage = .multipleElements(array)
      }
    }
  }

  public mutating func append(_ element: __owned Element) {
    push(element)
  }

  public mutating func append<S: Sequence>(contentsOf newElements: __owned S) where S.Element == Element {
    for element in newElements {
      push(element)
    }
  }

  public mutating func push(_ newElement: __owned Element) {
    switch storage {
    case .empty:
      storage = .singleElement(newElement)
    case .singleElement(let element):
      storage = .multipleElements([element, newElement])
    case .multipleElements(var array):
      storage = .empty
      array.append(newElement)
      storage = .multipleElements(array)
    }
  }

  public mutating func popLast() -> Element? {
    switch storage {
    case .empty:
      return nil
    case .singleElement(let element):
      storage = .empty
      return element
    case .multipleElements(var array):
      storage = .empty
      let element = array.popLast()
      storage = .multipleElements(array)
      return element
    }
  }

  public mutating func sort(by areInIncreasingOrder: (Element, Element) -> Bool) {
    switch storage {
    case .empty, .singleElement:
      return
    case .multipleElements(var array):
      storage = .empty
      array.sort(by: areInIncreasingOrder)
      storage = .multipleElements(array)
    }
  }
}
