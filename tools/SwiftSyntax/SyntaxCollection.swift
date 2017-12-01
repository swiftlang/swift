//===-------------- SyntaxCollection.swift - Syntax Collection ------------===//
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

import Foundation

/// Represents a collection of Syntax nodes of a specific type. SyntaxCollection
/// behaves as a regular Swift collection, and has accessors that return new
/// versions of the collection with different children.
public class SyntaxCollection<SyntaxElement: Syntax>: Syntax {
  /// Creates a new SyntaxCollection by replacing the underlying layout with
  /// a different set of raw syntax nodes.
  ///
  /// - Parameter layout: The new list of raw syntax nodes underlying this
  ///                     collection.
  /// - Returns: A new SyntaxCollection with the new layout underlying it.
  internal func replacingLayout(
    _ layout: [RawSyntax]) -> SyntaxCollection<SyntaxElement> {
    let newRaw = data.raw.replacingLayout(layout)
    let (newRoot, newData) = data.replacingSelf(newRaw)
    return SyntaxCollection<SyntaxElement>(root: newRoot, data: newData)
  }

  /// Creates a new SyntaxCollection by appending the provided syntax element
  /// to the children.
  ///
  /// - Parameter syntax: The element to append.
  /// - Returns: A new SyntaxCollection with that element appended to the end.
  public func appending(
    _ syntax: SyntaxElement) -> SyntaxCollection<SyntaxElement> {
    var newLayout = data.raw.layout
    newLayout.append(syntax.raw)
    return replacingLayout(newLayout)
  }

  /// Creates a new SyntaxCollection by prepending the provided syntax element
  /// to the children.
  ///
  /// - Parameter syntax: The element to prepend.
  /// - Returns: A new SyntaxCollection with that element prepended to the
  ///            beginning.
  public func prepending(
    _ syntax: SyntaxElement) -> SyntaxCollection<SyntaxElement> {
    return inserting(syntax, at: 0)
  }

  /// Creates a new SyntaxCollection by inserting the provided syntax element
  /// at the provided index in the children.
  ///
  /// - Parameters:
  ///   - syntax: The element to insert.
  ///   - index: The index at which to insert the element in the collection.
  ///
  /// - Returns: A new SyntaxCollection with that element appended to the end.
  public func inserting(_ syntax: SyntaxElement,
                        at index: Int) -> SyntaxCollection<SyntaxElement> {
    var newLayout = data.raw.layout
    /// Make sure the index is a valid insertion index (0 to 1 past the end)
    precondition((newLayout.startIndex...newLayout.endIndex).contains(index),
                 "inserting node at invalid index \(index)")
    newLayout.insert(syntax.raw, at: index)
    return replacingLayout(newLayout)
  }

  /// Creates a new SyntaxCollection by removing the syntax element at the
  /// provided index.
  ///
  /// - Parameter index: The index of the element to remove from the collection.
  /// - Returns: A new SyntaxCollection with the element at the provided index
  ///            removed.
  public func removing(childAt index: Int) -> SyntaxCollection<SyntaxElement> {
    var newLayout = data.raw.layout
    newLayout.remove(at: index)
    return replacingLayout(newLayout)
  }

  /// Creates a new SyntaxCollection by removing the first element.
  ///
  /// - Returns: A new SyntaxCollection with the first element removed.
  public func removingFirst() -> SyntaxCollection<SyntaxElement> {
    var newLayout = data.raw.layout
    newLayout.removeFirst()
    return replacingLayout(newLayout)
  }

  /// Creates a new SyntaxCollection by removing the last element.
  ///
  /// - Returns: A new SyntaxCollection with the last element removed.
  public func removingLast() -> SyntaxCollection<SyntaxElement> {
    var newLayout = data.raw.layout
    newLayout.removeLast()
    return replacingLayout(newLayout)
  }

  /// Returns an iterator over the elements of this syntax collection.
  public func makeIterator() -> SyntaxCollectionIterator<SyntaxElement> {
    return SyntaxCollectionIterator(collection: self)
  }
}

/// Conformance for SyntaxCollection to the Collection protocol.
extension SyntaxCollection: Collection {
  public var startIndex: Int {
    return data.childCaches.startIndex
  }
  
  public var endIndex: Int {
    return data.childCaches.endIndex
  }
  
  public func index(after i: Int) -> Int {
    return data.childCaches.index(after: i)
  }
  
  public subscript(_ index: Int) -> SyntaxElement {
    return child(at: index)! as! SyntaxElement
  }
}

/// A type that iterates over a syntax collection using its indices.
public struct SyntaxCollectionIterator<Element: Syntax>: IteratorProtocol {
  private let collection: SyntaxCollection<Element>
  private var index: SyntaxCollection<Element>.Index

  fileprivate init(collection: SyntaxCollection<Element>) {
    self.collection = collection
    self.index = collection.startIndex
  }

  public mutating func next() -> Element? {
    guard
      !(self.collection.isEmpty || self.index == self.collection.endIndex)
    else {
      return nil
    }

    let result = collection[index]
    collection.formIndex(after: &index)
    return result
  }
}

