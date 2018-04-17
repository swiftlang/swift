//===-------------------- SyntaxData.swift - Syntax Data ------------------===//
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

/// A unique identifier for a node in the tree.
/// Currently, this is an index path from the current node to the root of the
/// tree. It's an implementation detail and shouldn't be
/// exposed to clients.
typealias NodeIdentifier = [Int]

/// SyntaxData is the underlying storage for each Syntax node.
/// It's modelled as an array that stores and caches a SyntaxData for each raw
/// syntax node in its layout. It is up to the specific Syntax nodes to maintain
/// the correct layout of their SyntaxData backing stores.
///
/// SyntaxData is an implementation detail, and should not be exposed to clients
/// of libSyntax.
///
/// The following relationships are preserved:
///   parent.cachedChild(at: indexInParent) === self
///   raw.layout.count == childCaches.count
///   pathToRoot.first === indexInParent
final class SyntaxData: Equatable {

  let raw: RawSyntax
  let indexInParent: Int
  weak var parent: SyntaxData?

  let childCaches: [AtomicCache<SyntaxData>]

  let positionCache: AtomicCache<AbsolutePosition>

  fileprivate func calculatePosition(_ initPos: AbsolutePosition) ->
      AbsolutePosition {
    guard let parent = parent else {
      assert(raw.isSourceFile, "cannot find SourceFileSyntax as root")
      // If this node is SourceFileSyntax, its location is the start of the file.
      return initPos
    }

    // If the node is the first child of its parent, the location is same with
    // the parent's location.
    guard indexInParent != 0 else { return parent.position }

    // Otherwise, the location is same with the previous sibling's location
    // adding the stride of the sibling.
    for idx in (0..<indexInParent).reversed() {
      if let sibling = parent.cachedChild(at: idx) {
        let pos = sibling.position.copy()
        sibling.raw.accumulateAbsolutePosition(pos)
        return pos
      }
    }
    return parent.position
  }

  var position: AbsolutePosition {
    return positionCache.value { return calculatePosition(UTF8Position()) }
  }

  var positionAfterSkippingLeadingTrivia: AbsolutePosition {
    let result = position.copy()
    _ = raw.accumulateLeadingTrivia(result)
    return result
  }

  fileprivate func getNextSiblingPos() -> AbsolutePosition {
    // If this node is root, the position of the next sibling is the end of
    // this node.
    guard let parent = parent else {
      assert(raw.isSourceFile, "cannot find SourceFileSyntax as root")
      let result = self.position.copy()
      raw.accumulateAbsolutePosition(result)
      return result
    }

    // Find the first valid sibling and return its position.
    for i in indexInParent+1..<parent.raw.layout.count {
      guard let sibling = parent.cachedChild(at: i) else { continue }
      return sibling.position
    }
    // Otherwise, use the parent's sibling instead.
    return parent.getNextSiblingPos()
  }

  var byteSize: Int {
    return getNextSiblingPos().byteOffset - self.position.byteOffset
  }

  /// Creates a SyntaxData with the provided raw syntax, pointing to the
  /// provided index, in the provided parent.
  /// - Parameters:
  ///   - raw: The raw syntax underlying this node.
  ///   - indexInParent: The index in the parent's layout where this node will
  ///                    reside.
  ///   - parent: The parent of this node, or `nil` if this node is the root.
  required init(raw: RawSyntax, indexInParent: Int = 0, 
                parent: SyntaxData? = nil) {
    self.raw = raw
    self.indexInParent = indexInParent
    self.parent = parent
    self.childCaches = raw.layout.map { _ in AtomicCache<SyntaxData>() }
    self.positionCache = AtomicCache<AbsolutePosition>()
  }

  /// The index path from this node to the root. This can be used to uniquely
  /// identify this node in the tree.
  lazy private(set) var pathToRoot: NodeIdentifier = {
    var path = [Int]()
    var node = self
    while let parent = node.parent {
      path.append(node.indexInParent)
      node = parent
    }
    return path
  }()

  /// Returns the child data at the provided index in this data's layout.
  /// This child is cached and will be used in subsequent accesses.
  /// - Note: This function traps if the index is out of the bounds of the
  ///         data's layout.
  ///
  /// - Parameter index: The index to create and cache.
  /// - Returns: The child's data at the provided index.
  func cachedChild(at index: Int) -> SyntaxData? {
    if raw.layout[index] == nil { return nil }
    return childCaches[index].value { realizeChild(index) }
  }

  /// Returns the child data at the provided cursor in this data's layout.
  /// This child is cached and will be used in subsequent accesses.
  /// - Note: This function traps if the cursor is out of the bounds of the
  ///         data's layout.
  ///
  /// - Parameter cursor: The cursor to create and cache.
  /// - Returns: The child's data at the provided cursor.
  func cachedChild<CursorType: RawRepresentable>(
    at cursor: CursorType) -> SyntaxData?
    where CursorType.RawValue == Int {
    return cachedChild(at: cursor.rawValue)
  }

  /// Walks up the provided node's parent tree looking for the receiver.
  /// - parameter data: The potential child data.
  /// - returns: `true` if the receiver exists in the parent chain of the
  ///            provided data.
  /// - seealso: isDescendent(of:)
  func isAncestor(of data: SyntaxData) -> Bool {
    return data.isDescendent(of: self)
  }

  /// Walks up the receiver's parent tree looking for the provided node.
  /// - parameter data: The potential ancestor data.
  /// - returns: `true` if the data exists in the parent chain of the receiver.
  /// - seealso: isAncestor(of:)
  func isDescendent(of data: SyntaxData) -> Bool {
    if data == self { return true }
    var node = self
    while let parent = node.parent {
      if parent == data { return true }
      node = parent
    }
    return false
  }

  /// Creates a copy of `self` and recursively creates `SyntaxData` nodes up to
  /// the root.
  /// - parameter newRaw: The new RawSyntax that will back the new `Data`
  /// - returns: A tuple of both the new root node and the new data with the raw
  ///            layout replaced.
  func replacingSelf(
    _ newRaw: RawSyntax) -> (root: SyntaxData, newValue: SyntaxData) {
    // If we have a parent already, then ask our current parent to copy itself
    // recursively up to the root.
    if let parent = parent {
      let (root, newParent) = parent.replacingChild(newRaw, at: indexInParent)
      let newMe = newParent.cachedChild(at: indexInParent)!
      return (root: root, newValue: newMe)
    } else {
      // Otherwise, we're already the root, so return the new data as both the
      // new root and the new data.
      let newMe = SyntaxData(raw: newRaw, indexInParent: indexInParent,
                             parent: nil)
      return (root: newMe, newValue: newMe)
    }
  }

  /// Creates a copy of `self` with the child at the provided index replaced
  /// with a new SyntaxData containing the raw syntax provided.
  ///
  /// - Parameters:
  ///   - child: The raw syntax for the new child to replace.
  ///   - index: The index pointing to where in the raw layout to place this
  ///            child.
  /// - Returns: The new root node created by this operation, and the new child
  ///            syntax data.
  /// - SeeAlso: replacingSelf(_:)
  func replacingChild(_ child: RawSyntax, 
    at index: Int) -> (root: SyntaxData, newValue: SyntaxData) {
    let newRaw = raw.replacingChild(index, with: child)
    return replacingSelf(newRaw)
  }

  /// Creates a copy of `self` with the child at the provided cursor replaced
  /// with a new SyntaxData containing the raw syntax provided.
  ///
  /// - Parameters:
  ///   - child: The raw syntax for the new child to replace.
  ///   - cursor: A cursor that points to the index of the child you wish to
  ///             replace
  /// - Returns: The new root node created by this operation, and the new child
  ///            syntax data.
  /// - SeeAlso: replacingSelf(_:)
  func replacingChild<CursorType: RawRepresentable>(_ child: RawSyntax, 
    at cursor: CursorType) -> (root: SyntaxData, newValue: SyntaxData)
    where CursorType.RawValue == Int {
    return replacingChild(child, at: cursor.rawValue)
  }

  /// Creates the child's syntax data for the provided cursor.
  ///
  /// - Parameter cursor: The cursor pointing into the raw syntax's layout for
  ///                     the child you're creating.
  /// - Returns: A new SyntaxData for the specific child you're
  ///            creating, whose parent is pointing to self.
  func realizeChild<CursorType: RawRepresentable>(
    _ cursor: CursorType) -> SyntaxData
      where CursorType.RawValue == Int {
    return realizeChild(cursor.rawValue)
  }

  /// Creates the child's syntax data for the provided index.
  ///
  /// - Parameter cursor: The cursor pointing into the raw syntax's layout for
  ///                     the child you're creating.
  /// - Returns: A new SyntaxData for the specific child you're
  ///            creating, whose parent is pointing to self.
  func realizeChild(_ index: Int) -> SyntaxData {
    return SyntaxData(raw: raw.layout[index]!,
                      indexInParent: index,
                      parent: self)
  }

  /// Tells whether two SyntaxData nodes have the same identity.
  /// This is not structural equality.
  /// - Returns: True if both datas are exactly the same.
  static func ==(lhs: SyntaxData, rhs: SyntaxData) -> Bool {
    return lhs === rhs
  }
}

/// An absolute position in a source file as text - the absolute byteOffset from
/// the start, line, and column.
public class AbsolutePosition {
  public fileprivate(set) var byteOffset: Int
  public fileprivate(set) var line: Int
  public fileprivate(set) var column: Int

  required public init(line: Int = 1, column: Int = 1, byteOffset: Int = 0) {
    self.line = line
    self.column = column
    self.byteOffset = byteOffset
  }

  internal func add(text: String) {
     preconditionFailure("this function must be overridden")
  }

  internal func copy() -> Self {
    return type(of: self).init(line: line, column: column, byteOffset: byteOffset)
  }
}

extension AbsolutePosition {

  /// Add some number of columns to the position.
  internal func add(columns: Int) {
    column += columns
    byteOffset += columns
  }

  /// Add some number of newlines to the position, resetting the column.
  /// Size is byte size of newline char.
  /// '\n' and '\r' are 1, '\r\n' is 2.
  internal func add(lines: Int, size: Int) {
    line += lines
    column = 1
    byteOffset += lines * size
  }

  /// Use some text as a reference for adding to the absolute position,
  /// taking note of newlines, etc.
  fileprivate func add<C: BidirectionalCollection>(text chars: C)
      where C.Element: UnsignedInteger  {
    let cr: C.Element = 13
    let nl: C.Element = 10
    var idx = chars.startIndex
    while idx != chars.endIndex {
      let c = chars[idx]
      idx = chars.index(after: idx)
      switch c {
      case cr:
        if chars[idx] == nl {
          add(lines: 1, size: 2)
          idx = chars.index(after: idx)
        } else {
          add(lines: 1, size: 1)
        }
      case nl:
        add(lines: 1, size: 1)
      default:
        add(columns: 1)
      }
    }
  }
}

class UTF8Position: AbsolutePosition {
  internal override func add(text: String) {
    add(text: text.utf8)
  }
}

class UTF16Position: AbsolutePosition {
  internal override func add(text: String) {
    add(text: text.utf16)
  }
}
