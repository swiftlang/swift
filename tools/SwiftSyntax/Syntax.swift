//===-------------------- Syntax.swift - Syntax Protocol ------------------===//
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

/// A Syntax node represents a tree of nodes with tokens at the leaves.
/// Each node has accessors for its known children, and allows efficient
/// iteration over the children through its `children` property.
public protocol Syntax: 
  CustomStringConvertible, TextOutputStreamable {}

internal protocol _SyntaxBase: Syntax {
  /// The type of sequence containing the indices of present children.
  typealias PresentChildIndicesSequence =
    LazyFilterSequence<Range<Int>>
    
  /// The root of the tree this node is currently in.
  var _root: SyntaxData { get } // Must be of type SyntaxData

  /// The data backing this node.
  /// - note: This is unowned, because the reference to the root data keeps it
  ///         alive. This means there is an implicit relationship -- the data
  ///         property must be a descendent of the root. This relationship must
  ///         be preserved in all circumstances where Syntax nodes are created.
  var _data: SyntaxData { get }

#if DEBUG
  func validate()
#endif
}
extension _SyntaxBase {
  public func validate() {
    // This is for implementers to override to perform structural validation.
  }
}

extension Syntax {
  var data: SyntaxData {
    guard let base = self as? _SyntaxBase else {
      fatalError("only first-class syntax nodes can conform to Syntax")
    }
    return base._data
  }

  var _root: SyntaxData {
    guard let base = self as? _SyntaxBase else {
      fatalError("only first-class syntax nodes can conform to Syntax")
    }
    return base._root
  }

  /// Access the raw syntax assuming the node is a Syntax.
  var raw: RawSyntax {
    return data.raw
  }

  /// An iterator over children of this node.
  public var children: SyntaxChildren {
    return SyntaxChildren(node: self)
  }

  /// The number of children, `present` or `missing`, in this node.
  /// This value can be used safely with `child(at:)`.
  public var numberOfChildren: Int {
    return data.childCaches.count
  }

  /// Whether or not this node is marked as `present`.
  public var isPresent: Bool {
    return raw.isPresent
  }

  /// Whether or not this node is marked as `missing`.
  public var isMissing: Bool {
    return raw.isMissing
  }

  /// Whether or not this node represents an Expression.
  public var isExpr: Bool {
    return raw.kind.isExpr
  }

  /// Whether or not this node represents a Declaration.
  public var isDecl: Bool {
    return raw.kind.isDecl
  }

  /// Whether or not this node represents a Statement.
  public var isStmt: Bool {
    return raw.kind.isStmt
  }

  /// Whether or not this node represents a Type.
  public var isType: Bool {
    return raw.kind.isType
  }

  /// Whether or not this node represents a Pattern.
  public var isPattern: Bool {
    return raw.kind.isPattern
  }

  /// The parent of this syntax node, or `nil` if this node is the root.
  public var parent: Syntax? {
    guard let parentData = data.parent else { return nil }
    return makeSyntax(root: _root, data: parentData)
  }

  /// The index of this node in the parent's children.
  public var indexInParent: Int {
    return data.indexInParent
  }

  /// The absolute position of the starting point of this node. If the first token
  /// is with leading trivia, the position points to the start of the leading
  /// trivia.
  public var position: AbsolutePosition {
    return data.position
  }

  /// The absolute position of the starting point of this node, skipping any
  /// leading trivia attached to the first token syntax.
  public var positionAfterSkippingLeadingTrivia: AbsolutePosition {
    return data.positionAfterSkippingLeadingTrivia
  }

  /// The textual byte length of this node including leading and trailing trivia.
  public var byteSize: Int {
    return data.byteSize
  }

  /// The leading trivia of this syntax node. Leading trivia is attached to
  /// the first token syntax contained by this node. Without such token, this
  /// property will return nil.
  public var leadingTrivia: Trivia? {
    return raw.leadingTrivia
  }

  /// The trailing trivia of this syntax node. Trailing trivia is attached to
  /// the last token syntax contained by this node. Without such token, this
  /// property will return nil.
  public var trailingTrivia: Trivia? {
    return raw.trailingTrivia
  }

  /// When isImplicit is true, the syntax node doesn't include any
  /// underlying tokens, e.g. an empty CodeBlockItemList.
  public var isImplicit: Bool {
    return leadingTrivia == nil
  }

  /// The textual byte length of this node exluding leading and trailing trivia.
  public var byteSizeAfterTrimmingTrivia: Int {
    return data.byteSize - (leadingTrivia?.byteSize ?? 0) -
      (trailingTrivia?.byteSize ?? 0)
  }

  /// The root of the tree in which this node resides.
  public var root: Syntax {
    return makeSyntax(root: _root,  data: _root)
  }

  /// The sequence of indices that correspond to child nodes that are not
  /// missing.
  ///
  /// This property is an implementation detail of `SyntaxChildren`.
  internal var presentChildIndices: _SyntaxBase.PresentChildIndicesSequence {
    return raw.layout.indices.lazy.filter {
      self.raw.layout[$0]?.isPresent == true
    }
  }

  /// Gets the child at the provided index in this node's children.
  /// - Parameter index: The index of the child node you're looking for.
  /// - Returns: A Syntax node for the provided child, or `nil` if there
  ///            is not a child at that index in the node.
  public func child(at index: Int) -> Syntax? {
    guard raw.layout.indices.contains(index) else { return nil }
    guard let childData = data.cachedChild(at: index) else { return nil }
    return makeSyntax(root: _root, data: childData)
  }

  /// A source-accurate description of this node.
  public var description: String {
    var s = ""
    self.write(to: &s)
    return s
  }
  
  /// Prints the raw value of this node to the provided stream.
  /// - Parameter stream: The stream to which to print the raw tree.
  public func write<Target>(to target: inout Target)
    where Target: TextOutputStream {
    data.raw.write(to: &target)
  }

  /// The starting location, in the provided file, of this Syntax node.
  /// - Parameters:
  ///   - file: The file URL this node resides in.
  ///   - afterLeadingTrivia: Whether to skip leading trivia when getting
  ///                         the node's location. Defaults to `true`.
  public func startLocation(
    in file: URL,
    afterLeadingTrivia: Bool = true
  ) -> SourceLocation {
    let pos = afterLeadingTrivia ? 
      data.position.copy() :
      data.positionAfterSkippingLeadingTrivia.copy()
    return SourceLocation(file: file.path, position: pos)
  }


  /// The ending location, in the provided file, of this Syntax node.
  /// - Parameters:
  ///   - file: The file URL this node resides in.
  ///   - afterTrailingTrivia: Whether to skip trailing trivia when getting
  ///                          the node's location. Defaults to `false`.
  public func endLocation(
    in file: URL,
    afterTrailingTrivia: Bool = false
  ) -> SourceLocation {
    let pos = data.position.copy()
    raw.accumulateAbsolutePosition(pos)
    if afterTrailingTrivia {
      raw.accumulateTrailingTrivia(pos)
    }
    return SourceLocation(file: file.path, position: pos)
  }

  /// The source range, in the provided file, of this Syntax node.
  /// - Parameters:
  ///   - file: The file URL this node resides in.
  ///   - afterLeadingTrivia: Whether to skip leading trivia when getting
  ///                          the node's start location. Defaults to `true`.
  ///   - afterTrailingTrivia: Whether to skip trailing trivia when getting
  ///                          the node's end location. Defaults to `false`.
  public func sourceRange(
    in file: URL,
    afterLeadingTrivia: Bool = true,
    afterTrailingTrivia: Bool = false
  ) -> SourceRange {
    let start = startLocation(in: file, afterLeadingTrivia: afterLeadingTrivia)
    let end = endLocation(in: file, afterTrailingTrivia: afterTrailingTrivia)
    return SourceRange(start: start, end: end)
  }
}

/// Determines if two nodes are equal to each other.
public func ==(lhs: Syntax, rhs: Syntax) -> Bool {
  return lhs.data === rhs.data
}

/// MARK: - Nodes

/// A Syntax node representing a single token.
public struct TokenSyntax: _SyntaxBase, Hashable {
  let _root: SyntaxData
  unowned let _data: SyntaxData 

  /// Creates a Syntax node from the provided root and data.
  internal init(root: SyntaxData, data: SyntaxData) {
    self._root = root
    self._data = data
#if DEBUG
    validate()
#endif
  }

  /// The text of the token as written in the source code.
  public var text: String {
    return tokenKind.text
  }
  
  /// Returns a new TokenSyntax with its kind replaced
  /// by the provided token kind.
  public func withKind(_ tokenKind: TokenKind) -> TokenSyntax {
    guard raw.kind == .token else {
      fatalError("TokenSyntax must have token as its raw")
    }
    let newRaw = RawSyntax(kind: tokenKind, leadingTrivia: raw.leadingTrivia!,
                           trailingTrivia: raw.trailingTrivia!,
                           presence: raw.presence)
    let (root, newData) = data.replacingSelf(newRaw)
    return TokenSyntax(root: root, data: newData)
  }

  /// Returns a new TokenSyntax with its leading trivia replaced
  /// by the provided trivia.
  public func withLeadingTrivia(_ leadingTrivia: Trivia) -> TokenSyntax {
    guard raw.kind == .token else {
      fatalError("TokenSyntax must have token as its raw")
    }
    let newRaw = RawSyntax(kind: raw.tokenKind!, leadingTrivia: leadingTrivia,
                           trailingTrivia: raw.trailingTrivia!,
                           presence: raw.presence)
    let (root, newData) = data.replacingSelf(newRaw)
    return TokenSyntax(root: root, data: newData)
  }

  /// Returns a new TokenSyntax with its trailing trivia replaced
  /// by the provided trivia.
  public func withTrailingTrivia(_ trailingTrivia: Trivia) -> TokenSyntax {
    guard raw.kind == .token else {
      fatalError("TokenSyntax must have token as its raw")
    }
    let newRaw = RawSyntax(kind: raw.tokenKind!,
                           leadingTrivia: raw.leadingTrivia!,
                           trailingTrivia: trailingTrivia,
                           presence: raw.presence)
    let (root, newData) = data.replacingSelf(newRaw)
    return TokenSyntax(root: root, data: newData)
  }

  /// Returns a new TokenSyntax with its leading trivia removed.
  public func withoutLeadingTrivia() -> TokenSyntax {
    return withLeadingTrivia([])
  }

  /// Returns a new TokenSyntax with its trailing trivia removed.
  public func withoutTrailingTrivia() -> TokenSyntax {
    return withTrailingTrivia([])
  }

  /// Returns a new TokenSyntax with all trivia removed.
  public func withoutTrivia() -> TokenSyntax {
    return withoutLeadingTrivia().withoutTrailingTrivia()
  }

  /// The leading trivia (spaces, newlines, etc.) associated with this token.
  public var leadingTrivia: Trivia {
    guard raw.kind == .token else {
      fatalError("TokenSyntax must have token as its raw")
    }
    return raw.leadingTrivia!
  }

  /// The trailing trivia (spaces, newlines, etc.) associated with this token.
  public var trailingTrivia: Trivia {
    guard raw.kind == .token else {
      fatalError("TokenSyntax must have token as its raw")
    }
    return raw.trailingTrivia!
  }

  /// The kind of token this node represents.
  public var tokenKind: TokenKind {
    guard raw.kind == .token else {
      fatalError("TokenSyntax must have token as its raw")
    }
    return raw.tokenKind!
  }

  public static func ==(lhs: TokenSyntax, rhs: TokenSyntax) -> Bool {
    return lhs._data === rhs._data
  }

  public var hashValue: Int {
    return ObjectIdentifier(_data).hashValue
  }
}
