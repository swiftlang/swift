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

  /// Whether or not this node it marked as `present`.
  public var isPresent: Bool {
    return raw.presence == .present
  }

  /// Whether or not this node it marked as `missing`.
  public var isMissing: Bool {
    return raw.presence == .missing
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

  public func withKind(_ tokenKind: TokenKind) -> TokenSyntax {
    guard case let .token(_, leadingTrivia, trailingTrivia, presence) = raw else {
      fatalError("TokenSyntax must have token as its raw")
    }
    let (root, newData) = data.replacingSelf(.token(tokenKind, leadingTrivia,
                                                    trailingTrivia, presence))
    return TokenSyntax(root: root, data: newData)
  }

  /// Returns a new TokenSyntax with its leading trivia replaced
  /// by the provided trivia.
  public func withLeadingTrivia(_ leadingTrivia: Trivia) -> TokenSyntax {
    guard case let .token(kind, _, trailingTrivia, presence) = raw else {
      fatalError("TokenSyntax must have token as its raw")
    }
    let (root, newData) = data.replacingSelf(.token(kind, leadingTrivia,
                                                    trailingTrivia, presence))
    return TokenSyntax(root: root, data: newData)
  }

  /// Returns a new TokenSyntax with its trailing trivia replaced
  /// by the provided trivia.
  public func withTrailingTrivia(_ trailingTrivia: Trivia) -> TokenSyntax {
    guard case let .token(kind, leadingTrivia, _, presence) = raw else {
      fatalError("TokenSyntax must have token as its raw")
    }
    let (root, newData) = data.replacingSelf(.token(kind, leadingTrivia,
                                                    trailingTrivia, presence))
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
    guard case .token(_, let leadingTrivia, _, _) = raw else {
      fatalError("TokenSyntax must have token as its raw")
    }
    return leadingTrivia
  }

  /// The trailing trivia (spaces, newlines, etc.) associated with this token.
  public var trailingTrivia: Trivia {
    guard case .token(_, _, let trailingTrivia, _) = raw else {
      fatalError("TokenSyntax must have token as its raw")
    }
    return trailingTrivia
  }

  /// The kind of token this node represents.
  public var tokenKind: TokenKind {
    guard case .token(let kind, _, _, _) = raw else {
      fatalError("TokenSyntax must have token as its raw")
    }
    return kind
  }

  public static func ==(lhs: TokenSyntax, rhs: TokenSyntax) -> Bool {
    return lhs._data === rhs._data
  }

  public var hashValue: Int {
    return ObjectIdentifier(_data).hashValue
  }
}
