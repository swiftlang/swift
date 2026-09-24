//===--- FieldIndexTrie.swift - type-tree leaf numbering ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Basic

//===----------------------------------------------------------------------===//
//                              FieldIndexTrieCache
//===----------------------------------------------------------------------===//

// A cache to avoid rebuilding the tree for a given type.
// TODO: This should be instantiated per Module instead.
public struct FieldIndexTrieCache {
  private var cache: Dictionary<Type, FieldIndexTrie> = [:]
  private let function: Function
  public init(for function: Function) {
    self.function = function
  }
  public mutating func fieldIndices(for type: Type) -> FieldIndexTrie {
    if let trie = cache[type] {
      return trie
    }

    let trie = FieldIndexTrie(of: type, in: function)
    cache[type] = trie
    return trie
  }
}

//===----------------------------------------------------------------------===//
//                              FieldIndexTrie
//===----------------------------------------------------------------------===//

/// For a single fixed type T, establishes a mapping between all projections into
/// the type T and a range of indices corresponding to the fields the projection
/// would "select". The indices are a contiguous space of integers suitable for
/// tracking projection-sensitive information using a simple Array.
///
/// For example, if you have T = ((Int, Int), Int) then the count of "leaves"
/// or real fields is 3. So the whole index space is [0, 3) for those fields:
///
///   ((Int,  Int), Int)
///      ^     ^     ^
///      0     1     2
///
/// Suppose you have a value `t` of this type. The projections into this type
/// produce an IndexRange (or sub-range) of that index space to which it refers:
///
///   t      -->   [0, 3)
///   t.0    -->   [0, 2)
///   t.0.0  -->   [0, 1)
///   t.0.1  -->   [1, 2)
///   t.1    -->   [2, 3)
///
public struct FieldIndexTrie: CustomStringConvertible {
  public typealias Path = SmallProjectionPath
  public typealias FieldKind = SmallProjectionPath.FieldKind

  // A subset of fields within a Type, represented as a contiguous range of indices
  // within this trie's index-space mapping.
  public typealias IndexRange = Range<Int>

  private final class Node {
    let range: IndexRange         // The contiguous index range of this node and its entire sub-tree.
    let childKind: FieldKind?     // The kind of projection into the `children`
    let children: [Node]          // Index in this array corresponds to their field index.

    init(range: Range<Int>, childKind: FieldKind?, children: [Node]) {
      self.range = range
      self.childKind = childKind
      self.children = children

      assert((children.count > 0 && childKind != nil) ||
             (children.count == 0 && childKind == nil), "expected no children iff when childKind is nil")
    }
  }

  private let root: Node

  /// The total number of leaf subelements.
  public var leafCount: Int { wholeRange.upperBound }

  /// The range covering every leaf: `0..<leafCount`.
  public var wholeRange: Range<Int> { root.range }

  public init(of type: Type, in function: Function) {
    var cursor = 0
    self.root = Self.build(type, in: function, cursor: &cursor)
  }

  /// Depth-first construction that assigns contiguous ranges via a monotonically-increasing `cursor`.
  private static func build(_ type: Type, in function: Function, cursor: inout Int) -> Node {
    let begin = cursor
    var children: [Node] = []
    var childKind: FieldKind? = nil

    if type.isStruct,
       let fields = type.getNominalFields(in: function) {
      for field in fields {
        childKind = .structField
        children.append(build(field, in: function, cursor: &cursor))
      }
    }

    if type.isTuple {
      for element in type.tupleElements {
        childKind = .tupleField
        children.append(build(element, in: function, cursor: &cursor))
      }
    }

    // If there were no children, or its field is opaque, an instance of this type itself still occupies one leaf.
    if cursor == begin {
      cursor += 1
    }

    return Node(range: begin..<cursor, childKind: childKind, children: children)
  }

  /// The half-open leaf range covered by `path` from the root.
  public func range(of path: Path) -> Range<Int> {
    var node = root
    var p = path

    while true {
      let (kind, index, rest) = p.pop()
      if kind == .root { break }
      p = rest

      guard let nodeKind = node.childKind,
            nodeKind == kind,
            index >= 0, index < node.children.count else {
        return node.range
      }

      node = node.children[index]
    }

    return node.range
  }

  /// Visits every node in the trie, passing its leaf range and a human-readable path string.
  /// The root itself is visited with the empty path. Intended for debugging and unit testing.
  func forEachNode(_ body: (IndexRange, String) -> Void) {
    func visit(_ node: Node, _ display: String) {
      body(node.range, display.isEmpty ? "<root>" : display)

      guard let childKind = node.childKind else { return }

      for (index, child) in node.children.enumerated() {
        let componentStr = FieldIndexTrie.Path(childKind, index: index).description
        let childDisplay = display.isEmpty ? componentStr : "\(display).\(componentStr)"
        visit(child, childDisplay)
      }
    }
    visit(root, "")
  }

  public var description: String {
    var nodeStrings: [String] = []
    self.forEachNode { range, display in
      nodeStrings.append("  \(display) -> \(range)")
    }
    return """
           {
           leafCount: \(leafCount)
           nodes: \n\(nodeStrings.joined(separator: "\n"))
           }
           """
  }
}

//===----------------------------------------------------------------------===//
//                               Unit Tests
//===----------------------------------------------------------------------===//

let fieldIndexTrieTest = Test("field_index_trie") {
  function, arguments, context in

  let value = arguments.takeValue()
  let type = value.type.objectType
  let trie = FieldIndexTrie(of: type, in: function)

  print("type: \(type)")
  print(trie)

  assert(trie.wholeRange == 0..<trie.leafCount)

  // Check some basic out-of-range projections.
  let whole = trie.wholeRange
  assert(trie.range(of: FieldIndexTrie.Path()) == whole)
  assert(trie.range(of: FieldIndexTrie.Path(.anything)) == whole)
}
