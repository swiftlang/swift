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
  private var classInstanceCache: Dictionary<Type, FieldIndexTrie> = [:]
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
  public mutating func fieldIndices(forClassInstance type: Type) -> FieldIndexTrie {
    if let trie = classInstanceCache[type] {
      return trie
    }

    let trie = FieldIndexTrie(ofClassInstance: type, in: function)
    classInstanceCache[type] = trie
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
/// A class type is normally a single opaque leaf, since a value of that type is just a reference.
/// A trie built for a class *instance* instead has a leaf for each of the class's own stored
/// properties, reached through `.classField` projections. Inherited stored properties have no
/// leaves. Only the root is expanded this way; class-typed fields within it are still opaque leaves.
public struct FieldIndexTrie: CustomStringConvertible {
  public typealias Path = SmallProjectionPath
  public typealias FieldKind = SmallProjectionPath.FieldKind

  // A subset of fields within a Type, represented as a contiguous range of indices
  // within this trie's index-space mapping.
  public typealias IndexRange = Range<Int>

  /// A child of a node is identified by its *offset* within `children`. A projection into the node
  /// instead uses a *field index*: the tuple element index, or the index given by `getIndexedField`,
  /// which counts a class's inherited fields first. They coincide except for a class instance with
  /// inherited fields.
  private final class Node {
    let type: Type                // The type of the field this node represents.
    let range: IndexRange         // The contiguous index range of this node and its entire sub-tree.
    let childKind: FieldKind?     // The kind of projection into the `children`
    let children: [Node]          // The fields, in field index order.

    /// The field index of `children[0]`.
    let firstFieldIndex: Int

    init(type: Type, range: Range<Int>, childKind: FieldKind?, children: [Node], firstFieldIndex: Int = 0) {
      self.type = type
      self.range = range
      self.childKind = childKind
      self.children = children
      self.firstFieldIndex = firstFieldIndex

      assert((children.count > 0 && childKind != nil) ||
             (children.count == 0 && childKind == nil), "expected no children iff when childKind is nil")
    }

    /// The offset of the child selected by a field index, or nil if there is none.
    func childOffset(forFieldIndex fieldIndex: Int) -> Int? {
      let offset = fieldIndex - firstFieldIndex
      return children.indices.contains(offset) ? offset : nil
    }

    /// The field index selecting the child at `offset`.
    func fieldIndex(ofChildAt offset: Int) -> Int {
      offset + firstFieldIndex
    }
  }

  private let root: Node

  /// The total number of leaf subelements. This is always at least 1, even for types with no fields.
  public var leafCount: Int { wholeRange.upperBound }

  /// The range covering every leaf: `0..<leafCount`.
  public var wholeRange: Range<Int> { root.range }

  /// Does this type have any visible, initializable fields?
  public var hasFields: Bool { root.children.count > 0 }

  public init(of type: Type, in function: Function) {
    var cursor = 0
    self.root = Self.build(type, in: function, cursor: &cursor)
  }

  /// Builds the trie for an instance of the class `type`, rather than a reference to one.
  public init(ofClassInstance type: Type, in function: Function) {
    assert(type.isClass, "expected a class type: \(type)")
    var cursor = 0
    guard let fields = type.getNominalFields(in: function) else {
      self.root = Self.build(type, in: function, cursor: &cursor)
      return
    }

    let firstOwnField = type.superClassType?.bridged.getNumNominalFields() ?? 0
    var children: [Node] = []
    for field in firstOwnField..<fields.count {
      children.append(Self.build(fields[field], in: function, cursor: &cursor))
    }

    if children.isEmpty {
      self.root = Node(type: type, range: 0..<1, childKind: nil, children: [])
    } else {
      self.root = Node(type: type, range: 0..<cursor, childKind: .classField, children: children,
                       firstFieldIndex: firstOwnField)
    }
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

    return Node(type: type, range: begin..<cursor, childKind: childKind, children: children)
  }

  /// The path of nodes from the root down to the node for `leaf`, excluding the root.
  private func pathToLeaf(_ leaf: Int) -> [(kind: FieldKind, fieldIndex: Int, node: Node)] {
    assert(wholeRange.contains(leaf), "leaf \(leaf) out of range \(wholeRange)")
    var path: [(kind: FieldKind, fieldIndex: Int, node: Node)] = []
    var node = root
    while let kind = node.childKind {
      let offset = node.children.firstIndex(where: { $0.range.contains(leaf) })!
      let fieldIndex = node.fieldIndex(ofChildAt: offset)
      node = node.children[offset]
      path.append((kind, fieldIndex, node))
    }
    return path
  }

  /// The type of the field corresponding to `leaf`.
  public func leafType(_ leaf: Int) -> Type {
    (pathToLeaf(leaf).last?.node.type ?? root.type).objectType
  }

  /// Emits the projections from `base` down to the field for `leaf`, and passes its address to `body`.
  /// The `base` is an address of this trie's type, or for a class instance, either the reference or
  /// an address holding it. Any borrow scope needed for the projection ends after `body`.
  public func withElementAddress(forLeaf leaf: Int, from base: Value, _ builder: Builder, _ body: (Value) -> ()) {
    var address = base
    var borrow: Value? = nil
    for (kind, fieldIndex, _) in pathToLeaf(leaf) {
      switch kind {
      case .structField:
        address = builder.createStructElementAddr(structAddress: address, fieldIndex: fieldIndex)
      case .tupleField:
        address = builder.createTupleElementAddr(tupleAddress: address, elementIndex: fieldIndex)
      case .classField:
        assert(address == base, "only the root of a trie is a class instance")
        let instance: Value = base.type.isAddress ? builder.createLoadBorrow(fromAddress: base)
                                                  : builder.createBeginBorrow(of: base)
        borrow = instance
        address = builder.createRefElementAddr(classReference: instance, fieldIndex: fieldIndex)
      default:
        fatalError("unexpected projection kind \(kind)")
      }
    }
    body(address)
    if let borrow {
      builder.createEndBorrow(of: borrow)
    }
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
            let offset = node.childOffset(forFieldIndex: index) else {
        return node.range
      }

      node = node.children[offset]
    }

    return node.range
  }

  /// Visits the largest subtrees whose leaves are all within `leaves`, in leaf order, passing the
  /// path from the root to each subtree. Together, the visited subtrees cover exactly `leaves`.
  /// With `excludingRoot`, a fully covered root is visited as its children instead.
  public func forEachCoveringSubtree(of leaves: Set<Int>, excludingRoot: Bool = false, _ body: (Path) -> Void) {
    // Paths are built leaf-to-root, so record each step and assemble the path once a subtree is found.
    var steps: [(kind: FieldKind, index: Int)] = []
    func visit(_ node: Node) {
      if node.range.allSatisfy(leaves.contains) && !(excludingRoot && node === root && node.childKind != nil) {
        body(steps.reversed().reduce(Path()) { $0.push($1.kind, index: $1.index) })
        return
      }
      guard let childKind = node.childKind else { return }
      for (offset, child) in node.children.enumerated() where child.range.contains(where: leaves.contains) {
        steps.append((childKind, node.fieldIndex(ofChildAt: offset)))
        visit(child)
        steps.removeLast()
      }
    }
    visit(root)
  }

  /// Visits every node in the trie, passing its leaf range and a human-readable path string.
  /// The root itself is visited with the empty path. Intended for debugging and unit testing.
  func forEachNode(_ body: (IndexRange, String) -> Void) {
    func visit(_ node: Node, _ display: String) {
      body(node.range, display.isEmpty ? "<root>" : display)

      guard let childKind = node.childKind else { return }

      for (offset, child) in node.children.enumerated() {
        let componentStr = FieldIndexTrie.Path(childKind, index: node.fieldIndex(ofChildAt: offset)).description
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
  let ofClassInstance = arguments.hasUntaken && arguments.takeBool()
  let trie = ofClassInstance ? FieldIndexTrie(ofClassInstance: type, in: function)
                             : FieldIndexTrie(of: type, in: function)

  print("type: \(type)")
  print(trie)

  assert(trie.wholeRange == 0..<trie.leafCount)

  // Check some basic out-of-range projections.
  let whole = trie.wholeRange
  assert(trie.range(of: FieldIndexTrie.Path()) == whole)
  assert(trie.range(of: FieldIndexTrie.Path(.anything)) == whole)

  if ofClassInstance, let fields = type.getNominalFields(in: function) {
    for field in fields.indices {
      print("field \(field) -> \(trie.range(of: FieldIndexTrie.Path(.classField, index: field)))")
    }
  }
}
