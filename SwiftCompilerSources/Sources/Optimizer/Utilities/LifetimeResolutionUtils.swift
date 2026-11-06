//===--- LifetimeResolutionUtils.swift - Utilities for LifetimeResolution -===//
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

import SIL

//===----------------------------------------------------------------------===//
//                              StateIndexSpace
//===----------------------------------------------------------------------===//

/// The index space of the dataflow state that LifetimeResolution tracks for a storage root. Each index
/// is an element of the root's state, whose demand and availability are tracked independently.
///
/// The first elements are the leaves of a FieldIndexTrie, so a range of leaves within the trie is also a
/// range of this space. Elements that depend on the kind of root rather than its type follow them.
struct StateIndexSpace: CustomStringConvertible {
  typealias IndexRange = Range<Int>

  enum Mode {
    /// Memory, whose elements are the leaves of its type.
    case storage

    /// `self` in a root class initializer, whose elements are the stored properties of the instance.
    case rootClassSelf
  }

  /// What the state at an index is about.
  enum Element: Equatable {
    /// A leaf of `fields`.
    case field(Int)
  }

  let mode: Mode

  /// The fields of the root, which are the first elements of the space.
  let fields: FieldIndexTrie

  /// The elements that are initialized where the root begins.
  let initiallyInitialized: IndexRange

  init(_ mode: Mode, fields: FieldIndexTrie, initiallyInitialized: Bool) {
    self.mode = mode
    self.fields = fields
    self.initiallyInitialized = initiallyInitialized ? 0..<fields.leafCount : 0..<0
  }

  /// The number of elements.
  var count: Int { fields.leafCount }

  var isEmpty: Bool { count == 0 }

  /// The range covering every element: `0..<count`.
  var wholeRange: IndexRange { 0..<count }

  /// The elements that are fields.
  var fieldRange: IndexRange { 0..<fields.leafCount }

  /// The elements needed by a use of a class instance as a whole, as opposed to one of its stored properties.
  var instanceRange: IndexRange { wholeRange }

  /// The maximal ranges of elements that are all initialized, or all uninitialized, where the root begins.
  var initialRanges: [(range: IndexRange, initialized: Bool)] {
    let ranges = [(0..<initiallyInitialized.lowerBound, false),
                  (initiallyInitialized, true),
                  (initiallyInitialized.upperBound..<count, false)]
    return ranges.filter { !$0.0.isEmpty }
  }

  func element(at index: Int) -> Element {
    assert(wholeRange.contains(index), "index \(index) out of range \(wholeRange)")
    return .field(index)
  }

  func range(of element: Element) -> IndexRange? {
    switch element {
    case .field(let leaf):
      return fieldRange.contains(leaf) ? leaf..<(leaf + 1) : nil
    }
  }

  /// The fields selected by a projection path from the root.
  func range(ofFieldPath path: SmallProjectionPath) -> IndexRange {
    fields.range(of: path)
  }

  /// The fields within `range`.
  func fieldLeaves(in range: IndexRange) -> IndexRange {
    range.clamped(to: fieldRange)
  }

  /// Whether the element at `index` is a part of the storage that has an address.
  func hasAddress(_ index: Int) -> Bool {
    fieldRange.contains(index)
  }

  /// The type of the part of the storage at `index`.
  func leafType(_ index: Int) -> Type {
    precondition(hasAddress(index), "element \(element(at: index)) has no address")
    return fields.leafType(index)
  }

  /// Emits the projections from the root's `base` address down to the part of the storage at `index`, and passes
  /// its address to `body`. See FieldIndexTrie.withElementAddress.
  func withElementAddress(forIndex index: Int, from base: Value, _ builder: Builder, _ body: (Value) -> ()) {
    precondition(hasAddress(index), "element \(element(at: index)) has no address")
    fields.withElementAddress(forLeaf: index, from: base, builder, body)
  }

  /// An array of state with an entry per element.
  func makeStates<State>(repeating state: State) -> [State] {
    Array(repeating: state, count: count)
  }

  func description(of element: Element) -> String {
    switch element {
    case .field(let leaf):
      var path: SmallProjectionPath? = nil
      fields.forEachCoveringSubtree(of: [leaf]) { if path == nil { path = $0 } }
      return "field \(path.map { $0.isEmpty ? "<root>" : $0.description } ?? "?")"
    }
  }

  var description: String {
    let elements = wholeRange.map { "  \($0): \(description(of: element(at: $0)))" }
    return """
           {
           mode: \(mode)
           count: \(count)
           elements: \n\(elements.joined(separator: "\n"))
           initiallyInitialized: \(initiallyInitialized)
           }
           """
  }
}

//===----------------------------------------------------------------------===//
//                               Unit Tests
//===----------------------------------------------------------------------===//

let lifetimeResolutionStateSpaceTest = FunctionTest("lifetime_resolution_state_space") {
  function, arguments, context in
  var indexCache = FieldIndexTrieCache(for: function)
  let value = arguments.takeValue()
  guard let root = ResolvableRoot(value, &indexCache, context) else {
    print("not a resolvable root: \(value)")
    return
  }
  let space = root.stateIndexSpace(&indexCache)
  print(space)
  print("instanceRange: \(space.instanceRange)")

  assert(space.wholeRange == 0..<space.count)
  assert(space.fieldRange.lowerBound == 0 && space.fieldRange.upperBound <= space.count)
  for index in space.wholeRange {
    assert(space.range(of: space.element(at: index)) == index..<(index + 1))
  }
  assert(space.initialRanges.map { $0.range.count }.reduce(0, +) == space.count)
}
