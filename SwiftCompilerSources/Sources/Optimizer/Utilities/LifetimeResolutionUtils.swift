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

    /// The memory holding `self` in a derived class initializer. Its elements are the own stored properties of the
    /// instance, its superclass part, and the reference held in the memory.
    case derivedClassSelf
  }

  /// What the state at an index is about.
  enum Element: Equatable {
    /// A leaf of `fields`.
    case field(Int)

    /// The superclass part of a class instance, which `super.init` initializes.
    case superclass

    /// The reference to the instance held in memory.
    case reference
  }

  let mode: Mode

  /// The fields of the root, which are the first elements of the space.
  let fields: FieldIndexTrie

  /// The type of the instance that the root holds, which is the type of the `reference` element.
  let instanceType: Type

  /// The elements that are initialized where the root begins.
  let initiallyInitialized: IndexRange

  init(_ mode: Mode, fields: FieldIndexTrie, instanceType: Type, initiallyInitialized: Bool) {
    self.mode = mode
    self.fields = fields
    self.instanceType = instanceType
    self.initiallyInitialized = initiallyInitialized ? 0..<fields.leafCount : 0..<0
  }

  /// The number of elements.
  var count: Int { mode == .derivedClassSelf ? fields.leafCount + 2 : fields.leafCount }

  private var superclassIndex: Int? { mode == .derivedClassSelf ? fields.leafCount : nil }
  private var referenceIndex: Int? { mode == .derivedClassSelf ? fields.leafCount + 1 : nil }

  var isEmpty: Bool { count == 0 }

  /// The range covering every element: `0..<count`.
  var wholeRange: IndexRange { 0..<count }

  /// The elements that are fields.
  var fieldRange: IndexRange { 0..<fields.leafCount }

  /// The elements needed by a use of a class instance as a whole, as opposed to one of its stored properties.
  var instanceRange: IndexRange { superclassIndex.map { 0..<($0 + 1) } ?? wholeRange }

  /// The maximal ranges of elements that are all initialized, or all uninitialized, where the root begins.
  var initialRanges: [(range: IndexRange, initialized: Bool)] {
    let ranges = [(0..<initiallyInitialized.lowerBound, false),
                  (initiallyInitialized, true),
                  (initiallyInitialized.upperBound..<count, false)]
    return ranges.filter { !$0.0.isEmpty }
  }

  func element(at index: Int) -> Element {
    assert(wholeRange.contains(index), "index \(index) out of range \(wholeRange)")
    switch index {
    case superclassIndex: return .superclass
    case referenceIndex: return .reference
    default: return .field(index)
    }
  }

  func index(of element: Element) -> Int? {
    switch element {
    case .field(let leaf): fieldRange.contains(leaf) ? leaf : nil
    case .superclass: superclassIndex
    case .reference: referenceIndex
    }
  }

  func range(of element: Element) -> IndexRange? {
    index(of: element).map { $0..<($0 + 1) }
  }

  /// The fields selected by a projection path from the root.
  func range(ofFieldPath path: SmallProjectionPath) -> IndexRange {
    fields.range(of: path)
  }

  /// Whether the element at `index` is a part of the storage that has an address.
  func hasAddress(_ index: Int) -> Bool {
    fieldRange.contains(index) || index == referenceIndex
  }

  /// The type of the part of the storage at `index`.
  func leafType(_ index: Int) -> Type {
    precondition(hasAddress(index), "element \(element(at: index)) has no address")
    return index == referenceIndex ? instanceType : fields.leafType(index)
  }

  /// Emits the projections from the root's `base` address down to the part of the storage at `index`, and passes
  /// its address to `body`. See FieldIndexTrie.withElementAddress.
  func withElementAddress(forIndex index: Int, from base: Value, _ builder: Builder, _ body: (Value) -> ()) {
    precondition(hasAddress(index), "element \(element(at: index)) has no address")
    if index == referenceIndex {
      body(base)
      return
    }
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
    case .superclass:
      return "superclass"
    case .reference:
      return "reference"
    }
  }

  var description: String {
    let lines = ["{", "mode: \(mode)", "count: \(count)", "elements:"]
              + wholeRange.map { "  \($0): \(description(of: element(at: $0)))" }
              + ["initiallyInitialized: \(initiallyInitialized)", "}"]
    return lines.joined(separator: "\n")
  }
}

//===----------------------------------------------------------------------===//
//                            Derived Class Self
//===----------------------------------------------------------------------===//

/// A use of the memory holding `self` in a derived class initializer.
enum DerivedSelfUse {
  /// Stores the initializer's `self` argument.
  case initialStore(StoreInst)

  /// Borrows `self`, e.g. to access one of its stored properties.
  case borrow(LoadBorrowInst)

  /// Copies `self`, for a use of all of it.
  case copy(LoadInst)

  /// Loads `self` to pass it to `super.init`: a take, or a copy if LifetimeResolution demoted the take.
  case superInit(LoadInst, FullApplySite)

  /// Stores the result of `super.init`.
  case storeBack(StoreInst)

  case destroy(DestroyAddrInst)

  /// Takes `self` to deallocate it without running its deinit, which LifetimeResolution emits for a failure before
  /// `super.init`.
  case deallocate(LoadInst)

  /// Has no effect on `self`, like a `dealloc_stack`, `debug_value` or `diagnose`.
  case ignored

  case unknown

  init(_ use: Operand) {
    switch use.instruction {
    case let store as StoreInst where use == store.destinationOperand:
      if let argument = store.source as? FunctionArgument, argument.isSelf {
        self = .initialStore(store)
      } else {
        self = .storeBack(store)
      }
    case let loadBorrow as LoadBorrowInst:
      self = .borrow(loadBorrow)
    case let load as LoadInst:
      if let apply = superInitCall(passingSelfLoadedBy: load) {
        self = .superInit(load, apply)
      } else if load.loadOwnership == .copy {
        self = .copy(load)
      } else if load.loadOwnership == .take,
                load.uses.ignoreDebugUses.singleUse?.instruction is DeallocPartialRefInst {
        self = .deallocate(load)
      } else {
        self = .unknown
      }
    case let destroy as DestroyAddrInst:
      self = .destroy(destroy)
    case is DeallocStackInst, is DebugValueInst, is DiagnoseInst:
      self = .ignored
    default:
      self = .unknown
    }
  }
}

/// The call to `super.init` that takes `self` as loaded by `load`, if any.
func superInitCall(passingSelfLoadedBy load: LoadInst) -> FullApplySite? {
  guard let upcast = load.uses.ignoreDebugUses.ignore(usersOfType: DiagnoseInst.self).singleUse?.instruction
                       as? UpcastInst else {
    return nil
  }
  for use in upcast.uses {
    guard let apply = use.instruction as? FullApplySite,
          let argIndex = apply.calleeArgumentIndex(of: use),
          argIndex == apply.calleeArgumentConventions.selfIndex else {
      continue
    }
    if apply.callee is ObjCSuperMethodInst {
      return apply
    }
    if let callee = (apply.callee as? FunctionRefBaseInst)?.referencedFunction,
       callee.isInitializer || callee.name == "superinit" {
      return apply
    }
  }
  return nil
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
  print("immutableLeaves: \(root.immutableLeaves(space).sorted())")

  assert(space.wholeRange == 0..<space.count)
  assert(space.fieldRange.lowerBound == 0 && space.fieldRange.upperBound <= space.count)
  for index in space.wholeRange {
    assert(space.index(of: space.element(at: index)) == index)
    assert(space.range(of: space.element(at: index)) == index..<(index + 1))
  }
  assert(space.initialRanges.map { $0.range.count }.reduce(0, +) == space.count)
}

let lifetimeResolutionDerivedSelfUsesTest = FunctionTest("lifetime_resolution_derived_self_uses") {
  function, arguments, context in
  let value = arguments.takeValue()
  for use in function.instructions.flatMap({ $0.operands }) where use.value == value {
    switch DerivedSelfUse(use) {
    case .initialStore(let store): print("initialStore: \(store)")
    case .borrow(let loadBorrow): print("borrow: \(loadBorrow)")
    case .copy(let load): print("copy: \(load)")
    case .superInit(let load, let apply): print("superInit: \(load)\n  call: \(apply)")
    case .storeBack(let store): print("storeBack: \(store)")
    case .destroy(let destroy): print("destroy: \(destroy)")
    case .deallocate(let load): print("deallocate: \(load)")
    case .ignored: print("ignored: \(use.instruction)")
    case .unknown: print("unknown: \(use.instruction)")
    }
  }
}
