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

import AST
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
//                              Resolvable Root
//===----------------------------------------------------------------------===//

/// Identifies a resolvable storage root.
struct ResolvableRoot {
  let storage: Value

  /// The address that all accesses project from: the `mark_uninitialized` itself for
  /// an `alloc_stack`, or the `project_box` for an `alloc_box`. For `self` in a root class
  /// initializer, it is instead the `mark_uninitialized` reference whose stored properties are projected.
  let address: Value

  /// How the elements of the storage's state are laid out.
  let mode: StateIndexSpace.Mode

  /// The source variable, for diagnostics.
  let varDecl: VarDecl?

  /// The `mark_uninitialized` of the storage, if any; its kind identifies `self` in an initializer.
  let markUninitialized: MarkUninitializedInst?

  let isLexical: Bool

  /// Whether the storage corresponds to a `@noImplicitCopy` parameter.
  let isNoImplicitCopy: Bool

  /// The elements of the dataflow state tracked for this storage.
  let space: StateIndexSpace

  /// For derived class `self`, the store of the initializer's `self` argument into its memory.
  private let initialStoreOfDerivedSelf: StoreInst?

  /// An instruction requiring the whole root to be initialized, which is not found by walking uses of the address.
  enum ExtraLivenessUse {
    /// The function exits, at which point an `inout` parameter must be initialized.
    case functionExit(TermInst)
    /// The lifetime of the `alloc_box` ends.
    case boxLifetimeEnd(Instruction)

    var instruction: Instruction {
      switch self {
      case .functionExit(let exit): return exit
      case .boxLifetimeEnd(let end): return end
      }
    }
  }

  /// Extra instructions to consider as full liveness uses of this root, that are not captured by walking uses of
  /// the address.
  let extraLivenessUses: [ExtraLivenessUse]

  /// Either the instruction defining this root, or the very first instruction
  /// after which it is defined.
  var startInstruction: Instruction {
    if let inst = address.definingInstruction {
      return inst
    }
    return address.nextInstruction
  }

  /// The instruction after which markers for uses before initialization are inserted. For derived class `self`,
  /// it is the initial store of `self`, after which its stored properties can be projected.
  var markerInsertionPoint: Instruction { initialStoreOfDerivedSelf ?? startInstruction }

  /// Whether the storage is a class instance whose stored properties are tracked: `self` in a class initializer.
  var isClassInstance: Bool { mode != .storage }

  /// Whether the binding is a `let` (vs a `var`), which may only be initialized once.
  var isLet: Bool { !(storage is FunctionArgument) && (varDecl?.isLet ?? false) }

  /// `self` in a struct, enum, or root class initializer that initializes the stored properties itself.
  var isRootSelf: Bool {
    switch markUninitialized?.kind {
    case .rootSelf?, .crossModuleRootSelf?: true
    default: false
    }
  }

  /// The initializer's `self`, if this is `self` within an initializer.
  var initSelfKind: MarkUninitializedInst.Kind? {
    switch markUninitialized?.kind {
    case .variable, .indirectResult, nil: nil
    case let kind?: kind
    }
  }

  /// `self` of a method or initializer.
  var isSelf: Bool { initSelfKind != nil || ((storage as? FunctionArgument)?.isSelf ?? false) }

  /// The elements of the storage that may only be initialized once: all of a `let` binding,
  /// or the `let` stored properties of `self` in a struct or class initializer.
  func immutableLeaves() -> Set<Int> {
    if isRootSelf || mode == .derivedClassSelf {
      let function = address.parentFunction
      let type = address.type.objectType
      guard let fields = type.getNominalFields(in: function) else {
        return []
      }
      let kind: SmallProjectionPath.FieldKind = isClassInstance ? .classField : .structField
      // An inherited stored property has no element of its own.
      let firstOwnField = isClassInstance ? (type.superClassType?.bridged.getNumNominalFields() ?? 0) : 0
      var leaves = Set<Int>()
      for field in firstOwnField..<fields.count where fields.getUserFacingFieldDecl(withIndex: field)?.isLet ?? false {
        leaves.formUnion(space.range(ofFieldPath: SmallProjectionPath(kind, index: field)))
      }
      return leaves
    }
    switch markUninitialized?.kind {
    case .variable?, nil:
      return isLet ? Set(space.wholeRange) : []
    default:
      return []
    }
  }

  /// Build from a storage allocation (`alloc_box` / `alloc_stack`), an address argument, or the `self` argument of a
  /// root class initializer. Callers holding only an address must first walk up to the allocation, e.g. with
  /// `allocation(backing:)`.
  init?(_ val: Value, _ indexCache: inout FieldIndexTrieCache) {
    let function = val.parentFunction
    var extraUses: [ExtraLivenessUse] = []
    var initialStore: StoreInst? = nil
    let startsInitialized: Bool

    switch val {
    case let allocStack as AllocStackInst:
      storage = val
      markUninitialized = Self.findMarkUninitialized(allocStack)

      if let mu = markUninitialized {
        address = mu

        let indices = indexCache.fieldIndices(for: storage.type.objectType)

        // If this is the 'self' for an empty type in its initializer, we don't have any fields to initialize.
        // So, treat the storage as being born initialized
        startsInitialized = mu.kind == .rootSelf && !indices.hasFields

        if mu.kind == .derivedSelf {
          initialStore = Self.findInitialStoreOfDerivedSelf(mu)
        }
        mode = initialStore != nil ? .derivedClassSelf : .storage
      } else {
        address = storage
        startsInitialized = false
        mode = .storage
      }

      varDecl = allocStack.varDecl
      isLexical = allocStack.isLexical

    case let allocBox as AllocBoxInst:
      storage = allocBox
      guard let (projectBox, boxIsLexical) = Self.findProjectBox(of: allocBox) else {
        return nil
      }
      address = projectBox
      markUninitialized = Self.findMarkUninitialized(allocBox)
      varDecl = allocBox.varDecl
      isLexical = boxIsLexical
      startsInitialized = false
      mode = .storage

      // Treat any lifetime ends or copies of the box itself as extra uses.
      let box: Value = markUninitialized ?? storage
      box.uses.endingLifetime.forEach { extraUses.append(.boxLifetimeEnd($0.instruction)) }
      // TODO: add copies of the box too

    case let arg as FunctionArgument where arg.type.isAddress:
      storage = arg
      address = arg
      markUninitialized = nil
      varDecl = arg.findVarDecl()
      isLexical = arg.isLexical
      mode = .storage

      switch arg.convention {
      case .indirectIn:
        startsInitialized = true
      case .indirectOut:
        startsInitialized = false
      case .indirectInout:
        startsInitialized = true

        // For an inout parameter, all function exit terminators are considered uses.
        function.exitingBlocks.forEach { extraUses.append(.functionExit($0.terminator)) }

      default:
        return nil   // unimplemented convention
      }

    case _ where val.type.isClass && val.ownership == .owned:
      // `self` in a root class initializer, whose instance's stored properties start uninitialized.
      guard let mu = Self.findMarkUninitialized(val), mu.kind == .rootSelf else {
        return nil
      }
      storage = val
      address = mu
      markUninitialized = mu
      varDecl = (val as? FunctionArgument)?.findVarDecl()
      isLexical = (val as? Argument)?.isLexical ?? false
      startsInitialized = !indexCache.fieldIndices(forClassInstance: val.type).hasFields
      mode = .rootClassSelf

    default:
      return nil
    }

    extraLivenessUses = extraUses
    initialStoreOfDerivedSelf = initialStore

    let fields = mode != .storage ? indexCache.fieldIndices(forClassInstance: address.type.objectType)
                                  : indexCache.fieldIndices(for: address.type)
    space = StateIndexSpace(mode, fields: fields, instanceType: address.type.objectType,
                            initiallyInitialized: startsInitialized)

    if let arg = storage as? FunctionArgument {
      isNoImplicitCopy = arg.isNoImplicitCopy
    } else {
      // For `consuming` args, we typically store the result in memory because it's mutable, so look for that store.
      // TODO: For performance, it's worth investigating whether this set of addresses should be cached per-function.
      let address = self.address
      isNoImplicitCopy = address.parentFunction.arguments.contains { arg in
        arg.isNoImplicitCopy
          && arg.uses.users(ofType: StoreInst.self).contains { $0.source == arg && $0.destination == address }
      }
    }
  }

  /// The storage allocation (`alloc_box`, `alloc_stack`, or argument) backing `address`, via its access path,
  /// or the reference to the class instance whose stored property it is. For derived class `self`, that is the
  /// memory holding the reference.
  static func allocation(backing address: Value) -> Value? {
    switch address.accessPath.base {
    case .box(let projectBox):
      return projectBox.box.referenceRoot
    case .stack(let allocStack):
      return allocStack
    case .argument(let argument):
      return argument
    case .class(let projection):
      let reference = projection.instance.referenceRoot
      if let load = reference as? LoadInstruction, let markUninitialized = load.address as? MarkUninitializedInst,
         markUninitialized.kind == .derivedSelf {
        return allocation(backing: markUninitialized)
      }
      return (reference as? MarkUninitializedInst)?.operand.value ?? reference
    default:
      return nil
    }
  }

  /// Change the storage such that it indicates a dynamic lifetime.
  func convertToDynamic(_ context: MutatingContext) {
    if let allocStack = storage as? AllocStackInst {
      allocStack.setDynamicLifetime(context)
    }
    if let allocBox = storage as? AllocBoxInst {
      allocBox.setDynamicLifetime(context)
    }
  }

  /// Find the sole project_box reachable from an alloc_box from SILGen.
  private static func findProjectBox(of boxValue: Value) -> (projectBox: ProjectBoxInst, isLexical: Bool)? {
    for use in boxValue.uses {
      switch use.instruction {
      case let projectBox as ProjectBoxInst:
        return (projectBox, false)
      case let beginBorrow as BeginBorrowInst:
        if let (projectBox, isLexical) = findProjectBox(of: beginBorrow) {
          return (projectBox, isLexical || beginBorrow.isLexical)
        }
      case let markUninit as MarkUninitializedInst:
        if let result = findProjectBox(of: markUninit) {
          return result
        }
      default:
        break
      }
    }
    return nil
  }

  /// The store of the initializer's `self` argument into the memory holding derived class `self`, if every use
  /// of the memory is recognized and there is exactly one such store.
  /// FIXME: This is probably too pedantic; can we just trust the derivedSelf on the mark_uninitialized?
  private static func findInitialStoreOfDerivedSelf(_ markUninitialized: MarkUninitializedInst) -> StoreInst? {
    var initialStore: StoreInst? = nil
    for use in markUninitialized.uses {
      switch DerivedSelfUse(use) {
      case .initialStore(let store):
        if initialStore != nil { return nil }
        initialStore = store
      case .unknown:
        return nil
      default:
        break
      }
    }
    return initialStore
  }

  /// The `mark_uninitialized` of an allocation or of the `self` argument, if it has exactly one.
  private static func findMarkUninitialized(_ value: Value) -> MarkUninitializedInst? {
    value.uses.singleUser(ofType: MarkUninitializedInst.self)
  }
}

//===----------------------------------------------------------------------===//
//                            Class Instance Uses
//===----------------------------------------------------------------------===//

/// Walks the uses of a class instance, looking through borrows and copies of its reference.
protocol ClassInstanceUseWalker {
  /// The address of one of the instance's stored properties.
  mutating func storedPropertyUse(_ projection: RefElementAddrInst) -> WalkResult

  /// Any other use, which needs the whole instance.
  mutating func instanceUse(_ use: Operand) -> WalkResult
}

extension ClassInstanceUseWalker {
  mutating func walkDownUses(ofClassInstance instance: Value) -> WalkResult {
    for use in instance.uses {
      let result: WalkResult
      switch use.instruction {
      case let projection as RefElementAddrInst:
        result = storedPropertyUse(projection)
      case let borrow as BeginBorrowInst:
        result = walkDownUses(ofClassInstance: borrow)
      case let copy as CopyValueInst:
        result = walkDownUses(ofClassInstance: copy)
      case is EndBorrowInst, is DebugValueInst, is ClassMethodInst:
        result = .continueWalk
      case is DestroyValueInst where instance is CopyValueInst:
        result = .continueWalk
      default:
        result = instanceUse(use)
      }
      if result == .abortWalk {
        return .abortWalk
      }
    }
    return .continueWalk
  }
}

/// Walks every use of a resolvable root, whatever its mode.
protocol ResolvableRootUseWalker: AddressDefUseWalker, ClassInstanceUseWalker where Path == UnusedWalkingPath {
  /// A use of the memory holding `self` in a derived class initializer.
  mutating func derivedSelfUse(_ use: DerivedSelfUse, of operand: Operand) -> WalkResult
}

extension ResolvableRootUseWalker {
  mutating func walkDownUses(of root: ResolvableRoot) -> WalkResult {
    switch root.mode {
    case .storage:
      return walkDownUses(ofAddress: root.address, path: Path())
    case .rootClassSelf:
      return walkDownUses(ofClassInstance: root.address)
    case .derivedClassSelf:
      for use in root.address.uses {
        if derivedSelfUse(DerivedSelfUse(use), of: use) == .abortWalk {
          return .abortWalk
        }
      }
      return .continueWalk
    }
  }

  mutating func storedPropertyUse(_ projection: RefElementAddrInst) -> WalkResult {
    walkDownUses(ofAddress: projection, path: Path())
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

  /// The call to `super.init`, if this loads `self` to pass to it.
  var superInitCall: FullApplySite? {
    if case .superInit(_, let apply) = self {
      return apply
    }
    return nil
  }

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
      if let apply = findSuperInitCall(passingSelfLoadedBy: load) {
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
private func findSuperInitCall(passingSelfLoadedBy load: LoadInst) -> FullApplySite? {
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
//                                  Markers
//===----------------------------------------------------------------------===//

/// A marker that LifetimeResolution leaves where it corrected a misuse, which DiagnoseResolvedLifetimes turns into
/// a user-facing diagnostic.
enum LifetimeResolutionMarker {
  /// `assign undef to [init] %leaf`, after the root's `markerInsertionPoint`: the leaf is read before being
  /// initialized.
  case useBeforeInit(AssignInst)

  /// `diagnose [let_reinit] %addr`, right before the def: immutable storage at `%addr` may already be initialized.
  case letReinit(DiagnoseInst)

  /// `diagnose [missing_super_init] %self`, after the root's `markerInsertionPoint`: derived class `self` is used
  /// before `super.init`.
  case missingSuperInit(DiagnoseInst)

  /// `diagnose [unpermitted_copy] %x`, right after the `load`, `copy_value`, or `copy_addr` that copies: the copy
  /// is not permitted, e.g. a consume demoted because the value is used afterwards.
  case unpermittedCopy(DiagnoseInst)

  init?(_ inst: Instruction) {
    switch inst {
    case let diagnose as DiagnoseInst:
      switch diagnose.kind {
      case .unpermittedCopy: self = .unpermittedCopy(diagnose)
      case .letReinit: self = .letReinit(diagnose)
      case .missingSuperInit: self = .missingSuperInit(diagnose)
      case .invalid: return nil
      }
    case let assign as AssignInst where assign.assignOwnership == .initialize && assign.source is Undef:
      self = .useBeforeInit(assign)
    default:
      return nil
    }
  }

  /// The `diagnose` instruction of the marker, unless it is a `useBeforeInit`, whose `assign` is a real
  /// initialization.
  var diagnose: DiagnoseInst? {
    switch self {
    case .letReinit(let diagnose), .missingSuperInit(let diagnose), .unpermittedCopy(let diagnose): diagnose
    case .useBeforeInit: nil
    }
  }

  static func markUsesBeforeInit(of leaves: [Int], in root: ResolvableRoot, _ context: some MutatingContext) {
    let builder = Builder(after: root.markerInsertionPoint, context)
    for leaf in leaves {
      root.space.withElementAddress(forIndex: leaf, from: root.address, builder) { address in
        let undef = Undef.get(type: address.type.objectType, context)
        builder.createAssign(source: undef, destination: address, ownership: .initialize)
      }
    }
  }

  static func markLetReinit(of address: Value, before def: Instruction, _ context: some MutatingContext) {
    Builder(before: def, context).createDiagnose(operand: address, kind: .letReinit)
  }

  static func markMissingSuperInit(of root: ResolvableRoot, _ context: some MutatingContext) {
    Builder(after: root.markerInsertionPoint, context).createDiagnose(operand: root.address, kind: .missingSuperInit)
  }

  static func markUnpermittedCopy(_ load: LoadInst, _ context: some MutatingContext) {
    Builder(after: load, context).createDiagnose(operand: load, kind: .unpermittedCopy)
  }

  static func markUnpermittedCopy(_ copy: CopyValueInst, _ context: some MutatingContext) {
    Builder(after: copy, context).createDiagnose(operand: copy, kind: .unpermittedCopy)
  }

  static func markUnpermittedCopy(_ copyAddr: CopyAddrInst, _ context: some MutatingContext) {
    Builder(after: copyAddr, context).createDiagnose(operand: copyAddr.source, kind: .unpermittedCopy)
  }

  /// The `load`, `copy_value`, or `copy_addr` that an `unpermitted_copy` marker is placed after.
  var copy: Instruction? {
    guard case .unpermittedCopy(let marker) = self else {
      return nil
    }
    switch marker.operand.value {
    case let load as LoadInst:
      return load
    case let copy as CopyValueInst:
      return copy
    case let address where address.type.isAddress:
      guard let copyAddr = marker.previous as? CopyAddrInst, copyAddr.source == address else {
        return nil
      }
      return copyAddr
    default:
      return nil
    }
  }
}

//===----------------------------------------------------------------------===//
//                              Projection Paths
//===----------------------------------------------------------------------===//

extension AccessPath {
  /// The projection path from the storage root, which for a stored property of a class instance
  /// starts at the instance.
  var projectionPathFromRoot: SmallProjectionPath {
    if case .class(let projection) = base {
      return projectionPath.push(.classField, index: projection.fieldIndex)
    }
    return projectionPath
  }
}

extension Value {
  /// The projection path of this address from its storage root.
  var projectionPathFromRoot: SmallProjectionPath { accessPath.projectionPathFromRoot }
}

//===----------------------------------------------------------------------===//
//                               Unit Tests
//===----------------------------------------------------------------------===//

let lifetimeResolutionStateSpaceTest = FunctionTest("lifetime_resolution_state_space") {
  function, arguments, context in
  var indexCache = FieldIndexTrieCache(for: function)
  let value = arguments.takeValue()
  guard let root = ResolvableRoot(value, &indexCache) else {
    print("not a resolvable root: \(value)")
    return
  }
  let space = root.space
  print(space)
  print("instanceRange: \(space.instanceRange)")
  print("immutableLeaves: \(root.immutableLeaves().sorted())")

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
