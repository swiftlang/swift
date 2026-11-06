//===--- LifetimeResolution.swift -----------------------------------------==//
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

private func log(_ message: @autoclosure () -> String) {
  llvmDebug("lifetime-resolution", message())
}

// TODO: determine whether this needs OSSACompleteLifetime to run first.
let lifetimeResolutionPass = FunctionPass(name: "lifetime-resolution") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else { return }

  log("\n\n\n*** Starting LifetimeResolution on \(function.name.string)")

  var resolver = Resolver(function, context)
  var indexCache = FieldIndexTrieCache(for: function)

  // Visit blocks in post-order, each bottom-up, so a value's uses in the blocks it dominates are
  // resolved before the value itself.
  for block in function.postOrderBlocks(context) {
    for inst in block.instructions.reversed() {
      // Ignore `mark_uninitialized` this pass is driven by the storage itself.
      if inst is MarkUninitializedInst { continue }

      for result in inst.results {
        resolver.resolve(result, &indexCache)
      }
    }
    for argument in block.arguments {
      resolver.resolve(argument, &indexCache)
    }
  }
}

extension Resolver {
  mutating func resolve(_ root: Value, _ indexCache: inout FieldIndexTrieCache) {
    if run(on: root, &indexCache) {
      return
    }

    // TODO: Eventually we should analyze and resolve trivial, non-address values
    //       in the same manner that we do for single-def values.
    if root.type.isTrivial(in: root.parentFunction) {
      return
    }

    // TODO: resolve `.guaranteed` values too. It should amount to having all consuming uses
    //   require a copy, including those on the boundary, and inserting end_access/end_borrow.
    if root.ownership == .owned {
      resolveSingleDef(root, context)
    }
  }
}


/// Identifies a resolvable storage root.
struct ResolvableRoot {
  let storage: Value

  /// The address that all accesses project from: the `mark_uninitialized` itself for
  /// an `alloc_stack`, or the `project_box` for an `alloc_box`. For a class instance, it is
  /// instead the `mark_uninitialized` reference whose stored properties are projected.
  let address: Value

  /// The source variable, for diagnostics.
  let varDecl: VarDecl?

  /// The `mark_uninitialized` of the storage, if any; its kind identifies `self` in an initializer.
  let markUninitialized: MarkUninitializedInst?

  let isLexical: Bool

  /// Whether the storage corresponds to a `@noImplicitCopy` parameter.
  let isNoImplicitCopy: Bool

  // Does this address start fully initialized?
  private let startsInitialized: Bool

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

  // Extra instructions to consider as full liveness uses of this root, that are not captured by walking uses of
  // the address.
  let extraLivenessUses: [ExtraLivenessUse]

  // Either the instruction defining this root, or the very first instruction
  // after which it is defined.
  var startInstruction: Instruction {
      if let inst = address.definingInstruction {
          return inst
      }
      return address.nextInstruction
  }

  /// Whether the storage is the instance referenced by `address`, i.e., `self` in a root class initializer.
  var isClassInstance: Bool { !address.type.isAddress }

  /// Whether the binding is a `let` (vs a `var`), which may only be initialized once.
  var isLet: Bool { !(storage is FunctionArgument) && (varDecl?.isLet ?? false) }

  /// `self` in a struct or enum initializer that initializes the stored properties itself.
  var isRootSelf: Bool {
    switch markUninitialized?.kind {
    case .rootSelf?, .crossModuleRootSelf?: true
    default: false
    }
  }

  /// The elements of the storage that may only be initialized once: all of a `let` binding,
  /// or the `let` stored properties of `self` in a struct or class initializer.
  func immutableLeaves(_ space: StateIndexSpace) -> Set<Int> {
    switch markUninitialized?.kind {
    case .variable?, nil:
      return isLet ? Set(space.wholeRange) : []
    case .rootSelf?, .crossModuleRootSelf?:
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
    default:
      return []
    }
  }

  /// Build from a storage allocation (`alloc_box` / `alloc_stack`). Callers holding only
  /// an address must first walk up to the allocation.
  init?(_ val: Value, _ indexCache: inout FieldIndexTrieCache, _ context: FunctionPassContext) {
    let function = val.parentFunction
    var extraUses: [ExtraLivenessUse] = []

    switch val {
    case let allocStack as AllocStackInst:
      storage = val
      markUninitialized = Self.findMarkUninitialized(allocStack)

      if let mu = markUninitialized {
        address = mu

        let indicies = indexCache.fieldIndices(for: storage.type.objectType)

        // If this is the 'self' for an empty type in its initializer, we don't have any fields to initialize.
        // So, treat the storage as being born initialized
        startsInitialized = mu.kind == .rootSelf && !indicies.hasFields

      } else {
        address = storage
        startsInitialized = false
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

      break

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

    default:
      return nil
    }

    extraLivenessUses = extraUses

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

  /// The elements of the dataflow state tracked for this storage.
  func stateIndexSpace(_ indexCache: inout FieldIndexTrieCache) -> StateIndexSpace {
    if isClassInstance {
      return StateIndexSpace(.rootClassSelf, fields: indexCache.fieldIndices(forClassInstance: address.type),
                             initiallyInitialized: startsInitialized)
    }
    return StateIndexSpace(.storage, fields: indexCache.fieldIndices(for: address.type),
                           initiallyInitialized: startsInitialized)
  }

  // Change the storage such that it indicates a dynamic lifetime.
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

  // A stack-backed local `let` or `var`; its `mark_uninitialized [var]` is the address.
  private static func findMarkUninitialized(_ value: Value) -> MarkUninitializedInst? {
    if let mu = value.uses.singleUser(ofType: MarkUninitializedInst.self) {
      return mu
    }
    return nil
  }
}

private struct Resolver {
  let context: FunctionPassContext
  let function: Function

  typealias BlockState = Dictionary<BasicBlock, BlockInfo>
  var state: BlockState

  init(_ function: Function, _ context: FunctionPassContext) {
    self.context = context
    self.function = function
    self.state = [:]
  }

  mutating func reset() {
    // TODO: avoid reallocation by just resetting the BlockInfo's themselves.
    self.state = [:]
  }

  // - Returns: true iff legalization was successful
  mutating func run(on value: Value, _ indexCache: inout FieldIndexTrieCache) -> Bool {
    guard let root = ResolvableRoot(value, &indexCache, context) else {
      return false
    }

    log("\nResolver.run(on: \(value))")
    reset()

    ////////////////
    // Step 1: Canonicalize copies/takes.
    //
    // This step comes before availability analysis, because we must know whether a `load`
    // is a take or not before we can determine what memory locations must be destroyed, etc.

    log("\n** Demand Analysis start **")

    let space = root.stateIndexSpace(&indexCache)
    log("StateIndexSpace of \(root.address.type): \n\(space)")

    // The reference to a class instance is itself an SSA value, whose copies and destroys are resolved first.
    if root.isClassInstance {
      resolveSingleDef(root.address, context)
    }

    // Nothing else about the root needs to be tracked, e.g., a class instance without stored properties.
    if space.isEmpty {
      return true
    }

    self.state = Self.createBlockState(root, space, context)

    // Initialize the demand per block
    for blk in function.blocks {
      state[blk]!.initialDemand()
    }

    logState("\n** state before solveDemand **")
    solveDemand(space)

    logState("\n** state before legalizeDemand **")
    legalizeDemand(for: root, space)

    ////////////////
    // Step 2: Determine availability at lifetime ends, handling partially initialized destroys.

    logState("\n** state before initializing availability **")
    for blk in function.blocks {
      state[blk]!.initialAvailability()
    }

    logState("\n** state before solveAvailability **")
    solveAvailability(space)

    logState("\n** state before legalizeAvailability **")
    legalizeAvailability(root, space)

    logState("\n** final state **")

    log("\n** Demand Analysis Phase finished **")
    return true
  }

  func logState(_ header: String? = nil) {
    if let header {
      log(header)
    }
    for blk in function.blocks {
      guard let info = state[blk] else {
        log("\n!! no info for bb\(blk.index)")
        continue
      }
      log("**bb\(blk.index)**\n\(info)")
    }
  }

  // Solve the backward demand-flow to a fixpoint.
  mutating func solveDemand(_ space: StateIndexSpace) {
    var worklist = BasicBlockWorklist(context)
    defer { worklist.deinitialize() }

    // Seed with every block.
    worklist.pushIfNotVisited(contentsOf: function.blocks)

    // Use popAndForget so a block can be re-enqueued when one of
    // its successors changes later; required to reach a fixpoint across loops.
    while let block = worklist.popAndForget() {
      guard let info = state[block] else { fatalError("no block state?") }

      var exits = space.makeStates(repeating: Demand.nothing)
      let range = info.demand.indices
      assert(exits.count == range.upperBound)

      // exit[i] = ⨆ over successors of the demand they place on field i
      for succ in block.successors {
        if let succInfo = state[succ] {
          for i in range {
            exits[i].formUnion(succInfo.demand[i].entry)
          }
        }
      }

      // entry[i] = gen[i] ∪ (exit[i] \ kill[i])
      for i in range {
        var demand = info.demand[i]
        let exit = exits[i]

        var entry = exit
        entry.subtract(demand.kill)
        entry.formUnion(demand.gen)

        guard entry != demand.entry || exit != demand.exit else {
          continue
        }

        // This block's demand changed. Update it and process its predecessors.
        demand.exit = exit
        demand.entry = entry
        state[block]!.demand[i] = demand
        worklist.pushIfNotVisited(contentsOf: block.predecessors)
      }
    }
  }

  //===--------------------------------------------------------------------===//
  // Legalization
  //===--------------------------------------------------------------------===//

  // Legalize the SIL from the solved demand, leaving markers behind for LifetimeResolutionDiagnose
  // to turn into user-facing diagnostics for any misuses that were corrected by this phase.
  mutating func legalizeDemand(for root: ResolvableRoot, _ space: StateIndexSpace) {
    for block in function.blocks {
      guard var events = state[block]?.events else { continue }
      defer { state[block]!.events = events }

      // Tracks the demand between individual uses within this block as we walk bottom-up.
      // Initialized to the demand starting at the block's exit.
      var current = state[block]!.demand.map { $0.exit }

      for i in events.list.indices.reversed() {
        legalize(event: &events.list[i], successorDemand: &current, root, space)
      }
    }
  }

  // Legalize a single event with respect to the demand reaching it from its successor instruction.
  func legalize(event: inout EventList.Event, successorDemand current: inout [Demand], _ root: ResolvableRoot,
                _ space: StateIndexSpace) {
    let noDemand = current[event.range].allSatisfy { $0 == .nothing }
    let someDemand = !noDemand

    let kind = event.kind
    var newKind: EventList.Kind = kind

    switch kind {
    case .take where someDemand:
      // A consume with demand still below it (use-after-consume): legalize with a copy.
      newKind = transform(kind, to: .load(.copy), root)

    case let .take(op, _):
      adjustAccessForTake(op.value)

    case .use(_, .own) where someDemand:
      // A use demanding ownership, where there is some demand following it, can only be satisfied with a copy.
      newKind = transform(kind, to: .load(.copy), root)

    case .use(_, .own) where noDemand:
      // Canonicalize it into a take, as this is the last use.
      newKind = transform(kind, to: .load(.take), root)

    case .use(_, .borrow) where someDemand:
      // A noncopyable type forces a load_borrow here, since there's demand for the memory
      // after this use no matter what nested accesses we may be nested within.
      if root.address.type.objectType.isMoveOnly {
        newKind = transform(kind, to: .loadBorrow, root)
        break
      }

    // TODO: for Copyable types, we should have a conservative check to see if we're in a nested mutating access
    //       for this memory. If not, then it's proven safe to transform into a load_borrow (rdar://186687340)


    case .use(_, .borrow) where noDemand:
      // No users need ownership of the value, and there is no demand after this use.
      // We could either take or borrow; either way, we should avoid a copy if possible.
      // TODO: If ownership originally was guaranteed, yet we're choosing to do a take,
      //          we'd need to insert destroy_value's on non-consuming paths. It's probably hard to
      //          emit a load_borrow for a copyable type, given access scopes may end.
      newKind = transform(kind, to: .load(.take), root)

    case .root(_, initialized: false) where someDemand:
      // Unsatisfied demand reaching the root means there exists a use-before-init of those leaves.
      let demanded = event.range.filter { current[$0] != .nothing }
      initializeWithUndef(leaves: demanded, of: root, space)

    default:
      break
    }

    // Update the kind, in case we did a transform.
    event.kind = newKind

    // Compose this use's transfer to get the demand above it.
    current[event.range].mutatingEach {
      $0.subtract(event.kind.demandKill)
      $0.formUnion(event.kind.demandGen)
    }
  }

  enum TransformRequest: CustomStringConvertible {
    case load(LoadInst.LoadOwnership)
    case loadBorrow

    var description: String {
      switch self {
      case let .load(kind): return "\(kind)"
      case .loadBorrow: return "borrow"
      }
    }
  }

  func transform(_ kind: EventList.Kind, to request: TransformRequest, _ root: ResolvableRoot) -> EventList.Kind {
    log("requested transform \(root.isLexical ? "(lexical) " : "")to \(request), given \(kind))")

    let newOp: Operand
    let newDem: Demand
    switch kind {
    case let .use(op, dem):
      newOp = transformOperand(op, to: request, wasTake: false, root)
      newDem = dem
    case let .take(op, dem):
      newOp = transformOperand(op, to: request, wasTake: true, root)
      newDem = dem
    default:
      fatalError("unexpected event kind to transform: \(kind)")
    }

    // Reclassify the event based on the transform request.
    if case .load(.take) = request {
      return .take(newOp, newDem)
    }
    return .use(newOp, newDem)
  }

  // Performs the requested transformation.
  // If we're turning a take into a copy, or copying a noncopyable type or `@noImplicitCopy` binding,
  // we add a `diagnose` instruction.
  func transformOperand(_ op: Operand, to request: TransformRequest, wasTake: Bool,
                        _ root: ResolvableRoot) -> Operand {

    // Whether a copy of a value with the given type is unpermitted.
    func isUnpermittedCopy(of type: Type) -> Bool {
      if type.isMoveOnly || wasTake {
        return true
      }

      // If we aren't converting from a take, then we still need to diagnose implicit copies
      // if it was a no-implicit-copy binding.
      return root.isNoImplicitCopy && op.value.projectionPathFromRoot.isEmpty
    }

    switch op.instruction {
    case let apply as FullApplySite:
      // We can only handle transform that provides a copy to the apply.
      guard case .load(.copy) = request, wasTake else {
        fatalError("cannot satisfy transform to \(request) for apply operand \(op.index) of \(apply)")
      }

      // Copy the value into a temporary alloc_stack and pass that to the apply, i.e,
      //    %alloc = alloc_stack
      //    copy_addr %op to [init] %alloc
      //    diagnose %op [unpermitted_copy]
      //    apply(%alloc) : (@in) -> ...
      //    dealloc_stack %alloc
      let type = op.value.type.objectType
      let allocBuilder = Builder(before: apply, context)
      let alloc = allocBuilder.createAllocStack(type)
      allocBuilder.createCopyAddr(from: op.value, to: alloc, takeSource: false, initializeDest: true)
      if isUnpermittedCopy(of: op.value.type) {
        allocBuilder.createDiagnose(operand: op.value, kind: .unpermittedCopy)
      }
      op.set(to: alloc, context)
      Builder(after: apply, context).createDeallocStack(alloc)
      return op

    // copy_addr
    case let copyAddr as CopyAddrInst:
      assert(op.value == copyAddr.source, "only the source is being read!")

      guard case let .load(newOwnership) = request else {
        fatalError("unhandled transform of copy_addr to \(request)")
      }

      let becomesCopy = newOwnership != .take
      copyAddr.set(isTakeOfSource: !becomesCopy, context)

      if becomesCopy && isUnpermittedCopy(of: copyAddr.type) {
        Builder(after: copyAddr, context).createDiagnose(operand: op.value, kind: .unpermittedCopy)
      }

      if !becomesCopy {
        adjustAccessForTake(copyAddr.source)
      }

      return op

    case let load as LoadInst:
      switch request {
      // load [old] --> load [new]
      case let .load(newOwnership):
        load.set(ownership: newOwnership, context)

        if newOwnership == .copy && isUnpermittedCopy(of: load.type) {
          Builder(after: load, context).createDiagnose(operand: load, kind: .unpermittedCopy)
        }

        if newOwnership == .take {
          adjustAccessForTake(load.address)
        }

        return load.operand

      // load [old] --> load_borrow
      case .loadBorrow:
        // Place the new load_borrow right after this load.
        let loadBorrow = Builder(after: load, context).createLoadBorrow(fromAddress: load.address)

        // Build a live range of the current users of the load.
        var loadUsers = PartitionedUses(of: load, context)
        defer { loadUsers.deinitialize() }
        loadUsers.partitionUses(withDeinitBarriers: root.isLexical)

        assert(loadUsers.consumes.isEmpty,
          "Can't transform load with these non-destroy consumers existing: \(loadUsers.consumes)")

        var range = loadUsers.fullLiveRange()
        defer { range.deinitialize() }

        // Insert end_borrow's across the entire liveness boundary of the current load.

        // TODO: this doesn't yet handle accesses that end early:
        //      %a = begin_access [read]
        //      %x = load [copy] %a
        //      end_access %a
        //      use %x
        //  When introducing the load_borrow, we need to sink all outer access scopes of the address too!
        range.alongBoundary(context) { builder in
          builder.createEndBorrow(of: loadBorrow)
        }

        // Replace users of the load with the load_borrow, and then delete the load.
        for use in load.uses {
          if let destroy = use.instruction as? DestroyValueInst {
            context.erase(instruction: destroy)
            continue
          }
          use.set(to: loadBorrow, context)
        }
        context.erase(instruction: load)
        return loadBorrow.operand
      }
    default:
      fatalError("unexpected inst to transform: \(op.instruction)")
    }
  }

  func adjustAccessForTake(_ addr: Value) {
    assert(addr.type.isAddress)

    guard case let .access(beginAccess) = addr.enclosingAccessScope else {
      return
    }

    switch beginAccess.accessKind {
    case .read:
      // If it's static read, we turn it into a deinit.
      // For a dynamic read, it needs to be reinitialized at end-of-access, so make it a modify.
      beginAccess.set(accessKind: beginAccess.isStatic ? .deinit : .modify, context: context)
    case .deinit where beginAccess.enforcement == .dynamic:
      // Only a read or modify can be dynamically enforced.
      beginAccess.set(accessKind: .modify, context: context)
    default:
      break
    }
  }

  // Emit an `assign undef to [init]` of each given leaf of the root, right after the root's definition.
  func initializeWithUndef(leaves: [Int], of root: ResolvableRoot, _ space: StateIndexSpace) {
    let builder = Builder(after: root.startInstruction, context)
    for leaf in leaves {
      space.withElementAddress(forIndex: leaf, from: root.address, builder) { address in
        let undef = Undef.get(type: address.type.objectType, context)
        builder.createAssign(source: undef, destination: address, ownership: .initialize)
      }
    }
  }

  // Forward dataflow solving for the availability of fields, to a fixed-point.
  mutating func solveAvailability(_ space: StateIndexSpace) {
    var worklist = BasicBlockWorklist(context)
    defer { worklist.deinitialize() }

    // Seed with every block.
    worklist.pushIfNotVisited(contentsOf: function.blocks)

    // Use popAndForget so a block can be re-enqueued when one of
    // its predecessors changes later; required to reach a fixpoint across loops.
    while let block = worklist.popAndForget() {
      guard let info = state[block] else { fatalError("no block state?") }

      var entries = space.makeStates(repeating: Availability.unknown)
      let range = info.availability.indices
      assert(entries.count == range.upperBound)

      // entry[i] = ⨆ over predecessors of the availability they provide for field i
      for pred in block.predecessors {
        if let predInfo = state[pred] {
          for i in range {
            entries[i].formUnion(predInfo.availability[i].exit)
          }
        }
      }

      // exit[i] = gen[i] ∪ (entry[i] \ kill[i])
      for i in range {
        var avail = info.availability[i]
        let entry = entries[i]

        var exit = entry
        exit.subtract(avail.kill)
        exit.formUnion(avail.gen)

        guard entry != avail.entry || exit != avail.exit else {
          continue
        }

        // This block's availability changed. Update it and process its successors.
        avail.entry = entry
        avail.exit = exit
        state[block]!.availability[i] = avail
        worklist.pushIfNotVisited(contentsOf: block.successors)
      }
    }
  }

  mutating func legalizeAvailability(_ root: ResolvableRoot, _ space: StateIndexSpace) {
    typealias PartialAvailability = (inst: Instruction, avail: [Availability], range: StateIndexSpace.IndexRange)
    var partialDestroys: [PartialAvailability] = []
    var destroysBeforeInit: [PartialAvailability] = []
    let immutableLeaves = root.immutableLeaves(space)

    // Do a first pass over blocks, collecting any needed partial destroys for a second step.
    for block in function.blocks {
      guard var events = state[block]?.events else { continue }

      // Tracks the availability between individual events within this block as we walk top-down.
      // Initialized to the availability at the block's entry.
      var current = state[block]!.availability.map { $0.entry }

      // - Returns true iff this event's instruction was deleted.
      func legalize(kind: EventList.Kind, range: EventList.IndexRange) -> Bool {
        // Log the computed availability for now.
        let fields = current[range].enumerated()
                .map { "\(range.lowerBound + $0.offset)=\($0.element)" }
                .joined(separator: " ")
        log("availability at [\(fields)] | \(kind.inst)")

        let summary = current[range].reduce(Availability.unknown, { $0.union($1) })

        var didDelete = false

        switch kind {
        case let .def(op):
          // All of `self` may be reassigned, even after initializing its `let` properties.
          let assignsWholeSelf = root.isRootSelf && op.value.projectionPathFromRoot.isEmpty
          if !assignsWholeSelf,
             range.contains(where: { immutableLeaves.contains($0) && current[$0].contains(.yes) }) {
            Builder(before: op.instruction, context).createDiagnose(operand: op.value, kind: .letReinit)
          }

          let nontrivial = space.fieldLeaves(in: range).filter { !space.leafType($0).isTrivial(in: function) }
          if nontrivial.isEmpty {
            // Nothing needs to be destroyed, but LetPropertyLowering relies on knowing the initializations.
            makeDef(op.instruction, initialization: summary == .no)
            break
          }
          let defSummary = nontrivial.reduce(Availability.unknown, { $0.union(current[$1]) })
          assert(defSummary != .unknown, "missing information!")

          switch defSummary {
          case .no:
            makeDef(op.instruction, initialization: true)
          case .yes:
            if !makeDef(op.instruction, initialization: false) {
              destroysBeforeInit.append((op.instruction, current, range))
            }
          default:
            // Only some fields are available, so destroy those first to make this a full initialization.
            destroysBeforeInit.append((op.instruction, current, range))
            makeDef(op.instruction, initialization: true)
          }
        case let .end(op):
          assert(summary != .unknown, "missing information!")

          // If fully available, this lifetime ending instruction can stay as-is.
          if summary == .yes {
            break
          }

          // If completely unavailable (consumed), it's lifetime ended earlier, so delete this one.
          // A class instance must still be deallocated, though.
          if summary == .no && !root.isClassInstance {
            context.erase(instruction: op.instruction)
            didDelete = true
            break
          }

          // Otherwise, it's partially available. It will be replaced with partial destroys later.
          partialDestroys.append((op.instruction, current, range))
          didDelete = true

        default:
          break
        }

        // Compose this event's forward transfer to get the availability below it.
        current[range].mutatingEach {
          $0.subtract(kind.availKill)
          $0.formUnion(kind.availGen)
        }

        return didDelete
      }

      // Process the events for this block, removing any that must be dropped.
      events.list.removeAll(where: legalize)
      state[block]!.events = events
    }

    // Step 2: insert partial destroys where needed.
    if partialDestroys.isEmpty && destroysBeforeInit.isEmpty {
      return
    }

    // Only fields that are conditionally initialized where they must be destroyed need a runtime test.
    var conditionalLeaves = Set<Int>()
    for (_, avail, range) in partialDestroys + destroysBeforeInit {
      conditionalLeaves.formUnion(range.filter { avail[$0] == .partial })
    }

    var controlVar: ControlVariable? = nil
    if !conditionalLeaves.isEmpty {
      controlVar = ControlVariable(for: root, space, context)
    }

    for (destroy, avail, range) in partialDestroys {
      if root.isClassInstance {
        endInstanceLifetime(at: destroy, avail, root, space, controlVar)
        continue
      }
      destroyAvailableFields(avail, range, before: destroy, root, space, controlVar)

      // Everything in range is now uninitialized.
      controlVar?.update(range, initialized: false, before: destroy)
      context.erase(instruction: destroy)
    }

    for (def, avail, range) in destroysBeforeInit {
      destroyAvailableFields(avail, range, before: def, root, space, controlVar)
    }

    if let controlVar {
      trackControlVariable(controlVar, conditionalLeaves)
      root.convertToDynamic(context)
    }
  }

  // Converts a def into an initialization or assignment of its destination.
  // - Returns false iff the def cannot be expressed as an assignment.
  @discardableResult
  func makeDef(_ inst: Instruction, initialization: Bool) -> Bool {
    switch inst {
    case let store as StoreInst:
      if store.storeOwnership != .trivial {
        store.set(ownership: initialization ? .initialize : .assign, context)
      }
    case let assign as AssignInst:
      // TODO: handle `[reinit]` once class initializers are supported.
      if assign.assignOwnership != .reinitialize {
        assign.set(ownership: initialization ? .initialize : .reassign, context)
      }
    case let copyAddr as CopyAddrInst:
      copyAddr.set(isInitializationOfDestination: initialization, context)
    case let tupleAddrConstructor as TupleAddrConstructorInst:
      tupleAddrConstructor.set(isInitializationOfDestination: initialization, context)
    case is FullApplySite:
      return initialization
    default:
      // TODO: handle assign_or_init, store_weak, store_unowned, etc.
      log("unhandled def: \(inst)")
    }
    return true
  }

  // Keep the control variable's bits in sync with the availability of the fields at each event.
  // This must happen after any partial destroys are inserted, so a def's bits are only set after
  // the fields it overwrites have been tested.
  func trackControlVariable(_ controlVar: ControlVariable, _ conditionalLeaves: Set<Int>) {
    for block in function.blocks {
      guard let events = state[block]?.events else { continue }

      for (kind, range) in events.list where range.contains(where: conditionalLeaves.contains) {
        switch kind {
        case .def:
          controlVar.update(range, initialized: true, before: kind.inst)
        case .take, .end:
          controlVar.update(range, initialized: false, before: kind.inst)
        case .root:
          break  // Initialized upon creation.
        case .use, .liveness:
          break
        }
      }
    }
  }

  // Ends the lifetime of a class instance that may not be fully initialized at `destroy`. Unless all of its
  // stored properties turn out to be initialized, in which case the destroy runs its deinit, the initialized
  // ones are destroyed and the instance is deallocated without running its deinit.
  func endInstanceLifetime(at destroy: Instruction, _ avail: [Availability], _ root: ResolvableRoot,
                           _ space: StateIndexSpace, _ controlVar: ControlVariable?) {
    let instance = destroy.operands[0].value
    let partialLeaves = space.fieldRange.filter { avail[$0] == .partial }
    var deallocPoint = destroy
    var deallocAvail = avail

    if !partialLeaves.isEmpty && space.fieldRange.allSatisfy({ avail[$0] != .no }) {
      guard let controlVar else { fatalError("missing control variable for conditional destroy") }

      //   start:        cond_br (test bits), keepBlock, deallocBlock
      //   keepBlock:    destroy_value instance; br cont
      //   deallocBlock: <destroy fields>; dealloc_partial_ref instance; br cont
      //   cont:         <destroy> ...
      let isFullyInitialized = controlVar.testAll(partialLeaves, before: destroy)
      let start = destroy.parentBlock
      let cont = context.splitBlock(before: destroy)
      let keepBlock = context.createBlock(after: start)
      let deallocBlock = context.createBlock(after: keepBlock)

      let keepBuilder = Builder(atEndOf: keepBlock, location: destroy.location, context)
      keepBuilder.createDestroyValue(operand: instance)
      keepBuilder.createBranch(to: cont)

      deallocPoint = Builder(atEndOf: deallocBlock, location: destroy.location, context).createBranch(to: cont)
      Builder(atEndOf: start, location: destroy.location, context)
        .createCondBranch(condition: isFullyInitialized, trueBlock: keepBlock, falseBlock: deallocBlock)

      // A sole conditionally initialized property is known to be uninitialized when deallocating.
      if partialLeaves.count == 1 {
        deallocAvail[partialLeaves[0]] = .no
      }
    }

    destroyAvailableFields(deallocAvail, space.fieldRange, before: deallocPoint, root, space, controlVar)
    let builder = Builder(before: deallocPoint, location: destroy.location, context)
    let metatype = builder.createMetatype(ofInstanceType: instance.type.canonicalType, representation: .thick)
    builder.createDeallocPartialRef(instance, metatype: metatype)
    context.erase(instruction: destroy)
  }

  // Destroys only the available fields of partially available memory, testing the control variable
  // for fields that are only conditionally available.
  func destroyAvailableFields(_ current: [Availability], _ range: EventList.IndexRange, before inst: Instruction,
                              _ root: ResolvableRoot, _ space: StateIndexSpace, _ controlVar: ControlVariable?) {
    log("destroying available fields before \(inst)")

    for leaf in space.fieldLeaves(in: range) {
      if current[leaf] == .no || space.leafType(leaf).isTrivial(in: function) {
        continue
      }

      if current[leaf] == .yes {
        let builder = Builder(before: inst, context)
        space.withElementAddress(forIndex: leaf, from: root.address, builder) { builder.createDestroyAddr(address: $0) }
        continue
      }

      assert(current[leaf] == .partial)
      guard let controlVar else { fatalError("missing control variable for conditional destroy") }

      // Split the block into a diamond around the destroy of this field:
      //
      //   start:  cond_br (test bit), trueBlock, falseBlock
      //   trueBlock:  destroy_addr field; br cont
      //   falseBlock: br cont
      //   cont:  <inst> ...
      let condition = controlVar.test(leaf, before: inst)
      let start = inst.parentBlock
      let cont = context.splitBlock(before: inst)
      let trueBlock = context.createBlock(after: start)
      let falseBlock = context.createBlock(after: trueBlock)

      let trueBuilder = Builder(atEndOf: trueBlock, location: inst.location, context)
      space.withElementAddress(forIndex: leaf, from: root.address, trueBuilder) {
        trueBuilder.createDestroyAddr(address: $0)
      }
      trueBuilder.createBranch(to: cont)

      Builder(atEndOf: falseBlock, location: inst.location, context).createBranch(to: cont)
      Builder(atEndOf: start, location: inst.location, context)
        .createCondBranch(condition: condition, trueBlock: trueBlock, falseBlock: falseBlock)
    }
  }

  // Classifies every use of the root's address, following projections and access scopes, into the events of each block.
  private struct UseCollector : AddressDefUseWalker, AddressUseVisitor, ClassInstanceUseWalker {
    private(set) var uses: Dictionary<BasicBlock, EventList> = [:]
    private let space: StateIndexSpace
    private let function: Function
    let context: Context
    private let localReachabilityCache = LocalVariableReachabilityCache()

    init(_ space: StateIndexSpace, _ function: Function, _ context: Context) {
      self.space = space
      self.function = function
      self.context = context
    }

    mutating func addUse(_ k:  EventList.Kind, _ range: EventList.IndexRange) {
      log("\taddUse of \(range) as \(k)")
      let blk = k.inst.parentBlock
      uses[blk, default: EventList(blk)].add(k, range)
    }

    mutating func addLiveness(_ operand: Operand) {
      addUse(.liveness(operand.instruction), range(of: operand))
    }

    // The elements of the root accessed by an operand.
    func range(of operand: Operand) -> EventList.IndexRange {
      space.range(ofFieldPath: operand.value.projectionPathFromRoot)
    }

    mutating func collectUses(of root: ResolvableRoot) -> Bool {
      let address = root.address
      log("** starting collectUses on \(address) **")

      // Add the root use to do detect uninitialized paths.
      for (range, initialized) in space.initialRanges {
        addUse(.root(root, initialized: initialized), range)
      }

      let result = root.isClassInstance ? walkDownUses(ofClassInstance: address)
                                        : walkDownUses(ofAddress: address, path: Path())
      if result == .abortWalk {
        return false
      }

      // Add any extra uses specific to this root.
      root.extraLivenessUses.forEach { addUse(.liveness($0.instruction), space.wholeRange) }

      // Ensure correct order within each block
      for blk in uses.keys {
        uses[blk]?.finalize()
      }

      return true
    }

    // Conservatively keep the memory initialized wherever a value depending on it is still used.
    mutating func addDependentUses(of markDep: MarkDependenceInstruction, _ range: EventList.IndexRange) {
      var dependentUses = Stack<Instruction>(context)
      defer { dependentUses.deinitialize() }
      if !gatherDependentUses(of: markDep, into: &dependentUses, localReachabilityCache, context) {
        addFunctionExits(range)
      }
      for use in dependentUses {
        addUse(.liveness(use), range)
      }
    }

    mutating func addFunctionExits(_ range: EventList.IndexRange) {
      for block in function.exitingBlocks {
        addUse(.liveness(block.terminator), range)
      }
    }

    // Implement ClassInstanceUseWalker

    mutating func storedPropertyUse(_ projection: RefElementAddrInst) -> WalkResult {
      return walkDownUses(ofAddress: projection, path: Path())
    }

    mutating func instanceUse(_ use: Operand) -> WalkResult {
      if use.instruction is DestroyValueInst {
        addUse(.end(use), space.instanceRange)
      } else {
        addUse(.liveness(use.instruction), space.instanceRange)
      }
      return .continueWalk
    }

    // Implement AddressDefUseWalker

    mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
      log("leafUse of \(range(of: address)) in \t\(address.instruction)")
      // Only an unrecognized builtin aborts the classification.
      if classifyAddress(operand: address) == .abortWalk {
        addLiveness(address)
      }
      return .continueWalk
    }

    // The base of a mark_dependence on the root, one of its projections or accesses.
    mutating func unmatchedPath(address: Operand, path: UnusedWalkingPath) -> WalkResult {
      return leafUse(address: address, path: path)
    }

    // Implement AddressUseVisitor

    mutating func loadedAddressUse(of operand: Operand, intoValue value: Value) -> WalkResult {
      guard let load = operand.instruction as? LoadInst else {
        addLiveness(operand)
        return .continueWalk
      }
      // If the loaded SSA value is consumed by some non-destroying instruction, its demand is "own".
      func hasConsumingUser(_ op: Operand) -> Bool {
        return op.ownership == .forwardingConsume || op.instruction is MoveValueInst
      }
      let range = self.range(of: operand)
      switch load.loadOwnership {
      case .take:
          addUse(.take(operand, .own), range)   // TODO: what about a take where all non-destroy users borrow?
      case .unqualified, .copy:
        if load.uses.filter(hasConsumingUser).isEmpty {
          addUse(.use(operand, .borrow), range)
        } else {
          addUse(.use(operand, .own), range)
        }
      case .trivial:
        // We cannot meaningfully transform `load [trivial]` into a take or copy, as it currently has no ownership.
        // Treat it as a generic use.
        addUse(.liveness(load), range)
      }
      return .continueWalk
    }

    mutating func loadedAddressUse(of operand: Operand, intoAddress address: Operand) -> WalkResult {
      let srcDestInst = operand.instruction as! SourceDestAddrInstruction
      addUse(srcDestInst.isTakeOfSource ? .take(operand, .own) : .use(operand, .own), range(of: operand))
      return .continueWalk
    }

    mutating func leafAddressUse(of operand: Operand) -> WalkResult {
      switch operand.instruction {
      case let store as StoringInstruction where store.destination == operand.value:
        addUse(.def(operand), range(of: operand))
      case let tac as TupleAddrConstructorInst where tac.destinationOperand == operand:
        addUse(.def(operand), range(of: operand))
      case let srcDestInst as SourceDestAddrInstruction where srcDestInst.destinationOperand == operand:
        addUse(.def(operand), range(of: operand))
      case is DestroyAddrInst:
        addUse(.end(operand), range(of: operand))
      case is DeallocStackInst:
        break
      default:
        addLiveness(operand)
      }
      return .continueWalk
    }

    mutating func appliedAddressUse(of operand: Operand, by apply: FullApplySite) -> WalkResult {
      switch apply.convention(of: operand) {
      case .indirectOut:
        addUse(.def(operand), range(of: operand))
      case .indirectIn:
        addUse(.take(operand, .own), range(of: operand))
      default:
        addLiveness(operand)
      }
      return .continueWalk
    }

    mutating func scopedAddressUse(of operand: Operand) -> WalkResult {
      let range = self.range(of: operand)
      switch operand.instruction {
      case let loadBorrow as LoadBorrowInst:
        // The borrowed value aliases the memory until the end of its scope.
        addUse(.liveness(loadBorrow), range)
        for use in loadBorrow.uses {
          if use.instruction is EndBorrowInst {
            addUse(.liveness(use.instruction), range)
          } else if let markDep = use.instruction as? MarkDependenceInstruction, use == markDep.baseOperand {
            addDependentUses(of: markDep, range)
          } else if use.endsLifetime {
            // TODO: follow the reborrow to the end_borrows of its phi, rather than conservatively
            //       keeping the memory initialized until the function exits.
            addFunctionExits(range)
          }
        }
      case is StoreBorrowInst:
        addUse(.def(operand), range)
      default:
        addLiveness(operand)
      }
      return .continueWalk
    }

    mutating func dependentAddressUse(of operand: Operand, dependentValue value: Value) -> WalkResult {
      return dependentAddressUse(of: operand)
    }

    mutating func dependentAddressUse(of operand: Operand, dependentAddress address: Value) -> WalkResult {
      return dependentAddressUse(of: operand)
    }

    private mutating func dependentAddressUse(of operand: Operand) -> WalkResult {
      if let markDep = operand.instruction as? MarkDependenceInstruction {
        addDependentUses(of: markDep, range(of: operand))
      } else {
        addLiveness(operand)
      }
      return .continueWalk
    }

    // Includes the base of a mark_dependence producing an address.
    mutating func projectedAddressUse(of operand: Operand, into value: Value) -> WalkResult {
      return dependentAddressUse(of: operand)
    }

    mutating func escapingAddressUse(of operand: Operand) -> WalkResult {
      if operand.instruction is MarkDependenceInstruction {
        addFunctionExits(range(of: operand))
      } else {
        addLiveness(operand)
      }
      return .continueWalk
    }

    mutating func scopeEndingAddressUse(of operand: Operand) -> WalkResult {
      addLiveness(operand)
      return .continueWalk
    }

    mutating func yieldedAddressUse(of operand: Operand) -> WalkResult {
      addLiveness(operand)
      return .continueWalk
    }

    mutating func unknownAddressUse(of operand: Operand) -> WalkResult {
      addLiveness(operand)
      return .continueWalk
    }
  }

  static func createBlockState(_ root: ResolvableRoot, _ space: StateIndexSpace,
                               _ context: FunctionPassContext) -> BlockState {
    let function = root.address.parentFunction
    var uc = UseCollector(space, function, context)
    guard uc.collectUses(of: root) else {
      fatalError("collectUses aborted its walk?")
    }

    var state: BlockState = [:]

    let emptyDemand = space.makeStates(repeating: BlockDemand())
    let emptyAvailability = space.makeStates(repeating: BlockAvailability())

    // Initialize an empty block state along with the accesses for that particular block.
    // We need state for all blocks, so information transits across them.
    for block in function.blocks {
      let uses = uc.uses[block]
      state[block] = BlockInfo(emptyDemand, emptyAvailability, uses)
    }

    return state
  }

  // What kind of ownership is demanded by a successor instruction or block?
  struct BlockDemand: CustomStringConvertible {
    var entry: Demand = .nothing // What is the ownership demanded by this block of its predecessors?
    var gen: Demand = .nothing   // What demand is generated within this block?
    var kill: Demand = .nothing  // What demand is satisfied within this block?
    var exit: Demand = .nothing  // What is the ownership demanded from this block by successors?

    var description: String {
      return "[entry: \(entry), gen: \(gen), kill: \(kill), exit: \(exit)]"
    }
  }

  // What is the initialization state of a field within this block?
  // The forward dual of BlockDemand: information flows from predecessors to successors.
  struct BlockAvailability: CustomStringConvertible {
    var entry: Availability = .unknown // Availability flowing in from predecessors.
    var gen: Availability = .unknown   // Availability produced within this block (reaches exit).
    var kill: Availability = .unknown  // Availability overwritten within this block.
    var exit: Availability = .unknown  // Availability flowing out to successors.

    var description: String {
      return "[entry: \(entry), gen: \(gen), kill: \(kill), exit: \(exit)]"
    }
  }

  /// Represents the state of ownership demanded during the backwards demand analysis.
  ///        All
  ///        / \
  ///   Borrow  Own
  ///        \ /
  ///        Nothing
  struct Demand: OptionSet, CustomStringConvertible {
    let rawValue: UInt8

    static let borrow = Demand(rawValue: 1 << 0)  // 0b01
    static let own  = Demand(rawValue: 1 << 1)  // 0b10

    static let nothing: Demand = []                // ⊥
    static let all: Demand     = [.borrow, .own] // ⊤

    var description: String {
      switch self {
      case .nothing: "∅"
      case .borrow:  "β"
      case .own:   "ω"
      case .all:     "⊤"
      default: fatalError("unknown bits: \(rawValue)")
      }
    }
  }

  /// Represents whether a field is initialized during the forward availability analysis.
  ///       Partial
  ///        / \
  ///     Yes   No
  ///        \ /
  ///      Unknown
  struct Availability: OptionSet, CustomStringConvertible {
    let rawValue: UInt8

    static let yes = Availability(rawValue: 1 << 0)  // 0b01 : definitely initialized
    static let no  = Availability(rawValue: 1 << 1)  // 0b10 : definitely uninitialized

    static let unknown: Availability = []          // ⊥
    static let partial: Availability = [.yes, .no] // ⊤ : initialized on some paths, not others

    var description: String {
      switch self {
      case .unknown: "?"
      case .yes:     "✓"
      case .no:      "✗"
      case .partial: "⊤"
      default: fatalError("unknown bits: \(rawValue)")
      }
    }
  }

  // Classified memory events within a BasicBlock.
  struct EventList: CustomStringConvertible {
    typealias IndexRange = StateIndexSpace.IndexRange
    typealias Event = (kind: Kind, range: IndexRange)

    private let block: BasicBlock

    var list: [Event] // All of the categorized uses within this block.
    private(set) var finalized = false

    init(_ blk: BasicBlock) {
      self.block = blk
      self.list = []
    }

    var description: String {
      return list.map { "\($0.range.description) \($0.kind.description)" }
              .joined(separator: "\n")
    }

    enum Kind: CustomStringConvertible {
      case root(ResolvableRoot, initialized: Bool)  // Whether the elements in range are initialized at the start.
      case use(Operand, Demand)         // The demand summarizes the kinds of users of this operand's instruction.
      case take(Operand, Demand)        // The demand summarizes the kinds of users of this operand's instruction.
      case def(Operand)
      case end(Operand)       // An instruction representing the point at which the operand's lifetime has ended.
      case liveness(Instruction)       // A non-specific liveness use.

      var inst: Instruction {
        switch self {
        case let .root(r, _): r.startInstruction
        case let .use(op, _): op.instruction
        case let .take(op, _): op.instruction
        case let .def(op): op.instruction
        case let .end(op): op.instruction
        case let .liveness(inst): inst
        }
      }

      // What is the ownership demanded by this use?
      var demandGen: Demand {
        switch self {
        case let .use(_, dem): dem
        case let .take(_, dem): dem
        case .def, .root: .nothing
        case .end: .nothing  // Demand analysis ignores lifetime ends.
        case .liveness: .all
        }
      }

      // What ownership does this use satisfy?
      var demandKill: Demand {
        switch self {
        case .def: .all
        case .root(_, initialized: true): .all // A root that starts initialized is a def.
        default: .nothing
        }
      }

      // What availability does this event establish? (forward analysis)
      var availGen: Availability {
        switch self {
        case .def: .yes
        case .root(_, initialized: true): .yes  // A root that starts initialized is a def.
        case .root, .take, .end: .no    // these leave the operand uninitialized.
        case .use: .unknown       // doesn't change the state
        case .liveness: .unknown
        }
      }

      // What prior availability does this event overwrite?
      var availKill: Availability {
        switch self {
        case .def, .root, .take, .end: .partial    // Overwites all availability.
        case .use, .liveness: .unknown
        }
      }

      var description: String {
        let name: String =
        switch self {
        case let .use(_, dem): "use(\(dem))"
        case let .take(_, dem): "take(\(dem))"
        case .root: "root"
        case .def: "def"
        case .end: "end"
        case .liveness: "liveness"
        }

        return name + " | \(inst)"
      }
    }

    mutating func add(_ k: Kind, _ range: IndexRange) {
      assert(k.inst.parentBlock == block)
      list.append((kind: k, range: range))
    }

    // Ensure uses are in a faithful block-local order.
    mutating func finalize() {
      guard !finalized else { return }
      list.sort(by: { $0.kind.inst.strictlyDominatesInBlock($1.kind.inst) })
      finalized = true
    }
  }

  struct BlockInfo: CustomStringConvertible {
    var events: EventList?    // Uses appearing within this block, if any.
    var demand: Array<BlockDemand>
    var availability: Array<BlockAvailability>

    init(_ demand: Array<BlockDemand>, _ avail: Array<BlockAvailability>, _ useList: EventList? = nil) {
      assert(!demand.isEmpty)
      if let provided = useList {
        assert(!provided.list.isEmpty, "You should pass nil if there are no uses.")
      }

      self.events = useList
      self.demand = demand
      self.availability = avail
    }

    mutating func initialDemand() {
      guard let events else { return }
      assert(events.finalized, "AccessList needs to be finalized!")

      // Scan backwards over the uses within this block to compute its 'gen' bits, so that entry = gen ∪ (exit \ kill).
      for (use, range) in events.list.reversed() {
        demand[range].mutatingEach {
          $0.gen.subtract(use.demandKill)     // a def above wipes demand generated below it
          $0.gen.formUnion(use.demandGen)     // this use's own demand survives to the top
          $0.kill.formUnion(use.demandKill)
          $0.entry = $0.gen
        }
      }
    }

    mutating func initialAvailability() {
      guard let events else { return }
      assert(events.finalized, "AccessList needs to be finalized!")

      // Scan forwards so that exit = gen ∪ (entry \ kill).
      for (event, range) in events.list {
        availability[range].mutatingEach {
          $0.gen.subtract(event.availKill)   // a def/take below overwrites what was produced above it
          $0.gen.formUnion(event.availGen)   // this event's own availability survives to the bottom
          $0.kill.formUnion(event.availKill)
          $0.exit = $0.gen
        }
      }
    }

    var description: String {
      return """
             BlockInfo {
             demand: {\n\(demand.enumerated().map { "\($0.offset) | \($0.element)" }.joined(separator: "\n"))\n}
             availability: {\n\(availability.enumerated().map { "\($0.offset) | \($0.element)" }.joined(separator: "\n"))\n}
             uses: {
             \(events?.description ?? "nil")
             }
             }
             """
    }
  }
}

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

// Gathers the uses of values depending on `markDep`. Returns false if they can't all be found.
private func gatherDependentUses(of markDep: MarkDependenceInstruction, into uses: inout Stack<Instruction>,
                                 _ localReachabilityCache: LocalVariableReachabilityCache,
                                 _ context: Context) -> Bool {
  // The walker only tracks ~Escapable and @noescape dependents; for anything else it reports
  // success having collected nothing.
  guard let dependence = LifetimeDependence(markDep, context),
        !dependence.dependentValue.mayEscape else {
    return false
  }
  var dependentUses = Stack<Instruction>(context)
  defer { dependentUses.deinitialize() }
  var walker = LifetimeDependentUseWalker(markDep.parentFunction, localReachabilityCache, context) {
    dependentUses.push($0)
    return .continueWalk
  }
  defer { walker.deinitialize() }
  let result = walker.walkDown(dependence: dependence)
  uses.append(contentsOf: dependentUses)
  return result == .continueWalk
}

// Static-single assignment values (SSA), aka objects in SIL, are defined exactly once, so we
// use a simpler, cheaper analysis for such values instead of the storage-backed one above.
private func resolveSingleDef(_ root: Value, _ context: FunctionPassContext) {
  assert(!root.type.isAddress, "addresses are multi-def")
  log("\n\nLifetimeResolution.resolveSingleDef(\(root))\n\n")

  var uses = PartitionedUses(of: root, context)
  defer { uses.deinitialize() }

  // Step 0: Find and partition uses of the values.
  let isLexical = root.isInLexicalLiverange(context)
  uses.partitionUses(withDeinitBarriers: isLexical)

  log(uses.description)

  // If there are absolutely no liveness uses, there's nothing to do.
  guard uses.haveLivenessUses() else {
    // TODO: Eventually this pass should ensure a destroy is placed right after the def.
    return
  }

  // Step 1: Resolve where copies are required to ensure there are no consumes before uses.
  uses.insertCopies()

  // Step 2: Insert destroys after non-consuming boundary users.
  uses.fixupDestroys()
}

private struct PartitionedUses: CustomStringConvertible {
  // The uses of this value that are partitioned.
  let root: Value

  let context: FunctionPassContext

  // Lifetime delimiting instructions.
  //
  // These instructions represent the limits of permitted liveness for the value,
  // if otherwise not consumed upon reaching the instruction.
  // TODO: these should be some sort of new end_scope instruction that are used to limit how late
  //  a destroy/end_access can be inserted.
  var lifetimeLimits: Stack<DestroyValueInst>

  // Uses that require ownership of the value (not guaranteed).
  var consumes: Stack<Operand>

  // These are indirect uses of the root, including deinit barriers and dependent uses.
  var indirectUses: Stack<Instruction>

  // Uses that otherwise do not fit into the other buckets.
  var uses: Stack<Operand>

  // Set when some use's contribution to liveness could not be bounded, so that liveness must be
  // fully extended to lifetimeLimits, rather than hoisted above an unseen use.
  var hasUnboundedUse = false

  let localReachabilityCache = LocalVariableReachabilityCache()

  init(of root: Value, _ context: FunctionPassContext) {
    self.root = root
    self.context = context
    self.consumes = Stack(context)
    self.lifetimeLimits = Stack(context)
    self.uses = Stack(context)
    self.indirectUses = Stack(context)
  }

  mutating func deinitialize() {
    consumes.deinitialize()
    lifetimeLimits.deinitialize()
    uses.deinitialize()
    indirectUses.deinitialize()
  }

  // After partitioning, are there any non-destroy uses?
  func haveLivenessUses() -> Bool { !uses.isEmpty || !indirectUses.isEmpty || !consumes.isEmpty }

  var description: String {
    return """
           PartitionedUses of: \(root)) [
             uses = \(uses)
             consumes = \(consumes)
             indirectUses = \(indirectUses)
             hasUnboundedUse = \(hasUnboundedUse)
           ]
           """
  }

  mutating func partitionUses(withDeinitBarriers addBarriers: Bool) {
    if root.hasDirectPointerEscape {
      hasUnboundedUse = true
    }

    // TODO: should we use an InteriorUseWalker or some other robust walker?
    for use in root.uses {
      switch (use.ownership) {
      case .destroyingConsume:
        fallthrough
      case .forwardingConsume:
        // Perhaps Operand.isScopeEndingUse or Operand.endsLifetime is also useful here?
        if let destroy = use.instruction as? DestroyValueInst {
          lifetimeLimits.append(destroy)
          continue
        }

        consumes.append(use)

      default:
        collectUse(use)
      }
    }

    // Only lexical roots need to include deinit barriers.
    // TODO: study https://gist.github.com/atrick/cc03c4d07fb0a7bee92c223ae5e5695b and the current implementation
    //   to tailor destroy insertion correctly for non-copyable types.
    if addBarriers {
      addDeinitBarriers(of: root)
    }
  }

  // A use that opens a scope keeps the value live until that scope closes, so record the
  // scope-ending uses in its place: they post-dominate the opening use, so they alone
  // delimit the liveness it contributes.
  private mutating func collectUse(_ use: Operand) {
    let borrowInst = BorrowingInstruction(use.instruction)
    if let borrowInst = borrowInst,
       collectScopeEnds(of: borrowInst) {
      return
    }

    uses.append(use)

    if let markDep = use.instruction as? MarkDependenceInstruction,
       use == markDep.baseOperand {
      collectDependentUses(of: markDep)
    } else if borrowInst != nil {
      // The scope can't be determined from lifetime-ending uses and isn't a dependence we can
      // walk, so liveness past this use is unknown.
      hasUnboundedUse = true
    }
  }

  // Records the instructions closing borrowInst's scope. Returns true if we were able to find
  // all scope ends. Otherwise, there may be escaping dependency or unhandled mark_dependence.
  private mutating func collectScopeEnds(of borrowInst: BorrowingInstruction) -> Bool {
    // TODO: remove this stack by changing visitScopeEndingOperands to take a non-escaping
    // closure, as visitInnerBorrowUses also wants.
    var ends = Stack<Instruction>(context)
    defer { ends.deinitialize() }
    let result = borrowInst.visitScopeEndingOperands(context) {
      ends.push($0.instruction)
      return .continueWalk
    }
    guard result == .continueWalk else {
      return false
    }
    indirectUses.append(contentsOf: ends)
    return true
  }

  // The value depending on `markDep` keeps the root alive through its own uses, which are not
  // uses of the root. Record them so that liveness respects them.
  private mutating func collectDependentUses(of markDep: MarkDependenceInstruction) {
    if !gatherDependentUses(of: markDep, into: &indirectUses, localReachabilityCache, context) {
      hasUnboundedUse = true
    }
  }

  private mutating func addDeinitBarriers(of root: Value) {
    var liverange = InstructionRange(for: root, context)
    defer { liverange.deinitialize() }

    liverange.insert(contentsOf: consumes.users)
    liverange.insert(contentsOf: uses.users)
    // FIXME: this should add `indirectUses`. We also need a cache of information about the dependent values's uses, to
    //  effectively extend the range of instructions to be considered deinit barriers so it includes all
    //  dependent value's lifetimeLimits too.

    collectDeinitBarriers(into: &indirectUses, liverange: liverange,
      lifetimeLimits: lifetimeLimits, def: root, context)
  }

  // Compute the full range in which the root must be kept live.
  func fullLiveRange() -> InstructionRange {
    // TODO: cache this liverange to avoid recomputing it for callers?
    var liverange = InstructionRange(for: root, context)

    // Omit the destroys from the liverange during copy resolution. They're hoisted afterwards.
    liverange.insert(contentsOf: consumes.users)
    liverange.insert(contentsOf: uses.users)
    liverange.insert(contentsOf: indirectUses)  // Deinit barriers must be treated as uses.
    if hasUnboundedUse {
      // Liveness past an unbounded use is unknown, so the extend liveness to scope-ends.
      liverange.insert(contentsOf: lifetimeLimits)
    }
    return liverange
  }

  // We do this by computing the LiveRange of only the non-destroy / non-scope-ending
  // uses. This lets us see which consuming uses are within the live range, rather
  // than on the boundary of its liveness (i.e., last use). Those inner consuming uses
  // are exactly where copies are required.
  //
  //                  ┌─────────┐
  //               ┌──┼ x = ... ┼──┐
  //               │  └─────────┘  │
  //               │               │
  //            ┌──▼───────┐  ┌────▼─────┐
  // boundary ─►│consume(x)│  │consume(x)│
  //            └──────┬───┘  │use(x)    │◄─ boundary
  //                   │      └─┬────────┘
  //                   │        │
  //                  ┌▼────────▼┐
  //                  │destroy(x)│
  //                  └──────────┘
  //
  //                       │  After copy resolution, consumes appear
  //                       │  only on the boundary, if at all.
  //                       ▼
  //
  //                  ┌─────────┐
  //               ┌──┼ x = ... ┼──┐
  //               │  └─────────┘  │
  //               │               │
  //            ┌──▼───────┐  ┌────▼──────┐
  // boundary ─►│consume(x)│  │y = copy(x)│
  //            └──────┬───┘  │consume(y) │
  //                   │      │use(x)     │◄─ boundary
  //                   │      └─┬─────────┘
  //                   │        │
  //                  ┌▼────────▼┐
  //                  │destroy(x)│
  //                  └──────────┘
  //
  // Crucially, the live range of certain roots includes deinit barriers,
  // which causes some final consumes to need a copy anyway:
  //
  // Liveness before insertion for some lexical roots:
  //       ┌─────────┐
  //    ┌──┼ x = ... ┼──┐
  //    │  └─────────┘  │
  //    │               │
  // ┌──▼───────┐  ┌────▼─────┐
  // │consume(x)│  │use(x)    │
  // └──────┬───┘  └─┬────────┘
  //        │        │
  //        │        │
  //       ┌▼────────▼┐
  //       │barrier() │ ◄─ liveness boundary of x
  //       │destroy(x)│
  //       └──────────┘
  //
  //
  // Since the consume is interior with respect to this
  // deinit-barrier extended liveness, it consumes a copy instead:
  //        ┌─────────┐
  //     ┌──┼ x = ... ┼──┐
  //     │  └─────────┘  │
  //     │               │
  //  ┌──▼───────┐  ┌────▼─────┐
  //  │y = copy x│  │use(x)    │
  //  │consume(y)│  └─┬────────┘
  //  └──────┬───┘    │
  //         │        │
  //        ┌▼────────▼┐
  //        │barrier() │ ◄─ boundary
  //        │destroy(x)│
  //        └──────────┘
  mutating func insertCopies() {
    var liverange = fullLiveRange()
    defer { liverange.deinitialize() }
    log("liverange during insertCopies:\n\(liverange)")

    // If there's no consumes, there's no copies to insert.
    if consumes.isEmpty {
      return
    }

    // Convert the set of *all* consumes into a set that only contains *boundary* consumes.
    var boundaryConsumes: Stack<Operand> = Stack(context)
    while let cons = consumes.pop() {
      // TODO: how to handle consume and use within the same instruction efficiently?
      // It's effectively when the instructions among the 'consumes' overlap with each other, or with any overlap with
      // the instructions in the 'consumes' set.
      if liverange.contains(cons.instruction) {
        log("will insert a copy for operand: \(cons) to convert this into a non-consuming use: \(cons.instruction)")
        replaceWithCopy(cons, context)
        uses.append(cons)
        continue
      }

      boundaryConsumes.push(cons)
    }
    consumes.deinitialize()
    consumes = boundaryConsumes
  }

  // Insert destroy_value instructions along the boundary of the live range, anywhere the value
  // would exit the live range unconsumed: block exits, after last uses with respect to deinit barriers.
  func fixupDestroys() {
    assert(root.ownership == .owned, "fixupDestroys for non-owned root: \(root)")

    guard !hasUnboundedUse else {
      // TODO: for now, trust SILGen's placement of destroy_value.
      //  We probably should extend liveness until scope-ends.
      log("not placing destroys: liveness is not bounded")
      return
    }

    var liverange = fullLiveRange()
    defer { liverange.deinitialize() }
    log("liverange during fixupDestroys:\n\(liverange)")

    // These are just the existing destroy_values that we might
    // re-use if they're already in the right places.
    var hoistableDestroys: InstructionSet = InstructionSet(context)
    defer { hoistableDestroys.deinitialize() }
    hoistableDestroys.insert(contentsOf: lifetimeLimits)

    log("replacing destroys: \(hoistableDestroys)")
    placeDestroys(of: root, atBoundaryOf: liverange, reusing: &hoistableDestroys, context)
  }
}


fileprivate extension InstructionRange {
  func alongBoundary(_ context: FunctionPassContext, insertFunc: (Builder) -> ()) {
    for endInst in ends {
      Builder.insert(after: endInst, context, insertFunc: insertFunc)
    }
    for exitBlock in exitBlocks {
      let builder = Builder(atBeginOf: exitBlock, context)
      insertFunc(builder)
    }
  }
}



// Collects all deinit barriers that exist on any path from the given lifetimeLimits towards the
// definition, stopping at the boundary of `liverange`.
private func collectDeinitBarriers(
  into barriers: inout Stack<Instruction>,
  liverange: InstructionRange,
  lifetimeLimits: Stack<DestroyValueInst>,
  def: Value,
  _ context: FunctionPassContext
) {
  log("liverange during collectDeinitBarriers:\n\(liverange)")

  let calleeAnalysis = context.calleeAnalysis
  let defInst = def.definingInstruction
  let defBlock = def.parentBlock

  enum ScanResult {
    case foundBarrier(Instruction)
    case hitBoundary
    case exhausted
  }

  // Scans backward within a single block, starting at (and including) `first`.
  // A block argument has no defining instruction, so running out of instructions in its
  // block is itself the boundary.
  func scan(from first: Instruction?, in block: BasicBlock) -> ScanResult {
    for inst in ReverseInstructionList(first: first) {
      if inst == defInst || liverange.inclusiveRangeContains(inst) {
        return .hitBoundary
      }
      if inst.isDeinitBarrier(calleeAnalysis) {
        return .foundBarrier(inst)
      }
    }
    return block == defBlock ? .hitBoundary : .exhausted
  }

  var worklist = BasicBlockWorklist(context)
  defer { worklist.deinitialize() }

  for destroy in lifetimeLimits {
    // Scan the block containing this lifetime limit backwards, stopping the first time we find either
    //  - a deinit barrier
    //  - a current boundary of liveness
    switch scan(from: destroy.previous, in: destroy.parentBlock) {
    case .foundBarrier(let barrier):
      barriers.append(barrier)
      continue
    case .hitBoundary:
      continue
    case .exhausted:
      break
    }

    // If the same block had no barrier or liveness boundary; keep walking
    // backward into unvisited predecessor blocks.
    worklist.pushIfNotVisited(contentsOf: destroy.parentBlock.predecessors)

    while let block = worklist.pop() {
      switch scan(from: block.terminator, in: block) {
      case .foundBarrier(let barrier):
        barriers.append(barrier)
      case .hitBoundary:
        break
      case .exhausted:
        worklist.pushIfNotVisited(contentsOf: block.predecessors)
      }
    }
  }
}

// Given an operand in the consume set, convert its use into a copy.
private func replaceWithCopy(_ op: Operand, _ context: FunctionPassContext) {
  let builder = Builder(before: op.instruction, context)
  let copyValue = builder.createCopyValue(operand: op.value)

  // If we're creating a copy of a noncopyable type, or forced to provide a copy
  // to a `move_value [allows_diagnostics]`, then it must be diagnosed.
  if op.value.type.isMoveOnly {
    builder.createDiagnose(operand: copyValue, kind: .unpermittedCopy)
  } else if let move = op.instruction as? MoveValueInst,
            move.allowsDiagnostics {
    builder.createDiagnose(operand: copyValue, kind: .unpermittedCopy)
  }

  op.set(to: copyValue, context)
}

extension Value {
  /// Whether a use lets a pointer to this value escape, so that its liveness can't be derived from its uses.
  /// An escape nested within a borrow scope is bounded by that scope instead.
  var hasDirectPointerEscape: Bool {
    uses.contains { $0.ownership == .pointerEscape || $0.ownership == .forwardingUnowned }
  }
}

private extension MutableCollection {
  mutating func mutatingEach(_ body: (inout Element) -> Void) {
    var i = startIndex
    while i != endIndex {
      body(&self[i])
      i = index(after: i)
    }
  }
}

private extension Function {
  // NOTE: This scans the entire function!
  var exitingBlocks: LazyFilterSequence<BasicBlockList> {
    return blocks.lazy.filter { $0.terminator.isFunctionExiting }
  }
}

/// A "control" variable: a bit vector in memory, represented as an integer, tracking whether each field of a
/// root is dynamically initialized. Bit `i` corresponds to element `i` of the root's StateIndexSpace.
private struct ControlVariable {
  let address: Value
  let intType: Type
  let numBits: Int
  let context: FunctionPassContext

  init(for root: ResolvableRoot, _ space: StateIndexSpace, _ context: FunctionPassContext) {
    let numBits = space.count
    guard numBits <= UInt.bitWidth else {
      fatalError("TODO: control variable with more than \(UInt.bitWidth) bits")
    }
    let function = root.address.parentFunction
    self.intType = context.getBuiltinIntegerType(bitWidth: numBits)
    self.numBits = numBits
    self.context = context

    // Use an auto-generated location so the alloc_stack doesn't look like the storage of a user variable.
    let loc = function.location.asAutoGenerated

    // Create the control variable as the first instruction in the function so that it dominates all exits.
    let entryBuilder = Builder(before: function.entryBlock.instructions.first!, location: loc, context)
    self.address = entryBuilder.createAllocStack(intType)

    // Initialize it at the root, which may be within a loop. An argument's root is the function entry.
    let initBuilder: Builder
    if let rootInst = root.address.definingInstruction {
      initBuilder = Builder(before: rootInst, location: loc, context)
    } else {
      initBuilder = entryBuilder
    }
    let initial = initBuilder.createIntegerLiteral(Self.mask(of: space.initiallyInitialized), type: intType)
    initBuilder.createStore(source: initial, destination: address, ownership: .trivial)

    // Deallocate before each function exit.
    for block in function.exitingBlocks {
      let exitBuilder = Builder(before: block.terminator, location: loc, context)
      exitBuilder.createDeallocStack(address)
    }
  }

  private static func mask(of range: Range<Int>) -> UInt {
    let width = range.count
    let ones: UInt = width == UInt.bitWidth ? ~0 : (1 << width) - 1
    return ones << range.lowerBound
  }

  private func builder(before inst: Instruction) -> Builder {
    Builder(before: inst, location: inst.location.asAutoGenerated, context)
  }

  /// Sets or clears the bits for `range`, just before `inst`.
  func update(_ range: Range<Int>, initialized: Bool, before inst: Instruction) {
    let builder = builder(before: inst)
    let allBits = Self.mask(of: 0..<numBits)
    let bits = Self.mask(of: range)

    let newValue: Value
    if bits == allBits {
      newValue = builder.createIntegerLiteral(initialized ? allBits : 0, type: intType)
    } else {
      let oldValue = builder.createLoad(fromAddress: address, ownership: .trivial)
      let operand = builder.createIntegerLiteral(initialized ? bits : allBits & ~bits, type: intType)
      newValue = builder.createBuiltinBinaryFunction(name: initialized ? "or" : "and",
                                                     operandType: intType, resultType: intType,
                                                     arguments: [oldValue, operand])
    }
    builder.createStore(source: newValue, destination: address, ownership: .trivial)
  }

  /// Returns a `Builtin.Int1` that is true iff all of `bits` are set, computed just before `inst`.
  func testAll(_ bits: [Int], before inst: Instruction) -> Value {
    if bits.count == 1 {
      return test(bits[0], before: inst)
    }
    let builder = builder(before: inst)
    let value = builder.createLoad(fromAddress: address, ownership: .trivial)
    let mask = builder.createIntegerLiteral(bits.reduce(0) { $0 | Self.mask(of: $1..<($1 + 1)) }, type: intType)
    let boolType = context.getBuiltinIntegerType(bitWidth: 1)
    let selected = builder.createBuiltinBinaryFunction(name: "and", operandType: intType, resultType: intType,
                                                       arguments: [value, mask])
    return builder.createBuiltinBinaryFunction(name: "cmp_eq", operandType: intType, resultType: boolType,
                                               arguments: [selected, mask])
  }

  /// Returns a `Builtin.Int1` that is true iff `bit` is set, computed just before `inst`.
  func test(_ bit: Int, before inst: Instruction) -> Value {
    let builder = builder(before: inst)
    let value = builder.createLoad(fromAddress: address, ownership: .trivial)
    if numBits == 1 {
      return value
    }
    let boolType = context.getBuiltinIntegerType(bitWidth: 1)
    let selected = builder.createBuiltinBinaryFunction(name: "and", operandType: intType, resultType: intType,
      arguments: [value, builder.createIntegerLiteral(Self.mask(of: bit..<(bit + 1)), type: intType)])
    return builder.createBuiltinBinaryFunction(name: "cmp_ne", operandType: intType, resultType: boolType,
      arguments: [selected, builder.createIntegerLiteral(0, type: intType)])
  }
}

let lifetimeResolutionResolveTest = FunctionTest("lifetime_resolution_resolve") {
  function, arguments, context in
  var resolver = Resolver(function, context)
  var indexCache = FieldIndexTrieCache(for: function)
  let root = arguments.takeValue()
  resolver.resolve(root, &indexCache)
}
