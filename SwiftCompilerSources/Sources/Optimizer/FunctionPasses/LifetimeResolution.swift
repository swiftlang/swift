//===--- LifetimeResolution.swift -----------------------------------------===//
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
      // Ignore `mark_uninitialized`; this pass is driven by the storage itself.
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

  /// - Returns: true iff `value` is a resolvable root, which is then resolved.
  mutating func run(on value: Value, _ indexCache: inout FieldIndexTrieCache) -> Bool {
    guard let root = ResolvableRoot(value, &indexCache) else {
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

    let space = root.space
    log("StateIndexSpace of \(root.address.type): \n\(space)")

    // The reference to a class instance is itself an SSA value, whose copies and destroys are resolved first.
    if root.mode == .rootClassSelf {
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

    log("\n** Resolution finished **")
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
      let uninitializedFields = demanded.filter { space.fieldRange.contains($0) }
      LifetimeResolutionMarker.markUsesBeforeInit(of: uninitializedFields, in: root, context)
      if let superclass = space.index(of: .superclass), demanded.contains(superclass) {
        LifetimeResolutionMarker.markMissingSuperInit(of: root, context)
      }
      assert(space.index(of: .reference).map { !demanded.contains($0) } ?? true,
             "the reference is defined right after the root")

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
      //    diagnose [unpermitted_copy] %op
      //    apply(%alloc) : (@in) -> ...
      //    dealloc_stack %alloc
      let type = op.value.type.objectType
      let allocBuilder = Builder(before: apply, context)
      let alloc = allocBuilder.createAllocStack(type)
      let copyAddr = allocBuilder.createCopyAddr(from: op.value, to: alloc, takeSource: false, initializeDest: true)
      if isUnpermittedCopy(of: op.value.type) {
        LifetimeResolutionMarker.markUnpermittedCopy(copyAddr, context)
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
        LifetimeResolutionMarker.markUnpermittedCopy(copyAddr, context)
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
          LifetimeResolutionMarker.markUnpermittedCopy(load, context)
        }

        if newOwnership == .take {
          adjustAccessForTake(load.address)
        }

        return load.addressOperand

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
        return loadBorrow.addressOperand
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
    let immutableLeaves = root.immutableLeaves()

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
            LifetimeResolutionMarker.markLetReinit(of: op.value, before: op.instruction, context)
          }

          let nontrivial = range.filter { space.hasAddress($0) && !space.leafType($0).isTrivial(in: function) }
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
        case let .superInit(op):
          // FIXME: what about `.partial` availability? Suppose they did `if c { super.init }; super.init`.
          if current[range].contains(where: { $0.contains(.yes) }) {
            LifetimeResolutionMarker.markLetReinit(of: op.value, before: op.instruction, context)
          }

        case let .end(op) where root.mode == .derivedClassSelf:
          let holdsReference = current[space.index(of: .reference)!]
          let superInitialized = current[space.index(of: .superclass)!]
          if holdsReference == .no {
            // A failed `super.init` already released the instance.
            context.erase(instruction: op.instruction)
            didDelete = true
          } else if holdsReference == .yes && superInitialized == .yes {
            // FIXME: a stored property consumed after `super.init` and not reinitialized must be diagnosed.
            break
          } else {
            partialDestroys.append((op.instruction, current, range))
            didDelete = true
          }

        case let .end(op):
          assert(summary != .unknown, "missing information!")

          // If fully available, this lifetime ending instruction can stay as-is.
          if summary == .yes {
            break
          }

          // If completely unavailable (consumed), its lifetime ended earlier, so delete this one.
          // A class instance must still be deallocated, though.
          if summary == .no && root.mode != .rootClassSelf {
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
      switch root.mode {
      case .rootClassSelf:
        endInstanceLifetime(at: destroy, avail, root, space, controlVar)
      case .derivedClassSelf:
        endDerivedSelfLifetime(at: destroy, avail, root, space, controlVar)
      case .storage:
        destroyAvailableElements(avail, range, before: destroy, root, space, controlVar)

        // Everything in range is now uninitialized.
        controlVar?.update(range, initialized: false, before: destroy)
        context.erase(instruction: destroy)
      }
    }

    for (def, avail, range) in destroysBeforeInit {
      destroyAvailableElements(avail, range, before: def, root, space, controlVar)
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
        case .def, .superInit:
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

      //   keep:    destroy_value instance
      //   dealloc: <destroy fields>; dealloc_partial_ref instance
      let allInitialized = controlVar.testAll(partialLeaves, before: destroy)
      let (keep, dealloc) = context.splitIntoDiamond(before: destroy, on: allInitialized)
      Builder(before: keep, location: destroy.location, context).createDestroyValue(operand: instance)
      deallocPoint = dealloc

      // A sole conditionally initialized property is known to be uninitialized when deallocating.
      if partialLeaves.count == 1 {
        deallocAvail[partialLeaves[0]] = .no
      }
    }

    destroyAvailableElements(deallocAvail, space.fieldRange, before: deallocPoint, root, space, controlVar)
    let builder = Builder(before: deallocPoint, location: destroy.location, context)
    let metatype = builder.createMetatype(ofInstanceType: instance.type.canonicalType, representation: .thick)
    builder.createDeallocPartialRef(instance, metatype: metatype)
    context.erase(instruction: destroy)
  }

  // Destroys only the available elements of partially available memory, testing the control variable
  // for elements that are only conditionally available.
  func destroyAvailableElements(_ current: [Availability], _ range: EventList.IndexRange, before inst: Instruction,
                                _ root: ResolvableRoot, _ space: StateIndexSpace, _ controlVar: ControlVariable?) {
    log("destroying available elements before \(inst)")

    for leaf in range where space.hasAddress(leaf) {
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

      let (initialized, _) = context.splitIntoDiamond(before: inst, on: controlVar.test(leaf, before: inst))
      let builder = Builder(before: initialized, location: inst.location, context)
      space.withElementAddress(forIndex: leaf, from: root.address, builder) { builder.createDestroyAddr(address: $0) }
    }
  }

  // Ends the lifetime of derived class `self` at a `destroy_addr` of its memory, which may no longer hold the
  // instance, or hold one whose superclass part isn't initialized. A failed `super.init` already released the
  // instance. Otherwise, before `super.init`, the initialized stored properties are destroyed and the instance is
  // deallocated without running its deinit, as `super.init` initializes the parts the deinit needs.
  func endDerivedSelfLifetime(at destroy: Instruction, _ avail: [Availability], _ root: ResolvableRoot,
                              _ space: StateIndexSpace, _ controlVar: ControlVariable?) {
    let reference = space.index(of: .reference)!
    let superclass = space.index(of: .superclass)!
    var deallocPoint = destroy

    // The `super.init` bit is set before the call, so whether it failed is only known from the reference.
    //   holds:  <the rest>
    //   empty:  nothing
    if avail[reference] == .partial {
      guard let controlVar else { fatalError("missing control variable for conditional destroy") }
      (deallocPoint, _) = context.splitIntoDiamond(before: destroy, on: controlVar.test(reference, before: destroy))
    }

    //   keep:     destroy_addr self
    //   dealloc:  <destroy fields>; dealloc_partial_ref self
    switch avail[superclass] {
    case .yes:
      Builder(before: deallocPoint, location: destroy.location, context).createDestroyAddr(address: root.address)
      context.erase(instruction: destroy)
      return
    case .partial:
      guard let controlVar else { fatalError("missing control variable for conditional destroy") }
      let superInitialized = controlVar.test(superclass, before: deallocPoint)
      let (keep, dealloc) = context.splitIntoDiamond(before: deallocPoint, on: superInitialized)
      Builder(before: keep, location: destroy.location, context).createDestroyAddr(address: root.address)
      deallocPoint = dealloc
    default:
      break
    }

    destroyAvailableElements(avail, space.fieldRange, before: deallocPoint, root, space, controlVar)
    let builder = Builder(before: deallocPoint, location: destroy.location, context)
    let instance = builder.createLoad(fromAddress: root.address, ownership: .take)
    let metatype = builder.createMetatype(ofInstanceType: space.instanceType.canonicalType, representation: .thick)
    builder.createDeallocPartialRef(instance, metatype: metatype)
    context.erase(instruction: destroy)
  }

  // Classifies every use of the root's address, following projections and access scopes, into the events of each block.
  private struct UseCollector: ResolvableRootUseWalker, AddressUseVisitor {
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

    mutating func addUse(_ k: EventList.Kind, _ range: EventList.IndexRange) {
      log("\taddUse of \(range) as \(k)")
      switch k {
      case .def, .take, .use:
        assert(range.allSatisfy(space.hasAddress), "\(k) of elements without an address: \(range)")
      default:
        break
      }
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

      // Add the root use to detect uninitialized paths.
      for (range, initialized) in space.initialRanges {
        addUse(.root(root, initialized: initialized), range)
      }

      if walkDownUses(of: root) == .abortWalk {
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

    // Implement ResolvableRootUseWalker

    mutating func derivedSelfUse(_ use: DerivedSelfUse, of operand: Operand) -> WalkResult {
      let superclass = space.range(of: .superclass)!
      let reference = space.range(of: .reference)!
      switch use {
      case .initialStore, .storeBack:
        addUse(.def(operand), reference)
      case .borrow(let loadBorrow):
        addUse(.liveness(loadBorrow), reference)
        for end in loadBorrow.uses.users(ofType: EndBorrowInst.self) {
          addUse(.liveness(end), reference)
        }
        return walkDownUses(ofClassInstance: loadBorrow)
      case .copy(let load):
        addUse(.liveness(load), space.wholeRange)
      case .superInit(let load, _):
        // `super.init` needs the own stored properties, but only takes the reference to the instance.
        if !space.fieldRange.isEmpty {
          addUse(.liveness(load), space.fieldRange)
        }
        addUse(.superInit(operand), superclass)
        addUse(.take(operand, .own), reference)
      case .destroy, .deallocate:
        addUse(.end(operand), space.wholeRange)
      case .ignored:
        break
      case .unknown:
        return .abortWalk
      }
      return .continueWalk
    }

    mutating func instanceUse(_ use: Operand) -> WalkResult {
      // A value loaded from memory holding `self` doesn't end the lifetime of the instance.
      if use.instruction is DestroyValueInst && space.mode == .rootClassSelf {
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
      indirect case root(ResolvableRoot, initialized: Bool)  // Whether the elements in range are initialized at the start.
      case use(Operand, Demand)         // The demand summarizes the kinds of users of this operand's instruction.
      case take(Operand, Demand)        // The demand summarizes the kinds of users of this operand's instruction.
      case def(Operand)
      case superInit(Operand) // `self` passed to `super.init`, which initializes the superclass part of the instance.
      case end(Operand)       // An instruction representing the point at which the operand's lifetime has ended.
      case liveness(Instruction)       // A non-specific liveness use.

      var inst: Instruction {
        switch self {
        case let .root(r, _): r.startInstruction
        case let .use(op, _): op.instruction
        case let .take(op, _): op.instruction
        case let .def(op): op.instruction
        case let .superInit(op): op.instruction
        case let .end(op): op.instruction
        case let .liveness(inst): inst
        }
      }

      // What is the ownership demanded by this use?
      var demandGen: Demand {
        switch self {
        case let .use(_, dem): dem
        case let .take(_, dem): dem
        case .def, .superInit, .root: .nothing
        case .end: .nothing  // Demand analysis ignores lifetime ends.
        case .liveness: .all
        }
      }

      // What ownership does this use satisfy?
      var demandKill: Demand {
        switch self {
        case .def, .superInit: .all
        case .root(_, initialized: true): .all // A root that starts initialized is a def.
        default: .nothing
        }
      }

      // What availability does this event establish? (forward analysis)
      var availGen: Availability {
        switch self {
        case .def, .superInit: .yes
        case .root(_, initialized: true): .yes  // A root that starts initialized is a def.
        case .root, .take, .end: .no    // these leave the operand uninitialized.
        case .use: .unknown       // doesn't change the state
        case .liveness: .unknown
        }
      }

      // What prior availability does this event overwrite?
      var availKill: Availability {
        switch self {
        case .def, .superInit, .root, .take, .end: .partial    // Overwrites all availability.
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
        case .superInit: "super.init"
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
        assert(!provided.list.isEmpty, "You should pass nil if there are no events.")
      }

      self.events = useList
      self.demand = demand
      self.availability = avail
    }

    mutating func initialDemand() {
      guard let events else { return }
      assert(events.finalized, "EventList needs to be finalized!")

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
      assert(events.finalized, "EventList needs to be finalized!")

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

private extension MutableCollection {
  mutating func mutatingEach(_ body: (inout Element) -> Void) {
    var i = startIndex
    while i != endIndex {
      body(&self[i])
      i = index(after: i)
    }
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
