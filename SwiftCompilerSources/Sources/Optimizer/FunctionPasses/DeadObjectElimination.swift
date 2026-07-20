//===--- DeadObjectElimination.swift --------------------------------------===//
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

/// Eliminates dead `alloc_stack` and `alloc_ref` allocations.
///
/// **Stack allocations**
///
/// A stack allocation is dead if its stored values never escape and all loads can be
/// forwarded directly to the stored values:
///
/// ```
///   %s = alloc_stack $Foo
///   store %v to [init] %s : $*Foo
///   %l = load [take] %s : $*Foo
///   dealloc_stack %s : $*Foo
/// ```
/// ->
/// ```
///   // %l is replaced with %v
/// ```
///
/// This also handles field-level stores and loads. Destroy instructions for field
/// values whose containing object is destroyed replace the original aggregate destroy:
///
/// ```
///   %s = alloc_stack $Foo
///   %fa = struct_element_addr %s : $*Foo, #Foo.x
///   store %x to [init] %fa : $*Int
///   destroy_addr %s : $*Foo
///   dealloc_stack %s : $*Foo
/// ```
/// ->
/// ```
///   destroy_value %x : $Int
/// ```
///
/// **Heap allocations**
///
/// A heap allocation is dead if it never escapes and its destructor has no observable
/// side effects. Stored field values are destroyed in place of the object destructor:
///
/// ```
///   %obj = alloc_ref $Foo
///   %addr = ref_element_addr %obj : $Foo, #Foo.x
///   store %x to [init] %addr : $*Int
///   destroy_value %obj : $Foo
/// ```
/// ->
/// ```
///   destroy_value %x : $Int
/// ```
///
let deadObjectElimination = FunctionPass(name: "dead-object-elimination") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else {
    return
  }

  var destructorSideEffects = DestructorSideEffects()

  for inst in function.instructions {
    switch inst {
    case let allocStack as AllocStackInst:
      if !context.continueWithNextSubpassRun(for: allocStack) {
        return
      }
      _ = processAllocStack(allocStack, context)

    case let allocRef as AllocRefInstBase:
      if !context.continueWithNextSubpassRun(for: allocRef) {
        return
      }
      processAllocRef(allocRef, &destructorSideEffects, context)

    default:
      break
    }
  }
}

/// Runs in the diagnostic pipeline and only removes `alloc_stack` allocations that do not
/// need to be preserved for debug info.
///
let mandatoryDeadObjectElimination = FunctionPass(name: "mandatory-dead-object-elimination") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else {
    return
  }

  if removeDeadStackAllocations(in: function, context) {
    context.removeTriviallyDeadInstructionsPreservingDebugInfo(in: function)
  }
}

func removeDeadStackAllocations(in function: Function, _ context: FunctionPassContext) -> Bool {
  var changed = false

  for inst in function.instructions {
    if let allocStack = inst as? AllocStackInst,
       !allocStack.needsToBePreservedForDebugInfo
    {
      if !context.continueWithNextSubpassRun(for: allocStack) {
        return changed
      }
      changed  = processAllocStack(allocStack, context) || changed
    }
  }
  return changed
}

private func processAllocRef(_ allocRef: AllocRefInstBase,
                             _ destructorSideEffects: inout DestructorSideEffects,
                             _ context: FunctionPassContext)
{
  var uses = UseCollector(of: allocRef, context)
  defer { uses.deinitialize() }

  guard uses.analyzeUses(ofObject: allocRef, &destructorSideEffects) else {
    return
  }

  uses.rewriteUses()

  context.erase(instructionIncludingAllUsers: allocRef)
}

private func processAllocStack(_ allocStack: AllocStackInst, _ context: FunctionPassContext) -> Bool {
  var uses = UseCollector(of: allocStack, context)
  defer { uses.deinitialize() }

  guard uses.analyzeUses(ofAllocStack: allocStack) else {
    return false
  }

  if allocStack.hasDynamicLifetime && uses.hasAccesses {
    return false
  }

  uses.rewriteUses()

  context.erase(instructionIncludingAllUsers: allocStack)
  return true
}

/// Collects and classifies all uses of an allocation, validates that the allocation can
/// be safely eliminated, and rewrites those uses so the allocation can be erased.
///
private struct UseCollector : AddressDefUseWalker {

  let context: FunctionPassContext
  var objectLiverange: BasicBlockRange

  typealias SSAUpdater = InstructionBasedSSAUpdater<FunctionPassContext>

  // A `mark_dependence` inserted after each mutating access to act as an SSA anchor for the
  // updated aggregate value. It holds the updated value in SSA form until all loads are rewritten,
  // then is replaced by the actual updated value.
  private typealias PlaceholderInst = MarkDependenceInst

  private enum Access {
    case destroy(Instruction)
    case store(StoreLikeInstruction)
    case load(LoadLikeInstruction)
    // A hybrid between "destroy" and "load"
    case loadTake(LoadInst)

    var instruction: Instruction {
      switch self {
        case .destroy(let destroy): return destroy
        case .store(let store):     return store
        case .loadTake(let load):   return load
        case .load(let load):       return load
      }
    }

    var valueType: Type {
      switch self {
      case .destroy(let destroy): return destroy.operands[0].value.type.objectType
      case .store(let store):     return store.valueType
      case .loadTake(let load):   return load.type
      case .load(let load):       return load.valueType
      }
    }

    var isTrivialStore: Bool {
      guard case .store(let store) = self else {
        return false
      }
      return !store.needDestroy
    }
  }

  // Groups the accesses for a single projection-path level during rewriting.
  private struct SelectedAccesses {
    // Unconditional destroys at this level (including parent-level destroys passed down from the caller).
    var destroys: [Instruction]

    // Pairs each store / projected-destroy / load-take with its placeholder.
    var mutatingAccesses = [(subPath: SmallProjectionPath, access: Access, placeholder: PlaceholderInst)]()

    // Non-consuming reads processed after all mutations.
    var loads = [(subPath: SmallProjectionPath, load: LoadLikeInstruction)]()

    let valueType: Type

    init(parentDestroys: [Instruction], valueType: Type) {
      self.destroys = parentDestroys
      self.valueType = valueType
    }

    var hasNonDestroyAccesses: Bool { !mutatingAccesses.isEmpty || !loads.isEmpty }

    var canDropWithoutRewriting: Bool {
      mutatingAccesses.allSatisfy({ $0.access.isTrivialStore }) && loads.isEmpty
    }

    mutating func addAndInsertPlaceholder(access: Access,
                                          subPath: SmallProjectionPath,
                                          _ ssaUpdater: inout SSAUpdater,
                                          _ context: FunctionPassContext)
    {
      switch access {
      case .destroy(let destroy):
        if subPath.isEmpty {
          destroys.append(destroy)
        } else {
          addMutatingAccess(access, at: destroy, path: subPath, &ssaUpdater, context)
        }
      case .store(let store):
        context.salvageDebugInfo(of: store)
        addMutatingAccess(access, at: store, path: subPath, &ssaUpdater, context)
      case .load(let load):
        loads.append((subPath: subPath, load: load))
      case .loadTake(let load):
        addMutatingAccess(access, at: load, path: subPath, &ssaUpdater, context)
      }
    }

    private mutating func addMutatingAccess(_ access: Access, at insertionPoint: Instruction, path: SmallProjectionPath,
                                            _ ssaUpdater: inout SSAUpdater,
                                            _ context: FunctionPassContext)
    {
      let builder = Builder(after: insertionPoint, context)
      let undef = Undef.get(type: valueType, context)
      let isTrivial = valueType.isTrivial(in: insertionPoint.parentFunction)
      let placeholder = builder.createMarkDependence(value: undef, base: undef,
                                                     ownership: isTrivial ? .none : .owned,
                                                     kind: .Unresolved)
      mutatingAccesses.append((subPath: path, access: access, placeholder: placeholder))
      ssaUpdater.addAvailableValue(placeholder, after: placeholder)
    }
  }

  // Flat sorted array that encodes the tree of accesses by projection path.
  // Entries are sorted in prefix order (shorter / lexicographically earlier paths first),
  // so parent-path entries precede child-path entries and all entries sharing a path prefix
  // form a contiguous group.
  // `validateAccessTree` and `rewriteUses` exploit this by scanning the array recursively:
  // each call processes a contiguous sub-array whose entries all have a given path as prefix.
  //
  private var accessTree = [(path: SmallProjectionPath, access: Access)]()

  // True if there were stores to addresses that couldn't be reduced to a constant projection
  // path (e.g. through a pointer or unknown index). Such stores are safe to ignore unless
  // there are also loads, because a trivial store has no ownership consequences.
  private var hasTrivialStoresToUnknownStorage = false

  init(of startInstruction: SingleValueInstruction, _ context: FunctionPassContext) {
    self.context = context
    self.objectLiverange = BasicBlockRange(begin: startInstruction.parentBlock, context)
  }

  mutating func deinitialize() {
    objectLiverange.deinitialize()
  }

  var hasAccesses: Bool { !accessTree.isEmpty }

  /// Returns true if the object allocation can be eliminated.
  /// Walks all object-level users (transparently following forwarding instructions), records
  /// field-address uses in `accessTree` and validates the tree.
  ///
  mutating func analyzeUses(ofObject object: AllocRefInstBase,
                            _ destructorSideEffects: inout DestructorSideEffects
  ) -> Bool {
    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushIfNotVisited(object)

    while let inst = worklist.pop() {
      switch inst {

      case is FixLifetimeInst,
           is EndBorrowInst,
           is DebugValueInst:
        break

      case is BeginBorrowInst,
           is MoveValueInst,
           is EndInitLetRefInst,
           is BeginDeallocRefInst,
           is UpcastInst,
           is UncheckedRefCastInst,
           is KeyPathInst,
           is EndCOWMutationInst,
           is Allocation:
        worklist.pushIfNotVisited(contentsOf: (inst as! SingleValueInstruction).users)

      case let ecm as BeginCOWMutationInst:
        worklist.pushIfNotVisited(contentsOf: ecm.instanceResult.users)

      case let destroy as DestroyValueInst:
        if destroy.isDeadEnd {
          break
        }
        fallthrough
      case is DeallocPartialRefInst:
        if destructorSideEffects.doesDestructorHaveSideEffects(of: object, context) {
          return false
        }
        objectLiverange.insert(inst.parentBlock)
        accessTree.append((SmallProjectionPath(), .destroy(inst)))

      case is DeallocStackInst, is DeallocStackRefInst, is DeallocRefInst:
        objectLiverange.insert(inst.parentBlock)

      case let dropDeinit as DropDeinitInst:
        guard dropDeinit.uses.isEmpty else {
          return false
        }

      case let elementAddr as RefElementAddrInst:
        guard walkDownUses(ofAddress: elementAddr, path: UnusedWalkingPath()) == .continueWalk else {
          return false
        }

      case let tailAddr as RefTailAddrInst:
        guard walkDownUses(ofAddress: tailAddr, path: UnusedWalkingPath()) == .continueWalk else {
          return false
        }

      default:
        return false
      }
    }
    return validateAccessTree()
  }

  /// Returns true if the stack allocation can be eliminated.
  mutating func analyzeUses(ofAllocStack allocStack: AllocStackInst) -> Bool {
    guard walkDownUses(ofAddress: allocStack, path: UnusedWalkingPath()) == .continueWalk else {
      return false
    }
    return validateAccessTree()
  }

  mutating func walkDown(address operand: Operand, path: Path) -> WalkResult {
    if let ba = operand.instruction as? BeginAccessInst, ba.enforcement == .dynamic {
      return .abortWalk
    }
    return walkDownDefault(address: operand, path: path)
  }

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    switch address.instruction {
    case is DebugValueInst:
      return .continueWalk

    case let fixLifetime as FixLifetimeInst:
      if fixLifetime.operand.value is AllocStackInst {
        accessTree.append((SmallProjectionPath(), .load(fixLifetime)))
      }
      return .continueWalk

    case let deallocStack as DeallocStackInst:
      objectLiverange.insert(deallocStack.parentBlock)
      return .continueWalk

    case let injectEnum as InjectEnumAddrInst:
      let f = injectEnum.parentFunction
      if let payloadType = injectEnum.enum.type.getEnumCasePayload(of: injectEnum.element, in: f) {
        if payloadType.isEmpty(in: f) {
          // We do not necessarily have stores for "empty" enum payloads. Bail to avoid `undef` values.
          return .abortWalk
        }
      } else {
        // There is no `init_enum_data_addr` paired to a payload-less `inject_enum_addr`. Therefore
        // we need to treat the `inject_enum_addr` as "store".
        guard let projectionPath = injectEnum.enum.storageProjectionPath else {
          return .abortWalk
        }
        accessTree.append((projectionPath, .store(injectEnum)))
      }
      return .continueWalk

    case let store as StoreInst where store.destinationOperand == address:
      guard let projectionPath = store.destination.storageProjectionPath else {
        if store.needDestroy {
          return .abortWalk
        } else {
          hasTrivialStoresToUnknownStorage = true
          return .continueWalk
        }
      }
      accessTree.append((projectionPath, .store(store)))
      return .continueWalk

    case let load as LoadInst:
      guard let projectionPath = load.address.storageProjectionPath else {
        return .abortWalk
      }
      if load.type.isEmpty(in: load.parentFunction) {
        // We do not necessarily have stores for "empty" values. Therefore we need to bail to avoid `undef` values.
        // Loads of "empty" types can be removed by a simplification optimization, anyway.
        return .abortWalk
      }
      if load.loadOwnership == .take {
        accessTree.append((projectionPath, .loadTake(load)))
      } else {
        accessTree.append((projectionPath, .load(load)))
      }
      return .continueWalk

    case let loadBorrow as LoadBorrowInst:
      guard let projectionPath = loadBorrow.address.storageProjectionPath else {
        return .abortWalk
      }
      guard loadBorrow.uses.endingLifetime.users.allSatisfy({ $0 is EndBorrowInst}) else {
        return .abortWalk
      }
      accessTree.append((projectionPath, .load(loadBorrow)))
      return .continueWalk

    case let destroy as DestroyAddrInst:
      guard let projectionPath = destroy.destroyedAddress.storageProjectionPath else {
        return .abortWalk
      }
      accessTree.append((projectionPath, .destroy(destroy)))
      return .continueWalk

    case let builtin as BuiltinInst:
      switch builtin.id {
      case .ZeroInitializer, .PrepareInitialization:
        return .continueWalk
      case .AddressOfRawLayout:
        for user in builtin.users {
          guard let pta = user as? PointerToAddressInst,
                walkDownUses(ofAddress: pta, path: UnusedWalkingPath()) == .continueWalk
          else {
            return .abortWalk
          }
        }
        return .continueWalk
      default:
        return .abortWalk
      }

    case let a2p as AddressToPointerInst:
      if let singleUse = a2p.uses.singleUse,
         let builtin = singleUse.instruction as? BuiltinInst,
         builtin.id == .DestroyArray,
         let projectionPath = address.value.storageProjectionPath
      {
        accessTree.append((projectionPath, .destroy(builtin)))
        return .continueWalk
      }
      return .abortWalk

    default:
      return .abortWalk
    }
  }

  mutating func unmatchedPath(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    switch address.instruction {
    case let md as MarkDependenceInstruction:
      assert(address == md.baseOperand, "uses of `value` should be handled by the walker")
      guard address.value is AllocStackInst else {
        return .abortWalk
      }
      accessTree.append((SmallProjectionPath(), .load(md as! LoadLikeInstruction)))
      return .continueWalk
    default:
      return .abortWalk
    }
  }

  mutating func typeUse(typeDependentOperand: Operand, path: UnusedWalkingPath) -> WalkResult {
    return .abortWalk
  }

  /// Sorts `accessTree` and validates all access groups.
  /// Returns false if any group is inconsistent (type mismatch, or a `load_borrow` overlaps
  /// a mutation within the same group), meaning the allocation cannot be eliminated.
  private mutating func validateAccessTree() -> Bool {
    accessTree.sort { $0.path.shouldBePlaced(before: $1.path) }

    var index = 0
    while index < accessTree.count {
      guard validateChildrenRecursively(at: &index) else {
        return false
      }
    }
    return true
  }

  private func validateChildrenRecursively(at index: inout Int) -> Bool {
    let firstPath = accessTree[index].path
    let valueType = accessTree[index].access.valueType
    var hasStore = false
    var hasLoad = false
    var projectedLoadBorrows = Stack<LoadBorrowInst>(context)
    defer { projectedLoadBorrows.deinitialize() }
    var projectedMutations = Stack<Instruction>(context)
    defer { projectedMutations.deinitialize() }

    while index < accessTree.count {
      let (path, access) = accessTree[index]
      guard let subPath = firstPath.subtract(from: path) else {
        break
      }

      // Handle entries at exactly `firstPath` (subPath.isEmpty) and any entries at deeper paths.
      // A direct load/store accesses the whole value at `firstPath`, so sub-projection
      // entries cannot form independent sub-groups — they must be validated at this level.
      if subPath.isEmpty || hasLoad || hasStore {
        guard access.valueType == valueType.project(path: subPath, in: access.instruction.parentFunction) else {
          return false
        }

        switch access {
        case .destroy(let destroy):
          if !subPath.isEmpty {
            projectedMutations.append(destroy)
          }
        case .store(let store):
          hasStore = true
          if !subPath.isEmpty {
            projectedMutations.append(store)
          }
        case .load(let load):
          hasLoad = true
          if let loadBorrow = load as? LoadBorrowInst {
            projectedLoadBorrows.append(loadBorrow)
          }
        case .loadTake(let load):
          hasLoad = true
          if !subPath.isEmpty {
            projectedMutations.append(load)
          }
        }
        index += 1

      } else {
        // Without a direct load or store yet, sub-projection entries recurse to form their own
        // independent validation groups.
        guard validateChildrenRecursively(at: &index) else {
          return false
        }
      }
    }

    if hasLoad && hasTrivialStoresToUnknownStorage {
      return false
    }

    // Bail if a `load_borrow` scope overlaps a mutation of another projected field of the same value.
    // We cannot optimize this because a field mutation ends the lifetime of the whole value which would make
    // the borrow scope illegally overlap the lifetime of its enclosing value. E.g.
    // ```
    //   %1 = struct_element_addr %0, #S.field1
    //   %2 = struct_element_addr %0, #S.field2
    //   %3 = load_borrow %1
    //   store %x to %2       // optimized to `destructure_struct` + `struct`
    //   end_borrow %3
    // ```
    // optimized to
    // ```
    //   %1 = begin_borrow %0
    //   %3 = struct_extract %1, #S.field1        // replacement for `%3 = load_borrow`
    //   (%4, %5) = destructure_struct %0         // -> wrong end lifetime of %0 inside borrow scope!
    //   %6 = struct $S (%4, %x)                  // replacement for `store`
    //   end_borrow %1
    // ```
    for loadBorrow in projectedLoadBorrows {
      var liverange = InstructionRange(begin: loadBorrow, ends: loadBorrow.uses.endingLifetime.users, context)
      defer { liverange.deinitialize() }
      for mutation in projectedMutations {
        if liverange.contains(mutation) {
          return false
        }
      }
    }

    return true
  }

  func rewriteUses() {
    var index = 0
    while index < accessTree.count {
      rewriteUsesRecursively(at: &index, parentDestroys: [])
    }
  }

  // Rewrites all accesses in a single projection-path group rooted at `accessTree[index].path`.
  // `parentDestroys` are destroy instructions at the parent-path level; this group inserts
  // `destroy_value` calls for its field values at those points.
  //
  // The SSA updater tracks the "current" aggregate value at `firstPath` as mutations are applied:
  // each mutating access gets a `mark_dependence` placeholder inserted after it and registered
  // as a new available value. This lets the updater insert phi arguments at control-flow joins
  // and supply the correct value to each load and destroy. Once all accesses are rewritten the
  // placeholder is replaced by the actual updated aggregate.
  //
  private func rewriteUsesRecursively(at index: inout Int, parentDestroys: [Instruction]) {
    var accesses = SelectedAccesses(parentDestroys: parentDestroys, valueType: accessTree[index].access.valueType)

    let isTrivial = accesses.valueType.isTrivial(in: accessTree[index].access.instruction.parentFunction)
    let firstPath = accessTree[index].path

    // Threads the current stored value through the control flow during rewriting. After each
    // mutating access (store, `load [take]`, projected destroy) a `mark_dependence` placeholder
    // anchors the updated aggregate in SSA form.
    //
    var ssaUpdater = SSAUpdater(type: accesses.valueType,
                                ownership: isTrivial ? .none : .owned,
                                context)
    defer { ssaUpdater.deinitialize() }

    // Prevent any phi arguments to be created "before" the allocation
    let undef = Undef.get(type: accesses.valueType, context)
    ssaUpdater.addAvailableValue(undef, after: objectLiverange.begin.instructions.first!)

    // Example:
    // ```                                             access       projection path
    //   %2 = alloc_stack $Pair
    //   store %0 to [init] %2                      <- store        ()
    //   %4 = struct_element_addr %2, #Pair.first
    //   %5 = load [take] %4                        <- loadTake     (s0)
    //   store %1 to [init] %4                      <- store        (s0)
    //   %6 = load [copy] %2                        <- load         ()
    //   destroy_addr %2                            <- destroy      ()
    // ```
    while index < accessTree.count {
      let (path, access) = accessTree[index]
      guard let subPath = firstPath.subtract(from: path) else {
        break
      }

      // Once a direct load or store is seen, all remaining deeper entries are handled flat
      // (see `validateChildrenRecursively` for the same logic).
      if subPath.isEmpty || accesses.hasNonDestroyAccesses {
        accesses.addAndInsertPlaceholder(access: access, subPath: subPath, &ssaUpdater, context)
        index += 1
      } else {
        // Sub-projection entries with no direct load/store at this level yet recurse with the current
        // destroys as their own `parentDestroys`.
        rewriteUsesRecursively(at: &index, parentDestroys: accesses.destroys)
      }
    }

    if accesses.canDropWithoutRewriting {
      context.erase(instructions: accesses.mutatingAccesses.lazy.map { $0.placeholder })
      return
    }

    for destroy in accesses.destroys {
      ssaUpdater.addAvailableValue(undef, after: destroy)
    }

    // Now we have:
    // ```                                                          current SSAUpdater value
    //   %2 = alloc_stack $Pair
    //   store %0 to [init] %2                                      undef
    //   %m1 = mark_dependence undef                <- placeholder
    //   %4 = struct_element_addr %2, #Pair.first                   %m1
    //   %5 = load [take] %4
    //   %m2 = mark_dependence undef                <- placeholder
    //   store %1 to [init] %4                                      %m2
    //   %m3 = mark_dependence undef                <- placeholder
    //   %6 = load [copy] %2                                        %m3
    //   destroy_addr %2
    //                                                              undef
    // ```

    rewrite(mutatingAccesses: accesses.mutatingAccesses, &ssaUpdater)
    // ```
    //   %2 = alloc_stack $Pair
    //   store %0 to [init] %2
    //   %m1 = mark_dependence %0                   <- placeholder operand wired up
    //   %4 = struct_element_addr %2, #Pair.first
    //   (%5, %d1) = destructure_struct %m1         <- destruct; uses of `load [take]` replaced with %5
    //   %s1 = struct $Pair (undef, %d1)            <- reconstruct the value; taken value is undef
    //   %m2 = mark_dependence %s1                  <- placeholder operand wired up
    //   (%d2, %d3) = destructure_struct %m1        <- destruct (%d2 = undef)
    //   store %1 to [init] %4
    //   %s2 = struct $Pair (%1, %d3)               <- reconstruct with stored value %1
    //   %m3 = mark_dependence %s2                  <- placeholder operand wired up
    //   %6 = load [copy] %2
    //   destroy_addr %2
    // ```

    if !isTrivial {
      insertDestroys(at: accesses.destroys, &ssaUpdater)
    }
    // ```
    //   %2 = alloc_stack $Pair
    //   store %0 to [init] %2
    //   %m1 = mark_dependence %0
    //   %4 = struct_element_addr %2, #Pair.first
    //   (%5, %d1) = destructure_struct %m1
    //   %s1 = struct $Pair (undef, %d1)
    //   %m2 = mark_dependence %s1
    //   (%d2, %d3) = destructure_struct %m1
    //   store %1 to [init] %4
    //   %s2 = struct $Pair (%1, %d3)
    //   %m3 = mark_dependence %s2
    //   %6 = load [copy] %2
    //   destroy_value %m3                          <- destroy current value before a `destroy` access
    //   destroy_addr %2
    // ```

    rewrite(loads: accesses.loads, &ssaUpdater)
    // ```
    //   %2 = alloc_stack $Pair
    //   store %0 to [init] %2
    //   %m1 = mark_dependence %0
    //   %4 = struct_element_addr %2, #Pair.first
    //   (%5, %d1) = destructure_struct %m1
    //   %s1 = struct $Pair (undef, %d1)
    //   %m2 = mark_dependence %s1
    //   (%d2, %d3) = destructure_struct %m1
    //   store %1 to [init] %4
    //   %s2 = struct $Pair (%1, %d3)
    //   %m3 = mark_dependence %s2
    //   %6 = copy_value %m3                        <- uses of `load [copy]` replaced with %6
    //   destroy_value %m3
    //   destroy_addr %2
    // ```

    eraseStores(of: accesses.mutatingAccesses.lazy.map(\.access))
    // ```
    //   %2 = alloc_stack $Pair
    //                                              <- store removed
    //   %m1 = mark_dependence %0
    //   %4 = struct_element_addr %2, #Pair.first
    //   (%5, %d1) = destructure_struct %m1
    //   %s1 = struct $Pair (undef, %d1)
    //   %m2 = mark_dependence %s1
    //   (%d2, %d3) = destructure_struct %m1
    //                                              <- store removed
    //   %s2 = struct $Pair (%1, %d3)
    //   %m3 = mark_dependence %s2
    //   %6 = copy_value %m3
    //   destroy_value %m3
    //   destroy_addr %2
    // ```

    if !isTrivial {
      insertMissingLifetimeEnds(for: accesses.mutatingAccesses.lazy.map(\.placeholder), ssaUpdater: &ssaUpdater)
    }

    for (_, _, placeholder) in accesses.mutatingAccesses {
      placeholder.replace(with: placeholder.value, context)
    }
    // ```
    //   %2 = alloc_stack $Pair
    //                                              <- placeholder removed
    //   %4 = struct_element_addr %2, #Pair.first
    //   (%5, %d1) = destructure_struct %0          <- operand of placeholder forwarded
    //   %s1 = struct $Pair (undef, %d1)
    //                                              <- placeholder removed
    //   (%d2, %d3) = destructure_struct %s1        <- operand of placeholder forwarded
    //   %s2 = struct $Pair (%1, %d3)
    //                                              <- placeholder removed
    //   %6 = copy_value %s2                        <- operand of placeholder forwarded
    //   destroy_value %s2                          <- operand of placeholder forwarded
    //   destroy_addr %2
    // ```
  }

  private func eraseStores(of accesses: some Sequence<Access>) {
    for access in accesses {
      if case .store(let store) = access {
        context.erase(instruction: store)
      }
    }
  }

  private func rewrite(mutatingAccesses: [(subPath: SmallProjectionPath, access: Access, placeholder: PlaceholderInst)],
                       _ ssaUpdater: inout SSAUpdater)
  {
    for (path, access, placeHolder) in mutatingAccesses {
      let accessInst = access.instruction
      let builder = Builder(before: accessInst, context)
      let value = ssaUpdater.getValue(before: accessInst)
      let updatedValue: Value

      switch access {
      case .destroy:
        let undef = Undef.get(type: access.valueType, context)
        let (projected, updated) = value.createProjectionAndReplace(with: undef, path: path, builder: builder)
        builder.emitDestroy(of: projected)
        updatedValue = updated

      case .store(let store):
        let source = store.materializeValue(context)
        let (projected, updated) = value.createProjectionAndReplace(with: source, path: path, builder: builder)
        if store.storeOwnership == .assign, projected.ownership == .owned {
          builder.createDestroyValue(operand: projected)
        } else if projected.ownership == .owned {
          // A `store [init]` where the old value is not destroyed yet, can happen e.g. if the old value
          // is a payload-less enum case of a non-trivial enum.
          builder.createEndLifetime(of: projected)
        }
        updatedValue = updated

      case .loadTake(let load):
        let undef = Undef.get(type: load.type, context)
        let (projected, updated) = value.createProjectionAndReplace(with: undef, path: path, builder: builder)
        insertMarkDependencies(for: load, context)
        load.replace(with: projected, context)
        updatedValue = updated

      case .load:
        fatalError()
      }
      placeHolder.valueOperand.set(to: updatedValue, context)
    }
  }

  private func insertDestroys(at destroys: [Instruction], _ ssaUpdater: inout SSAUpdater) {
    for destroy in destroys {
      let builder = Builder(before: destroy, context)
      let value = ssaUpdater.getValue(before: destroy)
      builder.createDestroyValue(operand: value)
    }
  }

  private func rewrite(loads: [(subPath: SmallProjectionPath, load: LoadLikeInstruction)],
                       _ ssaUpdater: inout SSAUpdater)
  {
    for (subPath, load) in loads {
      let value = ssaUpdater.getValue(before: load)
      if let loadInst = load as? LoadInstruction {
        insertMarkDependencies(for: loadInst, context)
      }
      load.rewrite(with: value, projection: subPath, context)
    }
  }

  private func insertMissingLifetimeEnds(for placeholders: some Sequence<PlaceholderInst>,
                                         ssaUpdater: inout SSAUpdater)
  {
    for placeholder in placeholders {
      insertMissingLifetimeEnds(for: placeholder, &ssaUpdater)
    }
    for phi in ssaUpdater.insertedPhis {
      insertMissingLifetimeEnds(for: phi.value, &ssaUpdater)
    }
  }

  /// A lifetime-end can be missing for two reasons:
  /// 1. If the memory contains a payload-less enum case, the memory may be missing a `destroy_addr`
  ///    Therefore, after optimization, it is missing a `destroy_value`.
  ///    -> we need to insert `end_lifetime`s at liverange exit blocks.
  /// 2. Due to dead-end blocks. Memory locations may be missing destroys (and even `dealloc_stack`s)
  ///    in dead-end regions.
  ///    -> we need to insert `destroy_value [dead_end]` at `unreachable`s.
  ///
  /// If a missing destroy is within the `objectLiverange` (i.e. backward reachable from the object
  /// deallocation), we know it's case 1.
  /// Otherwise the destroy - including the object deallocation! - was cut off by an `unreachable`
  /// and it's case 2.
  private func insertMissingLifetimeEnds(for value: Value, _ ssaUpdater: inout SSAUpdater) {
    guard value.ownership == .owned else {
      return
    }
    insertMissingLifetimeEndsInObjectLiverange(for: value)
    insertMissingLifetimeEndsInDeadEndBlocks(for: value, &ssaUpdater)
  }

  private func insertMissingLifetimeEndsInObjectLiverange(for value: Value) {
    var users = Stack<Instruction>(context)
    defer { users.deinitialize() }

    for user in value.users {
      users.append(user)
      if let beginBorrow = user as? BeginBorrowInst {
        users.append(contentsOf: beginBorrow.scopeEndingOperands.users)
      }
    }

    if users.isEmpty, objectLiverange.inclusiveRangeContains(value.parentBlock) {
      Builder(afterDefinitionOf: value, context).createEndLifetime(of: value)
      return
    }

    var liverange = InstructionRange(for: value, context)
    defer { liverange.deinitialize() }
    liverange.insert(contentsOf: users)

    for exit in liverange.exits {
      if objectLiverange.inclusiveRangeContains(exit.parentBlock) {
        Builder(before: exit, context).createEndLifetime(of: value)
      }
    }
    for user in users where !user.endsLifetime(of: value) {
      if !liverange.contains(user), objectLiverange.inclusiveRangeContains(user.parentBlock) {
        let endLifetime = Builder(after: user, context).createEndLifetime(of: value)
        liverange.insert(endLifetime)
      }
    }
  }

  private func insertMissingLifetimeEndsInDeadEndBlocks(for value: Value, _ ssaUpdater: inout SSAUpdater) {
    completeLifetime(
      ofOwned: value,
      createEnd: { builder in
        let v = ssaUpdater.getValue(before: builder.insertionBlock!.terminator)
        builder.createDestroyValue(operand: v, isDeadEnd: true)
      },
      context)
  }
}

private extension SmallProjectionPath {
  var isValidForDeadObjectElimination: Bool {
    let (kind, _, subPath) = pop()
    switch kind {
    case .root:
      return true
    case .structField, .tupleField, .enumCase, .indexedElement, .existential, .vectorBase, .classField, .tailElements:
      return subPath.isValidForDeadObjectElimination
    default:
      return false
    }
  }

  // The predicate for sorting `UseCollector.accessTree`.
  // Puts longer paths after shorter paths, i.e. sub-projections after their "parents".
  func shouldBePlaced(before rhs: SmallProjectionPath) -> Bool {
    let (kind, index, subPath) = pop()
    let (rhsKind, rhsIndex, rhsSubPath) = rhs.pop()
    if kind != rhsKind {
      return kind.rawValue < rhsKind.rawValue
    } else if kind == .root {
      return false
    }
    if index != rhsIndex {
      return index < rhsIndex
    }
    return subPath.shouldBePlaced(before: rhsSubPath)
  }
}

private struct DestructorSideEffects {

  var cache = [Type : Bool]()

  mutating func doesDestructorHaveSideEffects(of allocRef: AllocRefInstBase, _ context: FunctionPassContext) -> Bool {
    let type = allocRef.type
    if let cached = cache[type] {
      return cached
    }
    let hasSideEffects = computeDestructorSideEffects(of: allocRef, context)
    cache[type] = hasSideEffects
    return hasSideEffects
  }

  private func computeDestructorSideEffects(of allocRef: AllocRefInstBase,
                                            _ context: FunctionPassContext) -> Bool
  {
    guard let destructorFn = getDestructorFunction(of: allocRef, context) else {
      return true
    }

    guard let selfArg = destructorFn.arguments.singleElement else {
      return true
    }

    for inst in destructorFn.instructions {
      switch inst {
      case is DeallocStackInst, is BeginBorrowInst, is EndBorrowInst, is EndLifetimeInst:
        break

      case let fixLifetime as FixLifetimeInst:
        // A fix_lifetime of self cannot have a side effect, because in the destructor, Self is deleted.
        guard fixLifetime.operand.value.isDerived(from: selfArg) else {
          return true
        }

      case let destroy as DestroyValueInst:
        guard destroy.destroyedValue.isDerived(from: selfArg) else {
          return true
        }

      case let deallocRef as DeallocRefInst:
        guard deallocRef.operand.value.isDerived(from: selfArg) else {
          return true
        }

      case let store as StoreInst:
        guard let object = store.destination.accessBase.reference,
              object.referenceRoot == selfArg
        else {
          return true
        }

      case let builtin as BuiltinInst where builtin.id == .DestroyArray:

        // Check if destroyArray destroys the tail elements of this object.
        guard let atp = builtin.arguments[1] as? AddressToPointerInst,
              let rta = atp.address as? RefTailAddrInst,
              rta.instance.referenceRoot == selfArg
        else {
          return true
        }

      default:
        if inst.mayHaveSideEffects {
          return true
        }
      }
    }
    return false
  }

  private func getDestructorFunction(of allocRef: AllocRefInstBase,
                                     _ context: FunctionPassContext) -> Function? {
    // Skip alloc_ref_dynamic unless the dynamic type's deinit is known equivalent.
    if let dynamicRef = allocRef as? AllocRefDynamicInst,
       !dynamicRef.isDynamicTypeDeinitAndSizeKnownEquivalentToBaseType
    {
      return nil
    }

    guard let destructorFn = context.calleeAnalysis.getDestructor(ofExactType: allocRef.type) else {
      return nil
    }

    // If the destructor has an ObjC calling convention, we cannot analyze it
    // since it could be swapped out at runtime.
    if destructorFn.loweredFunctionTypeInContext.functionTypeRepresentation == .objCMethod {
      return nil
    }

    guard context.loadFunction(function: destructorFn, loadCalleesRecursively: false) else {
      return nil
    }

    return destructorFn
  }
}

private extension Value {
  var storageProjectionPath: SmallProjectionPath? {
    let accessPath = constantAccessPath
    let projectionPath: SmallProjectionPath
    switch accessPath.base {
    case .stack:
      projectionPath = accessPath.projectionPath
    case .class(let refElementAddr):
      projectionPath = accessPath.projectionPath.push(.classField, index: refElementAddr.fieldIndex)
    case .tail:
      projectionPath = accessPath.projectionPath.push(.tailElements)
    default:
      return nil
    }
    guard projectionPath.isValidForDeadObjectElimination else {
      return nil
    }
    return projectionPath
  }

  func isDerived(from value: Value) -> Bool {
    var walker = RootValueWalker(targetValue: value)
    return walker.walkUp(value: self, path: UnusedWalkingPath()) == .continueWalk
  }
}

private extension AllocStackInst {
  var needsToBePreservedForDebugInfo: Bool {
    debugVariable != nil || (isLexical && !type.isTrivial(in: parentFunction))
  }
}

private struct RootValueWalker : ValueUseDefWalker {
  let targetValue: Value

  var walkUpCache = WalkerCache<UnusedWalkingPath>()

  mutating func rootDef(value: Value, path: UnusedWalkingPath) -> WalkResult {
    return value == targetValue ? .continueWalk : .abortWalk
  }
}

// ---------------- Instructions which are handled as accesses -----------------

private protocol StoreLikeInstruction: Instruction {
  var valueType: Type { get }
  var storeOwnership: StoreInst.StoreOwnership { get }
  var needDestroy: Bool { get }
  func materializeValue(_ context: FunctionPassContext) -> Value
}

extension StoreInst: StoreLikeInstruction {
  var valueType: Type { source.type }
  var needDestroy: Bool { source.ownership != .none }
  func materializeValue(_ context: FunctionPassContext) -> Value { source }
}

// Created for payload-less enum cases.
// Enum values with payloads are handled by their stores to the payload address.
extension InjectEnumAddrInst: StoreLikeInstruction {
  var valueType: Type { self.enum.type.objectType }
  var needDestroy: Bool { false }
  var storeOwnership: StoreInst.StoreOwnership { .trivial }

  func materializeValue(_ context: FunctionPassContext) -> Value {
    let builder = Builder(before: self, context)
    return builder.createEnum(caseIndex: caseIndex, payload: nil, enumType: valueType)
  }
}

private protocol LoadLikeInstruction: Instruction {
  var valueType: Type { get }
  func rewrite(with value: Value, projection: SmallProjectionPath, _ context: FunctionPassContext)
}

extension LoadInst : LoadLikeInstruction {
  var valueType: Type { type }

  func rewrite(with value: Value, projection: SmallProjectionPath, _ context: FunctionPassContext) {
    let builder = Builder(before: self, context)
    switch loadOwnership {
    case .unqualified:
      fatalError("unqualified loads shouldn't appear in OSSA")
    case .take:
      fatalError("load [take] should be handled separately")
    case .trivial, .copy:
      let projectedValue = value.createProjectionAndCopy(path: projection, builder: builder)
      replace(with: projectedValue, context)
    }
  }
}

extension LoadBorrowInst: LoadLikeInstruction {
  var valueType: Type { type }

  func rewrite(with value: Value, projection: SmallProjectionPath, _ context: FunctionPassContext) {
    let builder = Builder(before: self, context)
    let beginBorrow = builder.createBeginBorrow(of: value)
    let projectedValue = beginBorrow.createProjection(path: projection, builder: builder)
    uses.endingLifetime.replaceAll(with: beginBorrow, context)
    replace(with: projectedValue, context)
  }
}

// `mark_dependence/_addr` where the `base` uses the object/stack is treated as "load".
private extension MarkDependenceInstruction {
  var valueType: Type { base.type.objectType }

  func rewrite(with value: Value, projection: SmallProjectionPath, _ context: FunctionPassContext) {
    baseOperand.set(to: value, context)
  }
}

extension MarkDependenceInst: LoadLikeInstruction {}
extension MarkDependenceAddrInst: LoadLikeInstruction {}

extension FixLifetimeInst: LoadLikeInstruction {
  var valueType: Type { operand.value.type.objectType }

  func rewrite(with value: Value, projection: SmallProjectionPath, _ context: FunctionPassContext) {
    operand.set(to: value, context)
  }
}
