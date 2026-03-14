//===--- SimplifyAllocStack.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import AST

extension AllocStackInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if optimizeEnum(context) {
      return
    }
    _ = optimizeExistential(context)
  }
}

private extension AllocStackInst {
  /// Replaces an alloc_stack of an enum by an alloc_stack of the payload if only one enum case (with payload)
  /// is stored to that location.
  ///
  /// For example:
  /// ```
  ///   %0 = alloc_stack $Optional<T>
  ///   %1 = init_enum_data_addr %loc
  ///   store %2 to %1
  ///   ...
  ///   %3 = unchecked_take_enum_data_addr %0
  ///   %4 = load %3
  /// ```
  /// is transformed to
  /// ```
  ///   %0 = alloc_stack $T
  ///   store %2 to %0
  ///   ...
  ///   %4 = load %0
  /// ```
  func optimizeEnum(_ context: SimplifyContext) -> Bool {
    guard let payloadType = getUniqueEnumPayloadType(),
          let nonPayloadDestroys = getNonPayloadDestroys(context)
    else {
      return false
    }

    let builder = Builder(before: self, context)
    let newAlloc = builder.createAllocStack(payloadType,
                                            hasDynamicLifetime: hasDynamicLifetime,
                                            isLexical: isLexical,
                                            isFromVarDecl: isFromVarDecl,
                                            usesMoveableValueDebugInfo: usesMoveableValueDebugInfo)
    let oldAllocType = type
    if let varInfo = debugVariable {
      builder.createDebugValue(value: Undef.get(type: oldAllocType, context), debugVariable: varInfo)
    }
    self.replace(with: newAlloc, context)

    context.erase(instructions: nonPayloadDestroys)

    newAlloc.rewriteStore(context)

    for use in newAlloc.uses {
      switch use.instruction {
      case let iea as InjectEnumAddrInst:
        context.erase(instruction: iea)
      case let ieda as InitEnumDataAddrInst:
        ieda.replace(with: newAlloc, context)
      case let uteda as UncheckedTakeEnumDataAddrInst:
        uteda.replace(with: newAlloc, context)
      case let dv as DebugValueInst:
        // TODO: Add support for op_enum_fragment
        dv.operand.set(to: Undef.get(type: oldAllocType, context), context)
      case is DestroyAddrInst, is DeallocStackInst, is StoreInst:
        break
      default:
        fatalError("unexpected alloc_stack user");
      }
    }
    return true
  }

  /// Replace
  /// ```
  ///   store %enum to %allocstack
  /// ```
  /// ->
  /// ```
  ///   %p = unchecked_enum_data %enum, #E.a
  ///   store %p to %newAlloc
  /// ```
  private func rewriteStore(_ context: SimplifyContext) {
    // Handle a `store` before replacing all other uses because we need the `unchecked_take_enum_data_addr`
    // not to be replaced, yet.
    guard let store = uses.users(ofType: StoreInst.self).singleElement else {
      return
    }
    guard let take = uses.users(ofType: UncheckedTakeEnumDataAddrInst.self).singleElement else {
      fatalError("a store requires a single unchecked_take_enum_data_addr")
    }

    // The new store is placed immediately before the `unchecked_take_enum_data_addr` because the original
    // store might still store a different enum case, e.g.
    // ```
    //   store %enum to %allocstack                              // might store Optional.none!
    //   switch_enum %e, case #Optional.some: bb1
    // bb1:
    //   %a = unchecked_take_enum_data_addr %enum, #Optional.some
    // ```
    let builder = Builder(before: take, context)
    let type = take.type.objectType
    let ued = builder.createUncheckedEnumData(enum: store.source, caseIndex: take.caseIndex, resultType: type)

    assert(store.storeOwnership != .assign, "single store can only initialize")

    if store.parentBlock != take.parentBlock && store.storeOwnership == .initialize {
      assert(store.parentBlock == ued.parentBlock.singlePredecessor)
      // Insert compensating destroys for all branches where the enum is not used
      for succ in store.parentBlock.successors {
        if succ != ued.parentBlock {
          Builder(atBeginOf: succ, context).createDestroyValue(operand: store.source)
        }
      }
    }

    // Even if the enum itself is not trivial, the specific case value can be trivial.
    let ownership = store.storeOwnership == .initialize && type.isTrivial(in: parentFunction)
                      ? .trivial
                      : store.storeOwnership
    builder.createStore(source: ued, destination: self, ownership: ownership)
    context.erase(instruction: store)
  }

  private func getUniqueEnumPayloadType() -> Type? {
    if !type.isEnum {
      return nil
    }
    var payloadType: Type? = nil
    let store = uses.users(ofType: StoreInst.self).first

    for use in uses {
      switch use.instruction {
      case is DestroyAddrInst, is DeallocStackInst, is DebugValueInst,
           // We accept `inject_enum_addr` with different cases. Such can only be non-payload cases because
           // otherwise there would be a corresponding `init_enum_data_addr` or `unchecked_take_enum_data_addr`.
           is InjectEnumAddrInst:
        break
      case let ieda as InitEnumDataAddrInst:
        if let previouslyFoundPayloadType = payloadType, previouslyFoundPayloadType != ieda.type {
          return nil
        }
        if store != nil {
          // For simplicity, if there is a store we don't handle any other initializations of the enum.
          return nil
        }
        payloadType = ieda.type
      case let uted as UncheckedTakeEnumDataAddrInst:
        if let previouslyFoundPayloadType = payloadType, previouslyFoundPayloadType != uted.type {
          return nil
        }
        if let store {
          if payloadType != nil {
            // For simplicity, we don't support multiple `unchecked_take_enum_data_addr` in case of a `store`.
            return nil
          }
          let storeBlock = store.parentBlock
          let takeBlock = uted.parentBlock
          // If the store is in the take-block then we know that the store is already storing the right enum case.
          // Or it is in a predecessor. Then we check later in `getNonPayloadDestroys` that in adjacent successors
          // the (potentially) other enum cases are not used.
          guard takeBlock == storeBlock || takeBlock.singlePredecessor == storeBlock else {
            return nil
          }
        }
        payloadType = uted.type
      case let s as StoreInst:
        if s != store {
          // For simplicity we only handle a single store to the enum.
          return nil
        }
      default:
        return nil
      }
    }
    return payloadType
  }

  /// Returns the list of `destroy_addr`s which destroy payload-free injected enum cases.
  /// Such destroys must be removed once the `alloc_stack` is replaced with the "regular" payload case.
  /// ```
  ///   %1 = alloc_stack $Optional<T>
  ///   cond_br %c, bb1, bb2
  /// bb1:
  ///   %3 = init_enum_data_addr %1, #Optional.some   // our "regular" payload case
  ///   ...
  ///   inject_enum_addr %1, #Optional.some
  ///   ...
  ///   destroy_addr %1                               // not a non-payload destroy
  /// bb2:
  ///   inject_enum_addr %1, #Optional.none
  ///   destroy_addr %1                               // a non-payload destroy
  /// ```
  /// Returns nil if such destroys cannot be uniquely identified.
  ///
  private func getNonPayloadDestroys(_ context: SimplifyContext) -> [DestroyAddrInst]? {

    var payloadLiverange = InstructionWorklist(context)
    defer { payloadLiverange.deinitialize() }
    var noPayloadLiverange = InstructionWorklist(context)
    defer { noPayloadLiverange.deinitialize() }

    initialize(payloadLiverange: &payloadLiverange, noPayloadLiverange: &noPayloadLiverange)

    var useBlocks = EnumCaseUseBlocks(forUsesOf: self, context)
    defer { useBlocks.deinitialize() }

    guard hasValidUsesForStore(useBlocks) else {
      return nil
    }

    if noPayloadLiverange.isEmpty {
      return []
    }

    var interstingBlocks = BasicBlockSet(context)
    defer { interstingBlocks.deinitialize() }
    interstingBlocks.insert(contentsOf: users.filter(isInteresting).map { $0.parentBlock })

    while let inst = payloadLiverange.pop() {
      switch inst {
      case let destroy as DestroyAddrInst where destroy.destroyedAddress == self:
        break
      case let dealloc as DeallocStackInst where dealloc.allocatedValue == self:
        break
      default:
        payloadLiverange.pushSuccessors(of: inst, isTransparent: { !interstingBlocks.contains($0) })
      }
    }

    var nonPayloadDestroys = [DestroyAddrInst]()

    // Check if the payload and non-payload liveranges are strictly not overlapping.
    while let inst = noPayloadLiverange.pop() {
      if useBlocks.blocks.contains(inst.parentBlock) {
        return nil
      }
      switch inst {
      case let destroy as DestroyAddrInst where destroy.destroyedAddress == self:
        if payloadLiverange.hasBeenPushed(destroy) {
          // Both liferanges overlap at this `destroy_addr`.
          return nil
        }
        nonPayloadDestroys.append(destroy)
      case let dealloc as DeallocStackInst where dealloc.allocatedValue == self:
        break
      default:
        noPayloadLiverange.pushSuccessors(of: inst, isTransparent: { !interstingBlocks.contains($0) })
      }
    }
    return nonPayloadDestroys
  }

  private func initialize(payloadLiverange: inout InstructionWorklist, noPayloadLiverange: inout InstructionWorklist) {
    for use in uses {
      switch use.instruction {
      case let inject as InjectEnumAddrInst:
        if inject.element.hasAssociatedValues {
          payloadLiverange.pushIfNotVisited(inject)
        } else {
          noPayloadLiverange.pushIfNotVisited(inject)
        }
      case let store as StoreInst:
        let take = uses.users(ofType: UncheckedTakeEnumDataAddrInst.self).singleElement!
        if store.parentBlock == take.parentBlock {
          payloadLiverange.pushIfNotVisited(store)
        } else {
          assert(take.parentBlock.singlePredecessor == store.parentBlock)
          for succ in store.parentBlock.successors {
            if succ == take.parentBlock {
              payloadLiverange.pushIfNotVisited(succ.instructions.first!)
            } else {
              noPayloadLiverange.pushIfNotVisited(succ.instructions.first!)
            }
          }
        }
      default:
        break
      }
    }
  }

  /// In case of a `store` to the enum we require that at an `unchecked_take_enum_data_addr` we can assume
  /// the enum has that specific case. This is not true in general because `unchecked_take_enum_data_addr`
  /// is a side-effect free address projection (for some enums). E.g
  /// ```
  ///   store %1 to %allocstack
  ///   %2 = unchecked_take_enum_data_addr %allocstack, #Optional.some // Here we don't know the case, yet
  ///   cond_br %c, bb1, bb2
  /// bb1:
  ///   %3 = load %2   // Only here we know that %allocstack is an Optional.some
  /// ```
  private func hasValidUsesForStore(_ useBlocks: EnumCaseUseBlocks) -> Bool {
    if uses.users(ofType: StoreInst.self).isEmpty {
      return true
    }
    // Only if there is an actual use of the enum in the same block as the `unchecked_take_enum_data_addr`,
    // we know that the enum has the `unchecked_take_enum_data_addr`'s case.
    let take = uses.users(ofType: UncheckedTakeEnumDataAddrInst.self).singleElement!
    return useBlocks.blocks.contains(take.parentBlock)
  }

  /// Replaces an alloc_stack of an existential by an alloc_stack of the concrete type.
  ///
  /// For example:
  /// ```
  ///   %0 = alloc_stack $any P
  ///   %1 = init_existential_addr %0, $T
  ///   use %1
  /// ```
  /// is transformed to
  /// ```
  ///   %0 = alloc_stack $T
  ///   use %0
  /// ```
  ///
  /// Also, if the alloc_stack is already an opened existential and the concrete type is known,
  /// replace it as well:
  /// ```
  ///   %0 = metatype $@thick T.Type
  ///   %1 = init_existential_metatype %0, $@thick any P.Type
  ///   %2 = open_existential_metatype %1 : $@thick any P.Type to $@thick (@opened("X", P) Self).Type
  ///   ...
  ///   %3 = alloc_stack $@opened("X", any P) Self
  ///   use %3
  /// ```
  /// is transformed to
  /// ```
  ///   ...
  ///   %3 = alloc_stack $T
  ///   use %3
  /// ```
  func optimizeExistential(_ context: SimplifyContext) -> Bool {
    // TODO: support non-root existential archetypes
    guard type.isExistential || type.isRootExistentialArchetype,
          let concreteFormalType = getConcreteTypeOfExistential()
    else {
      return false
    }

    let builder = Builder(before: self, context)
    let newAlloc = builder.createAllocStack(concreteFormalType.loweredType(in: parentFunction),
                                            hasDynamicLifetime: hasDynamicLifetime,
                                            isLexical: isLexical,
                                            isFromVarDecl: isFromVarDecl,
                                            usesMoveableValueDebugInfo: usesMoveableValueDebugInfo)    
    for use in uses {
      switch use.instruction {
      case let dea as DeinitExistentialAddrInst:
        context.erase(instruction: dea)
      case let iea as InitExistentialAddrInst:
        if iea.type != newAlloc.type {
          // We need a cast if the concrete type of the init_existential_addr is itself an opened existential
          // for which we know the concrete type (which is differnt).
          let builder = Builder(before: iea, context)
          let addrCast = builder.createUncheckedAddrCast(from: newAlloc, to: iea.type)
          iea.replace(with: addrCast, context)
        } else {
          iea.replace(with: newAlloc, context)
        }
      case let oea as OpenExistentialAddrInst:
        assert(oea.uses.ignore(usersOfType: DestroyAddrInst.self).isEmpty)
        oea.replace(with: newAlloc, context)
      case let cab as CheckedCastAddrBranchInst:
        let builder = Builder(before: cab, context)
        builder.createCheckedCastAddrBranch(
          source: newAlloc, sourceFormalType: concreteFormalType,
          destination: cab.destination, targetFormalType: cab.targetFormalType,
          options: cab.checkedCastOptions,
          consumptionKind: cab.consumptionKind,
          successBlock: cab.successBlock, failureBlock: cab.failureBlock)
        context.erase(instruction: cab)
      case let ucca as UnconditionalCheckedCastAddrInst:
        let builder = Builder(before: ucca, context)
        builder.createUnconditionalCheckedCastAddr(
          options: ucca.checkedCastOptions,
          source: newAlloc, sourceFormalType: concreteFormalType,
          destination: ucca.destination, targetFormalType: ucca.targetFormalType)
        context.erase(instruction: ucca)
      case let dv as DebugValueInst:
        if dv.location.isInlined {
          // We cannot change the type of an inlined instance of a variable
          // without renaming the inlined function to get a unique
          // specialization suffix (prior art exists in
          // SILCloner::remapFunction()).
          // For now, just remove affected inlined variables.
          use.set(to: Undef.get(type: type, context), context)
        } else {
          use.set(to: newAlloc, context)
        }
      default:
        use.set(to: newAlloc, context)
      }
    }
    context.erase(instruction: self)
    return true
  }

  // Returns the concrete type of this alloc_stack if known.
  // Assuming that its type is either an existential or an opened existential.
  private func getConcreteTypeOfExistential() -> CanonicalType? {
    var initExistential: InitExistentialAddrInst? = nil
    var requiresLegalFormalType = false

    for use in uses {
      switch use.instruction {
      case is DestroyAddrInst,
           is DeinitExistentialAddrInst,
           is DeallocStackInst,
           is DebugValueInst:
        break
      case let oea as OpenExistentialAddrInst:
        if !oea.uses.ignore(usersOfType: DestroyAddrInst.self).isEmpty {
          return nil
        }
      case let iea as InitExistentialAddrInst:
        if initExistential != nil {
          return nil
        }
        initExistential = iea
      case is CheckedCastAddrBranchInst, is UnconditionalCheckedCastAddrInst:
        // To construct a new cast instruction we need a formal type.
        requiresLegalFormalType = true
        if use != use.instruction.operands[0] {
          return nil
        }
      case is UncheckedAddrCastInst:
        if self.type.isExistential {
          // Bail if the address of the original existential escapes.
          // This is not a problem if the alloc_stack already contains the opened existential.
          return nil
        }
      default:
        return nil
      }
    }
    let concreteType: CanonicalType
    if let initExistential {
      assert(self.type.isExistential)
      if let cft = initExistential.concreteTypeOfDependentExistentialArchetype {
        // Case 1: We will replace the alloc_stack of an existential with the concrete type.
        //         `alloc_stack $any P` -> `alloc_stack $ConcreteType`
        concreteType = cft
      } else {
        // The instruction or argument which defines the archetype must dominate the alloc_stack
        // because after the transformation, the alloc_stack will use the archetype.
        for typeDependentOp in initExistential.typeDependentOperands {
          if !typeDependentOp.value.triviallyDominates(self) {
            return nil
          }
        }
        // Case 2: We will replace the alloc_stack of an existential with the existential archetype.
        //         `alloc_stack $any P` -> `alloc_stack $@opened("...")`
        concreteType = initExistential.type.canonicalType
      }
    } else if self.type.isExistentialArchetype, let cft = self.concreteTypeOfDependentExistentialArchetype {
      // Case 3: We will replace the alloc_stack of an existential archetype with the concrete type:
      //         `alloc_stack $@opened("...")` -> `alloc_stack $ConcreteType`
      concreteType = cft
    } else {
      return nil
    }
    if requiresLegalFormalType && !concreteType.isLegalFormalType {
      return nil
    }
    return concreteType
  }
}

/// Returns true if an enum user `inst` is relevant for enum case liverange computation
private func isInteresting(_ inst: Instruction) -> Bool {
  switch inst {
  case is DestroyAddrInst, is DeallocStackInst, is InjectEnumAddrInst, is StoreInst:
    return true
  default:
    return false
  }
}

/// Collects all blocks where a memory-located enum case is used.
private struct EnumCaseUseBlocks : AddressDefUseWalker {
  private(set) var blocks: BasicBlockSet

  init(forUsesOf allocStack: AllocStackInst, _ context: SimplifyContext) {
    self.blocks = BasicBlockSet(context)

    for use in allocStack.uses {
      switch use.instruction {
      case let uted as UncheckedTakeEnumDataAddrInst:
        _  = walkDownUses(ofAddress: uted, path: UnusedWalkingPath())
      case let ieda as InitEnumDataAddrInst:
        _  = walkDownUses(ofAddress: ieda, path: UnusedWalkingPath())
      default:
        break
      }
    }
  }

  mutating func deinitialize() {
    blocks.deinitialize()
  }

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    blocks.insert(address.instruction.parentBlock)
    return .continueWalk
  }
}
