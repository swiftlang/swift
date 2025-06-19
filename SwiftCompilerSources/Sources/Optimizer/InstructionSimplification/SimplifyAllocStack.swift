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
    guard let (payloadType, isSingleInitTakePair) = getEnumInfo() else {
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

    for use in newAlloc.uses {
      switch use.instruction {
      case let iea as InjectEnumAddrInst:
        context.erase(instruction: iea)
      case let da as DestroyAddrInst:
        if isSingleInitTakePair {
          // It's not possible that the enum has a payload at the destroy_addr, because it must have already
          // been taken by the take of the single init-take pair.
          // We _have_ to remove the destroy_addr, because we also remove all inject_enum_addrs which might
          // inject a payload-less case before the destroy_addr.
          // Otherwise the enum payload can still be valid at the destroy_addr, so we have to keep the destroy_addr.
          // Just replace the enum with the payload (and because it's not a singleInitTakePair, we can be sure
          // that the enum cannot have any other case than the payload case).
          context.erase(instruction: da)
        }
      case let ieda as InitEnumDataAddrInst:
        ieda.replace(with: newAlloc, context)
      case let uteda as UncheckedTakeEnumDataAddrInst:
        uteda.replace(with: newAlloc, context)
      case is DeallocStackInst:
        break
      case let dv as DebugValueInst:
        // TODO: Add support for op_enum_fragment
        dv.operand.set(to: Undef.get(type: oldAllocType, context), context)
      default:
        fatalError("unexpected alloc_stack user");
      }
    }
    return true
  }

  func getEnumInfo() -> (payloadType: Type, isSingleInitTakePair: Bool)? {
    if !type.isEnum {
      return nil
    }
    var numInits = 0
    var numTakes = 0
    var initBlock: BasicBlock? = nil
    var takeBlock: BasicBlock? = nil
    var caseIndex: Int? = nil
    var payloadType: Type? = nil
    for use in uses {
      switch use.instruction {
      case is DestroyAddrInst,
           is DeallocStackInst,
           is DebugValueInst,
           // We'll check init_enum_addr below.
           is InjectEnumAddrInst:
        break
      case let ieda as InitEnumDataAddrInst:
        if let previouslyFoundCase = caseIndex, previouslyFoundCase != ieda.caseIndex {
          return nil
        }
        caseIndex = ieda.caseIndex
        assert(payloadType == nil || payloadType! == ieda.type)
        payloadType = ieda.type
        numInits += 1
        initBlock = ieda.parentBlock
      case let uted as UncheckedTakeEnumDataAddrInst:
        if let previouslyFoundCase = caseIndex, previouslyFoundCase != uted.caseIndex {
          return nil
        }
        caseIndex = uted.caseIndex
        numTakes += 1
        takeBlock = uted.parentBlock
      default:
        return nil
      }
    }

    guard let caseIndex, let payloadType else {
      return nil
    }

    // If the enum has a single init-take pair in a single block, we know that the enum cannot contain any
    // valid payload outside that init-take pair.
    //
    // This also means that we can ignore any inject_enum_addr of another enum case, because this can only
    // inject a case without a payload.
    if numInits == 1 && numTakes == 1 && initBlock == takeBlock {
      return (payloadType, isSingleInitTakePair: true)
    }
    // No single init-take pair: We cannot ignore inject_enum_addrs with a mismatching case.
    if uses.users(ofType: InjectEnumAddrInst.self).contains(where: { $0.caseIndex != caseIndex}) {
      return nil
    }
    return (payloadType, isSingleInitTakePair: false)
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
    guard type.isExistential || type.isExistentialArchetype,
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
        assert(oea.uses.ignoreUsers(ofType: DestroyAddrInst.self).isEmpty)
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
        if !oea.uses.ignoreUsers(ofType: DestroyAddrInst.self).isEmpty {
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
