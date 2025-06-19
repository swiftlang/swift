//===--- TempLValueElimination.swift ---------------------------------------==//
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

import AST
import SIL

/// Eliminates copies from a temporary (an "l-value") to a destination.
///
/// ```
///   %temp = alloc_stack $T
///   ...                                     -+
///   store %x to %temp                        | no reads or writes to %destination
///   ...                                     -+
///   copy_addr [take] %temp to [init] %destination
///   dealloc_stack %temp
/// ```
/// ->
/// ```
///   ...
///   store %x to %destination
///   ...
/// ```
///
/// The name TempLValueElimination refers to the TempRValueElimination pass, which performs
/// a related transformation, just with the temporary on the "right" side.
///
/// The pass also performs a peephole optimization on `copy_addr` - `destroy_addr` sequences.
/// It replaces
///
/// ```
///   copy_addr %source to %destination
///   destroy_addr %source
/// ```
/// ->
/// ```
///   copy_addr [take] %source to %destination
/// ```
///
let tempLValueElimination = FunctionPass(name: "temp-lvalue-elimination") {
  (function: Function, context: FunctionPassContext) in

  for inst in function.instructions {
    switch inst {
    case let copy as CopyAddrInst:
      combineWithDestroy(copy: copy, context)
      tryEliminate(copy: copy, context)
    case let store as StoreInst:
      // Also handle `load`-`store` pairs which are basically the same thing as a `copy_addr`.
      if let load = store.source as? LoadInst, load.uses.isSingleUse, load.parentBlock == store.parentBlock {
        tryEliminate(copy: store, context)
      }
    default:
      break
    }
  }
}

private func tryEliminate(copy: CopyLikeInstruction, _ context: FunctionPassContext) {
  guard let allocStack = copy.sourceAddress as? AllocStackInst,
        allocStack.isDeallocatedInSameBlock(as: copy)
  else {
    return
  }
  let isTrivial = allocStack.type.isTrivial(in: copy.parentFunction)
  guard copy.isTakeOfSource || isTrivial else {
    return
  }

  // We need to move all destination address projections at the begin of the alloc_stack liverange,
  // because we are replacing the alloc_stack uses with the destination.
  // ```
  //                                                          %destination = struct_element_addr %1
  //   stores to %temp                            -->         stores to %destination
  //   %destination = struct_element_addr %1
  //   copy_addr [take] %temp to %destination
  // ```
  var projections = InstructionSet(context)
  defer { projections.deinitialize() }
  let destinationRootAddress = collectMovableProjections(of: copy.destinationAddress, in: &projections)

  // If true we need to explicitly destroy the destination at the begin of the liverange.
  // ```
  //                                                          destroy_addr %destination
  //   stores to %temp                            -->         stores to %destination
  //   copy_addr [take] %temp to %destination
  // ```
  let needDestroyEarly = !copy.isInitializationOfDestination && !isTrivial

  let firstUseOfAllocStack = InstructionList(first: allocStack).first(where: { $0.isUsing(allocStack) }) ??
                               // The conservative default, if the fist use is not in the alloc_stack's block.
                               allocStack.parentBlock.terminator

  if firstUseOfAllocStack == copy.loadingInstruction {
    // The alloc_stack is not written yet at the point of the copy. This is a very unusual corner case
    // which can only happen if the alloc_stack has an empty type (e.g. `$()`).
    return
  }

  let aliasAnalysis = context.aliasAnalysis
  let calleeAnalysis = context.calleeAnalysis

  if aliasAnalysis.mayAlias(allocStack, copy.destinationAddress) {
    // Catch the very unusual corner case where the copy is writing back to it's source address - the alloc_stack.
    return
  }

  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(firstUseOfAllocStack)

  // Check instructions within the liverange of the alloc_stack.
  while let inst = worklist.pop() {
    // If the destination root address is within the liverange it would prevent moving the projections
    // before the first use. Note that if the defining instruction of `destinationRootAddress` is nil
    // it can only be a function argument.
    if inst == destinationRootAddress.definingInstruction {
      return
    }

    // Check if the destination is not accessed within the liverange of the temporary.
    // This is unlikely, because the destination is initialized at the copy.
    // But still, the destination could contain an initialized value which is destroyed before the copy.
    if inst.mayReadOrWrite(address: copy.destinationAddress, aliasAnalysis) &&
        // Needed to treat `init_existential_addr` as not-writing projection.
        !projections.contains(inst)
    {
      return
    }

    // Check if replacing the alloc_stack with destination would invalidate the alias rules of indirect arguments.
    if let apply = inst as? FullApplySite,
       apply.hasInvalidArgumentAliasing(between: allocStack, and: copy.destinationAddress, aliasAnalysis)
    {
      return
    }

    // We must not shrink the liverange of an existing value in the destination.
    if needDestroyEarly && inst.isDeinitBarrier(calleeAnalysis) {
      return
    }

    worklist.pushSuccessors(of: inst, ignoring: copy)
  }

  if allocStack.isReadOrWritten(after: copy.loadingInstruction, aliasAnalysis) {
    // Bail in the unlikely case of the alloc_stack is re-initialized after its value has been taken by `copy`.
    return
  }

  moveProjections(of: copy.destinationAddress, within: worklist, before: firstUseOfAllocStack, context)

  if needDestroyEarly {
    // Make sure the destination is uninitialized before the liverange of the temporary.
    let builder = Builder(before: firstUseOfAllocStack, context)
    builder.createDestroyAddr(address: copy.destinationAddress)
  }

  // Replace all uses of the temporary with the destination address.
  for use in allocStack.uses {
    switch use.instruction {
    case let deallocStack as DeallocStackInst:
      context.erase(instruction: deallocStack)
    default:
      use.set(to: copy.destinationAddress, context)
    }
  }
  context.erase(instruction: allocStack)
  context.erase(instructionIncludingAllUsers: copy.loadingInstruction)
}

private extension FullApplySite {
  /// Returns true if after replacing `addr1` with `addr2` the apply would have invalid aliasing of
  /// indirect arguments.
  /// An indirect argument (except `@inout_aliasable`) must not alias with another indirect argument.
  /// For example, if we would replace `addr1` with `addr2` in
  /// ```
  ///   apply %f(%addr1, %addr2) : (@in T) -> @out T
  /// ```
  /// we would invalidate this rule.
  func hasInvalidArgumentAliasing(between addr1: Value, and addr2: Value, _ aliasAnalysis: AliasAnalysis) -> Bool {
    var addr1Accessed = false
    var addr2Accessed = false
    var mutatingAccess = false
    for argOp in argumentOperands {
      let convention = convention(of: argOp)!
      if convention.isExclusiveIndirect {
        if aliasAnalysis.mayAlias(addr1, argOp.value) {
          addr1Accessed = true
          if !convention.isGuaranteed {
            mutatingAccess = true
          }
        } else if aliasAnalysis.mayAlias(addr2, argOp.value) {
          addr2Accessed = true
          if !convention.isGuaranteed {
            mutatingAccess = true
          }
        }
      }
    }
    return mutatingAccess && addr1Accessed && addr2Accessed
  }
}

/// Replace
/// ```
///   copy_addr %source to %destination    -->     copy_addr [take] %source to %destination
///   destroy_addr %source
/// ```
private func combineWithDestroy(copy: CopyAddrInst, _ context: FunctionPassContext) {
  guard !copy.isTakeOfSource,
        let destroy = copy.source.uses.users(ofType: DestroyAddrInst.self).first,
        destroy.parentBlock == copy.parentBlock
  else {
    return
  }

  // Check if the destroy_addr is after the copy_addr and if there are no memory accesses between them.
  var debugInsts = Stack<DebugValueInst>(context)
  defer { debugInsts.deinitialize() }

  for inst in InstructionList(first: copy.next) {
    if inst == destroy {
      break
    }
    if let debugInst = inst as? DebugValueInst, debugInst.operand.value == copy.source {
      debugInsts.append(debugInst)
    }
    if inst.mayReadOrWriteMemory {
      return
    }
  }
  copy.set(isTakeOfSource: true, context)
  context.erase(instruction: destroy)
  // Don't let debug info think that the value is still valid after the `copy [take]`.
  context.erase(instructions: debugInsts)
}

private extension Value {
  var isMovableProjection: (SingleValueInstruction & UnaryInstruction)? {
    switch self {
      case let projectionInst as InitEnumDataAddrInst:          return projectionInst
      case let projectionInst as StructElementAddrInst:         return projectionInst
      case let projectionInst as TupleElementAddrInst:          return projectionInst
      case let projectionInst as UncheckedTakeEnumDataAddrInst: return projectionInst
      case let projectionInst as InitExistentialAddrInst:       return projectionInst
      case let projectionInst as RefElementAddrInst:            return projectionInst
      case let projectionInst as RefTailAddrInst:               return projectionInst
      case let projectionInst as ProjectBoxInst:                return projectionInst
      default: return nil
    }
  }
}

private func collectMovableProjections(of address: Value, in projections: inout InstructionSet) -> Value {
  var a = address
  while let projection = a.isMovableProjection {
    projections.insert(projection)
    a = projection.operand.value
  }
  return a
}

private func moveProjections(
  of address: Value,
  within worklist: InstructionWorklist,
  before insertionPoint: Instruction,
  _ context: FunctionPassContext
) {
  var a = address
  var ip = insertionPoint
  while let projection = a.isMovableProjection,
        worklist.hasBeenPushed(projection)
  {
    projection.move(before: ip, context)
    a = projection.operand.value
    ip = projection
  }
}

private extension AllocStackInst {
  func isReadOrWritten(after afterInst: Instruction, _ aliasAnalysis: AliasAnalysis) -> Bool {
    for inst in InstructionList(first: afterInst.next) {
      if let deallocStack = inst as? DeallocStackInst, deallocStack.allocatedValue == self {
        return false
      }
      if inst.mayReadOrWrite(address: self, aliasAnalysis) {
        return true
      }
    }
    fatalError("dealloc_stack expected to be in same block as `afterInst`")
  }

  func isDeallocatedInSameBlock(as inst: Instruction) -> Bool {
    if let deallocStack = uses.users(ofType: DeallocStackInst.self).singleElement,
       deallocStack.parentBlock == inst.parentBlock
    {
      return true
    }
    return false
  }
}
