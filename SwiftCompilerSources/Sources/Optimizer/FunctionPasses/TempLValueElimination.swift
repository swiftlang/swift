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
  removeTempLValues(in: function, keepDebugInfo: false, context)
}

let mandatoryTempLValueElimination = FunctionPass(name: "mandatory-temp-lvalue-elimination") {
  (function: Function, context: FunctionPassContext) in
  removeTempLValues(in: function, keepDebugInfo: true, context)
}

private func removeTempLValues(in function: Function, keepDebugInfo: Bool, _ context: FunctionPassContext) {
  for inst in function.instructions {
    switch inst {
    case let copy as CopyAddrInst:
      combineWithDestroy(copy: copy, keepDebugInfo: keepDebugInfo, context)
      tryEliminate(copy: copy, keepDebugInfo: keepDebugInfo, context)
    case let store as StoreInst:
      // Also handle `load`-`store` pairs which are basically the same thing as a `copy_addr`.
      if let load = store.source as? LoadInst, load.uses.isSingleUse, load.parentBlock == store.parentBlock {
        tryEliminate(copy: store, keepDebugInfo: keepDebugInfo, context)
      }
    default:
      break
    }
  }
}

private func tryEliminate(copy: CopyLikeInstruction, keepDebugInfo: Bool, _ context: FunctionPassContext) {
  guard let allocStack = copy.sourceAddress as? AllocStackInst,
        !allocStack.parentFunction.hasOwnership || !allocStack.hasDynamicLifetime
  else {
    return
  }

  if keepDebugInfo, allocStack.isFromVarDecl || allocStack.isLexical {
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

  if needDestroyEarly && !copy.parentFunction.hasOwnership {
    // In non-OSSA we cannot insert an early `destroy_addr`, because a loaded value could be retained later, e.g.
    // ```
    //   %1 = load %destination
    //   ...                     // we cannot insert a `destroy_addr %destination` here!
    //   stores to %temp
    //   strong_retain %1
    // ```
    return
  }

  guard let liverangeBegin = allocStack.liverangeBegin(context) else {
    return
  }

  if liverangeBegin == copy.loadingInstruction {
    // The alloc_stack is not written yet at the point of the copy. This is a very unusual corner case
    // which can only happen if the alloc_stack has an empty type (e.g. `$()`).
    return
  }

  // Writing an enum payload in other blocks than the copy's can prevent promoting the enum to registers,
  // e.g. a specialized `@out Optional` result which became a stack location.
  if liverangeBegin.parentBlock != copy.parentBlock && copy.destinationAddress.isInEnumPayload {
    return
  }

  let aliasAnalysis = context.aliasAnalysis
  let calleeAnalysis = context.calleeAnalysis

  if aliasAnalysis.mayAlias(allocStack, copy.destinationAddress) {
    // Catch the very unusual corner case where the copy is writing back to it's source address - the alloc_stack.
    return
  }

  // If exclusivity is checked for the alloc_stack we must not replace it with the copy-destination.
  // If the copy-destination is also in an access-scope this would result in an exclusivity violation
  // which was not there before.
  guard allocStack.uses.users(ofType: BeginAccessInst.self).isEmpty else {
    return
  }

  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(liverangeBegin)

  // Check instructions within the liverange of the alloc_stack.
  while let inst = worklist.pop() {
    // If the destination root address is within the liverange it would prevent moving the projections
    // before `liverangeBegin`. Note that if the defining instruction of `destinationRootAddress` is nil
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

    // On this path the destination would receive the alloc_stack's value without passing the copy.
    // A deallocation in the copy's block can only follow the `load` of a `load`-`store` pair.
    if let deallocStack = inst as? DeallocStackInst, deallocStack.allocatedValue == allocStack,
       deallocStack.parentBlock != copy.parentBlock
    {
      return
    }

    // A path back to `liverangeBegin` would execute the early destroy on the alloc_stack's value.
    if needDestroyEarly && inst.isImmediatePredecessor(of: liverangeBegin) {
      return
    }

    worklist.pushSuccessors(of: inst, ignoring: copy)
  }

  if allocStack.isReadOrWritten(after: copy, liverange: worklist, liverangeBegin: liverangeBegin,
                                aliasAnalysis, context) {
    return
  }

  moveProjections(of: copy.destinationAddress, within: worklist, before: liverangeBegin, context)

  if needDestroyEarly {
    // Make sure the destination is uninitialized before the liverange of the temporary.
    let builder = Builder(before: liverangeBegin, context)
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

  if keepDebugInfo {
    Builder(before: copy, context).createDebugStep()
  }

  // Salvage the debug variable attribute, if present.
  if let debugVariable = allocStack.debugVariable {
    let builder = Builder(before: liverangeBegin, location: allocStack.location, context)
    builder.createDebugValue(value: copy.destinationAddress, debugVariable: debugVariable)
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
private func combineWithDestroy(copy: CopyAddrInst, keepDebugInfo: Bool, _ context: FunctionPassContext) {
  guard !copy.isTakeOfSource else {
    return
  }

  if keepDebugInfo {
    // Combining degrades debug info if the source is an alloc_stack that has an
    // associated var decl or is lexical.
    if let allocStack = copy.source.definingInstruction as? AllocStackInst,
       allocStack.isFromVarDecl || allocStack.isLexical {
      return
    }
    // Debug values before the copy would become stale after the take, and debug
    // values between the copy and destroy would be lost.
    if !copy.source.uses.users(ofType: DebugValueInst.self).isEmpty {
      return
    }
  }

  // Check if the destroy_addr is after the copy_addr and if there are no memory accesses between them.
  var debugInsts = Stack<DebugValueInst>(context)
  defer { debugInsts.deinitialize() }

  for inst in InstructionList(first: copy.next) {
    switch inst {
    case let destroy as DestroyAddrInst where destroy.destroyedAddress == copy.source:
      copy.set(isTakeOfSource: true, context)
      context.erase(instruction: destroy)
      // Don't let debug info think that the value is still valid after the `copy [take]`.
      context.erase(instructions: debugInsts)
      return
    case let debugInst as DebugValueInst where debugInst.operands.contains(where: { $0.value == copy.source }):
      debugInsts.append(debugInst)
    default:
      if inst.mayReadOrWriteMemory {
        return
      }
    }
  }
}

private struct MovableProjection {
  var instruction: SingleValueInstruction
  var base: Value

  init<Inst>(_ inst: Inst) where Inst: SingleValueInstruction & UnaryInstruction {
    self.instruction = inst
    self.base = inst.operand.value
  }

  init<Inst>(_ inst: Inst, _ base: Value) where Inst: SingleValueInstruction {
    self.instruction = inst
    self.base = base
  }
}

private extension Value {
  var isMovableProjection: MovableProjection? {
    switch self {
      case let projectionInst as InitEnumDataAddrInst:    MovableProjection(projectionInst)
      case let projectionInst as StructElementAddrInst:   MovableProjection(projectionInst)
      case let projectionInst as TupleElementAddrInst:    MovableProjection(projectionInst)
      case let projectionInst as InitExistentialAddrInst: MovableProjection(projectionInst)
      case let projectionInst as RefElementAddrInst:      MovableProjection(projectionInst)
      case let projectionInst as RefTailAddrInst:         MovableProjection(projectionInst)
      case let projectionInst as ProjectBoxInst:          MovableProjection(projectionInst)
      case let projectionInst as UncheckedEnumDataAddrInstBase:
        MovableProjection(projectionInst, projectionInst.enum)
      default: nil
    }
  }

  var isInEnumPayload: Bool {
    var a: Value = self
    while let projection = a.isMovableProjection {
      if projection.instruction is InitEnumDataAddrInst {
        return true
      }
      a = projection.base
    }
    return false
  }
}

private func collectMovableProjections(of address: Value, in projections: inout InstructionSet) -> Value {
  var a = address
  while let projection = a.isMovableProjection {
    projections.insert(projection.instruction)
    a = projection.base
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
        worklist.hasBeenPushed(projection.instruction)
  {
    projection.instruction.move(before: ip, context)
    a = projection.base
    ip = projection.instruction
  }
}

private extension AllocStackInst {
  /// Returns the latest instruction which dominates all uses except `dealloc_stack`s.
  /// That is the use which dominates all other uses or, if there is none, the terminator of the nearest
  /// block which dominates all uses.
  func liverangeBegin(_ context: FunctionPassContext) -> Instruction? {
    if let use = InstructionList(first: self).first(where: { $0.isUsing(self) }) {
      return use
    }
    let domTree = context.dominatorTree
    var dominatingBlock: BasicBlock? = nil
    for use in uses.ignore(usersOfType: DeallocStackInst.self) {
      let useBlock = use.instruction.parentBlock
      var block = dominatingBlock ?? useBlock
      while !block.dominates(useBlock, domTree) {
        guard let idom = domTree.getImmediateDominator(of: block) else {
          return nil
        }
        block = idom
      }
      dominatingBlock = block
    }
    guard let dominatingBlock else {
      return nil
    }
    return dominatingBlock.instructions.first(where: { $0.isUsing(self) }) ?? dominatingBlock.terminator
  }

  /// Returns true if the alloc_stack is accessed after `copy` before it is deallocated, or if a path
  /// from `copy` re-enters the `liverange` other than at a `liverangeBegin` which re-initializes the alloc_stack.
  func isReadOrWritten(after copy: CopyLikeInstruction, liverange: InstructionWorklist, liverangeBegin: Instruction,
                       _ aliasAnalysis: AliasAnalysis, _ context: FunctionPassContext) -> Bool {
    // The `load` and the `store` of a `load`-`store` pair are in the same block.
    if copy.loadingInstruction != copy {
      for inst in InstructionList(first: copy.loadingInstruction.next) {
        if inst == copy {
          break
        }
        if let deallocStack = inst as? DeallocStackInst, deallocStack.allocatedValue == self {
          return false
        }
        if inst.mayReadOrWrite(address: self, aliasAnalysis) {
          return true
        }
      }
    }

    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushSuccessors(of: copy)

    while let inst = worklist.pop() {
      if let deallocStack = inst as? DeallocStackInst, deallocStack.allocatedValue == self {
        continue
      }
      if inst == liverangeBegin && liverangeBegin.isFullInitialization(of: self) {
        continue
      }
      if liverange.hasBeenPushed(inst) || inst.mayReadOrWrite(address: self, aliasAnalysis) {
        return true
      }
      worklist.pushSuccessors(of: inst)
    }
    return false
  }
}

private extension Instruction {
  func isImmediatePredecessor(of other: Instruction) -> Bool {
    if let next {
      return next == other
    }
    return other.previous == nil && parentBlock.successors.contains(other.parentBlock)
  }

  func isFullInitialization(of address: Value) -> Bool {
    switch self {
    case let store as StoreInst:
      return store.destination == address
    case let copy as CopyAddrInst:
      return copy.destination == address
    case let apply as ApplyInst:
      return apply.indirectResultOperands.contains { $0.value == address }
    default:
      return false
    }
  }
}
