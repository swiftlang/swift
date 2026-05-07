//===--- CommonSubexpressionElimination.swift - CSE -----------------------===//
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
//
// A dominator-tree DFS pass that eliminates redundant instructions by replacing
// each duplicate with the first occurrence that dominates it.
//
// It is invoked using the flag:
//   "common-subexpression-elimination"   — post-inlining, runs on canonical SIL
//   "high-level-common-subexpression-elimination" — pre-inlining, also CSEs
//                                                   semantic array calls
//
// Lazy property getter applies are CSE-ed: two calls to the same getter on the
// same `self` value in a dominator relationship can share the first call's result.
//
//===----------------------------------------------------------------------===//

import SIL

// -----------------------------------------------------------------------
// Pass entry points
// -----------------------------------------------------------------------

let commonSubexpressionElimination = FunctionPass(name: "common-subexpression-elimination") {
  (function: Function, context: FunctionPassContext) in
  runCSE(function, context, false)
}

let highLevelCSE = FunctionPass(name: "high-level-common-subexpression-elimination") {
  (function: Function, context: FunctionPassContext) in
  runCSE(function, context, true)
}

func runCSE(
  _ function: Function,
  _ context: FunctionPassContext,
  _ runsOnHighLevelSil: Bool
) {
  guard let entryBlock = function.blocks.first else {
    return
  }

  var map = ScopedHashMap()

  // This will contain lazy property getter calls which can be optimized since
  // they are dominated by an equivalent call, meaning the switch_enum call in
  // the getter can be eliminated.
  var lazyPropertyGetters: [ApplyInst] = []

  processFunction(
    startBlock: entryBlock,
    &map,
    runsOnHighLevelSil,
    &lazyPropertyGetters,
    context
  )
  processLazyPropertyGetters(lazyPropertyGetters, function, context)
  if context.hasChanged(.Branches) {
    _ = context.removeDeadBlocks(in: function)
  }
  if context.needBreakInfiniteLoops {
    breakInfiniteLoops(in: function, context)
  }
}

// -----------------------------------------------------------------------
// DFS over the dominator tree
// -----------------------------------------------------------------------

/// Performs an iterative depth-first traversal of the dominator tree rooted at
/// `startBlock`, processing each block's instructions for CSE opportunities.
private func processFunction(
  startBlock: BasicBlock,
  _ map: inout ScopedHashMap,
  _ runsOnHighLevelSil: Bool = false,
  _ lazyPropertyGetters: inout [ApplyInst],
  _ context: FunctionPassContext
) {
  // Process the entry block, then drive the DFS.
  if !processBlock(startBlock, &map, runsOnHighLevelSil, &lazyPropertyGetters, context) {
    return
  }

  // Each stack frame holds the block being visited and an index into its
  // dominator-tree children so we can resume after processing each child.
  var dfsStack: [(block: BasicBlock, childIndex: Int)] = [(startBlock, 0)]

  while !dfsStack.isEmpty {
    let (block, childIndex) = dfsStack[dfsStack.count - 1]
    let children = context.dominatorTree.getChildren(of: block)

    if childIndex < children.count {
      // Advance the child pointer for this frame, then visit the next child.
      dfsStack[dfsStack.count - 1].childIndex += 1
      let child = children[childIndex]
      if !processBlock(child, &map, runsOnHighLevelSil, &lazyPropertyGetters, context) {
        return
      }
      dfsStack.append((child, 0))
    } else {
      // All children exhausted — pop this block's entries from the map
      // and remove its frame from the DFS stack.
      map.pop(block: block)
      dfsStack.removeLast()
    }
  }
}

// -----------------------------------------------------------------------
// Per-block processing
// -----------------------------------------------------------------------

/// Scans `block`'s instructions for CSE candidates.
///
/// For each instruction that can be CSE-ed:
///   - If an equivalent instruction already exists in `map`, replace `inst`
///     with that earlier occurrence and erase `inst`.
///   - Otherwise, insert `inst` into `map` so dominated blocks can find it.
private func processBlock(
  _ block: BasicBlock,
  _ map: inout ScopedHashMap,
  _ runsOnHighLevelSil: Bool = false,
  _ lazyPropertyGetters: inout [ApplyInst],
  _ context: FunctionPassContext
) -> Bool {
  // `block.instructions` skips deleted instructions automatically, so it is
  // safe to erase the current instruction while iterating.
  for inst in block.instructions {
    if inst.isTriviallyDead {
      context.erase(instruction: inst)
      continue
    }

    // getHash returns nil for ineligible instructions.
    guard
      let ref = InstructionReference(
        inst: inst,
        runsOnHighLevelSil: runsOnHighLevelSil,
        calleeAnalysis: context.calleeAnalysis
      )
    else {
      continue
    }
    // If an instruction can be handled here, then it must also be handled
    // in isIdenticalTo, otherwise looking up a key in the map will fail to
    // match itself.
    assert(inst.isIdenticalTo(inst), "inst must match itself for map to work")

    if let availInst = map.lookup(ref) {
      if !context.continueWithNextSubpassRun(for: inst) {
        return false
      }
      if let ai = inst as? ApplyInst, isOptimizableLazyPropertyGetter(ai) {
        lazyPropertyGetters.append(ai)
      } else {
        tryCSEReplace(inst: inst, with: availInst, context)
      }
      continue
    }

    map.insert(ref)
  }
  return true
}

private func hasGuaranteedSelf(_ ai: ApplyInst) -> Bool {
  let conventions = ai.calleeArgumentConventions
  guard let selfIdx = conventions.selfIndex else {
    return false
  }
  return conventions[selfIdx] == .directGuaranteed
}

private func canHandleArraySemanticsCall(_ ai: ApplyInst) -> Bool {
  switch ai.arraySemanticsCallKind {
  case .getCount, .getCapacity, .checkIndex, .checkSubscript:
    return hasGuaranteedSelf(ai)
  default:
    return false
  }
}

private func isPureOrGlobalInit(_ ai: ApplyInst, calleeAnalysis: CalleeAnalysis?) -> Bool {
  if !ai.mayReadOrWriteMemory {
    return true
  }

  if let ca = calleeAnalysis {
    let effects = ca.getSideEffects(ofApply: ai)
    if effects.getMemBehavior(observeRetains: false) == .None {
      return true
    }
  }

  if let callee = ai.referencedFunction, callee.isGlobalInitFunction {
    return true
  }

  return false
}

private func isOptimizableLazyPropertyGetter(_ ai: ApplyInst) -> Bool {
  guard let callee = ai.referencedFunction,
    callee.isDefinition,
    callee.isLazyPropertyGetter
  else {
    return false
  }

  // We cannot inline a non-ossa function into an ossa function
  if ai.parentFunction.hasOwnership && !callee.hasOwnership {
    return false
  }

  // Only handle classes, but not structs.
  // Lazy property getters of structs have an indirect inout self parameter.
  // We don't know if the whole struct is overwritten between two getter calls.
  // In such a case, the lazy property could be reset to an Optional.none.
  if ai.arguments[0].type.isAddress {
    return false
  }

  // Does the entry block check if the property has already been initialized?
  // We don't handle getters of generic types, which have a switch_enum_addr.
  // This will be obsolete with opaque values anyway.
  guard let sei = callee.entryBlock.terminator as? SwitchEnumInst else {
    return false
  }

  guard let someDest = sei.getUniqueSuccessor(forCaseIndex: Builder.optionalSomeCaseIndex) else {
    return false
  }
  return someDest.arguments.count == 1
}

/// Replace lazy property getters (which are dominated by the same getter)
/// by a direct load of the value.
private func processLazyPropertyGetters(
  _ lazyPropertyGetters: [ApplyInst],
  _ function: Function,
  _ context: FunctionPassContext
) {
  for ai in lazyPropertyGetters {
    guard let callee = ai.referencedFunction else {
      continue
    }
    assert(callee.isLazyPropertyGetter)
    let callBlock = ai.parentBlock

    if ai.inliningCanInvalidateStackNesting {
      context.notifyInvalidatedStackNesting()
    }

    assert(callee.isDefinition)
    context.inlineFunction(apply: ai, mandatoryInline: false)

    // After inlining the getter, the call block's terminator is the
    // switch_enum from the getter's entry block.
    guard let sei = callBlock.terminator as? SwitchEnumInst,
      let someDest = sei.getUniqueSuccessor(forCaseIndex: Builder.optionalSomeCaseIndex),
      someDest.arguments.count == 1
    else {
      continue
    }

    let builder = Builder(before: sei, context)
    let ued = builder.createUncheckedEnumData(
      enum: sei.enumOp,
      caseIndex: Builder.optionalSomeCaseIndex,
      resultType: someDest.arguments[0].type
    )
    _ = builder.createBranch(to: someDest, arguments: [ued])
    context.erase(instruction: sei)
  }

  if (context.needFixStackNesting) {
    context.fixStackNesting(in: function)
  }
}

// -----------------------------------------------------------------------
// Replace + erase
// -----------------------------------------------------------------------

/// Replaces `inst` with the dominating equivalent `availInst` and erases it.
private func tryCSEReplace(
  inst: Instruction,
  with availInst: Instruction,
  _ context: FunctionPassContext
) {
  for (result, availResult) in zip(inst.results, availInst.results) {
    result.uses.replaceAll(with: availResult, context)
  }
  context.erase(instruction: inst)
}

// -----------------------------------------------------------------------
// getHash — eligibility check and hash computation in one pass
// -----------------------------------------------------------------------

/// Returns the semantic hash of `inst` if it is eligible for CSE, or `nil` if
/// it must be skipped.
///
/// The hash must satisfy: if `a.isIdenticalTo(b)` then `getHash(a) == getHash(b)`.
/// It covers instruction kind, result type(s), operand SSA values, and any
/// instruction-specific attributes (field indices, literal values, etc.).
private func getHash(
  of inst: Instruction,
  runsOnHighLevelSil: Bool = false,
  calleeAnalysis: CalleeAnalysis? = nil
) -> Int? {
  // In OSSA, skip instructions that produce an owned result: replacing them
  // would require inserting copies (via OwnershipRAUW or similar) to maintain
  // ownership correctness. While this is possible, the inserted copies often
  // cannot be optimized away, making the CSE counterproductive. Trivial (none)
  // and guaranteed results are safe to replace without any fixup.
  if inst.parentFunction.hasOwnership && inst.results.contains(where: { $0.ownership == .owned }) {
    return nil
  }

  var hasher = Hasher()

  // Helper: hash kind (via dynamic type) + result type + all operands.
  // Used for most cast/conversion SingleValueInstructions.
  func hashTypeOps(_ i: SingleValueInstruction) {
    hasher.combine(ObjectIdentifier(type(of: i)))
    hasher.combine(i.type)
    for op in i.operands {
      hasher.combine(op.value.hashable)
    }
  }

  switch inst {

  case let bi as BuiltinInst:
    switch bi.id {
    case .OnFastPath:
      return nil  // must not be moved or removed even though side-effect-free
    case .Once, .OnceWithContext:
      break  // always eligible; fall through to hash below
    default:
      if bi.mayReadOrWriteMemory {
        return nil
      }
    }
    hasher.combine(ObjectIdentifier(BuiltinInst.self))
    hasher.combine(bi.name._bridged.data)
    hasher.combine(bi.substitutionMap.hasAnySubstitutableParams)
    for op in bi.operands {
      hasher.combine(op.value.hashable)
    }

  case let emi as ExistentialMetatypeInst:
    // Only CSE the value-operand form; the address form is not safe.
    if emi.operand.value.type.isAddress {
      return nil
    }
    hasher.combine(ObjectIdentifier(ExistentialMetatypeInst.self))
    hasher.combine(emi.type)

  // These instructions all use the same hash pattern: kind + result type + operands.
  case let x as UpcastInst: hashTypeOps(x)
  case let x as UncheckedRefCastInst: hashTypeOps(x)
  case let x as UncheckedAddrCastInst: hashTypeOps(x)
  case let x as UncheckedTrivialBitCastInst: hashTypeOps(x)
  case let x as UncheckedBitwiseCastInst: hashTypeOps(x)
  case let x as RefToRawPointerInst: hashTypeOps(x)
  case let x as RawPointerToRefInst: hashTypeOps(x)
  case let x as BridgeObjectToRefInst: hashTypeOps(x)
  case let x as BridgeObjectToWordInst: hashTypeOps(x)
  case let x as ClassifyBridgeObjectInst: hashTypeOps(x)
  case let x as ValueToBridgeObjectInst: hashTypeOps(x)
  case let x as RefToBridgeObjectInst: hashTypeOps(x)
  case let x as ThickToObjCMetatypeInst: hashTypeOps(x)
  case let x as ObjCToThickMetatypeInst: hashTypeOps(x)
  case let x as ObjCMetatypeToObjectInst: hashTypeOps(x)
  case let x as ObjCExistentialMetatypeToObjectInst: hashTypeOps(x)
  case let x as AddressToPointerInst: hashTypeOps(x)
  case let x as RefTailAddrInst: hashTypeOps(x)
  case let x as ProjectBoxInst: hashTypeOps(x)
  case let x as MarkDependenceInst: hashTypeOps(x)
  case let x as RefToUnownedInst: hashTypeOps(x)
  case let x as UnownedToRefInst: hashTypeOps(x)
  case let x as RefToUnmanagedInst: hashTypeOps(x)
  case let x as UnmanagedToRefInst: hashTypeOps(x)
  case let x as SelectEnumInst: hashTypeOps(x)
  case let x as TuplePackElementAddrInst: hashTypeOps(x)

  // PointerToAddressInst also hashes isStrict to distinguish strict vs
  // non-strict pointer dereferences.
  case let x as PointerToAddressInst:
    hasher.combine(ObjectIdentifier(PointerToAddressInst.self))
    hasher.combine(x.type)
    hasher.combine(x.isStrict)
    for op in x.operands {
      hasher.combine(op.value.hashable)
    }

  case let x as FunctionRefInst:
    hasher.combine(ObjectIdentifier(FunctionRefInst.self))
    hasher.combine(x.referencedFunction)

  case let x as GlobalAddrInst:
    hasher.combine(ObjectIdentifier(GlobalAddrInst.self))
    hasher.combine(x.global)

  // MetatypeInst has no operands; the type encodes the metatype identity.
  case let x as MetatypeInst:
    hasher.combine(ObjectIdentifier(MetatypeInst.self))
    hasher.combine(x.type)

  case let x as IntegerLiteralInst:
    hasher.combine(ObjectIdentifier(IntegerLiteralInst.self))
    hasher.combine(x.type)
    hasher.combine(x.value)  // Int? — nil for literals that don't fit in Int

  // FloatLiteralInst: hash the actual bit pattern via the bridge.
  // If bits is nil, we fall back to type-only, which is a safe hash collision.
  case let x as FloatLiteralInst:
    hasher.combine(ObjectIdentifier(FloatLiteralInst.self))
    hasher.combine(x.type)
    hasher.combine(x.bits)

  case let x as StringLiteralInst:
    hasher.combine(ObjectIdentifier(StringLiteralInst.self))
    hasher.combine(x.encoding)
    hasher.combine(x.value.string)

  case let x as TypeValueInst:
    hasher.combine(ObjectIdentifier(TypeValueInst.self))
    hasher.combine(x.type)
    hasher.combine(x.paramType)

  // FIXME: no StructDecl so we rely on the SIL type
  case let x as StructExtractInst:
    hasher.combine(ObjectIdentifier(StructExtractInst.self))
    hasher.combine(x.type)
    hasher.combine(x.fieldIndex)
    hasher.combine(x.operands[0].value.hashable)

  case let x as StructElementAddrInst:
    hasher.combine(ObjectIdentifier(StructElementAddrInst.self))
    hasher.combine(x.type)
    hasher.combine(x.fieldIndex)
    hasher.combine(x.operands[0].value.hashable)

  // fieldIndex + operand already uniquely identifies the projection within
  // the tuple, and the operand's SSA type encodes the tuple type.
  case let x as TupleExtractInst:
    hasher.combine(ObjectIdentifier(TupleExtractInst.self))
    hasher.combine(x.fieldIndex)
    hasher.combine(x.operands[0].value.hashable)

  case let x as TupleElementAddrInst:
    hasher.combine(ObjectIdentifier(TupleElementAddrInst.self))
    hasher.combine(x.fieldIndex)
    hasher.combine(x.operands[0].value.hashable)

  case let x as RefElementAddrInst:
    hasher.combine(ObjectIdentifier(RefElementAddrInst.self))
    hasher.combine(x.fieldIndex)
    hasher.combine(x.operands[0].value.hashable)

  // We use caseIndex (an Int) as the case discriminator, which encodes the
  // same information as VarDecl* within a given enum type.
  case let x as EnumInst:
    hasher.combine(ObjectIdentifier(EnumInst.self))
    hasher.combine(x.caseIndex)
    if let payload = x.payload {
      hasher.combine(payload.hashable)
    }

  // We use the result type as a proxy for the case — each case has a unique
  // payload type, so the type already encodes the case.
  case let x as UncheckedEnumDataInst:
    hasher.combine(ObjectIdentifier(UncheckedEnumDataInst.self))
    hasher.combine(x.type)
    hasher.combine(x.operands[0].value.hashable)

  // FIXME: no StructDecl so we rely on the SIL type
  case let x as StructInst:
    hasher.combine(ObjectIdentifier(StructInst.self))
    hasher.combine(x.type)
    for op in x.operands {
      hasher.combine(op.value.hashable)
    }

  case let x as TupleInst:
    hasher.combine(ObjectIdentifier(TupleInst.self))
    hasher.combine(x.type)
    for op in x.operands {
      hasher.combine(op.value.hashable)
    }

  // CondFailInst has no result value, so we only hash the condition operand.
  case let x as CondFailInst:
    hasher.combine(ObjectIdentifier(CondFailInst.self))
    hasher.combine(x.operand.value.hashable)

  case let x as IndexAddrInst:
    hasher.combine(ObjectIdentifier(IndexAddrInst.self))
    hasher.combine(x.type)
    for op in x.operands {
      hasher.combine(op.value.hashable)
    }

  case let x as IndexRawPointerInst:
    hasher.combine(ObjectIdentifier(IndexRawPointerInst.self))
    hasher.combine(x.type)
    for op in x.operands {
      hasher.combine(op.value.hashable)
    }

  case let x as ClassMethodInst:
    hasher.combine(ObjectIdentifier(ClassMethodInst.self))
    hasher.combine(x.type)
    hasher.combine(x.operands[0].value.hashable)

  case let x as SuperMethodInst:
    hasher.combine(ObjectIdentifier(SuperMethodInst.self))
    hasher.combine(x.type)
    hasher.combine(x.operands[0].value.hashable)

  case let x as WitnessMethodInst:
    hasher.combine(ObjectIdentifier(WitnessMethodInst.self))
    hasher.combine(x.type)
    hasher.combine(x.lookupType)
    hasher.combine(x.member)
    hasher.combine(x.conformance)
    for op in x.operands {
      hasher.combine(op.value.hashable)
    }

  case let x as ObjCProtocolInst:
    hasher.combine(ObjectIdentifier(ObjCProtocolInst.self))
    hasher.combine(x.type)
    hasher.combine(ObjectIdentifier(x.protocolDecl))

  case let x as ValueMetatypeInst:
    hasher.combine(ObjectIdentifier(ValueMetatypeInst.self))
    hasher.combine(x.type)
    hasher.combine(x.operands[0].value.hashable)

  case let x as InitExistentialMetatypeInst:
    hasher.combine(ObjectIdentifier(InitExistentialMetatypeInst.self))
    hasher.combine(x.type)
    hasher.combine(x.operands[0].value.hashable)
    for c in x.conformances {
      hasher.combine(c)
    }

  case let x as ScalarPackIndexInst:
    hasher.combine(ObjectIdentifier(ScalarPackIndexInst.self))
    hasher.combine(x.componentIndex)
    hasher.combine(x.indexedPackType)

  case let x as DynamicPackIndexInst:
    hasher.combine(ObjectIdentifier(DynamicPackIndexInst.self))
    hasher.combine(x.indexedPackType)
    for op in x.operands {
      hasher.combine(op.value.hashable)
    }

  case let x as ApplyInst:
    if !x.parentFunction.hasOwnership {
      if x.functionConvention.results.contains(where: { $0.convention != .unowned }) {
        return nil
      }
    }
    if isOptimizableLazyPropertyGetter(x) || (runsOnHighLevelSil && canHandleArraySemanticsCall(x))
      || (isPureOrGlobalInit(x, calleeAnalysis: calleeAnalysis))
    {
      hasher.combine(ObjectIdentifier(ApplyInst.self))
      for op in x.operands {
        hasher.combine(op.value.hashable)
      }
    } else {
      return nil
    }

  default:
    return nil
  }

  return hasher.finalize()
}

// -----------------------------------------------------------------------
// InstructionReference — Hashable wrapper with cached hash
// -----------------------------------------------------------------------

/// A `Hashable` wrapper around an `Instruction` that provides semantic equality
/// via `isIdenticalTo` and caches the pre-computed hash from `getHash(of:)`.
///
/// The failable initializer returns `nil` for instructions that are not eligible
/// for CSE.  Once constructed, the hash stored in `hash` is reused for every map
/// operation, avoiding redundant computation during lookup, insertion, and pop.
struct InstructionReference: Hashable {
  let inst: Instruction
  let hash: Int

  /// Returns `nil` if `inst` is not eligible for CSE.
  init?(inst: Instruction, runsOnHighLevelSil: Bool = false, calleeAnalysis: CalleeAnalysis? = nil) {
    guard
      let h = getHash(
        of: inst,
        runsOnHighLevelSil: runsOnHighLevelSil,
        calleeAnalysis: calleeAnalysis
      )
    else {
      return nil
    }
    self.inst = inst
    self.hash = h
  }

  static func == (lhs: Self, rhs: Self) -> Bool {
    if let lv = lhs.inst as? TypeValueInst, let rv = rhs.inst as? TypeValueInst {
      return lv.type == rv.type && lv.paramType == rv.paramType
    }
    return lhs.inst.isIdenticalTo(rhs.inst)
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(hash)
  }
}

// -----------------------------------------------------------------------
// ScopedHashMap — keyed hash table with DFS-scope push/pop
// -----------------------------------------------------------------------

/// Scoped hash table for the CSE pass.
///
/// Maps semantically-equivalent instructions (keyed by `InstructionReference`)
/// to their position in a backing stack array. The stack grows monotonically
/// during DFS of the dominator tree; `pop(block:)` unwinds it when the pass
/// leaves a basic block's scope.
///
/// Storing `InstructionReference` (rather than bare `Instruction`) in the
/// stack means `pop` can reuse the cached hash when removing map entries,
/// without re-running `getHash`.
struct ScopedHashMap {
  private var map: [InstructionReference: Int] = [:]
  private var stack: [InstructionReference] = []

  /// Insert `ref` into the map.
  ///
  /// If an equivalent instruction is already present, its entry is overwritten,
  /// which is consistent with dominator-tree-order traversal where a closer
  /// dominator should win.
  mutating func insert(_ ref: InstructionReference) {
    map[ref] = stack.count
    stack.append(ref)
  }

  /// Return the previously inserted instruction that is semantically
  /// equivalent to `ref.inst`, or `nil` if none exists.
  func lookup(_ ref: InstructionReference) -> Instruction? {
    guard let idx = map[ref] else {
      return nil
    }
    return stack[idx].inst
  }

  /// Remove all instructions whose `parentBlock` is `block`.
  ///
  /// Only removes a contiguous suffix of the stack, which is correct because
  /// the pass inserts instructions in dominator-tree DFS order and calls
  /// `pop(block:)` on the way back up — so all instructions from a given
  /// block are always at the top of the stack when that block is popped.
  mutating func pop(block: BasicBlock) {
    while let ref = stack.last, ref.inst.parentBlock === block {
      map.removeValue(forKey: ref)
      stack.removeLast()
    }
  }
}

extension ScopedHashMap: CustomStringConvertible {
  /// Lists each instruction in the map, one per line, in insertion order.
  public var description: String {
    stack.map { "  \($0.inst)" }.joined(separator: "\n")
  }
}

// -----------------------------------------------------------------------
// Tests
// -----------------------------------------------------------------------

/// Build a ScopedHashMap from all instructions in the first basic block
/// (duplicates are skipped via lookup), then print the map contents.
/// Used to verify that semantically identical instructions are deduplicated.
let scopedHashTableTest = FunctionTest("scoped-hash-table") {
  function,
  arguments,
  context in
  var map = ScopedHashMap()
  for inst in function.blocks.first!.instructions {
    guard let ref = InstructionReference(inst: inst) else {
      continue
    }
    if map.lookup(ref) == nil {
      map.insert(ref)
    }
  }
  print(map)
}

/// Insert `inst1` and then look up `inst2`.
/// Prints "hit" if the lookup succeeds, "miss" otherwise.
let scopedHashTableInsertLookupTest = FunctionTest("scoped-hash-table-insert-lookup") {
  function,
  arguments,
  context in
  let inst1 = arguments.takeInstruction()
  let inst2 = arguments.takeInstruction()
  var map = ScopedHashMap()
  map.insert(InstructionReference(inst: inst1)!)
  print(map.lookup(InstructionReference(inst: inst2)!) != nil ? "hit" : "miss")
}

/// Takes `@instruction` (expected to be an apply), prints "true" if it is a
/// call to a lazy property getter that the CSE pass can handle, "false" otherwise.
let isOptimizableLazyPropertyGetterTest = FunctionTest("is-optimizable-lazy-property-getter") {
  function,
  arguments,
  context in
  let inst = arguments.takeInstruction()
  guard let ai = inst as? ApplyInst else {
    print("not an apply")
    return
  }
  print(isOptimizableLazyPropertyGetter(ai) ? "true" : "false")
}

/// Insert `keepInst` (in a block other than `popBlock`) and `popInst` (in
/// `popBlock`), then call `pop(block: popBlock)`.
/// Prints whether each instruction remains in the map afterwards.
let scopedHashTablePopTest = FunctionTest("scoped-hash-table-pop") {
  function,
  arguments,
  context in
  let keepInst = arguments.takeInstruction()
  let popInst = arguments.takeInstruction()
  let popBlock = arguments.takeBlock()
  let keepRef = InstructionReference(inst: keepInst)!
  let popRef = InstructionReference(inst: popInst)!
  var map = ScopedHashMap()
  map.insert(keepRef)
  map.insert(popRef)
  map.pop(block: popBlock)
  print(map.lookup(keepRef) != nil ? "keep: hit" : "keep: miss")
  print(map.lookup(popRef) != nil ? "pop: hit" : "pop: miss")
}
