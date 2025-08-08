//===--- OptUtils.swift - Utilities for optimizations ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL
import OptimizerBridging

// Default to SIL.Type within the Optimizer module.
typealias Type = SIL.`Type`

extension Value {
  var lookThroughBorrow: Value {
    if let beginBorrow = self as? BeginBorrowInst {
      return beginBorrow.borrowedValue.lookThroughBorrow
    }
    return self
  }

  var lookThroughCopy: Value {
    if let copy = self as? CopyValueInst {
      return copy.fromValue.lookThroughCopy
    }
    return self
  }

  var lookThoughOwnershipInstructions: Value {
    switch self {
    case let beginBorrow as BeginBorrowInst:
      return beginBorrow.borrowedValue.lookThoughOwnershipInstructions
    case let copy as CopyValueInst:
      return copy.fromValue.lookThoughOwnershipInstructions
    case let move as MoveValueInst:
      return move.fromValue.lookThoughOwnershipInstructions
    default:
      return self
    }
  }

  var lookThroughIndexScalarCast: Value {
    if let castBuiltin = self as? BuiltinInst {
      switch castBuiltin.id {
      case .TruncOrBitCast, .SExtOrBitCast:
        return castBuiltin.arguments[0]
      default:
        return self
      }
    }
    return self
  }

  func isInLexicalLiverange(_ context: some Context) -> Bool {
    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }

    worklist.pushIfNotVisited(self)
    while let v = worklist.pop() {
      if v.ownership == .none {
        continue
      }
      if v.isLexical {
        return true
      }
      switch v {
      case let fw as ForwardingInstruction:
        worklist.pushIfNotVisited(contentsOf: fw.definedOperands.lazy.map { $0.value })
      case let bf as BorrowedFromInst:
        worklist.pushIfNotVisited(bf.borrowedValue)
      case let bb as BeginBorrowInst:
        worklist.pushIfNotVisited(bb.borrowedValue)
      case let arg as Argument:
        if let phi = Phi(arg) {
          worklist.pushIfNotVisited(contentsOf: phi.incomingValues)
        } else if let termResult = TerminatorResult(arg),
               let fw = termResult.terminator as? ForwardingInstruction
        {
          worklist.pushIfNotVisited(contentsOf: fw.definedOperands.lazy.map { $0.value })
        }
      default:
        continue
      }
    }
    return false
  }

  /// Walks over all fields of an aggregate and checks if a reference count
  /// operation for this value is required. This differs from a simple `Type.isTrivial`
  /// check, because it treats a value_to_bridge_object instruction as "trivial".
  /// It can also handle non-trivial enums with trivial cases.
  func isTrivial(_ context: some Context) -> Bool {
    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }

    worklist.pushIfNotVisited(self)
    while let v = worklist.pop() {
      if v.type.isTrivial(in: parentFunction) {
        continue
      }
      if v.type.isValueTypeWithDeinit {
        return false
      }
      switch v {
      case is ValueToBridgeObjectInst:
        break
      case let si as StructInst:
        worklist.pushIfNotVisited(contentsOf: si.operands.values)
      case let ti as TupleInst:
        worklist.pushIfNotVisited(contentsOf: ti.operands.values)
      case let en as EnumInst:
        if let payload = en.payload {
          worklist.pushIfNotVisited(payload)
        }
      default:
        return false
      }
    }
    return true
  }

  func createProjection(path: SmallProjectionPath, builder: Builder) -> Value {
    let (kind, index, subPath) = path.pop()
    switch kind {
    case .root:
      return self
    case .structField:
      let structExtract = builder.createStructExtract(struct: self, fieldIndex: index)
      return structExtract.createProjection(path: subPath, builder: builder)
    case .tupleField:
      let tupleExtract = builder.createTupleExtract(tuple: self, elementIndex: index)
      return tupleExtract.createProjection(path: subPath, builder: builder)
    default:
      fatalError("path is not materializable")
    }
  }

  func createAddressProjection(path: SmallProjectionPath, builder: Builder) -> Value {
    let (kind, index, subPath) = path.pop()
    switch kind {
    case .root:
      return self
    case .structField:
      let structExtract = builder.createStructElementAddr(structAddress: self, fieldIndex: index)
      return structExtract.createAddressProjection(path: subPath, builder: builder)
    case .tupleField:
      let tupleExtract = builder.createTupleElementAddr(tupleAddress: self, elementIndex: index)
      return tupleExtract.createAddressProjection(path: subPath, builder: builder)
    default:
      fatalError("path is not materializable")
    }
  }

  func createProjectionAndCopy(path: SmallProjectionPath, builder: Builder) -> Value {
    if path.isEmpty {
      return self.copyIfNotTrivial(builder)
    }
    if self.ownership == .owned {
      let borrow = builder.createBeginBorrow(of: self)
      let projectedValue = borrow.createProjection(path: path, builder: builder)
      let result = projectedValue.copyIfNotTrivial(builder)
      builder.createEndBorrow(of: borrow)
      return result
    }
    let projectedValue = self.createProjection(path: path, builder: builder)
    return projectedValue.copyIfNotTrivial(builder)
  }

  func copyIfNotTrivial(_ builder: Builder) -> Value {
    if type.isTrivial(in: parentFunction) {
      return self
    }
    return builder.createCopyValue(operand: self)
  }

  /// True if this value is a valid in a static initializer, including all its operands.
  func isValidGlobalInitValue(_ context: some Context) -> Bool {
    guard let svi = self as? SingleValueInstruction else {
      return false
    }
    if let beginAccess = svi as? BeginAccessInst {
      return beginAccess.address.isValidGlobalInitValue(context)
    }
    if !svi.isValidInStaticInitializerOfGlobal(context) {
      return false
    }
    for op in svi.operands {
      if !op.value.isValidGlobalInitValue(context) {
        return false
      }
    }
    return true
  }

  /// Performs a simple dominance check without using the dominator tree.
  /// Returns true if `instruction` is in the same block as this value, but "after" this value,
  /// or if this value is a function argument.
  func triviallyDominates(_ instruction: Instruction) -> Bool {
    switch self {
    case is FunctionArgument:
      return true
    case let arg as Argument:
      return arg.parentBlock == instruction.parentBlock
    case let svi as SingleValueInstruction:
      return svi.dominatesInSameBlock(instruction)
    case let mvi as MultipleValueInstructionResult:
      return mvi.parentInstruction.dominatesInSameBlock(instruction)
    default:
      return false
    }
  }
}

extension FullApplySite {
  func isSemanticCall(_ name: StaticString, withArgumentCount: Int) -> Bool {
    if arguments.count == withArgumentCount,
       let callee = referencedFunction,
       callee.hasSemanticsAttribute(name)
    {
      return true
    }
    return false
  }
}

extension Builder {
  static func insert(after inst: Instruction, _ context: some MutatingContext, insertFunc: (Builder) -> ()) {
    Builder.insert(after: inst, location: inst.location, context, insertFunc: insertFunc)
  }

  static func insert(after inst: Instruction, location: Location,
                     _ context: some MutatingContext, insertFunc: (Builder) -> ()) {
    if inst is TermInst {
      for succ in inst.parentBlock.successors {
        assert(succ.hasSinglePredecessor,
               "the terminator instruction must not have critical successors")
        let builder = Builder(before: succ.instructions.first!, location: location,
                              context)
        insertFunc(builder)
      }
    } else {
      let builder = Builder(after: inst, location: location, context)
      insertFunc(builder)
    }
  }

  func destroyCapturedArgs(for paiOnStack: PartialApplyInst) {
    precondition(paiOnStack.isOnStack, "Function must only be called for `partial_apply`s on stack!")
    self.bridged.destroyCapturedArgs(paiOnStack.bridged)
  }
}

extension Value {
  /// Return true if all elements occur on or after `instruction` in
  /// control flow order. If this returns true, then zero or more uses
  /// of `self` may be operands of `instruction` itself.
  ///
  /// This performs a backward CFG walk from `instruction` to `self`.
  func usesOccurOnOrAfter(instruction: Instruction, _ context: some Context)
  -> Bool {
    var users = InstructionSet(context)
    defer { users.deinitialize() }
    users.insert(contentsOf: self.users)

    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }

    let pushPreds = { (block: BasicBlock) in
      block.predecessors.lazy.map({ pred in pred.terminator }).forEach {
        worklist.pushIfNotVisited($0)
      }
    }
    if let prev = instruction.previous {
      worklist.pushIfNotVisited(prev)
    } else {
      pushPreds(instruction.parentBlock)
    }
    let definingInst = self.definingInstruction
    while let lastInst = worklist.pop() {
      for inst in ReverseInstructionList(first: lastInst) {
        if users.contains(inst) {
          return false
        }
        if inst == definingInst {
          break
        }
      }
      if lastInst.parentBlock != self.parentBlock {
        pushPreds(lastInst.parentBlock)
      }
    }
    return true
  }
}

extension Value {
  /// Makes this new owned value available to be used in the block `destBlock`.
  ///
  /// Inserts required `copy_value` and `destroy_value` operations in case the `destBlock`
  /// is in a different control region than this value. For example, if `destBlock` is
  /// in a loop while this value is not in that loop, the value has to be copied for
  /// each loop iteration.
  func makeAvailable(in destBlock: BasicBlock, _ context: some MutatingContext) -> Value {
    assert(uses.isEmpty)
    assert(ownership == .owned)

    let beginBlock = parentBlock
    var useToDefRange = BasicBlockRange(begin: beginBlock, context)
    defer { useToDefRange.deinitialize() }

    useToDefRange.insert(destBlock)

    // The value needs to be destroyed at every exit of the liverange.
    for exitBlock in useToDefRange.exits {
      let builder = Builder(before: exitBlock.instructions.first!, context)
      builder.createDestroyValue(operand: self)
    }
  
    if useToDefRange.contains(destBlock) {
      // The `destBlock` is within a loop, so we need to copy the value at each iteration.
      let builder = Builder(before: destBlock.instructions.first!, context)
      return builder.createCopyValue(operand: self)
    }
    return self
  }

  /// Copies this value at `insertionPoint` and makes the copy available to be used in `destBlock`.
  ///
  /// For details see `makeAvailable`.
  func copy(at insertionPoint: Instruction, andMakeAvailableIn destBlock: BasicBlock,
            _ context: some MutatingContext) -> Value {
    let builder = Builder(before: insertionPoint, context)
    let copiedValue = builder.createCopyValue(operand: self)
    return copiedValue.makeAvailable(in: destBlock, context)
  }
}

extension Instruction {
  var isTriviallyDead: Bool {
    if results.contains(where: { !$0.uses.isEmpty }) {
      return false
    }
    return self.canBeRemovedIfNotUsed
  }

  var isTriviallyDeadIgnoringDebugUses: Bool {
    if results.contains(where: { !$0.uses.ignoreDebugUses.isEmpty }) {
      return false
    }
    return self.canBeRemovedIfNotUsed
  }

  private var canBeRemovedIfNotUsed: Bool {
    // TODO: it is horrible to hard-code exceptions here, but currently there is no Instruction API for this.
    switch self {
    case is TermInst, is MarkUninitializedInst, is DebugValueInst:
      return false
    case is BorrowedFromInst:
      // A dead borrowed-from can only be removed if the argument (= operand) is also removed.
      return false
    case let bi as BuiltinInst:
      if bi.id == .OnFastPath {
        return false
      }
    case is UncheckedEnumDataInst:
      // Don't remove UncheckedEnumDataInst in OSSA in case it is responsible
      // for consuming an enum value.
      return !parentFunction.hasOwnership
    case is ExtendLifetimeInst:
      // An extend_lifetime can only be removed if the operand is also removed.
      // If its operand is trivial, it will be removed by MandatorySimplification.
      return false
    default:
      break
    }
    return !mayReadOrWriteMemory && !hasUnspecifiedSideEffects
  }

  func isValidInStaticInitializerOfGlobal(_ context: some Context) -> Bool {
    // Rule out SILUndef and SILArgument.
    if operands.contains(where: { $0.value.definingInstruction == nil }) {
      return false
    }
    switch self {
    case let bi as BuiltinInst:
      switch bi.id {
      case .ZeroInitializer:
        let type = bi.type.isBuiltinVector ? bi.type.builtinVectorElementType(in: parentFunction) : bi.type
        return type.isBuiltinInteger || type.isBuiltinFloat
      case .PtrToInt:
        return bi.operands[0].value is StringLiteralInst
      case .IntToPtr:
        return bi.operands[0].value is IntegerLiteralInst
      case .StringObjectOr:
        // The first operand can be a string literal (i.e. a pointer), but the
        // second operand must be a constant. This enables creating a
        // a pointer+offset relocation.
        // Note that StringObjectOr requires the or'd bits in the first
        // operand to be 0, so the operation is equivalent to an addition.
        return bi.operands[1].value is IntegerLiteralInst
      case .ZExtOrBitCast:
        return true;
      case .USubOver:
        // Handle StringObjectOr(tuple_extract(usub_with_overflow(x, offset)), bits)
        // This pattern appears in UTF8 String literal construction.
        if let tei = bi.uses.getSingleUser(ofType: TupleExtractInst.self),
           tei.isResultOfOffsetSubtract {
          return true
        }
        return false
      case .OnFastPath:
        return true
      default:
        return false
      }
    case let tei as TupleExtractInst:
      // Handle StringObjectOr(tuple_extract(usub_with_overflow(x, offset)), bits)
      // This pattern appears in UTF8 String literal construction.
      if tei.isResultOfOffsetSubtract,
         let bi = tei.uses.getSingleUser(ofType: BuiltinInst.self),
         bi.id == .StringObjectOr {
        return true
      }
      return false
    case let sli as StringLiteralInst:
      switch sli.encoding {
      case .Bytes, .UTF8, .UTF8_OSLOG:
        return true
      case .ObjCSelector:
        // Objective-C selector string literals cannot be used in static
        // initializers.
        return false
      }
    case let gvi as GlobalValueInst:
      return context.canMakeStaticObjectReadOnly(objectType: gvi.type)
    case is StructInst,
         is TupleInst,
         is EnumInst,
         is IntegerLiteralInst,
         is FloatLiteralInst,
         is ObjectInst,
         is VectorInst,
         is UncheckedRefCastInst,
         is UpcastInst,
         is ValueToBridgeObjectInst,
         is ConvertFunctionInst,
         is ThinToThickFunctionInst,
         is AddressToPointerInst,
         is GlobalAddrInst,
         is FunctionRefInst:
      return true
    default:
      return false
    }
  }

  /// Returns true if `otherInst` is in the same block and is strictly dominated by this instruction.
  /// To be used as simple dominance check if both instructions are most likely located in the same block
  /// and no DominatorTree is available (like in instruction simplification).
  func dominatesInSameBlock(_ otherInst: Instruction) -> Bool {
    if parentBlock != otherInst.parentBlock {
      return false
    }
    // Walk in both directions. This is most efficient if both instructions are located nearby but it's not clear
    // which one comes first in the block's instruction list.
    var forwardIter = self
    var backwardIter = self
    while let f = forwardIter.next {
      if f == otherInst {
        return true
      }
      forwardIter = f
      if let b = backwardIter.previous {
        if b == otherInst {
          return false
        }
        backwardIter = b
      }
    }
    return false
  }
  
  /// Returns true if `otherInst` is in the same block and is strictly dominated by this instruction or
  /// the parent block of the instruction dominates parent block of `otherInst`.
  func dominates(
    _ otherInst: Instruction,
    _ domTree: DominatorTree
  ) -> Bool {
    if parentBlock == otherInst.parentBlock {
      return dominatesInSameBlock(otherInst)
    } else {
      return parentBlock.dominates(
        otherInst.parentBlock,
        domTree
      )
    }
  }

  /// If this instruction uses a (single) existential archetype, i.e. it has a type-dependent operand,
  /// returns the concrete type if it is known.
  var concreteTypeOfDependentExistentialArchetype: CanonicalType? {
    // For simplicity only support a single type dependent operand, which is true in most of the cases anyway.
    if let openArchetypeOp = typeDependentOperands.singleElement,
       // Match the sequence
       //   %1 = metatype $T
       //   %2 = init_existential_metatype %1, any P.Type
       //   %3 = open_existential_metatype %2 to $@opened(...)
       //   this_instruction_which_uses $@opened(...)  // type-defs: %3
       let oemt = openArchetypeOp.value as? OpenExistentialMetatypeInst,
       let iemt = oemt.operand.value as? InitExistentialMetatypeInst,
       let mt = iemt.metatype as? MetatypeInst
    {
      return mt.type.canonicalType.instanceTypeOfMetatype
    }
    // TODO: also handle open_existential_addr and open_existential_ref.
    // Those cases are currently handled in SILCombine's `propagateConcreteTypeOfInitExistential`.
    // Eventually we want to replace the SILCombine implementation with this one.
    return nil
  }
}

// Match the pattern:
// tuple_extract(usub_with_overflow(x, integer_literal, integer_literal 0), 0)
private extension TupleExtractInst {
  var isResultOfOffsetSubtract: Bool {
    if fieldIndex == 0,
       let bi = tuple as? BuiltinInst,
       bi.id == .USubOver,
       bi.operands[1].value is IntegerLiteralInst,
       let overflowLiteral = bi.operands[2].value as? IntegerLiteralInst,
       let overflowValue = overflowLiteral.value,
       overflowValue == 0
    {
      return true
    }
    return false
  }
}

extension StoreInst {
  func trySplit(_ context: FunctionPassContext) {
    let builder = Builder(after: self, context)
    let type = source.type
    if type.isStruct {
      if (type.nominal as! StructDecl).hasUnreferenceableStorage {
        return
      }
      if parentFunction.hasOwnership && source.ownership != .none {
        let destructure = builder.createDestructureStruct(struct: source)
        for (fieldIdx, fieldValue) in destructure.results.enumerated() {
          let destFieldAddr = builder.createStructElementAddr(structAddress: destination, fieldIndex: fieldIdx)
          builder.createStore(source: fieldValue, destination: destFieldAddr, ownership: splitOwnership(for: fieldValue))
        }
      } else {
        guard let fields = type.getNominalFields(in: parentFunction) else {
          return
        }
        for idx in 0..<fields.count {
          let srcField = builder.createStructExtract(struct: source, fieldIndex: idx)
          let fieldAddr = builder.createStructElementAddr(structAddress: destination, fieldIndex: idx)
          builder.createStore(source: srcField, destination: fieldAddr, ownership: splitOwnership(for: srcField))
        }
      }
    } else if type.isTuple {
      if parentFunction.hasOwnership && source.ownership != .none {
        let destructure = builder.createDestructureTuple(tuple: source)
        for (elementIdx, elementValue) in destructure.results.enumerated() {
          let elementAddr = builder.createTupleElementAddr(tupleAddress: destination, elementIndex: elementIdx)
          builder.createStore(source: elementValue, destination: elementAddr, ownership: splitOwnership(for: elementValue))
        }
      } else {
        for idx in 0..<type.tupleElements.count {
          let srcField = builder.createTupleExtract(tuple: source, elementIndex: idx)
          let destFieldAddr = builder.createTupleElementAddr(tupleAddress: destination, elementIndex: idx)
          builder.createStore(source: srcField, destination: destFieldAddr, ownership: splitOwnership(for: srcField))
        }
      }
    } else {
      return
    }
    context.erase(instruction: self)
  }

  private func splitOwnership(for fieldValue: Value) -> StoreOwnership {
    switch self.storeOwnership {
    case .trivial, .unqualified:
      return self.storeOwnership
    case .assign, .initialize:
      return fieldValue.type.isTrivial(in: parentFunction) ? .trivial : self.storeOwnership
    }
  }
}

extension LoadInst {
  @discardableResult
  func trySplit(_ context: FunctionPassContext) -> Bool {
    var elements = [LoadInst]()
    let builder = Builder(before: self, context)
    if type.isStruct {
      if (type.nominal as! StructDecl).hasUnreferenceableStorage {
        return false
      }
      guard let fields = type.getNominalFields(in: parentFunction) else {
        return false
      }
      for idx in 0..<fields.count {
        let fieldAddr = builder.createStructElementAddr(structAddress: address, fieldIndex: idx)
        let splitLoad = builder.createLoad(fromAddress: fieldAddr, ownership: self.splitOwnership(for: fieldAddr))
        elements.append(splitLoad)
      }
      let newStruct = builder.createStruct(type: self.type, elements: elements)
      self.replace(with: newStruct, context)
      
      return true
    } else if type.isTuple {
      let builder = Builder(before: self, context)
      for idx in 0..<type.tupleElements.count {
        let fieldAddr = builder.createTupleElementAddr(tupleAddress: address, elementIndex: idx)
        let splitLoad = builder.createLoad(fromAddress: fieldAddr, ownership: self.splitOwnership(for: fieldAddr))
        elements.append(splitLoad)
      }
      let newTuple = builder.createTuple(type: self.type, elements: elements)
      self.replace(with: newTuple, context)
      
      return true
    }
    return false
  }

  private func splitOwnership(for fieldValue: Value) -> LoadOwnership {
    switch self.loadOwnership {
    case .trivial, .unqualified:
      return self.loadOwnership
    case .copy, .take:
      return fieldValue.type.isTrivial(in: parentFunction) ? .trivial : self.loadOwnership
    }
  }
  
  func trySplit(
    alongPath projectionPath: SmallProjectionPath,
    _ context: FunctionPassContext
  ) -> [LoadInst]? {
    guard !projectionPath.isEmpty else {
      return nil
    }
    
    let (fieldKind, index, pathRemainder) = projectionPath.pop()
    
    var elements = [LoadInst]()
    let builder = Builder(before: self, context)
    
    switch fieldKind {
    case .structField where !(type.nominal as! StructDecl).hasUnreferenceableStorage && type.isStruct:
      guard let fields = type.getNominalFields(in: parentFunction) else {
        return nil
      }
      for idx in 0..<fields.count {
        let fieldAddr = builder.createStructElementAddr(structAddress: address, fieldIndex: idx)
        let splitLoad = builder.createLoad(fromAddress: fieldAddr, ownership: self.splitOwnership(for: fieldAddr))
        elements.append(splitLoad)
      }
      let newStruct = builder.createStruct(type: self.type, elements: elements)
      self.replace(with: newStruct, context)
      
      // TODO: Sometimes `index < elements.count` might fail, should investigate and find real fix. See `embedded/stdlib-strings-datatables.swift` without this check.
      if index < elements.count, let recursiveSplitLoad = elements[index].trySplit(alongPath: pathRemainder, context) {
        elements.remove(at: index)
        elements += recursiveSplitLoad
      }
      
      return elements
    case .tupleField where type.isTuple:
      let builder = Builder(before: self, context)
      for idx in 0..<type.tupleElements.count {
        let fieldAddr = builder.createTupleElementAddr(tupleAddress: address, elementIndex: idx)
        let splitLoad = builder.createLoad(fromAddress: fieldAddr, ownership: self.splitOwnership(for: fieldAddr))
        elements.append(splitLoad)
      }
      let newTuple = builder.createTuple(type: self.type, elements: elements)
      self.replace(with: newTuple, context)
      
      if let recursiveSplitLoad = elements[index].trySplit(alongPath: pathRemainder, context) {
        elements.remove(at: index)
        elements += recursiveSplitLoad
      }
      
      return elements
    default:
      return nil
    }
  }
}

extension FunctionPassContext {
  /// Returns true if any blocks were removed.
  func removeDeadBlocks(in function: Function) -> Bool {
    var reachableBlocks = ReachableBlocks(function: function, self)
    defer { reachableBlocks.deinitialize() }

    var blocksRemoved = false
    for block in function.blocks {
      if !reachableBlocks.isReachable(block: block) {
        block.dropAllReferences(self)
        erase(block: block)
        blocksRemoved = true
      }
    }
    return blocksRemoved
  }

  func removeTriviallyDeadInstructionsPreservingDebugInfo(in function: Function) {
    for inst in function.reversedInstructions {
      if inst.isTriviallyDead {
        erase(instruction: inst)
      }
    }
  }

  func removeTriviallyDeadInstructionsIgnoringDebugUses(in function: Function) {
    for inst in function.reversedInstructions {
      if inst.isTriviallyDeadIgnoringDebugUses {
        erase(instructionIncludingDebugUses: inst)
      }
    }
  }
}

extension BasicBlock {
  func dropAllReferences(_ context: FunctionPassContext) {
    for arg in arguments {
      arg.uses.replaceAll(with: Undef.get(type: arg.type, context), context)
    }
    for inst in instructions.reversed() {
      for result in inst.results {
        result.uses.replaceAll(with: Undef.get(type: result.type, context), context)
      }
      context.erase(instruction: inst)
    }
  }
}

extension SimplifyContext {

  /// Replaces a pair of redundant instructions, like
  /// ```
  ///   %first = enum $E, #E.CaseA!enumelt, %replacement
  ///   %second = unchecked_enum_data %first : $E, #E.CaseA!enumelt
  /// ```
  /// Replaces `%second` with `%replacement` and deletes the instructions if possible - or required.
  /// The operation is not done if it would require to insert a copy due to keep ownership correct.
  func tryReplaceRedundantInstructionPair(first: SingleValueInstruction, second: SingleValueInstruction,
                                          with replacement: Value) {
    let singleUse = preserveDebugInfo ? first.uses.singleUse : first.uses.ignoreDebugUses.singleUse
    let canEraseFirst = singleUse?.instruction == second

    if !canEraseFirst && first.parentFunction.hasOwnership {
      if replacement.ownership == .owned {
        // We cannot add more uses to `replacement` without inserting a copy.
        return
      }
      if first.ownership == .owned {
        // We have to insert a compensating destroy because we are deleting the second instruction but
        // not the first one. This can happen if the first instruction is an `enum` which constructs a
        // non-trivial enum from a trivial payload.
        let builder = Builder(before: second, self)
        builder.createDestroyValue(operand: first)
      }
    }

    second.replace(with: replacement, self)

    if canEraseFirst {
      erase(instructionIncludingDebugUses: first)
    }
  }
}

extension ProjectedValue {
  /// Returns true if the address can alias with `rhs`.
  ///
  /// Example:
  ///   %1 = struct_element_addr %s, #field1
  ///   %2 = struct_element_addr %s, #field2
  ///
  /// `%s`.canAddressAlias(with: `%1`) -> true
  /// `%s`.canAddressAlias(with: `%2`) -> true
  /// `%1`.canAddressAlias(with: `%2`) -> false
  ///
  func canAddressAlias(with rhs: ProjectedValue, complexityBudget: Int = Int.max, _ context: some Context) -> Bool {
    // self -> rhs will succeed (= return false) if self is a non-escaping "local" object,
    // but not necessarily rhs.
    if !isEscaping(using: EscapesToValueVisitor(target: rhs), complexityBudget: complexityBudget, context) {
      return false
    }
    // The other way round: rhs -> self will succeed if rhs is a non-escaping "local" object,
    // but not necessarily self.
    if !rhs.isEscaping(using: EscapesToValueVisitor(target: self), complexityBudget: complexityBudget, context) {
      return false
    }
    return true
  }
}

private struct EscapesToValueVisitor : EscapeVisitor {
  let target: ProjectedValue

  mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
    if operand.value == target.value && path.projectionPath.mayOverlap(with: target.path) {
      return .abort
    }
    if operand.instruction is ReturnInst {
      // Anything which is returned cannot escape to an instruction inside the function.
      return .ignore
    }
    return .continueWalk
  }

  var followTrivialTypes: Bool { true }
  var followLoads: Bool { false }
}

extension Function {
  /// Analyzes the global initializer function and returns global it initializes (from `alloc_global` instruction).
  var initializedGlobal: GlobalVariable? {
    guard isGlobalInitOnceFunction,
          let firstBlock = blocks.first
    else {
      return nil
    }
    for inst in firstBlock.instructions {
      if let allocGlobal = inst as? AllocGlobalInst {
        return allocGlobal.global
      }
    }
    return nil
  }

  /// True if this function has a dynamic-self metadata argument and any instruction is type dependent on it.
  var mayBindDynamicSelf: Bool {
    guard let dynamicSelf = self.dynamicSelfMetadata else {
      return false
    }
    return dynamicSelf.uses.contains { $0.isTypeDependent }
  }
}

extension FullApplySite {
  var inliningCanInvalidateStackNesting: Bool {
    guard let calleeFunction = referencedFunction else {
      return false
    }

    // In OSSA `partial_apply [on_stack]`s are represented as owned values rather than stack locations.
    // It is possible for their destroys to violate stack discipline.
    // When inlining into non-OSSA, those destroys are lowered to dealloc_stacks.
    // This can result in invalid stack nesting.
    if calleeFunction.hasOwnership && !parentFunction.hasOwnership {
      return true
    }
    // Inlining of coroutines can result in improperly nested stack allocations.
    if self is BeginApplyInst {
      return true
    }
    return false
  }
}

extension BeginApplyInst {
  var canInline: Bool { BeginApply_canInline(bridged) }
}

extension GlobalVariable {
  /// Removes all `begin_access` and `end_access` instructions from the initializer.
  ///
  /// Access instructions are not allowed in the initializer, because the initializer must not contain
  /// instructions with side effects (initializer instructions are not executed).
  /// Exclusivity checking does not make sense in the initializer.
  ///
  /// The initializer functions of globals, which reference other globals by address, contain access
  /// instructions. After the initializing code is copied to the global's initializer, those access
  /// instructions must be stripped.
  func stripAccessInstructionFromInitializer(_ context: FunctionPassContext) {
    guard let initInsts = staticInitializerInstructions else {
      return
    }
    for initInst in initInsts {
      switch initInst {
      case let beginAccess as BeginAccessInst:
        beginAccess.replace(with: beginAccess.address, context)
      case let endAccess as EndAccessInst:
        context.erase(instruction: endAccess)
      default:
        break
      }
    }
  }
}

extension InstructionRange {
  /// Adds the instruction range of a borrow-scope by transitively visiting all (potential) re-borrows.
  mutating func insert(borrowScopeOf borrow: BeginBorrowInstruction, _ context: some Context) {
    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }

    worklist.pushIfNotVisited(borrow)
    while let value = worklist.pop() {
      for use in value.uses {
        switch use.instruction {
        case let endBorrow as EndBorrowInst:
          self.insert(endBorrow)
        case let branch as BranchInst:
          worklist.pushIfNotVisited(branch.getArgument(for: use).lookThroughBorrowedFromUser)
        default:
          break
        }
      }
    }
  }
}

func canDynamicallyCast(from sourceType: CanonicalType, to destType: CanonicalType,
                        in function: Function, sourceTypeIsExact: Bool
) -> Bool? {
  switch classifyDynamicCastBridged(sourceType.bridged, destType.bridged, function.bridged, sourceTypeIsExact) {
    case .willSucceed: return true
    case .maySucceed:  return nil
    case .willFail:    return false
    default: fatalError("unknown result from classifyDynamicCastBridged")
  }
}

extension CheckedCastAddrBranchInst {
  var dynamicCastResult: Bool? {
    switch classifyDynamicCastBridged(bridged) {
      case .willSucceed: return true
      case .maySucceed:  return nil
      case .willFail:    return false
      default: fatalError("unknown result from classifyDynamicCastBridged")
    }
  }
}

extension CopyAddrInst {
  @discardableResult
  func trySplit(_ context: FunctionPassContext) -> Bool {
    let builder = Builder(before: self, context)
    if source.type.isStruct {
      if (source.type.nominal as! StructDecl).hasUnreferenceableStorage {
        return false
      }
      guard let fields = source.type.getNominalFields(in: parentFunction) else {
        return false
      }
      for idx in 0..<fields.count {
        let srcFieldAddr = builder.createStructElementAddr(structAddress: source, fieldIndex: idx)
        let destFieldAddr = builder.createStructElementAddr(structAddress: destination, fieldIndex: idx)
        builder.createCopyAddr(from: srcFieldAddr, to: destFieldAddr,
                               takeSource: isTake(for: srcFieldAddr), initializeDest: isInitializationOfDestination)
      }
      context.erase(instruction: self)
      return true
    } else if source.type.isTuple {
      let builder = Builder(before: self, context)
      for idx in 0..<source.type.tupleElements.count {
        let srcFieldAddr = builder.createTupleElementAddr(tupleAddress: source, elementIndex: idx)
        let destFieldAddr = builder.createTupleElementAddr(tupleAddress: destination, elementIndex: idx)
        builder.createCopyAddr(from: srcFieldAddr, to: destFieldAddr,
                               takeSource: isTake(for: srcFieldAddr), initializeDest: isInitializationOfDestination)
      }
      context.erase(instruction: self)
      return true
    }
    return false
  }

  private func isTake(for fieldValue: Value) -> Bool {
    return isTakeOfSource && !fieldValue.type.objectType.isTrivial(in: parentFunction)
  }

  @discardableResult
  func replaceWithLoadAndStore(_ context: some MutatingContext) -> (load: LoadInst, store: StoreInst) {
    let builder = Builder(before: self, context)
    let load = builder.createLoad(fromAddress: source, ownership: loadOwnership)
    let store = builder.createStore(source: load, destination: destination, ownership: storeOwnership)
    context.erase(instruction: self)
    return (load, store)
  }

  var loadOwnership: LoadInst.LoadOwnership {
    if !parentFunction.hasOwnership {
      return .unqualified
    }
    if type.isTrivial(in: parentFunction) {
      return .trivial
    }
    if isTakeOfSource {
      return .take
    }
    return .copy
  }

  var storeOwnership: StoreInst.StoreOwnership {
    if !parentFunction.hasOwnership {
      return .unqualified
    }
    if type.isTrivial(in: parentFunction) {
      return .trivial
    }
    if isInitializationOfDestination {
      return .initialize
    }
    return .assign
  }
}

extension Type {
  /// True if a type can be expanded without a significant increase to code
  /// size.
  /// Expanding a type can mean expressing it as a SSA value (which ultimately
  /// is represented as multiple SSA values in LLVM IR) instead of indirectly
  /// via memory operations (copy_addr), or exploding an SSA value into its
  /// constituent projections.
  /// Once a value is represented as its projections we don't "reconstitute" the
  /// aggregate value anymore leading to register pressure and code size bloat.
  /// Therefore, we try to keep "larger" values indirect and not exploated
  /// throughout the pipeline.
  ///
  /// False if expanding a type is invalid. For example, expanding a
  /// struct-with-deinit drops the deinit.
  func shouldExpand(_ context: some Context) -> Bool {
    if !context.options.useAggressiveReg2MemForCodeSize {
      return true
    }
    return context.bridgedPassContext.shouldExpand(self.bridged)
  }
}

/// Used by TempLValueElimination and TempRValueElimination to make the optimization work by both,
/// `copy_addr` and `load`-`store`-pairs.
protocol CopyLikeInstruction: Instruction {
  var sourceAddress: Value { get }
  var destinationAddress: Value { get }
  var isTakeOfSource: Bool { get }
  var isInitializationOfDestination: Bool { get }
  var loadingInstruction: Instruction { get }
}

extension CopyAddrInst: CopyLikeInstruction {
  var sourceAddress: Value { source }
  var destinationAddress: Value { destination }
  var loadingInstruction: Instruction { self }
}

// A `store` which has a `load` as source operand. This is basically the same as a `copy_addr`.
extension StoreInst: CopyLikeInstruction {
  var sourceAddress: Value { load.address }
  var destinationAddress: Value { destination }
  var isTakeOfSource: Bool { load.loadOwnership == .take }
  var isInitializationOfDestination: Bool { storeOwnership != .assign }
  var loadingInstruction: Instruction { load }
  private var load: LoadInst { source as! LoadInst }
}

func eraseIfDead(functions: [Function], _ context: ModulePassContext) {
  var toDelete = functions
  while true {
    var remaining = [Function]()
    for fn in toDelete {
      if !fn.isPossiblyUsedExternally && !fn.isReferencedInModule {
        context.erase(function: fn)
      } else {
        remaining.append(fn)
      }
    }
    if remaining.count == toDelete.count {
      return
    }
    toDelete = remaining
  }
}
