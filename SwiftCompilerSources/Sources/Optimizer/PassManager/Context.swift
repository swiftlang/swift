//===--- Context.swift - defines the context types ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL
import OptimizerBridging

extension Context {
  var bridgedPassContext: BridgedPassContext {
    BridgedPassContext(_bridged)
  }

  var options: Options { Options(_bridged: bridgedPassContext) }

  var diagnosticEngine: DiagnosticEngine {
    return DiagnosticEngine(bridged: bridgedPassContext.getDiagnosticEngine())
  }

  // The calleeAnalysis is not specific to a function and therefore can be provided in
  // all contexts.
  var calleeAnalysis: CalleeAnalysis {
    let bridgeCA = bridgedPassContext.getCalleeAnalysis()
    return CalleeAnalysis(bridged: bridgeCA)
  }

  var hadError: Bool { bridgedPassContext.hadError() }

  /// Enable diagnostics requiring WMO (for @noLocks, @noAllocation
  /// annotations, Embedded Swift, and class specialization). SourceKit is the
  /// only consumer that has this disabled today (as it disables WMO
  /// explicitly).
  var enableWMORequiredDiagnostics: Bool {
    bridgedPassContext.enableWMORequiredDiagnostics()
  }

  func canMakeStaticObjectReadOnly(objectType: Type) -> Bool {
    bridgedPassContext.canMakeStaticObjectReadOnly(objectType.bridged)
  }

  func notifyNewFunction(function: Function, derivedFrom: Function) {
    bridgedPassContext.addFunctionToPassManagerWorklist(function.bridged, derivedFrom.bridged)
  }
}

extension MutatingContext {
  func notifyInvalidatedStackNesting() { bridgedPassContext.notifyInvalidatedStackNesting() }
  var needFixStackNesting: Bool { bridgedPassContext.getNeedFixStackNesting() }

  func tryOptimizeApplyOfPartialApply(closure: PartialApplyInst) -> Bool {
    if bridgedPassContext.tryOptimizeApplyOfPartialApply(closure.bridged) {
      notifyInstructionsChanged()
      notifyCallsChanged()

      for use in closure.callee.uses {
        if use.instruction is FullApplySite {
          notifyInstructionChanged(use.instruction)
        }
      }
      return true
    }
    return false
  }

  func tryDeleteDeadClosure(closure: SingleValueInstruction, needKeepArgsAlive: Bool = true) -> Bool {
    if bridgedPassContext.tryDeleteDeadClosure(closure.bridged, needKeepArgsAlive) {
      notifyInstructionsChanged()
      return true
    }
    return false
  }

  func tryDevirtualize(apply: FullApplySite, isMandatory: Bool) -> ApplySite? {
    let result = bridgedPassContext.tryDevirtualizeApply(apply.bridged, isMandatory)
    if let newApply = result.newApply.instruction {
      erase(instruction: apply)
      notifyInstructionsChanged()
      notifyCallsChanged()
      if result.cfgChanged {
        notifyBranchesChanged()
      }
      notifyInstructionChanged(newApply)
      return newApply as! FullApplySite
    }
    return nil
  }
  
  func tryOptimizeKeypath(apply: FullApplySite) -> Bool {
    return bridgedPassContext.tryOptimizeKeypath(apply.bridged)
  }

  func inlineFunction(apply: FullApplySite, mandatoryInline: Bool) {
    // This is only a best-effort attempt to notify the new cloned instructions as changed.
    // TODO: get a list of cloned instructions from the `inlineFunction`
    let instAfterInling: Instruction?
    switch apply {
    case is ApplyInst:
      instAfterInling = apply.next
    case let beginApply as BeginApplyInst:
      let next = beginApply.next!
      instAfterInling = (next is EndApplyInst ? nil : next)
    case is TryApplyInst:
      instAfterInling = apply.parentBlock.next?.instructions.first
    default:
      instAfterInling = nil
    }

    bridgedPassContext.inlineFunction(apply.bridged, mandatoryInline)

    if let instAfterInling = instAfterInling {
      notifyNewInstructions(from: apply, to: instAfterInling)
    }
  }

  func loadFunction(function: Function, loadCalleesRecursively: Bool) -> Bool {
    if function.isDefinition {
      return true
    }
    _bridged.loadFunction(function.bridged, loadCalleesRecursively)
    return function.isDefinition
  }

  private func notifyNewInstructions(from: Instruction, to: Instruction) {
    var inst = from
    while inst != to {
      if !inst.isDeleted {
        notifyInstructionChanged(inst)
      }
      if let next = inst.next {
        inst = next
      } else {
        inst = inst.parentBlock.next!.instructions.first!
      }
    }
  }

  func getContextSubstitutionMap(for type: Type) -> SubstitutionMap {
    SubstitutionMap(bridged: _bridged.getContextSubstitutionMap(type.bridged))
  }

  /// Notifies the pass manager that the optimization result of the current pass depends
  /// on the body (i.e. SIL instructions) of another function than the currently optimized one.
  func notifyDependency(onBodyOf otherFunction: Function) {
    bridgedPassContext.notifyDependencyOnBodyOf(otherFunction.bridged)
  }
}

/// The context which is passed to the run-function of a FunctionPass.
struct FunctionPassContext : MutatingContext {
  let _bridged: BridgedContext

  // A no-op.
  var notifyInstructionChanged: (Instruction) -> () { return { inst in } }

  func continueWithNextSubpassRun(for inst: Instruction? = nil) -> Bool {
    return bridgedPassContext.continueWithNextSubpassRun(inst.bridged)
  }

  func createSimplifyContext(preserveDebugInfo: Bool,
                             notifyInstructionChanged: @escaping (Instruction) -> ()
  ) -> SimplifyContext {
    SimplifyContext(_bridged: _bridged, notifyInstructionChanged: notifyInstructionChanged,
                    preserveDebugInfo: preserveDebugInfo)
  }

  var deadEndBlocks: DeadEndBlocksAnalysis {
    let bridgeDEA = bridgedPassContext.getDeadEndBlocksAnalysis()
    return DeadEndBlocksAnalysis(bridged: bridgeDEA)
  }

  var dominatorTree: DominatorTree {
    let bridgedDT = bridgedPassContext.getDomTree()
    return DominatorTree(bridged: bridgedDT)
  }

  var postDominatorTree: PostDominatorTree {
    let bridgedPDT = bridgedPassContext.getPostDomTree()
    return PostDominatorTree(bridged: bridgedPDT)
  }

  func loadFunction(name: StaticString, loadCalleesRecursively: Bool) -> Function? {
    return name.withUTF8Buffer { (nameBuffer: UnsafeBufferPointer<UInt8>) in
      let nameStr = BridgedStringRef(data: nameBuffer.baseAddress, count: nameBuffer.count)
      return _bridged.loadFunction(nameStr, loadCalleesRecursively).function
    }
  }

  func modifyEffects(in function: Function, _ body: (inout FunctionEffects) -> ()) {
    notifyEffectsChanged()
    function._modifyEffects(body)
  }

  fileprivate func notifyEffectsChanged() {
    _bridged.notifyChanges(.Effects)
  }

  func eliminateDeadAllocations(in function: Function) -> Bool {
    if bridgedPassContext.eliminateDeadAllocations(function.bridged) {
      notifyInstructionsChanged()
      return true
    }
    return false
  }

  func specializeClassMethodInst(_ cm: ClassMethodInst) -> Bool {
    if bridgedPassContext.specializeClassMethodInst(cm.bridged) {
      notifyInstructionsChanged()
      notifyCallsChanged()
      return true
    }
    return false
  }

  func specializeWitnessMethodInst(_ wm: WitnessMethodInst) -> Bool {
    if bridgedPassContext.specializeWitnessMethodInst(wm.bridged) {
      notifyInstructionsChanged()
      notifyCallsChanged()
      return true
    }
    return false
  }

  func specializeApplies(in function: Function, isMandatory: Bool) -> Bool {
    if bridgedPassContext.specializeAppliesInFunction(function.bridged, isMandatory) {
      notifyInstructionsChanged()
      notifyCallsChanged()
      return true
    }
    return false
  }

  func mangleOutlinedVariable(from function: Function) -> String {
    return String(taking: bridgedPassContext.mangleOutlinedVariable(function.bridged))
  }

  func mangle(withClosureArguments closureArgs: [Value], closureArgIndices: [Int], from applySiteCallee: Function) -> String {
    closureArgs.withBridgedValues { bridgedClosureArgsRef in
      closureArgIndices.withBridgedArrayRef{bridgedClosureArgIndicesRef in 
        String(taking: bridgedPassContext.mangleWithClosureArgs(
          bridgedClosureArgsRef,
          bridgedClosureArgIndicesRef, 
          applySiteCallee.bridged
        ))
      }
    }
  }

  func mangle(withBoxToStackPromotedArguments argIndices: [Int], from original: Function) -> String {
    argIndices.withBridgedArrayRef { bridgedArgIndices in
      String(taking: bridgedPassContext.mangleWithBoxToStackPromotedArgs(bridgedArgIndices, original.bridged))
    }
  }

  func createSpecializedFunctionDeclaration(from original: Function, withName specializedFunctionName: String,
                                            withParams specializedParameters: [ParameterInfo],
                                            makeThin: Bool = false,
                                            makeBare: Bool = false) -> Function
  {
    return specializedFunctionName._withBridgedStringRef { nameRef in
      let bridgedParamInfos = specializedParameters.map { $0._bridged }

      return bridgedParamInfos.withUnsafeBufferPointer { paramBuf in
        bridgedPassContext.createSpecializedFunctionDeclaration(nameRef, paramBuf.baseAddress, paramBuf.count,
                                                                original.bridged, makeThin, makeBare).function
      }
    }
  }

  func buildSpecializedFunction<T>(specializedFunction: Function, buildFn: (Function, FunctionPassContext) -> T) -> T {
    let nestedBridgedContext = bridgedPassContext.initializeNestedPassContext(specializedFunction.bridged)
    let nestedContext = FunctionPassContext(_bridged: nestedBridgedContext)
    defer { bridgedPassContext.deinitializedNestedPassContext() }

    return buildFn(specializedFunction, nestedContext)
  }

  /// Makes sure that the lifetime of `value` ends at all control flow paths, even in dead-end blocks.
  /// Inserts destroys in dead-end blocks if those are missing.
  func completeLifetime(of value: Value) {
    if bridgedPassContext.completeLifetime(value.bridged) {
      notifyInstructionsChanged()
    }
  }
}

struct SimplifyContext : MutatingContext {
  let _bridged: BridgedContext
  let notifyInstructionChanged: (Instruction) -> ()
  let preserveDebugInfo: Bool
}

extension Type {
  func getStaticSize(context: SimplifyContext) -> Int? {
    let v = context.bridgedPassContext.getStaticSize(self.bridged)
    return v == -1 ? nil : v
  }
  
  func getStaticAlignment(context: SimplifyContext) -> Int? {
    let v = context.bridgedPassContext.getStaticAlignment(self.bridged)
    return v == -1 ? nil : v
  }
  
  func getStaticStride(context: SimplifyContext) -> Int? {
    let v = context.bridgedPassContext.getStaticStride(self.bridged)
    return v == -1 ? nil : v
  }
}

//===----------------------------------------------------------------------===//
//                          Builder initialization
//===----------------------------------------------------------------------===//

private extension Instruction {
  /// Returns self, unless self is a meta instruction, in which case the next
  /// non-meta instruction is returned. Returns nil if there are no non-meta
  /// instructions in the basic block.
  var nextNonMetaInstruction: Instruction? {
    for inst in InstructionList(first: self) where !(inst is MetaInstruction) {
      return inst
    }
    return nil
  }
}

extension Builder {
  /// Creates a builder which inserts _before_ `insPnt`, using a custom `location`.
  init(before insPnt: Instruction, location: Location, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: insPnt.parentFunction)
    self.init(insertAt: .before(insPnt), location: location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts before `insPnt`, using `insPnt`'s next
  /// non-meta instruction's location.
  /// This function should be used when moving code to an unknown insert point,
  /// when we want to inherit the location of the closest non-meta instruction.
  /// For replacing an existing meta instruction with another, use
  /// ``Builder.init(replacing:_:)``.
  init(before insPnt: Instruction, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: insPnt.parentFunction)
    self.init(insertAt: .before(insPnt), location: insPnt.location,
              context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts _before_ `insPnt`, using the exact location of `insPnt`,
  /// for the purpose of replacing that meta instruction with an equivalent instruction.
  /// This function does not delete `insPnt`.
  init(replacing insPnt: MetaInstruction, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: insPnt.parentFunction)
    self.init(insertAt: .before(insPnt), location: insPnt.location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts _after_ `insPnt`, using a custom `location`.
  ///
  /// TODO: this is usually incorrect for terminator instructions. Instead use
  /// `Builder.insert(after:location:_:insertFunc)` from OptUtils.swift. Rename this to afterNonTerminator.
  init(after insPnt: Instruction, location: Location, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: insPnt.parentFunction)
    guard let nextInst = insPnt.next else {
      fatalError("cannot insert an instruction after a block terminator.")
    }
    self.init(insertAt: .before(nextInst), location: location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts _after_ `insPnt`, using `insPnt`'s next
  /// non-meta instruction's location.
  ///
  /// TODO: this is incorrect for terminator instructions. Instead use `Builder.insert(after:location:_:insertFunc)`
  /// from OptUtils.swift. Rename this to afterNonTerminator.
  init(after insPnt: Instruction, _ context: some MutatingContext) {
    self.init(after: insPnt, location: insPnt.location, context)
  }

  /// Creates a builder which inserts at the end of `block`, using a custom `location`.
  init(atEndOf block: BasicBlock, location: Location, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: block.parentFunction)
    self.init(insertAt: .atEndOf(block), location: location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts at the begin of `block`, using a custom `location`.
  init(atBeginOf block: BasicBlock, location: Location, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: block.parentFunction)
    let firstInst = block.instructions.first!
    self.init(insertAt: .before(firstInst), location: location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts at the begin of `block`, using the location of the first
  /// non-meta instruction of `block`.
  init(atBeginOf block: BasicBlock, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: block.parentFunction)
    let firstInst = block.instructions.first!
    self.init(insertAt: .before(firstInst),
              location: firstInst.location, context.notifyInstructionChanged, context._bridged)
  }

  /// Creates a builder which inserts instructions into an empty function, using the location of the function itself.
  init(atStartOf function: Function, _ context: some MutatingContext) {
    context.verifyIsTransforming(function: function)
    self.init(insertAt: .atStartOf(function), location: function.location,
              context.notifyInstructionChanged, context._bridged)
  }

  init(staticInitializerOf global: GlobalVariable, _ context: some MutatingContext) {
    self.init(insertAt: .staticInitializer(global),
              location: Location.artificialUnreachableLocation, { _ in }, context._bridged)
  }
}

//===----------------------------------------------------------------------===//
//                          Modifying the SIL
//===----------------------------------------------------------------------===//

extension Undef {
  static func get(type: Type, _ context: some MutatingContext) -> Undef {
    context._bridged.getSILUndef(type.bridged).value as! Undef
  }
}

extension BasicBlock {
  func addArgument(type: Type, ownership: Ownership, _ context: some MutatingContext) -> Argument {
    context.notifyInstructionsChanged()
    return bridged.addBlockArgument(type.bridged, ownership._bridged).argument
  }
  
  func addFunctionArgument(type: Type, _ context: some MutatingContext) -> FunctionArgument {
    context.notifyInstructionsChanged()
    return bridged.addFunctionArgument(type.bridged).argument as! FunctionArgument
  }

  func insertFunctionArgument(atPosition: Int, type: Type, ownership: Ownership, decl: ValueDecl? = nil,
                              _ context: some MutatingContext) -> FunctionArgument
  {
    context.notifyInstructionsChanged()
    return bridged.insertFunctionArgument(atPosition, type.bridged, ownership._bridged,
                                          (decl as Decl?).bridged).argument as! FunctionArgument
  }

  func eraseArgument(at index: Int, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.eraseArgument(index)
  }

  func moveAllInstructions(toBeginOf otherBlock: BasicBlock, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    context.notifyBranchesChanged()
    bridged.moveAllInstructionsToBegin(otherBlock.bridged)
  }

  func moveAllInstructions(toEndOf otherBlock: BasicBlock, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    context.notifyBranchesChanged()
    bridged.moveAllInstructionsToEnd(otherBlock.bridged)
  }

  func eraseAllArguments(_ context: some MutatingContext) {
    // Arguments are stored in an array. We need to erase in reverse order to avoid quadratic complexity.
    for argIdx in (0 ..< arguments.count).reversed() {
      eraseArgument(at: argIdx, context)
    }
  }

  func moveAllArguments(to otherBlock: BasicBlock, _ context: some MutatingContext) {
    bridged.moveArgumentsTo(otherBlock.bridged)
  }
}

extension Argument {
  func set(reborrow: Bool, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.setReborrow(reborrow)
  }
}

extension FunctionArgument {
  /// Copies the following flags from `arg`:
  /// 1. noImplicitCopy
  /// 2. lifetimeAnnotation
  /// 3. closureCapture
  /// 4. parameterPack
  func copyFlags(from arg: FunctionArgument, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.copyFlags(arg.bridged)
  }
}

extension AllocRefInstBase {
  func setIsStackAllocatable(_ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.AllocRefInstBase_setIsStackAllocatable()
    context.notifyInstructionChanged(self)
  }
}

extension Sequence where Element == Operand {
  func replaceAll(with replacement: Value, _ context: some MutatingContext) {
    for use in self {
      use.set(to: replacement, context)
    }
  }
}

extension Operand {
  func set(to value: Value, _ context: some MutatingContext) {
    instruction.setOperand(at: index, to: value, context)
  }

  func changeOwnership(from: Ownership, to: Ownership, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.changeOwnership(from._bridged, to._bridged)
    context.notifyInstructionChanged(instruction)
  }
}

extension Instruction {
  func setOperand(at index : Int, to value: Value, _ context: some MutatingContext) {
    if let apply = self as? FullApplySite, apply.isCallee(operand: operands[index]) {
      context.notifyCallsChanged()
    }
    context.notifyInstructionsChanged()
    bridged.setOperand(index, value.bridged)
    context.notifyInstructionChanged(self)
  }

  func move(before otherInstruction: Instruction, _ context: some MutatingContext) {
    BridgedContext.moveInstructionBefore(bridged, otherInstruction.bridged)
    context.notifyInstructionsChanged()
  }
}

extension BuiltinInst {
  func constantFold(_ context: SimplifyContext) -> Value? {
    context.bridgedPassContext.constantFoldBuiltin(bridged).value
  }
}

extension RefCountingInst {
  func setAtomicity(isAtomic: Bool, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.RefCountingInst_setIsAtomic(isAtomic)
    context.notifyInstructionChanged(self)
  }
}

extension AllocRefInst {
  func setIsBare(_ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.AllocRefInst_setIsBare()
    context.notifyInstructionChanged(self)
  }
}

extension RefElementAddrInst {
  func set(isImmutable: Bool, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.RefElementAddrInst_setImmutable(isImmutable)
    context.notifyInstructionChanged(self)
  }
}

extension GlobalAddrInst {
  func clearToken(_ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.GlobalAddrInst_clearToken()
    context.notifyInstructionChanged(self)
  }
}

extension GlobalValueInst {
  func setIsBare(_ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.GlobalValueInst_setIsBare()
    context.notifyInstructionChanged(self)
  }
}

extension LoadInst {
  func set(ownership: LoadInst.LoadOwnership, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.LoadInst_setOwnership(ownership.rawValue)
    context.notifyInstructionChanged(self)
  }
}

extension PointerToAddressInst {
  func set(alignment: Int?, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.PointerToAddressInst_setAlignment(UInt64(alignment ?? 0))
    context.notifyInstructionChanged(self)
  }
}

extension CopyAddrInst {
  func set(isTakeOfSource: Bool, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.CopyAddrInst_setIsTakeOfSrc(isTakeOfSource)
    context.notifyInstructionChanged(self)
  }

  func set(isInitializationOfDestination: Bool, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.CopyAddrInst_setIsInitializationOfDest(isInitializationOfDestination)
    context.notifyInstructionChanged(self)
  }
}

extension MarkDependenceInstruction {
  func resolveToNonEscaping(_ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.MarkDependenceInstruction_resolveToNonEscaping()
    context.notifyInstructionChanged(self)
  }

  func settleToEscaping(_ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.MarkDependenceInstruction_settleToEscaping()
    context.notifyInstructionChanged(self)
  }
}

extension BeginAccessInst {
  func set(accessKind: BeginAccessInst.AccessKind, context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.BeginAccess_setAccessKind(accessKind.rawValue)
    context.notifyInstructionChanged(self)
  }
}

extension TermInst {
  func replaceBranchTarget(from fromBlock: BasicBlock, to toBlock: BasicBlock, _ context: some MutatingContext) {
    context.notifyBranchesChanged()
    bridged.TermInst_replaceBranchTarget(fromBlock.bridged, toBlock.bridged)
  }
}

extension ForwardingInstruction {
  func setForwardingOwnership(to ownership: Ownership, _ context: some MutatingContext) {
    context.notifyInstructionsChanged()
    bridged.ForwardingInst_setForwardingOwnership(ownership._bridged)
  }
}

extension Function {
  func set(needStackProtection: Bool, _ context: FunctionPassContext) {
    context.notifyEffectsChanged()
    bridged.setNeedStackProtection(needStackProtection)
  }

  func set(thunkKind: ThunkKind, _ context: FunctionPassContext) {
    context.notifyEffectsChanged()
    switch thunkKind {
    case .noThunk:                 bridged.setThunk(.IsNotThunk)
    case .thunk:                   bridged.setThunk(.IsThunk)
    case .reabstractionThunk:      bridged.setThunk(.IsReabstractionThunk)
    case .signatureOptimizedThunk: bridged.setThunk(.IsSignatureOptimizedThunk)
    }
  }

  func set(isPerformanceConstraint: Bool, _ context: FunctionPassContext) {
    context.notifyEffectsChanged()
    bridged.setIsPerformanceConstraint(isPerformanceConstraint)
  }

  func fixStackNesting(_ context: FunctionPassContext) {
    context.bridgedPassContext.fixStackNesting(bridged)
  }

  func appendNewBlock(_ context: FunctionPassContext) -> BasicBlock {
    context.notifyBranchesChanged()
    return context._bridged.appendBlock(bridged).block
  }
}

extension DeclRef {
  func calleesAreStaticallyKnowable(_ context: some Context) -> Bool {
    context._bridged.calleesAreStaticallyKnowable(bridged)
  }
}
