//===--- AllocBoxToStack.swift --------------------------------------------===//
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

/// Replaces `alloc_box` with `alloc_stack` if the box is not escaping.
///
/// ```
///   %1 = alloc_box  ${ var T }
///   %2 = project_box %1, 0
///   ...
///   store %3 to %2              // uses of the box field
///   ...
///   destroy_value %1            // end of lifetime of the box
/// ```
/// ->
/// ```
///   %2 = alloc_stack $T
///   ...
///   store %3 to %2              // uses of the stack location
///   ...
///   destroy_addr %2             // end of lifetime
///   dealloc_stack %2
/// ```
///
/// This transformation also works inter-procedurally. If the box is passed to a callee,
/// the callee is specialized: instead of the box argument the stack address is passed
/// as `@inout_aliasable` parameter:
///
/// ```
///   sil @closure : $(@guaranteed var { T }) -> () {
///   bb0(%0 : @guaranteed ${ var T }):
///     %1 = project_box %0
///     %2 = load %1
///     ...
/// ```
/// ->
/// ```
///   sil @specialized_closure : $(@inout_aliasable T) -> () {
///   bb0(%0 : $*T):
///     %2 = load %0
///     ...
/// ```
///
let allocBoxToStack = FunctionPass(name: "allocbox-to-stack") {
  (function: Function, context: FunctionPassContext) in

  _ = tryConvertBoxesToStack(in: function, context)
}

/// The "mandatory" version of the pass, which runs in the mandatory pipeline.
/// In contrast to the regular version, it is a module pass because it deletes the originals
/// of specialized closures, so that successive diagnostic passes don't report errors for
/// the unspecialized closures.
///
let mandatoryAllocBoxToStack = ModulePass(name: "mandatory-allocbox-to-stack") {
  (moduleContext: ModulePassContext) in

  var worklist = FunctionWorklist()
  worklist.pushIfNotVisited(contentsOf: moduleContext.functions)

  var originalsOfSpecializedFunctions = FunctionWorklist()

  while let function = worklist.pop() {
    moduleContext.transform(function: function) { context in
      let specFns = tryConvertBoxesToStack(in: function, context)
      worklist.pushIfNotVisited(contentsOf: specFns.specializedFunctions)
      originalsOfSpecializedFunctions.pushIfNotVisited(contentsOf: specFns.originalFunctions)
    }
  }

  /// This is needed because box-promotion is mandatory for non-copyable types. Without removing the
  /// original, un-specialized versions of closures we risk getting false errors in diagnostic passes.
  eraseIfDead(functions: originalsOfSpecializedFunctions.functions, moduleContext)
}

/// Converts all non-escaping `alloc_box` to `alloc_stack` and specializes called functions if a
/// box is passed to a function.
/// Returns the list of original functions for which a specialization has been created.
private func tryConvertBoxesToStack(in function: Function, _ context: FunctionPassContext) -> FunctionSpecializations {
  var promotableBoxes = Array<(AllocBoxInst, Flags)>()
  var functionsToSpecialize = FunctionSpecializations()

  findPromotableBoxes(in: function, &promotableBoxes, &functionsToSpecialize)

  functionsToSpecialize.createSpecializedFunctions(context)

  for (box, flags) in promotableBoxes {
    let stack = createAllocStack(for: box, flags: flags, context)
    functionsToSpecialize.rewriteUses(of: box, with: stack, context)
    context.erase(instruction: box)

    hoistMarkUnresolvedInsts(stackAddress: stack, checkKind: .consumableAndAssignable, context)
  }
  if !promotableBoxes.isEmpty {
    function.fixStackNesting(context)
  }

  return functionsToSpecialize
}

private func findPromotableBoxes(in function: Function,
                                 _ promotableBoxes: inout Array<(AllocBoxInst, Flags)>,
                                 _ functionsToSpecialize: inout FunctionSpecializations
) {
  for inst in function.instructions {
    if let allocBox = inst as? AllocBoxInst {
      if let (promotableArgs, flags) = canPromote(allocBox: allocBox) {
        promotableBoxes.append((allocBox, flags))
        functionsToSpecialize.add(promotableArguments: promotableArgs)
      }
    }
  }
}

private typealias Flags = (isLexical: Bool, isFromVarDecl: Bool)

private func canPromote(allocBox: AllocBoxInst) -> (promotableArguments: [FunctionArgument], flags: Flags)? {
  // For simplicity we only support boxes with a single field. This is okay because SILGen only generates
  // such kind of boxes.
  guard allocBox.type.getBoxFields(in: allocBox.parentFunction).count == 1 else {
    return nil
  }

  var argumentsToPromote = Array<FunctionArgument>()
  var flags = (isLexical: false, isFromVarDecl: false)

  // Contains all visited box _and_ closure values (`partial_apply`) of the current function and
  // all callees, which need to be specialized.
  var worklist = CrossFunctionValueWorklist()
  worklist.pushIfNotVisited(allocBox)

  while let value = worklist.pop() {
    for use in value.uses {
      // Note: all instructions which are handled here must also be handled in `FunctionSpecializations.rewriteUses`!
      switch use.instruction {
      case is StrongRetainInst, is StrongReleaseInst, is ProjectBoxInst, is DestroyValueInst,
           is EndBorrowInst, is DebugValueInst, is DeallocStackInst:
        break
      case let deallocBox as DeallocBoxInst where deallocBox.parentFunction == allocBox.parentFunction:
        break
      case let beginBorrow as BeginBorrowInst:
        flags.isLexical = flags.isLexical || beginBorrow.isLexical
        flags.isFromVarDecl = flags.isFromVarDecl || beginBorrow.isFromVarDecl
        fallthrough
      case is MarkUninitializedInst, is CopyValueInst, is MoveValueInst:
        worklist.pushIfNotVisited(use.instruction as! SingleValueInstruction)
      case let apply as ApplySite:
        if apply.isCallee(operand: use) {
          // Calling the closure does not escape the closure value.
          break
        }
        guard let callee = apply.getSpecializableCallee() else {
          return nil
        }
        let calleeArg = apply.calleeArgument(of: use, in: callee)!
        // Continue checking the box (or closure) value in the callee.
        worklist.pushIfNotVisited(calleeArg)

        if value.type.isBox {
          // We need to specialize this function by replacing the box argument with an address.
          argumentsToPromote.append(calleeArg)
        }
        if let partialApply = apply as? PartialApplyInst {
          // We need to check if the captured argument is escaping via the partial_apply.
          worklist.pushIfNotVisited(partialApply)
        }
      default:
        return nil
      }
    }
  }
  return (argumentsToPromote, flags)
}

/// Utility for specializing functions by promoting box arguments to `@inout_aliasable` address arguments.
private struct FunctionSpecializations {

  // All box arguments (in all functions) which are promoted from box to address.
  private var promotableArguments = CrossFunctionValueWorklist()
  private var originals = FunctionWorklist()
  private var originalToSpecialized = Dictionary<Function, Function>()

  var originalFunctions: [Function] { originals.functions }
  var specializedFunctions: [Function] { originals.functions.lazy.map { originalToSpecialized[$0]! } }

  mutating func add(promotableArguments: [FunctionArgument]) {
    for arg in promotableArguments {
      self.promotableArguments.pushIfNotVisited(arg)
      self.originals.pushIfNotVisited(arg.parentFunction)
    }
  }

  mutating func createSpecializedFunctions(_ context: FunctionPassContext) {
    // It's important to first create _all_ declarations before creating the function bodies, because
    // a function body may reference another specialized declaration - in any order.
    for f in originals.functions {
      originalToSpecialized[f] = createSpecializedDeclaration(for: f, context)
    }
    for f in originals.functions {
      createSpecializedBody(for: f, context)
    }
  }

  /// Rewrites all uses of `box` with the `stack` address where `box` is either an `alloc_box` in
  /// the original function or a promoted box argument in a specialized function.
  func rewriteUses(of box: Value, with stack: Value, _ context: FunctionPassContext) {
    while let use = box.uses.first {
      let user = use.instruction
      switch user {
      case is StrongRetainInst, is StrongReleaseInst, is DestroyValueInst, is EndBorrowInst, is DeallocBoxInst:
        context.erase(instruction: user)
      case let projectBox as ProjectBoxInst:
        assert(projectBox.fieldIndex == 0, "only single-field boxes are handled")
        projectBox.replace(with: stack, context)
      case is MarkUninitializedInst, is CopyValueInst, is BeginBorrowInst, is MoveValueInst:
        // First, replace the instruction with the original `box`, which adds more uses to `box`.
        // In a later iteration those additional uses will be handled.
        (user as! SingleValueInstruction).replace(with: box, context)
      case let apply as ApplySite:
        specialize(apply: apply, context)
      default:
        fatalError("unhandled box user")
      }
    }
  }

  /// Replaces `apply` with a new apply of the specialized callee.
  private func specialize(apply: ApplySite, _ context: FunctionPassContext) {
    let fri = apply.callee as! FunctionRefInst
    let callee = fri.referencedFunction
    let builder = Builder(before: apply, context)
    let newArgs = apply.argumentOperands.map { (argOp) -> Value in
      if promotableArguments.hasBeenPushed(apply.calleeArgument(of: argOp, in: callee)!) {
        return builder.createProjectBox(box: argOp.value, fieldIndex: 0)
      } else {
        return argOp.value
      }
    }
    let specializedCallee = builder.createFunctionRef(originalToSpecialized[callee]!)

    switch apply {
    case let applyInst as ApplyInst:
      let newApply = builder.createApply(function: specializedCallee, applyInst.substitutionMap, arguments: newArgs)
      applyInst.replace(with: newApply, context)
    case let partialAp as PartialApplyInst:
      let newApply = builder.createPartialApply(function: specializedCallee, substitutionMap:
                                                partialAp.substitutionMap,
                                                capturedArguments: newArgs,
                                                calleeConvention: partialAp.calleeConvention,
                                                hasUnknownResultIsolation: partialAp.hasUnknownResultIsolation,
                                                isOnStack: partialAp.isOnStack)
      partialAp.replace(with: newApply, context)
    case let tryApply as TryApplyInst:
      builder.createTryApply(function: specializedCallee, tryApply.substitutionMap, arguments: newArgs,
                             normalBlock: tryApply.normalBlock, errorBlock: tryApply.errorBlock)
      context.erase(instruction: tryApply)
    case let beginApply as BeginApplyInst:
      let newApply = builder.createBeginApply(function: specializedCallee, beginApply.substitutionMap,
                                              arguments: newArgs)
      beginApply.replace(with: newApply, context)
    default:
      fatalError("unknown apply")
    }
    // It is important to delete the dead `function_ref`. Otherwise it will still reference the original
    // function which prevents deleting it in the mandatory-allocbox-to-stack pass.
    if fri.uses.isEmpty {
      context.erase(instruction: fri)
    }
  }

  private func createSpecializedDeclaration(for function: Function, _ context: FunctionPassContext) -> Function
  {
    let argIndices = function.arguments.enumerated().filter {
      promotableArguments.hasBeenPushed($0.element)
    }.map { $0.offset }
    let name = context.mangle(withBoxToStackPromotedArguments: argIndices, from: function)

    if let existingSpecialization = context.lookupFunction(name: name) {
      // This can happen if a previous run of the pass already created this specialization.
      return existingSpecialization
    }

    let params = function.convention.parameters.enumerated().map { (paramIdx, param) in
      let arg = function.arguments[function.convention.indirectSILResultCount + paramIdx]
      if promotableArguments.hasBeenPushed(arg) {
        return ParameterInfo(type: param.type.getBoxFields(in: function).singleElement!.canonicalType,
                             convention: .indirectInoutAliasable,
                             options: 0,
                             hasLoweredAddresses: param.hasLoweredAddresses)
      } else {
        return param
      }
    }
    return context.createSpecializedFunctionDeclaration(from: function, withName: name, withParams: params)
  }

  private func createSpecializedBody(for original: Function, _ context: FunctionPassContext)
  {
    let specializedFunc = originalToSpecialized[original]!
    if specializedFunc.isDefinition {
      // This can happen if a previous run of the pass already created this specialization.
      return
    }
    context.buildSpecializedFunction(specializedFunction: specializedFunc) { (specializedFunc, specContext) in
      cloneFunction(from: original, toEmpty: specializedFunc, specContext)

      replaceBoxWithStackArguments(in: specializedFunc, original: original, specContext)
    }
    context.notifyNewFunction(function: specializedFunc, derivedFrom: original)
  }

  private func replaceBoxWithStackArguments(in specializedFunc: Function, original: Function,
                                            _ context: FunctionPassContext
  ) {
    for (argIdx, (origBoxArg, boxArg)) in zip(original.arguments, specializedFunc.arguments).enumerated() {
      if promotableArguments.hasBeenPushed(origBoxArg) {
        let boxFields = boxArg.type.getBoxFields(in: specializedFunc)
        let stackArg = specializedFunc.entryBlock.insertFunctionArgument(
          atPosition: argIdx, type: boxFields[0], ownership: .none, decl: boxArg.decl, context)
        stackArg.copyFlags(from: boxArg, context)

        rewriteUses(of: boxArg, with: stackArg, context)
        specializedFunc.entryBlock.eraseArgument(at: argIdx + 1, context)

        hoistMarkUnresolvedInsts(
          stackAddress: stackArg,
          checkKind: boxFields.isMutable(fieldIndex: 0) ? .consumableAndAssignable : .noConsumeOrAssign,
          context)
      }
    }
  }
}

/// Replaces an `alloc_box` with an `alloc_stack` and inserts `destroy_addr` and `dealloc_stack`
/// at the end of the lifetime.
private func createAllocStack(for allocBox: AllocBoxInst, flags: Flags, _ context: FunctionPassContext) -> Value {
  let builder = Builder(before: allocBox, context)
  let unboxedType = allocBox.type.getBoxFields(in: allocBox.parentFunction)[0]
  let asi = builder.createAllocStack(unboxedType,
                                     debugVariable: allocBox.debugVariable,
                                     hasDynamicLifetime: allocBox.hasDynamicLifetime,
                                     isLexical: flags.isLexical,
                                     isFromVarDecl: flags.isFromVarDecl)
  let stackLocation: Value
  if let mu = allocBox.uses.getSingleUser(ofType: MarkUninitializedInst.self) {
    stackLocation = builder.createMarkUninitialized(value: asi, kind: mu.kind)
  } else {
    stackLocation = asi
  }

  for destroy in getFinalDestroys(of: allocBox, context) {
    let loc = allocBox.location.asCleanup.withScope(of: destroy.location)
    Builder.insert(after: destroy, location: loc, context) { builder in
      if  !unboxedType.isTrivial(in: allocBox.parentFunction), !(destroy is DeallocBoxInst) {
        builder.createDestroyAddr(address: stackLocation)
      }
      builder.createDeallocStack(asi)
    }
  }
  return stackLocation
}

/// Returns the list of final destroy instructions of `allocBox`.
/// In case the box is copied, not all `destroy_value`s are final destroys, e.g.
/// ```
///   %1 = alloc_box  ${ var T }
///   %2 = copy_value %1
///   destroy_value %1            // not a final destroy
///   destroy_value %2            // a final destroy
/// ```
private func getFinalDestroys(of allocBox: AllocBoxInst, _ context: FunctionPassContext) -> [Instruction] {
  var liverange = InstructionRange(for: allocBox, context)
  defer { liverange.deinitialize() }

  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(allocBox)

  var destroys = Stack<Instruction>(context)
  defer { destroys.deinitialize() }

  while let value = worklist.pop() {
    for use in value.uses {
      let user = use.instruction
      liverange.insert(user)
      switch user {
      case is MarkUninitializedInst, is CopyValueInst, is MoveValueInst, is PartialApplyInst, is BeginBorrowInst:
        worklist.pushIfNotVisited(user as! SingleValueInstruction)
      case is StrongReleaseInst, is DestroyValueInst, is DeallocBoxInst:
        destroys.push(user)
      case let apply as FullApplySite:
        if apply.convention(of: use) == .directOwned {
          destroys.push(user)
        }
      default:
        break
      }
    }
  }
  return destroys.filter { !liverange.contains($0) }
}

/// Hoists `mark_unresolved_non_copyable_value` instructions from inside the def-use chain of `stackAddress`
/// right after the `stackAddress`.
/// ```
///    %1 = alloc_stack $T                                %1 = alloc_stack $T
///    %2 = begin_access [read] %1                  -->   %2 = mark_unresolved_non_copyable_value %1
///    %3 = mark_unresolved_non_copyable_value %2         %3 = begin_access [read] %2
/// ```
private func hoistMarkUnresolvedInsts(stackAddress: Value,
                                      checkKind: MarkUnresolvedNonCopyableValueInst.CheckKind,
                                      _ context: FunctionPassContext
) {
  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(stackAddress)
  var foundMarkUninit = false
  while let addr = worklist.pop() {
    for use in addr.uses {
      switch use.instruction {
      case is BeginAccessInst, is MarkUninitializedInst:
        worklist.pushIfNotVisited(use.instruction as! SingleValueInstruction)
      case let mu as MarkUnresolvedNonCopyableValueInst:
        mu.replace(with: mu.operand.value, context)
        foundMarkUninit = true
      default:
        break
      }
    }
  }
  guard foundMarkUninit else {
    return
  }
  let builder: Builder
  if let inst = stackAddress as? SingleValueInstruction {
    builder = Builder(after: inst, context)
  } else {
    builder = Builder(atBeginOf: stackAddress.parentBlock, context)
  }
  let mu = builder.createMarkUnresolvedNonCopyableValue(value: stackAddress, checkKind: checkKind,  isStrict: false)
  stackAddress.uses.ignore(user: mu).ignoreDebugUses.ignoreUsers(ofType: DeallocStackInst.self)
    .replaceAll(with: mu, context)
}

private extension ApplySite {
  func getSpecializableCallee() -> Function? {
    if let callee = referencedFunction,
       callee.isDefinition,
       callee.canBeInlinedIntoCaller(withSerializedKind: parentFunction.serializedKind)
    {
      if self is FullApplySite,
         // If the function is inlined later, there is no point in specializing it.
         !callee.shouldOptimize || callee.inlineStrategy == .always
      {
        return nil
      }
      return callee
    }
    return nil
  }
}
