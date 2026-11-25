//===--- FunctionSignatureOptimization.swift -------------------------------==//
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

/// Optimizes the signatures of OSSA functions to reduce ownership overhead at call sites.
///
/// For each call to an optimizable callee (see `FullApplySite.optimizableCallee`), the pass checks if the
/// callee's signature can be changed in a way which is beneficial for that call site:
///
/// * Argument specializations (see `getSpecializationKind`):
///   - `@owned` -> `@guaranteed`, if the callee just destroys the argument
///   - `@guaranteed` -> `@owned`, if the callee just copies the argument
///   - removing dead arguments
///   - exploding a partially used struct argument into its fields
/// * Converting the single direct `@owned` result to `@guaranteed`:
///   - if the result is borrowed from the arguments (see `getResultBorrowSources`)
///   - if the result is forwarded from an `@owned` argument (see `getForwardedOwnedArgument`)
///
/// In both cases the body of the original function is moved to a new specialized function, and the
/// original function becomes a thunk which calls the specialized function. This keeps all other
/// references to the original function valid.
/// For argument specializations, the thunk is then inlined at the call site, so that the caller
/// calls the specialized function directly and the ownership operations in the thunk can be cleaned
/// up together with those of the caller (e.g. a `copy_value` in the caller and a `destroy_value` in the
/// inlined thunk) by subsequent optimizations.
/// For result conversions, the call site is directly replaced by an apply of the specialized function.
///
let functionSignatureOptimization = ModulePass(name: "function-signature-optimization") {
  (moduleContext: ModulePassContext) in

  // Caches for the analysis results of callees.
  // Besides saving compile time, the caches are needed for correctness: once a callee is specialized,
  // its body is replaced by a thunk. Analyzing the thunk would give different results than analyzing
  // the original function, which is what the mangled name of the existing specialization is derived from.
  var functionSpecializations = Dictionary<Function, [ArgumentSpecialization]>()
  var resultBorrowSources = Dictionary<Function, [Int]>()
  var forwardedOwnedArguments = Dictionary<Function, Int?>()

  for function in moduleContext.functions {
    guard function.hasOwnership,
          function.shouldOptimize
    else {
      continue
    }

    // Repeat until nothing changes, because an optimization can expose new opportunities, e.g.
    // after inlining a thunk the caller contains an apply of the specialized function, whose
    // signature can be optimized further (like removing fields of an exploded struct, which are dead).
    var changed: Bool
    repeat {
      changed = false
      for inst in function.instructions {
        switch inst {
        case let apply as ApplyInst:
          if trySpecialize(apply: apply, cacheIn: &functionSpecializations, moduleContext) ||
             tryConvertResultToGuaranteed(apply: apply, cacheIn: &resultBorrowSources, moduleContext) ||
             tryConvertForwardedResultToGuaranteed(apply: apply, cacheIn: &forwardedOwnedArguments, moduleContext) {
            changed = true
          }
        case let tryApply as TryApplyInst:
          if trySpecialize(apply: tryApply, cacheIn: &functionSpecializations, moduleContext) {
            changed = true
          }
        default:
          break
        }
      }
    } while changed
  }
}

/// Specializes the callee of `apply` with argument specializations (see `getParameterSpecializations`)
/// if at least one of them is beneficial for `apply` (see `FullApplySite.canBenefit`).
/// The callee becomes a thunk of the specialized function, which is then inlined into the caller.
/// Returns true if `apply` was replaced.
private func trySpecialize(apply: FullApplySite,
                           cacheIn functionSpecializations: inout Dictionary<Function, [ArgumentSpecialization]>,
                           _ moduleContext: ModulePassContext) -> Bool {
  guard let callee = apply.optimizableCallee else {
    return false
  }

  let specializations: [ArgumentSpecialization]
  if let existingSpecializations = functionSpecializations[callee] {
    specializations = existingSpecializations
  } else {
    specializations = moduleContext.transform(function: callee) { context in
      getParameterSpecializations(for: callee, context)
    }
    functionSpecializations[callee] = specializations
  }

  let benefit = moduleContext.transform(function: apply.parentFunction) { context in
    apply.canBenefit(from: specializations, context)
  }
  guard benefit else {
    return false
  }

  // Note that all specializations are applied, even those which are not beneficial for this
  // call site. This avoids creating multiple specializations of the same function.
  specialize(function: callee, with: specializations, callerSiteApply: apply, moduleContext)

  // Inlining the thunk lets the caller directly call the specialized function. It also exposes the
  // ownership instructions of the thunk to the caller's optimizations.
  moduleContext.transform(function: apply.parentFunction) { context in
    _ = context.inlineFunction(apply: apply, mandatoryInline: false)
  }
  return true
}

private extension FullApplySite {
  /// The callee if it's a function whose signature can be optimized.
  var optimizableCallee: Function? {
    guard let callee = referencedFunction,
          callee.hasOwnership,
          callee.shouldOptimize,
          callee.isDefinition,
          // The specialized function is an additional function, unless the original function
          // can be removed. Also, it has shared linkage which prevents the LLVM MachineOutliner
          // from outlining code from it.
          !callee.isOptimizedForSize,
          // Specializing a function which is defined in another module creates a copy of it.
          // This is not worth it if it's called from a thunk, e.g. a witness thunk which
          // just forwards to a generic default implementation in the stdlib.
          !(callee.isDefinedExternally && parentFunction.thunkKind != .noThunk),
          // There is nothing to gain for a function which never returns.
          callee.blocks.contains(where: { $0.terminator.isFunctionExiting }),
          // Changing the conventions or removing arguments would invalidate the lifetime
          // dependencies, which refer to argument indices.
          !callee.convention.hasLifetimeDependencies()
    else {
      return nil
    }
    return callee
  }
}

/// Returns the argument specializations which can be applied to `function`, independent of a call site.
/// The specializations are sorted by argument index.
private func getParameterSpecializations(for function: Function,
                                         _ context: FunctionPassContext
) -> [ArgumentSpecialization] {
  var specializations = [ArgumentSpecialization]()
  for (argIndex, arg) in function.arguments.enumerated() {
    guard let paramIndex = function.argumentConventions.parameterIndex(ofArgumentIndex: argIndex) else {
      continue
    }
    // Skip the implicit isolation parameter of `nonisolated(nonsending)` functions because
    // removing it would also change isolation.
    if function.convention.parameters[paramIndex].hasOption(.implicitLeading) {
      continue
    }
    if let specKind = getSpecializationKind(for: arg, context) {
      specializations.append(ArgumentSpecialization(argumentIndex: argIndex, kind: specKind))
    }
  }
  return specializations
}

private func getSpecializationKind(for argument: FunctionArgument,
                                   _ context: FunctionPassContext
) -> ArgumentSpecialization.Kind? {
  // A thin metatype has no runtime representation. There is nothing to gain from removing it.
  if argument.type.isMetatype, argument.type.representationOfMetatype == .thin {
    return nil
  }
  if argument.uses.ignoreDebugUses.isEmpty {
    // Don't remove a dead generic metatype argument. IRGen uses it as a source for the type
    // metadata (including its generic arguments) which the function may need. Without it,
    // the metadata must be instantiated with a runtime call.
    if argument.type.isMetatype, argument.type.hasPrimaryArchetype {
      return nil
    }
    return .dead
  }

  switch argument.convention {
  case .directOwned:
    if !argument.type.isMoveOnly,
       argument.isDestroyedAtFunctionExits(context)
    {
      return .ownedToGuaranteed
    }
  case .directGuaranteed:
    if argument.isCopiedAtFunctionEntry(context) {
      return .guaranteedToOwned
    }
    // TODO: should we also explode partially dead trivial structs?
    if argument.isPartiallyUsed(context),
       // The `@guaranteed` result of a function is borrowed from "self" (see `specialize`).
       !(argument.parentFunction.convention.hasGuaranteedResult && argument.isSelf)
    {
      return .explode
    }
  default:
    break
  }
  return nil
}

private extension FunctionArgument {
  /// Returns true if the `@owned` argument is only destroyed, and there are no deinit barriers between
  /// the destroys and the function exits. In this case the argument can be passed as `@guaranteed` and
  /// the caller can destroy it after the call instead.
  func isDestroyedAtFunctionExits(_ context: FunctionPassContext) -> Bool {
    precondition(ownership == .owned)

    guard uses.endingLifetime.allSatisfy({ $0.instruction is DestroyValueInst }) else {
      return false
    }

    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushIfNotVisited(contentsOf: uses.endingLifetime.users)

    let calleeAnalysis = context.calleeAnalysis

    while let inst = worklist.pop() {
      if inst.isDeinitBarrier(calleeAnalysis) {
        return false
      }
      worklist.pushSuccessors(of: inst)
    }
    return true
  }

  /// Returns true if the `@guaranteed` argument is only used by `copy_value`s and each path from the
  /// function entry executes exactly one of the copies. In this case the argument can be passed as
  /// `@owned` and the copies can be replaced by the argument itself. The copy is then done in the
  /// caller, where it can be combined with a destroy of the passed value.
  func isCopiedAtFunctionEntry(_ context: FunctionPassContext) -> Bool {
    precondition(ownership == .guaranteed)

    guard uses.ignoreDebugUses.allSatisfy({ $0.instruction is CopyValueInst }) else {
      return false
    }

    var entryToCopies = BasicBlockRange(begin: parentFunction.entryBlock, context)
    defer { entryToCopies.deinitialize() }

    for copy in uses.users(ofType: CopyValueInst.self) {
      // Bail if there are multiple copies in a block or if a copy is on the path to another copy.
      if entryToCopies.inclusiveRangeContains(copy.parentBlock) {
        return false
      }
      entryToCopies.insert(copy.parentBlock)
    }

    // All paths from the function entry must end in a copy.
    guard entryToCopies.exits.isEmpty else {
      return false
    }

    // No copy must be within the range, i.e. on a path to another copy (also handles
    // copies in loops and copies which were inserted before a copy on their path).
    guard uses.users(ofType: CopyValueInst.self).allSatisfy({ !entryToCopies.contains($0.parentBlock) }) else {
      return false
    }

    return true
  }

  /// Returns true if the struct argument is only used by `struct_extract`s (looking through single-field
  /// structs), at most two fields are used, and at least one unused field is non-trivial.
  /// In this case it's beneficial to explode the struct into its fields, because the unused non-trivial
  /// fields don't need to be passed to the function.
  ///
  /// Note that exploding passes _all_ fields as separate arguments. The unused fields then become
  /// dead arguments of the specialized function, which are removed by a subsequent specialization.
  func isPartiallyUsed(_ context: FunctionPassContext) -> Bool {
    var usedFields = Stack<Int>(context)
    defer { usedFields.deinitialize() }
    var numUsedFields = 0

    var worklist = OperandWorklist(context)
    defer { worklist.deinitialize() }

    // The (multi-field) struct type which is exploded. Single-field wrapper structs around it
    // are looked through.
    var structType: Type? = nil

    worklist.pushIfNotVisited(contentsOf: uses)

    while let use = worklist.pop() {
      switch use.instruction {
      case let structExtract as StructExtractInst:
        guard let fields = use.value.type.getNominalFields(in: parentFunction) else {
          return false
        }
        if fields.count == 1 {
          worklist.pushIfNotVisited(contentsOf: structExtract.uses)
        } else {
          if let structType = structType {
            assert(structType == use.value.type)
          } else {
            structType = use.value.type
          }
          if !usedFields.contains(structExtract.fieldIndex) {
            usedFields.append(structExtract.fieldIndex)
            numUsedFields += 1
          }
        }
      case is DebugValueInst:
        break
      default:
        return false
      }
    }
    guard let structType else {
      return false
    }
    if numUsedFields > 2 {
      return false
    }
    for (fieldIdx, field) in structType.getNominalFields(in: parentFunction)!.enumerated() {
      // TODO: should we also explode partially dead trivial structs?
      if !usedFields.contains(fieldIdx), !field.isTrivial(in: parentFunction) {
        return true
      }
    }
    return false
  }
}

//===----------------------------------------------------------------------===//
//                    Owned -> guaranteed result conversion
//===----------------------------------------------------------------------===//

/// Replaces `apply` with an apply of a specialized callee which returns its single direct result as
/// `@guaranteed` instead of `@owned`. This is done if the returned value is borrowed from the callee's
/// arguments (see `getResultBorrowSources`) and the caller doesn't need to own the result.
private func tryConvertResultToGuaranteed(apply: ApplyInst,
                                          cacheIn resultBorrowSources: inout Dictionary<Function, [Int]>,
                                          _ moduleContext: ModulePassContext) -> Bool {
  guard let callee = apply.optimizableCallee else {
    return false
  }

  let borrowSources: [Int]
  if let existingBorrowSources = resultBorrowSources[callee] {
    borrowSources = existingBorrowSources
  } else {
    borrowSources = moduleContext.transform(function: callee) { context in
      return getResultBorrowSources(of: callee, context)
    }
    resultBorrowSources[callee] = borrowSources
  }
  if borrowSources.isEmpty {
    return false
  }

  let benefit = moduleContext.transform(function: apply.parentFunction) { context in
    apply.canBenefitFromGuaranteedResult(borrowedFrom: borrowSources, context)
  }
  guard benefit else {
    return false
  }

  let specializedFunction = specializeWithGuaranteedResult(function: callee, borrowedFrom: borrowSources,
                                                           callerSiteApply: apply, moduleContext)
  moduleContext.transform(function: apply.parentFunction) { context in
    apply.replace(withGuaranteedResultApplyOf: specializedFunction, arguments: Array(apply.arguments), context)
  }
  return true
}

/// Replaces `apply` with an apply of a specialized callee which takes the `@owned` argument, from which
/// the result is forwarded (see `getForwardedOwnedArgument`), as `@guaranteed` and returns its single direct
/// result as `@guaranteed`. This is done if the caller passes a copy of a guaranteed value to the
/// argument and doesn't need to own the result. The copy in the caller is removed.
private func tryConvertForwardedResultToGuaranteed(apply: ApplyInst,
                                                   cacheIn forwardedOwnedArguments: inout Dictionary<Function, Int?>,
                                                   _ moduleContext: ModulePassContext) -> Bool {
  guard let callee = apply.optimizableCallee else {
    return false
  }

  let forwardedOwnedArgument: Int?
  // Note that the dictionary's value is optional. Therefore this also hits if the cached result is nil.
  if let existingForwardedOwnedArgument = forwardedOwnedArguments[callee] {
    forwardedOwnedArgument = existingForwardedOwnedArgument
  } else {
    forwardedOwnedArgument = getForwardedOwnedArgument(of: callee)
    forwardedOwnedArguments[callee] = forwardedOwnedArgument
  }
  guard let argumentIndex = forwardedOwnedArgument else {
    return false
  }

  let benefit = moduleContext.transform(function: apply.parentFunction) { context in
    apply.canBenefitFromGuaranteedResult(forwardedFrom: argumentIndex, context)
  }
  guard benefit else {
    return false
  }

  let specializedFunction = specializeWithGuaranteedResult(function: callee, forwardedFrom: argumentIndex,
                                                           callerSiteApply: apply, moduleContext)
  moduleContext.transform(function: apply.parentFunction) { context in
    // Instead of the copy, pass the copied guaranteed value (see `canBenefitFromGuaranteedResult`).
    let copy = apply.arguments[argumentIndex] as! CopyValueInst
    var arguments = Array(apply.arguments)
    arguments[argumentIndex] = copy.fromValue
    apply.replace(withGuaranteedResultApplyOf: specializedFunction, arguments: arguments, context)
    context.erase(instruction: copy)
  }
  return true
}

private extension ApplyInst {
  /// Replaces this apply with an apply of `specializedFunction`, which returns the result as `@guaranteed`.
  /// The `@guaranteed` result of the new apply is a borrow introducer. Therefore the destroys of the
  /// original result are replaced by `end_borrow`s of the new apply.
  func replace(withGuaranteedResultApplyOf specializedFunction: Function, arguments: [Value],
               _ context: FunctionPassContext) {
    let newApply = replace(withCallTo: specializedFunction, arguments: arguments, context) as! ApplyInst
    for destroy in newApply.uses.users(ofType: DestroyValueInst.self) {
      Builder(before: destroy, context).createEndBorrow(of: newApply)
      context.erase(instruction: destroy)
    }
  }
}

/// Returns the index of the `@owned` argument which is forwarded to the single direct `@owned` result
/// of `function`, or nil if there is no such argument.
///
/// If a caller passes a copy of a guaranteed value to such an argument, both, the argument and the
/// result can be converted to `@guaranteed`. This removes the copy in the caller and turns the
/// destroys of the result into end-of-borrow-scopes:
/// ```
///   sil @f : $(@owned Array<Int>) -> @owned AnyObject {
///   bb0(%0 : @owned $Array<Int>):
///     %1 = destructure_struct %0
///     ...
///     %4 = init_existential_ref %3
///     return %4
/// ```
/// ->
/// ```
///   sil @f : $(@guaranteed Array<Int>) -> @guaranteed AnyObject {
///   bb0(%0 : @guaranteed $Array<Int>):
///     %1 = destructure_struct %0     // forwards a guaranteed value
///     ...
///     %4 = init_existential_ref %3
///     return_borrow %4 from_scopes ()
/// ```
private func getForwardedOwnedArgument(of function: Function) -> Int? {
  guard let returnInst = function.returnInstruction as? ReturnInst,
        returnInst.returnedValue.ownership == .owned
  else {
    return nil
  }
  var value = returnInst.returnedValue
  while true {
    // The forwarding chain must be the only use which ends the value's lifetime. Otherwise, e.g. a
    // `destroy_value` in a dead-end block would end up destroying a guaranteed value.
    guard value.uses.endingLifetime.singleElement != nil else {
      return nil
    }
    if let argument = value as? FunctionArgument {
      guard argument.convention == .directOwned,
            !argument.type.isMoveOnly
      else {
        return nil
      }
      return argument.index
    }
    guard let fwdInst = value.definingInstruction as? ForwardingInstruction,
          fwdInst.canForwardGuaranteedValues,
          // All other results and operands must be trivial, because they are not converted to
          // guaranteed values.
          fwdInst.results.allSatisfy({ $0 == value || $0.ownership == .none }),
          let forwardedOperand = fwdInst.forwardedOperands.lazy.filter({ $0.value.ownership != .none }).singleElement,
          forwardedOperand.value.ownership == .owned,
          fwdInst.operands.allSatisfy({ $0 == forwardedOperand || $0.value.ownership == .none })
    else {
      return nil
    }
    value = forwardedOperand.value
  }
}

/// Returns the indices of the arguments from whose memory - or value - the single direct `@owned`
/// result of `function` is borrowed. If the result is empty, the result cannot be returned
/// `@guaranteed`.
///
/// The result can be returned `@guaranteed` - i.e. without the callee retaining it - if the returned
/// value is, on all paths, borrowed from something which the caller keeps alive anyway, e.g. loaded
/// from an indirect argument:
/// ```
///   sil @f : $(@inout Array<Int>) -> @owned Array<Int> {
///   bb0(%0 : $*Array<Int>):
///     %1 = load [copy] %0
///     return %1
/// ```
/// ->
/// ```
///   sil @f : $(@inout Array<Int>) -> @guaranteed Array<Int> {
///   bb0(%0 : $*Array<Int>):
///     %1 = load_borrow %0
///     return_borrow %1 from_scopes (%1)
/// ```
/// The returned argument indices are the contract for callers: the borrow is only valid as long as
/// the memory of those arguments is not modified (see `argumentScopesOverlapReturnLifetime`).
private func getResultBorrowSources(of function: Function, _ context: FunctionPassContext) -> [Int] {
  guard let returnInst = function.returnInstruction as? ReturnInst,
        returnInst.returnedValue.ownership == .owned
  else {
    return []
  }

  var borrowingInstructions = Stack<SingleValueInstruction>(context)
  defer { borrowingInstructions.deinitialize() }

  guard collectBorrowingInstructions(of: returnInst.returnedValue, in: &borrowingInstructions, context) else {
    return []
  }

  var borrowedArguments = IterableArgumentSet(context)
  defer { borrowedArguments.deinitialize() }

  guard collectBorrowedArguments(of: borrowingInstructions, in: &borrowedArguments, context),
        !mayModifyMemory(after: borrowingInstructions, borrowedArguments: borrowedArguments, context) else {
    return []
  }

  return borrowedArguments.map { $0.index }
}

/// Collects the instructions which produce the owned `returnedValue`, looking through phis.
/// Each of those instructions can be converted to produce a borrowed value instead:
/// * `load [copy]` -> `load_borrow`
/// * `copy_value` -> `begin_borrow`
/// * a self-recursive apply -> an apply of the specialized function, which returns a `@guaranteed` result
/// Returns false if the returned value has another origin.
private func collectBorrowingInstructions(of returnedValue: Value,
                                          in results: inout Stack<SingleValueInstruction>,
                                          _ context: FunctionPassContext) -> Bool
{
  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushIfNotVisited(returnedValue)
  while let value = worklist.pop() {
    // The `return` or the branch to the phi must be the only use which ends the value's lifetime,
    // because all uses of the value are replaced by the borrowed value. Otherwise, e.g. a `destroy_value`
    // in a dead-end block would end up destroying a guaranteed value.
    guard value.uses.endingLifetime.singleElement != nil else {
      return false
    }
    switch value {
    case let load as LoadInst where load.loadOwnership == .copy:
      results.append(load)

    case let copy as CopyValueInst:
      results.append(copy)

    case let apply as ApplyInst where apply.referencedFunction == apply.parentFunction:
      // The recursive call will call the specialized function with the `@guaranteed` result. Its
      // direct `@guaranteed` arguments are forwarded to the result and must therefore not be owned.
      if apply.hasOwnedDirectGuaranteedArgument {
        return false
      }
      results.append(apply)

    case let argument as Argument:
      guard let phi = Phi(argument) else {
        return false
      }
      worklist.pushIfNotVisited(contentsOf: phi.incomingValues)

    default:
      return false
    }
  }
  return true
}

/// Collects the function arguments from which the values of `borrowingInstructions` are borrowed:
/// * for a `load [copy]`: the base argument of the loaded address
/// * for a `copy_value`: the borrow introducers of the copied value, which must be function arguments
/// * for a self-recursive apply: the caller's arguments which are passed to the borrowed arguments of
///   the recursive call. This is computed iteratively until a fixed point is reached, because the set
///   of borrowed arguments grows while processing the recursive applies.
/// Returns false if a borrowed value doesn't originate from a function argument.
private func collectBorrowedArguments(of borrowingInstructions: Stack<SingleValueInstruction>,
                                      in borrowedArguments: inout IterableArgumentSet,
                                      _ context: FunctionPassContext) -> Bool
{
  for borrowingInst in borrowingInstructions {
    switch borrowingInst {
    case let load as LoadInst:
      guard let argument = load.address.baseArgumentOfAddress else {
        return false
      }
      borrowedArguments.insert(argument)

    case let copy as CopyValueInst:
      if borrowedArguments.insertBorrowIntroducers(of: copy.fromValue, context) == .unknownBorrowIntroducer {
        return false
      }

    default:
      break
    }
  }

  var argumentAdded: Bool

  repeat {
    argumentAdded = false
    for case let apply as ApplyInst in borrowingInstructions {
      // Inserting into `borrowedArguments` while iterating over it is fine: the iteration doesn't
      // visit newly inserted arguments, but they are handled in the next iteration of the outer loop.
      for argument in borrowedArguments {
        let argumentValue = apply.operand(forCalleeArgumentIndex: argument.index)!.value
        if argumentValue.type.isAddress {
          guard let callerArgument = argumentValue.baseArgumentOfAddress else {
            return false
          }
          if borrowedArguments.insert(callerArgument) {
            argumentAdded = true
          }
        } else {
          switch borrowedArguments.insertBorrowIntroducers(of: argumentValue, context) {
          case .unknownBorrowIntroducer: return false
          case .insertedNewArgument:     argumentAdded = true
          case .nothingInserted:         break
          }
        }
      }
    }
  } while argumentAdded

  return true
}

private extension IterableArgumentSet {
  enum InsertResult {
    case nothingInserted, insertedNewArgument, unknownBorrowIntroducer
  }

  /// Inserts the borrow introducers of the guaranteed `value`, which must all be function arguments.
  mutating func insertBorrowIntroducers(of value: Value, _ context: FunctionPassContext) -> InsertResult {
    guard value.ownership == .guaranteed else {
      return .unknownBorrowIntroducer
    }
    var result = InsertResult.nothingInserted
    for introducer in value.getBorrowIntroducers(context) {
      guard let argument = introducer.value as? FunctionArgument else {
        return .unknownBorrowIntroducer
      }
      if insert(argument) {
        result = .insertedNewArgument
      }
    }
    return result
  }
}

/// Returns true if the borrowed memory may be modified within the callee after the borrowing instructions.
/// After converting the result to `@guaranteed`, the borrow scopes of the borrowing instructions extend
/// to the function return. Modifications of the memory in the caller - after the call - are checked by
/// `argumentScopesOverlapReturnLifetime`.
private func mayModifyMemory(after borrowingInstructions: Stack<SingleValueInstruction>,
                             borrowedArguments: IterableArgumentSet,
                             _ context: FunctionPassContext) -> Bool
{
  for borrowingInst in borrowingInstructions {
    switch borrowingInst {
    case let load as LoadInst:
      if mayModifyMemory(load.address, after: load, until: nil, context) {
        return true
      }

    case let apply as ApplyInst:
      // The result of the recursive call is borrowed from the memory of its arguments.
      for argument in borrowedArguments {
        let argumentValue = apply.operand(forCalleeArgumentIndex: argument.index)!.value
        if argumentValue.type.isAddress,
           mayModifyMemory(argumentValue, after: apply, until: nil, context)
        {
          return true
        }
      }

    default:
      break
    }
  }
  return false
}

private extension ApplyInst {
  /// Returns true if this call site can use a `@guaranteed` result which is borrowed from the
  /// arguments at `argumentIndices` (see `getResultBorrowSources`).
  func canBenefitFromGuaranteedResult(borrowedFrom argumentIndices: [Int], _ context: FunctionPassContext) -> Bool {
    return canConvertResultFromOwnedToGuaranteed &&
           argumentScopesOverlapReturnLifetime(of: argumentIndices.map { arguments[$0] }, context) &&
           isResultOnlyDestroyed
  }

  /// Returns true if this call site can use a `@guaranteed` result which is forwarded from the
  /// argument at `argumentIndex` (see `getForwardedOwnedArgument`).
  func canBenefitFromGuaranteedResult(forwardedFrom argumentIndex: Int, _ context: FunctionPassContext) -> Bool {
    // The copy is removed and the copied value is passed to the `@guaranteed` argument instead.
    // TODO: support owned copied values by inserting a borrow scope around the result's lifetime.
    guard let copy = arguments[argumentIndex] as? CopyValueInst,
          copy.uses.singleElement != nil,
          copy.fromValue.ownership == .guaranteed
    else {
      return false
    }
    return canConvertResultFromOwnedToGuaranteed &&
           argumentScopesOverlapReturnLifetime(of: [copy.fromValue], context) &&
           isResultOnlyDestroyed
  }

  /// A `@guaranteed` result only helps if the caller doesn't have to own the returned value
  /// anyway. If it does, the removed retain in the callee is just moved to the caller.
  var isResultOnlyDestroyed: Bool {
    !uses.endingLifetime.isEmpty && uses.endingLifetime.allSatisfy({ $0.instruction is DestroyValueInst })
  }

  /// Returns true if all uses of the result - except destroys, which are replaced by `end_borrow`s -
  /// can accept a guaranteed value.
  var canConvertResultFromOwnedToGuaranteed: Bool {
    !hasOwnedDirectGuaranteedArgument &&
      uses.ignore(usersOfType: DestroyValueInst.self).allSatisfy { $0.canAccept(ownership: .guaranteed) }
  }

  /// An apply with a `@guaranteed` result forwards its direct `@guaranteed` arguments to the result.
  /// Therefore such arguments must be guaranteed values. Owned values would need a borrow scope.
  // TODO: support owned arguments by inserting a borrow scope around the result's lifetime.
  var hasOwnedDirectGuaranteedArgument: Bool {
    argumentOperands.contains { convention(of: $0) == .directGuaranteed && $0.value.ownership == .owned }
  }

  /// Returns true if `argumentValues`, from which the `@guaranteed` result is borrowed, stay valid during
  /// the whole lifetime of the result in the caller:
  /// * memory of address arguments is not modified
  /// * owned values are not destroyed
  /// * the borrow scopes of guaranteed values are not ended
  func argumentScopesOverlapReturnLifetime(of argumentValues: [Value], _ context: FunctionPassContext) -> Bool {
    var returnLifetime = InstructionRange(begin: self, ends: uses.endingLifetime.users, context)
    defer { returnLifetime.deinitialize() }

    for arg in argumentValues {
      if arg.type.isAddress {
        if mayModifyMemory(arg, after: self, until: returnLifetime.insertedInstructions, context) {
          return false
        }
      } else {
        switch arg.ownership {
        case .unowned:
          return false
        case .none:
          break
        case .owned:
          if arg.uses.endingLifetime.users.contains(where: { returnLifetime.contains($0) }) {
            return false
          }
        case .guaranteed:
          for borrowIntroducer in arg.getBorrowIntroducers(context) {
            if borrowIntroducer.scopeEndingOperands.users.contains(where: { returnLifetime.contains($0) }) {
              return false
            }
          }
        }
      }
    }
    return true
  }
}

/// Returns true if any instruction from `startInst` (exclusive) to `ends` (exclusive) may write to
/// `address`. If `ends` is nil, all paths to the function exits are checked.
private func mayModifyMemory(_ address: Value, after startInst: Instruction,
                             until ends: InstructionSet?,
                             _ context: FunctionPassContext) -> Bool {
  let aliasAnalysis = context.aliasAnalysis
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushSuccessors(of: startInst)

  while let inst = worklist.pop() {
    if let ends, ends.contains(inst) {
      continue
    }
    if inst.mayWrite(toAddress: address, aliasAnalysis) {
      return true
    }
    worklist.pushSuccessors(of: inst)
  }
  return false
}

private extension FullApplySite {
  /// Returns true if at least one of the argument specializations is beneficial for this call site.
  func canBenefit(from parameterSpecializations: [ArgumentSpecialization], _ context: FunctionPassContext) -> Bool {
    for spec in parameterSpecializations {
      if let arg = operand(forCalleeArgumentIndex: spec.argumentIndex),
         arg.canBenefit(from: spec.kind, context)
      {
        return true
      }
    }
    return false
  }
}

private extension Operand {
  /// Returns true if the specialization of the argument is beneficial for this argument operand of an apply.
  func canBenefit(from specializationKind: ArgumentSpecialization.Kind, _ context: FunctionPassContext) -> Bool {
    switch specializationKind {
    case .ownedToGuaranteed:
      // After inlining the thunk, the copy is followed by a destroy after the call. Both can be removed.
      switch self.value {
      case is CopyValueInst:
        return true
      default:
        // TODO: check load_borrow
        return false
      }
    case .guaranteedToOwned:
      // After inlining the thunk, the copy which is passed to the call can be combined with the
      // destroy after the call, i.e. the value can be moved into the call.
      return self.value.isDestroyed(after: self.instruction, context)
    case .dead:
      return true
    case .explode:
      switch self.value {
      case is CopyValueInst:
        return true
      default:
        return false
      }
    }
  }
}

private extension Value {
  /// Returns true if this value is only destroyed after `beginInstruction`, without any other uses
  /// or deinit barriers between `beginInstruction` and the destroys.
  func isDestroyed(after beginInstruction: Instruction, _ context: FunctionPassContext) -> Bool {
    guard !uses.users(ofType: DestroyValueInst.self).isEmpty,
          uses.endingLifetime.allSatisfy({ $0.instruction is DestroyValueInst })
    else {
      return false
    }

    var userSet = InstructionSet(context)
    defer { userSet.deinitialize() }
    userSet.insert(contentsOf: uses.filter{ !$0.endsLifetime }.users)

    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushIfNotVisited(contentsOf: uses.endingLifetime.users)

    let calleeAnalysis = context.calleeAnalysis

    while let inst = worklist.pop() {
      if inst.isDeinitBarrier(calleeAnalysis) {
        return false
      }
      if userSet.contains(inst) {
        return false
      }
      worklist.pushPredecessors(of: inst, ignoring: beginInstruction)
    }
    return true
  }

  /// The function argument which is the base of this address, or nil if the address is not
  /// derived from a function argument or if it's within an access scope.
  var baseArgumentOfAddress: Argument? {
    switch enclosingAccessScope {
    case .access, .dependence:
      // A `load_borrow` must not be used after the end of its enclosing access scope, but the
      // returned borrow escapes to the caller.
      return nil
    case .base(let accessBase):
      guard case .argument(let argument) = accessBase else {
        return nil
      }
      return argument
    }
  }

}

/// Creates a specialized version of `function` with the `argumentSpecializations` applied and turns
/// `function` into a thunk which calls the specialized function.
/// Does nothing if the specialized function already exists, in which case `function` is already a thunk.
///
/// This is done in three steps:
/// 1. Create the declaration of the specialized function with the changed parameters.
/// 2. Move the body of `function` to the specialized function and create the thunk in `function`.
/// 3. Adapt the moved body in the specialized function to the changed parameters.
private func specialize(function: Function,
                        with argumentSpecializations: [ArgumentSpecialization],
                        callerSiteApply: FullApplySite,
                        _ moduleContext: ModulePassContext)
{
  let specializedFuncName = moduleContext.mangle(withSignatureSpecializedArguments: argumentSpecializations,
                                                 from: function)

  if moduleContext.lookupFunction(name: specializedFuncName) != nil {
    return
  }

  let specializedParams = computeSpecializedParameters(of: function, with: argumentSpecializations)

  // We are removing arguments from the original function. If the removed argument is the
  // "self" argument, the specialized function cannot be a "method" anymore. It's in general
  // safe to make it a "thin" function (even if "self" was not removed).
  // An exception is a function with a `@guaranteed` result: its result is borrowed from "self".
  // Therefore, if "self" is not removed, the specialized function must stay a "method".
  let representation = function.loweredFunctionType.functionTypeRepresentation
  let specializedRepresentation: FunctionTypeRepresentation
  if function.convention.hasGuaranteedResult,
     let selfArgIdx = function.selfArgumentIndex,
     !argumentSpecializations.contains(where: { $0.argumentIndex == selfArgIdx })
  {
    specializedRepresentation = .method
  } else {
    specializedRepresentation = representation == .method || representation == .witnessMethod ? .thin : representation
  }

  let convention = FunctionConvention(for: function.loweredFunctionType,
                                      hasLoweredAddresses: function.hasLoweredAddresses)

  // The specialized function only needs a generic signature if it still uses a generic parameter.
  // E.g. after removing a dead generic argument it can become non-generic.
  let isGeneric = specializedParams.contains { $0.type.hasTypeParameter } ||
                  convention.resultsWithError.contains { $0.type.hasTypeParameter } ||
                  convention.errorResult?.type.hasTypeParameter ?? false ||
                  (function.isGeneric && function.implicitlyUsesGenericParameter)

  let specializedFunction = moduleContext.createSpecializedFunctionDeclaration(
      from: function, withName: specializedFuncName,
      withParams: specializedParams,
      withRepresentation: specializedRepresentation,
      preserveGenericSignature: isGeneric)

  moduleContext.moveFunctionBody(from: function, to: specializedFunction)

  moduleContext.transform(function: function) { context in
    createThunk(in: function, calling: specializedFunction, with: argumentSpecializations,
                isGeneric: isGeneric, callerSiteApply: callerSiteApply, context)
  }

  moduleContext.buildSpecializedFunction(specializedFunction: specializedFunction) {
      (specializedFunction, specializedContext) in
    adaptSpecializedFunction(specializedFunction, of: function, to: argumentSpecializations,
                             isGeneric: isGeneric, specializedContext)
  }
  moduleContext.notifyNewFunction(function: specializedFunction, derivedFrom: function)
}

/// Returns the parameters of the specialized version of `function`, which are the original parameters
/// with the `argumentSpecializations` applied.
private func computeSpecializedParameters(of function: Function,
                                          with argumentSpecializations: [ArgumentSpecialization]
) -> [ParameterInfo] {
  var specializedParams = Array(function.convention.parameters)

  let argumentConventions = function.argumentConventions

  // The difference between parameter indices in `specializedParams` and in the original function,
  // caused by removed and inserted (exploded) parameters.
  var offset = 0

  for spec in argumentSpecializations {
    let origParamIdx = argumentConventions.parameterIndex(ofArgumentIndex: spec.argumentIndex)!
    let paramIdx = origParamIdx + offset

    switch spec.kind {
    case .ownedToGuaranteed:
      assert(specializedParams[paramIdx].convention == .directOwned)
      specializedParams[paramIdx] = specializedParams[paramIdx].with(convention: .directGuaranteed)
    case .guaranteedToOwned:
      assert(specializedParams[paramIdx].convention == .directGuaranteed)
      specializedParams[paramIdx] = specializedParams[paramIdx].with(convention: .directOwned)
    case .dead:
      specializedParams.remove(at: paramIdx)
      offset -= 1
    case .explode:
      // Replace the struct parameter with one parameter for each stored property.
      let toExplode = specializedParams.remove(at: paramIdx)
      for field in toExplode.type.nominal!.storedProperties {
        let fieldType = toExplode.type.getTypeOf(member: field).canonical
        let pi = ParameterInfo(type: fieldType,
                               convention: fieldType.isTrivial(in: function) ? .directUnowned : .directGuaranteed,
                               options: toExplode.options,
                               hasLoweredAddresses: toExplode.hasLoweredAddresses)
        specializedParams.insert(pi, at: origParamIdx + offset)
        offset += 1
      }
      offset -= 1
    }
  }
  return specializedParams
}

/// Creates the body of `thunk` (the original function, whose body was moved to `specializedFunction`):
/// it takes the original arguments, converts them to the arguments of the specialized function
/// according to `argumentSpecializations` and calls the specialized function.
private func createThunk(in thunk: Function, calling specializedFunction: Function,
                         with argumentSpecializations: [ArgumentSpecialization],
                         isGeneric: Bool,
                         callerSiteApply: FullApplySite,
                         _ context: FunctionPassContext)
{
  var newApplyArgs = createThunkEntryBlock(in: thunk, withArgumentsOf: specializedFunction, context)
  let newEntryBlock = thunk.entryBlock
  // Values which must be destroyed or whose borrow scope must be ended after the call.
  var toCleanup = [Value]()
  // The difference between argument indices in `newApplyArgs` and in the original function.
  var offset = 0
  for spec in argumentSpecializations {
    let argIdx = spec.argumentIndex + offset
    switch spec.kind {
    case .ownedToGuaranteed:
      // The thunk still owns the argument and destroys it after the call.
      toCleanup.append(newApplyArgs[argIdx])
    case .guaranteedToOwned:
      // The specialized function consumes the argument, so the thunk must pass a copy.
      let builder = Builder(atEndOf: newEntryBlock, location: thunk.location, context)
      let copy = builder.createCopyValue(operand: newApplyArgs[argIdx])
      newApplyArgs[argIdx] = copy
    case .dead:
      newApplyArgs.remove(at: argIdx)
      offset -= 1
    case .explode:
      // Pass the fields of the struct instead of the struct itself.
      let original = newApplyArgs.remove(at: argIdx)
      let builder = Builder(atEndOf: newEntryBlock, location: thunk.location, context)
      let borrow = builder.createBeginBorrow(of: original)
      let destructure = builder.createDestructureStruct(struct: borrow)
      toCleanup.append(borrow)
      for field in destructure.results {
        newApplyArgs.insert(field, at: spec.argumentIndex + offset)
        offset += 1
      }
      offset -= 1
    }
  }

  let newApplySite = createForwardingApply(
    to: specializedFunction, in: thunk,
    arguments: newApplyArgs,
    substitutionMap: isGeneric ? thunk.forwardingSubstitutionMap : SubstitutionMap(),
    template: callerSiteApply,
    location: thunk.location,
    context)

  Builder.insert(after: newApplySite, context) { builder in
    for v in toCleanup {
      if v is BeginBorrowInst {
        builder.createEndBorrow(of: v)
      } else {
        builder.createDestroyValue(operand: v)
      }
    }
  }
}

/// Adapts the body of `specializedFunction`, which was moved from `originalFunction`, to the
/// changed arguments according to `argumentSpecializations`.
private func adaptSpecializedFunction(_ specializedFunction: Function, of originalFunction: Function,
                                      to argumentSpecializations: [ArgumentSpecialization],
                                      isGeneric: Bool,
                                      _ context: FunctionPassContext)
{
  if !isGeneric {
    // A `debug_value undef` doesn't count as a use of a generic parameter (see
    // `implicitlyUsesGenericParameter`). Remove it, because its type is not valid
    // in the non-generic specialized function.
    context.erase(instructions: specializedFunction.instructions.lazy.filter {
      $0 is DebugValueInst && $0.operands.contains { $0.value.isUndefWithPrimaryArchetype }
    })
  }
  // The difference between argument indices in the specialized function and in the original function.
  var offset = 0
  for spec in argumentSpecializations {
    let arg = specializedFunction.arguments[spec.argumentIndex + offset]
    switch spec.kind {
    case .ownedToGuaranteed:
      // The destroys are done by the caller now (see `isDestroyedAtFunctionExits`).
      context.erase(instructions: arg.uses.users(ofType: DestroyValueInst.self))
      arg.set(ownership: .guaranteed, context)
    case .guaranteedToOwned:
      // The copies are done by the caller now (see `isCopiedAtFunctionEntry`).
      for copy in arg.uses.users(ofType: CopyValueInst.self) {
        copy.replace(with: arg, context)
      }
      arg.set(ownership: .owned, context)

      // The argument is consumed at the former copies. Therefore its `debug_value`s may end up
      // after its lifetime. Move them to the function entry, where the argument is alive.
      for dv in arg.uses.users(ofType: DebugValueInst.self) {
        let firstInst = specializedFunction.instructions.first!
        if dv != firstInst {
          dv.move(before: firstInst, context)
        }
      }
    case .dead:
      assert(arg.users.allSatisfy { $0 is DebugValueInst })
      context.erase(instructions: arg.users)
      specializedFunction.entryBlock.eraseArgument(at: arg.index, context)
      offset -= 1
    case .explode:
      // Insert the field arguments after the struct argument and replace the `struct_extract`s
      // with the corresponding field arguments. Then remove the struct argument.
      var elements = [Value]()
      for field in arg.type.getNominalFields(in: originalFunction)! {
        let fieldArg = specializedFunction.entryBlock.insertFunctionArgument(
          atPosition: spec.argumentIndex + offset + 1,
          type: field,
          ownership: field.isTrivial(in: specializedFunction) ? .none : .guaranteed,
          context)
        elements.append(fieldArg)
        offset += 1
      }
      for user in arg.users {
        switch user {
        case is DebugValueInst:
          context.erase(instruction: user)
        case let structExtract as StructExtractInst:
          structExtract.replace(with: elements[structExtract.fieldIndex], context)
        default:
          fatalError("unknown argument use")
        }
      }
      specializedFunction.entryBlock.eraseArgument(at: arg.index, context)
      offset -= 1
    }
  }
}

/// Creates a specialized version of `function` which returns its single direct result as `@guaranteed`.
/// The original `function` becomes a thunk which calls the specialized function and copies the result.
///
/// If `forwardedArgument` is not nil, the specialized function also takes this `@owned` argument as
/// `@guaranteed`. Its result is forwarded from this argument.
/// Otherwise the result is borrowed from the arguments at `borrowSources`.
private func specializeWithGuaranteedResult(function: Function,
                                            borrowedFrom borrowSources: [Int] = [],
                                            forwardedFrom forwardedArgument: Int? = nil,
                                            callerSiteApply: ApplyInst,
                                            _ moduleContext: ModulePassContext) -> Function
{
  var argumentSpecializations = [ArgumentSpecialization]()
  var specializedParams = Array(function.convention.parameters)
  if let forwardedArgument {
    argumentSpecializations.append(ArgumentSpecialization(argumentIndex: forwardedArgument, kind: .ownedToGuaranteed))
    let paramIdx = function.argumentConventions.parameterIndex(ofArgumentIndex: forwardedArgument)!
    specializedParams[paramIdx] = specializedParams[paramIdx].with(convention: .directGuaranteed)
  }

  let specializedFuncName = moduleContext.mangle(withSignatureSpecializedArguments: argumentSpecializations,
                                                 resultOwnedToGuaranteed: true,
                                                 from: function)

  if let existingFunction = moduleContext.lookupFunction(name: specializedFuncName) {
    return existingFunction
  }

  let specializedResults = function.convention.formalResults.map {
    ResultInfo(type: $0.type, convention: .guaranteed, options: $0.options,
               hasLoweredAddresses: $0.hasLoweredAddresses)
  }

  let specializedFunction = moduleContext.createSpecializedFunctionDeclaration(
      from: function, withName: specializedFuncName,
      withParams: specializedParams,
      withResults: specializedResults)

  moduleContext.moveFunctionBody(from: function, to: specializedFunction)

  moduleContext.transform(function: function) { context in
    createGuaranteedResultThunk(in: function, calling: specializedFunction, callerSiteApply: callerSiteApply,
                                borrowing: forwardedArgument, context)
  }

  moduleContext.buildSpecializedFunction(specializedFunction: specializedFunction) {
      (specializedFunction, specializedContext) in
    if let forwardedArgument {
      let returnInst = specializedFunction.returnInstruction as! ReturnInst
      convertForwardingChainToGuaranteed(from: specializedFunction.arguments[forwardedArgument],
                                         to: returnInst.returnedValue, specializedContext)

      // The borrow scope of the guaranteed argument doesn't end in the function.
      let builder = Builder(before: returnInst, specializedContext)
      builder.createReturnBorrow(of: returnInst.returnedValue, fromScopes: [])
      specializedContext.erase(instruction: returnInst)
    } else {
      convertResultToGuaranteed(in: specializedFunction, borrowedFrom: borrowSources, specializedContext)
    }
  }
  moduleContext.notifyNewFunction(function: specializedFunction, derivedFrom: function)
  return specializedFunction
}

/// Creates the body of `function` as a thunk which calls `specializedFunction`, which returns the
/// result as `@guaranteed`.
/// If `borrowedArgument` is not nil, `specializedFunction` takes this owned argument of `function` as
/// `@guaranteed` and its result is borrowed from it.
private func createGuaranteedResultThunk(in function: Function, calling specializedFunction: Function,
                                         callerSiteApply: ApplyInst,
                                         borrowing borrowedArgument: Int? = nil,
                                         _ context: FunctionPassContext) {
  var newApplyArgs = createThunkEntryBlock(in: function, withArgumentsOf: specializedFunction, context)
  let builder = Builder(atEndOf: function.entryBlock, location: function.location, context)

  // The returned value is borrowed from the argument. Therefore the owned argument must be
  // borrowed for the lifetime of the returned value.
  var argumentBorrow: BeginBorrowInst? = nil
  if let borrowedArgument {
    argumentBorrow = builder.createBeginBorrow(of: newApplyArgs[borrowedArgument])
    newApplyArgs[borrowedArgument] = argumentBorrow!
  }

  let fri = builder.createFunctionRef(specializedFunction)
  let newApply = builder.createApply(function: fri,
                                     function.isGeneric ? function.forwardingSubstitutionMap : SubstitutionMap(),
                                     arguments: newApplyArgs,
                                     isNonThrowing: callerSiteApply.isNonThrowing,
                                     isNonAsync: callerSiteApply.isNonAsync)

  // The thunk keeps its `@owned` result convention, so it has to take ownership of the
  // borrowed value returned by the specialized function.
  let copy = builder.createCopyValue(operand: newApply)
  // The `@guaranteed` result of the apply is a borrow introducer.
  builder.createEndBorrow(of: newApply)
  if let argumentBorrow {
    builder.createEndBorrow(of: argumentBorrow)
    builder.createDestroyValue(operand: argumentBorrow.borrowedValue)
  }
  builder.createReturn(of: copy)
}

/// Rewrites the body of `specializedFunction` so that it returns its result as a `@guaranteed`
/// value with a `return_borrow` instead of consuming it with a `return`.
private func convertResultToGuaranteed(in specializedFunction: Function,
                                       borrowedFrom argumentIndices: [Int],
                                       _ context: FunctionPassContext) {
  let returnInst = specializedFunction.returnInstruction as! ReturnInst

  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushIfNotVisited(returnInst.returnedValue)
  while let value = worklist.pop() {
    switch value {
    case let load as LoadInst where load.loadOwnership == .copy:
      let builder = Builder(before: load, context)
      let loadBorrow = builder.createLoadBorrow(fromAddress: load.address)
      load.replace(with: loadBorrow, context)

    case let copy as CopyValueInst:
      let builder = Builder(before: copy, context)
      let beginBorrow = builder.createBeginBorrow(of: copy.fromValue)
      copy.replace(with: beginBorrow, context)

    case let apply as ApplyInst:
      // The `@guaranteed` result of the new apply is a borrow introducer.
      apply.replace(withCallTo: specializedFunction, arguments: Array(apply.arguments), context)

    case let argument as Argument:
      let phi = Phi(argument)!
      worklist.pushIfNotVisited(contentsOf: phi.incomingValues)
      phi.value.set(ownership: .guaranteed, context)
      phi.value.set(reborrow: true, context)

    default:
      fatalError("Unexpected value: \(value)")
    }
  }

  // `borrowed-from` instructions for the new guaranteed phis.
  updateGuaranteedPhis(in: specializedFunction, context)

  let enclosingScopes = Array<Value>(returnInst.returnedValue.getBorrowIntroducers(context).map {
    let value = $0.value
    if let phi = Phi(value), let bf = phi.borrowedFrom {
      return bf
    }
    return value
  })
  let builder = Builder(before: returnInst, context)
  builder.createReturnBorrow(of: returnInst.returnedValue, fromScopes: enclosingScopes)
  context.erase(instruction: returnInst)
}

/// Converts the owned `argument` and the chain of forwarding instructions from the argument to
/// `returnedValue` to guaranteed (see `getForwardedOwnedArgument`).
private func convertForwardingChainToGuaranteed(from argument: FunctionArgument, to returnedValue: Value,
                                                _ context: FunctionPassContext) {
  var value = returnedValue
  while value != argument {
    let fwdInst = value.definingInstruction as! ForwardingInstruction
    fwdInst.setForwardingOwnership(to: .guaranteed, context)
    value = fwdInst.forwardedOperands.first(where: { $0.value.ownership != .none })!.value
  }
  argument.set(ownership: .guaranteed, context)
}

private extension ParameterInfo {
  func with(convention newConvention: ArgumentConvention) -> ParameterInfo {
    ParameterInfo(type: type, convention: newConvention, options: options, hasLoweredAddresses: hasLoweredAddresses)
  }
}

private extension Function {
  /// Returns true if the function body uses a generic parameter of the function, e.g. via the type of
  /// a value or a substitution map, even if no argument or result type contains a generic parameter.
  /// In this case the specialized function must keep the generic signature.
  var implicitlyUsesGenericParameter: Bool {
    for inst in instructions {
      if inst.results.contains(where: { $0.type.hasPrimaryArchetype }) {
        return true
      }
      if !(inst is DebugValueInst),
         inst.operands.contains(where: { $0.value.isUndefWithPrimaryArchetype })
      {
        return true
      }
      switch inst {
      case let apply as ApplySite:
        if apply.substitutionMap.usesGenericParameter {
          return true
        }
      case let builtin as BuiltinInst:
        if builtin.substitutionMap.usesGenericParameter {
          return true
        }
      case let typeValue as TypeValueInst:
        if typeValue.paramType.hasPrimaryArchetype {
          return true
        }
      default:
        break
      }
    }
    return false
  }
}

private extension Value {
  var isUndefWithPrimaryArchetype: Bool {
    self is Undef && type.hasPrimaryArchetype
  }
}

private extension SubstitutionMap {
  var usesGenericParameter: Bool {
    return replacementTypes.contains { $0.hasPrimaryArchetype }
  }
}
