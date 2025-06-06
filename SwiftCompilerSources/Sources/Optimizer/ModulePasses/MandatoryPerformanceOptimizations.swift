//===--- MandatoryPerformanceOptimizations.swift --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

/// Performs mandatory optimizations for performance-annotated functions, and global
/// variable initializers that are required to be statically initialized.
///
/// Optimizations include:
/// * de-virtualization
/// * mandatory inlining
/// * generic specialization
/// * mandatory memory optimizations
/// * dead alloc elimination
/// * instruction simplification
///
/// The pass starts with performance-annotated functions / globals and transitively handles
/// called functions.
///
let mandatoryPerformanceOptimizations = ModulePass(name: "mandatory-performance-optimizations") {
  (moduleContext: ModulePassContext) in

  var worklist = FunctionWorklist()
  // For embedded Swift, optimize all the functions (there cannot be any
  // generics, type metadata, etc.)
  if moduleContext.options.enableEmbeddedSwift {
    worklist.addAllNonGenericFunctions(of: moduleContext)
  } else {
    worklist.addAllMandatoryRequiredFunctions(of: moduleContext)
  }

  optimizeFunctionsTopDown(using: &worklist, moduleContext)

  // It's not required to set the perf_constraint flag on all functions in embedded mode.
  // Embedded mode already implies that flag.
  if !moduleContext.options.enableEmbeddedSwift {
    setPerformanceConstraintFlags(moduleContext)
  }
}

private func optimizeFunctionsTopDown(using worklist: inout FunctionWorklist,
                                      _ moduleContext: ModulePassContext) {
  while let f = worklist.pop() {
    moduleContext.transform(function: f) { context in
      if !context.loadFunction(function: f, loadCalleesRecursively: true) {
        return
      }
      optimize(function: f, context, moduleContext, &worklist)
    }

    // Generic specialization takes care of removing metatype arguments of generic functions.
    // But sometimes non-generic functions have metatype arguments which must be removed.
    // We need handle this case with a function signature optimization.
    removeMetatypeArgumentsInCallees(of: f, moduleContext)

    worklist.addCallees(of: f, moduleContext)
  }
}

private func setPerformanceConstraintFlags(_ moduleContext: ModulePassContext) {
  var worklist = FunctionWorklist()
  for f in moduleContext.functions where f.performanceConstraints != .none && f.isDefinition {
    worklist.pushIfNotVisited(f)
  }
  while let f = worklist.pop() {
    moduleContext.transform(function: f) { f.set(isPerformanceConstraint: true, $0) }
    worklist.addCallees(of: f, moduleContext)
  }
}

fileprivate struct PathFunctionTuple: Hashable {
  var path: SmallProjectionPath
  var function: Function
}

private func optimize(function: Function, _ context: FunctionPassContext, _ moduleContext: ModulePassContext, _ worklist: inout FunctionWorklist) {
  var alreadyInlinedFunctions: Set<PathFunctionTuple> = Set()

  // ObjectOutliner replaces calls to findStringSwitchCase with _findStringSwitchCaseWithCache, but this happens as a late SIL optimization,
  // which is a problem for Embedded Swift, because _findStringSwitchCaseWithCache will then reference non-specialized code. Solve this by
  // eagerly linking and specializing _findStringSwitchCaseWithCache whenever findStringSwitchCase is found in the module.
  if context.options.enableEmbeddedSwift {
    if function.hasSemanticsAttribute("findStringSwitchCase"),
        let f = context.lookupStdlibFunction(name: "_findStringSwitchCaseWithCache"),
        context.loadFunction(function: f, loadCalleesRecursively: true) {
      worklist.pushIfNotVisited(f)
    }
  }
  
  var changed = true
  while changed {
    changed = runSimplification(on: function, context, preserveDebugInfo: true) { instruction, simplifyCtxt in
      if let i = instruction as? OnoneSimplifiable {
        i.simplify(simplifyCtxt)
        if instruction.isDeleted {
          return
        }
      }
      switch instruction {
      case let apply as FullApplySite:
        inlineAndDevirtualize(apply: apply, alreadyInlinedFunctions: &alreadyInlinedFunctions, context, simplifyCtxt)

      // Embedded Swift specific transformations
      case let alloc as AllocRefInst:
        if context.options.enableEmbeddedSwift {
          specializeVTable(forClassType: alloc.type, errorLocation: alloc.location, moduleContext) {
            worklist.pushIfNotVisited($0)
          }
        }
      case let metatype as MetatypeInst:
        if context.options.enableEmbeddedSwift {
          let instanceType = metatype.type.loweredInstanceTypeOfMetatype(in: function)
          if instanceType.isClass {
            specializeVTable(forClassType: instanceType, errorLocation: metatype.location, moduleContext) {
              worklist.pushIfNotVisited($0)
            }
          }
        }
      case let classMethod as ClassMethodInst:
        if context.options.enableEmbeddedSwift {
          _ = context.specializeClassMethodInst(classMethod)
        }
      case let witnessMethod as WitnessMethodInst:
        if context.options.enableEmbeddedSwift {
          _ = context.specializeWitnessMethodInst(witnessMethod)
        }

      case let initExRef as InitExistentialRefInst:
        if context.options.enableEmbeddedSwift {
          for c in initExRef.conformances where c.isConcrete {
            specializeWitnessTable(for: c, moduleContext) {
              worklist.addWitnessMethods(of: $0)
            }
          }
        }

      case let bi as BuiltinInst:
        switch bi.id {
        case .BuildOrdinaryTaskExecutorRef,
             .BuildOrdinarySerialExecutorRef,
             .BuildComplexEqualitySerialExecutorRef:
          specializeWitnessTable(for: bi.substitutionMap.conformances[0], moduleContext) {
            worklist.addWitnessMethods(of: $0)
          }

        default:
          break
        }


      // We need to de-virtualize deinits of non-copyable types to be able to specialize the deinitializers.
      case let destroyValue as DestroyValueInst:
        if !devirtualizeDeinits(of: destroyValue, simplifyCtxt) {
          // If invoked from SourceKit avoid reporting false positives when WMO is turned off for indexing purposes.
          if moduleContext.enableWMORequiredDiagnostics {
            context.diagnosticEngine.diagnose(.deinit_not_visible, at: destroyValue.location)
          }
        }
      case let destroyAddr as DestroyAddrInst:
        if !devirtualizeDeinits(of: destroyAddr, simplifyCtxt) {
          // If invoked from SourceKit avoid reporting false positives when WMO is turned off for indexing purposes.
          if moduleContext.enableWMORequiredDiagnostics {
            context.diagnosticEngine.diagnose(.deinit_not_visible, at: destroyAddr.location)
          }
        }

      case let iem as InitExistentialMetatypeInst:
        if iem.uses.ignoreDebugUses.isEmpty {
          context.erase(instructionIncludingDebugUses: iem)
        }

      case let fri as FunctionRefInst:
        // Mandatory de-virtualization and mandatory inlining might leave referenced functions in "serialized"
        // functions with wrong linkage. Fix this by making the referenced function public.
        // It's not great, because it can prevent dead code elimination. But it's only a rare case.
        if function.serializedKind != .notSerialized,
           !fri.referencedFunction.hasValidLinkageForFragileRef(function.serializedKind)
        {
          fri.referencedFunction.set(linkage: .public, moduleContext)
        }
        
      case let copy as CopyAddrInst:
        if function.isGlobalInitOnceFunction, copy.source.type.isLoadable(in: function) {
          // In global init functions we have to make sure that redundant load elimination can remove all
          // loads (from temporary stack locations) so that globals can be statically initialized.
          // For this it's necessary to load copy_addr instructions to loads and stores.
          copy.replaceWithLoadAndStore(simplifyCtxt)
        }

      default:
        break
      }
    }

    _ = context.specializeApplies(in: function, isMandatory: true)

    removeUnusedMetatypeInstructions(in: function, context)

    // If this is a just specialized function, try to optimize copy_addr, etc.
    if eliminateRedundantLoads(in: function,
                               variant: function.isGlobalInitOnceFunction ? .mandatoryInGlobalInit : .mandatory,
                               context)
    {
      changed = true
    }

    changed = context.eliminateDeadAllocations(in: function) || changed
  }
}

private func inlineAndDevirtualize(apply: FullApplySite, alreadyInlinedFunctions: inout Set<PathFunctionTuple>,
                                   _ context: FunctionPassContext, _ simplifyCtxt: SimplifyContext) {
  // De-virtualization and inlining in/into a "serialized" function might create function references to functions
  // with wrong linkage. We need to fix this later (see handling of FunctionRefInst in `optimize`).
  if simplifyCtxt.tryDevirtualize(apply: apply, isMandatory: true) != nil {
    return
  }

  guard let callee = apply.referencedFunction else {
    return
  }

  if !context.loadFunction(function: callee, loadCalleesRecursively: true) {
    // We don't have the function body of the callee.
    return
  }

  if shouldInline(apply: apply, callee: callee, alreadyInlinedFunctions: &alreadyInlinedFunctions) {
    if apply.inliningCanInvalidateStackNesting  {
      simplifyCtxt.notifyInvalidatedStackNesting()
    }

    simplifyCtxt.inlineFunction(apply: apply, mandatoryInline: true)
  }
}

private func removeMetatypeArgumentsInCallees(of function: Function, _ context: ModulePassContext) {
  for inst in function.instructions {
    if let apply = inst as? FullApplySite {
      specializeByRemovingMetatypeArguments(apply: apply, context)
    }
  }
}

private func removeUnusedMetatypeInstructions(in function: Function, _ context: FunctionPassContext) {
  for inst in function.instructions {
    if let mt = inst as? MetatypeInst,
       mt.isTriviallyDeadIgnoringDebugUses {
      context.erase(instructionIncludingDebugUses: mt)
    }
  }
}

private func shouldInline(apply: FullApplySite, callee: Function, alreadyInlinedFunctions: inout Set<PathFunctionTuple>) -> Bool {
  if let beginApply = apply as? BeginApplyInst,
     !beginApply.canInline
  {
    return false
  }

  guard callee.canBeInlinedIntoCaller(withSerializedKind: apply.parentFunction.serializedKind) ||
        // Even if the serialization kind doesn't match, we need to make sure to inline witness method thunks
        // in embedded swift.
        callee.thunkKind == .thunk ||
        // Force inlining transparent co-routines. This might be necessary if `-enable-testing` is turned on.
        (apply is BeginApplyInst && callee.isTransparent)
  else {
    return false
  }

  // Cannot inline a non-ossa function into an ossa function
  if apply.parentFunction.hasOwnership && !callee.hasOwnership {
    return false
  }

  if callee.isTransparent {
    precondition(callee.hasOwnership, "transparent functions should have ownership at this stage of the pipeline")
    return true
  }

  if apply is BeginApplyInst {
    // Avoid co-routines because they might allocate (their context).
    return true
  }

  if callee.mayBindDynamicSelf {
    // We don't support inlining a function that binds dynamic self into a global-init function
    // because the global-init function cannot provide the self metadata.
    return false
  }

  if apply.parentFunction.isGlobalInitOnceFunction && callee.inlineStrategy == .always {
    // Some arithmetic operations, like integer conversions, are not transparent but `inline(__always)`.
    // Force inlining them in global initializers so that it's possible to statically initialize the global.
    return true
  }

  if apply.substitutionMap.isEmpty,
     let pathIntoGlobal = apply.resultIsUsedInGlobalInitialization(),
     alreadyInlinedFunctions.insert(PathFunctionTuple(path: pathIntoGlobal, function: callee)).inserted {
    return true
  }

  return false
}

private extension FullApplySite {
  func resultIsUsedInGlobalInitialization() -> SmallProjectionPath? {
    guard parentFunction.isGlobalInitOnceFunction,
          let global = parentFunction.getInitializedGlobal() else {
      return nil
    }

    switch numIndirectResultArguments {
    case 0:
      return singleDirectResult?.isStored(to: global)
    case 1:
      let resultAccessPath = arguments[0].accessPath
      switch resultAccessPath.base {
      case .global(let resultGlobal) where resultGlobal == global:
        return resultAccessPath.materializableProjectionPath
      case .stack(let allocStack) where resultAccessPath.projectionPath.isEmpty:
        return allocStack.getStoredValue(by: self)?.isStored(to: global)
      default:
        return nil
      }
    default:
      return nil
    }
  }
}

private extension AllocStackInst {
  func getStoredValue(by storingInstruction: Instruction) -> Value? {
    // If the only use (beside `storingInstruction`) is a load, it's the value which is
    // stored by `storingInstruction`.
    var loadedValue: Value? = nil
    for use in self.uses {
      switch use.instruction {
      case is DeallocStackInst:
        break
      case let load as LoadInst:
        if loadedValue != nil {
          return nil
        }
        loadedValue = load
      default:
        if use.instruction != storingInstruction {
          return nil
        }
      }
    }
    return loadedValue
  }
}

private extension Value {
  /// Analyzes the def-use chain of an apply instruction, and looks for a single chain that leads to a store instruction
  /// that initializes a part of a global variable or the entire variable:
  ///
  /// Example:
  ///   %g = global_addr @global
  ///   ...
  ///   %f = function_ref @func
  ///   %apply = apply %f(...)
  ///   store %apply to %g   <--- is a store to the global trivially (the apply result is immediately going into a store)
  ///
  /// Example:
  ///   %apply = apply %f(...)
  ///   %apply2 = apply %f2(%apply)
  ///   store %apply2 to %g   <--- is a store to the global (the apply result has a single chain into the store)
  ///
  /// Example:
  ///   %a = apply %f(...)
  ///   %s = struct $MyStruct (%a, %b)
  ///   store %s to %g   <--- is a partial store to the global (returned SmallProjectionPath is MyStruct.s0)
  ///
  /// Example:
  ///   %a = apply %f(...)
  ///   %as = struct $AStruct (%other, %a)
  ///   %bs = struct $BStruct (%as, %bother)
  ///   store %bs to %g   <--- is a partial store to the global (returned SmallProjectionPath is MyStruct.s0.s1)
  ///
  /// Returns nil if we cannot find a singular def-use use chain (e.g. because a value has more than one user)
  /// leading to a store to the specified global variable.
  func isStored(to global: GlobalVariable) -> SmallProjectionPath? {
    var singleUseValue: any Value = self
    var path = SmallProjectionPath()
    while true {
      // The initializer value of a global can contain access instructions if it references another
      // global variable by address, e.g.
      //   var p = Point(x: 10, y: 20)
      //   let o = UnsafePointer(&p)
      // Therefore ignore the `end_access` use of a `begin_access`.
      let relevantUses = singleUseValue.uses.ignoreDebugUses.ignoreUsers(ofType: EndAccessInst.self)

      guard let use = relevantUses.singleUse else {
        return nil
      }
      
      switch use.instruction {
      case is StructInst:
        path = path.push(.structField, index: use.index)
        break
      case is TupleInst:
        path = path.push(.tupleField, index: use.index)
        break
      case let ei as EnumInst:
        path = path.push(.enumCase, index: ei.caseIndex)
        break
      case let si as StoreInst:
        let accessPath = si.destination.getAccessPath(fromInitialPath: path)
        switch accessPath.base {
        case .global(let storedGlobal) where storedGlobal == global:
          return accessPath.materializableProjectionPath
        default:
          return nil
        }
      case is PointerToAddressInst, is AddressToPointerInst, is BeginAccessInst:
        break
      default:
        return nil
      }

      guard let nextInstruction = use.instruction as? SingleValueInstruction else {
        return nil
      }

      singleUseValue = nextInstruction
    }
  }
}

private extension Function {
  /// Analyzes the global initializer function and returns global it initializes (from `alloc_global` instruction).
  func getInitializedGlobal() -> GlobalVariable? {
    if !isDefinition {
      return nil
    }
    for inst in self.entryBlock.instructions {
      switch inst {
      case let agi as AllocGlobalInst:
        return agi.global
      default:
        break
      }
    }

    return nil
  }
}

fileprivate struct FunctionWorklist {
  private(set) var functions = Array<Function>()
  private var pushedFunctions = Set<Function>()
  private var currentIndex = 0

  mutating func pop() -> Function? {
    if currentIndex < functions.count {
      let f = functions[currentIndex]
      currentIndex += 1
      return f
    }
    return nil
  }
  
  mutating func addAllMandatoryRequiredFunctions(of moduleContext: ModulePassContext) {
    for f in moduleContext.functions {
      // Performance annotated functions
      if f.performanceConstraints != .none {
        pushIfNotVisited(f)
      }
      
      // Annotated global init-once functions
      if f.isGlobalInitOnceFunction {
        if let global = f.getInitializedGlobal(),
           global.mustBeInitializedStatically {
          pushIfNotVisited(f)
        }
      }
      
      // @const global init-once functions
      if f.isGlobalInitOnceFunction {
        if let global = f.getInitializedGlobal(),
           global.mustBeInitializedStatically {
          pushIfNotVisited(f)
        }
      }
    }
  }

  mutating func addAllNonGenericFunctions(of moduleContext: ModulePassContext) {
    for f in moduleContext.functions where !f.isGeneric {
      pushIfNotVisited(f)
    }
    return
  }

  mutating func addCallees(of function: Function, _ context: ModulePassContext) {
    for inst in function.instructions {
      switch inst {
      case let fri as FunctionRefInst:
        pushIfNotVisited(fri.referencedFunction)
      case let alloc as AllocRefInst:
        if context.options.enableEmbeddedSwift {
          addVTableMethods(forClassType: alloc.type, context)
        }
      case let metatype as MetatypeInst:
        if context.options.enableEmbeddedSwift {
          let instanceType = metatype.type.loweredInstanceTypeOfMetatype(in: function)
          if instanceType.isClass {
            addVTableMethods(forClassType: instanceType, context)
          }
        }

      default:
        break
      }
    }
  }

  mutating func addVTableMethods(forClassType classType: Type, _ context: ModulePassContext) {
    guard let vtable = classType.isGenericAtAnyLevel ?
                        context.lookupSpecializedVTable(for: classType) :
                        context.lookupVTable(for: classType.nominal!)
    else {
      return
    }
    for entry in vtable.entries where !entry.implementation.isGeneric {
      pushIfNotVisited(entry.implementation)
    }
  }

  mutating func addWitnessMethods(of witnessTable: WitnessTable) {
    for entry in witnessTable.entries {
      if case .method(_, let witness) = entry,
         let method = witness,
         // A new witness table can still contain a generic function if the method couldn't be specialized for
         // some reason and an error has been printed. Exclude generic functions to not run into an assert later.
         !method.isGeneric
      {
        pushIfNotVisited(method)
      }
    }
  }

  mutating func pushIfNotVisited(_ element: Function) {
    if pushedFunctions.insert(element).inserted {
      functions.append(element)
    }
  }
}
