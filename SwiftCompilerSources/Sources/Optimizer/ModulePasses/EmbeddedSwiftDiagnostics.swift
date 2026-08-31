//===--- EmbeddedSwiftDiagnostics.swift -----------------------------------===//
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

/// Diagnoses violations of Embedded Swift language restrictions.
///
let embeddedSwiftDiagnostics = ModulePass(name: "embedded-swift-diagnostics") {
  (moduleContext: ModulePassContext) in

  guard moduleContext.options.enableEmbeddedSwift,
        // Skip all embedded diagnostics if asked. This is used from SourceKit to avoid reporting
        // false positives when WMO is turned off for indexing purposes.
        moduleContext.enableWMORequiredDiagnostics
  else {
    return
  }

  // Try to start with public and exported functions to get better caller information in the diagnostics.
  let allFunctions = moduleContext.functions.lazy.filter { !$0.isGeneric }
                       .map { (function: $0, priority: $0.priority(moduleContext)) }
                       .sorted(by: { $0.priority < $1.priority })

  var checker = FunctionChecker(moduleContext)
  defer { checker.deinitialize() }

  for (function, _) in allFunctions {
    do {
      assert(checker.callStack.isEmpty)
      try checker.checkFunction(function)
    } catch let violation as Violation {
      checker.diagnose(violation, popCallStack: true)
    } catch {
      fatalError("unknown error thrown")
    }
  }

  checkVTables(moduleContext)
}

private struct FunctionChecker {
  let context: ModulePassContext
  var visitedFunctions = Set<Function>()
  var visitedConformances = Set<Conformance>()
  var reportedProblems = Set<ReportedProblem>()
  var callStack: Stack<CallSite>

  init(_ context: ModulePassContext) {
    self.context = context
    self.callStack = Stack(context)
  }

  mutating func deinitialize() {
    callStack.deinitialize()
  }

  mutating func checkFunction(_ function: Function) throws {
    guard function.isDefinition,
          // Avoid infinite recursion
          visitedFunctions.insert(function).inserted
    else {
      return
    }

    for inst in function.instructions {
      try checkInstruction(inst)
    }
  }

  mutating func checkInstruction(_ instruction: Instruction) throws {
    switch instruction {
    case is OpenExistentialBoxInst,
         is OpenExistentialBoxValueInst,
         is OpenExistentialValueInst,
         is OpenExistentialAddrInst,
         is OpenExistentialMetatypeInst:
      break

    case is AllocExistentialBoxInst:
      let alloc = instruction as! AllocExistentialBoxInst
      try diagnoseHeapAllocation(
        Violation(.embedded_swift_allocating_existential_box,
                  alloc.existentialType, alloc.formalConcreteType.rawType, in: alloc)
      )
      break

    case let iem as InitExistentialMetatypeInst:
      let concreteType = iem.operand.value.type.loweredInstanceTypeOfMetatype(in: iem.parentFunction)
      if !context.bridgedPassContext.fitsInOpaqueExistentialPayload(concreteType.bridged) {
        let existentialType = iem.type.objectType
        try diagnoseHeapAllocation(
          Violation(.embedded_swift_allocating_existential_metatype_box,
                    existentialType, concreteType.rawType, in: iem)
        )
      }

      for conf in iem.conformances {
        try checkConformance(conf, at: iem)
      }

    case let iea as InitExistentialAddrInst:
      if !context.bridgedPassContext.fitsInOpaqueExistentialPayload(iea.type.bridged) {
        try diagnoseHeapAllocation(
          Violation(.embedded_swift_allocating_existential_box,
                    iea.operands[0].value.type, iea.formalConcreteType.rawType, in: iea)
        )
      }
      fallthrough

    case is InitExistentialValueInst,
         is InitExistentialRefInst:
      let ie = instruction as! any InitExistentialInstruction

      for conf in ie.conformances {
        try checkConformance(conf, at: instruction)
      }

    case is ValueMetatypeInst,
         is MetatypeInst,
         is ExistentialMetatypeInst:
      let metaType = (instruction as! SingleValueInstruction).type
      switch metaType.representationOfMetatype {
      case .objC:
        let rawType = metaType.canonicalType.rawType.instanceTypeOfMetatype
        let type = rawType.isDynamicSelf ? rawType.staticTypeOfDynamicSelf : rawType
        throw Violation(.embedded_swift_metatype_type, type, in: instruction)

      case .thick, .thin:
        break
      }

    case let kpi as KeyPathInst:
      guard kpi.supportedInEmbeddedSwift else {
        throw Violation(.embedded_swift_keypath, in: instruction)
      }
      // A key path that captures values (subscript arguments) can't be a shared
      // immortal constant: IRGen allocates an instance and fills the captures in
      // every time the key path is formed. That is easy to write by accident in
      // hot code, so hint about it.
      if !kpi.operands.isEmpty {
        context.diagnosticEngine.diagnose(.perf_hint_keypath_captures_values,
                                          kpi.operands.count,
                                          at: instruction.location)
      }

    case is CheckedCastAddrBranchInst,
         is UnconditionalCheckedCastAddrInst:
       if let checkedCast = instruction as? CheckedCastAddrBranchInst {
         if !checkedCast.supportedInEmbeddedSwift {
           throw Violation(.embedded_swift_dynamic_cast, in: instruction)
         }
         checkCastTargetUniqueness(checkedCast.targetFormalType, in: instruction)
       } else {
         let checkedCast = instruction as! UnconditionalCheckedCastAddrInst
         if !checkedCast.supportedInEmbeddedSwift {
           throw Violation(.embedded_swift_dynamic_cast, in: instruction)
         }
         checkCastTargetUniqueness(checkedCast.targetFormalType, in: instruction)
       }

    // The value-form checked casts (e.g. a class-bound `any P` downcast to a
    // concrete class, `x as? C`) are lowered to an isa/metadata-pointer
    // comparison. If the target class has a non-unique definition, the
    // allocating module and this module may see different metadata records, so
    // the cast silently fails at runtime. Diagnose it.
    case let ccb as CheckedCastBranchInst:
      checkCastTargetUniqueness(ccb.targetFormalType, in: instruction)

    case let ucc as UnconditionalCheckedCastInst:
      checkCastTargetUniqueness(ucc.targetFormalType, in: instruction)

    case is AllocBoxInst,
         is AllocRefInst,
         is AllocRefDynamicInst:
      try diagnoseHeapAllocation(
        Violation(.embedded_swift_allocating_type,
                  (instruction as! SingleValueInstruction).type, in: instruction)
      )

    case is ThunkInst:
      try diagnoseHeapAllocation(
        Violation(.embedded_swift_allocating, in: instruction)
      )

    case let ba as BeginApplyInst:
      // The old yield_once_1 coroutine uses a heap-allocated frame, so it
      // cannot be used in no-allocations mode.
      if !ba.isCalleeAllocated {
        try diagnoseHeapAllocation(
          Violation(.embedded_swift_allocating_coroutine, in: instruction)
        )
      }

      // For yield_once_2, whether it allocates on the heap or the stack
      // depends on the provided allocator, which isn't knowable here.
      try checkApply(apply: ba)

    case let pai as PartialApplyInst:
      if !pai.isOnStack {
        try diagnoseHeapAllocation(
          Violation(.embedded_swift_allocating_closure, in: instruction)
        )
      }
      try checkApply(apply: pai)

    // Remaining apply instructions
    case let apply as ApplySite:
      try checkApply(apply: apply)

    case let destroy as DestroyValueInst where !destroy.isDeadEnd:
      let type = destroy.destroyedValue.type
      if let nominal = type.nominal,
         !nominal.hasClangNode,
         nominal.valueTypeDestructor != nil,
         !(destroy.destroyedValue.lookThoughOwnershipInstructions is DropDeinitInst)
      {
        throw Violation(.deinit_not_visible, type, in: destroy)
      }

    case let destroy as DestroyAddrInst:
      let type = destroy.destroyedAddress.type
      if let nominal = type.nominal,
         !nominal.hasClangNode,
         nominal.valueTypeDestructor != nil
      {
        throw Violation(.deinit_not_visible, type, in: destroy)
      }

    case let bi as BuiltinInst:
      switch bi.id {
      case .AllocRaw, .AllocRawTyped:
        try diagnoseHeapAllocation(
          Violation(.embedded_swift_allocation_raw, in: bi)
        )
      case .BuildOrdinaryTaskExecutorRef,
           .BuildOrdinarySerialExecutorRef,
           .BuildComplexEqualitySerialExecutorRef:
        // Those builtins implicitly create an existential.
        try checkConformance(bi.substitutionMap.conformances[0], at: bi)

      case .DestroyArray:
        let elementType = bi.substitutionMap.replacementType.loweredType(in: bi.parentFunction)
        if let nominal = elementType.nominal,
           !nominal.hasClangNode,
           nominal.valueTypeDestructor != nil
        {
          throw Violation(.deinit_not_visible, elementType, in: bi)
        }

      default:
        break
      }

    default:
      break
    }
  }

  // A class `as?`/`as!` downcast compares type-metadata pointers at runtime.
  // In Embedded Swift a type's metadata is emitted with a non-unique
  // definition unless the type is `@export(interface)` (or defined in the main
  // module), so an object allocated in one module and downcast in another can
  // compare against a different metadata record and the cast silently fails.
  // Warn about casting to such a type here (rdar://179424428).
  mutating func checkCastTargetUniqueness(_ targetType: CanonicalType, in instruction: Instruction) {
    // Only class metadata identity is at stake: `swift_dynamicCastClass`
    // compares isa pointers. Struct/enum casts don't rely on a unique metadata
    // record the same way.
    //
    // Generic classes are exempt: their metadata is instantiated on demand
    // through the runtime metadata accessor, which uniques it, so the redundant
    // per-module definitions still resolve to a single record at runtime.
    guard targetType.isClass,
          !targetType.isGenericAtAnyLevel,
          let nominal = targetType.nominal,
          nominal.hasNonUniqueDefinition
    else {
      return
    }
    diagnose(Violation(.embedded_swift_cast_to_nonunique_type, targetType.rawType, in: instruction),
             popCallStack: false)
  }

  mutating func checkApply(apply: ApplySite) throws {
    if apply.isAsync {
      try diagnoseHeapAllocation(
        Violation(.embedded_swift_allocating_async, in: apply)
      )
    }

    if !apply.callee.type.hasValidSignatureForEmbedded,
       // Some runtime functions have generic parameters in SIL, which are not used in IRGen.
       // Therefore exclude runtime functions at all.
       !apply.callsEmbeddedRuntimeFunction
    {
      switch apply.callee {
      case let cmi as ClassMethodInst:
        throw Violation(.embedded_cannot_specialize_class_method, cmi.member, in: apply)
      case let wmi as WitnessMethodInst:
        throw Violation(.embedded_cannot_specialize_witness_method, wmi.member, in: apply)
      default:
        if apply.substitutionMap.replacementTypes.contains(where: { $0.hasDynamicSelf }),
           apply.calleeHasGenericSelfMetatypeParameter
        {
          throw Violation(.embedded_call_generic_function_with_dynamic_self, in: apply)
        }
        throw Violation(.embedded_call_generic_function, in: apply)
      }
    }

    // Although all (non-generic) functions are initially put into the worklist there are two reasons
    // to call `checkFunction` recursively:
    // * To get a better caller info in the diagnostics.
    // * When passing an opened existential to a generic function, it's valid in Embedded swift even if the
    //   generic is not specialized. We need to check such generic functions, too.
    if let callee = apply.referencedFunction {
      callStack.push(CallSite(apply: apply, callee: callee))
      try checkFunction(callee)
      _ = callStack.pop()
    }
  }

  // Check for any violations in witness tables for existentials.
  mutating func checkConformance(_ conformance: Conformance, at instruction: Instruction) throws {
    guard conformance.isConcrete,
          // Avoid infinite recursion
          visitedConformances.insert(conformance).inserted,
          let witnessTable = context.lookupWitnessTable(for: conformance)
    else {
      return
    }
    for entry in witnessTable.entries {
      switch entry {
      case .invalid, .associatedType:
        break
      case .method(let requirement, let witness):
        if let witness = witness {
          callStack.push(CallSite(location: instruction.location, function: instruction.parentFunction,
                                  kind: .conformance))
          if witness.isGeneric {
            throw Violation(.embedded_cannot_specialize_witness_method, requirement,
                            at: witness.location, in: witness)
          }
          try checkFunction(witness)
          _ = callStack.pop()
        }
      case .baseProtocol(_, let witness):
        try checkConformance(witness, at: instruction)
      case .associatedConformance(_, let assocConf):
        // If it's not a class protocol, the associated type can never be used to create
        // an existential. Therefore this witness entry is never used at runtime in embedded swift.
        if assocConf.protocol.requiresClass {
          try checkConformance(assocConf, at: instruction)
        }
      }
    }
  }

  mutating func diagnose(_ violation: Violation, popCallStack: Bool) {
    // A problem which is found in a function of another module - e.g. in a standard library
    // function or in a specialization of such a function - is reported at the innermost call site
    // in the module which is currently compiled, because that's the code the user can change.
    let error = violation.diagnostic
    var diagPrinted = false
    var isDuplicate = false
    if let sourceLoc = violation.function.reportLocation(of: error.location, context) {
      diagPrinted = true
      isDuplicate = !report(error, at: sourceLoc)
    }

    var savedCallStack = Stack<CallSite>(context)

    // If the problem is not in the current module (e.g. because it's in a stdlib function), search
    // the callstack and use the location from a call site.
    while let callSite = callStack.pop() {
      if !popCallStack {
        savedCallStack.push(callSite)
      }

      if !diagPrinted {
        if let sourceLoc = callSite.function.reportLocation(of: callSite.location, context) {
          diagPrinted = true
          isDuplicate = !report(error, at: sourceLoc)
        }
      } else if !isDuplicate {
        // Print useful callsite information as a note (see `CallSite`)
        switch callSite.kind {
        case .constructorCall:
          context.diagnosticEngine.diagnose(.embedded_constructor_called, at: callSite.location)
        case .specializedCall:
          context.diagnosticEngine.diagnose(.embedded_specialization_called_from, at: callSite.location)
        case .conformance:
          context.diagnosticEngine.diagnose(.embedded_existential_created, at: callSite.location)
        case .call:
          break
        }
      }
    }
    if !diagPrinted {
      // The problem is in another module and not reachable from any call in the current module,
      // e.g. because the containing function is only referenced from a vtable. There is nothing
      // better to point at than the original location, which means loading the other module's
      // source file.
      context.diagnosticEngine.diagnose(error)
    }

    while let callSite = savedCallStack.pop() {
      callStack.push(callSite)
    }
  }

  /// Reports `error` at `sourceLoc` and returns true.
  ///
  /// Does nothing and returns false if the identical problem was already reported at this location.
  /// This can happen because a single call in the current module can reach multiple problems in
  /// other modules - e.g. an array literal allocates its buffer in several standard library
  /// functions - which would all be reported at that call.
  private mutating func report(_ error: Diagnostic<Location>, at sourceLoc: SourceLoc) -> Bool {
    if let problem = ReportedProblem(error, at: sourceLoc),
       !reportedProblems.insert(problem).inserted {
      return false
    }
    context.diagnosticEngine.diagnose(error.id, error.arguments, at: sourceLoc)
    return true
  }

  /// Emit a diagnostic describing a heap allocation.
  mutating func diagnoseHeapAllocation(_ violation: Violation) throws {
    // Under -no-allocations mode, heap allocations are fatal
    if context.options.noAllocations {
      throw violation
    }

    diagnose(violation, popCallStack: false)
  }
}

// Print errors for generic functions in vtables, which is not allowed in embedded Swift.
private func checkVTables(_ context: ModulePassContext) {
  for vTable in context.vTables {
    if !vTable.class.isGenericAtAnyLevel || vTable.isSpecialized {
      for entry in vTable.entries where entry.implementation.isGeneric {
        context.diagnosticEngine.diagnose(.embedded_cannot_specialize_class_method, entry.methodDecl,
                                          at: entry.methodDecl.location)
      }
    }
  }
}

/// Relevant call site information for diagnostics.
/// This information is printed as additional note(s) after the original diagnostic.
private struct CallSite {
  enum Kind {
    // A regular function call. Not every function call in the call stack is printed in diagnostics.
    // This is only used if the original instruction doesn't have a location.
    case call

    // If the error is in a constructor, this is the place where the object/value is created.
    case constructorCall

    // If the error is in a specialized function, this is the place where the generic function is originally
    // specialized with concrete types. This is useful if a specialized type is relevant for the error.
    case specializedCall

    // If the error is in a protocol witness method, this is the place where the existential is created.
    case conformance
  }

  /// The location of the call.
  let location: Location

  /// The function which contains the call.
  let function: Function

  let kind: Kind

  init(apply: ApplySite, callee: Function) {
    self.location = apply.location
    self.function = apply.parentFunction
    if let d = callee.location.decl, d is ConstructorDecl {
      self.kind = .constructorCall
    } else if callee.isSpecialization && !apply.parentFunction.isSpecialization {
      self.kind = .specializedCall
    } else {
      self.kind = .call
    }
  }

  init(location: Location, function: Function, kind: Kind) {
    self.location = location
    self.function = function
    self.kind = kind
  }
}

/// A problem which is diagnosed by this pass, plus the function which contains the offending code.
///
/// The containing function determines _where_ the problem is reported: if it is not part of the
/// module which is currently compiled, the problem is reported at the innermost call site in the
/// current module (see `FunctionChecker.diagnose`).
private struct Violation: Error {
  let diagnostic: Diagnostic<Location>

  /// The function which contains the offending code.
  let function: Function

  init(_ id: DiagID, _ arguments: DiagnosticArgument..., in instruction: Instruction) {
    self.diagnostic = Diagnostic(id, arguments, at: instruction.location)
    self.function = instruction.parentFunction
  }

  init(_ id: DiagID, _ arguments: DiagnosticArgument..., at location: Location, in function: Function) {
    self.diagnostic = Diagnostic(id, arguments, at: location)
    self.function = function
  }
}

private extension Function {
  // The priority (1 = highest) which defines the order in which functions are checked.
  // This is important to get good caller information in diagnostics.
  func priority(_ context: ModulePassContext) -> Int {
    // Functions in which a problem cannot be reported directly are checked last: a problem in such
    // a function - e.g. in a standard library function, in a specialization of one, or in a
    // compiler-generated function without a source location like `swift_readAtKeyPath` - is
    // reported at a call site in the current module, which requires that callers are checked first.
    guard reportLocation(of: location, context) != nil else {
      return 5
    }
    if let decl = location.decl {
      if decl is DestructorDecl || decl is ConstructorDecl {
        return 4
      }
      if let parent = decl.parentDeclContext, parent is ClassDecl {
        return 2
      }
    }
    if isPossiblyUsedExternally {
      return 1
    }
    return 3
  }

  /// The source location at which a problem at `location` in this function is reported, or nil if
  /// the problem must be reported at a call site instead.
  ///
  /// Problems are only reported in the module which is currently compiled, because that's the code
  /// which the user can change. Note that the module is determined from the function and not from
  /// `location`: a location of another module can have a valid `SourceLoc` if that module was built
  /// with a swiftsourceinfo file. And even in the current module a location doesn't necessarily
  /// refer to source code, e.g. in compiler-generated functions.
  func reportLocation(of location: Location, _ context: some Context) -> SourceLoc? {
    isInCurrentModule(context) ? location.sourceLoc : nil
  }
}

/// Identifies a problem which is already reported, so that it's not reported twice at the same
/// source location.
private struct ReportedProblem: Hashable {
  let id: DiagID
  let sourceLoc: UnsafeRawPointer?
  let arguments: String

  /// Returns nil if the diagnostic's arguments cannot be compared. Such a diagnostic is always
  /// reported, because it's not known if it's a duplicate.
  init?(_ error: Diagnostic<Location>, at sourceLoc: SourceLoc) {
    var arguments = ""
    for argument in error.arguments {
      guard let printableArgument = argument as? CustomStringConvertible else {
        return nil
      }
      arguments += printableArgument.description + ";"
    }
    self.id = error.id
    self.sourceLoc = sourceLoc.bridged.raw
    self.arguments = arguments
  }
}

private extension ApplySite {
  var callsEmbeddedRuntimeFunction: Bool {
    if let callee = referencedFunction,
       !callee.isDefinition,
       !callee.name.startsWith("$e")
    {
      return true
    }
    return false
  }

  var calleeHasGenericSelfMetatypeParameter: Bool {
    let convention = FunctionConvention(for: callee.type.canonicalType, in: parentFunction)
    guard convention.hasSelfParameter, let selfParam = convention.parameters.last else {
      return false
    }
    let selfParamType = selfParam.type
    return selfParamType.isMetatype && selfParamType.instanceTypeOfMetatype.isGenericTypeParameter
  }
}

