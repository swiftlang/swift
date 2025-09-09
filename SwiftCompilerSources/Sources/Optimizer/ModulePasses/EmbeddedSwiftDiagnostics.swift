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
  let allFunctions = Array(moduleContext.functions.lazy.filter { !$0.isGeneric })
                       .sorted(by: { $0.priority < $1.priority })

  var checker = FunctionChecker(moduleContext)
  defer { checker.deinitialize() }

  for function in allFunctions {
    do {
      assert(checker.callStack.isEmpty)
      try checker.checkFunction(function)
    } catch let error as Diagnostic<Location> {
      checker.diagnose(error)
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
    case is OpenExistentialMetatypeInst,
         is InitExistentialMetatypeInst:
      throw Diagnostic(.embedded_swift_metatype_type, instruction.operands[0].value.type, at: instruction.location)

    case is OpenExistentialBoxInst,
         is OpenExistentialBoxValueInst,
         is OpenExistentialValueInst,
         is OpenExistentialAddrInst,
         is InitExistentialAddrInst,
         is InitExistentialValueInst,
         is ExistentialMetatypeInst:
      throw Diagnostic(.embedded_swift_existential_type, instruction.operands[0].value.type, at: instruction.location)

    case let aeb as AllocExistentialBoxInst:
      throw Diagnostic(.embedded_swift_existential_type, aeb.type, at: instruction.location)

    case let ier as InitExistentialRefInst:
      for conf in ier.conformances {
        try checkConformance(conf, location: ier.location)
      }

    case is ValueMetatypeInst,
         is MetatypeInst:
      let metaType = (instruction as! SingleValueInstruction).type
      if metaType.representationOfMetatype != .thin {
        let rawType = metaType.canonicalType.rawType.instanceTypeOfMetatype
        let type = rawType.isDynamicSelf ? rawType.staticTypeOfDynamicSelf : rawType
        if !type.isClass {
          throw Diagnostic(.embedded_swift_metatype_type, type, at: instruction.location)
        }
      }

    case is KeyPathInst:
      throw Diagnostic(.embedded_swift_keypath, at: instruction.location)

    case is CheckedCastAddrBranchInst,
         is UnconditionalCheckedCastAddrInst:
      throw Diagnostic(.embedded_swift_dynamic_cast, at: instruction.location)

    case let abi as AllocBoxInst:
      // It needs a bit of work to support alloc_box of generic non-copyable structs/enums with deinit,
      // because we need to specialize the deinit functions, though they are not explicitly referenced in SIL.
      // Until this is supported, give an error in such cases. Otherwise IRGen would crash.
      if abi.allocsGenericValueTypeWithDeinit {
        throw Diagnostic(.embedded_capture_of_generic_value_with_deinit, at: abi.location)
      }
      fallthrough
    case is AllocRefInst,
         is AllocRefDynamicInst:
      if context.options.noAllocations {
        throw Diagnostic(.embedded_swift_allocating_type, (instruction as! SingleValueInstruction).type,
                              at: instruction.location)
      }

    case is ThunkInst:
      if context.options.noAllocations {
        throw Diagnostic(.embedded_swift_allocating, at: instruction.location)
      }

    case let ba as BeginApplyInst:
      if context.options.noAllocations {
        throw Diagnostic(.embedded_swift_allocating_coroutine, at: instruction.location)
      }
      try checkApply(apply: ba)

    case let pai as PartialApplyInst:
      if context.options.noAllocations && !pai.isOnStack {
        throw Diagnostic(.embedded_swift_allocating_closure, at: instruction.location)
      }
      try checkApply(apply: pai)

    // Remaining apply instructions
    case let apply as ApplySite:
      try checkApply(apply: apply)

    case let bi as BuiltinInst:
      switch bi.id {
      case .AllocRaw:
        if context.options.noAllocations {
          throw Diagnostic(.embedded_swift_allocating, at: instruction.location)
        }
      case .BuildOrdinaryTaskExecutorRef,
           .BuildOrdinarySerialExecutorRef,
           .BuildComplexEqualitySerialExecutorRef:
        // Those builtins implicitly create an existential.
        try checkConformance(bi.substitutionMap.conformances[0], location: bi.location)

      default:
        break
      }

    default:
      break
    }
  }

  mutating func checkApply(apply: ApplySite) throws {
    if context.options.noAllocations && apply.isAsync {
      throw Diagnostic(.embedded_swift_allocating_type, at: apply.location)
    }

    if !apply.callee.type.hasValidSignatureForEmbedded,
       // Some runtime functions have generic parameters in SIL, which are not used in IRGen.
       // Therefore exclude runtime functions at all.
       !apply.callsEmbeddedRuntimeFunction
    {
      switch apply.callee {
      case let cmi as ClassMethodInst:
        throw Diagnostic(.embedded_cannot_specialize_class_method, cmi.member, at: apply.location)
      case let wmi as WitnessMethodInst:
        throw Diagnostic(.embedded_cannot_specialize_witness_method, wmi.member, at: apply.location)
      default:
        if apply.substitutionMap.replacementTypes.contains(where: { $0.hasDynamicSelf }),
           apply.calleeHasGenericSelfMetatypeParameter
        {
          throw Diagnostic(.embedded_call_generic_function_with_dynamic_self, at: apply.location)
        }
        throw Diagnostic(.embedded_call_generic_function, at: apply.location)
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
  mutating func checkConformance(_ conformance: Conformance, location: Location) throws {
    guard conformance.isConcrete,
          // Avoid infinite recursion
          visitedConformances.insert(conformance).inserted,
          let witnessTable = context.lookupWitnessTable(for: conformance)
    else {
      return
    }
    if !conformance.protocol.requiresClass {
      throw Diagnostic(.embedded_swift_existential_protocol, conformance.protocol.name, at: location)
    }

    for entry in witnessTable.entries {
      switch entry {
      case .invalid, .associatedType:
        break
      case .method(let requirement, let witness):
        if let witness = witness {
          callStack.push(CallSite(location: location, kind: .conformance))
          if witness.isGeneric {
            throw Diagnostic(.embedded_cannot_specialize_witness_method, requirement, at: witness.location)
          }
          try checkFunction(witness)
          _ = callStack.pop()
        }
      case .baseProtocol(_, let witness):
        try checkConformance(witness, location: location)
      case .associatedConformance(_, let assocConf):
        // If it's not a class protocol, the associated type can never be used to create
        // an existential. Therefore this witness entry is never used at runtime in embedded swift.
        if assocConf.protocol.requiresClass {
          try checkConformance(assocConf, location: location)
        }
      }
    }
  }

  mutating func diagnose(_ error: Diagnostic<Location>) {
    var diagPrinted = false
    if error.location.hasValidLineNumber {
      context.diagnosticEngine.diagnose(error)
      diagPrinted = true
    }

    // If the original instruction doesn't have a location (e.g. because it's in a stdlib function),
    // search the callstack and use the location from a call site.
    while let callSite = callStack.pop() {
      if !diagPrinted {
        if callSite.location.hasValidLineNumber {
          context.diagnosticEngine.diagnose(error.id, error.arguments, at: callSite.location)
          diagPrinted = true
        }
      } else {
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
      context.diagnosticEngine.diagnose(error)
    }
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

  let location: Location
  let kind: Kind

  init(apply: ApplySite, callee: Function) {
    self.location = apply.location
    if let d = callee.location.decl, d is ConstructorDecl {
      self.kind = .constructorCall
    } else if callee.isSpecialization && !apply.parentFunction.isSpecialization {
      self.kind = .specializedCall
    } else {
      self.kind = .call
    }
  }

  init(location: Location, kind: Kind) {
    self.location = location
    self.kind = kind
  }
}

private extension Function {
  // The priority (1 = highest) which defines the order in which functions are checked.
  // This is important to get good caller information in diagnostics.
  var priority: Int {
    // There might be functions without a location, e.g. `swift_readAtKeyPath` generated by SILGen for keypaths.
    // It's okay to skip the ctor/dtor/method detection logic for those.
    if location.hasValidLineNumber {
      if let decl = location.decl {
        if decl is DestructorDecl || decl is ConstructorDecl {
          return 4
        }
        if let parent = decl.parent, parent is ClassDecl {
          return 2
        }
      }
    }
    if isPossiblyUsedExternally {
      return 1
    }
    return 3
  }
}

private extension AllocBoxInst {
  var allocsGenericValueTypeWithDeinit: Bool {
    type.getBoxFields(in: parentFunction).contains { $0.hasGenericValueDeinit(in: parentFunction) }
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

private extension Type {
  func hasGenericValueDeinit(in function: Function) -> Bool {
    guard isMoveOnly, let nominal = nominal else {
      return false
    }

    if nominal.isGenericAtAnyLevel && nominal.valueTypeDestructor != nil {
      return true
    }

    if isStruct {
      if let fields = getNominalFields(in: function) {
        return fields.contains { $0.hasGenericValueDeinit(in: function) }
      }
    } else if isEnum {
      if let enumCases = getEnumCases(in: function) {
        return enumCases.contains { $0.payload?.hasGenericValueDeinit(in: function) ?? false }
      }
    }
    return false
  }
}
