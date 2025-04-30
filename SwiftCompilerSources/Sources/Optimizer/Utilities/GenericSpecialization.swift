//===--- GenericSpecialization.swift ---------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

func specializeVTable(forClassType classType: Type,
                      errorLocation: Location,
                      _ context: ModulePassContext,
                      notifyNewFunction: (Function) -> ())
{
  var specializer = VTableSpecializer(errorLocation: errorLocation, context)
  specializer.specializeVTable(forClassType: classType, notifyNewFunction)
}

private struct VTableSpecializer {
  let errorLocation: Location
  let context: ModulePassContext

  // The type of the first class in the hierarchy which implements a method
  private var baseTypesOfMethods = Dictionary<Function, Type>()

  init(errorLocation: Location, _ context: ModulePassContext) {
    self.errorLocation = errorLocation
    self.context = context
  }

  mutating func specializeVTable(forClassType classType: Type, _ notifyNewFunction: (Function) -> ()) {
    // First handle super classes.
    // This is also required for non-generic classes - in case a superclass is generic, e.g.
    // `class Derived : Base<Int> {}` - for two reasons:
    // * A vtable of a derived class references the vtable of the super class. And of course the referenced
    //   super-class vtable needs to be a specialized vtable.
    // * Even a non-generic derived class can contain generic methods of the base class in case a base-class
    //   method is not overridden.
    //
    if let superClassTy = classType.superClassType {
      specializeVTable(forClassType: superClassTy, notifyNewFunction)
    }

    let classDecl = classType.nominal! as! ClassDecl
    guard let origVTable = context.lookupVTable(for: classDecl) else {
      if context.enableWMORequiredDiagnostics {
        context.diagnosticEngine.diagnose(.cannot_specialize_class, classType, at: errorLocation)
      }
      return
    }

    for entry in origVTable.entries {
      if baseTypesOfMethods[entry.implementation] == nil {
        baseTypesOfMethods[entry.implementation] = classType
      }
    }

    if classType.isGenericAtAnyLevel {
      if context.lookupSpecializedVTable(for: classType) != nil {
        // We already specialized the vtable
        return
      }
      let newEntries = specializeEntries(of: origVTable, notifyNewFunction)
      context.createSpecializedVTable(entries: newEntries, for: classType, isSerialized: false)
    } else {
      if !origVTable.entries.contains(where: { $0.implementation.isGeneric }) {
        // The vtable (of the non-generic class) doesn't contain any generic functions (from a generic base class).
        return
      }
      let newEntries = specializeEntries(of: origVTable, notifyNewFunction)
      context.replaceVTableEntries(of: origVTable, with: newEntries)
    }
  }

  private func specializeEntries(of vTable: VTable, _ notifyNewFunction: (Function) -> ()) -> [VTable.Entry] {
    return vTable.entries.map { entry in
      if !entry.implementation.isGeneric {
        return entry
      }
      let baseType = baseTypesOfMethods[entry.implementation]!
      let classContextSubs = baseType.contextSubstitutionMap
      let methodSubs = classContextSubs.getMethodSubstitutions(for: entry.implementation)

      guard !methodSubs.conformances.contains(where: {!$0.isValid}),
            context.loadFunction(function: entry.implementation, loadCalleesRecursively: true),
            let specializedMethod = context.specialize(function: entry.implementation, for: methodSubs) else
      {
        return entry
      }
      notifyNewFunction(specializedMethod)

      context.deserializeAllCallees(of: specializedMethod, mode: .allFunctions)
      specializedMethod.set(linkage: .public, context)
      specializedMethod.set(isSerialized: false, context)

      return VTable.Entry(kind: entry.kind, isNonOverridden: entry.isNonOverridden,
                          methodDecl: entry.methodDecl, implementation: specializedMethod)
    }
  }
}

/// Specializes a witness table of `conformance` for the concrete type of the conformance.
func specializeWitnessTable(for conformance: Conformance,
                            _ context: ModulePassContext,
                            _ notifyNewWitnessTable: (WitnessTable) -> ())
{
  if let existingSpecialization = context.lookupWitnessTable(for: conformance),
         existingSpecialization.isSpecialized
  {
    return
  }

  let baseConf = conformance.isInherited ? conformance.inheritedConformance: conformance
  if !baseConf.isSpecialized {
    var visited = Set<Conformance>()
    specializeDefaultMethods(for: conformance, visited: &visited, context, notifyNewWitnessTable)
    return
  }

  guard let witnessTable = context.lookupWitnessTable(for: baseConf.genericConformance) else {
    fatalError("no witness table found")
  }
  assert(witnessTable.isDefinition, "No witness table available")
  let substitutions = baseConf.specializedSubstitutions

  let newEntries = witnessTable.entries.map { origEntry in
    switch origEntry {
    case .invalid:
      return WitnessTable.Entry.invalid
    case .method(let requirement, let witness):
      guard let origMethod = witness else {
        return origEntry
      }
      let methodSubs = substitutions.getMethodSubstitutions(for: origMethod,
                         // Generic self types need to be handled specially (see `getMethodSubstitutions`)
                         selfType: origMethod.hasGenericSelf(context) ? conformance.type.canonical : nil)

      guard !methodSubs.conformances.contains(where: {!$0.isValid}),
            context.loadFunction(function: origMethod, loadCalleesRecursively: true),
            let specializedMethod = context.specialize(function: origMethod, for: methodSubs) else
      {
        return origEntry
      }
      return .method(requirement: requirement, witness: specializedMethod)
    case .baseProtocol(let requirement, let witness):
      let baseConf = context.getSpecializedConformance(of: witness,
                                                       for: conformance.type,
                                                       substitutions: conformance.specializedSubstitutions)
      specializeWitnessTable(for: baseConf, context, notifyNewWitnessTable)
      return .baseProtocol(requirement: requirement, witness: baseConf)
    case .associatedType(let requirement, let witness):
      let substType = witness.subst(with: conformance.specializedSubstitutions)
      return .associatedType(requirement: requirement, witness: substType)
    case .associatedConformance(let requirement, let assocConf):
      // TODO: once we have the API, replace this with:
      //       let concreteAssociateConf = assocConf.subst(with: conformance.specializedSubstitutions)
      let concreteAssociateConf = conformance.getAssociatedConformance(ofAssociatedType: requirement.rawType,
                                                                       to: assocConf.protocol)
      if concreteAssociateConf.isSpecialized {
        specializeWitnessTable(for: concreteAssociateConf, context, notifyNewWitnessTable)
      }
      return .associatedConformance(requirement: requirement,
                                    witness: concreteAssociateConf)
    }
  }
  let newWT = context.createSpecializedWitnessTable(entries: newEntries,conformance: conformance,
                                                    linkage: .shared, serialized: false)
  notifyNewWitnessTable(newWT)
}

/// Specializes the default methods of a non-generic witness table.
/// Default implementations (in protocol extentions) of non-generic protocol methods have a generic
/// self argument. Specialize such methods with the concrete type. Note that it is important to also
/// specialize inherited conformances so that the concrete self type is correct, even for derived classes.
private func specializeDefaultMethods(for conformance: Conformance,
                                      visited: inout Set<Conformance>,
                                      _ context: ModulePassContext,
                                      _ notifyNewWitnessTable: (WitnessTable) -> ())
{
  // Avoid infinite recursion, which may happen if an associated conformance is the conformance itself.
  guard visited.insert(conformance).inserted,
        let witnessTable = context.lookupWitnessTable(for: conformance.rootConformance)
  else {
    return
  }

  assert(witnessTable.isDefinition, "No witness table available")

  var specialized = false

  let newEntries = witnessTable.entries.map { origEntry in
    switch origEntry {
    case .invalid:
      return WitnessTable.Entry.invalid
    case .method(let requirement, let witness):
      guard let origMethod = witness,
            // Is it a generic method where only self is generic (= a default witness method)?
            origMethod.isGeneric, origMethod.isNonGenericWitnessMethod(context)
      else {
        return origEntry
      }
      // Replace the generic self type with the concrete type.
      let methodSubs = SubstitutionMap(genericSignature: origMethod.genericSignature,
                                       replacementTypes: [conformance.type])

      guard !methodSubs.conformances.contains(where: {!$0.isValid}),
            context.loadFunction(function: origMethod, loadCalleesRecursively: true),
            let specializedMethod = context.specialize(function: origMethod, for: methodSubs) else
      {
        return origEntry
      }
      specialized = true
      return .method(requirement: requirement, witness: specializedMethod)
    case .baseProtocol(_, let witness):
      specializeDefaultMethods(for: witness, visited: &visited, context, notifyNewWitnessTable)
      return origEntry
    case .associatedType:
      return origEntry
    case .associatedConformance(_, let assocConf):
      specializeDefaultMethods(for: assocConf, visited: &visited, context, notifyNewWitnessTable)
      return origEntry
    }
  }
  // If the witness table does not contain any default methods, there is no need to create a
  // specialized witness table.
  if specialized {
    let newWT = context.createSpecializedWitnessTable(entries: newEntries,conformance: conformance,
                                                      linkage: .shared, serialized: false)
    notifyNewWitnessTable(newWT)
  }
}

private extension Function {
  // True, if this is a non-generic method which might have a generic self argument.
  // Default implementations (in protocol extentions) of non-generic protocol methods have a generic
  // self argument.
  func isNonGenericWitnessMethod(_ context: some Context) -> Bool {
    switch loweredFunctionType.invocationGenericSignatureOfFunction.genericParameters.count {
    case 0:
      return true
    case 1:
      return hasGenericSelf(context)
    default:
      return false
    }
  }

  // True, if the self argument is a generic parameter.
  func hasGenericSelf(_ context: some Context) -> Bool {
    let convention = FunctionConvention(for: loweredFunctionType,
                                        hasLoweredAddresses: context.moduleHasLoweredAddresses)
    if convention.hasSelfParameter,
       let selfParam = convention.parameters.last,
       selfParam.type.isGenericTypeParameter
    {
      return true
    }
    return false
  }
}
