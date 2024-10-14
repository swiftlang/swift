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
      context.diagnosticEngine.diagnose(errorLocation.sourceLoc, .cannot_specialize_class, classType)
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
    return vTable.entries.compactMap { entry in
      if !entry.implementation.isGeneric {
        return entry
      }
      let baseType = baseTypesOfMethods[entry.implementation]!
      let classContextSubs = baseType.contextSubstitutionMap
      let methodSubs = classContextSubs.getMethodSubstitutions(for: entry.implementation)

      guard !methodSubs.conformances.contains(where: {!$0.isValid}),
            let specializedMethod = context.specialize(function: entry.implementation, for: methodSubs) else
      {
        context.diagnosticEngine.diagnose(entry.methodDecl.location.sourceLoc, .non_final_generic_class_function)
        return nil
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

func specializeWitnessTable(forConformance conformance: Conformance,
                            errorLocation: Location,
                            _ context: ModulePassContext,
                            _ notifyNewWitnessTable: (WitnessTable) -> ())
{
  let genericConformance = conformance.genericConformance
  guard let witnessTable = context.lookupWitnessTable(for: genericConformance) else {
    fatalError("no witness table found")
  }
  assert(witnessTable.isDefinition, "No witness table available")

  let newEntries = witnessTable.entries.map { origEntry in
    switch origEntry {
    case .invalid:
      return WitnessTable.Entry.invalid
    case .method(let requirement, let witness):
      guard let origMethod = witness else {
        return origEntry
      }
      let methodSubs = conformance.specializedSubstitutions.getMethodSubstitutions(for: origMethod)

      guard !methodSubs.conformances.contains(where: {!$0.isValid}),
            let specializedMethod = context.specialize(function: origMethod, for: methodSubs) else
      {
        context.diagnosticEngine.diagnose(errorLocation.sourceLoc, .cannot_specialize_witness_method, requirement)
        return origEntry
      }
      return .method(requirement: requirement, witness: specializedMethod)
    case .baseProtocol(let requirement, let witness):
      let baseConf = context.getSpecializedConformance(of: witness,
                                                       for: conformance.type,
                                                       substitutions: conformance.specializedSubstitutions)
      specializeWitnessTable(forConformance: baseConf, errorLocation: errorLocation, context, notifyNewWitnessTable)
      return .baseProtocol(requirement: requirement, witness: baseConf)
    case .associatedType(let requirement, let witness):
      let substType = witness.subst(with: conformance.specializedSubstitutions)
      return .associatedType(requirement: requirement, witness: substType)
    case .associatedConformance(let requirement, let proto, let witness):
      if witness.isSpecialized {
        specializeWitnessTable(forConformance: witness, errorLocation: errorLocation, context, notifyNewWitnessTable)
      }
      return .associatedConformance(requirement: requirement, protocol: proto, witness: witness)
    }
  }
  let newWT = context.createWitnessTable(entries: newEntries,conformance: conformance,
                                         linkage: .shared, serialized: false)
  notifyNewWitnessTable(newWT)
}
