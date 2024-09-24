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

import SIL

@discardableResult
func specializeVTable(forClassType classType: Type,
                      errorLocation: Location,
                      _ context: ModulePassContext) -> VTable?
{
  if !classType.isClass {
    return nil
  }
  if !classType.isGenericAtAnyLevel {
    return nil
  }
  let classDecl = classType.nominal

  if context.lookupSpecializedVTable(for: classType) != nil {
    return nil
  }

  guard let origVTable = context.lookupVTable(for: classDecl) else {
    context.diagnosticEngine.diagnose(errorLocation.sourceLoc, .cannot_specialize_class, classType)
    return nil
  }

  let classContextSubs = classType.contextSubstitutionMap

  let newEntries = origVTable.entries.map { origEntry in
    if !origEntry.implementation.isGeneric {
      return origEntry
    }
    let methodSubs = classContextSubs.getMethodSubstitutions(for: origEntry.implementation)

    guard !methodSubs.conformances.contains(where: {!$0.isValid}),
          let specializedMethod = context.specialize(function: origEntry.implementation, for: methodSubs) else
    {
      context.diagnosticEngine.diagnose(origEntry.methodDecl.location.sourceLoc, .non_final_generic_class_function)
      return origEntry
    }

    context.deserializeAllCallees(of: specializedMethod, mode: .allFunctions)
    specializedMethod.set(linkage: .public, context)
    specializedMethod.set(isSerialized: false, context)

    return VTable.Entry(kind: origEntry.kind, isNonOverridden: origEntry.isNonOverridden,
                        methodDecl: origEntry.methodDecl, implementation: specializedMethod)
  }

  let specializedVTable = context.createSpecializedVTable(entries: newEntries, for: classType, isSerialized: false)
  if let superClassTy = classType.superClassType {
    specializeVTable(forClassType: superClassTy, errorLocation: classDecl.location, context)
  }
  return specializedVTable
}

func specializeVTablesOfSuperclasses(_ moduleContext: ModulePassContext) {
  for vtable in moduleContext.vTables {
    if !vtable.isSpecialized,
       !vtable.class.isGenericAtAnyLevel,
       let superClassTy = vtable.class.superClassType,
       superClassTy.nominal.isGenericAtAnyLevel
    {
      specializeVTable(forClassType: superClassTy, errorLocation: vtable.class.location, moduleContext)
    }
  }
}
