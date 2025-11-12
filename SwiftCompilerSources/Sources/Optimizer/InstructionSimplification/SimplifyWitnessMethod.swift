//===--- SimplifyWitnessMethod.swift --------------------------------------===//
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

extension WitnessMethodInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    _ = tryReplaceExistentialArchetype(of: self, context)
  }
}

/// If the witness_method operates on an existential archetype (`@opened("...")`) and the concrete
/// type is known, replace the existential archetype with the concrete type.
/// For example:
/// ```
///   %3 = witness_method $@opened("...", P) Self, #P.foo, %2
/// ```
/// ->
/// ```
///   %3 = witness_method $ConcreteType, #P.foo, %2
/// ```
private func tryReplaceExistentialArchetype(of witnessMethod: WitnessMethodInst, _ context: SimplifyContext) -> Bool {
  guard let concreteType = witnessMethod.concreteTypeOfDependentExistentialArchetype else {
    return false
  }
  let conf = concreteType.checkConformance(to: witnessMethod.lookupProtocol)
  guard conf.isValid else {
    return false
  }

  let builder = Builder(before: witnessMethod, context)
  let newWmi = builder.createWitnessMethod(lookupType: concreteType,
                                           conformance: conf,
                                           member: witnessMethod.member,
                                           methodType: witnessMethod.type)
  witnessMethod.replace(with: newWmi, context)
  return true
}
