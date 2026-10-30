//===--- SimplifyCheckedCast.swift ----------------------------------------===//
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
import AST

extension CheckedCastAddrBranchInst : OnoneSimplifiable {
  func simplify(_ context: SimplifyContext) {
    guard let castWillSucceed = self.dynamicCastResult else {
      return
    }
    if castWillSucceed {
      // TODO: handle cases where the operand address types are different.
      // A test_only cast has no destination, so there is no type to match.
      if let destination, source.type != destination.type {
        return
      }
      replaceSuccess(context)
    } else {
      replaceFailure(context)
    }
  }
}

private extension CheckedCastAddrBranchInst {
  func replaceSuccess(_ context: SimplifyContext) {
    let builder = Builder(before: self, context)
    switch consumptionKind {
    case .TakeAlways, .TakeOnSuccess:
      builder.createCopyAddr(from: source, to: destination!, takeSource: true, initializeDest: true)
    case .CopyOnSuccess:
      builder.createCopyAddr(from: source, to: destination!, takeSource: false, initializeDest: true)
    case .TestOnly:
      // Reports success without producing a value: there is no destination
      // and nothing to move or copy.
      break
    }
    builder.createBranch(to: successBlock)
    context.erase(instruction: self)
  }

  func replaceFailure(_ context: SimplifyContext) {
    let builder = Builder(before: self, context)
    switch consumptionKind {
    case .TakeAlways:
      builder.createDestroyAddr(address: source)
    case .CopyOnSuccess, .TakeOnSuccess, .TestOnly:
      break
    }
    builder.createBranch(to: failureBlock)
    context.erase(instruction: self)
  }
}

extension UnconditionalCheckedCastInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if tryOptimizeCastToClassExistential(context) {
      return
    }
    tryOptimizeCastToExistentialMetatype(context)
  }
}

private extension UnconditionalCheckedCastInst {
  // Replace
  //     %1 = unconditional_checked_cast %0 : $C<Int> to any P
  // with
  //     %1 = init_existential_ref %0 : $C<Int> : $C<Int>, $any P
  // if the concrete class type C<Int> conforms to the class-bound protocol P.
  // Unlike the cast, init_existential_ref references the conformance statically, which lets the
  // witness table specialization create a specialized witness table for it.
  func tryOptimizeCastToClassExistential(_ context: SimplifyContext) -> Bool {
    let sourceType = sourceFormalType
    let targetType = targetFormalType
    guard targetType.isClassExistential,
          !sourceType.isExistential,
          sourceType.isClass,
          !sourceType.hasArchetype,
          !sourceType.hasTypeParameter,
          let proto = targetType.nominal as? ProtocolDecl,
          operand.value.type.isClass
    else {
      return false
    }
    let conformance = sourceType.checkConformance(to: proto)
    guard conformance.isValid,
          conformance.matchesActorIsolation(in: parentFunction)
    else {
      return false
    }
    let builder = Builder(before: self, context)
    let conformances = sourceType.collectExistentialConformances(existentialType: targetType)
    let iere = builder.createInitExistentialRef(instance: operand.value, existentialType: self.type,
                                                formalConcreteType: sourceType, conformances: conformances)
    self.replace(with: iere, context)
    return true
  }

  // Replace
  //     %1 = unconditional_checked_cast %0 : $@thick T.Type to any P.Type
  // with
  //     %1 = init_existential_metatype %0 : $@thick S.Type, $@thick any P.Type
  // if type T conforms to protocol P.
  // Note that init_existential_metatype is better than unconditional_checked_cast because it does not need
  // to do any runtime casting.
  func tryOptimizeCastToExistentialMetatype(_ context: SimplifyContext) {
    guard targetFormalType.isExistentialMetatype, sourceFormalType.isMetatype else {
      return
    }
    
    let targetInstanceType = targetFormalType.instanceTypeOfMetatype
    let sourceInstanceType = sourceFormalType.instanceTypeOfMetatype

    guard let nominal = targetInstanceType.nominal,
          let proto = nominal as? ProtocolDecl
    else {
      return
    }

    let conformance = sourceInstanceType.checkConformance(to: proto)
    guard conformance.isValid,
          conformance.matchesActorIsolation(in: parentFunction)
    else {
      return
    }
    
    let builder = Builder(before: self, context)
    let conformances = sourceInstanceType.collectExistentialConformances(existentialType: targetInstanceType)
    let iemt = builder.createInitExistentialMetatype(metatype: operand.value, existentialType: self.type, conformances: conformances)
    self.replace(with: iemt, context)
  }   
}
