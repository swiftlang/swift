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
      if source.type == destination.type {
        replaceSuccess(context)
      }
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
      builder.createCopyAddr(from: source, to: destination, takeSource: true, initializeDest: true)
    case .CopyOnSuccess:
      builder.createCopyAddr(from: source, to: destination, takeSource: false, initializeDest: true)
    }
    builder.createBranch(to: successBlock)
    context.erase(instruction: self)
  }

  func replaceFailure(_ context: SimplifyContext) {
    let builder = Builder(before: self, context)
    switch consumptionKind {
    case .TakeAlways:
      builder.createDestroyAddr(address: source)
    case .CopyOnSuccess, .TakeOnSuccess:
      break
    }
    builder.createBranch(to: failureBlock)
    context.erase(instruction: self)
  }
}

extension UnconditionalCheckedCastInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    tryOptimizeCastToExistentialMetatype(context)
  }
}

private extension UnconditionalCheckedCastInst {
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
    
    let instanceTy = targetFormalType.instanceTypeOfMetatype
    guard let nominal = instanceTy.nominal,
          let proto = nominal as? ProtocolDecl
    else {
      return
    }
    let conformance = sourceFormalType.instanceTypeOfMetatype.checkConformance(to: proto)
    guard conformance.isValid,
          conformance.matchesActorIsolation(in: parentFunction)
    else {
      return
    }
    
    let builder = Builder(before: self, context)
    let iemt = builder.createInitExistentialMetatype(metatype: operand.value, existentialType: self.type, conformances: [conformance])
    self.replace(with: iemt, context)
  }   
}
