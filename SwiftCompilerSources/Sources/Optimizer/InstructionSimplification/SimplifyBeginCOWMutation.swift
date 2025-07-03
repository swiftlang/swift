//===--- SimplifyBeginCOWMutation.swift - Simplify begin_cow_mutation -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension BeginCOWMutationInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {

    /// The buffer of an empty Array/Set/Dictionary singleton is known to be not
    /// unique. Replace the uniqueness result of such a
    /// `begin_cow_mutation` with a zero `integer_literal`, e.g.
    ///
    ///   %3 = global_addr @_swiftEmptyArrayStorage
    ///   %4 = address_to_pointer %3
    ///   %5 = raw_pointer_to_ref %4
    ///   %6 = unchecked_ref_cast %5
    ///   (%u, %b) = begin_cow_mutation %6
    /// ->
    ///   [...]
    ///   (%not_used, %b) = begin_cow_mutation %6
    ///   %u = integer_literal $Builtin.Int1, 0
    ///
    optimizeEmptySingleton(context)

    /// If the only use of the `begin_cow_instruction` is an `end_cow_instruction`,
    /// remove the pair, e.g.
    ///
    ///   (%u, %b) = begin_cow_mutation %0 : $Buffer
    ///   %e = end_cow_mutation %b : $Buffer
    ///
    if optimizeEmptyBeginEndPair(context) {
      return
    }

    /// If the operand of the `begin_cow_instruction` is an `end_cow_instruction`,
    /// which has no other uses, remove the pair, e.g.
    ///
    ///   %e = end_cow_mutation %0 : $Buffer
    ///   (%u, %b) = begin_cow_mutation %e : $Buffer
    ///
    if optimizeEmptyEndBeginPair(context) {
      return
    }
  }
}

private extension BeginCOWMutationInst {

  func optimizeEmptySingleton(_ context: SimplifyContext) {
    if !isEmptyCOWSingleton(instance) {
      return
    }
    if uniquenessResult.uses.ignoreDebugUses.isEmpty {
      /// Don't create an integer_literal which would be dead. This would result
      /// in an infinite loop in SILCombine.
      return
    }
    let builder = Builder(before: self, location: location, context)
    let zero = builder.createIntegerLiteral(0, type: uniquenessResult.type);
    uniquenessResult.uses.replaceAll(with: zero, context)
  }

  func optimizeEmptyBeginEndPair(_ context: SimplifyContext) -> Bool {
    if !uniquenessResult.uses.ignoreDebugUses.isEmpty {
      return false
    }
    let buffer = instanceResult
    guard buffer.uses.ignoreDebugUses.allSatisfy({
        if let endCOW = $0.instruction as? EndCOWMutationInst {
          return !endCOW.doKeepUnique
        }
        return false
      }) else
    {
      return false
    }

    for use in buffer.uses.ignoreDebugUses {
      let endCOW = use.instruction as! EndCOWMutationInst
      endCOW.replace(with: instance, context)
    }
    context.erase(instructionIncludingDebugUses: self)
    return true
  }

  func optimizeEmptyEndBeginPair(_ context: SimplifyContext) -> Bool {
    if !uniquenessResult.uses.ignoreDebugUses.isEmpty {
      return false
    }
    guard let endCOW = instance as? EndCOWMutationInst,
          !endCOW.doKeepUnique else {
      return false
    }
    if endCOW.uses.ignoreDebugUses.contains(where: { $0.instruction != self }) {
      return false
    }

    instanceResult.uses.replaceAll(with: endCOW.instance, context)
    context.erase(instructionIncludingDebugUses: self)
    context.erase(instructionIncludingDebugUses: endCOW)
    return true
  }
}

private func isEmptyCOWSingleton(_ value: Value) -> Bool {
  var v = value
  while true {
    switch v {
      case is UncheckedRefCastInst,
           is UpcastInst,
           is RawPointerToRefInst,
           is AddressToPointerInst,
           is CopyValueInst:
        v = (v as! UnaryInstruction).operand.value
      case let globalAddr as GlobalAddrInst:
        let name = globalAddr.global.name
        return name == StringRef("_swiftEmptyArrayStorage") ||
               name == StringRef("_swiftEmptyDictionarySingleton") ||
               name == StringRef("_swiftEmptySetSingleton")
      default:
        return false
    }
  }
}
