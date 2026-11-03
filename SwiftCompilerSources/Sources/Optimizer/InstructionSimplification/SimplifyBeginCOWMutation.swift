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
    let falseLiteral = builder.createBoolLiteral(false)
    uniquenessResult.uses.replaceAll(with: falseLiteral, context)
  }

  func optimizeEmptyBeginEndPair(_ context: SimplifyContext) -> Bool {
    if !uniquenessResult.uses.ignoreDebugUses.isEmpty {
      return false
    }
    let buffer = instanceResult
    var needKeepUnique = false
    for user in buffer.uses.ignoreDebugUses.users {
      if let endCOW = user as? EndCOWMutationInst {
        needKeepUnique = needKeepUnique || endCOW.doKeepUnique
      } else {
        return false
      }
    }
    if needKeepUnique {
      // The removed end_cow_mutations rely on the buffer being unique. Therefore the
      // keep_unique flag must be transferred to the end_cow_mutations which define the
      // operand of this instruction.
      // First check if all of them can be found, before modifying anything.
      guard visitEndCowMutations(of: instance, context, { _ in true }) else {
        return false
      }

      _ = visitEndCowMutations(of: instance, context) { ecm in
        ecm.set(keepUnique: true, context)
        return true
      }
    }

    for use in buffer.uses.ignoreDebugUses {
      let endCOW = use.instruction as! EndCOWMutationInst
      endCOW.replace(with: instance, context)
    }
    context.erase(instruction: self)
    return true
  }

  func optimizeEmptyEndBeginPair(_ context: SimplifyContext) -> Bool {
    if !uniquenessResult.uses.ignoreDebugUses.isEmpty {
      return false
    }
    // The end_cow_mutation instructions are removed below. Therefore all values in the
    // def-use chain must not have any other uses, which could rely on the buffer being
    // immutable, e.g. `ref_element_addr [immutable]`.
    guard visitEndCowMutations(of: instance, singleUseChain: true, context, { ecm in
      !ecm.doKeepUnique
    }) else {
      return false
    }

    _ = visitEndCowMutations(of: instance, context) { ecm in
      ecm.replace(with: ecm.instance, context)
      return true
    }

    instanceResult.uses.replaceAll(with: instance, context)
    context.erase(instruction: self)
    return true
  }
}

/// Calls `visit` for all `end_cow_mutation` instructions which define `initialValue`,
/// either directly or via phi arguments.
/// Returns false if `initialValue` is not exclusively defined by `end_cow_mutation`
/// instructions or if `visit` returns false for one of them.
/// If `singleUseChain` is true, all values in the def-use chain must have a single
/// (non-debug) use - which is the chain's use itself.
private func visitEndCowMutations(of initialValue: Value,
                                  singleUseChain: Bool = false,
                                  _ context: SimplifyContext,
                                  _ visit: (EndCOWMutationInst) -> Bool
) -> Bool {
  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushIfNotVisited(initialValue)
  while let value = worklist.pop() {
    if singleUseChain && !value.uses.ignoreDebugUses.isSingleUse {
      return false
    }
    if let ecm = value as? EndCOWMutationInst {
      guard visit(ecm) else {
        return false
      }
    } else if let phi = Phi(value) {
      worklist.pushIfNotVisited(contentsOf: phi.incomingValues)
    } else {
      return false
    }
  }
  return true
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
        return name.isEmptyCollectionSingleton
      default:
        return false
    }
  }
}

extension StringRef {
  /// Whether this is the name of one of the empty collection singletons.
  ///
  /// For historic reasons, we check both the C name and the mangled Swift
  /// name (for an @extern(c) declaration).
  var isEmptyCollectionSingleton: Bool {
    switch self {
    case "_swiftEmptyArrayStorage",
         "_swiftEmptyDictionarySingleton",
         "_swiftEmptySetSingleton",
         "$ss23_swiftEmptyArrayStorageSo06_SwiftbcD0Vvp",
         "$ss30_swiftEmptyDictionarySingletonSo06_SwiftbcD0Vvp",
         "$ss23_swiftEmptySetSingletonSo06_SwiftbcD0Vvp":
      return true
    default:
      return false
    }
  }
}
