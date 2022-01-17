//===--- ReleaseDevirtualizer.swift - Devirtualizes release-instructions --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Devirtualizes release instructions which are known to destruct the object.
///
/// This means, it replaces a sequence of
///    %x = alloc_ref [stack] $X
///      ...
///    strong_release %x
///    dealloc_stack_ref %x
/// with
///    %x = alloc_ref [stack] $X
///      ...
///    set_deallocating %x
///    %d = function_ref @dealloc_of_X
///    %a = apply %d(%x)
///    dealloc_stack_ref %x
///
/// The optimization is only done for stack promoted objects because they are
/// known to have no associated objects (which are not explicitly released
/// in the deinit method).
let releaseDevirtualizerPass = FunctionPass(
  name: "release-devirtualizer", { function, context in
    for block in function.blocks {
      // The last `release_value`` or `strong_release`` instruction before the
      // deallocation.
      var lastRelease: RefCountingInst?

      for instruction in block.instructions {
        if let release = lastRelease {
          // We only do the optimization for stack promoted object, because for
          // these we know that they don't have associated objects, which are
          // _not_ released by the deinit method.
          if let deallocStackRef = instruction as? DeallocStackRefInst {
            tryDevirtualizeReleaseOfObject(context, release, deallocStackRef)
            lastRelease = nil
            continue
          }
        }

        if instruction is ReleaseValueInst || instruction is StrongReleaseInst {
          lastRelease = instruction as? RefCountingInst
        } else if instruction.mayRelease || instruction.mayReadRefCount {
          lastRelease = nil
        }
      }
    }
  }
)

/// Tries to de-virtualize the final release of a stack promoted object.
private func tryDevirtualizeReleaseOfObject(
  _ context: PassContext,
  _ release: RefCountingInst,
  _ deallocStackRef: DeallocStackRefInst
) {
  let allocRefInstruction = deallocStackRef.allocRef
  var root = release.operands[0].value
  while let newRoot = stripRCIdentityPreservingInsts(root) {
    root = newRoot
  }

  if root != allocRefInstruction {
    return
  }

  let type = allocRefInstruction.type

  guard let dealloc = context.getDestructor(ofClass: type) else {
    return
  }

  let builder = Builder(at: release, location: release.location, context)

  var object: Value = allocRefInstruction
  if object.type != type {
    object = builder.createUncheckedRefCast(object: object, type: type)
  }

  // Do what a release would do before calling the deallocator: set the object
  // in deallocating state, which means set the RC_DEALLOCATING_FLAG flag.
  builder.createSetDeallocating(operand: object, isAtomic: release.isAtomic)

  // Create the call to the destructor with the allocated object as self
  // argument.
  let functionRef = builder.createFunctionRef(dealloc)

  let substitutionMap = context.getContextSubstitutionMap(for: type)
  builder.createApply(function: functionRef, substitutionMap, arguments: [object])
  context.erase(instruction: release)
}

private func stripRCIdentityPreservingInsts(_ value: Value) -> Value? {
  switch value {
  // First strip off RC identity preserving casts.
  case let inst as Instruction where inst is UpcastInst ||
    inst is UncheckedRefCastInst ||
    inst is InitExistentialRefInst ||
    inst is OpenExistentialRefInst ||
    inst is RefToBridgeObjectInst ||
    inst is BridgeObjectToRefInst ||
    inst is ConvertFunctionInst ||
    inst is UncheckedEnumDataInst:
    return inst.operands[0].value

  // Then if we have a struct_extract that is extracting a non-trivial member
  // from a struct with no other non-trivial members, a ref count operation on
  // the struct is equivalent to a ref count operation on the extracted
  // member. Strip off the extract.
  case let sei as StructExtractInst where sei.isFieldOnlyNonTrivialField:
    return sei.operands[0].value

  // If we have a struct instruction with only one non-trivial stored field, the
  // only reference count that can be modified is the non-trivial field. Return
  // the non-trivial field.
  case let si as StructInst:
    return si.uniqueNonTrivialOperand

  // If we have an enum instruction with a payload, strip off the enum to
  // expose the enum's payload.
  case let ei as EnumInst where !ei.operands.isEmpty:
    return ei.operands[0].value

  // If we have a tuple_extract that is extracting the only non trivial member
  // of a tuple, a retain_value on the tuple is equivalent to a retain_value on
  // the extracted value.
  case let tei as TupleExtractInst where tei.isEltOnlyNonTrivialElt:
    return tei.operands[0].value

  // If we are forming a tuple and the tuple only has one element with reference
  // semantics, a retain_value on the tuple is equivalent to a retain value on
  // the tuple operand.
  case let ti as TupleInst:
    return ti.uniqueNonTrivialOperand

  default:
    return nil
  }
}

private extension Instruction {
  /// Search the operands of this tuple for a unique non-trivial elt. If we find
  /// it, return it. Otherwise return `nil`.
  var uniqueNonTrivialOperand: Value? {
    var candidateElt: Value?
    let function = self.function

    // For each operand...
    for op in operands.enumerated() {
      // If the operand is not trivial...
      if !op.type.isTrivial(in: function) {
        // And we have not found a `candidateElt` yet, set index to `op` and continue.
        if candidateElt == nil {
          candidateElt = op
          continue
        }

        // Otherwise, we have two values that are non-trivial. Bail.
        return nil
      }
    }

    return candidateElt
  }
}

private extension TupleExtractInst {
  var isEltOnlyNonTrivialElt: Bool {
    let function = self.function
    // If the elt we are extracting is trivial, we cannot be a non-trivial
    // field... return false.
    if type.isTrivial(in: function) {
      return false
    }

    // Ok, we know that the elt we are extracting is non-trivial. Make sure that
    // we have no other non-trivial elts.
    let opTy = operand[0].type
    let fieldNo = self.fieldIndex

    // For each element index of the tuple...
    for (i, eltType) in opType.tupleElements.enumerated() {
      // If the element index is the one we are extracting, skip it...
      if i == fieldNo {
        continue
      }

      // Otherwise check if we have a non-trivial type. If we don't have one,
      // continue.
      if eltType.isTrivial(in: function) {
        continue
      }

      // If we do have a non-trivial type, return false. We have multiple
      // non-trivial types violating our condition.
      return false
    }

    // We checked every other elt of the tuple and did not find any
    // non-trivial elt except for ourselves. Return `true``.
    return true
  }
}
