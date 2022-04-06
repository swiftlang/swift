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

        switch instruction {
          case is ReleaseValueInst, is StrongReleaseInst:
            lastRelease = instruction as? RefCountingInst
          case is DeallocRefInst, is SetDeallocatingInst:
            lastRelease = nil
          default:
            if instruction.mayRelease {
              lastRelease = nil
            }
        }
      }
    }
  }
)

/// Tries to de-virtualize the final release of a stack-promoted object.
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

  guard let dealloc = context.calleeAnalysis.getDestructor(ofExactType: type) else {
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
  guard let inst = value as? Instruction else { return nil }

  switch inst {
  // First strip off RC identity preserving casts.
  case is UpcastInst,
       is UncheckedRefCastInst,
       is InitExistentialRefInst,
       is OpenExistentialRefInst,
       is RefToBridgeObjectInst,
       is BridgeObjectToRefInst,
       is ConvertFunctionInst,
       is UncheckedEnumDataInst:
    return inst.operands[0].value

  // Then if we have a struct_extract that is extracting a non-trivial member
  // from a struct with no other non-trivial members, a ref count operation on
  // the struct is equivalent to a ref count operation on the extracted
  // member. Strip off the extract.
  case let sei as StructExtractInst where sei.isFieldOnlyNonTrivialField:
    return sei.operand

  // If we have a struct or tuple instruction with only one non-trivial operand, the
  // only reference count that can be modified is the non-trivial operand. Return
  // the non-trivial operand.
  case is StructInst, is TupleInst:
    return inst.uniqueNonTrivialOperand

  // If we have an enum instruction with a payload, strip off the enum to
  // expose the enum's payload.
  case let ei as EnumInst where !ei.operands.isEmpty:
    return ei.operand

  // If we have a tuple_extract that is extracting the only non trivial member
  // of a tuple, a retain_value on the tuple is equivalent to a retain_value on
  // the extracted value.
  case let tei as TupleExtractInst where tei.isEltOnlyNonTrivialElt:
    return tei.operand

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

    for op in operands {
      if !op.value.type.isTrivial(in: function) {
        if candidateElt == nil {
          candidateElt = op.value
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

    if type.isTrivial(in: function) {
      return false
    }

    let opType = operand.type

    var nonTrivialEltsCount = 0
    for elt in opType.tupleElements {
      if elt.isTrivial(in: function) {
        nonTrivialEltsCount += 1
      }

      if nonTrivialEltsCount > 1 {
        return false
      }
    }

    return true
  }
}

private extension StructExtractInst {
  var isFieldOnlyNonTrivialField: Bool {
    let function = self.function

    if type.isTrivial(in: function) {
      return false
    }

    let structType = operand.type

    var nonTrivialFieldsCount = 0
    for field in structType.getNominalFields(in: function) {
      if field.isTrivial(in: function) {
        nonTrivialFieldsCount += 1
      }

      if nonTrivialFieldsCount > 1 {
        return false
      }
    }

    return true
  }
}
