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
  let type = allocRefInstruction.type
  let calleeAnalysis = context.calleeAnalysis
  
  guard let dealloc = calleeAnalysis.getDestructor(ofExactType: type) else {
    return
  }

  guard let uniqueReference = getUniqueReference(of: release.operand) else {
    return
  }

  var escapeInfo = EscapeInfo(calleeAnalysis: calleeAnalysis)
  var found = false
  if escapeInfo.isEscaping(object: uniqueReference,
      visitUse: { op, path, _ in
        if op.value === allocRefInstruction {
          if !path.isEmpty { return .markEscaping }
          found = true
          return .ignore
        }
        if op.value is Allocation { return .markEscaping }
        return .continueWalking
      }) {
    return
  }
  assert(found, "value must come from an allocation")

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

private func getUniqueReference(of value: Value) -> Value? {
  let function = value.function
  var val = value
  while true {
    if val.type.isClass {
      return val
    }
    switch val {
      case let ei as EnumInst where !ei.operands.isEmpty:
        val = ei.operand
      case is StructInst, is TupleInst:
        var uniqueNonTrivialOperand: Value?

        for op in (val as! SingleValueInstruction).operands {
          if !op.value.type.isTrivial(in: function) {
            if let _ = uniqueNonTrivialOperand {
              return nil
            }
            uniqueNonTrivialOperand = op.value
          }
        }
        guard let uniqueOp = uniqueNonTrivialOperand  else { return nil }
        val = uniqueOp
      default:
        return nil
    }
  }
}
