//===--- SimplifyCopyValue.swift ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

extension CopyValueInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if fromValue.ownership == .none {
      uses.replaceAll(with: fromValue, context)
      context.erase(instruction: self)
      return
    }
    if !context.preserveDebugInfo {
      tryRemoveProjectedCopy(copy: self, context)
    }
  }
}

/// Remove a `copy_value` from a projected owned value which outlives the enclosing owned value.
///
/// ```
///   %2 = begin_borrow %1
///   %3 = struct_extract %2, #S.a
///   %4 = copy_value %3                   // to be removed
///   %5 = some_forwarding_instructions %4
///   end_borrow %2
///   destroy_value %1                     // end of lifetime of the enclosing owned value
///   use %5                               // use of the copied value outside of the owned value's lifetime
/// ```

/// The `destroy_value` is replaced with destructure operations (`destructure_struct`, `destructure_tuple`).
/// The copied value can then be replaced by the corresponding element of the destructure.
/// The remaining elements are element-wise destroyed.
/// Any forwarding instructions of the copied value are moved out of the owned value's liferange.

/// ```
///   %2 = begin_borrow %1
///   end_borrow %2
///   (%3, %4) = destructure_struct %1
///   destroy_value %3                      // destroy the unused elements
///   %5 = some_forwarding_instructions %4  // moved out of %1's liferange
///   use %5
/// ```
///
/// Preconditions:
///   * The `destroy_value` must be in the same basic block as the `copy_value`
///   * There are no uses of the copy or its forwarding instructions inside the owned value's liverange
///
private func tryRemoveProjectedCopy(copy: CopyValueInst, _ context: SimplifyContext) {
  let block = copy.parentBlock

  let (projectionPath, root) = getProjectionPath(of: copy.fromValue)

  guard !projectionPath.isEmpty,
        projectionPath.isMaterializable,
        let beginBorrow = root as? BeginBorrowInst
  else {
    return
  }

  let ownedValue = beginBorrow.borrowedValue

  guard ownedValue.ownership == .owned,
        // Find the destroy_value of the owned value in the same block as copy.
        let destroy = ownedValue.uses.users(ofType: DestroyValueInst.self).first(where: { $0.parentBlock == block })
  else {
    return
  }

  guard checkForwardingChain(from: copy, to: destroy) else {
    return
  }

  // ---- All checks passed. Perform the transformation. ----

  let builder = Builder(before: destroy, context)
  let finalFieldElement = createDestructureChain(of: ownedValue, path: projectionPath, builder)

  moveForwardingChain(from: copy, before: destroy, context)

  copy.replace(with: finalFieldElement, context)

  context.erase(instruction: destroy)
}

/// Returns true if every non-forwarding, non-debug use of `copy` and its forwarding
/// chain is outside the owned value's liverange which ends at `destroy`.
private func checkForwardingChain(from value: Value, to destroy: DestroyValueInst) -> Bool {
  for use in value.uses {
    let user = use.instruction
    if user.parentBlock != destroy.parentBlock || destroy.strictlyDominatesInBlock(user) {
      continue
    }
    guard let fwdInst = user as? (SingleValueInstruction & ForwardingInstruction),
          fwdInst.singleForwardedOperand == use,
          fwdInst.operands.count == 1,
          checkForwardingChain(from: fwdInst, to: destroy)
    else {
      return false
    }
  }
  return true
}

/// Moves every forwarding instruction in the chain starting at `copy` to just
/// before `destroy`.
private func moveForwardingChain(from value: Value,
                                 before destroy: DestroyValueInst,
                                 _ context: SimplifyContext) {
  for use in value.uses {
    let user = use.instruction
    if user.parentBlock != destroy.parentBlock || destroy.strictlyDominatesInBlock(user) {
      continue
    }
    let fwdInst = user as! (SingleValueInstruction & ForwardingInstruction)
    fwdInst.move(before: destroy, context)
    moveForwardingChain(from: fwdInst, before: destroy, context)
  }
}

private func getProjectionPath(of value: Value,
                               initialPath: SmallProjectionPath = SmallProjectionPath()
) -> (SmallProjectionPath, root: Value) {
  switch value {
  case let sei as StructExtractInst:
    let structType = sei.struct.type
    guard structType.getNominalFields(in: sei.parentFunction) != nil,
          (structType.nominal as! StructDecl).valueTypeDestructor == nil
    else {
      return (initialPath, root: sei)
    }
    return getProjectionPath(of: sei.struct, initialPath: initialPath.push(.structField, index: sei.fieldIndex))
  case let tei as TupleExtractInst:
    return getProjectionPath(of: tei.tuple, initialPath: initialPath.push(.tupleField, index: tei.fieldIndex))
  default:
    return (initialPath, root: value)
  }
}

private func createDestructureChain(of value: Value, path: SmallProjectionPath, _ builder: Builder) -> Value {
  let (kind, index, subPath) = path.pop()

  let destructure: MultipleValueInstruction
  switch kind {
  case .root:
    return value
  case .structField:
    destructure = builder.createDestructureStruct(struct: value)
  case .tupleField:
    destructure = builder.createDestructureTuple(tuple: value)
  default:
    fatalError("unsupported projection kind")
  }
  for (i, element) in destructure.results.enumerated() {
    if i != index && element.ownership != .none {
      builder.createDestroyValue(operand: element)
    }
  }
  return createDestructureChain(of: destructure.results[index], path: subPath, builder)
}
