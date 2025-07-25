//===--- Verifier.swift ---------------------------------------------------===//
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
import OptimizerBridging

private protocol VerifiableInstruction : Instruction {
  func verify(_ context: FunctionPassContext)
}

private func require(_ condition: Bool, _ message: @autoclosure () -> String, atInstruction: Instruction? = nil) {
  if !condition {
    let msg = message()
    msg._withBridgedStringRef { stringRef in
      verifierError(stringRef, atInstruction.bridged, Optional<Argument>.none.bridged)
    }
  }
}

extension Function {
  func verify(_ context: FunctionPassContext) {
    for block in blocks {
      for arg in block.arguments {
        arg.verify(context)
      }
      for inst in block.instructions {

        inst.checkForwardingConformance()
        inst.checkGuaranteedResults()

        if let verifiableInst = inst as? VerifiableInstruction {
          verifiableInst.verify(context)
        }
      }
    }
  }
}

private extension Instruction {
  func checkForwardingConformance() {
    if bridged.shouldBeForwarding() {
      require(self is ForwardingInstruction, "instruction \(self)\nshould conform to ForwardingInstruction")
    } else {
      require(!(self is ForwardingInstruction), "instruction \(self)\nshould not conform to ForwardingInstruction")
    }
  }

  func checkGuaranteedResults() {
    for result in results where result.ownership == .guaranteed {
      require(BeginBorrowValue(result) != nil || self is ForwardingInstruction,
              "\(result) must either be a BeginBorrowValue or a ForwardingInstruction")
    }
  }
}

private extension Argument {
  func verify(_ context: FunctionPassContext) {
    if let phi = Phi(self), phi.value.ownership == .guaranteed {

      phi.verifyBorrowedFromUse()

      require(phi.isReborrow == phi.hasBorrowEndingUse ||
              // In a dead-end block an end_borrow might have been deleted.
              // TODO: this check is not needed anymore once we have complete OSSA lifetimes.
              (isReborrow && context.deadEndBlocks.isDeadEnd(parentBlock)),
              "\(self) has stale reborrow flag");
    }
  }

}

private extension Phi {
  func verifyBorrowedFromUse() {
    var forwardingBorrowedFromFound = false
    for use in value.uses {
      require(use.instruction is BorrowedFromInst,
              "guaranteed phi: \(self)\n has non borrowed-from use: \(use)")
      if use.index == 0 {
        require(!forwardingBorrowedFromFound, "phi \(self) has multiple forwarding borrowed-from uses")
        forwardingBorrowedFromFound = true
      }
    }
    require(forwardingBorrowedFromFound,
            "missing forwarding borrowed-from user of guaranteed phi \(self)")
  }
}

extension BorrowedFromInst : VerifiableInstruction {
  func verify(_ context: FunctionPassContext) {

    for ev in enclosingValues {
      require(ev.isValidEnclosingValueInBorrowedFrom, "invalid enclosing value in borrowed-from: \(ev)")
    }

    var computedEVs = Stack<Value>(context)
    defer { computedEVs.deinitialize() }

    guard let phi = Phi(borrowedValue) else {
      fatalError("borrowed value of borrowed-from must be a phi: \(self)")
    }
    gatherEnclosingValuesFromPredecessors(for: phi, in: &computedEVs, context)

    var existingEVs = ValueSet(insertContentsOf: enclosingValues, context)
    defer { existingEVs.deinitialize() }

    for computedEV in computedEVs {
      require(existingEVs.contains(computedEV),
                   "\(computedEV)\n  missing in enclosing values of \(self)")
    }
  }
}

private extension Value {
  var isValidEnclosingValueInBorrowedFrom: Bool {
    switch ownership {
    case .owned:
      return true
    case .guaranteed:
      return BeginBorrowValue(self) != nil ||
             self is BorrowedFromInst ||
             forwardingInstruction != nil
    case .none, .unowned:
      return false
    }
  }
}

extension LoadBorrowInst : VerifiableInstruction {
  func verify(_ context: FunctionPassContext) {
    if isUnchecked {
      return
    }

    var mutatingInstructions = MutatingUsesWalker(context)
    defer { mutatingInstructions.deinitialize() }

    mutatingInstructions.findMutatingUses(of: self.address)
    mutatingInstructions.verifyNoMutatingUsesInLiverange(of: self)
  }
}

extension VectorBaseAddrInst : VerifiableInstruction {
  func verify(_ context: FunctionPassContext) {
    require(vector.type.isBuiltinFixedArray,
            "vector operand of vector_element_addr must be a Builtin.FixedArray")
    require(type == vector.type.builtinFixedArrayElementType(in: parentFunction,
                                                             maximallyAbstracted: true).addressType,
            "result of vector_element_addr has wrong type")
  }
}

// Used to check if any instruction is mutating the memory location within the liverange of a `load_borrow`.
// Note that it is not checking if an instruction _may_ mutate the memory, but it's checking if any instruction
// _definitely_ will mutate the memory.
// Otherwise the risk would be too big for false alarms. It also means that this verification is not perfect and
// might miss some subtle violations.
private struct MutatingUsesWalker : AddressDefUseWalker {
  let context: FunctionPassContext
  var mutatingInstructions: InstructionSet

  init(_ context: FunctionPassContext) {
    self.context = context
    self.mutatingInstructions = InstructionSet(context)
  }

  mutating func deinitialize() {
    self.mutatingInstructions.deinitialize()
  }

  mutating func findMutatingUses(of address: Value) {
    _ = walkDownUses(ofAddress: address, path: UnusedWalkingPath())
  }

  mutating func verifyNoMutatingUsesInLiverange(of load: LoadBorrowInst) {
    // The liverange of a `load_borrow` can end in a branch instruction. In such cases we handle the re-borrow liveranges
    // (starting at the `borrowed_from` in the successor block) separately.
    // This worklist contains the starts of the individual linear liveranges,
    // i.e. `load_borrow` and `borrowed_from` instructions.
    var linearLiveranges = SpecificInstructionWorklist<SingleValueInstruction>(context)
    defer { linearLiveranges.deinitialize() }

    linearLiveranges.pushIfNotVisited(load)
    while let startInst = linearLiveranges.pop() {
      verifyNoMutatingUsesInLinearLiverange(of: startInst)

      for use in startInst.uses {
        if let phi = Phi(using: use) {
          if let bf = phi.borrowedFrom {
            linearLiveranges.pushIfNotVisited(bf)
          } else {
            require(false, "missing borrowed-from for \(phi.value)")
          }
        }
      }
    }
  }

  private mutating func verifyNoMutatingUsesInLinearLiverange(of startInst: SingleValueInstruction) {
    assert(startInst is LoadBorrowInst || startInst is BorrowedFromInst)

    var instWorklist = InstructionWorklist(context)
    defer { instWorklist.deinitialize() }

    // It would be good enough to only consider `end_borrow`s (and branches), but adding all users doesn't harm.
    for use in startInst.uses {
      instWorklist.pushPredecessors(of: use.instruction, ignoring: startInst)
    }

    while let inst = instWorklist.pop() {
      require(!mutatingInstructions.contains(inst),
              "Load borrow invalidated by a local write", atInstruction: inst)
      instWorklist.pushPredecessors(of: inst, ignoring: startInst)
    }
  }

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    if address.isMutatedAddress {
      mutatingInstructions.insert(address.instruction)
    }
    return .continueWalk
  }
}

private extension Operand {
  // Returns true if the operand's instruction is definitely writing to the operand's address value.
  var isMutatedAddress: Bool {
    assert(value.type.isAddress)
    switch instruction {
    case let store as StoringInstruction:
      return self == store.destinationOperand
    case let copy as SourceDestAddrInstruction:
      if self == copy.destinationOperand {
        return true
      } else if self == copy.sourceOperand && copy.isTakeOfSource {
        return true
      }
      return false
    case let load as LoadInst:
      return load.loadOwnership == .take
    case let partialApply as PartialApplyInst:
      if !partialApply.isOnStack,
         let convention = partialApply.convention(of: self)
      {
        switch convention {
        case .indirectIn, .indirectInGuaranteed:
          // Such operands are consumed by the `partial_apply` and therefore cound as "written".
          return true
        default:
          return false
        }
      }
      return false
    case is DestroyAddrInst, is IsUniqueInst:
      return true
    default:
      return false
    }
  }
}

func registerVerifier() {
  BridgedUtilities.registerVerifier(
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      bridgedFunction.function.verify(context)
    }
  )
}
