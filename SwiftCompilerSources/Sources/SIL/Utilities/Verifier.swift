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

import SILBridging

/// To add verification for a specific instruction, let the instruction class conform to
/// this protocol and implement the `verify` method.
private protocol VerifiableInstruction : Instruction {
  func verify(_ context: VerifierContext)
}

private func require(_ condition: Bool, _ message: @autoclosure () -> String, atInstruction: Instruction) {
  if !condition {
    let msg = message()
    msg._withBridgedStringRef { stringRef in
      BridgedVerifier.verifierError(stringRef, atInstruction.bridged)
    }
  }
}

private func require(_ condition: Bool, _ message: @autoclosure () -> String, atArgument: Argument) {
  if !condition {
    let msg = message()
    msg._withBridgedStringRef { stringRef in
      BridgedVerifier.verifierError(stringRef, atArgument.bridged)
    }
  }
}

struct VerifierContext: Context {
  let _bridged: BridgedContext
}

extension Function {
  func verify(_ context: VerifierContext) {
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

    if hasOwnership, isDefinition {
      verifyNoUnreachableBlocks(context)
      verifyNoInfiniteLoops(context)
    }
  }

  private func verifyNoUnreachableBlocks(_ context: VerifierContext) {
    var reachableBlocks = BasicBlockWorklist(context)
    defer { reachableBlocks.deinitialize() }
    reachableBlocks.transitivelyAddBlockWithSuccessors(startingAt: entryBlock)

    for block in blocks {
      require(reachableBlocks.hasBeenPushed(block),
              "block is unreachable",
              atInstruction: block.instructions.first!)
    }
  }

  private func verifyNoInfiniteLoops(_ context: VerifierContext) {
    var noInfiniteLoops = BasicBlockWorklist(context)
    defer { noInfiniteLoops.deinitialize() }

    for block in blocks {
      if block.successors.isEmpty {
        noInfiniteLoops.transitivelyAddBlockWithPredecessors(startingAt: block)
      }
    }

    for block in blocks {
      require(noInfiniteLoops.hasBeenPushed(block),
              "function has infinite loop",
              atInstruction: block.instructions.first!)
    }
  }
}

private extension Instruction {
  func checkForwardingConformance() {
    if bridged.shouldBeForwarding() {
      require(self is ForwardingInstruction,
              "instruction \(self)\nshould conform to ForwardingInstruction",
              atInstruction: self)
    } else {
      require(!(self is ForwardingInstruction),
              "instruction \(self)\nshould not conform to ForwardingInstruction",
              atInstruction: self)
    }
  }

  func checkGuaranteedResults() {
    for result in results where result.ownership == .guaranteed {
      require(BeginBorrowValue(result) != nil || self is ForwardingInstruction || result.isGuaranteedApplyResult,
              "\(result) must either be a BeginBorrowValue or a ForwardingInstruction",
              atInstruction: self)
    }
  }
}

private extension Argument {
  func verify(_ context: VerifierContext) {
    if let phi = Phi(self), phi.value.ownership == .guaranteed {

      phi.verifyBorrowedFromUse()

      require(phi.isReborrow == phi.hasBorrowEndingUse,
              "\(self) has stale reborrow flag", atArgument: self);
    }
  }

}

private extension Phi {
  func verifyBorrowedFromUse() {
    var forwardingBorrowedFromFound = false
    for use in value.uses {
      require(use.instruction is BorrowedFromInst,
              "guaranteed phi: \(self)\n has non borrowed-from use: \(use)",
              atArgument: self.value)
      if use.index == 0 {
        require(!forwardingBorrowedFromFound, "phi \(self) has multiple forwarding borrowed-from uses",
                atArgument: self.value)
        forwardingBorrowedFromFound = true
      }
    }
    require(forwardingBorrowedFromFound,
            "missing forwarding borrowed-from user of guaranteed phi \(self)",
            atArgument: self.value)
  }
}

extension BorrowedFromInst : VerifiableInstruction {
  func verify(_ context: VerifierContext) {

    for ev in enclosingValues {
      require(ev.isValidEnclosingValueInBorrowedFrom,
              "invalid enclosing value in borrowed-from: \(ev)",
              atInstruction: self)
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
              "\(computedEV)\n  missing in enclosing values of \(self)",
              atInstruction: self)
    }
  }
}

private extension Value {
  var isValidEnclosingValueInBorrowedFrom: Bool {
    switch ownership {
    case .owned:
      return true
    case .none:
      // "none" is like "owned" and can happen e.g. if a non-trivial enum is constructed with a trivial case:
      // ```
      //   %1 = enum $Optional<AnyObject>, #Optional.none!enumelt   // ownership: none
      //   ...
      //   %3 = borrowed %phi from (%1)
      // ```
      return true
    case .guaranteed:
      return BeginBorrowValue(self) != nil ||
             self is BorrowedFromInst ||
             forwardingInstruction != nil
    case .unowned:
      return false
    }
  }
}

extension LoadBorrowInst : VerifiableInstruction {
  func verify(_ context: VerifierContext) {
    if isUnchecked {
      return
    }

    var mutatingInstructions = MutatingUsesWalker(context)
    defer { mutatingInstructions.deinitialize() }

    mutatingInstructions.findMutatingUses(of: self.address)
    mutatingInstructions.verifyNoMutatingUsesInLiverange(of: self)
  }
}

extension BeginAccessInst : VerifiableInstruction {
  func verify(_ context: VerifierContext) {
    // Catch violations like
    // ```
    //   %1 = begin_access [read] %0
    //   store %2 to %0
    //   end_access %1
    // ```

    guard context.silStage == .canonical else {
      // Mandatory passes on raw SIL need to be completed until we can verify this.
      // Also, LoadableByAddress in lowered SIL can insert `copy_addr`s inside read-only access scope.
      // Therefore we can only run this verification in canonical SIL.
      return
    }

    if enforcement == .static {
      // This is a workaround for a bug in the move-only checker: rdar://151841926.
      // The move-only checker sometimes inserts destroy_addr within read-only static access scopes.
      // TODO: remove this once the bug is fixed.
      return
    }
    if accessKind == .read {
      var mutatingInstructions = MutatingUsesWalker(context)
      defer { mutatingInstructions.deinitialize() }

      mutatingInstructions.findMutatingUses(of: self.address)
      mutatingInstructions.verifyNoMutatingUsesInLinearLiverange(of: self)
    }
  }
}

extension VectorBaseAddrInst : VerifiableInstruction {
  func verify(_ context: VerifierContext) {
    require(vector.type.isBuiltinFixedArray,
            "vector operand of vector_element_addr must be a Builtin.FixedArray",
            atInstruction: self)
    require(type == vector.type.builtinFixedArrayElementType(in: parentFunction,
                                                             maximallyAbstracted: true).addressType,
            "result of vector_element_addr has wrong type",
            atInstruction: self)
  }
}

// Used to check if any instruction is mutating the memory location within the liverange of a `load_borrow`.
// Note that it is not checking if an instruction _may_ mutate the memory, but it's checking if any instruction
// _definitely_ will mutate the memory.
// Otherwise the risk would be too big for false alarms. It also means that this verification is not perfect and
// might miss some subtle violations.
private struct MutatingUsesWalker : AddressDefUseWalker {
  let context: VerifierContext
  var mutatingInstructions: InstructionSet

  init(_ context: VerifierContext) {
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
            require(false, "missing borrowed-from for \(phi.value)",
                    atArgument: phi.value)
          }
        }
      }
    }
  }

  mutating func verifyNoMutatingUsesInLinearLiverange(of startInst: SingleValueInstruction) {
    var instWorklist = InstructionWorklist(context)
    defer { instWorklist.deinitialize() }

    // It would be good enough to only consider `end_borrow`s (and branches), but adding all users doesn't harm.
    for use in startInst.uses {
      instWorklist.pushPredecessors(of: use.instruction, ignoring: startInst)
    }

    while let inst = instWorklist.pop() {
      require(!mutatingInstructions.contains(inst),
              "read-only scope invalidated by a local write", atInstruction: inst)
      instWorklist.pushPredecessors(of: inst, ignoring: startInst)
    }
  }

  mutating func walkDown(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    if let beginAccess = address.instruction as? BeginAccessInst, beginAccess.accessKind != .read {
      // Don't verify that there are no stores in read-only access scopes if there is a conflicting scope.
      // This is a programming error, but the compiler should not crash. The violation is caught at runtime.
      return .continueWalk
    }
    return walkDownDefault(address: address, path: path)
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
          // Such operands are consumed by the `partial_apply` and therefore count as "written".
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
  BridgedVerifier.registerVerifier(
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction) in
      let context = VerifierContext(_bridged: bridgedCtxt)
      bridgedFunction.function.verify(context)
    }
  )
}
