//===--- OwnershipUtilities.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension InstructionRange {
  /// Returns true if this instruction range is fully contained in the liverange of `value`.
  public func isFullyContainedIn(scopeOf value: Value) -> Bool {
    switch value.ownership {
    case .owned:
      return value.uses.endingLifetime.allSatisfy {
        !self.contains($0.instruction)
      }
    case .guaranteed:
      guard let beginBorrow = BeginBorrowValue(value.lookThroughForwardingInstructions) else {
        return false
      }
      if case .functionArgument = beginBorrow {
        // The lifetime of a guaranteed function argument spans over the whole function.
        return true
      }
      return beginBorrow.scopeEndingOperands.allSatisfy {
        !self.contains($0.instruction)
      }
    case .none, .unowned:
      return false
    }
  }
}

/// Extends the borrow scope of a guaranteed `value` to ensure it overlaps with a given instruction range.
///
/// This function attempts to move `end_borrow` instructions that fall within the specified range
/// to positions after the range ends, effectively extending the lifetime of the borrowed value
/// to cover the entire range. This is useful for optimizations where a borrowed value needs
/// to remain live throughout a specific code region.
///
/// Returns true if the borrow scope was successfully extended or already overlaps the range,
/// returns false if the extension is not possible due to conflicting lifetime constraints.
///
func extendBorrowScope(of value: Value,
                       toOverlap range: InstructionRange,
                       _ context: FunctionPassContext) -> Bool
{
  guard let beginBorrow = BeginBorrowValue(value.lookThroughForwardingInstructions) else {
    return false
  }
  if case .functionArgument = beginBorrow {
    // The lifetime of a guaranteed function argument spans over the whole function.
    return true
  }

  guard var endBorrowsToMove = getEndBorrows(of: beginBorrow, inRange: range, context) else {
    return false
  }
  defer { endBorrowsToMove.deinitialize() }

  if endBorrowsToMove.isEmpty {
    // The borrow scope already completely overlaps the range. Nothing to do.
    return true
  }

  var rangeEndInstructions = InstructionSet(context)
  defer { rangeEndInstructions.deinitialize() }
  rangeEndInstructions.insert(contentsOf: range.ends)
  rangeEndInstructions.insert(contentsOf: range.exits)

  var insertionPoints: Stack<Instruction>

  switch beginBorrow {
  case .beginBorrow(let bbi):
    guard let ips = getInsertionPoints(for: bbi, endBorrowsToMove, rangeEndInstructions, context) else {
      return false
    }
    insertionPoints = ips

  case .loadBorrow(let loadBorrow):
    guard let ips = getInsertionPoints(for: loadBorrow, endBorrowsToMove, rangeEndInstructions, context) else {
      return false
    }
    insertionPoints = ips

  default:
    return false
  }
  defer { insertionPoints.deinitialize() }

  // Move the `end_borrow`s out of the range.
  //
  context.erase(instructions: endBorrowsToMove)
  for insertionPoint in insertionPoints {
    Builder(before: insertionPoint, context).createEndBorrow(of: beginBorrow.value)
  }
  return true
}

private func getEndBorrows(of beginBorrow: BeginBorrowValue,
                           inRange range: InstructionRange,
                           _ context: FunctionPassContext) -> Stack<Instruction>?
{
  var endBorrowsToMove = Stack<Instruction>(context)

  for scopeEndingOp in beginBorrow.scopeEndingOperands {
    if range.contains(scopeEndingOp.instruction) {
      guard let endBorrow = scopeEndingOp.instruction as? EndBorrowInst else {
        endBorrowsToMove.deinitialize()
        return nil
      }
      endBorrowsToMove.append(endBorrow)
    }
  }
  return endBorrowsToMove
}

private func getInsertionPoints(for beginBorrow: BeginBorrowInst,
                                _ endBorrowsToMove: Stack<Instruction>,
                                _ rangeEndInstructions: InstructionSet,
                                _ context: FunctionPassContext) -> Stack<Instruction>?
{
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(contentsOf: endBorrowsToMove.map { $0.next! })

  let enclosingValue = beginBorrow.borrowedValue
  guard enclosingValue.ownership == .owned else {
    return nil
  }
  var enclosingScopeEnds = InstructionSet(context)
  defer { enclosingScopeEnds.deinitialize() }
  enclosingScopeEnds.insert(contentsOf: enclosingValue.uses.endingLifetime.users)

  var insertionPoints = Stack<Instruction>(context)

  while let inst = worklist.pop() {
    if rangeEndInstructions.contains(inst) {
      insertionPoints.append(inst)
    } else {
      if enclosingScopeEnds.contains(inst) {
        insertionPoints.deinitialize()
        return nil
      }
      worklist.pushSuccessors(of: inst)
    }
  }
  return insertionPoints
}

private func getInsertionPoints(for loadBorrow: LoadBorrowInst,
                                _ endBorrowsToMove: Stack<Instruction>,
                                _ rangeEndInstructions: InstructionSet,
                                _ context: FunctionPassContext) -> Stack<Instruction>?
{
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(contentsOf: endBorrowsToMove.map { $0.next! })

  let aliasAnalysis = context.aliasAnalysis
  var insertionPoints = Stack<Instruction>(context)

  while let inst = worklist.pop() {
    if rangeEndInstructions.contains(inst) {
      insertionPoints.append(inst)
    } else {
      if inst.mayWrite(toAddress: loadBorrow.address, aliasAnalysis) {
        insertionPoints.deinitialize()
        return nil
      }
      worklist.pushSuccessors(of: inst)
    }
  }
  return insertionPoints
}

extension Value {
  /// Looks through forwarding instructions in the use-def chain and returns the original forwarded value.
  /// It looks through phi-arguments, terminator instructions and all kind of forwarding instructions
  /// which forward exactly one (non-trivial) operand.
  public var lookThroughForwardingInstructions: Value {
    if let bfi = definingInstruction as? BorrowedFromInst,
       !bfi.borrowedPhi.isReborrow,
       bfi.enclosingValues.count == 1
    {
      // Return the single forwarded enclosingValue
      return bfi.enclosingValues[0]
    }
    if let fi = definingInstruction as? ForwardingInstruction,
       let forwardedOp = fi.singleForwardedOperand
    {
       return forwardedOp.value.lookThroughForwardingInstructions
    } else if let termResult = TerminatorResult(self),
              let fi = termResult.terminator as? ForwardingInstruction,
              let forwardedOp = fi.singleForwardedOperand
    {
      return forwardedOp.value.lookThroughForwardingInstructions
    }
    return self
  }
}

//===----------------------------------------------------------------------===//
//                               Unit Tests
//===----------------------------------------------------------------------===//

let extendBorrowScopeTest = FunctionTest("extend_borrow_scope") {
    function, arguments, context in

  let borrowValue = arguments.takeValue()
  let rangeBegin = arguments.takeValue() as! SingleValueInstruction
  let expectedResult = arguments.takeBool()

  var range = InstructionRange(begin: rangeBegin, ends: rangeBegin.users, context)
  defer { range.deinitialize() }

  let result = extendBorrowScope(of: borrowValue, toOverlap: range, context)
  precondition(result == expectedResult)
}
