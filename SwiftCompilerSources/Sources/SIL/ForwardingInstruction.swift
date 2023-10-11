//===--- ForwardingInstruction.swift - forwarding instruction protocols ---===//
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

import SILBridging

/// An instruction that forwards ownership from operands to results.
public protocol ForwardingInstruction : Instruction {
  var singleForwardedOperand: Operand? { get }

  /// Return true if the forwarded value has the same representation. If true, then the result can be mapped to the same storage without a move or copy.
  var preservesRepresentation: Bool { get }
}

extension ForwardingInstruction {
  public var forwardedOperands: OperandArray {
    // Some instructions have multiple real operands but only forward one.
    if let singleForwardingOp = singleForwardedOperand {
      return OperandArray(base: singleForwardingOp, count: 1)
    }
    // All others forward all operands (for enum, this may be zero operands).
    return operands
  }

  public var forwardedResults: ForwardedResults {
    ForwardedResults(inst: self)
  }

  /// If forwarding ownership is owned, then the instruction moves an owned operand to its result, ending its lifetime. If forwarding ownership is guaranteed, then the instruction propagates the lifetime of its borrows operand through its result.
  ///
  /// The resulting forwarded value's ownership (Value.ownership) is not identical to the instruction's forwarding ownership property. It differs when the result is trivial type. e.g. an owned or guaranteed value can be cast to a trivial type using owned or guaranteed forwarding.
  public var forwardingOwnership: Ownership {
    Ownership(bridged: bridged.ForwardingInst_forwardingOwnership())
  }
  
  /// A forwarding instruction preserves reference counts if it has a dynamically non-trivial result in which all references are forwarded from the operand.
  ///
  /// A cast can only forward guaranteed values if it preserves reference counts. Such casts cannot release any references within their operand's value and cannot retain any references owned by their result.
  public var preservesReferenceCounts: Bool {
    bridged.ForwardingInst_preservesOwnership()
  }
}

// An instruction that forwards a single value to a single result.
//
// For legacy reasons, some ForwardingInstructions that fit the SingleValueInstruction and UnaryInstruction requirements are not considered ConversionInstructions because certain routines do not want to see through them (InitExistentialValueInst, InitExistentialValueInst, OpenExistentialValueInst, OpenExistentialValueInst). This most likely has to do with type-dependent operands, although any ConversionInstruction should support type-dependent operands.
public protocol ConversionInstruction : SingleValueInstruction,
                                        UnaryInstruction,
                                        ForwardingInstruction
{}

extension Value {
  // If this value is produced by a ForwardingInstruction, return that instruction. This is convenient for following the forwarded value chain.
  // Unlike definingInstruction, a value's forwardingInstruction is not necessarily a valid insertion point. 
  public var forwardingInstruction: ForwardingInstruction? {
    if let inst = definingInstruction {
      return inst as? ForwardingInstruction
    }
    if let termResult = TerminatorResult(self) {
      return termResult.terminator as? ForwardingInstruction
    }
    return nil
  }
}

//===----------------------------------------------------------------------===//
//                      singleForwardedOperand
//===----------------------------------------------------------------------===//

extension ForwardingInstruction {
  // See ForwardingOperation::getSingleForwardingOperand().
  public var singleForwardedOperand: Operand? {
    let definedOps = self.definedOperands
    assert(definedOps.count == 1, "expected single operand for forwarding")
    return definedOps[0]
  }
}

extension ConversionInstruction {
  public var singleForwardedOperand: Operand? { operand }
}

extension EnumInst {
  public var singleForwardedOperand: Operand? {
    return operand // nil for an enum with no payload
  }
}

// -----------------------------------------------------------------------------
// Instructions with multiple forwarded operands have nil singleForwardedOperand.

extension StructInst {
  public var singleForwardedOperand: Operand? { nil }
}

extension TupleInst {
  public var singleForwardedOperand: Operand? { nil }
}

extension LinearFunctionInst {
  public var singleForwardedOperand: Operand? { nil }
}

extension DifferentiableFunctionInst {
  public var singleForwardedOperand: Operand? { nil }
}

// -----------------------------------------------------------------------------
// Instructions with a singleForwardedOperand and additional operands.

extension MarkDependenceInst {
  public var singleForwardedOperand: Operand? {
    return valueOperand
  }
}

extension RefToBridgeObjectInst {
  public var singleForwardedOperand: Operand? {
    return convertedOperand
  }
}

extension TuplePackExtractInst {
  public var singleForwardedOperand: Operand? {
    return tupleOperand
  }
}

//===----------------------------------------------------------------------===//
//                         forwardedResults
//===----------------------------------------------------------------------===//

public struct ForwardedResults : Collection {
  private let inst: Instruction
  private let maxResults: Int

  fileprivate init(inst: ForwardingInstruction) {
    self.inst = inst
    if let ti = inst as? TermInst {
      self.maxResults = ti.successors.count
    } else {
      self.maxResults = inst.results.count
    }
  }

  public var startIndex: Int { skipEmptyResults(at: 0) }

  public var endIndex: Int { maxResults }

  public func index(after index: Int) -> Int {
    return skipEmptyResults(at: index + 1)
  }

  public subscript(_ index: Int) -> Value {
    if let ti = inst as? TermInst {
      return getTerminatorResult(termInst: ti, index: index)!
    }
    return inst.results[index]
  }

  private func skipEmptyResults(at index: Int) -> Int {
    guard let ti = inst as? TermInst else { return index }
    var next = index
    while next != endIndex {
      if getTerminatorResult(termInst: ti, index: next) != nil { break }
      next += 1
    }
    return next
  }

  // Forwarding terminators have a zero or one result per successor.
  private func getTerminatorResult(termInst: TermInst,
    index: Int) -> Argument? {
    let succ = termInst.successors[index]
    guard succ.arguments.count == 1 else {
      // The default enum payload may be empty.
      assert(succ.arguments.count == 0, "terminator must forward a single value")
      return nil
    }
    return succ.arguments[0]
  }
}

//===----------------------------------------------------------------------===//
//                     preservesRepresentation
//===----------------------------------------------------------------------===//

extension ForwardingInstruction {
  // Conservatively assume that a conversion changes representation.
  // Operations can be added as needed to participate in SIL opaque values.
  // See ForwardingOperation::hasSameRepresentation().
  public var preservesRepresentation: Bool { false }
}

extension ConvertFunctionInst {
  public var preservesRepresentation: Bool { true }
}

extension DestructureTupleInst {
  public var preservesRepresentation: Bool { true }
}

extension DestructureStructInst {
  public var preservesRepresentation: Bool { true }
}

extension InitExistentialRefInst {
  public var preservesRepresentation: Bool { true }
}

extension ObjectInst {
  public var preservesRepresentation: Bool { true }
}

extension OpenExistentialBoxValueInst {
  public var preservesRepresentation: Bool { true }
}

extension OpenExistentialRefInst {
  public var preservesRepresentation: Bool { true }
}

extension OpenExistentialValueInst {
  public var preservesRepresentation: Bool { true }
}

extension MarkUnresolvedNonCopyableValueInst {
  public var preservesRepresentation: Bool { true }
}

extension MarkUninitializedInst {
  public var preservesRepresentation: Bool { true }
}

extension StructExtractInst {
  public var preservesRepresentation: Bool { true }
}

extension TupleExtractInst {
  public var preservesRepresentation: Bool { true }
}

extension TuplePackExtractInst {
  public var preservesRepresentation: Bool { true }
}
