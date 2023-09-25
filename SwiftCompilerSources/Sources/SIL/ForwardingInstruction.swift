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
public protocol ForwardingInstruction : Instruction {}

extension ForwardingInstruction {
  public var singleForwardedOperand: Operand? {
    Operand(bridged: bridged.ForwardingInst_singleForwardedOperand())
  }

  public var forwardedOperands: OperandArray {
    let operands = bridged.ForwardingInst_forwardedOperands()
    return OperandArray(base: operands.base, count: operands.count)
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
    bridged.ForwardingInst_preservesReferenceCounts()
  }

  /// Return true if the forwarded value has the same representation. If true, then the result can be mapped to the same storage without a move or copy.
  public var preservesRepresentation: Bool {
    bridged.ForwardingInst_preservesRepresentation()
  }

  /// Return true if the forwarded value is address-only either before or after forwarding.
  public var isAddressOnly: Bool { bridged.ForwardingInst_isAddressOnly() }
}

extension Value {
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

// An instruction that forwards a single value to a single result.
//
// For legacy reasons, some ForwardingInstructions that fit the SingleValueInstruction and UnaryInstruction requirements are not considered ConversionInstructions because certain routines do not want to see through them (InitExistentialValueInst, InitExistentialValueInst, OpenExistentialValueInst, OpenExistentialValueInst). This most likely has to do with type-dependent operands, although any ConversionInstruction should support type-dependent operands.
public protocol ConversionInstruction : SingleValueInstruction,
                                        UnaryInstruction,
                                        ForwardingInstruction
{}

extension ConversionInstruction {
  public var singleForwardedOperand: Operand { operand }
}

// Visit all forwarded results. Return true if all visitors return true.
extension ForwardingInstruction {
  public func visitForwardedResults(visitor: (Value) -> Bool) -> Bool {
    if let svi = self as? SingleValueInstruction {
      return visitor(svi)
    }
    if let mvi = self as? MultipleValueInstruction {
      return mvi.results.allSatisfy {
        visitor($0)
      }
    }
    let ti = self as! TermInst
    return ti.successors.allSatisfy { successor in
      assert(successor.arguments.count == 1,
        "terminator must forward a single value")
      return visitor(successor.arguments[0])
    }
  }
}
