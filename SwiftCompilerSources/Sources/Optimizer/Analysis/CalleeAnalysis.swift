//===--- CalleeAnalysis.swift - the callee analysis -----------------------===//
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

import OptimizerBridging
import SIL

struct CalleeAnalysis {
  let bridged: BridgedCalleeAnalysis

  static func register() {
    BridgedCalleeAnalysis.registerAnalysis(
      // isDeinitBarrierFn:
      { (inst : BridgedInstruction, bca: BridgedCalleeAnalysis) -> Bool in
        return inst.instruction.isDeinitBarrier(bca.analysis)
      },
      // getMemBehaviorFn
      { (bridgedApply: BridgedInstruction, observeRetains: Bool, bca: BridgedCalleeAnalysis) -> BridgedMemoryBehavior in
        let apply = bridgedApply.instruction as! FullApplySite
        let e = bca.analysis.getSideEffects(ofApply: apply)
        return e.getMemBehavior(observeRetains: observeRetains)
      }
    )
  }

  func getCallees(callee: Value) -> FunctionArray? {
    let bridgedFuncs = bridged.getCallees(callee.bridged)
    if bridgedFuncs.isIncomplete() {
      return nil
    }
    return FunctionArray(bridged: bridgedFuncs)
  }

  func getIncompleteCallees(callee: Value) -> FunctionArray {
    return FunctionArray(bridged: bridged.getCallees(callee.bridged))
  }

  func getDestructor(ofExactType type: Type) -> Function? {
    let destructors = FunctionArray(bridged: bridged.getDestructors(type.bridged, /*isExactType*/ true))
    if destructors.count == 1 {
      return destructors[0]
    }
    return nil
  }

  func getDestructors(of type: Type) -> FunctionArray? {
    let bridgedDtors = bridged.getDestructors(type.bridged, /*isExactType*/ false)
    if bridgedDtors.isIncomplete() {
      return nil
    }
    return FunctionArray(bridged: bridgedDtors)
  }

  /// Returns the global (i.e. not argument specific) side effects of an apply.
  func getSideEffects(ofApply apply: FullApplySite) -> SideEffects.GlobalEffects {
    return getSideEffects(ofCallee: apply.callee)
  }

  func getSideEffects(ofCallee callee: Value) -> SideEffects.GlobalEffects {
    guard let callees = getCallees(callee: callee) else {
      return .worstEffects
    }

    var result = SideEffects.GlobalEffects()
    for callee in callees {
      let calleeEffects = callee.getSideEffects()
      result.merge(with: calleeEffects)
    }
    return result
  }

  /// Returns the argument specific side effects of an apply.
  func getSideEffects(of apply: FullApplySite, operand: Operand, path: SmallProjectionPath) -> SideEffects.GlobalEffects {
    var result = SideEffects.GlobalEffects()
    guard let calleeArgIdx = apply.calleeArgumentIndex(of: operand) else {
      return result
    }
    let convention = apply.convention(of: operand)!
    let argument = operand.value.at(path)

    guard let callees = getCallees(callee: apply.callee) else {
      return .worstEffects.restrictedTo(argument: argument, withConvention: convention)
    }
  
    for callee in callees {
      let calleeEffects = callee.getSideEffects(forArgument: argument,
                                                atIndex: calleeArgIdx,
                                                withConvention: convention)
      result.merge(with: calleeEffects)
    }
    return result.restrictedTo(argument: argument, withConvention: convention)
  }
}

extension Value {
  fileprivate func isBarrier(_ analysis: CalleeAnalysis) -> Bool {
    guard let callees = analysis.getCallees(callee: self) else {
      return true
    }
    return callees.contains { $0.isDeinitBarrier }
  }
}

extension FullApplySite {
  fileprivate func isBarrier(_ analysis: CalleeAnalysis) -> Bool {
    return callee.isBarrier(analysis)
  }
}

extension EndApplyInst {
  fileprivate func isBarrier(_ analysis: CalleeAnalysis) -> Bool {
    return (operand.value.definingInstruction as! FullApplySite).isBarrier(analysis)
  }
}

extension AbortApplyInst {
  fileprivate func isBarrier(_ analysis: CalleeAnalysis) -> Bool {
    return (operand.value.definingInstruction as! FullApplySite).isBarrier(analysis)
  }
}

extension Instruction {
  /// Whether lifetime ends of lexical values may safely be hoisted over this
  /// instruction.
  ///
  /// Deinitialization barriers constrain variable lifetimes. Lexical
  /// end_borrow, destroy_value, and destroy_addr cannot be hoisted above them.
  final func isDeinitBarrier(_ analysis: CalleeAnalysis) -> Bool {
    if let site = self as? FullApplySite {
      return site.isBarrier(analysis)
    }
    if let eai = self as? EndApplyInst {
      return eai.isBarrier(analysis)
    }
    if let aai = self as? AbortApplyInst {
      return aai.isBarrier(analysis)
    }
    return mayAccessPointer || mayLoadWeakOrUnowned || maySynchronize
  }
}

struct FunctionArray : RandomAccessCollection, FormattedLikeArray {
  fileprivate let bridged: BridgedCalleeAnalysis.CalleeList

  var startIndex: Int { 0 }
  var endIndex: Int { bridged.getCount() }

  subscript(_ index: Int) -> Function {
    return bridged.getCallee(index).function
  }
}
// Bridging utilities

extension BridgedCalleeAnalysis {
  var analysis: CalleeAnalysis { .init(bridged: self) }
}

