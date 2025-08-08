//===--- Context.swift - defines the context types ------------------------===//
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

import AST
import SIL
import OptimizerBridging

extension Context {
  var bridgedPassContext: BridgedPassContext {
    BridgedPassContext(_bridged)
  }

  var options: Options { Options(_bridged: bridgedPassContext) }

  var diagnosticEngine: DiagnosticEngine {
    return DiagnosticEngine(bridged: bridgedPassContext.getDiagnosticEngine())
  }

  // The calleeAnalysis is not specific to a function and therefore can be provided in
  // all contexts.
  var calleeAnalysis: CalleeAnalysis {
    let bridgeCA = bridgedPassContext.getCalleeAnalysis()
    return CalleeAnalysis(bridged: bridgeCA)
  }

  var hadError: Bool { bridgedPassContext.hadError() }

  /// Enable diagnostics requiring WMO (for @noLocks, @noAllocation
  /// annotations, Embedded Swift, and class specialization). SourceKit is the
  /// only consumer that has this disabled today (as it disables WMO
  /// explicitly).
  var enableWMORequiredDiagnostics: Bool {
    bridgedPassContext.enableWMORequiredDiagnostics()
  }

  func canMakeStaticObjectReadOnly(objectType: Type) -> Bool {
    bridgedPassContext.canMakeStaticObjectReadOnly(objectType.bridged)
  }

  func notifyNewFunction(function: Function, derivedFrom: Function) {
    bridgedPassContext.addFunctionToPassManagerWorklist(function.bridged, derivedFrom.bridged)
  }
}

extension MutatingContext {
  func notifyInvalidatedStackNesting() { bridgedPassContext.notifyInvalidatedStackNesting() }
  var needFixStackNesting: Bool { bridgedPassContext.getNeedFixStackNesting() }

  func tryOptimizeApplyOfPartialApply(closure: PartialApplyInst) -> Bool {
    if bridgedPassContext.tryOptimizeApplyOfPartialApply(closure.bridged) {
      notifyInstructionsChanged()
      notifyCallsChanged()

      for use in closure.callee.uses {
        if use.instruction is FullApplySite {
          notifyInstructionChanged(use.instruction)
        }
      }
      return true
    }
    return false
  }

  func tryDeleteDeadClosure(closure: SingleValueInstruction, needKeepArgsAlive: Bool = true) -> Bool {
    if bridgedPassContext.tryDeleteDeadClosure(closure.bridged, needKeepArgsAlive) {
      notifyInstructionsChanged()
      return true
    }
    return false
  }

  func tryDevirtualize(apply: FullApplySite, isMandatory: Bool) -> ApplySite? {
    let result = bridgedPassContext.tryDevirtualizeApply(apply.bridged, isMandatory)
    if let newApply = result.newApply.instruction {
      erase(instruction: apply)
      notifyInstructionsChanged()
      notifyCallsChanged()
      if result.cfgChanged {
        notifyBranchesChanged()
      }
      notifyInstructionChanged(newApply)
      return newApply as! FullApplySite
    }
    return nil
  }
  
  func tryOptimizeKeypath(apply: FullApplySite) -> Bool {
    return bridgedPassContext.tryOptimizeKeypath(apply.bridged)
  }

  func inlineFunction(apply: FullApplySite, mandatoryInline: Bool) {
    // This is only a best-effort attempt to notify the new cloned instructions as changed.
    // TODO: get a list of cloned instructions from the `inlineFunction`
    let instAfterInling: Instruction?
    switch apply {
    case is ApplyInst:
      instAfterInling = apply.next
    case let beginApply as BeginApplyInst:
      let next = beginApply.next!
      instAfterInling = (next is EndApplyInst ? nil : next)
    case is TryApplyInst:
      instAfterInling = apply.parentBlock.next?.instructions.first
    default:
      instAfterInling = nil
    }

    bridgedPassContext.inlineFunction(apply.bridged, mandatoryInline)

    if let instAfterInling = instAfterInling {
      notifyNewInstructions(from: apply, to: instAfterInling)
    }
  }

  func loadFunction(function: Function, loadCalleesRecursively: Bool) -> Bool {
    if function.isDefinition {
      return true
    }
    _bridged.loadFunction(function.bridged, loadCalleesRecursively)
    return function.isDefinition
  }

  private func notifyNewInstructions(from: Instruction, to: Instruction) {
    var inst = from
    while inst != to {
      if !inst.isDeleted {
        notifyInstructionChanged(inst)
      }
      if let next = inst.next {
        inst = next
      } else {
        inst = inst.parentBlock.next!.instructions.first!
      }
    }
  }

  func getContextSubstitutionMap(for type: Type) -> SubstitutionMap {
    SubstitutionMap(bridged: _bridged.getContextSubstitutionMap(type.bridged))
  }

  /// Notifies the pass manager that the optimization result of the current pass depends
  /// on the body (i.e. SIL instructions) of another function than the currently optimized one.
  func notifyDependency(onBodyOf otherFunction: Function) {
    bridgedPassContext.notifyDependencyOnBodyOf(otherFunction.bridged)
  }
}

extension Instruction {
  var arraySemanticsCallKind: BridgedArrayCallKind {
    return BridgedPassContext.getArraySemanticsCallKind(self.bridged)
  }

  func canHoistArraySemanticsCall(to toInst: Instruction, _ context: FunctionPassContext) -> Bool {
    return context.bridgedPassContext.canHoistArraySemanticsCall(self.bridged, toInst.bridged)
  }

  func hoistArraySemanticsCall(before toInst: Instruction, _ context: some MutatingContext) {
    context.bridgedPassContext.hoistArraySemanticsCall(self.bridged, toInst.bridged) // Internally updates dom tree.
    context.notifyInstructionsChanged()
  }
}
