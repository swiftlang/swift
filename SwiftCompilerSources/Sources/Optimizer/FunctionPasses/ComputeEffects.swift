//===--- ComputeEffects.swift - Compute function effects ------------------===//
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

fileprivate typealias Selection = ArgumentEffect.Selection

/// Computes effects for function arguments.
///
/// For example, if an argument does not escape, adds a non-escaping effect,
/// e.g. "[escapes !%0.**]":
///
///   sil [escapes !%0.**] @foo : $@convention(thin) (@guaranteed X) -> () {
///   bb0(%0 : $X):
///     %1 = tuple ()
///     return %1 : $()
///   }
///
/// The pass does not try to change or re-compute _defined_ effects.
/// Currently, only escaping effects are handled.
/// In future, this pass may also add other effects, like memory side effects.
let computeEffects = FunctionPass(name: "compute-effects", {
  (function: Function, context: PassContext) in
  var argsWithDefinedEffects = getArgIndicesWithDefinedEffects(of: function)

  struct IgnoreRecursiveCallVisitor : EscapeInfoVisitor {
    func visitUse(operand: Operand, path: EscapePath) -> UseResult {
      return isOperandOfRecursiveCall(operand) ? .ignore : .continueWalk
    }
  }
  var escapeInfo = EscapeInfo(calleeAnalysis: context.calleeAnalysis, visitor: IgnoreRecursiveCallVisitor())
  var newEffects = Stack<ArgumentEffect>(context)
  let returnInst = function.returnInstruction

  for arg in function.arguments {
    // We are not interested in arguments with trivial types.
    if !arg.type.isNonTrivialOrContainsRawPointer(in: function) { continue }
    
    // Also, we don't want to override defined effects.
    if argsWithDefinedEffects.contains(arg.index) { continue }
    
    // First check: is the argument (or a projected value of it) escaping at all?
    if !escapeInfo.isEscapingWhenWalkingDown(object: arg, path: SmallProjectionPath(.anything)) {
      let selectedArg = Selection(arg, pathPattern: SmallProjectionPath(.anything))
      newEffects.push(ArgumentEffect(.notEscaping, selectedArg: selectedArg))
      continue
    }
  
    // Now compute effects for two important cases:
    //   * the argument itself + any value projections, and...
    if addArgEffects(context: context, arg, argPath: SmallProjectionPath(), to: &newEffects, returnInst) {
      //   * single class indirections
      _ = addArgEffects(context: context, arg, argPath: SmallProjectionPath(.anyValueFields).push(.anyClassField),
                        to: &newEffects, returnInst)
    }
  }

  context.modifyEffects(in: function) { (effects: inout FunctionEffects) in
    effects.removeDerivedEffects()
    effects.argumentEffects.append(contentsOf: newEffects)
  }
  newEffects.removeAll()
})


/// Returns true if an argument effect was added.
private
func addArgEffects(context: PassContext, _ arg: FunctionArgument, argPath ap: SmallProjectionPath,
                   to newEffects: inout Stack<ArgumentEffect>,
                   _ returnInst: ReturnInst?) -> Bool {
  // Correct the path if the argument is not a class reference itself, but a value type
  // containing one or more references.
  let argPath = arg.type.isClass ? ap : ap.push(.anyValueFields)
  
  struct ArgEffectsVisitor : EscapeInfoVisitor {
    init(toSelection: Selection?, returnInst: ReturnInst?) {
      self.toSelection = toSelection
      self.returnInst = returnInst
    }
    
    var toSelection: Selection?
    var returnInst: ReturnInst?
    
    mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
      if operand.instruction == returnInst {
        // The argument escapes to the function return
        if path.followStores {
          // The escaping path must not introduce a followStores.
          return .abort
        }
        if let ta = toSelection {
          if ta.value != .returnValue { return .abort }
          toSelection = Selection(.returnValue, pathPattern: path.projectionPath.merge(with: ta.pathPattern))
        } else {
          toSelection = Selection(.returnValue, pathPattern: path.projectionPath)
        }
        return .ignore
      }
      if isOperandOfRecursiveCall(operand) {
        return .ignore
      }
      return .continueWalk
    }
    
    mutating func visitDef(def: Value, path: EscapePath) -> DefResult {
      guard let destArg = def as? FunctionArgument else {
        return .continueWalkUp
      }
      // The argument escapes to another argument (e.g. an out or inout argument)
      if path.followStores {
        // The escaping path must not introduce a followStores.
        return .abort
      }
      let argIdx = destArg.index
      if let ta = toSelection {
        if ta.value != .argument(argIdx) { return .abort }
        toSelection = Selection(.argument(argIdx), pathPattern: path.projectionPath.merge(with: ta.pathPattern))
      } else {
        toSelection = Selection(.argument(argIdx), pathPattern: path.projectionPath)
      }
      return .walkDown
    }
  }
  
  var walker = EscapeInfo(calleeAnalysis: context.calleeAnalysis, visitor: ArgEffectsVisitor(toSelection: nil, returnInst: returnInst))
  if walker.isEscapingWhenWalkingDown(object: arg, path: argPath) {
    return false
  }
  
  let toSelection = walker.visitor.toSelection
  let fromSelection = Selection(arg, pathPattern: argPath)

  guard let toSelection = toSelection else {
    newEffects.push(ArgumentEffect(.notEscaping, selectedArg: fromSelection))
    return true
  }
  
  // If the function never returns, the argument can not escape to another arg/return.
  guard let returnInst = returnInst else {
    return false
  }

  let exclusive = isExclusiveEscape(context: context, fromArgument: arg, fromPath: argPath, to: toSelection, returnInst)

  newEffects.push(ArgumentEffect(.escaping(toSelection, exclusive), selectedArg: fromSelection))
  return true
}

/// Returns a set of argument indices for which there are "defined" effects (as opposed to derived effects).
private func getArgIndicesWithDefinedEffects(of function: Function) -> Set<Int> {
  var argsWithDefinedEffects = Set<Int>()

  for effect in function.effects.argumentEffects {
    if effect.isDerived { continue }

    if case .argument(let argIdx) = effect.selectedArg.value {
      argsWithDefinedEffects.insert(argIdx)
    }

    switch effect.kind {
    case .notEscaping:
      break
    case .escaping(let to, _):
      if case .argument(let toArgIdx) = to.value {
        argsWithDefinedEffects.insert(toArgIdx)
      }
    case .sideEffect:
      // TODO: update this depending on whether sideEffects are computed with escapeinfo or not
      break
    }
  }
  return argsWithDefinedEffects
}

/// Returns true if `op` is passed to a recursive call to the current function -
/// at the same argument index.
private func isOperandOfRecursiveCall(_ op: Operand) -> Bool {
  let inst = op.instruction
  if let applySite = inst as? FullApplySite,
     let callee = applySite.referencedFunction,
     callee == inst.function,
     let argIdx = applySite.argumentIndex(of: op),
     op.value == callee.arguments[argIdx] {
    return true
  }
  return false
}

/// Returns true if when walking from the `toSelection` to the `fromArgument`,
/// there are no other arguments or escape points than `fromArgument`. Also, the
/// path at the `fromArgument` must match with `fromPath`.
private
func isExclusiveEscape(context: PassContext, fromArgument: Argument, fromPath: SmallProjectionPath, to toSelection: Selection,
                       _ returnInst: ReturnInst) -> Bool {
  switch toSelection.value {
  
  // argument -> return
  case .returnValue:
    struct IsExclusiveReturnEscapeVisitor : EscapeInfoVisitor {
      let fromArgument: Argument
      let toSelection: Selection
      let returnInst: ReturnInst
      let fromPath: SmallProjectionPath
      
      mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
        if operand.instruction == returnInst {
          if path.followStores { return .abort }
          if path.projectionPath.matches(pattern: toSelection.pathPattern) {
            return .ignore
          }
          return .abort
        }
        return .continueWalk
      }
      
      mutating func visitDef(def: Value, path: EscapePath) -> DefResult {
        guard let arg = def as? FunctionArgument else {
          return .continueWalkUp
        }
        if path.followStores { return .abort }
        if arg == fromArgument && path.projectionPath.matches(pattern: fromPath) {
          return .walkDown
        }
        return .abort
      }
    }
    let visitor = IsExclusiveReturnEscapeVisitor(fromArgument: fromArgument, toSelection: toSelection, returnInst: returnInst, fromPath: fromPath)
    var walker = EscapeInfo(calleeAnalysis: context.calleeAnalysis, visitor: visitor)
    if walker.isEscaping(object: returnInst.operand, path: toSelection.pathPattern) {
      return false
    }
  // argument -> argument
  case .argument(let toArgIdx):
    struct IsExclusiveArgumentEscapeVisitor : EscapeInfoVisitor {
      let fromArgument: Argument
      let fromPath: SmallProjectionPath
      let toSelection: Selection
      let toArg: FunctionArgument
      
      mutating func visitDef(def: Value, path: EscapePath) -> DefResult {
        guard let arg = def as? FunctionArgument else {
          return .continueWalkUp
        }
        if path.followStores { return .abort }
        if arg == fromArgument && path.projectionPath.matches(pattern: fromPath) { return .walkDown }
        if arg == toArg && path.projectionPath.matches(pattern: toSelection.pathPattern) { return .walkDown }
        return .abort
      }
    }
    let toArg = returnInst.function.arguments[toArgIdx]
    let visitor = IsExclusiveArgumentEscapeVisitor(fromArgument: fromArgument, fromPath: fromPath, toSelection: toSelection, toArg: toArg)
    var walker = EscapeInfo(calleeAnalysis: context.calleeAnalysis, visitor: visitor)
    if walker.isEscaping(object: toArg, path: toSelection.pathPattern) {
      return false
    }
  }
  return true
}

