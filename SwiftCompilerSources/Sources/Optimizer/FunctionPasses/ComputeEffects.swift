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
fileprivate typealias Path = ArgumentEffect.Path

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

  var escapeInfo = EscapeInfo(calleeAnalysis: context.calleeAnalysis)
  var newEffects = Stack<ArgumentEffect>(context)
  let returnInst = function.returnInstruction

  for arg in function.arguments {
    // We are not interested in arguments with trivial types.
    if !arg.type.isNonTrivialOrContainsRawPointer(in: function) { continue }
    
    // Also, we don't want to override defined effects.
    if argsWithDefinedEffects.contains(arg.index) { continue }
    
    // First check: is the argument (or a projected value of it) escaping at all?
    if !escapeInfo.isEscapingWhenWalkingDown(object: arg, path: Path(.anything),
        visitUse: { op, _, _ in
          isOperandOfRecursiveCall(op) ? .ignore : .continueWalking
        }) {
      let selectedArg = Selection(arg, pathPattern: Path(.anything))
      newEffects.push(ArgumentEffect(.notEscaping, selectedArg: selectedArg))
      continue
    }
  
    // Now compute effects for two important cases:
    //   * the argument itself + any value projections, and...
    if addArgEffects(arg, argPath: Path(), to: &newEffects, returnInst, &escapeInfo) {
      //   * single class indirections
      _ = addArgEffects(arg, argPath: Path(.anyValueFields).push(.anyClassField),
                        to: &newEffects, returnInst, &escapeInfo)
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
func addArgEffects(_ arg: FunctionArgument, argPath ap: Path,
                   to newEffects: inout Stack<ArgumentEffect>,
                   _ returnInst: ReturnInst?,
                   _ escapeInfo: inout EscapeInfo) -> Bool {

  var toSelection: Selection?
  // Correct the path if the argument is not a class reference itself, but a value type
  // containing one or more references.
  let argPath = arg.type.isClass ? ap : ap.push(.anyValueFields)

  if escapeInfo.isEscapingWhenWalkingDown(object: arg, path: argPath,
      visitUse: { op, path, followStores in
        if op.instruction == returnInst {
          // The argument escapes to the function return
          if followStores {
            // The escaping path must not introduce a followStores.
            return .markEscaping
          }
          if let ta = toSelection {
            if ta.value != .returnValue { return .markEscaping }
            toSelection = Selection(.returnValue, pathPattern: path.merge(with: ta.pathPattern))
          } else {
            toSelection = Selection(.returnValue, pathPattern: path)
          }
          return .ignore
        }
        if isOperandOfRecursiveCall(op) {
          return .ignore
        }
        return .continueWalking
      },
      visitDef: { def, path, followStores in
        guard let destArg = def as? FunctionArgument else {
          return .continueWalkingUp
        }
        // The argument escapes to another argument (e.g. an out or inout argument)
        if followStores {
          // The escaping path must not introduce a followStores.
          return .markEscaping
        }
        let argIdx = destArg.index
        if let ta = toSelection {
          if ta.value != .argument(argIdx) { return .markEscaping }
          toSelection = Selection(.argument(argIdx), pathPattern: path.merge(with: ta.pathPattern))
        } else {
          toSelection = Selection(.argument(argIdx), pathPattern: path)
        }
        return .continueWalkingDown
      }) {
    return false
  }

  let fromSelection = Selection(arg, pathPattern: argPath)

  guard let toSelection = toSelection else {
    newEffects.push(ArgumentEffect(.notEscaping, selectedArg: fromSelection))
    return true
  }
  
  // If the function never returns, the argument can not escape to another arg/return.
  guard let returnInst = returnInst else {
    return false
  }

  let exclusive = isExclusiveEscape(fromArgument: arg, fromPath: argPath, to: toSelection, returnInst, &escapeInfo)

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
func isExclusiveEscape(fromArgument: Argument, fromPath: Path, to toSelection: Selection,
                       _ returnInst: ReturnInst, _ escapeInfo: inout EscapeInfo) -> Bool {
  switch toSelection.value {
  
  // argument -> return
  case .returnValue:
    if escapeInfo.isEscaping(
          object: returnInst.operand, path: toSelection.pathPattern,
          visitUse: { op, path, followStores in
            if op.instruction == returnInst {
              if followStores { return .markEscaping }
              if path.matches(pattern: toSelection.pathPattern) {
                return .ignore
              }
              return .markEscaping
            }
            return .continueWalking
          },
          visitDef: { def, path, followStores in
            guard let arg = def as? FunctionArgument else {
              return .continueWalkingUp
            }
            if followStores { return .markEscaping }
            if arg == fromArgument && path.matches(pattern: fromPath) {
              return .continueWalkingDown
            }
            return .markEscaping
          }) {
      return false
    }
    
  // argument -> argument
  case .argument(let toArgIdx):
    let toArg = returnInst.function.arguments[toArgIdx]
    if escapeInfo.isEscaping(object: toArg, path: toSelection.pathPattern,
        visitDef: { def, path, followStores in
          guard let arg = def as? FunctionArgument else {
            return .continueWalkingUp
          }
          if followStores { return .markEscaping }
          if arg == fromArgument && path.matches(pattern: fromPath) { return .continueWalkingDown }
          if arg == toArg && path.matches(pattern: toSelection.pathPattern) { return .continueWalkingDown }
          return .markEscaping
        }) {
      return false
    }
  }
  return true
}
