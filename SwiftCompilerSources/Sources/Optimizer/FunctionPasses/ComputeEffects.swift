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

  struct IgnoreRecursiveCallVisitor : EscapeVisitor {
    func visitUse(operand: Operand, path: EscapePath) -> UseResult {
      return isOperandOfRecursiveCall(operand) ? .ignore : .continueWalk
    }
  }
  var newEffects = Stack<ArgumentEffect>(context)
  let returnInst = function.returnInstruction

  for arg in function.arguments {
    // We are not interested in arguments with trivial types.
    if !arg.type.isNonTrivialOrContainsRawPointer(in: function) { continue }
    
    // Also, we don't want to override defined effects.
    if argsWithDefinedEffects.contains(arg.index) { continue }
    
    // First check: is the argument (or a projected value of it) escaping at all?
    if !arg.at(.anything).isEscapingWhenWalkingDown(using: IgnoreRecursiveCallVisitor(),
                                                    context) {
      newEffects.push(ArgumentEffect(.notEscaping, argumentIndex: arg.index, pathPattern: SmallProjectionPath(.anything)))
      continue
    }
  
    // Now compute effects for two important cases:
    //   * the argument itself + any value projections, and...
    if addArgEffects(arg, argPath: SmallProjectionPath(), to: &newEffects, returnInst, context) {
      //   * single class indirections
      _ = addArgEffects(arg, argPath: SmallProjectionPath(.anyValueFields).push(.anyClassField),
                        to: &newEffects, returnInst, context)
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
func addArgEffects(_ arg: FunctionArgument, argPath ap: SmallProjectionPath,
                   to newEffects: inout Stack<ArgumentEffect>,
                   _ returnInst: ReturnInst?, _ context: PassContext) -> Bool {
  // Correct the path if the argument is not a class reference itself, but a value type
  // containing one or more references.
  let argPath = arg.type.isClass ? ap : ap.push(.anyValueFields)
  
  struct ArgEffectsVisitor : EscapeVisitorWithResult {
    enum EscapeDestination {
      case notSet
      case toReturn(SmallProjectionPath)
      case toArgument(Int, SmallProjectionPath) // argument index, path
    }
    var result = EscapeDestination.notSet
    
    mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
      if operand.instruction is ReturnInst {
        // The argument escapes to the function return
        if path.followStores {
          // The escaping path must not introduce a followStores.
          return .abort
        }
        switch result {
          case .notSet:
            result = .toReturn(path.projectionPath)
          case .toReturn(let oldPath):
            result = .toReturn(oldPath.merge(with: path.projectionPath))
          case .toArgument:
            return .abort
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
      switch result {
        case .notSet:
          result = .toArgument(argIdx, path.projectionPath)
        case .toArgument(let oldArgIdx, let oldPath) where oldArgIdx == argIdx:
          result = .toArgument(argIdx, oldPath.merge(with: path.projectionPath))
        default:
          return .abort
      }
      return .walkDown
    }
  }
  
  guard let result = arg.at(argPath).visitByWalkingDown(using: ArgEffectsVisitor(),
                                                        context) else {
    return false
  }
  
  // If the function never returns, the argument can not escape to another arg/return.
  guard let returnInst = arg.function.returnInstruction else {
    return false
  }

  let effect: ArgumentEffect
  switch result {
  case .notSet:
    effect = ArgumentEffect(.notEscaping, argumentIndex: arg.index, pathPattern: argPath)
  case .toReturn(let toPath):
    let exclusive = isExclusiveEscapeToReturn(fromArgument: arg, fromPath: argPath,
                                              toPath: toPath, returnInst: returnInst, context)
    effect = ArgumentEffect(.escapingToReturn(toPath, exclusive),
                            argumentIndex: arg.index, pathPattern: argPath)
  case .toArgument(let toArgIdx, let toPath):
    let exclusive = isExclusiveEscapeToArgument(fromArgument: arg, fromPath: argPath,
                                                toArgumentIndex: toArgIdx, toPath: toPath, context)
    effect = ArgumentEffect(.escapingToArgument(toArgIdx, toPath, exclusive),
                            argumentIndex: arg.index, pathPattern: argPath)
  }
  newEffects.push(effect)
  return true
}

/// Returns a set of argument indices for which there are "defined" effects (as opposed to derived effects).
private func getArgIndicesWithDefinedEffects(of function: Function) -> Set<Int> {
  var argsWithDefinedEffects = Set<Int>()

  for effect in function.effects.argumentEffects {
    if effect.isDerived { continue }

    argsWithDefinedEffects.insert(effect.argumentIndex)

    switch effect.kind {
    case .notEscaping, .escapingToReturn:
      break
    case .escapingToArgument(let toArgIdx, _, _):
      argsWithDefinedEffects.insert(toArgIdx)
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
func isExclusiveEscapeToReturn(fromArgument: Argument, fromPath: SmallProjectionPath,
                               toPath: SmallProjectionPath,
                               returnInst: ReturnInst, _ context: PassContext) -> Bool {
  struct IsExclusiveReturnEscapeVisitor : EscapeVisitor {
    let fromArgument: Argument
    let fromPath: SmallProjectionPath
    let toPath: SmallProjectionPath
    
    mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
      if operand.instruction is ReturnInst {
        if path.followStores { return .abort }
        if path.projectionPath.matches(pattern: toPath) {
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
  let visitor = IsExclusiveReturnEscapeVisitor(fromArgument: fromArgument, fromPath: fromPath, toPath: toPath)
  return !returnInst.operand.at(toPath).isEscaping(using: visitor, context)
}

private
func isExclusiveEscapeToArgument(fromArgument: Argument, fromPath: SmallProjectionPath,
                                 toArgumentIndex: Int, toPath: SmallProjectionPath, _ context: PassContext) -> Bool {
  struct IsExclusiveArgumentEscapeVisitor : EscapeVisitor {
    let fromArgument: Argument
    let fromPath: SmallProjectionPath
    let toArgumentIndex: Int
    let toPath: SmallProjectionPath
    
    mutating func visitDef(def: Value, path: EscapePath) -> DefResult {
      guard let arg = def as? FunctionArgument else {
        return .continueWalkUp
      }
      if path.followStores { return .abort }
      if arg == fromArgument && path.projectionPath.matches(pattern: fromPath) { return .walkDown }
      if arg.index == toArgumentIndex && path.projectionPath.matches(pattern: toPath) { return .walkDown }
      return .abort
    }
  }
  let visitor = IsExclusiveArgumentEscapeVisitor(fromArgument: fromArgument, fromPath: fromPath,
                                                 toArgumentIndex: toArgumentIndex, toPath: toPath)
  let toArg = fromArgument.function.arguments[toArgumentIndex]
  return !toArg.at(toPath).isEscapingWhenWalkingDown(using: visitor, context)
}

