//===--- BorrowAccessorFixup.swift ----------------------------------------===//
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
import OptimizerBridging

/// SILGen produces the result of a borrow accessor from within local borrow scopes, and uses
/// `unchecked_ownership` to silence the ownership verifier:
/// ```
///   %ld = load_borrow %self
///   %fwd = unchecked_ownership %ld
///   %ex = struct_extract %fwd, #Struct.storedProperty
///   end_borrow %ld
///   return %ex
/// ```
/// Transform this into valid OSSA:
/// ```
///   %ld = load_borrow %self
///   %ex = struct_extract %ld, #Struct.storedProperty
///   return_borrow %ex from_scopes (%ld)
/// ```
/// A trivial result has no enclosing scope, so only the `unchecked_ownership` instructions are removed.
///
/// A `@guaranteed_address` or `@inout` address result that is derived from local borrow scopes are
/// handled the same way:
/// ```
///   %ld = load_borrow %self
///   %fwd = unchecked_ownership %ld
///   %addr = apply %borrowAccessor(%fwd)
///   end_borrow %ld
///   return %addr
/// ```
/// ->
/// ```
///   %ld = load_borrow %self
///   %addr = apply %borrowAccessor(%ld)
///   return_borrow %addr from_scopes (%ld)
/// ```
///
/// Returns true if the function changed.
func fixupBorrowAccessors(in function: Function, _ context: FunctionPassContext) -> Bool {
  guard function.convention.hasGuaranteedResult || function.convention.hasAddressResult else {
    return false
  }

  var changed = false
  for inst in function.instructions {
    if let uncheckedOwnership = inst as? UncheckedOwnershipInst {
      uncheckedOwnership.replace(with: uncheckedOwnership.operand.value, context)
      changed = true
    }
  }

  // A function that never returns has no result to enclose.
  guard let returnInst = function.returnInstruction as? ReturnInst else {
    return changed
  }

  let enclosingScopes = getEnclosingLocalScopes(of: returnInst.returnedValue, context)
  if enclosingScopes.isEmpty {
    return changed
  }
  for scope in enclosingScopes {
    assert(!scope.scopeEndingOperands.isEmpty)
    for scopeEnd in scope.scopeEndingOperands {
      // SILGen does not emit reborrows, so every scope ends with an end_borrow.
      assert(scopeEnd.instruction is EndBorrowInst)
      context.erase(instruction: scopeEnd.instruction)
    }
  }

  // The scopes are ordered from inner to outer.
  let builder = Builder(before: returnInst, context)
  builder.createReturnBorrow(of: returnInst.returnedValue, fromScopes: enclosingScopes.map { $0.value })
  context.erase(instruction: returnInst)
  return true
}

/// Returns the local borrow scopes that enclose `value`, ordered from inner to outer.
private func getEnclosingLocalScopes(of value: Value, _ context: FunctionPassContext) -> [BeginBorrowValue] {
  if value.type.isAddress {
    return enclosingObjects(of: value).flatMap { getEnclosingLocalScopes(of: $0, context) }
  }
  var scopes = [BeginBorrowValue]()
  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushIfNotVisited(value)
  while let current = worklist.pop() {
    // A trivial value, such as the dereference_borrow of a trivial type, has no scope.
    if current.ownership == .guaranteed, let scope = BeginBorrowValue(current), scope.hasLocalScope {
      scopes.append(scope)
    }
    worklist.pushIfNotVisited(contentsOf: current.getEnclosingValues(context))
  }
  // Walking up visits inner scopes before outer scopes.
  return scopes
}

/// The objects which `address` is only valid within, ordered from inner to outer.
private func enclosingObjects(of address: Value) -> [Value] {
  let accessBaseAndScopes = address.accessBaseWithScopes
  var objects = [Value]()
  // The scopes are ordered from inner to outer.
  for case let .dependence(markDep) in accessBaseAndScopes.scopes {
    if markDep.base.type.isAddress {
      objects.append(contentsOf: enclosingObjects(of: markDep.base))
    } else {
      objects.append(markDep.base)
    }
  }
  switch accessBaseAndScopes.base {
  case let .borrowMutateAccessor(apply):
    if let selfArgument = apply.selfArgument {
      if selfArgument.type.isAddress {
        objects.append(contentsOf: enclosingObjects(of: selfArgument))
      } else {
        objects.append(selfArgument)
      }
    }
  case let .box(projectBox):
    objects.append(projectBox.box)
  case let .class(refElementAddr):
    objects.append(refElementAddr.instance)
  case let .tail(refTailAddr):
    objects.append(refTailAddr.instance)
  case .stack, .global, .argument, .yield, .storeBorrow, .pointer, .index, .unidentified:
    break
  }
  return objects
}

func registerBorrowAccessorFixup() {
  BridgedOptimizerUtilities.registerBorrowAccessorFixup(
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction) -> Bool in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      return fixupBorrowAccessors(in: bridgedFunction.function, context)
    }
  )
}
