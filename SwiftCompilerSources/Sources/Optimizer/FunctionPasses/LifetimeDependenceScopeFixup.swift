//===--- LifetimeDependenceScopeFixup.swift ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//

// For an apply that returns a lifetime dependent value, 
// LifetimeDependenceInsertion inserts mark_dependence [unresolved] on parent
// value's access scope. LifetimeDependenceScopeFixup then extends the access
// scope to cover all uses of the dependent value.

// This pass must run after LifetimeDependenceInsertion and before
// LifetimeDependenceDiagnostics.

import SIL

private let verbose = false

private func log(_ message: @autoclosure () -> String) {
  if verbose {
    print("### \(message())")
  }
}

let lifetimeDependenceScopeFixupPass = FunctionPass(
  name: "lifetime-dependence-scope-fixup")
{ (function: Function, context: FunctionPassContext) in
  log("Scope fixup for lifetime dependence in \(function.name)")

  for instruction in function.instructions {
    guard let markDep = instruction as? MarkDependenceInst else {
      continue
    }
    if let lifetimeDep = LifetimeDependence(markDep, context) {
      fixup(dependence: lifetimeDep, context)
    }
  }
}

private func fixup(dependence: LifetimeDependence,
  _ context: FunctionPassContext) {
  log("Scope fixup for lifetime dependent instructions: \(dependence)")

  guard case .access(let bai) = dependence.scope else {
    return
  }
  var range = InstructionRange(begin: bai, context)
  var walker = LifetimeDependenceScopeFixupWalker(bai.parentFunction, context) {
    range.insert($0.instruction)
    return .continueWalk
  }
  defer {walker.deinitialize()}
  _ = walker.walkDown(root: dependence.dependentValue)
  defer {range.deinitialize()}

  var beginAccess = bai
  while (true) {
    var endAcceses = [Instruction]()
    // Collect original end_access instructions
    for end in beginAccess.endInstructions {
      endAcceses.append(end)
    }

    // Insert original end_access instructions to prevent access scope shortening
    range.insert(contentsOf: endAcceses)
    assert(!range.ends.isEmpty)

    // Create new end_access at the end of extended uses
    for end in range.ends {
      let endBuilder = Builder(after: end, context)
      _ = endBuilder.createEndAccess(beginAccess: beginAccess)
    }

    // Delete original end_access instructions
    for endAccess in endAcceses {
      context.erase(instruction: endAccess)
    }

    // TODO: Add SIL support for lifetime dependence and write unit test
    // for nested access scopes
    guard case let .scope(enclosingBeginAccess) = beginAccess.address.enclosingAccessScope else {
      break
    }
    beginAccess = enclosingBeginAccess
  }
}

private struct LifetimeDependenceScopeFixupWalker : LifetimeDependenceDefUseWalker {
  let function: Function
  let context: Context
  let visitor: (Operand) -> WalkResult
  var visitedValues: ValueSet

  init(_ function: Function, _ context: Context, visitor: @escaping (Operand) -> WalkResult) {
    self.function = function
    self.context = context
    self.visitor = visitor
    self.visitedValues = ValueSet(context)
  }

  mutating func deinitialize() {
    visitedValues.deinitialize()
  }

  mutating func needWalk(for value: Value) -> Bool {
    visitedValues.insert(value)
  }

  mutating func deadValue(_ value: Value, using operand: Operand?)
  -> WalkResult {
    if let operand {
      return visitor(operand)
    }
    return .continueWalk
  }

  mutating func leafUse(of operand: Operand) -> WalkResult {
    return visitor(operand)
  }

  mutating func escapingDependence(on operand: Operand) -> WalkResult {
    _ = visitor(operand)
    return .abortWalk
  }

  mutating func returnedDependence(result: Operand) -> WalkResult {
    return .continueWalk
  }

  mutating func returnedDependence(address: FunctionArgument,
                                   using operand: Operand) -> WalkResult {
    return .continueWalk
  }
}

