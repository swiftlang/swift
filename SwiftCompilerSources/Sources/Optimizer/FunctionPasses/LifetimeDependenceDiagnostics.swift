//===--- LifetimeDependenceDiagnostics.swift - Lifetime dependence --------===//
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

import SIL

private let verbose = false

private func log(_ message: @autoclosure () -> String) {
  if verbose {
    print("### \(message())")
  }
}

/// Diagnostic pass.
///
/// Find the roots of all non-escapable values in this function. All
/// non-escapable values either depend on a NonEscapingScope, or they
/// are produced by a LifetimeDependentInstruction that has no
/// dependence on a parent value (@_unsafeNonEscapableResult).
let lifetimeDependenceDiagnosticsPass = FunctionPass(
  name: "lifetime-dependence-diagnostics")
{ (function: Function, context: FunctionPassContext) in
  if !context.options.hasFeature(.NonescapableTypes) {
    return
  }
  log("Diagnosing lifetime dependence in \(function.name)")
  log("\(function)")

  for argument in function.arguments where !argument.type.isEscapable {
    // Indirect results are not checked here. Type checking ensures
    // that they have a lifetime dependence.
    if let lifetimeDep = LifetimeDependence(argument, context) {
      analyze(dependence: lifetimeDep, context)
    }
  }
  for instruction in function.instructions {
    guard let markDep = instruction as? MarkDependenceInst else { continue }
    if let lifetimeDep = LifetimeDependence(markDep, context) {
      analyze(dependence: lifetimeDep, context)
    }
  }
}

/// Analyze a single Lifetime dependence and trigger diagnostics.
///
/// 1. Compute the LifetimeDependence scope.
///
/// 2. Walk down all dependent values checking that they are within range.
private func analyze(dependence: LifetimeDependence,
  _ context: FunctionPassContext) {
  log("Dependence scope:\n\(dependence)")
    
  // Compute this dependence scope.
  var range = dependence.computeRange(context)
  defer { range?.deinitialize() }

  let diagnostics =
    DiagnoseDependence(dependence: dependence, range: range, context: context)

  // Check each lifetime-dependent use via a def-use visitor
  var walker = DiagnoseDependenceWalker(diagnostics, context)
  defer { walker.deinitialize() }
  _ = walker.walkDown(root: dependence.parentValue)
}

/// Analyze and diagnose a single LifetimeDependence.
private struct DiagnoseDependence {
  let dependence: LifetimeDependence
  let range: InstructionRange?
  let context: FunctionPassContext

  var function: Function { dependence.function }

  /// Check that this use is inside the dependence scope.
  func checkInScope(operand: Operand) -> WalkResult {
    if let range, !range.inclusiveRangeContains(operand.instruction) {
      log("  out-of-range: \(operand.instruction)")
      reportError(operand: operand, diagID: .lifetime_outside_scope_use)
      return .abortWalk
    }
    log("  contains: \(operand.instruction)")
    return .continueWalk
  }

  func reportEscaping(operand: Operand) {
    log("  escaping: \(operand.instruction)")
    reportError(operand: operand, diagID: .lifetime_outside_scope_escape)
  }

  func reportUnknown(operand: Operand) {
    standardError.write("Unknown use: \(operand)\n\(function)")
    reportEscaping(operand: operand)
  }

  func checkFunctionResult(operand: Operand) -> WalkResult {
    // TODO: Get the argument dependence for this result. Check that it is the
    // same as the current dependence scope

    if function.hasUnsafeNonEscapableResult {
      return .continueWalk
    }
    // TODO: Take ResultInfo as an argument and provide better
    // diagnostics for missing lifetime dependencies.
    reportEscaping(operand: operand)
    return .abortWalk
  }

  func reportError(operand: Operand, diagID: DiagID) {
    // Identify the escaping variable.
    let escapingVar = LifetimeVariable(dependent: operand.value, context)
    let varName = escapingVar.name
    if let varName {
      context.diagnosticEngine.diagnose(escapingVar.sourceLoc,
        .lifetime_variable_outside_scope,
        varName)
    } else {
      context.diagnosticEngine.diagnose(escapingVar.sourceLoc,
        .lifetime_value_outside_scope)
    }
    // Identify the dependence scope.
    //
    // TODO: add bridging for function argument locations
    // [SILArgument.getDecl().getLoc()]
    //
    // TODO: For clear diagnostics: switch on dependence.scope.
    // For an access, report both the accessed variable, and the access.
    if let parentSourceLoc =
         dependence.parentValue.definingInstruction?.location.sourceLoc {
      context.diagnosticEngine.diagnose(parentSourceLoc,
                                        .lifetime_outside_scope_parent)
    }
    // Identify the use point.
    let userSourceLoc = operand.instruction.location.sourceLoc
    context.diagnosticEngine.diagnose(userSourceLoc, diagID)
  }
}

private extension Instruction {
  func findVarDecl() -> VarDecl? {
    if let varDeclInst = self as? VarDeclInstruction {
      return varDeclInst.varDecl
    }
    for result in results {
      for use in result.uses {
        if let debugVal = use.instruction as? DebugValueInst {
          return debugVal.varDecl
        }
      }
    }
    return nil
  }
}

// Identify a best-effort variable declaration based on a defining SIL
// value or any lifetime dependent use of that SIL value.
private struct LifetimeVariable {
  var varDecl: VarDecl?
  var sourceLoc: SourceLoc?
  
  var name: String? {
    return varDecl?.userFacingName
  }

  init(introducer: Value) {
    if introducer.type.isAddress {
      switch introducer.enclosingAccessScope {
      case let .scope(beginAccess):
        // TODO: report both the access point and original variable.
        self = LifetimeVariable(introducer: beginAccess.operand.value)
        return
      case .base(_):
        // TODO: use an address walker to get the allocation point.
        break
      }
    }
    if let arg = introducer as? Argument {
      self.varDecl = arg.varDecl
    } else {
      self.sourceLoc = introducer.definingInstruction?.location.sourceLoc
      self.varDecl = introducer.definingInstruction?.findVarDecl()
    }
    if let varDecl {
      sourceLoc = varDecl.sourceLoc
    }
  }

  init(dependent value: Value, _ context: Context) {
    // TODO: consider diagnosing multiple variable introducers. It's
    // unclear how more than one can happen.
    var introducers = Stack<Value>(context)
    gatherBorrowIntroducers(for: value, in: &introducers, context)
    if let firstIntroducer = introducers.pop() {
      self = LifetimeVariable(introducer: firstIntroducer)
      return
    }
    self.varDecl = nil
    self.sourceLoc = nil
  }
}

/// Walk down lifetime depenence uses. For each check that all dependent
/// leaf uses are non-escaping and within the dependence scope. The walk
/// starts with add address for .access dependencies. The walk can
/// transition from an address to a value at a load. The walk can
/// transition from a value to an address as follows:
///
///     %dependent_addr = mark_dependence [nonescaping] %base_addr on %value
///
/// TODO: handle stores to singly initialized temporaries like copies using a standard reaching-def analysis.
private struct DiagnoseDependenceWalker {
  let diagnostics: DiagnoseDependence
  let context: Context
  var visitedValues: ValueSet

  var function: Function { diagnostics.function }
  
  init(_ diagnostics: DiagnoseDependence, _ context: Context) {
    self.diagnostics = diagnostics
    self.context = context
    self.visitedValues = ValueSet(context)
  }
  
  mutating func deinitialize() {
    visitedValues.deinitialize()
  }
}

extension DiagnoseDependenceWalker : LifetimeDependenceDefUseWalker {
  mutating func needWalk(for value: Value) -> Bool {
    visitedValues.insert(value)
  }

  mutating func leafUse(of operand: Operand) -> WalkResult {
    return diagnostics.checkInScope(operand: operand)
  }

  mutating func deadValue(_ value: Value, using operand: Operand?)
    -> WalkResult {
    // Ignore a dead root value. It never escapes.
    if let operand {
      return diagnostics.checkInScope(operand: operand)
    }
    return .continueWalk
  }

  mutating func escapingDependence(on operand: Operand) -> WalkResult {
    diagnostics.reportEscaping(operand: operand)
    return .abortWalk
  }

  mutating func returnedDependence(result: Operand) -> WalkResult {
    return diagnostics.checkFunctionResult(operand: result)
  }

  mutating func returnedDependence(address: FunctionArgument,
                                   using operand: Operand) -> WalkResult {
    return diagnostics.checkFunctionResult(operand: operand)
  }

  // Override AddressUseVisitor here because LifetimeDependenceDefUseWalker
  // returns .abortWalk, and we want a more useful crash report.
  mutating func unknownAddressUse(of operand: Operand) -> WalkResult {
    diagnostics.reportUnknown(operand: operand)
    return .continueWalk
  }
}
