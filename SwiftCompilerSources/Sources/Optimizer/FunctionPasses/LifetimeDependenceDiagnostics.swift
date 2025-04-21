//===--- LifetimeDependenceDiagnostics.swift - Lifetime dependence --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Pass dependencies:
///
/// - After MoveOnly checking fixes non-Copyable lifetimes.
///
/// - Before MoveOnlyTypeEliminator removes ownership operations on trivial types, which loses variable information
/// required for diagnostics.
///
//===----------------------------------------------------------------------===//

import AST
import SIL

private let verbose = false

private func log(prefix: Bool = true, _ message: @autoclosure () -> String) {
  if verbose {
    debugLog(prefix: prefix, message())
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
  log(prefix: false, "\n--- Diagnosing lifetime dependence in \(function.name)")
  log("\(function)")
  log("\(function.convention)")

  for argument in function.arguments
      where !argument.type.isEscapable(in: function)
  {
    // Indirect results are not checked here. Type checking ensures
    // that they have a lifetime dependence.
    if let lifetimeDep = LifetimeDependence(argument, context) {
      _ = analyze(dependence: lifetimeDep, context)
    }
  }
  for instruction in function.instructions {
    if let markDep = instruction as? MarkDependenceInst, markDep.isUnresolved {
      if let lifetimeDep = LifetimeDependence(markDep, context) {
        if analyze(dependence: lifetimeDep, context) {
          // Note: This promotes the mark_dependence flag but does not invalidate analyses; preserving analyses is good,
          // although the change won't appear in -sil-print-function. Ideally, we could notify context of a flag change
          // without invalidating analyses.
          lifetimeDep.resolve(context)
          continue
        }
      }
      // For now, if the mark_dependence wasn't recognized as a lifetime dependency, or if the dependencies uses are not
      // in scope, conservatively settle it as escaping. For example, it is not uncommon for the pointer value returned
      // by `unsafeAddress` to outlive its `self` argument. This will not be diagnosed as an error, but the
      // mark_dependence will hanceforth be treated as an unknown use by the optimizer.  In the future, we should not
      // need to set this flag during diagnostics because, for escapable types, mark_dependence [unresolved] will all be
      // settled during an early LifetimeNormalization pass.
      markDep.settleToEscaping()
      continue
    }
    if let apply = instruction as? FullApplySite {
      // Handle ~Escapable results that do not have a lifetime dependence. This includes implicit initializers and
      // @_unsafeNonescapableResult.
      apply.resultOrYields.forEach {
        if let lifetimeDep = LifetimeDependence(unsafeApplyResult: $0,
                                                context) {
          _ = analyze(dependence: lifetimeDep, context)
        }
      }
      continue
    }
  }
}

/// Analyze a single Lifetime dependence and trigger diagnostics.
///
/// 1. Compute the LifetimeDependence scope.
///
/// 2. Walk down all dependent values checking that they are within range.
///
/// Return true on success.
private func analyze(dependence: LifetimeDependence, _ context: FunctionPassContext) -> Bool {
  log("Dependence scope:\n\(dependence)")

  if dependence.parentValue.type.objectType.isTrivial(in: dependence.function) {
    // Briefly, some versions of Span in the standard library violated trivial lifetimes; versions of the compiler built
    // at that time simply ignored dependencies on trivial values. For now, disable trivial dependencies to allow newer
    // compilers to build against those older standard libraries. This check is only relevant for ~6 mo (until July
    // 2025).
    if let sourceFileKind = dependence.function.sourceFileKind, sourceFileKind == .interface {
      return true
    }
  }

  // Compute this dependence scope.
  var range = dependence.computeRange(context)
  defer { range?.deinitialize() }

  var error = false
  let diagnostics =
    DiagnoseDependence(dependence: dependence, range: range,
                       onError: { error = true }, context: context)

  // Check each lifetime-dependent use via a def-use visitor
  var walker = DiagnoseDependenceWalker(diagnostics, context)
  defer { walker.deinitialize() }
  let result = walker.walkDown(dependence: dependence)
  // The walk may abort without a diagnostic error.
  assert(!error || result == .abortWalk)
  return result == .continueWalk
}

/// Analyze and diagnose a single LifetimeDependence.
private struct DiagnoseDependence {
  let dependence: LifetimeDependence
  let range: InstructionRange?
  let onError: ()->()
  let context: FunctionPassContext

  var function: Function { dependence.function }

  func diagnose(_ position: SourceLoc?, _ id: DiagID,
                _ args: DiagnosticArgument...) {
    context.diagnosticEngine.diagnose(id, args, at: position)
  }

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
    log("Unknown use: \(operand)\n\(function)")
    reportEscaping(operand: operand)
  }

  func checkInoutResult(argument inoutArg: FunctionArgument) -> WalkResult {
    // Check that the parameter dependence for this inout argument is the same as the current dependence scope.
    if let sourceArg = dependence.scope.parentValue as? FunctionArgument {
      // If the inout result is also the inout source, then it's always ok.
      if inoutArg == sourceArg {
        return .continueWalk
      }
      if function.argumentConventions.getDependence(target: inoutArg.index, source: sourceArg.index) != nil {
        // The inout result depends on a lifetime that is inherited or borrowed in the caller.
        log("  has dependent inout argument: \(inoutArg)")
        return .continueWalk
      }
    }
    return .abortWalk
  }

  func checkStoreToYield(address: Value) -> WalkResult {
    var walker = DependentAddressUseDefWalker(context: context, diagnostics: self)
    return walker.walkUp(address: address)
  }

  func checkYield(operand: Operand) -> WalkResult {
    switch dependence.scope {
    case .caller:
      return checkFunctionResult(operand: operand)
    default:
      // local scopes can be yielded without escaping.
      return .continueWalk
    }
  }

  func checkFunctionResult(operand: Operand) -> WalkResult {

    if function.hasUnsafeNonEscapableResult {
      return .continueWalk
    }
    // If the dependence scope is global, then it has immortal lifetime.
    if case .global = dependence.scope {
      return .continueWalk
    }
    // Check that the parameter dependence for this result is the same
    // as the current dependence scope.
    if let arg = dependence.scope.parentValue as? FunctionArgument,
       function.argumentConventions[resultDependsOn: arg.index] != nil {
      // The returned value depends on a lifetime that is inherited or
      // borrowed in the caller. The lifetime of the argument value
      // itself is irrelevant here.
      log("  has dependent function result")
      return .continueWalk
    }
    return .abortWalk
  }

  func reportError(operand: Operand, diagID: DiagID) {
    // If the dependent value is Escapable, then mark_dependence resolution fails, but this is not a diagnostic error.
    if dependence.dependentValue.isEscapable {
      return
    }
    onError()

    // Identify the escaping variable.
    let escapingVar = LifetimeVariable(dependent: operand.value, context)
    let varName = escapingVar.name
    if let varName {
      diagnose(escapingVar.sourceLoc, .lifetime_variable_outside_scope,
               varName)
    } else {
      diagnose(escapingVar.sourceLoc, .lifetime_value_outside_scope)
    }
    reportScope()
    // Identify the use point.
    let userSourceLoc = operand.instruction.location.sourceLoc
    diagnose(userSourceLoc, diagID)
  }

  // Identify the dependence scope.
  func reportScope() {
    if case let .access(beginAccess) = dependence.scope {
      let parentVar = LifetimeVariable(dependent: beginAccess, context)
      if let sourceLoc = beginAccess.location.sourceLoc ?? parentVar.sourceLoc {
        diagnose(sourceLoc, .lifetime_outside_scope_access,
                 parentVar.name ?? "")
      }
      return
    }
    if let arg = dependence.parentValue as? Argument,
       let varDecl = arg.varDecl,
       let sourceLoc = arg.sourceLoc {
      diagnose(sourceLoc, .lifetime_outside_scope_argument,
               varDecl.userFacingName)
      return
    }
    let parentVar = LifetimeVariable(dependent: dependence.parentValue, context)
    if let parentLoc = parentVar.sourceLoc {
      if let parentName = parentVar.name {
        diagnose(parentLoc, .lifetime_outside_scope_variable, parentName)
      } else {
        diagnose(parentLoc, .lifetime_outside_scope_value)
      }
    }
  }
}

// Identify a best-effort variable declaration based on a defining SIL
// value or any lifetime dependent use of that SIL value.
private struct LifetimeVariable {
  var varDecl: VarDecl?
  var sourceLoc: SourceLoc?
  
  var name: StringRef? {
    return varDecl?.userFacingName
  }

  init(dependent value: Value, _ context: some Context) {
    if value.type.isAddress {
      self = Self(accessBase: value.accessBase, context)
      return
    }
    if let firstIntroducer = getFirstVariableIntroducer(of: value, context) {
      self = Self(introducer: firstIntroducer)
      return
    }
    self.varDecl = nil
    self.sourceLoc = nil
  }

  private func getFirstVariableIntroducer(of value: Value, _ context: some Context) -> Value? {
    var introducer: Value?
    var useDefVisitor = VariableIntroducerUseDefWalker(context, scopedValue: value) {
      introducer = $0
      return .abortWalk
    }
    defer { useDefVisitor.deinitialize() }
    _ = useDefVisitor.walkUp(newLifetime: value)
    return introducer
  }

  private init(introducer: Value) {
    if let arg = introducer as? Argument {
      self.varDecl = arg.varDecl
    } else {
      self.sourceLoc = introducer.definingInstruction?.location.sourceLoc
      self.varDecl = introducer.definingInstruction?.findVarDecl()
    }
    if let varDecl {
      sourceLoc = varDecl.nameLoc
    }
  }

  // Record the source location of the variable decl if possible. The
  // caller will already have a source location for the formal access,
  // which is more relevant for diagnostics.
  private init(accessBase: AccessBase, _ context: some Context) {
    switch accessBase {
    case .box(let projectBox):
      // Note: referenceRoot looks through `begin_borrow [var_decl]` and `move_value [var_decl]`. But the box should
      // never be produced by one of these, except when it is redundant with the `alloc_box` VarDecl. It does not seem
      // possible for a box to be moved/borrowed directly into another variable's box. Reassignment always loads/stores
      // the value.
      self = Self(introducer: projectBox.box.referenceRoot)
    case .stack(let allocStack):
      self = Self(introducer: allocStack)
    case .global(let globalVar):
      self.varDecl = globalVar.varDecl
      self.sourceLoc = varDecl?.nameLoc
    case .class(let refAddr):
      self.varDecl = refAddr.varDecl
      self.sourceLoc = refAddr.location.sourceLoc
    case .tail(let refTail):
      self = Self(introducer: refTail.instance)
    case .argument(let arg):
      self.varDecl = arg.varDecl
      self.sourceLoc = arg.sourceLoc
    case .yield(let result):
      // TODO: bridge VarDecl for FunctionConvention.Yields
      self.varDecl = nil
      self.sourceLoc = result.parentInstruction.location.sourceLoc
    case .storeBorrow(let sb):
      self = .init(dependent: sb.source, context)
    case .pointer(let ptrToAddr):
      self.varDecl = nil
      self.sourceLoc = ptrToAddr.location.sourceLoc
    case .index, .unidentified:
      self.varDecl = nil
      self.sourceLoc = nil
    }
  }
}

/// Walk up an address into which a dependent value has been stored. If any address in the use-def chain is a
/// mark_dependence, follow the dependence base rather than the forwarded value. If any of the dependence bases in
/// within the current scope is with (either local checkInoutResult), then storing a value into that address is
/// nonescaping.
///
/// This supports store-to-yield. Storing to a yield is an escape unless the yielded memory location depends on another
/// lifetime that already depends on the current scope. When setter depends on 'newValue', 'newValue' is stored to the
/// yielded address, and the yielded addrses depends on the lifetime of 'self'. A mark_dependence should have already
/// been inserted for that lifetime depenence:
///
///   (%a, %t) = begin_apply %f(%self)
///              : $@yield_once @convention(method) (@inout Self) -> _inherit(0) @yields @inout Self.field
///   %dep = mark_dependence [nonescaping] %yield_addr on %self
///   store %newValue to [assign] %dep : $*Self.field
///
private struct DependentAddressUseDefWalker {
  let context: Context
  var diagnostics: DiagnoseDependence
}

extension DependentAddressUseDefWalker: AddressUseDefWalker {
  // Follow the dependence base, not the forwarded value. Similar to the way LifetimeDependenceUseDefWalker handles
  // MarkDependenceInst.
  mutating func walkUp(address: Value, path: UnusedWalkingPath = UnusedWalkingPath()) -> WalkResult {
    if let markDep = address as? MarkDependenceInst, let addressDep = LifetimeDependence(markDep, context) {
      switch addressDep.scope {
      case let .caller(arg):
        return diagnostics.checkInoutResult(argument: arg)
      case .owned, .initialized:
        // Storing a nonescaping value to local memory cannot escape.
        return .abortWalk
      default:
        break
      }
    }
    return walkUpDefault(address: address, path: UnusedWalkingPath())
  }

  mutating func rootDef(address: Value, path: UnusedWalkingPath) -> WalkResult {
    // This only searches for mark_dependence scopes.
    return .continueWalk
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
  let context: Context
  var diagnostics: DiagnoseDependence
  let localReachabilityCache = LocalVariableReachabilityCache()
  var visitedValues: ValueSet

  var function: Function { diagnostics.function }
  
  init(_ diagnostics: DiagnoseDependence, _ context: Context) {
    self.context = context
    self.diagnostics = diagnostics
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

  mutating func inoutDependence(argument: FunctionArgument, on operand: Operand) -> WalkResult {
    if diagnostics.checkInoutResult(argument: argument) == .abortWalk {
      diagnostics.reportEscaping(operand: operand)
      return .abortWalk
    }
    return .continueWalk
  }

  mutating func returnedDependence(result: Operand) -> WalkResult {
    if diagnostics.checkFunctionResult(operand: result) == .abortWalk {
      diagnostics.reportEscaping(operand: result)
      return .abortWalk
    }
    return .continueWalk
  }

  mutating func returnedDependence(address: FunctionArgument,
                                   on operand: Operand) -> WalkResult {
    if diagnostics.checkFunctionResult(operand: operand) == .abortWalk {
      diagnostics.reportEscaping(operand: operand)
      return .abortWalk
    }
    return .continueWalk
  }

  mutating func yieldedDependence(result: Operand) -> WalkResult {
    if diagnostics.checkYield(operand: result) == .abortWalk {
      diagnostics.reportEscaping(operand: result)
      return .abortWalk
    }
    return .continueWalk
  }

  mutating func storeToYieldDependence(address: Value, of operand: Operand) -> WalkResult {
    if diagnostics.checkStoreToYield(address: address) == .abortWalk {
      diagnostics.reportEscaping(operand: operand)
      return .abortWalk
    }
    return .continueWalk
  }

  // Override AddressUseVisitor here because LifetimeDependenceDefUseWalker
  // returns .abortWalk, and we want a more useful crash report.
  mutating func unknownAddressUse(of operand: Operand) -> WalkResult {
    diagnostics.reportUnknown(operand: operand)
    return .continueWalk
  }
}
