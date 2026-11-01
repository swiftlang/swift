//===--- BoxAccessSummary.swift - Accesses to a box during a call --------===//
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
//
// This is a cross-function utility that recursively analyzes all uses of a box (an alloc_box, or a box argument) into
// nested closures and local functions.
//
// Note: If this was a proper inter-procedural analysis, similar to AccessSummaryAnalysis (I'm not sure how to do that
// in SwiftCompilerSources), then we could operate on a single function at a time, avoid recursing into
// FunctionArguments, and use efficient intrusive data types like BasicBlockSet. This isn't a serious problem because
// the recursion depth is very shallow (1-2 functions) and set sizes are small. Furthermore, this analysis is only
// relevant within a closure group, so no information could be reused across multiple runs of a closure group pass.
//
//===----------------------------------------------------------------------===//

import SIL

/// Whether something happens during a call of a function.
enum PathSummary {
  /// It never happens.
  case noPaths
  /// It happens on every path that returns, throws or unwinds. Paths that trap or loop forever don't count: the call
  /// never completes on them.
  case allPaths
  /// It happens on some paths, or it can't be determined.
  case undecided
}

/// The accesses to a box argument during a call of its function.
struct BoxArgumentSummary {
  /// The strongest kind of access during the call: `.modify` if any access is not a read. Nil if there are no accesses.
  let kind: BeginAccessInst.AccessKind?

  /// Where the accesses of `kind` happen.
  ///
  /// Accesses of a weaker kind are ignored. With `.read`, an access in progress in the caller either never conflicts (a
  /// read) or conflicts with every access (a modify). With `.modify`, a modify on every path conflicts with any access
  /// in progress. A read on every path combined with a modify on some paths is undecided, because a read in progress
  /// only conflicts on some paths.
  let paths: PathSummary

  static let none = BoxArgumentSummary(kind: nil, paths: .noPaths)
  static let unknown = BoxArgumentSummary(kind: .modify, paths: .undecided)
}

/// Summarizes the accesses to a box, and the invocations of a closure argument, during a call of the function that
/// takes it.
///
/// The accesses to a box argument are those through its projections, through the callees that the box is passed to, and
/// through the closures that capture it. A use that isn't understood makes the summary `.unknown`. An access to a
/// stored property or tuple element counts as an access to the entire boxed value.
final class BoxAccessSummary {
  /// Used to classify the uses of box projections (`AddressUseVisitor`).
  let context: Context

  private let boxArgumentCache = FunctionArgumentCache<BoxArgumentSummary>()
  private let closureArgumentCache = FunctionArgumentCache<PathSummary>()

  init(_ context: Context) {
    self.context = context
  }

  /// Return true if the accesses to `allocBox` can be checked statically after being promoted to a stack address: every
  /// call in its function that it reaches is decided (condition 1 of `BoxValueWalker`), it doesn't reach any call
  /// through two captures (condition 2), and every use is understood. This is the entry point for
  /// MandatoryAllocBoxToStack, but could be called from a different pass in the future.
  func isStaticallyCheckable(_ allocBox: AllocBoxInst) -> Bool {
    var walker = BoxValueWalker(self, allocBox)
    return walker.walk(allocBox)
  }

  /// The accesses to box argument `arg` during a call of its function, or `.unknown` if its uses are not statically
  /// checkable (see `isStaticallyCheckable` and `BoxValueWalker.reach`).
  func boxArgument(_ arg: FunctionArgument) -> BoxArgumentSummary {
    assert(arg.type.isBox, "expected a box argument")
    return boxArgumentCache.result(for: arg, recursive: .unknown) {
      var walker = BoxValueWalker(self, arg)
      return walker.walk(arg) ? walker.accesses() : .unknown
    }
  }

  /// Whether closure argument `arg` is invoked during a call of its function.
  func closureArgumentInvocation(_ arg: FunctionArgument) -> PathSummary {
    assert(arg.type.isLoweredFunction, "expected a closure argument")
    return closureArgumentCache.result(for: arg, recursive: .undecided) {
      computeClosureArgumentInvocation(arg)
    }
  }

  private func computeClosureArgumentInvocation(_ arg: FunctionArgument) -> PathSummary {
    var invocations = PathCollector()
    arg.visitClosureUses { use in
      let user = use.instruction
      if user is DestroyValueInst || user is EndBorrowInst || user is DebugValueInst
          || user is DeallocStackInst || user is DestroyNotEscapedClosureInst {
        return
      } else if let apply = user as? FullApplySite, apply.isCallee(operand: use) {
        invocations.add(known: user.parentBlock)
      } else if let apply = user as? FullApplySite, let callee = apply.referencedFunction, callee.isDefinition,
                let calleeArg = apply.calleeArgument(of: use, in: callee), calleeArg.type.isLoweredFunction {
        switch closureArgumentInvocation(calleeArg) {
        case .noPaths:
          break
        case .allPaths:
          invocations.add(known: user.parentBlock)
        case .undecided:
          invocations.addPotential()
        }
      } else {
        // An unknown use may invoke the closure.
        invocations.addPotential()
      }
    }
    return invocations.paths(in: arg.parentFunction)
  }
}

/// Memoizes a result per function argument. A request for an argument whose result is being computed is a recursive
/// call, which can't be summarized.
private final class FunctionArgumentCache<Result> {
  private var entries = [FunctionArgument: Result?]()

  func result(for arg: FunctionArgument, recursive: Result, _ compute: () -> Result) -> Result {
    if let cached = entries[arg] {
      return cached ?? recursive
    }
    entries[arg] = .some(nil)
    let result = compute()
    entries[arg] = result
    return result
  }
}

/// Collects where something happens in a function: the blocks where it is known to happen, and whether it may also
/// happen somewhere that isn't known.
private struct PathCollector {
  private var knownBlocks = Set<BasicBlock>()
  private var hasPotential = false

  var hasAccesses: Bool { hasPotential || !knownBlocks.isEmpty }

  mutating func add(known block: BasicBlock) {
    knownBlocks.insert(block)
  }

  mutating func addPotential() {
    hasPotential = true
  }

  func paths(in function: Function) -> PathSummary {
    if hasPotential {
      return .undecided
    }
    if knownBlocks.isEmpty {
      return .noPaths
    }
    return allPathsReach(knownBlocks, in: function) ? .allPaths : .undecided
  }
}

/// Return true if no path from the entry returns, throws or unwinds without passing through a block in `blocks`. A path
/// that traps or loops forever doesn't count. This uses a Swift set rather than a pass context's block set, so it works
/// for any function.
private func allPathsReach(_ blocks: Set<BasicBlock>, in function: Function) -> Bool {
  var worklist = [function.entryBlock]
  var visited = Set<BasicBlock>(worklist)
  while let block = worklist.popLast() {
    if blocks.contains(block) || block.hasNoReturnCall {
      continue
    }
    if block.terminator.isFunctionExiting {
      return false
    }
    for succ in block.successors where visited.insert(succ).inserted {
      worklist.append(succ)
    }
  }
  return true
}

private extension BasicBlock {
  /// Return true if a call that never returns ends the path through this block. Before NoReturnFolding, such a call
  /// need not be followed by `unreachable`.
  var hasNoReturnCall: Bool {
    instructions.contains { ($0 as? FullApplySite)?.isCalleeNoReturn ?? false }
  }
}

private extension Value {
  /// Visit the uses of this closure value, looking through the instructions that forward it
  /// (`Operand.forwardedClosure`).
  func visitClosureUses(_ visit: (Operand) -> ()) {
    var worklist: [Value] = [self]
    var visited = Set<HashableValue>([self.hashable])
    while let value = worklist.popLast() {
      for use in value.uses {
        if let forwarded = use.forwardedClosure {
          if visited.insert(forwarded.hashable).inserted {
            worklist.append(forwarded)
          }
        } else {
          visit(use)
        }
      }
    }
  }
}

private extension Operand {
  /// The result of the instruction that forwards a closure value from this operand, if any. As with boxes, look past
  /// copy, borrow or move. But also handle function conversion or `mark_dependence`.
  var forwardedClosure: Value? {
    switch instruction {
    case is BeginBorrowInst, is CopyValueInst, is MoveValueInst, is ConvertFunctionInst,
         is ConvertEscapeToNoEscapeInst:
      return (instruction as! SingleValueInstruction)
    case let markDependence as MarkDependenceInst:
      return self == markDependence.valueOperand ? markDependence : nil
    default:
      return nil
    }
  }
}

/// Walks the uses of a box value, an alloc_box or a box argument, in its function: its projections, the calls it is
/// passed to, and the calls of the closures formed in the function that capture it. It decides whether the accesses to
/// the box can be checked statically once the mandatory pass promotes it, and summarizes the accesses for composing
/// into a caller. The callees and the closure bodies are covered by their own summaries (`BoxAccessSummary`), which
/// also guarantee condition 2 for the calls in them.
///
/// Promotion passes the box's address to its callees and to the closures that capture it as `@inout_aliasable`, and
/// makes the box's accesses static. DiagnoseStaticExclusivity then treats those captures as non-escaping: an access
/// that *may* happen during a call that passes the box or closure conflicts with an access in progress at that call.
///
/// Static enforcement must be equivalent to dynamic enforcement: it may only reject a program that dynamic enforcement
/// rejects every time it runs. So the box is only promoted if, at every call it reaches:
/// 1. the box is accessed during the call either on all paths or on no paths. Report exactly the conflicts that dynamic
///    enforcement reports every time the call executes, whatever access is in progress at the call. SIL requires the
///    same accesses to be in progress on every path to an instruction, so an access that encloses the call encloses it
///    whenever the call executes.
/// 2. the box doesn't reach the call through two captures (`reach`).
///
/// TODO: Move this into AccessEnforcementSelection once that pass is rewritten in Swift, then diagnose exclusivity
/// before AllocBoxToStack.
private struct BoxValueWalker {
  private let summary: BoxAccessSummary
  private let function: Function
  private var reads = PathCollector()
  private var modifies = PathCollector()

  /// For each closure value formed in the function that captures the box, and its copies, borrows and moves, the
  /// closure function's box argument.
  private var closureCaptures = [HashableValue: FunctionArgument]()

  /// The first capture of the box that reaches each call in the function, and whether it is promoted. See `reach`.
  private var reachableUses = [Instruction: Bool]()

  /// False once a call is found where condition 1 or 2 doesn't hold, or a use that isn't supported, such as a closure
  /// captured by another closure.
  private var isCheckable = true

  init(_ summary: BoxAccessSummary, _ root: Value) {
    self.summary = summary
    self.function = root.parentFunction
  }

  /// The accesses to the box that `walk` found.
  func accesses() -> BoxArgumentSummary {
    if modifies.hasAccesses {
      return BoxArgumentSummary(kind: .modify, paths: modifies.paths(in: function))
    }
    if reads.hasAccesses {
      return BoxArgumentSummary(kind: .read, paths: reads.paths(in: function))
    }
    return .none
  }

  private mutating func recordAccess(_ kind: BeginAccessInst.AccessKind, at block: BasicBlock) {
    if kind == .read { reads.add(known: block) } else { modifies.add(known: block) }
  }

  /// Condition 1: check that the box is accessed during a call on all paths or on no paths.
  private mutating func recordCalleeAccesses(_ accesses: BoxArgumentSummary, at block: BasicBlock) {
    guard let kind = accesses.kind else {
      return
    }
    switch accesses.paths {
    case .noPaths:
      // Unreachable: a summary with a kind always has accesses.
      break
    case .allPaths:
      recordAccess(kind, at: block)
    case .undecided:
      isCheckable = false
    }
  }

  /// Condition 2: the box must not reach the same call through two captures, if one of them is promoted (a box argument
  /// or a closure that captures the box).
  ///
  /// DiagnoseStaticExclusivity checks a call against the accesses in progress in the caller. It never checks two
  /// arguments of the same call against each other: inside the callee, two arguments are distinct storage. For example:
  ///
  ///     func test() {
  ///       var x = 0
  ///       func outer(_ c: () -> ()) {    // captures x: outer(c, x)
  ///         modify(&x) { c() }           // modifies x through outer's capture, calls c meanwhile
  ///       }
  ///       outer { x += 1 }               // c also captures x
  ///     }
  ///
  /// Once x is promoted, outer's capture and c's capture are both `@inout_aliasable` addresses of x and their accesses
  /// are static. The call to outer has no access in progress in test, and in outer, `c` is an argument whose captures
  /// can't be seen. So the conflict is neither diagnosed nor checked at run time. As a box, both accesses are dynamic
  /// and the conflict traps.
  ///
  /// For closures that are non-escaping in the language, this can't happen because of the Non-Escaping Recursion
  /// Restriction (SE-0176): a non-escaping closure may not be invoked during a non-escaping closure that captures the
  /// same variable. DiagnoseInvalidEscapingCaptures diagnoses it before DiagnoseStaticExclusivity runs, conservatively,
  /// by rejecting a call to a non-escaping function parameter that passes a non-escaping function parameter. That
  /// restriction is what lets DiagnoseStaticExclusivity ignore aliasing between arguments, and why two captures of the
  /// box's address alone are fine. It doesn't apply to local functions and escaping closures, whose captures are boxes,
  /// so it doesn't apply to the captures this pass promotes. Not promoting a box that reaches a call twice stands in
  /// for it. The walk of each callee enforces the same rule for the calls in the callee.
  private mutating func recordReachedCall(_ call: ApplySite, isPromoted: Bool) {
    if let isPromotedBefore = reachableUses[call] {
      if isPromotedBefore || isPromoted {
        isCheckable = false
      }
      return
    }
    reachableUses[call] = isPromoted
  }

  /// Walk the uses of `root`. Return true if they can be checked statically.
  mutating func walk(_ root: Value) -> Bool {
    // The box and the closure values (`partial_apply`) that capture it.
    var worklist: [Value] = [root]
    var visited = Set<HashableValue>([root.hashable])
    func push(_ value: Value) {
      if visited.insert(value.hashable).inserted {
        worklist.append(value)
      }
    }
    while let value = worklist.popLast() {
      for use in value.uses {
        switch use.instruction {
        case is BeginBorrowInst, is MarkUninitializedInst, is CopyValueInst, is MoveValueInst:
          // A copy, borrow, move or `mark_uninitialized` of a box or closure value refers to the same box or closure.
          // This must match what `canPromote` and `rewriteUses` in AllocBoxToStack handle.
          let result = use.instruction as! SingleValueInstruction
          if let capture = closureCaptures[value.hashable] {
            closureCaptures[result.hashable] = capture
          }
          push(result)
        case let projectBox as ProjectBoxInst:
          visit(projectBox)
        case is DestroyValueInst, is EndBorrowInst, is DebugValueInst, is DeallocStackInst, is DeallocBoxInst:
          break
        case let apply as ApplySite:
          if let partialApply = visit(apply, use) {
            push(partialApply)
          }
        default:
          // The box may be accessed through an unknown use.
          isCheckable = false
        }
      }
    }
    return isCheckable
  }

  /// A projection of the box. Record its accesses, and the calls that its address reaches directly or through a
  /// non-escaping closure that captures it. An address within an access, such as an `inout` argument, is an access in
  /// progress at the call, not a capture.
  private mutating func visit(_ projectBox: ProjectBoxInst) {
    var visitor = BoxProjectionAccessVisitor(summary.context)
    visitor.visit(projectBox)
    for access in visitor.accesses {
      recordAccess(access.accessKind, at: access.parentBlock)
    }
    for call in visitor.reachedCalls {
      recordReachedCall(call, isPromoted: false)
      // An address passed to a call may be accessed during the call. (Only used for a box argument's summary; an
      // alloc_box doesn't need its accesses.)
      modifies.addPotential()
    }
    if visitor.hasUnknownUse {
      isCheckable = false
    }
  }

  /// A use of the box, or of a closure that captures it, by `apply`. Return the `partial_apply` to follow if `apply`
  /// forms a closure that captures the box.
  private mutating func visit(_ apply: ApplySite, _ use: Operand) -> PartialApplyInst? {
    recordReachedCall(apply, isPromoted: true)
    if apply.isCallee(operand: use) {
      // A call of a closure that captures the box.
      guard let capture = closureCaptures[use.value.hashable], apply is FullApplySite else {
        isCheckable = false
        return nil
      }
      recordCalleeAccesses(summary.boxArgument(capture), at: apply.parentBlock)
      return nil
    }
    guard let callee = apply.referencedFunction, callee.isDefinition,
          let calleeArg = apply.calleeArgument(of: use, in: callee)
    else {
      isCheckable = false
      return nil
    }
    if use.value.type.isBox {
      if let partialApply = apply as? PartialApplyInst {
        // A closure that captures the box. Its accesses happen at the calls that use it.
        closureCaptures[partialApply.hashable] = calleeArg
        return partialApply
      }
      // The box is passed to a callee.
      recordCalleeAccesses(summary.boxArgument(calleeArg), at: apply.parentBlock)
      return nil
    }
    // A closure that captures the box is passed to a callee. A closure captured by another closure is not supported.
    guard let capture = closureCaptures[use.value.hashable], apply is FullApplySite else {
      isCheckable = false
      return nil
    }
    // DiagnoseStaticExclusivity treats a closure argument as accessed at the call, whether or not the callee calls it.
    // The access happens on all paths only if the callee always calls it.
    var accesses = summary.boxArgument(capture)
    if accesses.paths == .allPaths, summary.closureArgumentInvocation(calleeArg) != .allPaths {
      accesses = BoxArgumentSummary(kind: accesses.kind, paths: .undecided)
    }
    recordCalleeAccesses(accesses, at: apply.parentBlock)
    return nil
  }
}

/// Visits the uses of a box projection's address, transitively through address projections, and records the accesses to
/// the box and the calls its address reaches. Any use that isn't understood sets `hasUnknownUse`, so callers treat the
/// box as accessed or captured in an unknown way.
private struct BoxProjectionAccessVisitor: AddressUseVisitor {
  let context: Context
  /// begin_access (not [unsafe]) on the projection or a projection of it.
  private(set) var accesses = [BeginAccessInst]()
  /// Calls the address reaches: full applies that take it, non-escaping partial_applies that capture it, and the calls
  /// of those closures (`addCalls`).
  private(set) var reachedCalls = [ApplySite]()
  private(set) var hasUnknownUse = false

  private var addresses = [Value]()
  private var visited = Set<HashableValue>()

  init(_ context: Context) {
    self.context = context
  }

  /// Visit all uses of `projectBox`.
  mutating func visit(_ projectBox: ProjectBoxInst) {
    push(projectBox)
    while let address = addresses.popLast() {
      for use in address.uses {
        if classifyAddress(operand: use) == .abortWalk {
          hasUnknownUse = true
        }
      }
    }
  }

  private mutating func push(_ address: Value) {
    if visited.insert(address.hashable).inserted {
      addresses.append(address)
    }
  }

  // An access to a part of the variable counts as an access to the whole variable.
  mutating func projectedAddressUse(of operand: Operand, into value: Value) -> WalkResult {
    push(value)
    return .continueWalk
  }

  mutating func scopedAddressUse(of operand: Operand) -> WalkResult {
    // The address within an access, such as an inout argument, is the access in progress, not a capture, so don't
    // follow it.
    if let access = operand.instruction as? BeginAccessInst, access.enforcement != .unsafe {
      accesses.append(access)
    }
    return .continueWalk
  }

  mutating func appliedAddressUse(of operand: Operand, by apply: FullApplySite) -> WalkResult {
    reachedCalls.append(apply)
    return .continueWalk
  }

  // Uses that neither access the box nor capture its address: ending a scope, initialization and other stores, and
  // loading, which copies the value and not the address.
  mutating func scopeEndingAddressUse(of operand: Operand) -> WalkResult { .continueWalk }
  mutating func leafAddressUse(of operand: Operand) -> WalkResult { .continueWalk }
  mutating func loadedAddressUse(of operand: Operand, intoValue value: Value) -> WalkResult { .continueWalk }
  mutating func loadedAddressUse(of operand: Operand, intoAddress address: Operand) -> WalkResult { .continueWalk }

  mutating func yieldedAddressUse(of operand: Operand) -> WalkResult { .abortWalk }

  mutating func dependentAddressUse(of operand: Operand, dependentValue value: Value) -> WalkResult {
    if let partialApply = value as? PartialApplyInst {
      reachedCalls.append(partialApply)
      addCalls(of: partialApply)
    } else if value.type.isLoweredFunction {
      addCalls(of: value)
    } else {
      // The dependent value may be used to access the box in ways that aren't tracked.
      return .abortWalk
    }
    return .continueWalk
  }

  /// Add the calls that `closure` is forwarded to, either as the callee or as an argument.
  private mutating func addCalls(of closure: Value) {
    closure.visitClosureUses { use in
      if let apply = use.instruction as? FullApplySite {
        reachedCalls.append(apply)
      }
    }
  }

  mutating func dependentAddressUse(of operand: Operand, dependentAddress address: Value) -> WalkResult {
    .abortWalk
  }

  mutating func escapingAddressUse(of operand: Operand) -> WalkResult { .abortWalk }

  mutating func unknownAddressUse(of operand: Operand) -> WalkResult { .abortWalk }
}
