//===--- LifetimeResolution.swift -----------------------------------------==//
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

import AST
import SIL

private func log(_ message: @autoclosure () -> String) {
  llvmDebug("lifetime-resolution", message())
}

let lifetimeResolutionPass = FunctionPass(name: "lifetime-resolution") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else { return }

  // Process results in reverse post-order to ensure dependent uses are already resolved.
  // TODO: resolve `.guaranteed` values too. It should amount to having all consuming uses
  //   require a copy, including those on the boundary, and inserting end_access/end_borrow.
  for block in function.blocks.reversed() {
    for inst in block.instructions.reversed() {
      for result in inst.results where result.ownership == .owned {
        resolve(result, context)
      }
    }
    for argument in block.arguments where argument.ownership == .owned {
      resolve(argument, context)
    }
  }
}

private func resolve(_ root: Value, _ context: FunctionPassContext) {
  // TODO: A trivial-typed value has no lifetime to resolve at the moment.
  if root.type.isTrivial(in: root.parentFunction) {
    return
  }

  log("\n\nLifetimeResolution.resolve(\(root))\n\n")

  var uses = PartitionedUses(of: root, context)
  defer { uses.deinitialize() }

  // Step 0: Find and partition uses of the values.
  uses.partitionUses()

  log(uses.description)

  // If there are absolutely no liveness uses, there's nothing to do.
  guard uses.haveLivenessUses() else {
    // TODO: Eventually this pass should ensure a destroy is placed right after the def.
    return
  }

  // Step 1: Resolve where copies are required to ensure there are no consumes before uses.
  var liverange = uses.insertCopies()
  defer { liverange.deinitialize() }


  // Step 2: Insert destroys after non-consuming boundary users, using the same liverange.
  guard !uses.hasUnboundedUse else {
    // TODO: for now, trust SILGen's placement of destroy_value.
    //  We probably should extend liveness until scope-ends.
    log("not placing destroys: liveness is not bounded")
    return
  }

  var hoistableDestroys: InstructionSet = InstructionSet(context)
  defer { hoistableDestroys.deinitialize() }
  hoistableDestroys.insert(contentsOf: uses.lifetimeLimits)

  log("replacing destroys: \(hoistableDestroys)")
  placeDestroys(of: root, atBoundaryOf: liverange, reusing: &hoistableDestroys, context)
}

private struct PartitionedUses: CustomStringConvertible {
  // The uses of this value that are partitioned.
  let root: Value

  let context: FunctionPassContext

  // Lifetime delimiting instructions.
  //
  // These instructions represent the limits of permitted liveness for the value,
  // if otherwise not consumed upon reaching the instruction.
  // TODO: these should be some sort of new end_scope instruction that are used to limit how late
  //  a destroy/end_access can be inserted.
  var lifetimeLimits: Stack<DestroyValueInst>

  // Uses that require ownership of the value (not guaranteed).
  var consumes: Stack<Operand>

  // These are indirect uses of the root, including deinit barriers and dependent uses.
  var indirectUses: Stack<Instruction>

  // Uses that otherwise do not fit into the other buckets.
  var uses: Stack<Operand>

  // Set when some use's contribution to liveness could not be bounded, so that liveness must be
  // fully extended to lifetimeLimits, rather than hoisted above an unseen use.
  var hasUnboundedUse = false

  let localReachabilityCache = LocalVariableReachabilityCache()

  init(of root: Value, _ context: FunctionPassContext) {
    self.root = root
    self.context = context
    self.consumes = Stack(context)
    self.lifetimeLimits = Stack(context)
    self.uses = Stack(context)
    self.indirectUses = Stack(context)
  }

  mutating func deinitialize() {
    consumes.deinitialize()
    lifetimeLimits.deinitialize()
    uses.deinitialize()
    indirectUses.deinitialize()
  }

  // After partitioning, are there any non-destroy uses?
  func haveLivenessUses() -> Bool { !uses.isEmpty || !indirectUses.isEmpty || !consumes.isEmpty }

  var description: String {
    return """
           PartitionedUses of: \(root)) [
             uses = \(uses)
             consumes = \(consumes)
             indirectUses = \(indirectUses)
             hasUnboundedUse = \(hasUnboundedUse)
           ]
           """
  }

  mutating func partitionUses() {
    // TODO: should we use an InteriorUseWalker or some other robust walker?
    for use in root.uses {
      switch (use.ownership) {
      case .destroyingConsume:
        fallthrough
      case .forwardingConsume:
        // Perhaps Operand.isScopeEndingUse or Operand.endsLifetime is also useful here?
        if let destroy = use.instruction as? DestroyValueInst {
          lifetimeLimits.append(destroy)
          continue
        }

        consumes.append(use)

      default:
        collectUse(use)
      }
    }

    // Only lexical roots need to include deinit barriers.
    // TODO: study https://gist.github.com/atrick/cc03c4d07fb0a7bee92c223ae5e5695b and the current implementation
    //   to tailor destroy insertion correctly for non-copyable types.
    if root.isInLexicalLiverange(context) {
      addDeinitBarriers(of: root)
    }
  }

  // A use that opens a scope keeps the value live until that scope closes, so record the
  // scope-ending uses in its place: they post-dominate the opening use, so they alone
  // delimit the liveness it contributes.
  private mutating func collectUse(_ use: Operand) {
    let borrowInst = BorrowingInstruction(use.instruction)
    if let borrowInst = borrowInst,
       collectScopeEnds(of: borrowInst) {
      return
    }

    uses.append(use)

    if let markDep = use.instruction as? MarkDependenceInstruction,
       use == markDep.baseOperand {
      collectDependentUses(of: markDep)
    } else if borrowInst != nil {
      // The scope can't be determined from lifetime-ending uses and isn't a dependence we can
      // walk, so liveness past this use is unknown.
      hasUnboundedUse = true
    }
  }

  // Records the instructions closing borrowInst's scope. Returns true if we were able to find
  // all scope ends. Otherwise, there may be escaping dependency or unhandled mark_dependence.
  private mutating func collectScopeEnds(of borrowInst: BorrowingInstruction) -> Bool {
    // TODO: remove this stack by changing visitScopeEndingOperands to take a non-escaping
    // closure, as visitInnerBorrowUses also wants.
    var ends = Stack<Instruction>(context)
    defer { ends.deinitialize() }
    let result = borrowInst.visitScopeEndingOperands(context) {
      ends.push($0.instruction)
      return .continueWalk
    }
    guard result == .continueWalk else {
      return false
    }
    indirectUses.append(contentsOf: ends)
    return true
  }

  // The value depending on `markDep` keeps the root alive through its own uses, which are not
  // uses of the root. Record them so that liveness respects them.
  private mutating func collectDependentUses(of markDep: MarkDependenceInstruction) {
    guard let dependence = LifetimeDependence(markDep, context) else {
      hasUnboundedUse = true
      return
    }
    // The walker only tracks ~Escapable and @noescape dependents; for anything else it reports
    // success having collected nothing.
    guard !dependence.dependentValue.mayEscape else {
      hasUnboundedUse = true
      return
    }
    var dependentUses = Stack<Instruction>(context)
    defer { dependentUses.deinitialize() }
    var walker = LifetimeDependentUseWalker(root.parentFunction, localReachabilityCache, context) {
      dependentUses.push($0)
      return .continueWalk
    }
    defer { walker.deinitialize() }
    if walker.walkDown(dependence: dependence) == .abortWalk {
      hasUnboundedUse = true
    }
    indirectUses.append(contentsOf: dependentUses)
  }

  private mutating func addDeinitBarriers(of root: Value) {
    var liverange = InstructionRange(for: root, context)
    defer { liverange.deinitialize() }

    liverange.insert(contentsOf: consumes.users)
    liverange.insert(contentsOf: uses.users)

    collectDeinitBarriers(into: &indirectUses, liverange: liverange,
      lifetimeLimits: lifetimeLimits, def: root, context)
  }

  // We do this by computing the LiveRange of only the non-destroy / non-scope-ending
  // uses. This lets us see which consuming uses are within the live range, rather
  // than on the boundary of its liveness (i.e., last use). Those inner consuming uses
  // are exactly where copies are required.
  //
  //                  ┌─────────┐
  //               ┌──┼ x = ... ┼──┐
  //               │  └─────────┘  │
  //               │               │
  //            ┌──▼───────┐  ┌────▼─────┐
  // boundary ─►│consume(x)│  │consume(x)│
  //            └──────┬───┘  │use(x)    │◄─ boundary
  //                   │      └─┬────────┘
  //                   │        │
  //                  ┌▼────────▼┐
  //                  │destroy(x)│
  //                  └──────────┘
  //
  //                       │  After copy resolution, consumes appear
  //                       │  only on the boundary, if at all.
  //                       ▼
  //
  //                  ┌─────────┐
  //               ┌──┼ x = ... ┼──┐
  //               │  └─────────┘  │
  //               │               │
  //            ┌──▼───────┐  ┌────▼──────┐
  // boundary ─►│consume(x)│  │y = copy(x)│
  //            └──────┬───┘  │consume(y) │
  //                   │      │use(x)     │◄─ boundary
  //                   │      └─┬─────────┘
  //                   │        │
  //                  ┌▼────────▼┐
  //                  │destroy(x)│
  //                  └──────────┘
  //
  // Crucially, the live range of certain roots includes deinit barriers,
  // which causes some final consumes to need a copy anyway:
  //
  // Liveness before insertion for some lexical roots:
  //       ┌─────────┐
  //    ┌──┼ x = ... ┼──┐
  //    │  └─────────┘  │
  //    │               │
  // ┌──▼───────┐  ┌────▼─────┐
  // │consume(x)│  │use(x)    │
  // └──────┬───┘  └─┬────────┘
  //        │        │
  //        │        │
  //       ┌▼────────▼┐
  //       │barrier() │ ◄─ liveness boundary of x
  //       │destroy(x)│
  //       └──────────┘
  //
  //
  // Since the consume is interior with respect to this
  // deinit-barrier extended liveness, it consumes a copy instead:
  //        ┌─────────┐
  //     ┌──┼ x = ... ┼──┐
  //     │  └─────────┘  │
  //     │               │
  //  ┌──▼───────┐  ┌────▼─────┐
  //  │y = copy x│  │use(x)    │
  //  │consume(y)│  └─┬────────┘
  //  └──────┬───┘    │
  //         │        │
  //        ┌▼────────▼┐
  //        │barrier() │ ◄─ boundary
  //        │destroy(x)│
  //        └──────────┘
  mutating func insertCopies() -> InstructionRange {
    // TODO: reuse the InstructionRange from `addDeinitBarriers` if it was computed?
    var liverange = InstructionRange(for: root, context)

    // Omit the destroys from the liverange during copy resolution. They're hoisted afterwards.
    liverange.insert(contentsOf: consumes.users)
    liverange.insert(contentsOf: uses.users)
    liverange.insert(contentsOf: indirectUses)  // Deinit barriers must be treated as uses.
    if hasUnboundedUse {
      // Liveness past an unbounded use is unknown, so the extend liveness to scope-ends.
      liverange.insert(contentsOf: lifetimeLimits)
    }
    log("liverange during insertCopies:\n\(liverange)")

    // If there's no consumes, there's no copies to insert.
    if consumes.isEmpty {
      return liverange
    }

    // Convert the set of *all* consumes into a set that only contains *boundary* consumes.
    var boundaryConsumes: Stack<Operand> = Stack(context)
    while let cons = consumes.pop() {
      // TODO: how to handle consume and use within the same instruction efficiently?
      // It's effectively when the instructions among the 'consumes' overlap with each other, or with any overlap with
      // the instructions in the 'consumes' set.
      if liverange.contains(cons.instruction) {
        log("will insert a copy for operand: \(cons) to convert this into a non-consuming use: \(cons.instruction)")
        replaceWithCopy(cons, context)
        uses.append(cons)
        continue
      }

      boundaryConsumes.push(cons)
    }
    consumes.deinitialize()
    consumes = boundaryConsumes

    // NOTE: this returned liverange doesn't accurately reflect the Operand.user instructions whose consuming use was
    // replaced with a copy!
    return liverange
  }
}

// Collects all deinit barriers that exist on any path from the given lifetimeLimits towards the
// definition, stopping at the boundary of `liverange`.
private func collectDeinitBarriers(
  into barriers: inout Stack<Instruction>,
  liverange: InstructionRange,
  lifetimeLimits: Stack<DestroyValueInst>,
  def: Value,
  _ context: FunctionPassContext
) {
  log("liverange during collectDeinitBarriers:\n\(liverange)")

  let calleeAnalysis = context.calleeAnalysis
  let defInst = def.definingInstruction
  let defBlock = def.parentBlock

  enum ScanResult {
    case foundBarrier(Instruction)
    case hitBoundary
    case exhausted
  }

  // Scans backward within a single block, starting at (and including) `first`.
  // A block argument has no defining instruction, so running out of instructions in its
  // block is itself the boundary.
  func scan(from first: Instruction?, in block: BasicBlock) -> ScanResult {
    for inst in ReverseInstructionList(first: first) {
      if inst == defInst || liverange.inclusiveRangeContains(inst) {
        return .hitBoundary
      }
      if inst.isDeinitBarrier(calleeAnalysis) {
        return .foundBarrier(inst)
      }
    }
    return block == defBlock ? .hitBoundary : .exhausted
  }

  var worklist = BasicBlockWorklist(context)
  defer { worklist.deinitialize() }

  for destroy in lifetimeLimits {
    // Scan the block containing this lifetime limit backwards, stopping the first time we find either
    //  - a deinit barrier
    //  - a current boundary of liveness
    switch scan(from: destroy.previous, in: destroy.parentBlock) {
    case .foundBarrier(let barrier):
      barriers.append(barrier)
      continue
    case .hitBoundary:
      continue
    case .exhausted:
      break
    }

    // If the same block had no barrier or liveness boundary; keep walking
    // backward into unvisited predecessor blocks.
    worklist.pushIfNotVisited(contentsOf: destroy.parentBlock.predecessors)

    while let block = worklist.pop() {
      switch scan(from: block.terminator, in: block) {
      case .foundBarrier(let barrier):
        barriers.append(barrier)
      case .hitBoundary:
        break
      case .exhausted:
        worklist.pushIfNotVisited(contentsOf: block.predecessors)
      }
    }
  }
}

// Given an operand in the consume set, convert its use into a copy.
private func replaceWithCopy(_ op: Operand, _ context: FunctionPassContext) {
  let builder = Builder(before: op.instruction, context)
  let copyValue = builder.createCopyValue(operand: op.value)
  op.set(to: copyValue, context)
}

let lifetimeResolutionResolveTest = FunctionTest("lifetime_resolution_resolve") {
  function, arguments, context in
  let root = arguments.takeValue()
  resolve(root, context)
}
