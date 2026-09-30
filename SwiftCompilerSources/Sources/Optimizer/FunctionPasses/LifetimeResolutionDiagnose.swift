//===--- LifetimeResolutionDiagnose.swift ---------------------------------==//
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

// Recognizes the markers left behind by LifetimeResolution's `legalize` and emits the user-facing diagnostics.
let lifetimeResolutionDiagnosePass = FunctionPass(name: "lifetime-resolution-diagnose") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else { return }

  diagnoseLifetimeViolations(function, context)
}

private func diagnoseLifetimeViolations(_ function: Function, _ context: FunctionPassContext) {
  // Collect the markers up front; `diagnose` markers are erased while emitting.
  var unpermittedCopies: [DiagnoseInst] = []
  var undefInits: [AssignInst] = []
  for inst in function.instructions {
    if let diagnose = inst as? DiagnoseInst, diagnose.kind == .unpermittedCopy {
      unpermittedCopies.append(diagnose)
    } else if let assign = inst as? AssignInst,
              assign.assignOwnership == .initialize, assign.source is Undef {
      undefInits.append(assign)
    }
  }

  for assign in undefInits {
    diagnoseUseBeforeInit(assign, context)
  }
  for diagnose in unpermittedCopies {
    diagnoseUnpermittedCopy(diagnose, context)
  }
}

// `diagnose [unpermitted_copy] %x`: report the demoted copy as though it were an illegal consuming use.
private func diagnoseUnpermittedCopy(_ marker: DiagnoseInst, _ context: FunctionPassContext) {
  defer { context.erase(instruction: marker) }

  switch marker.operand.value.definingInstruction {
  case let load as LoadInst:
    diagnoseUnpermittedCopy(ofAddress: load, context)
  case let copy as CopyValueInst:
    diagnoseUnpermittedCopy(ofValue: copy, marker, context)
  default:
    // An error message backstop for unrecognized diagnose scenarios to ensure we still emit an error.
    context.diagnosticEngine.diagnose(.sil_movechecking_bug_missed_copy, at: marker.location)
  }
}

private func diagnoseUnpermittedCopy(ofAddress load: LoadInst, _ context: FunctionPassContext) {
  let root = allocation(backing: load.address).flatMap { ResolvableRoot($0, context) }
  let name: StringRef = root?.varDecl?.userFacingName ?? ""

  // Error at the offending downstream use; note at the consume that was demoted.
  let offending = findOffendingUse(ofAddress: load.address, after: load, context) ?? load
  context.diagnosticEngine.diagnose(.sil_movechecking_value_used_after_consume,
    name, at: offending.location)
  context.diagnosticEngine.diagnose(.sil_movechecking_consuming_use_here,
    at: load.location)
}

// `%copy`'s only other use (besides `marker`) is what now consumes the copy instead of `root`.
private func diagnoseUnpermittedCopy(ofValue copy: CopyValueInst, _ marker: DiagnoseInst,
                                     _ context: FunctionPassContext) {
  guard let consuming = copy.uses.ignore(user: marker).singleUse else {
    context.diagnosticEngine.diagnose(.sil_movechecking_bug_missed_copy, at: marker.location)
    return
  }
  let root = copy.fromValue
  let consumingInst = consuming.instruction
  let name: StringRef = root.findVarDecl()?.userFacingName ?? ""

  // Error at the offending downstream use; note at the consume that was demoted.
  let offending = findOffendingUse(ofValue: root, ignoring: copy, after: consumingInst, context) ?? consumingInst
  context.diagnosticEngine.diagnose(.sil_movechecking_value_used_after_consume,
    name, at: offending.location)
  context.diagnosticEngine.diagnose(.sil_movechecking_consuming_use_here,
    at: consumingInst.location)
}

// `assign undef to [init] %addr`: report use-before-init at the first read that observes the undef store.
private func diagnoseUseBeforeInit(_ marker: AssignInst, _ context: FunctionPassContext) {
  let rootAddress = marker.destination
  guard let allocation = allocation(backing: rootAddress),
        let root = ResolvableRoot(allocation, context) else { return }
  // `%select{variable|constant}`: 1 == constant (`let`), 0 == variable (`var`).
  let name: StringRef = root.varDecl?.userFacingName ?? ""
  let isLet = root.isLet ? 1 : 0

  // "defined here" points at the `mark_uninitialized`, falling back to the allocation's address.
  let markUninit = allocation.uses.singleUser(ofType: MarkUninitializedInst.self)
  let definedHere = markUninit?.location ?? root.address.definingInstruction!.location

  let offending = findOffendingUse(ofAddress: rootAddress, after: marker, context)
  let errorLoc = offending?.location ?? definedHere
  context.diagnosticEngine.diagnose(.variable_used_before_initialized,
    name, isLet, at: errorLoc)
  context.diagnosticEngine.diagnose(.variable_defined_here,
    isLet, at: definedHere)
}

// Recover the storage allocation (`alloc_box` / `alloc_stack`) backing `address` via its access path.
private func allocation(backing address: Value) -> Value? {
  switch address.accessPath.base {
  case .box(let projectBox):
    return projectBox.box.referenceRoot
  case .stack(let allocStack):
    return allocStack
  default:
    return nil
  }
}

// Find the nearest downstream read of the same storage subelement as `address` after `start`.
//
// TODO: adopt `LocalVariableReachableAccess` (AddressUtils.swift) instead of classifying uses here directly.
private func findOffendingUse(ofAddress address: Value, after start: Instruction,
                              _ context: FunctionPassContext) -> Instruction? {
  let consumed = address.accessPath
  guard let base = consumed.base.address else { return nil }

  var reads = InstructionSet(context)
  defer { reads.deinitialize() }
  var defs = InstructionSet(context)
  defer { defs.deinitialize() }
  // Walk down from the storage base, classifying uses of the consumed subelement.
  var classifier = FieldUseClassifier(consumed: consumed, reads: reads, defs: defs)
  _ = classifier.walkDownUses(ofAddress: base, path: UnusedWalkingPath())

  return findOffendingUse(reads: reads, defs: defs, after: start, context)
}

// Find the nearest downstream use of owned SSA/object `root` after `start`, ignoring `copy`'s own uses.
private func findOffendingUse(ofValue root: Value, ignoring copy: CopyValueInst, after start: Instruction,
                              _ context: FunctionPassContext) -> Instruction? {
  var reads = InstructionSet(context)
  defer { reads.deinitialize() }
  var defs = InstructionSet(context)
  defer { defs.deinitialize() }
  var classifier = ValueUseClassifier(ignoring: copy, reads: reads)
  _ = classifier.walkDownUses(ofValue: root, path: SmallProjectionPath())

  return findOffendingUse(reads: reads, defs: defs, after: start, context)
}

// Shared scan: find the first instruction in `reads` reachable from `start` without passing through `defs`.
private func findOffendingUse(reads: InstructionSet, defs: InstructionSet, after start: Instruction,
                              _ context: FunctionPassContext) -> Instruction? {
  enum ScanResult { case found(Instruction), satisfied, passThrough }

  // Scan a block's instructions, optionally only those after `afterInst`, for a read or reassignment.
  func scan(_ block: BasicBlock, after afterInst: Instruction?) -> ScanResult {
    var reached = afterInst == nil
    for inst in block.instructions {
      if !reached {
        if inst == afterInst! { reached = true }
        continue
      }
      if reads.contains(inst) { return .found(inst) }
      if defs.contains(inst) { return .satisfied }
    }
    return .passThrough
  }

  // The offending use may be later in `start`'s own block.
  switch scan(start.parentBlock, after: start) {
  case .found(let use): return use
  case .satisfied: return nil
  case .passThrough: break
  }

  // Otherwise fan out to successors, breadth-first.
  var visited = BasicBlockSet(context)
  defer { visited.deinitialize() }
  var queue = Array(start.parentBlock.successors)
  var head = 0
  while head < queue.count {
    let block = queue[head]
    head += 1
    guard visited.insert(block) else { continue }
    switch scan(block, after: nil) {
    case .found(let use): return use
    case .satisfied: continue
    case .passThrough: queue.append(contentsOf: block.successors)
    }
  }
  return nil
}

// Classifies uses of a storage subelement into reads (loads) and reassignments (stores/assigns/destroys).
private struct FieldUseClassifier: AddressDefUseWalker {
  typealias Path = UnusedWalkingPath
  let consumed: AccessPath
  var reads: InstructionSet
  var defs: InstructionSet

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    let inst = address.instruction
    let usePath = address.value.accessPath
    switch inst {
    case is LoadInst, is LoadBorrowInst:
      // A read observes the consumed subelement if the two accesses may overlap.
      if !usePath.isDistinct(from: consumed) {
        reads.insert(inst)
      }
    case let store as StoreInst where store.destination == address.value:
      // A store reassigns (kills) demand only if it fully overwrites the subelement.
      if usePath.isEqualOrContains(consumed) { defs.insert(inst) }
    case let assign as AssignInst where assign.destination == address.value:
      if usePath.isEqualOrContains(consumed) { defs.insert(inst) }
    case is DestroyAddrInst:
      if usePath.isEqualOrContains(consumed) { defs.insert(inst) }
    default:
      break
    }
    return .continueWalk
  }
}

// Classifies uses of an owned SSA/object value into reads, walking transparently through forwarding instructions.
private struct ValueUseClassifier: ValueDefUseWalker {
  typealias Path = SmallProjectionPath
  let ignoring: CopyValueInst
  var reads: InstructionSet
  var walkDownCache = WalkerCache<SmallProjectionPath>()

  mutating func walkDown(value operand: Operand, path: SmallProjectionPath) -> WalkResult {
    if operand.instruction == ignoring {
      return .continueWalk
    }
    return walkDownDefault(value: operand, path: path)
  }

  mutating func leafUse(value: Operand, path: SmallProjectionPath) -> WalkResult {
    switch value.instruction {
    case is DestroyValueInst, is EndBorrowInst, is DebugValueInst:
      break
    default:
      reads.insert(value.instruction)
    }
    return .continueWalk
  }
}

let lifetimeResolutionDiagnoseTest = FunctionTest("lifetime_resolution_diagnose") {
  function, arguments, context in
  diagnoseLifetimeViolations(function, context)
}
