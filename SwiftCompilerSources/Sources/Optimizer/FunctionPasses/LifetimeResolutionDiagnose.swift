//===--- LifetimeResolutionDiagnose.swift ---------------------------------===//
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

// Turns each LifetimeResolutionMarker left behind by LifetimeResolution into a user-facing diagnostic.
let lifetimeResolutionDiagnosePass = FunctionPass(name: "lifetime-resolution-diagnose") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else { return }

  diagnoseLifetimeViolations(function, context)
}

private func diagnoseLifetimeViolations(_ function: Function, _ context: FunctionPassContext) {
  var markers: [LifetimeResolutionMarker] = []
  for inst in function.instructions {
    if let marker = LifetimeResolutionMarker(inst) {
      markers.append(marker)
      continue
    }

    switch inst {
    // Borrows of memory have now been checked by LifetimeResolution, so the `load_borrow` invariants can be enforced.
    case let loadBorrow as LoadBorrowInst where loadBorrow.isUnchecked:
      loadBorrow.set(isUnchecked: false, context)

    // TODO: add missed copy of noncopyable backstop

    default:
      break
    }
  }

  var diagnoser = Diagnoser(function, markers, context)
  diagnoser.diagnoseUsesBeforeInit(markers)
  diagnoser.diagnoseLetReinits(markers)
  diagnoser.diagnoseUsesAfterConsume()

  // The `diagnose` markers are erased last, as they identify which copies were demoted consumes.
  for case let diagnose? in markers.map({ $0.diagnose }) {
    context.erase(instruction: diagnose)
  }

  if !markers.isEmpty {
    // Ensure we actually did emit an error.
    guard context.hadError else {
      fatalError("LifetimeResolutionDiagnose didn't emit an error when it should have! \n\(function)")
    }

    // Like DefiniteInitialization, silence later diagnostics like ImplicitDestroyChecker's after reporting an error.
    function.addSemanticsAttribute("sil.optimizer.moveonly.diagnostic.ignore", context)
  }
}

//===----------------------------------------------------------------------===//
//                                  Storage
//===----------------------------------------------------------------------===//

/// A storage location that LifetimeResolution resolved, with helpers to describe it to the user.
private struct Storage {
  let root: ResolvableRoot

  var space: StateIndexSpace { root.space }
  var allocation: Value { root.storage }
  var function: Function { root.address.parentFunction }
  var type: Type { root.address.type.objectType }

  var rootName: String {
    if root.isSelf {
      return "self"
    }
    return root.varDecl?.userFacingName.string ?? ""
  }

  /// Where the storage was declared, for "defined here" notes.
  var definitionLocation: Location {
    root.markUninitialized?.location ?? allocation.definingInstruction?.location ?? function.location
  }

  /// The user-facing name of the subelement at `path`, e.g. `p.a`, `t.1`, or `self.b`.
  func name(of path: SmallProjectionPath) -> String {
    var name = rootName
    var type = self.type
    var remaining = path
    while true {
      let (kind, index, rest) = remaining.pop()
      switch kind {
      case .structField, .classField:
        guard let fields = type.getNominalFields(in: function), index < fields.count else { return name }
        let fieldName = fields.getUserFacingFieldDecl(withIndex: index)?.userFacingName
                          ?? fields.getNameOfField(withIndex: index)
        name += "." + fieldName.string
        type = fields[index]
      case .tupleField:
        guard type.isTuple, index < type.tupleElements.count else { return name }
        let label = type.tupleElements.labelName(at: index)
        name += "." + (label.count == 0 ? String(index) : label.string)
        type = type.tupleElements[index]
      default:
        return name
      }
      remaining = rest
    }
  }

  /// The stored property of `self` that `path` projects into, if any.
  func storedProperty(at path: SmallProjectionPath) -> VarDecl? {
    let (kind, index, _) = path.pop()
    guard kind == .structField || kind == .classField,
          let fields = type.getNominalFields(in: function), index < fields.count else {
      return nil
    }
    return fields.getUserFacingFieldDecl(withIndex: index)
  }

  /// The path of the first part of the storage among `leaves`, preferring a stored property of `self`
  /// over all of `self`.
  func firstPath(of leaves: Set<Int>) -> SmallProjectionPath {
    var firstPath: SmallProjectionPath? = nil
    space.fields.forEachCoveringSubtree(of: fieldLeaves(leaves), excludingRoot: root.isSelf) { path in
      if firstPath == nil { firstPath = path }
    }
    return firstPath ?? SmallProjectionPath()
  }

  /// The elements among `leaves` that are fields, which have a path.
  func fieldLeaves(_ leaves: Set<Int>) -> Set<Int> {
    leaves.filter { space.fieldRange.contains($0) }
  }
}

//===----------------------------------------------------------------------===//
//                               Use classification
//===----------------------------------------------------------------------===//

private enum UseKind {
  case read             // observes the value without taking ownership
  case consume          // takes ownership of the value
  case inoutArgument    // passed by reference, e.g. `&x`
  case closureCapture   // captured by a closure
  case functionCapture  // captured by a nested function
  case addressTaken     // converted to a pointer
  case returnFromInit   // `self` returned from an initializer
  case functionExit     // the end of an `inout` parameter's scope, where it must be initialized
  case superInit        // `self` passed to `super.init`, which needs its own stored properties
}

/// A part of some storage needed by a use.
private enum Subelement {
  /// The part at an access path, or all of the storage if nil.
  case path(AccessPath?)

  /// The superclass part of `self` in a derived class initializer, which `super.init` initializes.
  case superclass
}

/// The classified uses of some storage or value, each with the access path it uses within storage.
private struct UseMap {
  typealias Use = (kind: UseKind, accessPath: AccessPath?)
  private var uses: [Instruction: [Use]] = [:]
  private var kills: [Instruction: [AccessPath]] = [:]

  mutating func record(_ inst: Instruction, _ kind: UseKind, _ accessPath: AccessPath? = nil) {
    uses[inst, default: []].append((kind, accessPath))
  }

  mutating func kill(_ inst: Instruction, _ accessPath: AccessPath) {
    kills[inst, default: []].append(accessPath)
  }

  /// The strongest use of `subelement` by `inst`, where a nil access path is the whole storage.
  func use(_ inst: Instruction, of subelement: Subelement) -> Use? {
    let overlapping = uses[inst, default: []].filter { use in
      switch subelement {
      case .superclass:
        return use.accessPath == nil && use.kind != .superInit
      case .path(let path):
        guard let path, let accessPath = use.accessPath else { return true }
        return !accessPath.isDistinct(from: path)
      }
    }
    return overlapping.first(where: { $0.kind == .consume }) ?? overlapping.first
  }

  /// Whether `inst` overwrites all of `subelement`.
  func isKill(_ inst: Instruction, of subelement: Subelement) -> Bool {
    switch subelement {
    case .superclass:
      return uses[inst, default: []].contains { $0.kind == .superInit }
    case .path(let path):
      return kills[inst, default: []].contains { accessPath in
        path.map { accessPath.isEqualOrContains($0) } ?? accessPath.projectionPath.isEmpty
      }
    }
  }
}

/// Classifies the uses of some storage.
private struct AddressUseClassifier: ResolvableRootUseWalker {
  let demotedConsumes: Set<Instruction>
  let isInitSelf: Bool
  var uses = UseMap()

  /// Classifies a use of the memory holding `self` in a derived class initializer.
  mutating func derivedSelfUse(_ use: DerivedSelfUse, of operand: Operand) -> WalkResult {
    switch use {
    case .borrow(let loadBorrow):
      _ = walkDownUses(ofClassInstance: loadBorrow)
    case .copy(let load):
      _ = walkDownUses(ofClassInstance: load)
    case .superInit(_, let apply):
      uses.record(apply, .superInit)
    case .storeBack(let store):
      uses.kill(store, operand.value.accessPath)
    case .destroy(let destroy):
      uses.kill(destroy, operand.value.accessPath)
    case .deallocate(let load):
      uses.kill(load, operand.value.accessPath)
    case .initialStore, .ignored, .unknown:
      break
    }
    return .continueWalk
  }

  mutating func instanceUse(_ use: Operand) -> WalkResult {
    let inst = use.instruction
    switch inst {
    case is DestroyValueInst, is DiagnoseInst:
      break
    case is PartialApplyInst:
      uses.record(inst, .closureCapture)
    default:
      uses.record(inst, isInitSelf && flowsIntoReturn(use) ? .returnFromInit : .read)
    }
    return .continueWalk
  }

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    let inst = address.instruction
    let accessPath = address.value.accessPath
    func record(_ kind: UseKind) { uses.record(inst, kind, accessPath) }
    func kill() { uses.kill(inst, accessPath) }

    // Only a take of a noncopyable value consumes, unless it is a demoted consume.
    let isDemotedConsume = demotedConsumes.contains(inst)
    let isNoncopyable = address.value.type.isMoveOnly

    switch inst {
    case let load as LoadInst:
      if isInitSelf && flowsIntoReturn(load) {
        record(.returnFromInit)
      } else if isDemotedConsume || (isNoncopyable && load.uses.contains(where: isConsumingUse)) {
        record(.consume)
      } else {
        record(.read)
      }
    case is LoadBorrowInst:
      record(.read)
    case let copyAddr as CopyAddrInst:
      if copyAddr.destinationOperand == address {
        kill()
      } else if isInitSelf, let result = copyAddr.destination as? FunctionArgument, result.convention == .indirectOut {
        record(.returnFromInit)
      } else if isDemotedConsume || isNoncopyable {
        record(.consume)
      } else {
        record(.read)
      }
    case let store as StoringInstruction where store.destination == address.value:
      kill()
    case let tupleAddrConstructor as TupleAddrConstructorInst where tupleAddrConstructor.destinationOperand == address:
      kill()
    case is DestroyAddrInst:
      kill()
    case is PartialApplyInst:
      record(.closureCapture)
    case let apply as FullApplySite:
      switch apply.convention(of: address) {
      case .indirectOut:
        kill()
      case .indirectIn, .indirectInCXX:
        record(isNoncopyable ? .consume : .read)
      case .indirectInout, .indirectInoutAliasable:
        record(.inoutArgument)
      default:
        record(.read)
      }
    case let escape as MarkFunctionEscapeInst:
      record(escape.location.isClosureExpr ? .closureCapture : .functionCapture)
    case is AddressToPointerInst:
      record(.addressTaken)
    case is DeallocStackInst, is DiagnoseInst, is DebugValueInst, is EndAccessInst:
      break
    default:
      record(.read)
    }
    return .continueWalk
  }
}

/// Classifies the uses of an owned value, walking through forwarding instructions and borrows.
private struct ValueUseClassifier: ValueDefUseWalker {
  typealias Path = SmallProjectionPath
  let demotedConsumes: Set<Instruction>
  var uses = UseMap()
  var walkDownCache = WalkerCache<SmallProjectionPath>()

  mutating func walkDown(value operand: Operand, path: SmallProjectionPath) -> WalkResult {
    switch operand.instruction {
    case is CopyValueInst, is MoveValueInst:
      // The copied or moved value is a different binding.
      return leafUse(value: operand, path: path)
    default:
      return walkDownDefault(value: operand, path: path)
    }
  }

  mutating func leafUse(value: Operand, path: SmallProjectionPath) -> WalkResult {
    let inst = value.instruction
    switch inst {
    case is DestroyValueInst, is EndBorrowInst, is DebugValueInst, is DiagnoseInst:
      break
    default:
      if demotedConsumes.contains(inst) || (value.value.type.isMoveOnly && isConsumingUse(value)) {
        uses.record(inst, .consume)
      } else {
        uses.record(inst, .read)
      }
    }
    return .continueWalk
  }
}

/// Whether `use` takes ownership of its value, other than to destroy it.
private func isConsumingUse(_ use: Operand) -> Bool {
  use.endsLifetime && !(use.instruction is DestroyValueInst)
}

/// Whether `value` is what the function returns, possibly wrapped in an Optional by a failable initializer.
private func flowsIntoReturn(_ value: Value) -> Bool {
  guard let use = value.uses.ignoreDebugUses.singleUse else {
    return false
  }
  return flowsIntoReturn(use)
}

/// Whether the value used by `use` is what the function returns.
private func flowsIntoReturn(_ use: Operand) -> Bool {
  switch use.instruction {
  case is ReturnInst:
    return true
  case let enumInst as EnumInst:
    return flowsIntoReturn(enumInst)
  case let branch as BranchInst:
    return flowsIntoReturn(branch.getArgument(for: use))
  default:
    return false
  }
}

/// How `method` is named in a diagnostic: an accessor by its storage.
private func diagnosticName(of method: FuncDecl) -> (name: Identifier, isAccessor: Bool) {
  let accessor = method as? AccessorDecl
  return ((accessor?.storage ?? method).baseIdentifier, accessor != nil)
}

/// The method or accessor called by `inst`, if it is a call.
private func calledMethod(by inst: Instruction) -> FuncDecl? {
  guard let apply = inst as? FullApplySite else {
    return nil
  }
  if let classMethod = apply.callee as? ClassMethodInst {
    return classMethod.member.decl as? FuncDecl
  }
  return apply.referencedFunction?.location.decl as? FuncDecl
}

/// The first use reachable from some starting instruction.
private struct OffendingUse {
  let instruction: Instruction
  let kind: UseKind
  let accessPath: AccessPath?

  /// The blocks the search passed through, from the starting block to the use's block.
  let path: [BasicBlock]

  /// Whether this is the starting instruction itself, reached again around a loop.
  let isLoop: Bool
}

/// Finds the nearest use of `subelement` reachable from `start` without passing through a kill, breadth-first.
private func findOffendingUse(of subelement: Subelement, in uses: UseMap, after start: Instruction,
                              _ context: FunctionPassContext) -> OffendingUse? {
  enum ScanResult { case found(OffendingUse), satisfied, passThrough }

  func scan(_ block: BasicBlock, after afterInst: Instruction?, path: @autoclosure () -> [BasicBlock]) -> ScanResult {
    var reached = afterInst == nil
    for inst in block.instructions {
      if !reached {
        if inst == afterInst! { reached = true }
        continue
      }
      if let (kind, accessPath) = uses.use(inst, of: subelement) {
        return .found(OffendingUse(instruction: inst, kind: kind, accessPath: accessPath,
                                   path: path(), isLoop: inst == start))
      }
      if uses.isKill(inst, of: subelement) { return .satisfied }
    }
    return .passThrough
  }

  // The offending use may be later in `start`'s own block.
  switch scan(start.parentBlock, after: start, path: [start.parentBlock]) {
  case .found(let use): return use
  case .satisfied: return nil
  case .passThrough: break
  }

  // Otherwise fan out to successors, breadth-first. Each entry refers to the entry it was reached from,
  // or -1 for a successor of the starting block.
  var visited = BasicBlockSet(context)
  defer { visited.deinitialize() }
  var queue = start.parentBlock.successors.map { (block: $0, from: -1) }
  func path(to index: Int) -> [BasicBlock] {
    var path: [BasicBlock] = []
    var i = index
    while i >= 0 {
      path.append(queue[i].block)
      i = queue[i].from
    }
    path.append(start.parentBlock)
    return path.reversed()
  }
  var head = 0
  while head < queue.count {
    let (block, _) = queue[head]
    let index = head
    head += 1
    guard visited.insert(block) else { continue }
    switch scan(block, after: nil, path: path(to: index)) {
    case .found(let use): return use
    case .satisfied: continue
    case .passThrough: queue.append(contentsOf: block.successors.map { ($0, index) })
    }
  }
  return nil
}

/// The single instruction that uses `value`, ignoring markers and lifetime ends.
private func singleUse(of value: Value) -> Operand? {
  let uses = value.uses.lazy.filter {
    switch $0.instruction {
    case is DiagnoseInst, is DestroyValueInst, is EndBorrowInst, is DebugValueInst: false
    default: true
    }
  }
  return uses.singleElement
}

/// The call that a temporary copy at `address` is passed to, e.g. `useGeneric(t)`.
private func callReceivingTemporary(_ address: Value) -> Operand? {
  guard let allocStack = address as? AllocStackInst, !allocStack.isFromVarDecl else {
    return nil
  }
  return allocStack.uses.lazy.filter { $0.instruction is FullApplySite }.singleElement
}

//===----------------------------------------------------------------------===//
//                                 Consumes
//===----------------------------------------------------------------------===//

/// A consume that LifetimeResolution demoted to a copy because the value is still used afterwards.
private struct ConsumeSite {
  let marker: DiagnoseInst

  /// The demoted `load`, `copy_addr`, or `copy_value`.
  let instruction: Instruction

  /// The operand that receives ownership of the copy, e.g. a call argument, if known.
  let consumer: Operand?

  init?(_ marker: DiagnoseInst) {
    self.marker = marker
    switch LifetimeResolutionMarker.unpermittedCopy(marker).copy {
    case let load as LoadInst:
      instruction = load
      consumer = singleUse(of: load)
    case let copy as CopyValueInst:
      instruction = copy
      consumer = singleUse(of: copy)
    case let copyAddr as CopyAddrInst:
      instruction = copyAddr
      consumer = callReceivingTemporary(copyAddr.destination)
    default:
      return nil
    }
  }

  /// The consumed storage, unless a value was consumed.
  var address: Value? {
    switch instruction {
    case let load as LoadInst: load.address
    case let copyAddr as CopyAddrInst: copyAddr.source
    default: nil
    }
  }

  var isCopyable: Bool { return !marker.operand.value.type.isMoveOnly }

  /// The instruction that takes ownership of the copy, or the copy itself if unknown.
  var consumingInstruction: Instruction { consumer?.instruction ?? instruction }

  /// Where the consume appears in source. A loaded address is located at the consumed expression, whereas
  /// a copied value is located at its consumer.
  var location: Location { instruction is CopyValueInst ? consumingInstruction.location : instruction.location }
}

/// The instruction a use of storage ultimately serves, e.g. the call that a loaded argument is passed to.
private func userInstruction(of use: Instruction) -> Instruction {
  switch use {
  case let load as LoadInst:
    return singleUse(of: load)?.instruction ?? load
  case let loadBorrow as LoadBorrowInst:
    return singleUse(of: loadBorrow)?.instruction ?? loadBorrow
  case let copy as CopyValueInst:
    return singleUse(of: copy)?.instruction ?? copy
  case let copyAddr as CopyAddrInst:
    return callReceivingTemporary(copyAddr.destination)?.instruction ?? copyAddr
  default:
    return use
  }
}

/// How a use-after-consume is reported.
private enum ConsumeViolation: Equatable {
  case usedAfterConsume
  case consumedTwice
  case usedAfterPartialConsume
  case consumedInLoop
  case consumedAndUsedAtOnce
  case notReinitialized
  case usedAfterFailedSuperInit
  case unknownUse

  /// Whether the error is located at the consume, rather than at a use shared by several consumes.
  var isReportedAtConsume: Bool {
    switch self {
    case .consumedInLoop, .notReinitialized, .unknownUse: true
    default: false
    }
  }
}

//===----------------------------------------------------------------------===//
//                                 Diagnoser
//===----------------------------------------------------------------------===//

private struct Diagnoser {
  let function: Function
  let context: FunctionPassContext
  var indexCache: FieldIndexTrieCache

  /// The consumes that LifetimeResolution demoted, and the markers it left for unrecognized ones.
  let consumes: [ConsumeSite]
  let unrecognizedConsumes: [DiagnoseInst]
  let demotedConsumes: Set<Instruction>

  /// The uses of each storage allocation or owned value, computed on demand.
  private var storages: [HashableValue: Storage] = [:]
  private var useMaps: [HashableValue: UseMap] = [:]

  /// Like DefiniteInitialization, report at most one error per storage and source location.
  private var emittedErrors: [(storage: Value, location: Location)] = []

  /// Explanatory notes that are only worth seeing once: per variable, or per callee parameter.
  private var explainedStorage: [Value] = []
  private var suggestedParameters: Set<ParamDecl> = []

  init(_ function: Function, _ markers: [LifetimeResolutionMarker], _ context: FunctionPassContext) {
    self.function = function
    self.context = context
    self.indexCache = FieldIndexTrieCache(for: function)

    var consumes: [ConsumeSite] = []
    var unrecognizedConsumes: [DiagnoseInst] = []
    for case .unpermittedCopy(let marker) in markers {
      if let consume = ConsumeSite(marker) {
        consumes.append(consume)
      } else {
        unrecognizedConsumes.append(marker)
      }
    }
    self.consumes = consumes
    self.unrecognizedConsumes = unrecognizedConsumes
    self.demotedConsumes = Set(consumes.map { $0.instruction })
  }

  var diagnostics: DiagnosticEngine { context.diagnosticEngine }

  func sourceLoc(_ location: Location) -> SourceLoc? {
    location.getSourceLocation(diagnosticEngine: diagnostics)
  }

  mutating func shouldEmitError(for storage: Value, at location: Location) -> Bool {
    if emittedErrors.contains(where: { $0.storage == storage && $0.location.hasSameSourceLocation(as: location) }) {
      return false
    }
    emittedErrors.append((storage, location))
    return true
  }

  mutating func storage(backing address: Value) -> Storage? {
    guard let allocation = ResolvableRoot.allocation(backing: address) else {
      return nil
    }
    if let storage = storages[allocation.hashable] {
      return storage
    }
    guard let root = ResolvableRoot(allocation, &indexCache) else {
      return nil
    }
    let storage = Storage(root: root)
    storages[allocation.hashable] = storage
    return storage
  }

  /// Classifies every use of `storage`.
  mutating func uses(of storage: Storage) -> UseMap {
    if let uses = useMaps[storage.allocation.hashable] {
      return uses
    }
    var classifier = AddressUseClassifier(demotedConsumes: demotedConsumes,
                                          isInitSelf: storage.root.initSelfKind != nil)
    _ = classifier.walkDownUses(of: storage.root)
    var uses = classifier.uses

    // A box captured by a closure, rather than its projected address.
    if storage.allocation is AllocBoxInst {
      var worklist = Array(storage.allocation.uses)
      while let use = worklist.popLast() {
        switch use.instruction {
        case is PartialApplyInst:
          uses.record(use.instruction, .closureCapture)
        case let forwarding as SingleValueInstruction
               where forwarding is MarkUninitializedInst || forwarding is BeginBorrowInst ||
                     forwarding is CopyValueInst || forwarding is MoveValueInst:
          worklist.append(contentsOf: forwarding.uses)
        default:
          break
        }
      }
    }

    // The uses LifetimeResolution considered beyond those of the address.
    for extraUse in storage.root.extraLivenessUses {
      switch extraUse {
      case .functionExit(let exit):
        uses.record(exit, .functionExit)
      case .boxLifetimeEnd:
        break  // TODO: not yet diagnosed
      }
    }
    useMaps[storage.allocation.hashable] = uses
    return uses
  }

  /// Classifies every use of the owned `value`.
  mutating func uses(of value: Value) -> UseMap {
    if let uses = useMaps[value.hashable] {
      return uses
    }
    var classifier = ValueUseClassifier(demotedConsumes: demotedConsumes)
    _ = classifier.walkDownUses(ofValue: value, path: SmallProjectionPath(.anyValueFields))
    useMaps[value.hashable] = classifier.uses
    return classifier.uses
  }

  //===--------------------------------------------------------------------===//
  //                             Use before init
  //===--------------------------------------------------------------------===//

  /// Diagnoses the `useBeforeInit` and `missingSuperInit` markers.
  mutating func diagnoseUsesBeforeInit(_ markers: [LifetimeResolutionMarker]) {
    // Group the uninitialized elements of each storage by the use that first observes them.
    var reports: [(storage: Storage, use: OffendingUse?, leaves: Set<Int>)] = []
    func report(_ storage: Storage, _ use: OffendingUse?, _ leaves: StateIndexSpace.IndexRange) {
      if let i = reports.firstIndex(where: { $0.storage.allocation == storage.allocation &&
                                             $0.use?.instruction == use?.instruction }) {
        reports[i].leaves.formUnion(leaves)
      } else {
        reports.append((storage, use, Set(leaves)))
      }
    }

    for case .useBeforeInit(let marker) in markers {
      guard let storage = storage(backing: marker.destination) else {
        diagnostics.diagnose(.definite_init_bug_unknown_storage, at: marker.location)
        continue
      }
      let leaves = storage.space.range(ofFieldPath: marker.destination.projectionPathFromRoot)
      report(storage, findOffendingUse(of: .path(marker.destination.accessPath), in: uses(of: storage),
                                       after: marker, context), leaves)
    }
    for case .missingSuperInit(let marker) in markers {
      guard let storage = storage(backing: marker.operand.value),
            let superclass = storage.space.range(of: .superclass) else {
        diagnostics.diagnose(.definite_init_bug_unknown_storage, at: marker.location)
        continue
      }
      report(storage, findOffendingUse(of: .superclass, in: uses(of: storage), after: marker, context), superclass)
    }

    for (storage, use, leaves) in reports {
      emitUseBeforeInit(of: storage, at: use, leaves)
    }
  }

  private mutating func emitUseBeforeInit(of storage: Storage, at use: OffendingUse?, _ leaves: Set<Int>) {
    let useLocation = use?.instruction.location ?? storage.definitionLocation
    let isReturn = use?.kind == .returnFromInit

    if storage.root.mode == .derivedClassSelf {
      emitUseBeforeInit(ofDerivedSelf: storage, at: use, leaves)
      return
    }

    if storage.root.isRootSelf {
      if isReturn, let use {
        let returnLocation = explicitReturnLocation(of: use)
        let location = returnLocation ?? useLocation
        guard shouldEmitError(for: storage.allocation, at: location) else { return }
        if let returnLocation, (function.location.decl as? ConstructorDecl)?.isFailable ?? false,
           let returnLoc = sourceLoc(returnLocation) {
          diagnostics.diagnose(.return_from_failable_init_without_nil, [], at: returnLoc,
            fixIts: [DiagnosticFixIt(start: returnLoc, byteLength: "return".utf8.count, replacement: "return nil")])
        } else {
          diagnostics.diagnose(.return_from_init_without_initing_stored_properties, at: location)
        }
      } else if storage.root.isClassInstance {
        emitUseBeforeStoredPropertiesInit(of: storage, at: use, leaves)
        return
      } else {
        guard shouldEmitError(for: storage.allocation, at: useLocation) else { return }
        diagnostics.diagnose(.use_of_self_before_fully_init, at: useLocation)
      }
      noteUninitializedStoredProperties(of: storage, leaves, fallback: useLocation)
      return
    }

    guard shouldEmitError(for: storage.allocation, at: useLocation) else { return }

    if storage.root.initSelfKind != nil {
      // `self` of a delegating initializer, which must be initialized by `self.init` or assignment.
      if isReturn {
        diagnostics.diagnose(.superselfinit_not_called_before_return, 1, at: useLocation)
      } else {
        diagnostics.diagnose(.self_before_selfinit_value_type, at: useLocation)
      }
      return
    }

    // Name the whole variable if it is entirely uninitialized, otherwise its first uninitialized part.
    let name = storage.name(of: storage.firstPath(of: leaves))

    let isLet = storage.root.isLet ? 1 : 0
    let id: DiagID
    switch use?.kind {
    case .inoutArgument: id = .variable_inout_before_initialized
    case .closureCapture: id = .variable_closure_use_uninit
    case .functionCapture: id = .variable_function_use_uninit
    case .addressTaken: id = .variable_addrtaken_before_initialized
    default: id = .variable_used_before_initialized
    }
    diagnostics.diagnose(id, name, isLet, at: useLocation)
    diagnostics.diagnose(.variable_defined_here, isLet, at: storage.definitionLocation)
  }

  /// A use of a class instance before all of its own stored properties are initialized.
  private mutating func emitUseBeforeStoredPropertiesInit(of storage: Storage, at use: OffendingUse?,
                                                          _ leaves: Set<Int>) {
    let useLocation = use?.instruction.location ?? storage.definitionLocation
    guard shouldEmitError(for: storage.allocation, at: useLocation) else { return }
    if use?.kind == .closureCapture {
      diagnostics.diagnose(.self_closure_use_uninit, at: useLocation)
    } else if let method = use.flatMap({ calledMethod(by: $0.instruction) }) {
      let (name, isAccessor) = diagnosticName(of: method)
      diagnostics.diagnose(.self_use_before_fully_init, name, isAccessor ? 1 : 0, 0, at: useLocation)
    } else {
      diagnostics.diagnose(.variable_used_before_initialized, storage.name(of: storage.firstPath(of: leaves)), 0,
                           at: useLocation)
      return
    }
    noteUninitializedStoredProperties(of: storage, leaves, fallback: useLocation)
  }

  /// A use of `self` in a derived class initializer before `super.init`, or before its own stored properties
  /// are initialized.
  private mutating func emitUseBeforeInit(ofDerivedSelf storage: Storage, at use: OffendingUse?, _ leaves: Set<Int>) {
    let superclass = storage.space.index(of: .superclass)!
    guard leaves.contains(superclass) else {
      guard let use, use.kind == .superInit else {
        emitUseBeforeStoredPropertiesInit(of: storage, at: use, leaves)
        return
      }
      // An implicit `super.init` has no location of its own.
      let isImplicit = use.instruction.location.sourceLoc == nil
      let location = isImplicit ? function.location : use.instruction.location
      guard shouldEmitError(for: storage.allocation, at: location) else { return }
      let id: DiagID = isImplicit ? .ivar_not_initialized_at_implicit_superinit : .ivar_not_initialized_at_superinit
      diagnostics.diagnose(id, storage.name(of: storage.firstPath(of: leaves)), 0, at: location)
      return
    }

    let useLocation = use?.instruction.location ?? storage.definitionLocation
    if let use, use.kind == .returnFromInit {
      let location = explicitReturnLocation(of: use) ?? useLocation
      guard shouldEmitError(for: storage.allocation, at: location) else { return }
      diagnostics.diagnose(.superselfinit_not_called_before_return, 0, at: location)
    } else if use?.kind == .closureCapture {
      guard shouldEmitError(for: storage.allocation, at: useLocation) else { return }
      diagnostics.diagnose(.self_closure_use_uninit, at: useLocation)
    } else if let upcast = use?.instruction as? UpcastInst,
              let (name, isAccessor, location) = inheritedMember(of: upcast) {
      guard shouldEmitError(for: storage.allocation, at: location) else { return }
      diagnostics.diagnose(.self_use_before_fully_init, name, isAccessor ? 1 : 0, 1, at: location)
    } else if let method = use.flatMap({ calledMethod(by: $0.instruction) }) {
      guard shouldEmitError(for: storage.allocation, at: useLocation) else { return }
      let (name, isAccessor) = diagnosticName(of: method)
      diagnostics.diagnose(.self_use_before_fully_init, name, isAccessor ? 1 : 0, 1, at: useLocation)
    } else {
      guard shouldEmitError(for: storage.allocation, at: useLocation) else { return }
      diagnostics.diagnose(.self_before_superinit, at: useLocation)
    }
  }

  /// The inherited method, or stored property, accessed through `upcast` of `self`, and where it is accessed.
  private func inheritedMember(of upcast: Value) -> (name: Identifier, isAccessor: Bool, location: Location)? {
    for use in upcast.uses {
      switch use.instruction {
      case let borrow as BeginBorrowInst:
        if let member = inheritedMember(of: borrow) {
          return member
        }
      case let classMethod as ClassMethodInst:
        guard let method = classMethod.member.decl as? FuncDecl,
              let apply = classMethod.uses.lazy.compactMap({ $0.instruction as? FullApplySite }).first else {
          continue
        }
        let (name, isAccessor) = diagnosticName(of: method)
        return (name, isAccessor, apply.location)
      case let projection as RefElementAddrInst:
        guard let field = upcast.type.getNominalFields(in: function)?
                            .getUserFacingFieldDecl(withIndex: projection.fieldIndex) else {
          continue
        }
        // The access is located at the assignment or read, rather than the projection.
        let access = projection.uses.singleUser(ofType: BeginAccessInst.self)
        let accessor = access?.uses.lazy.filter({ !($0.instruction is EndAccessInst) }).first?.instruction
        return (field.baseIdentifier, true, (accessor ?? projection).location)
      default:
        continue
      }
    }
    return nil
  }

  /// The last `return` statement on the way to `use`, if there is one.
  private func explicitReturnLocation(of use: OffendingUse) -> Location? {
    use.path.reversed().lazy.map { $0.terminator.location }.first(where: { $0.isReturn })
  }

  /// One "'self.x' not initialized" note per uninitialized stored property, or part of one.
  private func noteUninitializedStoredProperties(of storage: Storage, _ leaves: Set<Int>, fallback: Location) {
    storage.space.fields.forEachCoveringSubtree(of: storage.fieldLeaves(leaves), excludingRoot: true) { path in
      let loc = storage.storedProperty(at: path)?.nameLoc ?? sourceLoc(fallback)
      diagnostics.diagnose(.stored_property_not_initialized, storage.name(of: path), at: loc)
    }
  }

  //===--------------------------------------------------------------------===//
  //                              Let re-init
  //===--------------------------------------------------------------------===//

  /// Diagnoses the `letReinit` markers.
  mutating func diagnoseLetReinits(_ markers: [LifetimeResolutionMarker]) {
    for case .letReinit(let marker) in markers {
      let address = marker.operand.value
      guard let storage = storage(backing: address) else {
        diagnostics.diagnose(.definite_init_bug_unknown_storage, at: marker.location)
        continue
      }
      let path = address.projectionPathFromRoot

      // The superclass part of derived class `self`, initialized by a second `super.init`.
      if storage.root.mode == .derivedClassSelf && path.isEmpty {
        let superInit = (marker.next as? LoadInst).flatMap { DerivedSelfUse($0.addressOperand).superInitCall }
        let location = superInit?.location ?? marker.location
        guard shouldEmitError(for: storage.allocation, at: location) else { continue }
        diagnostics.diagnose(.selfinit_multiple_times, 0, at: location)
        continue
      }

      guard shouldEmitError(for: storage.allocation, at: marker.location) else { continue }
      diagnostics.diagnose(.immutable_property_already_initialized, storage.name(of: path), at: marker.location)

      let decl = storage.root.isSelf ? storage.storedProperty(at: path) : storage.root.varDecl
      if let decl {
        if decl.hasInitialValue {
          diagnostics.diagnose(.initial_value_provided_in_let_decl, at: decl.nameLoc)
        }
        decl.emitLetToVarNoteIfSimple()
      }
    }
  }

  //===--------------------------------------------------------------------===//
  //                            Use after consume
  //===--------------------------------------------------------------------===//

  private struct UseAfterConsume {
    let violation: ConsumeViolation
    let use: OffendingUse?
    let name: String
    let storage: Value
    let consumedType: Type
    let declaration: (name: String, type: Type, location: SourceLoc?)?
    var consumes: [ConsumeSite]
  }

  /// Diagnoses the `unpermittedCopy` markers.
  mutating func diagnoseUsesAfterConsume() {
    for marker in unrecognizedConsumes {
      diagnostics.diagnose(.sil_movechecking_bug_missed_copy, at: marker.location)
    }

    // Each use is reported once, noting every consume that reaches it.
    var reports: [UseAfterConsume] = []
    for consume in consumes {
      guard let report = analyze(consume) else {
        diagnostics.diagnose(.sil_movechecking_bug_missed_copy, at: consume.marker.location)
        continue
      }
      if !report.violation.isReportedAtConsume,
         let i = reports.firstIndex(where: { $0.violation == report.violation && $0.storage == report.storage &&
                                             $0.use?.instruction == report.use?.instruction }) {
        reports[i].consumes.append(consume)
      } else {
        reports.append(report)
      }
    }

    for report in reports {
      emit(report)
    }
  }

  private mutating func analyze(_ consume: ConsumeSite) -> UseAfterConsume? {
    let storageValue: Value
    let consumedType: Type
    var storage: Storage? = nil
    var consumedPath: AccessPath? = nil
    let uses: UseMap

    if let copy = consume.instruction as? CopyValueInst {
      storageValue = copy.fromValue
      consumedType = copy.fromValue.type
      uses = self.uses(of: copy.fromValue)
    } else {
      let address = consume.address!
      guard let s = self.storage(backing: address) else { return nil }
      storage = s
      storageValue = s.allocation
      consumedType = address.type.objectType
      consumedPath = address.accessPath
      uses = self.uses(of: s)
    }

    // A failed `super.init` already released `self`, which the memory then no longer holds.
    if let storage, storage.root.mode == .derivedClassSelf, let load = consume.instruction as? LoadInst,
       let superInit = DerivedSelfUse(load.addressOperand).superInitCall {
      let use = findOffendingUse(of: .path(nil), in: uses, after: superInit, context)
      return UseAfterConsume(violation: use != nil ? .usedAfterFailedSuperInit : .unknownUse, use: use,
                             name: storage.rootName, storage: storageValue, consumedType: consumedType,
                             declaration: nil, consumes: [consume])
    }

    let use = findOffendingUse(of: .path(consumedPath), in: uses, after: consume.instruction, context)
    let violation: ConsumeViolation
    if let use {
      if use.isLoop {
        violation = .consumedInLoop
      } else if use.kind == .functionExit {
        violation = .notReinitialized
      } else if userInstruction(of: use.instruction) == consume.consumingInstruction {
        violation = .consumedAndUsedAtOnce
      } else if use.kind == .consume {
        violation = .consumedTwice
      } else if let consumedPath, let usePath = use.accessPath, usePath.contains(consumedPath) {
        violation = .usedAfterPartialConsume
      } else if use.accessPath == nil, storage?.root.isClassInstance ?? false {
        // A use of the whole instance after consuming one of its stored properties.
        violation = .usedAfterPartialConsume
      } else {
        violation = .usedAfterConsume
      }
    } else {
      violation = .unknownUse
    }

    // Name what was consumed, or for a partial consume, what was used.
    let name: String
    let declaration: (name: String, type: Type, location: SourceLoc?)?
    if let storage {
      if violation == .usedAfterPartialConsume {
        name = use!.accessPath.map { storage.name(of: $0.projectionPathFromRoot) } ?? storage.rootName
      } else {
        name = storage.name(of: consumedPath!.projectionPathFromRoot)
      }
      declaration = storage.root.isSelf ? nil
                                        : storage.root.varDecl.map { (storage.rootName, storage.type, $0.nameLoc) }
    } else {
      let varDecl = storageValue.findVarDecl()
      name = varDecl?.userFacingName.string ?? ""
      declaration = varDecl.map { ($0.userFacingName.string, storageValue.type, $0.nameLoc) }
    }

    return UseAfterConsume(violation: violation, use: use, name: name, storage: storageValue,
                           consumedType: consumedType, declaration: declaration, consumes: [consume])
  }

  private mutating func emit(_ report: UseAfterConsume) {
    let first = report.consumes[0]
    let errorLocation = report.violation.isReportedAtConsume ? first.location : report.use!.instruction.location
    guard shouldEmitError(for: report.storage, at: errorLocation) else { return }

    switch report.violation {
    case .usedAfterConsume, .unknownUse:
      diagnostics.diagnose(.sil_movechecking_value_used_after_consume, report.name, at: errorLocation)
    case .consumedTwice:
      diagnostics.diagnose(.sil_movechecking_owned_value_consumed_more_than_once, report.name, at: errorLocation)
    case .usedAfterPartialConsume:
      diagnostics.diagnose(.sil_movechecking_use_after_partial_consume, report.name, at: errorLocation)
    case .consumedInLoop:
      diagnostics.diagnose(.sil_movechecking_value_consumed_in_a_loop, report.name, at: errorLocation)
    case .consumedAndUsedAtOnce:
      diagnostics.diagnose(.sil_movechecking_owned_value_consumed_and_used_at_same_time, report.name,
                           at: errorLocation)
    case .notReinitialized:
      let isClosureCapture = (report.storage as? FunctionArgument)?.isClosureCapture ?? false
      diagnostics.diagnose(.sil_movechecking_not_reinitialized_before_end_of_function, report.name,
                           isClosureCapture ? 1 : 0, at: errorLocation)
      noteExit(report.use!, report.name)
    case .usedAfterFailedSuperInit:
      diagnostics.diagnose(.self_inside_catch_superselfinit, 0, at: errorLocation)
      return
    }

    // Point out every consume that reaches the use, explaining why it consumes where possible.
    for consume in report.consumes where !consume.location.hasSameSourceLocation(as: errorLocation) {
      if report.violation == .usedAfterPartialConsume {
        diagnostics.diagnose(.sil_movechecking_partial_consume_here, at: consume.location)
      } else {
        noteConsume(consume)
      }
    }

    // If the type is copyable, then suggest using `copy` to fix the use-after-consume.
    for consume in report.consumes where consume.isCopyable {
      diagnostics.diagnose(.sil_movechecking_add_explicit_copy, copySuggestion(for: consume, in: report),
                           at: consume.location)
    }

    // Explain that the value cannot be implicitly copied, and suggest borrowing instead.
    guard report.consumedType.isMoveOnly else { return }
    if let declaration = report.declaration, let loc = declaration.location,
       !explainedStorage.contains(where: { $0 == report.storage }) {
      explainedStorage.append(report.storage)
      diagnostics.diagnose(.sil_movechecking_noncopyable_declared_here, declaration.name, declaration.type, at: loc)
    }
    for consume in report.consumes {
      if suggestBorrowingParameter(for: consume) { break }
    }
  }

  /// What to `copy` at `consume` to keep the use in `report` valid.
  private mutating func copySuggestion(for consume: ConsumeSite, in report: UseAfterConsume) -> String {
    // Copying the reference to a class instance doesn't keep its stored properties valid.
    guard report.violation == .usedAfterPartialConsume, let address = consume.address,
          let storage = storage(backing: address), storage.root.isClassInstance else {
      return report.name
    }
    return storage.name(of: address.projectionPathFromRoot)
  }

  /// Points out where the function exits before `name` is reinitialized: by throwing an error, or by an
  /// explicit `return`. Falling off the end of the function needs no explanation.
  private func noteExit(_ use: OffendingUse, _ name: String) {
    switch use.instruction {
    case is ReturnInst:
      if let returnLocation = explicitReturnLocation(of: use) {
        diagnostics.diagnose(.sil_movechecking_exit_without_reinit, 2, name, at: returnLocation)
      }

    case is ThrowInst, is ThrowAddrInst:
      // The last call on the path whose error leads to the exit, e.g. `try f()`.
      let throwingCall = zip(use.path, use.path.dropFirst()).reversed().lazy.compactMap { predecessor, block in
        (predecessor.terminator as? TryApplyInst).flatMap { $0.errorBlock == block ? $0 : nil }
      }.first

      if let throwingCall {
        diagnostics.diagnose(.sil_movechecking_exit_without_reinit, 0, name, at: throwingCall.location)
      } else {
        // The `throw` is located at the end of its statement, so point at its start instead.
        let location = use.instruction.location
        diagnostics.diagnose(.sil_movechecking_exit_without_reinit, 1, name,
                             at: location.startSourceLoc ?? sourceLoc(location))
      }

    default:
      break
    }
  }

  /// The parameter of a call's callee that receives `operand`, if it is a parameter written in source.
  private func calleeParameter(of operand: Operand) -> (callee: AbstractFunctionDecl, param: ParamDecl?, isSelf: Bool)? {
    guard let apply = operand.instruction as? FullApplySite,
          let callee = apply.referencedFunction?.location.decl as? AbstractFunctionDecl,
          let argIndex = apply.calleeArgumentIndex(of: operand) else {
      return nil
    }
    let conventions = apply.calleeArgumentConventions
    if argIndex == conventions.selfIndex {
      return (callee, nil, true)
    }
    let paramIndex = argIndex - conventions.firstParameterIndex
    let params = callee.parameterList
    guard paramIndex >= 0 && paramIndex < params.count else {
      return (callee, nil, false)
    }
    return (callee, params[paramIndex], false)
  }

  private func noteConsume(_ consume: ConsumeSite) {
    let location = consume.location
    if let consumer = consume.consumer, let (callee, _, isSelf) = calleeParameter(of: consumer) {
      if isSelf {
        diagnostics.diagnose(.sil_movechecking_consumed_by_method_here, callee, at: location)
      } else {
        diagnostics.diagnose(.sil_movechecking_consumed_by_argument_here, callee, at: location)
      }
      return
    }
    diagnostics.diagnose(.sil_movechecking_consuming_use_here, at: location)
  }

  /// Suggests changing an explicitly `consuming` parameter to `borrowing`. Returns true if a suggestion was made.
  private mutating func suggestBorrowingParameter(for consume: ConsumeSite) -> Bool {
    guard let consumer = consume.consumer,
          let (callee, param, _) = calleeParameter(of: consumer),
          let param, let specifierLoc = param.consumingSpecifierLoc else {
      return false
    }
    guard suggestedParameters.insert(param).inserted else { return true }
    diagnostics.diagnose(.sil_movechecking_consider_borrowing_parameter, [callee], at: specifierLoc,
      fixIts: [DiagnosticFixIt(start: specifierLoc, byteLength: "consuming".utf8.count, replacement: "borrowing")])
    return true
  }
}

let lifetimeResolutionDiagnoseTest = FunctionTest("lifetime_resolution_diagnose") {
  function, arguments, context in
  diagnoseLifetimeViolations(function, context)
}
