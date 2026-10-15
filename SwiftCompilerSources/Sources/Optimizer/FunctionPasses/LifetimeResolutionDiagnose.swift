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

// Recognizes the markers left behind by LifetimeResolution's `legalize` and emits the user-facing diagnostics:
//   - `assign undef to [init]`      -> use-before-init
//   - `diagnose [let_reinit]`       -> second initialization of immutable storage
//   - `diagnose [unpermitted_copy]` -> use-after-consume
let lifetimeResolutionDiagnosePass = FunctionPass(name: "lifetime-resolution-diagnose") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else { return }

  diagnoseLifetimeViolations(function, context)
}

private func diagnoseLifetimeViolations(_ function: Function, _ context: FunctionPassContext) {
  // The `diagnose` markers are erased last, as they identify which copies were demoted consumes.
  var unpermittedCopies: [DiagnoseInst] = []
  var letReinits: [DiagnoseInst] = []
  var undefInits: [AssignInst] = []
  for inst in function.instructions {
    switch inst {
    case let diagnose as DiagnoseInst:
      switch diagnose.kind {
      case .unpermittedCopy: unpermittedCopies.append(diagnose)
      case .letReinit: letReinits.append(diagnose)
      case .invalid: break
      }
    case let assign as AssignInst where assign.assignOwnership == .initialize && assign.source is Undef:
      undefInits.append(assign)
    default:
      break
    }
  }

  var diagnoser = Diagnoser(function, unpermittedCopies: unpermittedCopies, context)
  diagnoser.diagnoseUsesBeforeInit(undefInits)
  diagnoser.diagnoseLetReinits(letReinits)
  diagnoser.diagnoseUsesAfterConsume()

  for marker in unpermittedCopies + letReinits {
    context.erase(instruction: marker)
  }
}

//===----------------------------------------------------------------------===//
//                                  Storage
//===----------------------------------------------------------------------===//

/// A storage location that LifetimeResolution resolved, with helpers to describe it to the user.
private struct Storage {
  let root: ResolvableRoot
  let indices: FieldIndexTrie

  var allocation: Value { root.storage }
  var function: Function { root.address.parentFunction }
  var type: Type { root.address.type.objectType }

  /// The initializer's `self`, if this is `self` within an initializer.
  var initSelfKind: MarkUninitializedInst.Kind? {
    switch root.markUninitialized?.kind {
    case .variable, .indirectResult, nil: nil
    case let kind?: kind
    }
  }

  /// `self` of a method or initializer.
  var isSelf: Bool { initSelfKind != nil || ((allocation as? FunctionArgument)?.isSelf ?? false) }

  var rootName: String {
    if isSelf {
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
      case .structField:
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
    guard case (.structField, let index, _) = path.pop(),
          let fields = type.getNominalFields(in: function), index < fields.count else {
      return nil
    }
    return fields.getUserFacingFieldDecl(withIndex: index)
  }
}

/// The storage allocation (`alloc_box`, `alloc_stack`, or argument) backing `address`, via its access path.
private func allocation(backing address: Value) -> Value? {
  switch address.accessPath.base {
  case .box(let projectBox):
    return projectBox.box.referenceRoot
  case .stack(let allocStack):
    return allocStack
  case .argument(let argument):
    return argument
  default:
    return nil
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

  /// The strongest use of `subelement` by `inst`, where a nil `subelement` or access path is the whole storage.
  func use(_ inst: Instruction, of subelement: AccessPath?) -> Use? {
    let overlapping = uses[inst, default: []].filter { use in
      guard let subelement, let accessPath = use.accessPath else { return true }
      return !accessPath.isDistinct(from: subelement)
    }
    return overlapping.first(where: { $0.kind == .consume }) ?? overlapping.first
  }

  /// Whether `inst` overwrites all of `subelement`.
  func isKill(_ inst: Instruction, of subelement: AccessPath?) -> Bool {
    kills[inst, default: []].contains { accessPath in
      subelement.map { accessPath.isEqualOrContains($0) } ?? accessPath.projectionPath.isEmpty
    }
  }
}

/// Classifies the uses of some storage.
private struct AddressUseClassifier: AddressDefUseWalker {
  typealias Path = UnusedWalkingPath
  let demotedConsumes: Set<Instruction>
  let isInitSelf: Bool
  var uses = UseMap()

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
  var current = value
  while let use = current.uses.ignoreDebugUses.singleUse {
    switch use.instruction {
    case is ReturnInst:
      return true
    case let enumInst as EnumInst:
      current = enumInst
    case let branch as BranchInst:
      current = branch.getArgument(for: use)
    default:
      return false
    }
  }
  return false
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
private func findOffendingUse(of subelement: AccessPath?, in uses: UseMap, after start: Instruction,
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
    switch marker.operand.value {
    case let load as LoadInst:
      instruction = load
      consumer = singleUse(of: load)
    case let copy as CopyValueInst:
      instruction = copy
      consumer = singleUse(of: copy)
    case let address where address.type.isAddress:
      // `copy_addr` immediately precedes its marker.
      guard let copyAddr = marker.previous as? CopyAddrInst, copyAddr.source == address else {
        return nil
      }
      instruction = copyAddr
      consumer = callReceivingTemporary(copyAddr.destination)
    default:
      return nil
    }
  }

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

  init(_ function: Function, unpermittedCopies: [DiagnoseInst], _ context: FunctionPassContext) {
    self.function = function
    self.context = context
    self.indexCache = FieldIndexTrieCache(for: function)

    var consumes: [ConsumeSite] = []
    var unrecognizedConsumes: [DiagnoseInst] = []
    for marker in unpermittedCopies {
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
    guard let allocation = allocation(backing: address) else {
      return nil
    }
    if let storage = storages[allocation.hashable] {
      return storage
    }
    guard let root = ResolvableRoot(allocation, &indexCache, context) else {
      return nil
    }
    let storage = Storage(root: root, indices: indexCache.fieldIndices(for: root.address.type))
    storages[allocation.hashable] = storage
    return storage
  }

  /// Classifies every use of `storage`.
  mutating func uses(of storage: Storage) -> UseMap {
    if let uses = useMaps[storage.allocation.hashable] {
      return uses
    }
    var classifier = AddressUseClassifier(demotedConsumes: demotedConsumes, isInitSelf: storage.initSelfKind != nil)
    _ = classifier.walkDownUses(ofAddress: storage.root.address, path: UnusedWalkingPath())
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

  /// `assign undef to [init] %leaf`: the leaf is read before being initialized.
  mutating func diagnoseUsesBeforeInit(_ markers: [AssignInst]) {
    // Group the uninitialized leaves of each storage by the use that first observes them.
    var reports: [(storage: Storage, use: OffendingUse?, leaves: Set<Int>)] = []
    for marker in markers {
      guard let storage = storage(backing: marker.destination) else {
        diagnostics.diagnose(.definite_init_bug_unknown_storage, at: marker.location)
        continue
      }
      let accessPath = marker.destination.accessPath
      let leaves = storage.indices.range(of: accessPath.projectionPath)
      let use = findOffendingUse(of: accessPath, in: uses(of: storage), after: marker, context)

      if let i = reports.firstIndex(where: { $0.storage.allocation == storage.allocation &&
                                             $0.use?.instruction == use?.instruction }) {
        reports[i].leaves.formUnion(leaves)
      } else {
        reports.append((storage, use, Set(leaves)))
      }
    }

    for (storage, use, leaves) in reports {
      emitUseBeforeInit(of: storage, at: use, leaves)
    }
  }

  private mutating func emitUseBeforeInit(of storage: Storage, at use: OffendingUse?, _ leaves: Set<Int>) {
    let useLocation = use?.instruction.location ?? storage.definitionLocation
    let isReturn = use?.kind == .returnFromInit

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
      } else {
        guard shouldEmitError(for: storage.allocation, at: useLocation) else { return }
        diagnostics.diagnose(.use_of_self_before_fully_init, at: useLocation)
      }
      noteUninitializedStoredProperties(of: storage, leaves, fallback: useLocation)
      return
    }

    guard shouldEmitError(for: storage.allocation, at: useLocation) else { return }

    if storage.initSelfKind != nil {
      // `self` of a delegating initializer, which must be initialized by `self.init` or assignment.
      if isReturn {
        diagnostics.diagnose(.superselfinit_not_called_before_return, 1, at: useLocation)
      } else {
        diagnostics.diagnose(.self_before_selfinit_value_type, at: useLocation)
      }
      return
    }

    // Name the whole variable if it is entirely uninitialized, otherwise its first uninitialized part.
    var firstPath: SmallProjectionPath? = nil
    storage.indices.forEachCoveringSubtree(of: leaves) { path in
      if firstPath == nil { firstPath = path }
    }
    let name = storage.name(of: firstPath ?? SmallProjectionPath())

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

  /// The last `return` statement on the way to `use`, if there is one.
  private func explicitReturnLocation(of use: OffendingUse) -> Location? {
    use.path.reversed().lazy.map { $0.terminator.location }.first(where: { $0.isReturn })
  }

  /// One "'self.x' not initialized" note per uninitialized stored property, or part of one.
  private func noteUninitializedStoredProperties(of storage: Storage, _ leaves: Set<Int>, fallback: Location) {
    storage.indices.forEachCoveringSubtree(of: leaves, excludingRoot: true) { path in
      let loc = storage.storedProperty(at: path)?.nameLoc ?? sourceLoc(fallback)
      diagnostics.diagnose(.stored_property_not_initialized, storage.name(of: path), at: loc)
    }
  }

  //===--------------------------------------------------------------------===//
  //                              Let re-init
  //===--------------------------------------------------------------------===//

  /// `diagnose [let_reinit] %addr`: immutable storage at `%addr` may already be initialized.
  mutating func diagnoseLetReinits(_ markers: [DiagnoseInst]) {
    for marker in markers {
      let address = marker.operand.value
      guard let storage = storage(backing: address) else {
        diagnostics.diagnose(.definite_init_bug_unknown_storage, at: marker.location)
        continue
      }
      guard shouldEmitError(for: storage.allocation, at: marker.location) else { continue }

      let path = address.accessPath.projectionPath
      diagnostics.diagnose(.immutable_property_already_initialized, storage.name(of: path), at: marker.location)

      let decl = storage.root.isRootSelf ? storage.storedProperty(at: path) : storage.root.varDecl
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

  /// `diagnose [unpermitted_copy] %x`: a consume was demoted to a copy, because the value is used afterwards.
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
      let address = (consume.instruction as? LoadInst)?.address ?? (consume.instruction as! CopyAddrInst).source
      guard let s = self.storage(backing: address) else { return nil }
      storage = s
      storageValue = s.allocation
      consumedType = address.type.objectType
      consumedPath = address.accessPath
      uses = self.uses(of: s)
    }

    let use = findOffendingUse(of: consumedPath, in: uses, after: consume.instruction, context)
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
      let path = violation == .usedAfterPartialConsume ? use!.accessPath! : consumedPath!
      name = storage.name(of: path.projectionPath)
      declaration = storage.isSelf ? nil : storage.root.varDecl.map { (storage.rootName, storage.type, $0.nameLoc) }
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
    }

    // Point out every consume that reaches the use, explaining why it consumes where possible.
    for consume in report.consumes where !consume.location.hasSameSourceLocation(as: errorLocation) {
      if report.violation == .usedAfterPartialConsume {
        diagnostics.diagnose(.sil_movechecking_partial_consume_here, at: consume.location)
      } else {
        noteConsume(consume)
      }
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
