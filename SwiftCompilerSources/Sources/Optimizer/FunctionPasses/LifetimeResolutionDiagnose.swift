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
  // Collect the markers up front. The `diagnose` markers are erased once every diagnostic has been emitted.
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

  var diagnoser = Diagnoser(function, context)
  diagnoser.diagnoseUsesBeforeInit(undefInits)
  diagnoser.diagnoseLetReinits(letReinits)
  diagnoser.diagnoseUsesAfterConsume(unpermittedCopies)

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

  var rootName: String {
    if initSelfKind != nil {
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
  case inoutArgument    // passed by reference, e.g. `&x`
  case closureCapture   // captured by a closure
  case functionCapture  // captured by a nested function
  case addressTaken     // converted to a pointer
  case returnFromInit   // `self` returned from an initializer
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

  /// The use of `subelement` by `inst`, where a nil `subelement` or access path is the whole storage.
  func use(_ inst: Instruction, of subelement: AccessPath?) -> Use? {
    uses[inst, default: []].first { use in
      guard let subelement, let accessPath = use.accessPath else { return true }
      return !accessPath.isDistinct(from: subelement)
    }
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
  let isInitSelf: Bool
  var uses = UseMap()

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    let inst = address.instruction
    let accessPath = address.value.accessPath
    func record(_ kind: UseKind) { uses.record(inst, kind, accessPath) }
    func kill() { uses.kill(inst, accessPath) }

    switch inst {
    case let load as LoadInst:
      if isInitSelf && flowsIntoReturn(load) {
        record(.returnFromInit)
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

/// Classifies the uses of an owned value, walking transparently through forwarding instructions.
private struct ValueUseClassifier: ValueDefUseWalker {
  typealias Path = SmallProjectionPath
  let ignoring: CopyValueInst
  var uses = UseMap()
  var walkDownCache = WalkerCache<SmallProjectionPath>()

  mutating func walkDown(value operand: Operand, path: SmallProjectionPath) -> WalkResult {
    if operand.instruction == ignoring {
      return .continueWalk
    }
    return walkDownDefault(value: operand, path: path)
  }

  mutating func leafUse(value: Operand, path: SmallProjectionPath) -> WalkResult {
    let inst = value.instruction
    switch inst {
    case is DestroyValueInst, is EndBorrowInst, is DebugValueInst, is DiagnoseInst:
      break
    default:
      uses.record(inst, .read)
    }
    return .continueWalk
  }
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
                                   path: path()))
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

//===----------------------------------------------------------------------===//
//                                 Diagnoser
//===----------------------------------------------------------------------===//

private struct Diagnoser {
  let function: Function
  let context: FunctionPassContext
  var indexCache: FieldIndexTrieCache

  /// The uses of each storage allocation, computed on demand.
  private var storages: [HashableValue: Storage] = [:]
  private var useMaps: [HashableValue: UseMap] = [:]

  /// Like DefiniteInitialization, report at most one error per storage and source location.
  private var emittedErrors: [(storage: Value, location: Location)] = []

  init(_ function: Function, _ context: FunctionPassContext) {
    self.function = function
    self.context = context
    self.indexCache = FieldIndexTrieCache(for: function)
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
    var classifier = AddressUseClassifier(isInitSelf: storage.initSelfKind != nil)
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
    useMaps[storage.allocation.hashable] = uses
    return uses
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

  /// `diagnose [unpermitted_copy] %x`: report the demoted copy as though it were an illegal consuming use.
  mutating func diagnoseUsesAfterConsume(_ markers: [DiagnoseInst]) {
    for marker in markers {
      switch marker.operand.value {
      case let load as LoadInst:
        diagnoseUseAfterConsume(ofAddress: load)
      case let copy as CopyValueInst:
        diagnoseUseAfterConsume(ofValue: copy, marker)
      default:
        // An error message backstop for unrecognized diagnose scenarios to ensure we still emit an error.
        diagnostics.diagnose(.sil_movechecking_bug_missed_copy, at: marker.location)
      }
    }
  }

  private mutating func diagnoseUseAfterConsume(ofAddress load: LoadInst) {
    let storage = self.storage(backing: load.address)
    let name = storage?.name(of: load.address.accessPath.projectionPath) ?? ""
    let uses = storage.map { self.uses(of: $0) } ?? UseMap()

    // Error at the offending downstream use; note at the consume that was demoted.
    let offending = findOffendingUse(of: load.address.accessPath, in: uses, after: load, context)?.instruction ?? load
    diagnostics.diagnose(.sil_movechecking_value_used_after_consume, name, at: offending.location)
    diagnostics.diagnose(.sil_movechecking_consuming_use_here, at: load.location)
  }

  // `%copy`'s only other use (besides `marker`) is what now consumes the copy instead of its source.
  private func diagnoseUseAfterConsume(ofValue copy: CopyValueInst, _ marker: DiagnoseInst) {
    guard let consuming = copy.uses.ignore(user: marker).singleUse else {
      diagnostics.diagnose(.sil_movechecking_bug_missed_copy, at: marker.location)
      return
    }
    let consumingInst = consuming.instruction
    let name = copy.fromValue.findVarDecl()?.userFacingName.string ?? ""
    var classifier = ValueUseClassifier(ignoring: copy)
    _ = classifier.walkDownUses(ofValue: copy.fromValue, path: SmallProjectionPath())

    // Error at the offending downstream use; note at the consume that was demoted.
    let offending = findOffendingUse(of: nil, in: classifier.uses, after: consumingInst, context)?.instruction
                      ?? consumingInst
    diagnostics.diagnose(.sil_movechecking_value_used_after_consume, name, at: offending.location)
    diagnostics.diagnose(.sil_movechecking_consuming_use_here, at: consumingInst.location)
  }
}

let lifetimeResolutionDiagnoseTest = FunctionTest("lifetime_resolution_diagnose") {
  function, arguments, context in
  diagnoseLifetimeViolations(function, context)
}
