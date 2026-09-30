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

  log("\n\n\n*** Starting LifetimeResolution on \(function.name.string)")

  var resolver = Resolver(function, context)
  var indexCache = FieldIndexTrieCache(for: function)

  // Process results in reverse post-order to ensure dependent uses are already resolved.
  // TODO: resolve `.guaranteed` values too. It should amount to having all consuming uses
  //   require a copy, including those on the boundary, and inserting end_access/end_borrow.
  for block in function.blocks.reversed() {
    for inst in block.instructions.reversed() {
      // Ignore `mark_uninitialized` this pass is driven by the storage itself.
      if inst is MarkUninitializedInst { continue }

      for result in inst.results where result.ownership == .owned || inst is AllocStackInst {
        resolver.resolve(result, &indexCache)
      }
    }
    for argument in block.arguments where argument.ownership == .owned {
      resolver.resolve(argument, &indexCache)
    }
  }
}

extension Resolver {
  mutating func resolve(_ root: Value, _ indexCache: inout FieldIndexTrieCache) {
    if root is AllocBoxInst || root is AllocStackInst {
      _ = run(on: root, &indexCache)
      return
    }

    // TODO: Eventually we should analyze and resolve trivial, non-address values
    //       in the same manner that we do for single-def values.
    if root.type.isTrivial(in: root.parentFunction) {
      return
    }

    resolveSingleDef(root, context)
  }
}


/// Identifies a resolvable storage root.
struct ResolvableRoot {
  let storage: Value

  /// The address that all accesses project from: the `mark_uninitialized` itself for
  /// an `alloc_stack`, or the `project_box` for an `alloc_box`.
  let address: Value

  /// The source variable, for diagnostics.
  let varDecl: VarDecl?

  /// Whether the binding is a `let` (vs a `var`). Selects the diagnostic wording.
  let isLet: Bool

  let isLexical: Bool

  /// Build from a storage allocation (`alloc_box` / `alloc_stack`). Callers holding only
  /// an address must first walk up to the allocation.
  init?(_ allocation: Value, _ context: FunctionPassContext) {
    self.storage = allocation

    let function = allocation.parentFunction
    var address: Value
    let varDecl: VarDecl?
    let isLet: Bool
    let isLexical: Bool

    switch allocation {
    case let allocStack as AllocStackInst:
      address = allocStack

      // A stack-backed local `let` or `var`; its `mark_uninitialized [var]` is the address.
      if let mu = allocStack.uses.singleUser(ofType: MarkUninitializedInst.self) {
        address = mu
      }

      varDecl = allocStack.varDecl
      isLet = allocStack.debugVariable?.isLet() ?? true   // TODO: find a more reliable way to discover this.
      isLexical = allocStack.isLexical
    case let allocBox as AllocBoxInst:
      guard let (projectBox, boxIsLexical) = Self.findProjectBox(of: allocBox) else { return nil }
      address = projectBox
      varDecl = allocBox.varDecl
      isLet = !allocBox.type.getBoxFields(in: function).isMutable(fieldIndex: 0)
      isLexical = boxIsLexical
    default:
      return nil
    }

    // Only handle a loadable, nontrivial value (a class, or a noncopyable
    // struct that just wraps one).
    let objectType = address.type.objectType
    guard objectType.isLoadable(in: function), !objectType.isTrivial(in: function)
    else {
      return nil
    }

    self.address = address
    self.varDecl = varDecl
    self.isLet = isLet
    self.isLexical = isLexical
  }

  /// Find the sole project_box reachable from an alloc_box from SILGen.
  private static func findProjectBox(of boxValue: Value) -> (projectBox: ProjectBoxInst, isLexical: Bool)? {
    for use in boxValue.uses {
      switch use.instruction {
      case let projectBox as ProjectBoxInst:
        return (projectBox, false)
      case let beginBorrow as BeginBorrowInst:
        if let (projectBox, isLexical) = findProjectBox(of: beginBorrow) {
          return (projectBox, isLexical || beginBorrow.isLexical)
        }
      case let markUninit as MarkUninitializedInst:
        if let result = findProjectBox(of: markUninit) {
          return result
        }
      default:
        break
      }
    }
    return nil
  }
}

private struct Resolver {
  let context: FunctionPassContext
  let function: Function

  typealias BlockState = Dictionary<BasicBlock, BlockInfo>
  var state: BlockState

  init(_ function: Function, _ context: FunctionPassContext) {
    self.context = context
    self.function = function
    self.state = [:]
  }

  mutating func reset() {
    // TODO: avoid reallocation by just resetting the BlockInfo's themselves.
    self.state = [:]
  }

  // - Returns: true iff legalization was successful
  mutating func run(on value: Value, _ indexCache: inout FieldIndexTrieCache) -> Bool {
    log("\nResolver.run(on: \(value))")

    reset()

    guard let root = ResolvableRoot(value, context) else {
      log("\n ** skipping due to unrecognized ResolvableRoot \(value)")
      return false
    }

    ////////////////
    // Step 1: Canonicalize copies/takes.
    //
    // This step comes before availability analysis, because we must know whether a `load`
    // is a take or not before we can determine what memory locations must be destroyed, etc.

    log("\n** Demand Analysis start **")

    let type = root.address.type
    let indices = indexCache.fieldIndices(for: type)
    log("FieldIndexTrie of \(type): \n\(indices)")

    self.state = Self.createBlockState(root, indices)

    // Initialize the demand per block
    for blk in function.blocks {
      state[blk]!.initialDemand()
    }

    logState("\n** state before solveDemand **")
    solveDemand(indices)

    logState("\n** state before legalizeDemand **")
    legalizeDemand(for: root)

    ////////////////
    // Step 2: Determine availability at lifetime ends, handling partially initialized destroys.

    logState("\n** state before initializing availability **")
    for blk in function.blocks {
      state[blk]!.initialAvailability()
    }

    logState("\n** state before solveAvailability **")
    solveAvailability(indices)

    logState("\n** state before legalizeAvailability **")
    legalizeAvailability()

    logState("\n** final state **")

    log("\n** Demand Analysis Phase finished **")
    return true
  }

  func logState(_ header: String? = nil) {
    if let header {
      log(header)
    }
    for blk in function.blocks {
      guard let info = state[blk] else {
        log("\n!! no info for bb\(blk.index)")
        continue
      }
      log("**bb\(blk.index)**\n\(info)")
    }
  }

  // Solve the backward demand-flow to a fixpoint.
  mutating func solveDemand(_ indices: FieldIndexTrie) {
    var worklist = BasicBlockWorklist(context)
    defer { worklist.deinitialize() }

    // Seed with every block.
    worklist.pushIfNotVisited(contentsOf: function.blocks)

    // Use popAndForget so a block can be re-enqueued when one of
    // its successors changes later; required to reach a fixpoint across loops.
    while let block = worklist.popAndForget() {
      guard let info = state[block] else { fatalError("no block state?") }

      var exits = Array.init(repeating: Demand.nothing, count: indices.leafCount)
      let range = info.demand.indices
      assert(exits.count == range.upperBound)

      // exit[i] = ⨆ over successors of the demand they place on field i
      for succ in block.successors {
        if let succInfo = state[succ] {
          for i in range {
            exits[i].formUnion(succInfo.demand[i].entry)
          }
        }
      }

      // entry[i] = gen[i] ∪ (exit[i] \ kill[i])
      for i in range {
        var demand = info.demand[i]
        let exit = exits[i]

        var entry = exit
        entry.subtract(demand.kill)
        entry.formUnion(demand.gen)

        guard entry != demand.entry || exit != demand.exit else {
          continue
        }

        // This block's demand changed. Update it and process its predecessors.
        demand.exit = exit
        demand.entry = entry
        state[block]!.demand[i] = demand
        worklist.pushIfNotVisited(contentsOf: block.predecessors)
      }
    }
  }

  //===--------------------------------------------------------------------===//
  // Legalization
  //===--------------------------------------------------------------------===//

  // Legalize the SIL from the solved demand, leaving markers behind for LifetimeResolutionDiagnose
  // to turn into user-facing diagnostics for any misuses that were corrected by this phase.
  mutating func legalizeDemand(for root: ResolvableRoot) {
    for block in function.blocks {
      guard var events = state[block]?.events else { continue }
      defer { state[block]!.events = events }

      // Tracks the demand between individual uses within this block as we walk bottom-up.
      // Initialized to the demand starting at the block's exit.
      var current = state[block]!.demand.map { $0.exit }

      for i in events.list.indices.reversed() {
        legalize(event: &events.list[i], successorDemand: &current, root)
      }
    }
  }

  // Legalize a single event with respect to the demand reaching it from its successor instruction.
  func legalize(event: inout EventList.Event, successorDemand current: inout [Demand], _ root: ResolvableRoot) {
    let noDemand = current[event.range].allSatisfy { $0 == .nothing }
    let someDemand = !noDemand

    let kind = event.kind
    var newKind: EventList.Kind = kind

    switch kind {
    case .take where someDemand:
      // A consume with demand still below it (use-after-consume): legalize with a copy.
      newKind = transform(kind, to: .load(.copy), root.isLexical)

    case .use(_, .own) where someDemand:
      // A use demanding ownership, where there is some demand following it, can only be satisfied with a copy.
      newKind = transform(kind, to: .load(.copy), root.isLexical)

    case .use(_, .own) where noDemand:
      // Canonicalize it into a take, as this is the last use.
      newKind = transform(kind, to: .load(.take), root.isLexical)

    case .use(_, .borrow) where someDemand:
      // A noncopyable type forces a load_borrow here, since there's demand for the memory
      // after this use no matter what nested accesses we may be nested within.
      if root.address.type.objectType.isMoveOnly {
        newKind = transform(kind, to: .loadBorrow, root.isLexical)
        break
      }

    // TODO: for Copyable types, we should have a conservative check to see if we're in a nested mutating access
    //       for this memory. If not, then it's proven safe to transform into a load_borrow (rdar://186687340)


    case .use(_, .borrow) where noDemand:
      // No users need ownership of the value, and there is no demand after this use.
      // We could either take or borrow; either way, we should avoid a copy if possible.
      // TODO: If ownership originally was guaranteed, yet we're choosing to do a take,
      //          we'd need to insert destroy_value's on non-consuming paths. It's probably hard to
      //          emit a load_borrow for a copyable type, given access scopes may end.
      newKind = transform(kind, to: .load(.take), root.isLexical)

    case .root where someDemand:
      // Unsatisfied demand reaching the root means there exists a use-before-init.
      initializeWithUndef(address: root.address, after: root.address.definingInstruction!)

    case .def where noDemand:
      // Arrived at a def with no demand below it: a dead assignment.
      log("!!!! could delete assignment: \(kind.inst)")

    default:
      break
    }

    // Update the kind, in case we did a transform.
    event.kind = newKind

    // Compose this use's transfer to get the demand above it.
    current[event.range].mutatingEach {
      $0.subtract(event.kind.kills)
      $0.formUnion(event.kind.gens)
    }
  }

  enum TransformRequest: CustomStringConvertible {
    case load(LoadInst.LoadOwnership)
    case loadBorrow

    var description: String {
      switch self {
      case let .load(kind): return "\(kind)"
      case .loadBorrow: return "borrow"
      }
    }
  }

  func transform(_ kind: EventList.Kind, to request: TransformRequest, _ isLexical: Bool) -> EventList.Kind {
    log("requested transform \(isLexical ? "(lexical) " : "")to \(request), given \(kind))")

    switch kind {
    case let .use(op, dem):
      fallthrough
    case let .take(op, dem):
      let newOp = transformOperand(op, to: request, isLexical)

      // Reclassify the event based on the transform request.
      if case .load(.take) = request {
        return .take(newOp, dem)
      }
      return .use(newOp, dem)

      default:
        fatalError("unexpected event kind to transform: \(kind)")
    }
  }

  // Performs the requested transformation.
  // If we're turning a take into a copy, or copying a noncopyable type, we add a `diagnose` instruction.
  func transformOperand(_ op: Operand, to request: TransformRequest, _ isLexical: Bool) -> Operand {
    switch op.instruction {
    // copy_addr
    case let copyAddr as CopyAddrInst:
      assert(op.value == copyAddr.source, "only the source is being read!")

      guard case let .load(newOwnership) = request else {
        fatalError("unhandled transform of copy_addr to \(request)")
      }

      let wasTake = copyAddr.isTakeOfSource
      let becomesCopy = newOwnership != .take
      copyAddr.set(isTakeOfSource: !becomesCopy, context)

      if becomesCopy && (wasTake || copyAddr.type.isMoveOnly) {
        Builder(after: copyAddr, context).createDiagnose(operand: op.value, kind: .unpermittedCopy)
      }
      return op

    case let load as LoadInst:
      switch request {
      // load [old] --> load [new]
      case let .load(newOwnership):
        let priorOwnership = load.loadOwnership
        load.set(ownership: newOwnership, context)

        if newOwnership == .copy && (priorOwnership == .take || load.type.isMoveOnly) {
          Builder(after: load, context).createDiagnose(operand: load, kind: .unpermittedCopy)
        }
        return load.operand

      // load [old] --> load_borrow
      case .loadBorrow:
        // Place the new load_borrow right after this load.
        let loadBorrow = Builder(after: load, context).createLoadBorrow(fromAddress: load.address)

        // Build a live range of the current users of the load.
        var loadUsers = PartitionedUses(of: load, context)
        defer { loadUsers.deinitialize() }
        loadUsers.partitionUses(withDeinitBarriers: isLexical)

        assert(loadUsers.consumes.isEmpty,
          "Can't transform load with these non-destroy consumers existing: \(loadUsers.consumes)")

        var range = loadUsers.fullLiveRange()
        defer { range.deinitialize() }

        // Insert end_borrow's across the entire liveness boundary of the current load.

        // TODO: this doesn't yet handle accesses that end early:
        //      %a = begin_access [read]
        //      %x = load [copy] %a
        //      end_access %a
        //      use %x
        //  When introducing the load_borrow, we need to sink all outer access scopes of the address too!
        range.alongBoundary(context) { builder in
          builder.createEndBorrow(of: loadBorrow)
        }

        // Replace users of the load with the load_borrow, and then delete the load.
        for use in load.uses {
          if let destroy = use.instruction as? DestroyValueInst {
            context.erase(instruction: destroy)
            continue
          }
          use.set(to: loadBorrow, context)
        }
        context.erase(instruction: load)
        return loadBorrow.operand
      }
    default:
      fatalError("unexpected inst to transform: \(op.instruction)")
    }
  }

  // Emit an `assign undef to [init] address`.
  func initializeWithUndef(address: Value, after inst: Instruction) {
    let builder = Builder(after: inst, context)
    let undef = Undef.get(type: address.type.objectType, context)
    builder.createAssign(source: undef, destination: address, ownership: .initialize)
  }

  // Forward dataflow solving for the availability of fields, to a fixed-point.
  mutating func solveAvailability(_ indices: FieldIndexTrie) {
    var worklist = BasicBlockWorklist(context)
    defer { worklist.deinitialize() }

    // Seed with every block.
    worklist.pushIfNotVisited(contentsOf: function.blocks)

    // Use popAndForget so a block can be re-enqueued when one of
    // its predecessors changes later; required to reach a fixpoint across loops.
    while let block = worklist.popAndForget() {
      guard let info = state[block] else { fatalError("no block state?") }

      var entries = Array.init(repeating: Availability.unknown, count: indices.leafCount)
      let range = info.availability.indices
      assert(entries.count == range.upperBound)

      // entry[i] = ⨆ over predecessors of the availability they provide for field i
      for pred in block.predecessors {
        if let predInfo = state[pred] {
          for i in range {
            entries[i].formUnion(predInfo.availability[i].exit)
          }
        }
      }

      // exit[i] = gen[i] ∪ (entry[i] \ kill[i])
      for i in range {
        var avail = info.availability[i]
        let entry = entries[i]

        var exit = entry
        exit.subtract(avail.kill)
        exit.formUnion(avail.gen)

        guard entry != avail.entry || exit != avail.exit else {
          continue
        }

        // This block's availability changed. Update it and process its successors.
        avail.entry = entry
        avail.exit = exit
        state[block]!.availability[i] = avail
        worklist.pushIfNotVisited(contentsOf: block.successors)
      }
    }
  }

  mutating func legalizeAvailability() {
    for block in function.blocks {
      guard var events = state[block]?.events else { continue }

      // Tracks the availability between individual events within this block as we walk top-down.
      // Initialized to the availability at the block's entry.
      var current = state[block]!.availability.map { $0.entry }

      // - Returns true iff this event's instruction was deleted.
      func legalize(kind: EventList.Kind, range: EventList.IndexRange) -> Bool {
        // Log the computed availability for now.
        let fields = current[range].enumerated()
                .map { "\(range.lowerBound + $0.offset)=\($0.element)" }
                .joined(separator: " ")
        log("availability at [\(fields)] | \(kind.inst)")

        var didDelete = false

        switch kind {
        case .def:
          // TODO: update any `store _ to [assign]` to a `store _ to [init]` if there is no availability.
          // Otherwise, we need to conditionally destroy.
          break
        case let .end(op):
          let fullyConsumed = current[range].allSatisfy { $0 == .no }

          if fullyConsumed {
            context.erase(instruction: op.instruction)
            didDelete = true
            break
          }

        // TODO: handle destruction of conditionally initialized storage!

        default:
          break
        }

        // Compose this event's forward transfer to get the availability below it.
        current[range].mutatingEach {
          $0.subtract(kind.availKill)
          $0.formUnion(kind.availGen)
        }

        return didDelete
      }

      // Process the events for this block, removing any that must be dropped.
      events.list.removeAll(where: legalize)
      state[block]!.events = events
    }
  }

  // FIXME: a LifetimeDependenceDefUseWalker would be better to use here.
  private struct UseCollector : AddressDefUseWalker {
    private(set) var uses: Dictionary<BasicBlock, EventList> = [:]
    private let indices: FieldIndexTrie

    init(_ indices: FieldIndexTrie) { self.indices = indices }

    mutating func addUse(_ k:  EventList.Kind, _ range: EventList.IndexRange) {
      log("\taddUse of \(range) as \(k)")
      let blk = k.inst.parentBlock
      uses[blk, default: EventList(blk)].add(k, range)
    }

    mutating func collectUses(of root: ResolvableRoot) -> Bool {
      let address = root.address
      log("** starting collectUses on \(address) **")

      // Add the root use to do detect uninitialized paths.
      addUse(.root(root), indices.wholeRange)

      if walkDownUses(ofAddress: address, path: Path()) == .abortWalk {
        return false
      }

      // Until we have a better handle on escaping uses, rely on AllocBoxToStack to leave behind only
      // alloc_box's that are escaping. To catch illegal consumes of the box, add its lifetime ends as
      // some unknown use of the whole box.
      if root.storage is AllocBoxInst {
        var storage = root.storage
        if let mu = storage.uses.singleUser(ofType: MarkUninitializedInst.self) {
          storage = mu
        }
        for end in storage.uses.endingLifetime {
          addUse(.unknown(end), indices.wholeRange)
        }
      }


      // Ensure correct order within each block
      for blk in uses.keys {
        uses[blk]?.finalize()
      }

      return true
    }

    mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
      let ap = address.value.accessPath
      let inst = address.instruction
      let path = ap.projectionPath
      let range = indices.range(of: path)
      log("leafUse(\(path)) of \(range) in \t\(inst)")

      switch inst {
      case let load as LoadInst:
        switch load.loadOwnership {
        case .take:
            addUse(.take(address, .own), range)   // TODO: what about a take where all non-destroy users borrow?
        case .unqualified, .copy:
          if !load.uses.filter({ $0.ownership == .forwardingConsume }).isEmpty {
            // If the loaded SSA value is consumed by some instruction, its demand is "own"
            addUse(.use(address, .own), range)
          } else {
            addUse(.use(address, .borrow), range)
          }
        default:
          fatalError("unexpected load: \(load)")
        }
      case let store as StoringInstruction where store.destination == address.value:
        addUse(.def(address), range)
      case let apply as FullApplySite where apply.convention(of: address) == .indirectOut:
        addUse(.def(address), range)
      case let tac as TupleAddrConstructorInst where tac.destinationOperand == address:
        addUse(.def(address), range)
      case let copyAddr as CopyAddrInst:
        if copyAddr.destination == address.value {
          addUse(.def(address), range)
        } else if copyAddr.isTakeOfSource {
          addUse(.take(address, .own), range)
        } else {
          addUse(.use(address, .own), range)
        }

      case is DestroyAddrInst:
        addUse(.end(address), range)
      case is DeallocStackInst:
        // skip
        break
      default:
        addUse(.unknown(address), range)
      }
      return .continueWalk
    }
  }

  static func createBlockState(_ root: ResolvableRoot, _ indices: FieldIndexTrie) -> BlockState {
    var uc = UseCollector(indices)
    guard uc.collectUses(of: root) else {
      fatalError("collectUses aborted its walk?")
    }

    var state: BlockState = [:]

    let emptyDemand = Array.init(repeating: BlockDemand(), count: indices.leafCount)
    let emptyAvailability = Array.init(repeating: BlockAvailability(), count: indices.leafCount)

    // Initialize an empty block state along with the accesses for that particular block.
    // We need state for all blocks, so information transits across them.
    let function = root.address.parentFunction
    for block in function.blocks {
      let uses = uc.uses[block]
      state[block] = BlockInfo(emptyDemand, emptyAvailability, uses)
    }

    return state
  }

  // What kind of ownership is demanded by a successor instruction or block?
  struct BlockDemand: CustomStringConvertible {
    var entry: Demand = .nothing // What is the ownership demanded by this block of its predecessors?
    var gen: Demand = .nothing   // What demand is generated within this block?
    var kill: Demand = .nothing  // What demand is satisfied within this block?
    var exit: Demand = .nothing  // What is the ownership demanded from this block by successors?

    var description: String {
      return "[entry: \(entry), gen: \(gen), kill: \(kill), exit: \(exit)]"
    }
  }

  // What is the initialization state of a field within this block?
  // The forward dual of BlockDemand: information flows from predecessors to successors.
  struct BlockAvailability: CustomStringConvertible {
    var entry: Availability = .unknown // Availability flowing in from predecessors.
    var gen: Availability = .unknown   // Availability produced within this block (reaches exit).
    var kill: Availability = .unknown  // Availability overwritten within this block.
    var exit: Availability = .unknown  // Availability flowing out to successors.

    var description: String {
      return "[entry: \(entry), gen: \(gen), kill: \(kill), exit: \(exit)]"
    }
  }

  /// Represents the state of ownership demanded during the backwards demand analysis.
  ///        All
  ///        / \
  ///   Borrow  Own
  ///        \ /
  ///        Nothing
  struct Demand: OptionSet, CustomStringConvertible {
    let rawValue: UInt8

    static let borrow = Demand(rawValue: 1 << 0)  // 0b01
    static let own  = Demand(rawValue: 1 << 1)  // 0b10

    static let nothing: Demand = []                // ⊥
    static let all: Demand     = [.borrow, .own] // ⊤

    var description: String {
      switch self {
      case .nothing: "∅"
      case .borrow:  "β"
      case .own:   "ω"
      case .all:     "⊤"
      default: fatalError("unknown bits: \(rawValue)")
      }
    }
  }

  /// Represents whether a field is initialized during the forward availability analysis.
  ///       Partial
  ///        / \
  ///     Yes   No
  ///        \ /
  ///      Unknown
  struct Availability: OptionSet, CustomStringConvertible {
    let rawValue: UInt8

    static let yes = Availability(rawValue: 1 << 0)  // 0b01 : definitely initialized
    static let no  = Availability(rawValue: 1 << 1)  // 0b10 : definitely uninitialized

    static let unknown: Availability = []          // ⊥
    static let partial: Availability = [.yes, .no] // ⊤ : initialized on some paths, not others

    var description: String {
      switch self {
      case .unknown: "?"
      case .yes:     "✓"
      case .no:      "✗"
      case .partial: "⊤"
      default: fatalError("unknown bits: \(rawValue)")
      }
    }
  }

  // Classified memory events within a BasicBlock.
  struct EventList: CustomStringConvertible {
    typealias IndexRange = FieldIndexTrie.IndexRange
    typealias Event = (kind: Kind, range: IndexRange)

    private let block: BasicBlock

    var list: [Event] // All of the categorized uses within this block.
    private(set) var finalized = false

    init(_ blk: BasicBlock) {
      self.block = blk
      self.list = []
    }

    var description: String {
      return list.map { "\($0.range.description) \($0.kind.description)" }
              .joined(separator: "\n")
    }

    enum Kind: CustomStringConvertible {
      case root(ResolvableRoot)
      case use(Operand, Demand)         // The demand summarizes the kinds of users of this operand's instruction.
      case take(Operand, Demand)        // The demand summarizes the kinds of users of this operand's instruction.
      case def(Operand)
      case end(Operand)       // An instruction representing the point at which the operand's lifetime has ended.
      case unknown(Operand)

      var inst: Instruction {
        switch self {
        case let .root(r): r.address.definingInstruction!
        case let .use(op, _): op.instruction
        case let .take(op, _): op.instruction
        case let .def(op): op.instruction
        case let .end(op): op.instruction
        case let .unknown(op): op.instruction
        }
      }

      // What is the ownership demanded by this use?
      var gens: Demand {
        switch self {
        case let .use(_, dem): dem
        case let .take(_, dem): dem
        case .def, .root: .nothing
        case .end: .nothing  // Demand analysis ignores lifetime ends.
        case .unknown: .all  // to be safe
        }
      }

      // What ownership does this use satisfy?
      var kills: Demand {
        switch self {
        case .def: .all
        default: .nothing
        }
      }

      // What availability does this event establish? (forward analysis)
      var availGen: Availability {
        switch self {
        case .def: .yes
        case .root, .take, .end: .no    // these leave the operand uninitialized.
        case .use: .unknown       // doesn't change the state
        case .unknown: .unknown   // TODO: conservatively handle once .unknown is removed
        }
      }

      // What prior availability does this event overwrite?
      var availKill: Availability {
        switch self {
        case .def, .root, .take, .end: .partial    // Overwites all availability.
        case .use, .unknown: .unknown
        }
      }

      var description: String {
        let name: String =
        switch self {
        case let .use(_, dem): "use(\(dem))"
        case let .take(_, dem): "take(\(dem))"
        case .root: "root"
        case .def: "def"
        case .end: "end"
        case .unknown: "UNKNOWN"
        }

        return name + " | \(inst)"
      }
    }

    mutating func add(_ k: Kind, _ range: IndexRange) {
      assert(k.inst.parentBlock == block)
      list.append((kind: k, range: range))
    }

    // Ensure uses are in a faithful block-local order.
    mutating func finalize() {
      guard !finalized else { return }
      list.sort(by: { $0.kind.inst.strictlyDominatesInBlock($1.kind.inst) })
      finalized = true
    }
  }

  struct BlockInfo: CustomStringConvertible {
    var events: EventList?    // Uses appearing within this block, if any.
    var demand: Array<BlockDemand>
    var availability: Array<BlockAvailability>

    init(_ demand: Array<BlockDemand>, _ avail: Array<BlockAvailability>, _ useList: EventList? = nil) {
      assert(!demand.isEmpty)
      if let provided = useList {
        assert(!provided.list.isEmpty, "You should pass nil if there are no uses.")
      }

      self.events = useList
      self.demand = demand
      self.availability = avail
    }

    mutating func initialDemand() {
      guard let events else { return }
      assert(events.finalized, "AccessList needs to be finalized!")

      // Scan backwards over the uses within this block to compute its 'gen' bits, so that entry = gen ∪ (exit \ kill).
      for (use, range) in events.list.reversed() {
        demand[range].mutatingEach {
          $0.gen.subtract(use.kills)     // a def above wipes demand generated below it
          $0.gen.formUnion(use.gens)     // this use's own demand survives to the top
          $0.kill.formUnion(use.kills)
          $0.entry = $0.gen
        }
      }
    }

    mutating func initialAvailability() {
      guard let events else { return }
      assert(events.finalized, "AccessList needs to be finalized!")

      // Scan forwards so that exit = gen ∪ (entry \ kill).
      for (event, range) in events.list {
        availability[range].mutatingEach {
          $0.gen.subtract(event.availKill)   // a def/take below overwrites what was produced above it
          $0.gen.formUnion(event.availGen)   // this event's own availability survives to the bottom
          $0.kill.formUnion(event.availKill)
          $0.exit = $0.gen
        }
      }
    }

    var description: String {
      return """
             BlockInfo {
             demand: {\n\(demand.enumerated().map { "\($0.offset) | \($0.element)" }.joined(separator: "\n"))\n}
             availability: {\n\(availability.enumerated().map { "\($0.offset) | \($0.element)" }.joined(separator: "\n"))\n}
             uses: {
             \(events?.description ?? "nil")
             }
             }
             """
    }
  }
}

// Static-single assignment values (SSA), aka objects in SIL, are defined exactly once, so we
// use a simpler, cheaper analysis for such values instead of the storage-backed one above.
private func resolveSingleDef(_ root: Value, _ context: FunctionPassContext) {
  assert(!root.type.isAddress, "addresses are multi-def")
  log("\n\nLifetimeResolution.resolveSingleDef(\(root))\n\n")

  var uses = PartitionedUses(of: root, context)
  defer { uses.deinitialize() }

  // Step 0: Find and partition uses of the values.
  let isLexical = root.isInLexicalLiverange(context)
  uses.partitionUses(withDeinitBarriers: isLexical)

  log(uses.description)

  // If there are absolutely no liveness uses, there's nothing to do.
  guard uses.haveLivenessUses() else {
    // TODO: Eventually this pass should ensure a destroy is placed right after the def.
    return
  }

  // Step 1: Resolve where copies are required to ensure there are no consumes before uses.
  uses.insertCopies()

  // Step 2: Insert destroys after non-consuming boundary users.
  uses.fixupDestroys()
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

  mutating func partitionUses(withDeinitBarriers addBarriers: Bool) {
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
    if addBarriers {
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

  // Compute the full range in which the root must be kept live.
  func fullLiveRange() -> InstructionRange {
    // TODO: cache this liverange to avoid recomputing it for callers?
    var liverange = InstructionRange(for: root, context)

    // Omit the destroys from the liverange during copy resolution. They're hoisted afterwards.
    liverange.insert(contentsOf: consumes.users)
    liverange.insert(contentsOf: uses.users)
    liverange.insert(contentsOf: indirectUses)  // Deinit barriers must be treated as uses.
    if hasUnboundedUse {
      // Liveness past an unbounded use is unknown, so the extend liveness to scope-ends.
      liverange.insert(contentsOf: lifetimeLimits)
    }
    return liverange
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
  mutating func insertCopies() {
    var liverange = fullLiveRange()
    defer { liverange.deinitialize() }
    log("liverange during insertCopies:\n\(liverange)")

    // If there's no consumes, there's no copies to insert.
    if consumes.isEmpty {
      return
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
  }

  // Insert destroy_value instructions along the boundary of the live range, anywhere the value
  // would exit the live range unconsumed: block exits, after last uses with respect to deinit barriers.
  func fixupDestroys() {
    assert(root.ownership == .owned, "fixupDestroys for non-owned root: \(root)")

    guard !hasUnboundedUse else {
      // TODO: for now, trust SILGen's placement of destroy_value.
      //  We probably should extend liveness until scope-ends.
      log("not placing destroys: liveness is not bounded")
      return
    }

    var liverange = fullLiveRange()
    defer { liverange.deinitialize() }
    log("liverange during fixupDestroys:\n\(liverange)")

    // These are just the existing destroy_values that we might
    // re-use if they're already in the right places.
    var hoistableDestroys: InstructionSet = InstructionSet(context)
    defer { hoistableDestroys.deinitialize() }
    hoistableDestroys.insert(contentsOf: lifetimeLimits)

    log("replacing destroys: \(hoistableDestroys)")
    placeDestroys(of: root, atBoundaryOf: liverange, reusing: &hoistableDestroys, context)
  }
}


fileprivate extension InstructionRange {
  func alongBoundary(_ context: FunctionPassContext, insertFunc: (Builder) -> ()) {
    for endInst in ends {
      Builder.insert(after: endInst, context, insertFunc: insertFunc)
    }
    for exitBlock in exitBlocks {
      let builder = Builder(atBeginOf: exitBlock, context)
      insertFunc(builder)
    }
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

  // If we're creating a copy of a noncopyable type, or forced to provide a copy
  // to a `move_value [allows_diagnostics]`, then it must be diagnosed.
  if op.value.type.isMoveOnly {
    builder.createDiagnose(operand: copyValue, kind: .unpermittedCopy)
  } else if let move = op.instruction as? MoveValueInst,
            move.allowsDiagnostics {
    builder.createDiagnose(operand: copyValue, kind: .unpermittedCopy)
  }

  op.set(to: copyValue, context)
}

private extension MutableCollection {
  mutating func mutatingEach(_ body: (inout Element) -> Void) {
    var i = startIndex
    while i != endIndex {
      body(&self[i])
      i = index(after: i)
    }
  }
}

let lifetimeResolutionResolveTest = FunctionTest("lifetime_resolution_resolve") {
  function, arguments, context in
  var resolver = Resolver(function, context)
  var indexCache = FieldIndexTrieCache(for: function)
  let root = arguments.takeValue()
  resolver.resolve(root, &indexCache)
}
