//===--- LocalVariableUtils.swift - Utilities for local variable access ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// SIL operates on three kinds of addressable memory:
///
/// 1. Temporary RValues. These are recognized by AddressInitializationWalker. These largely disappear with opaque SIL
/// values.
///
/// 2. Local variables. These are always introduced by either a VarDeclInstruction or a Function argument with non-nil
/// Argument.varDecl. They are accessed according to the structural rules define in this file. Loading or reassigning a
/// property requires a formal access (begin_access).
///
/// 3. Stored properties in heap objects or global variables. These are always formally accessed.
///
//===----------------------------------------------------------------------===//

import SIL

private let verbose = false

private func log(prefix: Bool = true, _ message: @autoclosure () -> String) {
  if verbose {
    debugLog(prefix: prefix, message())
  }
}

// Local variables are accessed in one of these ways.
//
// Note: @in is only immutable up to when it is destroyed, so still requires a local live range.
//
// A .dependenceSource access creates a new dependent value that keeps this local alive.
//
//     %local = alloc_stack // this local
//     %md = mark_dependence %val on %local
//     mark_dependence_addr %adr on %local
//
// The effect of .dependenceSource on reachability is like a load of this local. The dependent value depends on any
// value in this local that reaches this point.
//
// A .dependenceDest access is the point where another value becomes dependent on this local:
//
//     %local = alloc_stack // this local
//     %md = mark_dependence %local on %val
//     mark_dependence_addr %local on %val
//
// The effect of .dependenceDest on reachability is like a store of this local. All uses of this local reachable from
// this point are uses of the dependence base.
//
// Note that the same mark_dependence_addr often involves two locals:
//
//     mark_dependence_addr %localDest on %localSource
//
struct LocalVariableAccess: CustomStringConvertible {
  enum Kind {
    case incomingArgument // @in, @inout, @inout_aliasable
    case outgoingArgument // @inout, @inout_aliasable
    case inoutYield       // indirect yield from this accessor
    case beginAccess // Reading or reassigning a 'var'
    case load        // Reading a 'let'. Returning 'var' from an initializer.
    case dependenceSource // A value/address depends on this local here (like a load)
    case dependenceDest  // This local depends on another value/address here (like a store)
    case store       // 'var' initialization and destruction
    case storeBorrow // scoped initialization of temporaries
    case apply       // indirect arguments
    case escape      // alloc_box captures
  }
  let kind: Kind
  // All access have an operand except .incomingArgument and .outgoingArgument.
  let operand: Operand?

  // All access have a representative instruction except .incomingArgument.
  var instruction: Instruction?

  init(_ kind: Kind, _ operand: Operand) {
    self.kind = kind
    self.operand = operand
    self.instruction = operand.instruction
  }

  init(_ kind: Kind, _ instruction: Instruction?) {
    self.kind = kind
    self.operand = nil
    self.instruction = instruction
  }

  /// Does this access either fully or partially modify the variable?
  var isModify: Bool {
    switch kind {
    case .beginAccess:
      switch (instruction as! BeginAccessInst).accessKind {
      case .read, .deinit:
        return false
      case .`init`, .modify:
        return true
      }
    case .load, .dependenceSource, .dependenceDest:
      return false
    case .incomingArgument, .outgoingArgument, .store, .storeBorrow, .inoutYield:
      return true
    case .apply:
      let apply = instruction as! FullApplySite
      if let convention = apply.convention(of: operand!) {
        // A direct argument may modify a captured variable.
        return !convention.isIndirectIn
      }
      // A callee argument may modify a captured variable.
      return true
    case .escape:
      return true
    }
  }

  var isEscape: Bool {
    switch kind {
    case .escape:
      return true
    default:
      return false
    }
  }

  var description: String {
    var str = ""
    switch self.kind {
    case .incomingArgument:
      str += "incomingArgument"
    case .outgoingArgument:
      str += "outgoingArgument"
    case .inoutYield:
      str += "inoutYield"
    case .beginAccess:
      str += "beginAccess"
    case .load:
      str += "load"
    case .dependenceSource:
      str += "dependenceSource"
    case .dependenceDest:
      str += "dependenceDest"
    case .store:
      str += "store"
    case .storeBorrow:
      str += "storeBorrow"
    case .apply:
      str += "apply"
    case .escape:
      str += "escape"
    }
    if let inst = instruction {
      str += "\(inst)"
    }
    return str
  }
}

/// Class instance for caching local variable information.
class LocalVariableAccessInfo: CustomStringConvertible {
  let access: LocalVariableAccess

  private var _isFullyAssigned: Bool?

  /// Cache whether the allocation has escaped prior to this access.
  /// This returns `nil` until reachability is computed.
  var hasEscaped: Bool?

  init(localAccess: LocalVariableAccess) {
    self.access = localAccess
    switch localAccess.kind {
    case .beginAccess:
      switch (localAccess.instruction as! BeginAccessInst).accessKind {
      case .read, .deinit:
        self._isFullyAssigned = false
      case .`init`, .modify:
        break // lazily compute full assignment
      }
    case .load, .dependenceSource, .dependenceDest:
      self._isFullyAssigned = false
    case .store, .storeBorrow:
      if let store = localAccess.instruction as? StoringInstruction {
        self._isFullyAssigned = LocalVariableAccessInfo.isBase(address: store.destination)
      } else {
        self._isFullyAssigned = true
      }
    case .apply:
      let apply = localAccess.instruction as! FullApplySite
      if let convention = apply.convention(of: localAccess.operand!) {
        self._isFullyAssigned = convention.isIndirectOut
      } else {
        self._isFullyAssigned = false
      }
    case .escape:
      self._isFullyAssigned = false
      self.hasEscaped = true
    case .inoutYield:
      self._isFullyAssigned = false
    case .incomingArgument, .outgoingArgument:
      fatalError("Function arguments are never mapped to LocalVariableAccessInfo")
    }
  }

  var instruction: Instruction { access.instruction! }

  var isModify: Bool { access.isModify }

  var isEscape: Bool { access.isEscape }

  /// Is this access a full assignment such that none of the variable's components are reachable from a previous
  /// access.
  func isFullyAssigned(_ context: Context) -> Bool {
    if let cached = _isFullyAssigned {
      return cached
    }
    if access.kind != .beginAccess {
      fatalError("Invalid LocalVariableAccess")
    }
    assert(isModify)
    let beginAccess = access.instruction as! BeginAccessInst
    let initializer = AddressInitializationWalker.findSingleInitializer(ofAddress: beginAccess, context: context)
    _isFullyAssigned = (initializer != nil) ? true : false
    return _isFullyAssigned!
  }

  var description: String {
    return "assign: \(_isFullyAssigned == nil ? "unknown" : String(describing: _isFullyAssigned!)), "
      + "\(access)"
  }

  // Does this address correspond to the local variable's base address? Any writes to this address will be a full
  // assignment. This should match any instructions that the LocalVariableAccessMap initializer below recognizes as an
  // allocation.
  static private func isBase(address: Value) -> Bool {
    // TODO: create an API alternative to 'accessPath' that bails out on the first path component and succeeds on the
    // first begin_access.
    let path = address.accessPath
    return path.base.isLocal && path.projectionPath.isEmpty
  }
}

/// Model the formal accesses of an addressable variable introduced by an alloc_box, alloc_stack, or indirect
/// FunctionArgument.
///
/// This instantiates a unique LocalVariableAccessInfo instances for each access instruction, caching it an an access
/// map.
///
/// TODO: In addition to isFullyAssigned, consider adding a lazily computed access path if any need arises.
struct LocalVariableAccessMap: Collection, CustomStringConvertible {
  let context: Context
  let allocation: Value

  let liveInAccess: LocalVariableAccess?

  // All mapped accesses have a valid instruction.
  //
  // TODO: replace the List,Dictionary with an OrderedDictionary.
  private var accessList: [LocalVariableAccessInfo]
  private var accessMap: Dictionary<Instruction, LocalVariableAccessInfo>

  var function: Function { allocation.parentFunction }

  var isBoxed: Bool { allocation is AllocBoxInst }

  var mayAlias: Bool {
    if let arg = allocation as? FunctionArgument, arg.convention == .indirectInoutAliasable {
      return true
    }
    return false
  }

  init?(allocation: Value, _ context: Context) {
    switch allocation {
    case is AllocBoxInst, is AllocStackInst:
      self.liveInAccess = nil
      break
    case let arg as FunctionArgument:
      switch arg.convention {
      case .indirectIn, .indirectInout, .indirectInoutAliasable:
        self.liveInAccess = LocalVariableAccess(.incomingArgument, nil)
      default:
        return nil
      }
    default:
      return nil
    }
    self.context = context
    self.allocation = allocation
    accessList = []
    accessMap = [:]
    if walkAccesses(context) == .abortWalk {
      return nil
    }
  }

  private mutating func walkAccesses(_ context: Context) -> WalkResult {
    var walker = LocalVariableAccessWalker(context)
    defer { walker.deinitialize() }
    if walker.walkDown(allocation: allocation) == .abortWalk {
      return .abortWalk
    }
    for localAccess in walker.accessStack {
      let info = LocalVariableAccessInfo(localAccess: localAccess)
      if mayAlias {
        // Local allocations can only escape prior to assignment if they are boxed or inout_aliasable.
        info.hasEscaped = true
      } else if !isBoxed {
        // Boxed allocation requires reachability to determine whether the box escaped prior to assignment.
        info.hasEscaped = info.isEscape
      }
      accessMap[localAccess.instruction!] = info
      accessList.append(info)
    }
    return .continueWalk
  }

  var startIndex: Int { 0 }

  var endIndex: Int { accessList.count }

  func index(after index: Int) -> Int {
    return index + 1
  }

  subscript(_ accessIndex: Int) -> LocalVariableAccessInfo { accessList[accessIndex] }

  subscript(instruction: Instruction) -> LocalVariableAccessInfo? { accessMap[instruction] }

  var description: String {
    "Access map for: \(allocation)\n" + map({String(describing: $0)}).joined(separator: "\n")
  }
}

/// Gather the accesses of a local allocation: alloc_box, alloc_stack, @in, @inout.
///
/// This is used to populate LocalVariableAccessMap.
///
/// Start walk:
///   walkDown(allocation:)
///
/// TODO: This should only handle allocations that have a var decl. And SIL verification should guarantee that the
/// allocated address is never used by a path projection outside of an access.
struct LocalVariableAccessWalker {
  let context: Context
  var visitedValues: ValueSet
  var accessStack: Stack<LocalVariableAccess>

  init(_ context: Context) {
    self.context = context
    self.visitedValues = ValueSet(context)
    self.accessStack = Stack(context)
  }

  mutating func deinitialize() {
    visitedValues.deinitialize()
    accessStack.deinitialize()
  }

  mutating func walkDown(allocation: Value) -> WalkResult {
    if allocation.type.isAddress {
      return walkDownAddressUses(address: allocation)
    }
    return walkDown(root: allocation)
  }

  private mutating func visit(_ localAccess: LocalVariableAccess) {
    accessStack.push(localAccess)
  }
}

// Extend ForwardingDefUseWalker to walk down uses of the box.
extension LocalVariableAccessWalker : ForwardingDefUseWalker {
  mutating func needWalk(for value: Value) -> Bool {
    visitedValues.insert(value)
  }

  mutating func nonForwardingUse(of operand: Operand) -> WalkResult {
    if operand.instruction.isIncidentalUse {
      return .continueWalk
    }
    switch operand.instruction {
    case let pbi as ProjectBoxInst:
      return walkDownAddressUses(address: pbi)
    case let transition as OwnershipTransitionInstruction:
      return walkDownUses(of: transition.ownershipResult, using: operand)
    case is DestroyValueInst:
      visit(LocalVariableAccess(.store, operand))
    case is DeallocBoxInst:
      break
    case let markDep as MarkDependenceInst:
      assert(markDep.baseOperand == operand)
      visit(LocalVariableAccess(.dependenceSource, operand))
    default:
      visit(LocalVariableAccess(.escape, operand))
    }
    return .continueWalk
  }

  mutating func deadValue(_ value: Value, using operand: Operand?) -> WalkResult {
    return .continueWalk
  }
}

// Extend AddressUseVisitor to find all access scopes, initializing stores, and captures.
extension LocalVariableAccessWalker: AddressUseVisitor {
  private mutating func walkDownAddressUses(address: Value) -> WalkResult {
    for operand in address.uses.ignoreTypeDependence {
      if classifyAddress(operand: operand) == .abortWalk {
        return .abortWalk
      }
    }
    return .continueWalk
  }

  // Handle storage type projections, like MarkUninitializedInst. Path projections are visited for field
  // initialization because SILGen does not emit begin_access [init] consistently.
  //
  // Stack-allocated temporaries are also treated like local variables for the purpose of finding all uses. Such
  // temporaries do not have access scopes, so we need to walk down any projection that may be used to initialize the
  // temporary.
  mutating func projectedAddressUse(of operand: Operand, into value: Value) -> WalkResult {
    if let md = value as? MarkDependenceInst {
      if operand == md.valueOperand {
        // Intercept mark_dependence destination to record an access point which can be used like a store when finding
        // all uses that affect the base after the point that the dependence was marked.
        visit(LocalVariableAccess(.dependenceDest, operand))
        // walk down the forwarded address as usual...
      } else {
        // A dependence is similar to loading from its source. Downstream uses are not accesses of the original local.
        visit(LocalVariableAccess(.dependenceSource, operand))
        return .continueWalk
      }
    }
    return walkDownAddressUses(address: value)
  }

  mutating func scopedAddressUse(of operand: Operand) -> WalkResult {
    switch operand.instruction {
    case is BeginAccessInst:
      visit(LocalVariableAccess(.beginAccess, operand))
      return .continueWalk
    case is BeginApplyInst:
      visit(LocalVariableAccess(.apply, operand))
      return .continueWalk
    case is LoadBorrowInst:
      visit(LocalVariableAccess(.load, operand))
      return .continueWalk
    case is StoreBorrowInst:
      visit(LocalVariableAccess(.storeBorrow, operand))
      return .continueWalk
    default:
      // A StoreBorrow should be guarded by an access scope.
      //
      // TODO: verify that we never hit this case.
      return .abortWalk // unexpected
    }
  }

  mutating func scopeEndingAddressUse(of operand: Operand) -> WalkResult {
    return .abortWalk // unexpected
  }

  mutating func leafAddressUse(of operand: Operand) -> WalkResult {
    switch operand.instruction {
    case is StoringInstruction, is SourceDestAddrInstruction, is DestroyAddrInst, is DeinitExistentialAddrInst,
         is InjectEnumAddrInst, is SwitchEnumAddrInst, is TupleAddrConstructorInst, is InitBlockStorageHeaderInst,
         is PackElementSetInst:
      // Handle instructions that initialize both temporaries and local variables.
      visit(LocalVariableAccess(.store, operand))
    case let md as MarkDependenceAddrInst:
      assert(operand == md.addressOperand)
      visit(LocalVariableAccess(.dependenceDest, operand))
    case is DeallocStackInst:
      break
    default:
      if !operand.instruction.isIncidentalUse {
        visit(LocalVariableAccess(.escape, operand))
      }
    }
    return .continueWalk
  }

  mutating func appliedAddressUse(of operand: Operand, by apply: FullApplySite) -> WalkResult {
    visit(LocalVariableAccess(.apply, operand))
    return .continueWalk
  }

  mutating func yieldedAddressUse(of operand: Operand) -> WalkResult {
    visit(LocalVariableAccess(.inoutYield, operand))
    return .continueWalk
  }

  mutating func dependentAddressUse(of operand: Operand, dependentValue value: Value) -> WalkResult {
    // Find all uses of partial_apply [on_stack].
    if let pai = value as? PartialApplyInst, !pai.mayEscape {
      var walker = NonEscapingClosureDefUseWalker(context)
      defer { walker.deinitialize() }
      if walker.walkDown(closure: pai) == .abortWalk {
        return .abortWalk
      }
      for operand in walker.applyOperandStack {
        visit(LocalVariableAccess(.apply, operand))
      }
    }
    // No other dependent uses can access to memory at this address.
    return .continueWalk
  }

  mutating func dependentAddressUse(of operand: Operand, dependentAddress address: Value) -> WalkResult {
    // No other dependent uses can access to memory at this address.
    return .continueWalk
  }

  mutating func loadedAddressUse(of operand: Operand, intoValue value: Value) -> WalkResult {
    visit(LocalVariableAccess(.load, operand))
    return .continueWalk
  }

  mutating func loadedAddressUse(of operand: Operand, intoAddress address: Operand) -> WalkResult {
    visit(LocalVariableAccess(.load, operand))
    return .continueWalk
  }

  mutating func escapingAddressUse(of operand: Operand) -> WalkResult {
    visit(LocalVariableAccess(.escape, operand))
    return .continueWalk
  }

  mutating func unknownAddressUse(of operand: Operand) -> WalkResult {
    return .abortWalk
  }
}

/// Map LocalVariableAccesses to basic blocks.
///
/// This caches flow-insensitive information about the local variable's accesses, for use with a flow-sensitive
/// analysis.
///
/// This allocates a dictionary for the block state rather than using BasicBlockSets in case the client wants to cache
/// it as an analysis. We expect a very small number of accesses per local variable.
struct LocalVariableAccessBlockMap {
  // Lattice, from most information to least information:
  // none -> read -> modify -> escape -> assign
  enum BlockEffect: Int {
    case read    // no modification or escape
    case modify  // no full assignment or escape
    case escape  // no full assignment
    case assign  // full assignment, other accesses may be before or after it.

    /// Return a merged lattice state such that the result has strictly less information.
    func meet(_ other: BlockEffect?) -> BlockEffect {
      guard let other else {
        return self
      }
      return other.rawValue > self.rawValue ? other : self
    }
  }
  struct BlockInfo {
    var effect: BlockEffect?
    var hasDealloc: Bool
  }
  var blockAccess: Dictionary<BasicBlock, BlockInfo>

  subscript(_ block: BasicBlock) -> BlockInfo? { blockAccess[block] }

  init(accessMap: LocalVariableAccessMap) {
    blockAccess = [:]
    for accessInfo in accessMap {
      let block = accessInfo.instruction.parentBlock
      let oldEffect = blockAccess[block]?.effect
      let newEffect = BlockEffect(for: accessInfo, accessMap.context).meet(oldEffect)
      blockAccess[block] = BlockInfo(effect: newEffect, hasDealloc: false)
    }
    // Find blocks that end the variable's scope. This is destroy_value for boxes.
    //
    // TODO: SIL verify that owned boxes are never forwarded.
    let deallocations = accessMap.allocation.uses.lazy.filter {
      $0.instruction is Deallocation || $0.instruction is DestroyValueInst
    }
    for dealloc in deallocations {
      let block = dealloc.instruction.parentBlock
      blockAccess[block, default: BlockInfo(effect: nil, hasDealloc: true)].hasDealloc = true
    }
  }
}

extension LocalVariableAccessBlockMap.BlockEffect {
  init(for accessInfo: LocalVariableAccessInfo, _ context: some Context) {
    // Assign from the lowest to the highest lattice values...
    self = .read
    if accessInfo.isModify {
      self = .modify
    }
    if accessInfo.isEscape {
      self = .escape
    }
    if accessInfo.isFullyAssigned(context) {
      self = .assign
    }
  }
}

/// Map an allocation (alloc_box, alloc_stack, @in, @inout) onto its reachable accesses.
class LocalVariableReachabilityCache {
  var cache = Dictionary<HashableValue, LocalVariableReachableAccess>()

  func reachability(for allocation: Value, _ context: some Context) -> LocalVariableReachableAccess? {
    if let reachability = cache[allocation.hashable] {
      return reachability
    }
    if let reachabilty = LocalVariableReachableAccess(allocation: allocation, context) {
      cache[allocation.hashable] = reachabilty
      return reachabilty
    }
    return nil
  }
}

/// Flow-sensitive, pessimistic data flow of local variable access. This finds all potentially reachable uses of an
/// assignment. This does not determine whether the assignment is available at each use; that would require optimistic,
/// iterative data flow. The only data flow state is pessimistic reachability, which is implicit in the block worklist.
struct LocalVariableReachableAccess {
  let context: Context
  let accessMap: LocalVariableAccessMap
  let blockMap: LocalVariableAccessBlockMap

  init?(allocation: Value, _ context: Context) {
    guard let accessMap = LocalVariableAccessMap(allocation: allocation, context) else {
      return nil
    }
    self.context = context
    self.accessMap = accessMap
    self.blockMap = LocalVariableAccessBlockMap(accessMap: accessMap)
  }
}

// Find reaching assignments...
extension LocalVariableReachableAccess {
  // Gather all fully assigned accesses that reach 'instruction'. If 'instruction' is itself a modify access, it is
  // ignored and the nearest assignments above 'instruction' are still gathered.
  func gatherReachingAssignments(for instruction: Instruction, in accessStack: inout Stack<LocalVariableAccess>)
    -> Bool {
    var blockList = BasicBlockWorklist(context)
    defer { blockList.deinitialize() }

    var initialEffect: BlockEffect? = nil
    if let prev = instruction.previous {
      initialEffect = backwardScanAccesses(before: prev, accessStack: &accessStack)
    }
    if !backwardPropagateEffect(in: instruction.parentBlock, effect: initialEffect, blockList: &blockList,
                                accessStack: &accessStack) {
      return false
    }
    while let block = blockList.pop() {
      let blockInfo = blockMap[block]
      var currentEffect = blockInfo?.effect
      // lattice: none -> read -> modify -> escape -> assign
      //
      // `blockInfo.effect` is the same as `currentEffect` returned by backwardScanAccesses, except when an early escape
      // happens below an assign, in which case we report the escape here.
      switch currentEffect {
      case .none, .read, .modify, .escape:
        break
      case .assign:
        currentEffect = backwardScanAccesses(before: block.instructions.reversed().first!, accessStack: &accessStack)
      }
      if !backwardPropagateEffect(in: block, effect: currentEffect, blockList: &blockList, accessStack: &accessStack) {
        return false
      }
    }
    // TODO: Verify that the accessStack.isEmpty condition never occurs.
    return !accessStack.isEmpty
  }

  private func backwardPropagateEffect(in block: BasicBlock, effect: BlockEffect?, blockList: inout BasicBlockWorklist,
                                       accessStack: inout Stack<LocalVariableAccess>)
    -> Bool {
    switch effect {
    case .none, .read, .modify:
      if block != accessMap.allocation.parentBlock {
        for predecessor in block.predecessors { blockList.pushIfNotVisited(predecessor) }
      } else if block == accessMap.function.entryBlock {
        accessStack.push(accessMap.liveInAccess!)
      }
    case .assign:
      break
    case .escape:
      return false
    }
    return true
  }

  // Check all instructions in this block before and including `first`. Return a BlockEffect indicating the combined
  // effects seen before stopping the scan. A .escape or .assign stops the scan.
  private func backwardScanAccesses(before first: Instruction, accessStack: inout Stack<LocalVariableAccess>)
    -> BlockEffect? {
    var currentEffect: BlockEffect?
    for inst in ReverseInstructionList(first: first) {
      guard let accessInfo = accessMap[inst] else {
        continue
      }
      currentEffect = BlockEffect(for: accessInfo, accessMap.context).meet(currentEffect)
      switch currentEffect! {
      case .read, .modify:
        continue
      case .assign:
        accessStack.push(accessInfo.access)
      case .escape:
        break
      }
      break
    }
    return currentEffect
  }
}

// Find reachable accesses...
extension LocalVariableReachableAccess {
  /// This performs a forward CFG walk to find known reachable uses from `assignment`. This ignores aliasing and
  /// escapes.
  ///
  /// The known live range is the range in which the assigned value is valid and may be used by dependent values. It
  /// includes the destroy or reassignment of the local.
  func gatherKnownLifetimeUses(from assignment: LocalVariableAccess,
                                in accessStack: inout Stack<LocalVariableAccess>) {
    if let modifyInst = assignment.instruction {
      _ = gatherReachableUses(after: modifyInst, in: &accessStack, lifetime: true)
      return
    }
    gatherKnownLifetimeUsesFromEntry(in: &accessStack)
  }

  /// This performs a forward CFG walk to find known reachable uses from the function entry. This ignores aliasing and
  /// escapes.
  private func gatherKnownLifetimeUsesFromEntry(in accessStack: inout Stack<LocalVariableAccess>) {
    assert(accessMap.liveInAccess!.kind == .incomingArgument, "only an argument access is live in to the function")
    let firstInst = accessMap.function.entryBlock.instructions.first!
    _ = gatherReachableUses(onOrAfter: firstInst, in: &accessStack, lifetime: true)
  }

  /// This performs a forward CFG walk to find all reachable uses of `modifyInst`. `modifyInst` may be a `begin_access
  /// [modify]` or instruction that initializes the local variable.
  ///
  /// This does not include the destroy or reassignment of the value set by `modifyInst`.
  ///
  /// Returns true if all possible reachable uses were visited. Returns false if any escapes may reach `modifyInst` are
  /// reachable from `modifyInst`.
  ///
  /// This does not gather the escaping accesses themselves. When escapes are reachable, it also does not guarantee that
  /// previously reachable accesses are gathered.
  ///
  /// This computes reachability separately for each store. If this store is a fully assigned access, then
  /// this never repeats work (it is a linear-time analysis over all assignments), because the walk always stops at the
  /// next fully-assigned access. Field assignment can result in an analysis that is quadratic in the number
  /// stores. Nonetheless, the analysis is highly efficient because it maintains no block state other than the
  /// block's intrusive bit set.
  func gatherAllReachableUses(of modifyInst: Instruction, in accessStack: inout Stack<LocalVariableAccess>) -> Bool {
    guard let accessInfo = accessMap[modifyInst] else {
      return false
    }
    if accessInfo.hasEscaped == nil {
      findAllEscapesPriorToAccess()
    }
    if accessInfo.hasEscaped! {
      return false
    }
    return gatherReachableUses(after: modifyInst, in: &accessStack, lifetime: false)
  }

  /// This performs a forward CFG walk to find all uses of this local variable reachable after `begin`.
  ///
  /// If `lifetime` is true, then this gathers the full known lifetime, including destroys and reassignments ignoring
  /// escapes.
  ///
  /// If `lifetime` is false, then this returns `false` if the walk ended early because of a reachable escape.
  private func gatherReachableUses(after begin: Instruction, in accessStack: inout Stack<LocalVariableAccess>,
                                   lifetime: Bool) -> Bool {
    if let term = begin as? TermInst {
      for succ in term.successors {
        if !gatherReachableUses(onOrAfter: succ.instructions.first!, in: &accessStack, lifetime: lifetime) {
          return false
        }
      }
      return true
    } else {
      return gatherReachableUses(onOrAfter: begin.next!, in: &accessStack, lifetime: lifetime)
    }
  }

  /// This performs a forward CFG walk to find all uses of this local variable reachable after and including `begin`.
  ///
  /// If `lifetime` is true, then this returns false if the walk ended early because of a reachable escape.
  private func gatherReachableUses(onOrAfter begin: Instruction, in accessStack: inout Stack<LocalVariableAccess>,
                                   lifetime: Bool) -> Bool {
    var blockList = BasicBlockWorklist(context)
    defer { blockList.deinitialize() }

    let initialBlock = begin.parentBlock
    let initialEffect = forwardScanAccesses(after: begin, accessStack: &accessStack, lifetime: lifetime)
    if !lifetime, initialEffect == .escape {
      return false
    }
    forwardPropagateEffect(in: initialBlock, blockInfo: blockMap[initialBlock], effect: initialEffect,
                           blockList: &blockList, accessStack: &accessStack)
    while let block = blockList.pop() {
      let blockInfo = blockMap[block]
      var currentEffect = blockInfo?.effect
      // lattice: none -> read -> modify -> escape -> assign
      //
      // `blockInfo.effect` is the same as `currentEffect` returned by forwardScanAccesses, except when an early
      // disallowed escape happens before an assign.
      switch currentEffect {
      case .none:
        break
      case .escape:
        if !lifetime {
          break
        }
        fallthrough
      case .read, .modify, .assign:
        let firstInst = block.instructions.first!
        currentEffect = forwardScanAccesses(after: firstInst, accessStack: &accessStack, lifetime: lifetime)
      }
      if !lifetime, currentEffect == .escape {
        return false
      }
      forwardPropagateEffect(in: block, blockInfo: blockInfo, effect: currentEffect, blockList: &blockList,
                             accessStack: &accessStack)
    }
    log("\n\(accessMap)")
    log(prefix: false, "Reachable access:\n\(accessStack.map({ String(describing: $0)}).joined(separator: "\n"))")
    return true
  }

  typealias BlockEffect = LocalVariableAccessBlockMap.BlockEffect
  typealias BlockInfo = LocalVariableAccessBlockMap.BlockInfo

  private func forwardPropagateEffect(in block: BasicBlock, blockInfo: BlockInfo?, effect: BlockEffect?,
                                      blockList: inout BasicBlockWorklist,
                                      accessStack: inout Stack<LocalVariableAccess>) {
    switch effect {
    case .none, .read, .modify, .escape:
      if let blockInfo, blockInfo.hasDealloc {
        break
      }
      if block.terminator.isFunctionExiting {
        accessStack.push(LocalVariableAccess(.outgoingArgument, block.terminator))
      } else {
        for successor in block.successors { blockList.pushIfNotVisited(successor) }
      }
    case .assign:
      break
    }
  }

  // Check all instructions in this block after and including `begin`. Return a BlockEffect indicating the combined
  // effects seen before stopping the scan. An .assign stops the scan. A .escape stops the scan if lifetime is false.
  private func forwardScanAccesses(after first: Instruction, accessStack: inout Stack<LocalVariableAccess>,
                                   lifetime: Bool)
    -> BlockEffect? {
    var currentEffect: BlockEffect?
    for inst in InstructionList(first: first) {
      guard let accessInfo = accessMap[inst] else {
        continue
      }
      currentEffect = BlockEffect(for: accessInfo, accessMap.context).meet(currentEffect)
      switch currentEffect! {
      case .assign:
        if lifetime {
          accessStack.push(accessInfo.access)
        }
        return currentEffect
      case .escape:
        if !lifetime {
          log("Local variable: \(accessMap.allocation)\n    escapes at \(inst)")
          return currentEffect
        }
        fallthrough
      case .read, .modify:
        accessStack.push(accessInfo.access)
      }
    }
    return currentEffect
  }
}

// Find prior escapes...
extension LocalVariableReachableAccess {
  /// For alloc_box only, find escapes (captures) of the box prior to each access.
  /// As a result, AccessInfo.hasEscaped will be non-nil for every access.
  ///
  /// This is an optimistic forward dataflow that propagates the escape bit to accesses.
  /// A block can be scanned at most twice. Once after it is marked visited to find any escapes within the block. The
  /// second time after it is marked escaped to propagate the hasEscaped bit to accesses within the block.
  private func findAllEscapesPriorToAccess() {
    var visitedBlocks = BasicBlockSet(context)
    var escapedBlocks = BasicBlockSet(context)
    var blockList = Stack<BasicBlock>(context)
    defer {
      visitedBlocks.deinitialize()
      escapedBlocks.deinitialize()
      blockList.deinitialize()
    }
    let forwardPropagate = { (from: BasicBlock, hasEscaped: Bool) in
      if let blockInfo = blockMap[from], blockInfo.hasDealloc {
        return
      }
      for successor in from.successors {
        if hasEscaped {
          if escapedBlocks.insert(successor) {
            blockList.push(successor)
          }
        } else if visitedBlocks.insert(successor) {
          blockList.push(successor)
        }
      }
    }
    var hasEscaped = propagateEscapeInBlock(after: accessMap.allocation.nextInstruction, hasEscaped: false)
    forwardPropagate(accessMap.allocation.parentBlock, hasEscaped)
    while let block = blockList.pop() {
      hasEscaped = escapedBlocks.contains(block)
      hasEscaped = propagateEscapeInBlock(after: block.instructions.first!, hasEscaped: hasEscaped)
      forwardPropagate(block, hasEscaped)
    }
  }

  private func propagateEscapeInBlock(after begin: Instruction, hasEscaped: Bool) -> Bool {
    var hasEscaped = hasEscaped
    for inst in InstructionList(first: begin) {
      guard let accessInfo = accessMap[inst] else {
        continue
      }
      if accessInfo.isEscape {
        hasEscaped = true
      } else {
        accessInfo.hasEscaped = hasEscaped
      }
    }
    return hasEscaped
  }
}

let localVariableReachingAssignmentsTest = FunctionTest("local_variable_reaching_assignments") {
  function, arguments, context in
  let allocation = arguments.takeValue()
  let instruction = arguments.takeInstruction()
  print("### Allocation: \(allocation)")
  let localReachabilityCache = LocalVariableReachabilityCache()
  guard let localReachability = localReachabilityCache.reachability(for: allocation, context) else {
    print("No reachability")
    return
  }
  print("### Access map:")
  print(localReachability.accessMap)
  print("### Instruction: \(instruction)")
  var reachingAssignments = Stack<LocalVariableAccess>(context)
  defer { reachingAssignments.deinitialize() }
  guard localReachability.gatherReachingAssignments(for: instruction, in: &reachingAssignments) else {
    print("!!! Reaching escape")
    return
  }
  print("### Reachable assignments:")
  print(reachingAssignments.map({ String(describing: $0)}).joined(separator: "\n"))
}

let localVariableReachableUsesTest = FunctionTest("local_variable_reachable_uses") {
  function, arguments, context in
  let allocation = arguments.takeValue()
  let modify = arguments.takeInstruction()
  print("### Allocation: \(allocation)")
  let localReachabilityCache = LocalVariableReachabilityCache()
  guard let localReachability = localReachabilityCache.reachability(for: allocation, context) else {
    print("No reachability")
    return
  }
  print("### Access map:")
  print(localReachability.accessMap)
  print("### Modify: \(modify)")
  var reachableUses = Stack<LocalVariableAccess>(context)
  defer { reachableUses.deinitialize() }
  guard localReachability.gatherAllReachableUses(of: modify, in: &reachableUses) else {
    print("!!! Reachable escape")
    return
  }
  print("### Reachable access:")
  print(reachableUses.map({ String(describing: $0)}).joined(separator: "\n"))
}
