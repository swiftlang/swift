//===--- RedundantLoadElimination.swift ------------------------------------==//
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

import AST
import SIL

/// Replaces redundant `load` or load-like instructions with already available values.
///
/// A load is redundant if the loaded value is already available at that point.
/// This can be via a preceding store to the same address:
///
///     store %1 to %addr
///     ...               // no writes to %addr
///     %2 = load %addr
/// ->
///     store %1 to %addr
///     ...               // no writes to %addr
///     // replace uses of %2 with the available value %1
///
/// or a preceding load from the same address:
///
///     %1 = load %addr
///     ...               // no writes to %addr
///     %2 = load %addr
/// ->
///     %1 = load %addr
///     ...               // no writes to %addr
///     // replace uses of %2 with the available value %1
///
/// In case of a partial redundant load, the load is split so that some of the new
/// individual loads can be eliminated in the next round of the optimization:
///
///     %fa1 = struct_element_addr %addr, #field1
///     store %1 to %fa1
///     ...               // no writes to %fa1
///     %2 = load %addr   // partially redundant
/// ->
///     %fa1 = struct_extract %addr, #field1
///     store %1 to %fa1
///     ...               // no writes to %fa1
///     %fa1 = struct_element_addr %addr, #field1
///     %f1 = load %fa1                              // this load is redundant now
///     %fa2 = struct_element_addr %addr, #field2
///     %f2 = load %fa2
///     %2 = struct (%f1, %f2)
///
/// This works in a similar fashion for other load-like instructions, like `copy_addr`,
/// `destroy_addr` or `load_borrow`.
///
/// `load_borrow`s are handled in two different ways:
///
/// 1. If a `load_borrow` - including its whole borrow scope - is located within the liverange
///    of a redundant `load [copy]` or `load [take]`, the `load_borrow` is replaced completely.
///    The available value is live throughout the borrow scope anyway, so it can be borrowed
///    instead of loading the value again from memory:
///
///     store %1 to [init] %addr
///     %2 = load_borrow %addr
///     ...                       // uses of %2
///     end_borrow %2
///     %3 = load [take] %addr
/// ->
///     %4 = begin_borrow %1
///     ...                       // replace uses of %2 with %4
///     end_borrow %4
///     // replace uses of %3 with %1
///
/// 2. Otherwise, a redundant `load_borrow` is not removed, because the borrowed value would
///    need to be copied. Instead, the available value is attached as "value hint" to the
///    `load_borrow`. The value hint has "none" ownership and therefore doesn't affect the
///    lifetime of the available value. It just tells other optimizations which value is
///    loaded by the `load_borrow`:
///
///     store %1 to [init] %addr
///     ...
///     %2 = load_borrow %addr
/// ->
///     %3 = unchecked_ownership %1, forwarding: @none
///     store %1 to [init] %addr
///     ...
///     %2 = load_borrow %addr, value_hint %3
///
/// The algorithm is a data flow analysis which starts at the original load and searches
/// for preceding stores or loads by following the control flow in backward direction.
/// The preceding stores and loads provide the "available values" with which the original
/// load can be replaced.
///
/// Redundant loads are replaced in a way that no additional copies of the loaded value are
/// introduced. If this is not possible, the redundant load is not replaced.
///
let redundantLoadElimination = FunctionPass(name: "redundant-load-elimination") {
    (function: Function, context: FunctionPassContext) in
  _ = eliminateRedundantLoads(in: function, variant: .regular, context)
}

// Early RLE does not touch loads from Arrays. This is important because later array optimizations,
// like ABCOpt, get confused if an array load in a loop is converted to a pattern with a phi argument.
let earlyRedundantLoadElimination = FunctionPass(name: "early-redundant-load-elimination") {
    (function: Function, context: FunctionPassContext) in
  _ = eliminateRedundantLoads(in: function, variant: .early, context)
}

let mandatoryRedundantLoadElimination = FunctionPass(name: "mandatory-redundant-load-elimination") {
    (function: Function, context: FunctionPassContext) in
  _ = eliminateRedundantLoads(in: function, variant: .mandatory, context)
}

enum RedundantLoadEliminationVariant {
  case mandatory, mandatoryInGlobalInit, early, regular
}

func eliminateRedundantLoads(in function: Function,
                             variant: RedundantLoadEliminationVariant,
                             _ context: FunctionPassContext) -> Bool
{
  // FIXME: this skip is a hack for ManualOwnership prototyping, to workaround rdar://161359163
  if function.performanceConstraints == .manualOwnership && variant == .mandatory {
    return false
  }

  // Avoid quadratic complexity by limiting the number of visited instructions.
  // This limit is sufficient for most "real-world" functions, by far.
  var complexityBudget = 50_000
  var changed = false

  for block in function.blocks.reversed() {

    // We cannot use for-in iteration here because if the load is split, the new
    // individual loads are inserted right before and they would be ignored by a for-in iteration.
    // Also the `load` might be moved to another location instead of being deleted.
    var iter = block.instructions.reversed().first
    while let succ = iter, let inst = succ.previous {

      if let load = inst.asLoadingInstruction {
        if !context.continueWithNextSubpassRun(for: load) {
          return changed
        }
        // Even if the budget is exhausted, give each load a small budget, so that simple cases,
        // e.g. a load which is directly preceded by a store, are still optimized.
        if complexityBudget < 20 {
          complexityBudget = 20
        }
        if load.isEligibleForElimination(in: variant, context) {
          if tryEliminate(load: load, complexityBudget: &complexityBudget, context) {
            changed = true
            // The `load` has been deleted: do not advance `iter`, because the next instruction to process is
            // now its new predecessor.
            continue
          }
        }
      }
      iter = succ.previous
    }
  }
  return changed
}

private extension Instruction {
  // TODO: replace this with `self as? LoadingInstruction` once the toolchain builders are upgraded to a compiler
  // version with fast type casting (https://github.com/swiftlang/swift/pull/88270). Until then, casting to the
  // concrete classes avoids a slow dynamic cast to the protocol.
  var asLoadingInstruction: LoadingInstruction? {
    switch self {
      case let load as LoadInst:             return load
      case let copy as CopyAddrInst:         return copy
      case let destroy as DestroyAddrInst:   return destroy
      case let loadBorrow as LoadBorrowInst: return loadBorrow
      default:                               return nil
    }
  }
}

/// Extends `LoadInst.LoadOwnership` by `borrow`.
private enum LoadKind {
  case unqualified, trivial, copy, take, borrow

  init(from loadOwnership: LoadInst.LoadOwnership) {
    switch loadOwnership {
      case .unqualified: self = .unqualified
      case .trivial:     self = .trivial
      case .copy:        self = .copy
      case .take:        self = .take
    }
  }
}

/// Either a `load` or load-like instruction (e.g. `copy_addr`, `destroy_addr`, `load_borrow`)
private protocol LoadingInstruction: Instruction {
  /// The address from which the value is loaded.
  var address: Value { get }

  /// The type of the loaded value.
  var type: Type { get }

  /// The ownership of the value which replaces the load, i.e. the ownership of the values
  /// which are merged by the SSAUpdater.
  var ownershipForSSAUpdater: Ownership { get }

  var kind: LoadKind { get }

  /// False if the instruction cannot be optimized, regardless of the available values.
  var canOptimize: Bool { get }

  /// Splits the instruction into its struct or tuple elements, so that the individual parts
  /// can be optimized separately. Returns false if the instruction cannot be split.
  func trySplit(_ context: FunctionPassContext) -> Bool

  /// Replaces the loaded value with `newValue`.
  /// For instructions which are not plain loads, e.g. `copy_addr` or `destroy_addr`, this
  /// rewrites the instruction accordingly. A `load_borrow` is not replaced, but `newValue` is
  /// set as its value hint.
  func replaceLoad(with newValue: Value, _ context: FunctionPassContext)
}

extension LoadInst : LoadingInstruction {
  var canOptimize: Bool { true }
  fileprivate var kind: LoadKind { LoadKind(from: loadOwnership) }
  fileprivate var ownershipForSSAUpdater: Ownership { ownership }

  func replaceLoad(with newValue: Value, _ context: FunctionPassContext) {
    // Make sure to keep dependencies valid after replacing the load
    insertMarkDependencies(for: self, context)
    self.replaceEfficiently(with: newValue, context)
  }

  func replaceEfficiently(with newValue: Value, _ context: FunctionPassContext) {
    if let existingLoad = newValue as? LoadInst {
      // As we are processing the loads in reverse control flow order, the replaced loads might accumulate
      // quite a lot of users. This happens if the are many loads from the same location in a row.
      // To avoid quadratic complexity in `uses.replaceAll`, we swap both load instructions and move the uses
      // from the `existingLoad` (which usually has a small number of uses) to this load - and delete the
      // `existingLoad`.
      existingLoad.uses.replaceAll(with: self, context)
      self.addressOperand.set(to: existingLoad.address, context)
      self.set(ownership: existingLoad.loadOwnership, context)
      self.set(location: existingLoad.location, context)
      self.move(before: existingLoad, context)
      context.erase(instruction: existingLoad)
    } else {
      replace(with: newValue, context)
    }
  }
}

extension CopyAddrInst : LoadingInstruction {
  var address: Value { source }
  var type: Type { address.type.objectType }
  fileprivate var kind: LoadKind { LoadKind(from: loadOwnership) }

  fileprivate var ownershipForSSAUpdater: Ownership {
    if !parentFunction.hasOwnership || type.isTrivial(in: parentFunction) {
      return .none
    }
    // Regardless of if the copy is taking or copying, the loaded value is an owned value.
    return .owned
  }

  var canOptimize: Bool {
    if !source.type.isLoadable(in: parentFunction) {
      // Although the original load's type is loadable (obviously), it can be projected-out
      // from the copy_addr's type which might be not loadable.
      return false
    }
    if !parentFunction.hasOwnership {
      if !isTakeOfSource || !isInitializationOfDestination {
        // For simplicity, bail if we would have to insert compensating copies or destroys.
        return false
      }
    }
    return true
  }

  func replaceLoad(with newValue: Value, _ context: FunctionPassContext) {
    let load = replaceWithLoadAndStore(context).load
    insertMarkDependencies(for: load, context)
    load.replaceEfficiently(with: newValue, context)
  }
}

// `destroy_addr` is equivalent to `load [take]` + `destroy_value`
extension DestroyAddrInst : LoadingInstruction {
  var address: Value { destroyedAddress }
  var type: Type { address.type.objectType }
  fileprivate var kind: LoadKind { .take }

  fileprivate var ownershipForSSAUpdater: Ownership {
    if !parentFunction.hasOwnership {
      return .none
    }
    return .owned
  }

  var canOptimize: Bool {
    parentFunction.hasOwnership &&
      destroyedAddress.type.isLoadable(in: parentFunction) && !type.isTrivial(in: parentFunction)
  }

  func replaceLoad(with newValue: Value, _ context: FunctionPassContext) {
    let builder = Builder(before: self, context)
    let load = builder.createLoad(fromAddress: destroyedAddress, ownership: .take)
    builder.createDestroyValue(operand: load)
    context.erase(instruction: self)
    insertMarkDependencies(for: load, context)
    load.replaceEfficiently(with: newValue, context)
  }
}

/// A redundant `load_borrow` is not removed, but the available value is set as its value hint.
/// Note that `load_borrow`s which are fully contained in the liverange of a redundant
/// `load [copy]` or `load [take]` are removed when that load is optimized (see `containedLoadBorrows`).
extension LoadBorrowInst : LoadingInstruction {
  var canOptimize: Bool { valueHint == nil }
  fileprivate var kind: LoadKind { .borrow }
  func trySplit(_ context: FunctionPassContext) -> Bool { false }

  // The value hint has "none" ownership. Therefore the available values don't need to be copied.
  fileprivate var ownershipForSSAUpdater: Ownership { .none }

  func replaceLoad(with newValue: Value, _ context: FunctionPassContext) {
    set(valueHint: newValue, context)
  }
}

private func tryEliminate(load: LoadingInstruction, complexityBudget: inout Int, _ context: FunctionPassContext) -> Bool {
  switch load.isRedundant(complexityBudget: &complexityBudget, context) {
  case .notRedundant:
    return false
  case .redundant(let availableValues, let containedLoadBorrows, let exitBlocks):
    replace(load: load, with: availableValues, containedLoadBorrows: containedLoadBorrows, exitBlocks: exitBlocks,
            context)
    return true
  case .maybePartiallyRedundant(let subPath):
    // Check if the a partial load would really be redundant to avoid unnecessary splitting.
    switch load.isRedundant(at: subPath, complexityBudget: &complexityBudget, context) {
      case .notRedundant, .maybePartiallyRedundant:
        return false
      case .redundant:
        // The new individual loads are inserted right before the current load and
        // will be optimized in the following loop iterations.
        return load.trySplit(context)
    }
  }
}

private extension LoadingInstruction {

  func isEligibleForElimination(in variant: RedundantLoadEliminationVariant, _ context: FunctionPassContext) -> Bool {
    if !canOptimize {
      return false
    }
    switch variant {
    case .mandatory, .mandatoryInGlobalInit:
      if kind == .take {
        // load [take] would require to shrinkMemoryLifetime. But we don't want to do this in the mandatory
        // pipeline to not shrink or remove an alloc_stack which is relevant for debug info.
        return false
      }
      switch address.accessBase {
      case .box, .stack, .global:
        break
      default:
        return false
      }
    case .early:
      // See the comment of `earlyRedundantLoadElimination`.
      if let nominal = self.type.nominal, nominal == context.swiftArrayDecl {
        return false
      }
    case .regular:
      break
    }
    // Check if the type can be expanded without a significant increase to code size.
    // We block redundant load elimination because it might increase register pressure for large values.
    // Furthermore, this pass also splits values into its projections (e.g shrinkMemoryLifetimeAndSplit).
    // But: it is required to remove loads, even of large structs, in global init functions to ensure
    // that globals (containing large structs) can be statically initialized.
    if variant != .mandatoryInGlobalInit, !self.type.shouldExpand(context) {
       return false
    }
    return true
  }

  func isRedundant(complexityBudget: inout Int, _ context: FunctionPassContext) -> DataflowResult {
    return isRedundant(at: address.constantAccessPath, complexityBudget: &complexityBudget, context)
  }

  func isRedundant(at accessPath: AccessPath, complexityBudget: inout Int, _ context: FunctionPassContext) -> DataflowResult {
    var liverange = Liverange(load: self, accessPath: accessPath, context)
    defer { liverange.deinitialize() }

    guard liverange.performDataflow(complexityBudget: &complexityBudget) else {
      return DataflowResult(notRedundantWith: liverange.potentiallyRedundantSubpath)
    }
    switch self.kind {
    case .trivial, .unqualified, .borrow:
      return .redundant(liverange.availableValues, containedLoadBorrows: [], exitBlocks: [])
    case .copy, .take:
      let exitBlocks = liverange.exits
      guard canHandle(exits: exitBlocks, of: liverange, context),
            let containedLoadBorrows = getContainedLoadBorrows(of: liverange),
            canMaterializeAddressAtLifetimeBorders(of: liverange, context),
            kind != .take || canShrinkMemoryLifetime(to: liverange, at: accessPath)
      else {
        return DataflowResult(notRedundantWith: liverange.potentiallyRedundantSubpath)
      }
      return .redundant(liverange.availableValues, containedLoadBorrows: containedLoadBorrows, exitBlocks: exitBlocks)
    }
  }

  /// Checks if the liverange of the value has "exits", i.e. paths which don't lead to the load.
  /// It means that we would have to insert a destroy on that exit to satisfy ownership rules.
  /// But an inserted destroy also means that we would need to insert copies of the value which
  /// were not there originally. For example:
  ///
  ///     store %1 to [init] %addr
  ///     cond_br bb1, bb2
  ///   bb1:
  ///     %2 = load [take] %addr
  ///   bb2:                      // liverange exit
  ///
  /// Only handle two special cases:
  /// * if the exit is a dead-end block (with an `unreachable`) we are okay, because there we can
  ///   insert a `destroy_value [dead_end]`.
  /// * in case of a `take`, we can store the value back to memory in the exit block. This requires
  ///   that the address of the access can be re-materialized in the exit block.
  ///
  private func canHandle(exits exitBlocks: [BasicBlock], of liverange: Liverange,
                         _ context: FunctionPassContext) -> Bool
  {
    if exitBlocks.isEmpty {
      return true
    }
    if liverange.foundLoop {
      return false
    }
    for exitBlock in exitBlocks {
      if exitBlock.isSafeDeadEndBlock(for: self.address, context) {
        continue
      }
      let storeBackPoint = exitBlock.instructions.first!
      if self.kind == .copy ||
         !canMaterializeAddress(at: storeBackPoint, context) ||
         // Storing back the value is a write to the memory.
         liverange.isWithinReadOnlyAccessScope(insertionPoint: storeBackPoint)
      {
        return false
      }
    }
    return true
  }

  /// Returns the `load_borrow`s whose borrow scopes are fully contained in the liverange. Those can
  /// be replaced by a borrow of the available value.
  /// A `load_borrow` which is not fully contained doesn't prevent optimizing a `load [copy]`:
  /// it just keeps on reading the memory. But it prevents optimizing a `load [take]`, because
  /// the memory is not initialized anymore within the liverange. In this case nil is returned.
  private func getContainedLoadBorrows(of liverange: Liverange) -> [LoadBorrowInst]? {
    var containedLoadBorrows = [LoadBorrowInst]()
    for loadBorrow in liverange.loadBorrows {
      if liverange.isFullyContained(scopeOf: loadBorrow) {
        containedLoadBorrows.append(loadBorrow)
      } else if self.kind == .take {
        return nil
      }
    }
    return containedLoadBorrows
  }

  /// Returns true if the address of the load can be re-created at all lifetime borders, where
  /// new loads are inserted.
  private func canMaterializeAddressAtLifetimeBorders(of liverange: Liverange,
                                                      _ context: FunctionPassContext) -> Bool
  {
    for case .lifetimeBorder(let borderInst) in liverange.availableValues {
      if !canMaterializeAddress(at: borderInst, context) {
        return false
      }
    }
    return true
  }

  /// In case of a `load [take]` the memory lifetime is shrunk up to the available values. This means
  /// that the memory is mutated at the position of an available value. Therefore all available values
  /// must be located in a memory region which allows mutation.
  private func canShrinkMemoryLifetime(to liverange: Liverange, at accessPath: AccessPath) -> Bool {
    for availableValue in liverange.availableValues {
      switch availableValue {
      case .viaLoad(let availableLoad):
        if availableLoad.address.isInImmutableAccessScope ||
           // The address of the available load is not necessarily derived from the access scope, e.g.
           //
           //   %1 = begin_access [read] %0
           //   %2 = load [copy] %0         // the available load
           //   end_access %1
           //
           liverange.isWithinReadOnlyAccessScope(insertionPoint: availableLoad)
        {
          return false
        }
      case .lifetimeBorder(let borderInst):
        // A `load [take]` is inserted at the lifetime border. The border can be located within a
        // read-only access scope, e.g. if the liverange is killed by a read in such a scope:
        //
        //   %1 = begin_access [read] %0
        //   %2 = struct_element_addr %1, #S.field
        //   %3 = load [copy] %2         // killing instruction
        //   end_access %1              // border: a `load [take] %0` must not be inserted here
        //
        if liverange.isWithinReadOnlyAccessScope(insertionPoint: borderInst) {
          return false
        }
      case .viaStore, .viaCopyAddr, .viaLoadBorrow:
        break
      }
      // Shrinking the memory lifetime splits the available loads and stores along the projection
      // path to the loaded memory location. This is not possible, e.g. if a struct on the path has
      // unreferenceable storage.
      if !availableValue.canSplit(alongPathTo: accessPath) {
        return false
      }
    }
    return true
  }

  /// The address from which this load's address can be re-created at another place in the function,
  /// together with the projection path which leads from that address to the load's address.
  ///
  /// If the load's address is enclosed in an access scope, the `begin_access` is used - and _not_
  /// the access base (e.g. an `alloc_stack`). Otherwise the re-created address would bypass the
  /// access scope. Several optimizations rely on the fact that all accesses to a memory location
  /// within an access scope go through the `begin_access`. For example, DestroyAddrHoisting treats
  /// such a memory location as `AccessStorage::Nested` storage and therefore only looks at uses of
  /// the `begin_access`.
  var materializationBase: (address: Value, path: SmallProjectionPath)? {
    let (accessPath, scope) = address.constantAccessPathWithScope
    guard let path = accessPath.materializableProjectionPath,
          let baseAddress = (scope as Value?) ?? accessPath.base.address
    else {
      return nil
    }
    return (baseAddress, path)
  }

  /// True, if the address of this load can be re-created at `instruction`.
  func canMaterializeAddress(at instruction: Instruction, _ context: FunctionPassContext) -> Bool {
    guard let (baseAddress, _) = materializationBase else {
      return false
    }
    // The base address must be available at `instruction`.
    if !baseAddress.strictlyDominates(instruction: instruction, context.dominatorTree) {
      return false
    }
    if let beginAccess = baseAddress as? BeginAccessInst {
      // The re-created address must not be used outside of its access scope.
      // Note that the address is re-created _before_ `instruction`. Therefore an `end_access`
      // itself is still a valid insertion point - hence `inclusiveRangeContains`.
      var accessScope = InstructionRange(begin: beginAccess, ends: beginAccess.endAccessInstructions, context)
      defer { accessScope.deinitialize() }
      if !accessScope.inclusiveRangeContains(instruction) {
        return false
      }
    } else if let reference = baseAddress.accessBase.reference {
      switch reference.ownership {
      case .none:
        return true
      case .owned, .unowned:
        // Can happen with project_box -> not supported yet
        return false
      case .guaranteed:
        // The base address is derived from a reference which is only valid within its ownership scope:
        //
        //   %ref = begin_borrow %1
        //   %base = ref_element_addr %ref
        //   end_borrow %ref                 <- %base must not be re-created after this point
        //
        // Dominance of `baseAddress` is not sufficient here: if the load is in a loop, the
        // materialization point can be located after the end of the scope.
        for borrowIntroducer in reference.getBorrowIntroducers(context) {
          guard borrowIntroducer.hasLocalScope else {
            // A borrow without a local scope, e.g. a guaranteed function argument, is valid
            // throughout the whole function.
            continue
          }
          let v = borrowIntroducer.value
          var scope = InstructionRange(for: v, context)
          defer { scope.deinitialize() }
          scope.insert(contentsOf: borrowIntroducer.scopeEndingOperands.users)
          guard scope.contains(instruction) else {
            return false
          }
        }
      }
    }
    return true
  }

  /// Re-creates the address of this load at the builder's insertion point.
  /// This is only legal if `canMaterializeAddress` returned true for that location.
  func materializeAddress(_ builder: Builder) -> Value {
    let (baseAddress, path) = materializationBase!
    return baseAddress.createAddressProjection(path: path, builder: builder)
  }
}

private extension Value {
  /// True if this address is enclosed in an access scope which forbids mutating the memory.
  var isInImmutableAccessScope: Bool {
    return accessBaseWithScopes.scopes.contains {
      guard case .access(let beginAccess) = $0 else {
        // Non-access scopes, e.g. a `mark_dependence`, don't restrict mutation.
        return false
      }
      return beginAccess.isReadOnly
    }
  }
}

private extension BasicBlock {
  /// Returns true if this block is a dead-end block which ends in `unreachable` and does not
  /// read from `address`.
  func isSafeDeadEndBlock(for address: Value, _ context: FunctionPassContext) -> Bool {
    guard terminator is UnreachableInst else {
      return false
    }
    let aliasAnalysis = context.aliasAnalysis
    return instructions.allSatisfy { !$0.mayRead(fromAddress: address, aliasAnalysis) }
  }
}

private func replace(load: LoadingInstruction,
                     with availableValues: [AvailableValue],
                     containedLoadBorrows: [LoadBorrowInst],
                     exitBlocks: [BasicBlock],
                     _ context: FunctionPassContext)
{
  // Note: an `InstructionBasedSSAUpdater` is needed (instead of a plain `SSAUpdater`), because an
  // available value can be located _after_ a use for which we need to construct the value. E.g. the
  // available value of the `load_borrow`'s block is located after the `load_borrow` here:
  //
  //   bb1:
  //     %2 = load_borrow %addr    // must read the value which comes in via the back edge
  //     ...
  //     %3 = load [take] %addr    // The load
  //     store %4 to [init] %addr  // an available value
  //     br bb1
  //
  var ssaUpdater = InstructionBasedSSAUpdater(type: load.type, ownership: load.ownershipForSSAUpdater,
                                              context)
  defer { ssaUpdater.deinitialize() }

  for availableValue in availableValues.replaceCopyAddrsWithLoadsAndStores(context) {
    // The value becomes available at the position of the available load/store. As `provideValue` can
    // delete that instruction, anchor the position at its predecessor instruction.
    let block = availableValue.instruction.parentBlock
    var predecessorInst = availableValue.instruction.previous
    // Splitting a store can also delete the `struct` and `tuple` instructions which define the stored value.
    // Deleting such a `struct` or `tuple` also deletes its `debug_value` users (salvaging debug info
    // replaces them with new `debug_value`s of the elements).
    while let inst = predecessorInst, inst is StructInst || inst is TupleInst || inst is DebugValueInst {
      predecessorInst = inst.previous
    }
    let value = provideValue(for: load, from: availableValue, context)
    if let predecessorInst {
      ssaUpdater.addAvailableValue(value, after: predecessorInst)
    } else {
      ssaUpdater.addAvailableValue(value, beforeFirstInstructionOf: block)
    }
  }
  let loadAccessPath = load.address.constantAccessPath

  for exitBlock in exitBlocks {
    let builder = Builder(atBeginOf: exitBlock, context)
    // The store is inserted before all existing instructions of the exit block. Therefore it needs
    // the value which is live on entry of the block.
    let valueInExitBlock = ssaUpdater.getValue(atBeginOf: exitBlock)
    if exitBlock.isSafeDeadEndBlock(for: load.address, context) {
      builder.createDestroyValue(operand: valueInExitBlock, isDeadEnd: true)
    } else {
      // The memory is not initialized on this path anymore (because the load was turned into a
      // `take` of the available value). Store the value back to re-initialize the memory.
      // Note that the address of the load itself is not available in the exit block. Therefore
      // re-create the address projections from the enclosing access of the load.
      let addr = load.materializeAddress(builder)
      builder.createStore(source: valueInExitBlock, destination: addr, ownership: .initialize)
    }
  }

  // Replace the `load_borrow`s, which are fully contained in the liverange, with a borrow scope
  // of the available value.
  for loadBorrow in containedLoadBorrows {
    let builder = Builder(before: loadBorrow, context)
    let beginBorrow = builder.createBeginBorrow(of: ssaUpdater.getValue(before: loadBorrow))
    let path = loadAccessPath.getMaterializableProjection(to: loadBorrow.address.constantAccessPath)!
    let value = beginBorrow.createProjection(path: path, builder: builder)
    // The scope ending instructions must end the borrow of the `begin_borrow`. If the borrowed value
    // is projected, the projection is not a borrow introducer and therefore cannot be end-borrowed.
    loadBorrow.uses.endingLifetime.replaceAll(with: beginBorrow, context)
    loadBorrow.replace(with: value, context)
  }

  load.replaceLoad(with: ssaUpdater.getValue(before: load), context)
}

private func provideValue(
  for load: LoadingInstruction,
  from availableValue: AvailableValue,
  _ context: FunctionPassContext
) -> Value {
  let projectionPath = availableValue.getProjectionPath(to: load.address)

  let builder = availableValue.getBuilderForProjections(context)
  let value: Value

  switch load.kind {
  case .unqualified:
    value = availableValue.value.createProjection(path: projectionPath, builder: builder)
  case .copy, .trivial:
    if case .lifetimeBorder(let atInstruction) = availableValue {
      assert(projectionPath.isEmpty)
      let builder = Builder(before: atInstruction, context)
      let addr = load.materializeAddress(builder)
      return builder.createLoad(fromAddress: addr, ownership: .copy)
    } else {
      // Note: even if the load is trivial, the available value may be projected out of a non-trivial value.
      value = availableValue.value.createProjectionAndCopy(path: projectionPath, builder: builder)
    }
  case .take:
    return shrinkMemoryLifetime(to: availableValue, projectionPath: projectionPath, load: load, context)
  case .borrow:
    // The value is used as value hint of the `load_borrow`, which must have "none" ownership.
    // This doesn't affect the lifetime of the available value and therefore no copy is needed.
    let conversion = builder.createUncheckedOwnership(operand: availableValue.value, forwardingOwnership: .none)
    value = conversion.createProjection(path: projectionPath, builder: builder)
  }
  if value.type != load.type {
    // TODO: should not be needed as load-simplification can do this.
    // Just add a check for the correct type
    assert(value.type.isClass && load.type.isClass, "unexpected type mismatch")
    return builder.createUncheckedRefCast(from: value, to: load.type)
  }
  return value
}

/// In case of a `load [take]` shrink lifetime of the value in memory back to the `availableValue`
/// and return the (possibly projected) available value. For example:
///
///     store %1 to [assign] %addr
///     ...
///     %2 = load [take] %addr
/// ->
///     destroy_addr %addr
///     ...
///     // replace %2 with %1
///
/// or
///
///     %1 = load [copy] %addr
///     ...
///     %2 = load [take] %addr
/// ->
///     %1 = load [take] %addr
///     %3 = copy_value %1
///     ...
///     // replace %2 with %3
///
/// If the available value is a lifetime border, a `load [take]` is inserted at the border.
///
private func shrinkMemoryLifetime(to availableValue: AvailableValue,
                                  projectionPath: SmallProjectionPath,
                                  load: LoadingInstruction,
                                  _ context: FunctionPassContext
) -> Value {
  switch availableValue {
  case .viaLoad(let availableLoad):
    assert(availableLoad.loadOwnership == .copy)
    // The available `load [copy]` becomes a `load [take]`. Its original uses keep using it and
    // the replaced load gets a copy. This doesn't introduce an additional copy, because the
    // `load [copy]` was a copy already.
    let projectedLoad = availableLoad.split(alongPath: projectionPath, context)
    projectedLoad.set(ownership: .take, context)
    let builder = Builder(after: projectedLoad, context)
    return builder.createCopyValue(operand: projectedLoad)
  case .viaStore(let availableStore):
    let builder = Builder(before: availableStore, context)
    if availableStore.storeOwnership == .assign {
      builder.createDestroyAddr(address: availableStore.destination)
      availableStore.set(ownership: .initialize, context)
    }
    let projectedStore = availableStore.split(alongPath: projectionPath, useConstructureOperands: true, context)
    let valueToAdd = projectedStore.source
    context.erase(instruction: projectedStore)
    return valueToAdd
  case .lifetimeBorder(let atInstruction):
    assert(projectionPath.isEmpty)
    let builder = Builder(before: atInstruction, context)
    let addr = load.materializeAddress(builder)
    return builder.createLoad(fromAddress: addr, ownership: .take)
  case .viaCopyAddr:
    fatalError("copy_addr must be lowered before shrinking lifetime")
  case .viaLoadBorrow:
    fatalError("load_borrow cannot be an available value for a load [take]")
  }
}

/// The result of the data flow analysis for a load.
private enum DataflowResult {
  /// The load is not redundant.
  case notRedundant

  /// The load is redundant and can be replaced by the `AvailableValue`s.
  /// `containedLoadBorrows` are `load_borrow`s within the liverange which can be replaced by a
  /// borrow of the available value. `exitBlocks` are the liverange exits where the value must be
  /// destroyed (in dead-end blocks) or stored back to memory (in case of a `load [take]`).
  case redundant([AvailableValue], containedLoadBorrows: [LoadBorrowInst], exitBlocks: [BasicBlock])

  /// The load is not redundant, but a part of the loaded memory - with the associated access
  /// path - might be available. In this case it might pay off to split the load.
  case maybePartiallyRedundant(AccessPath)

  init(notRedundantWith subPath: AccessPath?) {
    if let subPath = subPath {
      self = .maybePartiallyRedundant(subPath)
    } else {
      self = .notRedundant
    }
  }
}

/// An instruction preceding the original load which provides the loaded value.
private enum AvailableValue {
  case viaLoad(LoadInst)
  /// Only used if the original load is a `load_borrow`.
  case viaLoadBorrow(LoadBorrowInst)
  case viaStore(StoreInst)
  case viaCopyAddr(CopyAddrInst)

  /// A point where the memory is valid, but its value is not known, e.g. the function entry or
  /// the instruction after an instruction which writes to the memory (e.g. an `apply`).
  /// The value is provided by inserting a new load at this point.
  /// This is only done to enable removing `load_borrow`s which are contained in the liverange
  /// (see `containedLoadBorrows`).
  case lifetimeBorder(Instruction)

  var value: Value {
    switch self {
    case .viaLoad(let load):   return load
    case .viaLoadBorrow(let loadBorrow): return loadBorrow
    case .viaStore(let store): return store.source
    case .viaCopyAddr:         fatalError("copy_addr must be lowered")
    case .lifetimeBorder:      fatalError("lifetimeBorder not supported")
    }
  }

  func getProjectionPath(to address: Value) -> SmallProjectionPath {
    return getProjectionPath(to: address.constantAccessPath)
  }

  func getProjectionPath(to accessPath: AccessPath) -> SmallProjectionPath {
    let fromAddr: Value
    switch self {
    case .viaLoad(let load):
      fromAddr = load.address
    case .viaLoadBorrow(let loadBorrow):
      fromAddr = loadBorrow.address
    case .viaStore(let store):
      fromAddr = store.destination
    case .viaCopyAddr(let copyAddr):
      fromAddr = copyAddr.destination
    case .lifetimeBorder:
      return SmallProjectionPath()
    }
    return fromAddr.constantAccessPath.getMaterializableProjection(to: accessPath)!
  }

  /// True if the available load or store can be split along the projection path to `accessPath`
  /// (see `StoreInst.split` and `LoadInst.split`).
  func canSplit(alongPathTo accessPath: AccessPath) -> Bool {
    let availableType: Type
    switch self {
    case .viaLoad(let load):         availableType = load.type
    case .viaStore(let store):       availableType = store.source.type
    case .viaCopyAddr(let copyAddr): availableType = copyAddr.destination.type.objectType
    case .lifetimeBorder:
      // Nothing is split: a new load is inserted at the border (see `shrinkMemoryLifetime`).
      return true
    case .viaLoadBorrow:
      fatalError("load_borrow cannot be an available value for a load [take]")
    }
    return availableType.canProject(path: getProjectionPath(to: accessPath), in: instruction.parentFunction)
  }

  var instruction: Instruction {
    switch self {
    case .viaLoad(let load):         return load
    case .viaLoadBorrow(let loadBorrow): return loadBorrow
    case .viaStore(let store):       return store
    case .viaCopyAddr(let copyAddr): return copyAddr
    case .lifetimeBorder(let inst):  return inst
    }
  }

  func getBuilderForProjections(_ context: FunctionPassContext) -> Builder {
    switch self {
    case .viaLoad(let load):        return Builder(after: load, context)
    // The projections must be created within the borrow scope of the `load_borrow`.
    case .viaLoadBorrow(let loadBorrow): return Builder(after: loadBorrow, context)
    case .viaStore(let store):      return Builder(before: store, context)
    case .lifetimeBorder(let inst): return Builder(before: inst, context)
    case .viaCopyAddr:              fatalError("copy_addr must be lowered")
    }
  }
}

private extension Array where Element == AvailableValue {
  func replaceCopyAddrsWithLoadsAndStores(_ context: FunctionPassContext) -> [AvailableValue] {
    return map {
      if case .viaCopyAddr(let copyAddr) = $0 {
        return .viaStore(copyAddr.replaceWithLoadAndStore(context).store)
      } else {
        return $0
      }
    }
  }
}

/// Represents the liverange (in terms of basic blocks) of the loaded value.
///
/// In contrast to a BlockRange, this liverange has multiple begin blocks (containing the
/// available values) and a single end block (containing the original load). For example:
///
///   bb1:
///     store %1 to %addr   // begin block
///     br bb3
///   bb2:
///     store %2 to %addr   // begin block
///     br bb3
///   bb3:
///     %3 = load %addr     // end block
///
private struct Liverange {

  private let load: LoadingInstruction
  private let accessPath: AccessPath
  private let storageDefBlock: BasicBlock?
  private let context: FunctionPassContext
  private let loadAddress: AddressWithAccessPath

  private var worklist: InstructionWorklist
  private var visitedTerminators: Stack<TermInst>
  private var availableValueInstructions: InstructionSet

  /// All instructions within read-only access scopes of the loaded memory, which overlap with the
  /// liverange. More precisely: all instructions after the `begin_access` up to and including the
  /// `end_access`es, i.e. all instructions before which a write to the memory is not allowed.
  /// Only collected for a `load [take]`.
  private var instructionsInReadOnlyAccessScopes: InstructionSet

  private(set) var potentiallyRedundantSubpath: AccessPath? = nil
  private(set) var availableValues = Array<AvailableValue>()
  private(set) var loadBorrows: SpecificIterableInstructionSet<LoadBorrowInst>
  private(set) var foundLoop = false

  init(load: LoadingInstruction, accessPath: AccessPath, _ context: FunctionPassContext) {
    self.load = load
    self.accessPath = accessPath
    self.storageDefBlock = accessPath.base.reference?.referenceRoot.parentBlock
    self.context = context
    self.loadAddress = AddressWithAccessPath(load.address)

    self.worklist = InstructionWorklist(context)
    self.visitedTerminators = Stack(context)
    self.availableValueInstructions = InstructionSet(context)
    self.instructionsInReadOnlyAccessScopes = InstructionSet(context)
    self.loadBorrows = SpecificIterableInstructionSet(context)
  }

  mutating func deinitialize() {
    worklist.deinitialize()
    visitedTerminators.deinitialize()
    availableValueInstructions.deinitialize()
    instructionsInReadOnlyAccessScopes.deinitialize()
    loadBorrows.deinitialize()
  }

  mutating func performDataflow(complexityBudget: inout Int) -> Bool {
    let functionEntry = load.parentFunction.entryBlock.instructions.first!
    if load == functionEntry {
      return false
    }
    var killingInstructions = Stack<Instruction>(context)
    defer { killingInstructions.deinitialize() }
    // The `begin_access`es of the scopes which are already added to `instructionsInReadOnlyAccessScopes`.
    var handledReadOnlyBeginAccesses = InstructionSet(context)
    defer { handledReadOnlyBeginAccesses.deinitialize() }
    var functionEntryReached = false

    worklist.pushPredecessors(of: load)

    while let inst = worklist.pop() {
      complexityBudget -= 1
      if complexityBudget <= 0 {
        return false
      }

      switch visit(instruction: inst, &handledReadOnlyBeginAccesses) {
      case .available:
        availableValueInstructions.insert(inst)
      case .overwritten:
        // The memory must be valid again at this point, therefore this is a "border" of the
        // liverange in the same sense as an available value instruction.
        availableValueInstructions.insert(inst)
        killingInstructions.append(inst)
      case .transparent:
        if inst == functionEntry {
          // We reached the function entry without finding an available value.
          functionEntryReached = true
          continue
        }
        // Abort if we find the storage definition of the access in case of a loop, e.g.
        //
        //   bb1:
        //     %storage_root = apply
        //     %2 = ref_element_addr %storage_root
        //     %3 = load %2
        //     cond_br %c, bb1, bb2
        //
        // The storage root is different in each loop iteration. Therefore the load in a
        // successive loop iteration does not load from the same address as in the previous iteration.
        if inst.previous == nil,
           let storageDefBlock = storageDefBlock,
           inst.parentBlock == storageDefBlock
        {
          return false
        }
        worklist.pushPredecessors(of: inst)
      }
    }
    if functionEntryReached || !killingInstructions.isEmpty {
      // The data flow reached points where the memory is valid, but the value is not known.
      // An available value can only be provided by inserting new loads at those lifetime borders.
      guard shouldInsertLoadsAtLifetimeBorders(after: killingInstructions) else {
        return false
      }
      addLifetimeBorders(functionEntryReached: functionEntryReached, after: killingInstructions)
    }
    return true
  }

  /// Returns true if it pays off to insert loads at the lifetime borders, i.e. at the function entry
  /// (if reached by the data flow) and after the `killingInstructions`.
  private func shouldInsertLoadsAtLifetimeBorders(after killingInstructions: Stack<Instruction>) -> Bool {
    // Re-creating the load at the border of the liverange only pays off if it enables the
    // removal of `load_borrow`s. This is only done for `load [copy]` and `load [take]`
    // (see `containedLoadBorrows` in `isRedundant`).
    switch load.kind {
    case .copy, .take:
      break
    case .trivial, .unqualified, .borrow:
      return false
    }
    if loadBorrows.isEmpty {
      return false
    }
    // Heuristic: don't insert a load at a border which is in a loop that doesn't contain the load or one
    // of the removed `load_borrow`s. The inserted load would be executed more often than the removed
    // instructions. In case of a `load [take]` it would also require to store back the value in the
    // loop, e.g.
    //
    //   bb1:                         // loop header
    //     cond_br %c, bb2, bb3
    //   bb2:
    //     apply %f(%addr)            // killing instruction: the border is after it
    //     br bb1
    //   bb3:
    //     %2 = load_borrow %addr
    //     ...
    //     %3 = load [take] %addr     // the load
    //
    if killingInstructions.contains(where: { (inst: Instruction) -> Bool in
      guard let loop = context.loopTree.innermostLoop(of: inst.parentBlock) else {
        return false
      }
      return !loop.contains(block: self.load.parentBlock) &&
             !self.loadBorrows.contains(where: { loop.contains(block: $0.parentBlock) })
    })
    {
      return false
    }

    return true
  }

  /// Adds the lifetime borders - at the function entry (if `functionEntryReached`) and after the
  /// `killingInstructions` - as available values.
  private mutating func addLifetimeBorders(functionEntryReached: Bool, after killingInstructions: Stack<Instruction>) {
    if functionEntryReached {
      availableValues.append(.lifetimeBorder(load.parentFunction.entryBlock.instructions.first!))
    }
    for killingInst in killingInstructions {
      if let next = killingInst.next {
        availableValues.append(.lifetimeBorder(next))
      } else {
        for succ in (killingInst as! TermInst).successors {
          let succInst = succ.instructions.first!
          // Only add a border for successors which are within the liverange. And not for
          // successors which begin with an instruction that already ends the liverange - either
          // because it provides an available value or because it overwrites the memory itself.
          if worklist.hasBeenPushed(succInst),
             !availableValueInstructions.contains(succInst)
          {
            availableValues.append(.lifetimeBorder(succInst))
          }
        }
      }
    }
  }

  /// Returns exit blocks from the liverange, i.e. paths from a begin block to a function exit
  /// which don't go through the end-block. For example:
  ///
  ///     store %1 to %addr   // begin
  ///     cond_br bb1, bb2
  ///   bb1:
  ///     %2 = load %addr     // end
  ///   bb2:
  ///     ...                 // exit
  ///
  var exits: [BasicBlock] {
    var exitBlocks = [BasicBlock]()
    for terminator in visitedTerminators {
      for succ in terminator.successors {
        let succInst = succ.instructions.first!
        // A successor is an exit if it is not within the liverange. This is also the case if the
        // successor begins with an instruction which ends the liverange - either because it provides
        // an available value or because it overwrites the memory. For example a back edge to a loop
        // header which begins with a store.
        if !worklist.hasBeenPushed(succInst) || availableValueInstructions.contains(succInst),
           succInst != load
        {
          exitBlocks.append(succ)
        }
      }
    }
    return exitBlocks
  }

  /// Returns true if writing to the loaded memory right before `insertionPoint` would be within a
  /// read-only access scope of the memory.
  ///
  /// Note that the data flow walk visits the `end_access` of all access scopes which overlap with
  /// the liverange, because the load itself is located outside of such a scope.
  func isWithinReadOnlyAccessScope(insertionPoint: Instruction) -> Bool {
    return instructionsInReadOnlyAccessScopes.contains(insertionPoint)
  }

  /// Adds all instructions after `beginAccess` up to and including its `end_access`es to
  /// `instructionsInReadOnlyAccessScopes`.
  private mutating func addInstructions(inScopeOf beginAccess: BeginAccessInst) {
    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushIfNotVisited(contentsOf: beginAccess.scopeEndingOperands.users)

    while let inst = worklist.pop() {
      instructionsInReadOnlyAccessScopes.insert(inst)
      worklist.pushPredecessors(of: inst, ignoring: beginAccess)
    }
  }

  /// Returns true if the whole borrow scope of `loadBorrow`, i.e. the `load_borrow` and all its
  /// `end_borrow`s, are within the liverange and no available value is located within the
  /// borrow scope. In this case the `load_borrow` can be replaced by a borrow of the available value.
  func isFullyContained(scopeOf loadBorrow: LoadBorrowInst) -> Bool {
    guard worklist.hasBeenPushed(loadBorrow) else {
      return false
    }
    for user in loadBorrow.uses.endingLifetime.users {
      guard let endBorrow = user as? EndBorrowInst, worklist.hasBeenPushed(endBorrow) else {
        return false
      }
    }
    if availableValues.singleElement != nil {
      // Shortcut for the common case: with only a single available value, this value is located above
      // the `load_borrow` and therefore the whole borrow scope is within the liverange.
      return true
    }
    var scope = InstructionRange(begin: loadBorrow, context)
    defer { scope.deinitialize() }
    scope.insert(contentsOf: loadBorrow.uses.endingLifetime.users)

    return availableValues.allSatisfy { !scope.contains($0.instruction) }
  }

  private enum Result {
    case overwritten
    case available
    case transparent
  }

  private mutating func visit(instruction: Instruction, _ handledReadOnlyBeginAccesses: inout InstructionSet) -> Result {
    switch instruction {
    case let endBorrow as EndBorrowInst:
      // Record `load_borrow`s which are (potentially) located within the liverange. They are
      // candidates to be replaced by a borrow of the available value (see `containedLoadBorrows`).
      // Neither the `end_borrow` nor the `load_borrow` itself writes to memory.
      if let loadBorrow = endBorrow.borrow as? LoadBorrowInst,
         accessPath.getMaterializableProjection(to: loadBorrow.address.constantAccessPath) != nil
      {
        loadBorrows.insert(loadBorrow)
        return .transparent
      }
      fallthrough

    case is FixLifetimeInst:
      // Those scope-ending instructions are only irrelevant if the preceding load is not changed.
      // If it is changed from `load [copy]` -> `load [take]` the memory effects of those scope-ending
      // instructions prevent that the `load [take]` will illegally mutate memory which is protected
      // from mutation by the scope.
      if load.kind != .take {
        return .transparent
      }

    case let endAccess as EndAccessInst:
      // Memory effects of access scope instructions are only defined to prevent the optimizer from
      // moving loads and stores across a `begin_access`/`end_access`. Such instructions don't read
      // or write the memory themselves. Therefore they never prevent a load from being redundant -
      // not even a `load [take]`, which shrinks the memory lifetime up to the available value.
      // But taking the value out of memory is not allowed within a non-mutating access scope. This
      // is checked separately with `isInImmutableAccessScope` and `isWithinReadOnlyAccessScope`
      // in `isRedundant`.
      if load.kind == .take,
         endAccess.beginAccess.isReadOnly,
         context.aliasAnalysis.mayAlias(loadAddress, endAccess.beginAccess.address),
         // A scope with multiple `end_access`es only needs to be added once.
         handledReadOnlyBeginAccesses.insert(endAccess.beginAccess)
      {
        addInstructions(inScopeOf: endAccess.beginAccess)
      }
      return .transparent

    case is BeginAccessInst:
      return .transparent

    case let precedingLoad as LoadInst:
      if precedingLoad == load {
        // We need to stop the data flow analysis when we visit the original load again.
        // This happens if the load is in a loop.
        foundLoop = true
        return .available
      }
      if precedingLoad.loadOwnership == .take {
        // A `load [take]` de-initializes the memory. Therefore it cannot provide an available value.
        // This can happen in dead code when working with unsafe pointer APIs, e.g. two consecutive
        // `load [take]` from the same address.
        break
      }
      let precedingLoadPath = precedingLoad.address.constantAccessPath
      if precedingLoadPath.getMaterializableProjection(to: accessPath) != nil {
        availableValues.append(.viaLoad(precedingLoad))
        return .available
      }
      if accessPath.getMaterializableProjection(to: precedingLoadPath) != nil,
         potentiallyRedundantSubpath == nil {
        potentiallyRedundantSubpath = precedingLoadPath
      }
      if load.kind != .take {
        return .transparent
      }

    case let loadBorrow as LoadBorrowInst:
      if load.kind == .borrow {
        // A preceding `load_borrow` can provide the value for a `load_borrow`, because the value
        // is only used as value hint, which has "none" ownership.
        if loadBorrow == load {
          foundLoop = true
          return .available
        }
        if loadBorrow.address.constantAccessPath.getMaterializableProjection(to: accessPath) != nil {
          availableValues.append(.viaLoadBorrow(loadBorrow))
          return .available
        }
      }
      if loadBorrows.contains(loadBorrow) {
        return .transparent
      }

    case let precedingStore as StoreInst:
      if precedingStore.source is Undef {
        return .overwritten
      }
      let precedingStorePath = precedingStore.destination.constantAccessPath
      if precedingStorePath.getMaterializableProjection(to: accessPath) != nil {
        availableValues.append(.viaStore(precedingStore))
        return .available
      }
      if accessPath.getMaterializableProjection(to: precedingStorePath) != nil,
         potentiallyRedundantSubpath == nil {
        potentiallyRedundantSubpath = precedingStorePath
      }

    case let precedingCopy as CopyAddrInst where precedingCopy.canOptimize:
      let copyPath = precedingCopy.destination.constantAccessPath
      if copyPath.getMaterializableProjection(to: accessPath) != nil {
        availableValues.append(.viaCopyAddr(precedingCopy))
        return .available
      }
      if accessPath.getMaterializableProjection(to: copyPath) != nil, potentiallyRedundantSubpath == nil {
        potentiallyRedundantSubpath = copyPath
      }

    case is DebugValueInst:
      // Ignore memory reads of `debug_value`. This is relevant for `load [take]`, for which reading
      // instructions end the liverange.
      return .transparent

    default:
      break
    }
    if load.kind == .take {
      // In case of `take`, don't allow reading instructions in the liverange.
      // Otherwise we cannot shrink the memory liverange afterwards.
      if instruction.mayReadOrWrite(address: loadAddress, context.aliasAnalysis) {
        return .overwritten
      }
    } else {
      if instruction.mayWrite(toAddress: loadAddress, context.aliasAnalysis) {
        return .overwritten
      }
    }
    if let termInst = instruction as? TermInst {
      // Note that a terminator is only recorded if it does _not_ overwrite the memory. The
      // successors of an overwriting terminator are not liverange exits: the value is dead there
      // because the memory is re-initialized by the terminator itself.
      visitedTerminators.append(termInst)
    }
    return .transparent
  }
}
