//===--- AliasAnalysis.swift - the alias analysis -------------------------===//
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

import OptimizerBridging
import SIL

extension FunctionPassContext {
  var aliasAnalysis: AliasAnalysis {
    let bridgedAA = _bridged.getAliasAnalysis()
    return AliasAnalysis(bridged: bridgedAA, context: self)
  }
}

extension Instruction {
  func mayRead(fromAddress: Value, _ aliasAnalysis: AliasAnalysis) -> Bool {
    aliasAnalysis.getMemoryEffect(of: self, on: fromAddress).read
  }

  func mayWrite(toAddress: Value, _ aliasAnalysis: AliasAnalysis) -> Bool {
    if toAddress.isImmutableAddress {
      // Take a shortcut for indirect-in arguments.
      return false
    }
    return aliasAnalysis.getMemoryEffect(of: self, on: toAddress).write
  }

  func mayReadOrWrite(address: Value, _ aliasAnalysis: AliasAnalysis) -> Bool {
    let effect = aliasAnalysis.getMemoryEffect(of: self, on: address)
    if address.isImmutableAddress {
      return effect.read
    }
    return effect.read || effect.write
  }
}

/// Alias analysis.
///
/// It's mainly used to check if an instruction may read or write from/to a specific address.
///
struct AliasAnalysis {
  fileprivate let bridged: BridgedAliasAnalysis
  fileprivate let context: FunctionPassContext

  //===--------------------------------------------------------------------===//
  //                           Public interface
  //===--------------------------------------------------------------------===//

  /// Returns the effects of `inst`'s memory behavior on the memory pointed to by  the `address`.
  func getMemoryEffect(of inst: Instruction, on address: Value) -> SideEffects.Memory {
    precondition(address.type.isAddress, "getMemoryEffects requires address value")
    var result = computeMemoryEffect(of: inst, on: MemoryLocation.memoryAddress(address))
    if result.write && isImmutable(instruction: inst, inScopeOf: address) {
      result.write = false
    }
    // In the past we cached the result per instruction-address pair. But it turned out that the hit-miss rate was
    // pretty high (~ 1:7) and the cache lookup took as long as recomputing.
    return result
  }

  /// Returns true if `v1` and `v2` do or may alias.
  ///
  /// Usually `v1` and `v2` are addresses and in this case the return value is true if both addresses
  /// do or may point to the same memory location.
  ///
  /// If `v1` or `v2` is not an address, the function checks if any "interior" pointer of the value may alias
  /// with the other value or address.
  /// If a value is a class, "interior" pointer means: an address of any stored property of the class instance.
  /// If a value is a struct or another value type, "interior" pointers refer to any stored propery addresses of any
  /// class references in the struct or value type. For example:
  ///
  /// class C { var x: Int; var y: Int }
  /// struct S { var c1: C; var c2: C }
  ///
  /// `mayAlias(s, someAddress)` checks if someAddress aliases with `s.c1.x`, `s.c1.y`, `s.c2.x` or `s.c2.y`
  ///
  func mayAlias(_ v1: Value, _ v2: Value) -> Bool {
    if v1.type.isAddress && v2.type.isAddress {
      // The projection-path based check and TBAA can only be done if both values are really addresses.
      // This is the common case.
      let accessPath1 = v1.accessPath
      let accessPath2 = v2.accessPath
      if accessPath1.isDistinct(from: accessPath2) {
        return false
      }
      // Type-based alias analysis is only of minor importance. It's only needed if unsafe pointers are in play.
      // There are some critical functions in the stdlib which use unsafe pointers. Therefore we cannot omit TBAA.
      if isTypeDistinct(v1, v2, accessPath1.base, accessPath2.base) {
        return false
      }
    }
    // Finaly use escape info to check if one address "escapes" to the other address.
    return v1.allContainedAddresss.canAddressAlias(with: v2.allContainedAddresss, context)
  }

  static func register() {
    BridgedAliasAnalysis.registerAnalysis(
      // initFn
      { (bridgedAliasAnalysis: BridgedAliasAnalysis, size: Int) in
        assert(MemoryLayout<Cache>.size <= size, "wrong AliasAnalysis.cache size")
        bridgedAliasAnalysis.mutableCachePointer.initializeMemory(as: Cache.self, repeating: Cache(), count: 1)
      },
      // destroyFn
      { (bridgedAliasAnalysis: BridgedAliasAnalysis) in
        bridgedAliasAnalysis.mutableCachePointer.assumingMemoryBound(to: Cache.self).deinitialize(count: 1)
      },
      // getMemEffectsFn
      { (bridgedCtxt: BridgedPassContext,
         bridgedAliasAnalysis: BridgedAliasAnalysis,
         bridgedAddr: BridgedValue,
         bridgedInst: BridgedInstruction) -> BridgedMemoryBehavior in
        let aa = AliasAnalysis(bridged: bridgedAliasAnalysis, context: FunctionPassContext(_bridged: bridgedCtxt))
        return aa.getMemoryEffect(of: bridgedInst.instruction, on: bridgedAddr.value).bridged
      },
      // isObjReleasedFn
      { (bridgedCtxt: BridgedPassContext,
         bridgedAliasAnalysis: BridgedAliasAnalysis,
         bridgedObj: BridgedValue,
         bridgedInst: BridgedInstruction) -> Bool in
        let context = FunctionPassContext(_bridged: bridgedCtxt)
        let aa = AliasAnalysis(bridged: bridgedAliasAnalysis, context: context)
        let inst = bridgedInst.instruction
        let obj = bridgedObj.value
        let path = SmallProjectionPath(.anyValueFields)
        let budget = aa.getComplexityBudget(for: inst.parentFunction)
        if let apply = inst as? FullApplySite {
          // Workaround for quadratic complexity in ARCSequenceOpts.
          // We need to use an ever lower budget to not get into noticeable compile time troubles.
          let effect = aa.getOwnershipEffect(of: apply, for: obj, path: path, complexityBudget: budget / 10)
          return effect.destroy
        }
        return obj.at(path).isEscaping(using: EscapesToInstructionVisitor(target: inst, isAddress: false),
                                       complexityBudget: budget, context)
      },

      // isAddrVisibleFromObj
      { (bridgedCtxt: BridgedPassContext,
         bridgedAliasAnalysis: BridgedAliasAnalysis,
         bridgedAddr: BridgedValue,
         bridgedObj: BridgedValue) -> Bool in
        let context = FunctionPassContext(_bridged: bridgedCtxt)
        let aa = AliasAnalysis(bridged: bridgedAliasAnalysis, context: context)
        let addr = bridgedAddr.value.allContainedAddresss

        // This is similar to `canReferenceSameFieldFn`, except that all addresses of all objects are
        // considered which are transitively visible from `bridgedObj`.
        let anythingReachableFromObj = bridgedObj.value.at(SmallProjectionPath(.anything))
        return addr.canAddressAlias(with: anythingReachableFromObj,
                                    complexityBudget: aa.getComplexityBudget(for: bridgedObj.value.parentFunction),
                                    context)
      },

      // mayAliasFn
      { (bridgedCtxt: BridgedPassContext,
         bridgedAliasAnalysis: BridgedAliasAnalysis,
         bridgedLhs: BridgedValue,
         bridgedRhs: BridgedValue) -> Bool in
        let context = FunctionPassContext(_bridged: bridgedCtxt)
        let aa = AliasAnalysis(bridged: bridgedAliasAnalysis, context: context)
        return aa.mayAlias(bridgedLhs.value, bridgedRhs.value)
      }
    )
  }

  //===--------------------------------------------------------------------===//
  //                              Internals
  //===--------------------------------------------------------------------===//

  private var cache: Cache {
    unsafeAddress {
      bridged.cachePointer.assumingMemoryBound(to: Cache.self)
    }
    nonmutating unsafeMutableAddress {
      bridged.mutableCachePointer.assumingMemoryBound(to: Cache.self)
    }
  }

  // The actual logic to compute the memory effect of an instruction.
  private func computeMemoryEffect(of inst: Instruction, on memLoc: MemoryLocation) -> SideEffects.Memory {
    switch inst {
    case let beginAccess as BeginAccessInst:
      // begin_access does not physically read or write memory. But we model it as a memory read and/or write
      // to prevent optimizations to move other aliased loads/stores across begin_access into the access scope.
      return getAccessScopeEffect(of: beginAccess, on: memLoc)

    case let endAccess as EndAccessInst:
      // Similar to begin_access, we model it as a memory read and/or write to prevent optimizations to move
      // other aliased loads/stores into the access scope.
      return getAccessScopeEffect(of: endAccess.beginAccess, on: memLoc)

    case is InjectEnumAddrInst,
         is UncheckedTakeEnumDataAddrInst,
         is InitExistentialAddrInst,
         is DeinitExistentialAddrInst,
         is FixLifetimeInst,
         is ClassifyBridgeObjectInst,
         is ValueToBridgeObjectInst,
         is DeallocStackInst:
      if memLoc.mayAlias(with: (inst as! UnaryInstruction).operand.value, self) {
        return inst.memoryEffects
      }
      return .noEffects

    case is CondFailInst,
         is StrongRetainInst,
         is UnownedRetainInst,
         is StrongRetainUnownedInst,
         is RetainValueInst,
         is UnmanagedRetainValueInst,
         is CopyValueInst,
         is StrongCopyUnownedValueInst,
         is StrongCopyUnmanagedValueInst,
         is StrongCopyWeakValueInst,
         is BeginBorrowInst,
         is BeginCOWMutationInst:
      return .noEffects

    case let load as LoadInst:
      if memLoc.mayAlias(with: load.address, self) {
        switch load.loadOwnership {
        case .unqualified, .copy, .trivial:
          return .init(read: true)
        case .take:
          // "take" is conceptually a write to the memory location.
          return .worstEffects
        }
      } else {
        return .noEffects
      }
    case let store as StoreInst:
      if memLoc.isLetValue && store.destination.accessBase != memLoc.address.accessBase {
        return .noEffects
      }
      if memLoc.mayAlias(with: store.destination, self) {
        return inst.memoryEffects
      } else {
        switch store.storeOwnership {
        case .unqualified, .initialize, .trivial:
          return .noEffects
        case .assign:
          // Consider side effects of the destructor
          return defaultEffects(of: store, on: memLoc)
        }
      }
    case let storeBorrow as StoreBorrowInst:
      return memLoc.mayAlias(with: storeBorrow.destination, self) ? .init(write: true) : .noEffects

    case let mdi as MarkDependenceInst:
      if mdi.base.type.isAddress && memLoc.mayAlias(with: mdi.base, self) {
        return .init(read: true)
      }
      return .noEffects

    case let mdai as MarkDependenceAddrInst:
      if memLoc.mayAlias(with: mdai.address, self) {
        return .init(read: true, write: true)
      }
      if mdai.base.type.isAddress && memLoc.mayAlias(with: mdai.base, self) {
        return .init(read: true)
      }
      return .noEffects

    case let copy as SourceDestAddrInstruction:
      let mayRead = memLoc.mayAlias(with: copy.source, self)
      let mayWrite = memLoc.mayAlias(with: copy.destination, self)
      var effects = SideEffects.Memory(read: mayRead, write: mayWrite || (mayRead && copy.isTakeOfSource))
      if !copy.isInitializationOfDestination {
        effects.merge(with: defaultEffects(of: copy, on: memLoc))
      }
      return effects

    case let apply as FullApplySite:
      return getApplyEffect(of: apply, on: memLoc)

    case let partialApply as PartialApplyInst:
      return getPartialApplyEffect(of: partialApply, on: memLoc)

    case let endApply as EndApplyInst:
      let beginApply = endApply.beginApply
      if case .yield(let addr) = memLoc.address.accessBase, addr.parentInstruction == beginApply {
        // The lifetime of yielded values always end at the end_apply. This is required because a yielded
        // address is non-aliasing inside the begin/end_apply scope, but might be aliasing after the end_apply.
        // For example, if the callee yields an `ref_element_addr` (which is encapsulated in a begin/end_access).
        // Therefore, even if the callee does not write anything, the effects must be "read" and "write".
        return .worstEffects
      }
      return getApplyEffect(of: beginApply, on: memLoc)

    case let abortApply as AbortApplyInst:
      let beginApply = abortApply.beginApply
      if case .yield(let addr) = memLoc.address.accessBase, addr.parentInstruction == beginApply {
        // See the comment for `end_apply` above.
        return .worstEffects
      }
      return getApplyEffect(of: beginApply, on: memLoc)

    case let builtin as BuiltinInst:
      return getBuiltinEffect(of: builtin, on: memLoc)

    case let endBorrow as EndBorrowInst:
      switch endBorrow.borrow {
      case let storeBorrow as StoreBorrowInst:
        precondition(endBorrow.borrow.type.isAddress)
        return memLoc.mayAlias(with: storeBorrow, self) ? .worstEffects : .noEffects
      case let beginBorrow as BeginBorrowInst where !beginBorrow.hasPointerEscape:
        return getBorrowEffects(of: endBorrow, on: memLoc)
      case let loadBorrow as LoadBorrowInst:
        let borrowEffects = getBorrowEffects(of: endBorrow, on: memLoc)
        // In addition to the "regular" borrow effects, a load_borrow also has effects on the memory location
        // from where it loads the value. This includes "write" to prevent any optimization to change the
        // memory location after the load_borrow.
        if borrowEffects != .worstEffects && memLoc.mayAlias(with: loadBorrow.address, self) {
          return .worstEffects
        }
        return borrowEffects
      default:
        break
      }
      return defaultEffects(of: endBorrow, on: memLoc)

    case let debugValue as DebugValueInst:
      let v = debugValue.operand.value
      if v.type.isAddress, !(v is Undef), memLoc.mayAlias(with: v, self) {
        return .init(read: true)
      } else {
        return .noEffects
      }

    case let destroy as DestroyValueInst:
      if destroy.destroyedValue.type.isNoEscapeFunction {
        return .noEffects
      }
      if destroy.isDeadEnd {
        // We don't have to take deinit effects into acount for a `destroy_value [dead_end]`.
        // Such destroys are lowered to no-ops and will not call any deinit.
        return .noEffects
      }
      return defaultEffects(of: destroy, on: memLoc)

    default:
      let effects = inst.memoryEffects
      if effects == .noEffects {
        return effects
      }
      return defaultEffects(of: inst, on: memLoc)
    }
  }

  /// Returns the memory effects which protect the interior pointers of a borrowed value.
  /// For example, an `end_borrow` of a class reference must alias with all field addresses (= the interior
  /// pointers) of the class instance.
  private func getBorrowEffects(of endBorrow: EndBorrowInst, on memLoc: MemoryLocation) -> SideEffects.Memory {
    let accessPath = memLoc.address.accessPath
    switch accessPath.base {
    case .stack, .global, .argument, .storeBorrow:
      // Those access bases cannot be interior pointers of a borrowed value
      return .noEffects
    case .pointer, .index, .unidentified, .yield:
      // We don't know anything about this address -> get the conservative effects
      return defaultEffects(of: endBorrow, on: memLoc)
    case .box, .class, .tail:
      // Check if the memLoc is "derived" from the begin_borrow, i.e. is an interior pointer.
      var walker = FindBeginBorrowWalker(beginBorrow: endBorrow.borrow as! BeginBorrowInstruction)
      return walker.visitAccessStorageRoots(of: accessPath) ? .noEffects : .worstEffects
    }
  }

  private func getAccessScopeEffect(of beginAccess: BeginAccessInst, on memLoc: MemoryLocation) -> SideEffects.Memory {
    if !memLoc.mayAlias(with: beginAccess.address, self) {
      return .noEffects
    }
    switch beginAccess.accessKind {
    case .`init`:
      return .init(read: false, write: true)
    case .read:
      return .init(read: true, write: false)
    case .modify:
      return memLoc.isLetValue ? .noEffects : .worstEffects
    case .deinit:
      // For the same reason we treat a `load [take]` or a `destroy_addr`
      // as a memory write, we do that for a `begin_access [deinit]` as well.
      return .worstEffects
    }
  }

  private func getApplyEffect(of apply: FullApplySite, on memLoc: MemoryLocation) -> SideEffects.Memory {
    let calleeAnalysis = context.calleeAnalysis
    let visitor = FullApplyEffectsVisitor(apply: apply, calleeAnalysis: calleeAnalysis, isAddress: true)
    let memoryEffects: SideEffects.Memory

    // First try to figure out to which argument(s) the address "escapes" to.
    if let result = memLoc.addressWithPath.visit(using: visitor,
                                                 initialWalkingDirection: memLoc.walkingDirection,
                                                 context)
    {
      // The resulting effects are the argument effects to which `address` escapes to.
      memoryEffects = result.memory
    } else {
      // The address has unknown escapes. So we have to take the global effects of the called function(s).
      memoryEffects = calleeAnalysis.getSideEffects(ofApply: apply).memory
    }
    return memoryEffects
  }

  private func getPartialApplyEffect(of partialApply: PartialApplyInst, on memLoc: MemoryLocation) -> SideEffects.Memory {
    let visitor = PartialApplyEffectsVisitor(partialApply: partialApply)

    // Figure out to which argument(s) the address "escapes" to.
    if let result = memLoc.addressWithPath.visit(using: visitor,
                                                 initialWalkingDirection: memLoc.walkingDirection,
                                                 context)
    {
      // The resulting effects are the argument effects to which the address escapes to.
      return result
    }
    return .worstEffects
  }

  private func getBuiltinEffect(of builtin: BuiltinInst, on memLoc: MemoryLocation) -> SideEffects.Memory {
    switch builtin.id {
    case .Once, .OnceWithContext:
      if !memLoc.addressWithPath.isEscaping(using: AddressVisibleByBuiltinOnceVisitor(),
                                            initialWalkingDirection: memLoc.walkingDirection,
                                            context)
      {
        return .noEffects
      }
      let callee = builtin.operands[1].value
      return context.calleeAnalysis.getSideEffects(ofCallee: callee).memory
    case .PrepareInitialization, .ZeroInitializer:
      if builtin.arguments.count == 1, memLoc.mayAlias(with: builtin.arguments[0], self) {
        return .init(write: true)
      }
      return .noEffects
    default:
      return defaultEffects(of: builtin, on: memLoc)
    }
  }

  private func getOwnershipEffect(of apply: FullApplySite, for value: Value,
                                  path: SmallProjectionPath,
                                  complexityBudget: Int) -> SideEffects.Ownership {
    let visitor = FullApplyEffectsVisitor(apply: apply, calleeAnalysis: context.calleeAnalysis, isAddress: false)
    if let result = value.at(path).visit(using: visitor, complexityBudget: complexityBudget, context) {
      // The resulting effects are the argument effects to which `value` escapes to.
      return result.ownership
    } else {
      // `value` has unknown escapes. So we have to take the global effects of the called function(s).
      return visitor.calleeAnalysis.getSideEffects(ofApply: apply).ownership
    }
  }

  /// Gets the default effects of an instruction.
  /// It just checks if `memLoc` can somehow be visible by `inst` at all.
  private func defaultEffects(of inst: Instruction, on memLoc: MemoryLocation) -> SideEffects.Memory {
    if memLoc.addressWithPath.isEscaping(using: EscapesToInstructionVisitor(target: inst, isAddress: true),
                                         initialWalkingDirection: memLoc.walkingDirection,
                                         complexityBudget: getComplexityBudget(for: inst.parentFunction), context)
    {
      return inst.memoryEffects
    }
    return .noEffects
  }

  // To avoid quadratic complexity for large functions, we limit the amount of work that the EscapeUtils are
  // allowed to to. This keeps the complexity linear.
  //
  // This arbitrary limit is good enough for almost all functions. It lets
  // the EscapeUtils do several hundred up/down walks which is much more than needed in most cases.
  private func getComplexityBudget(for function: Function) -> Int {
    if cache.estimatedFunctionSize == nil {
      var numInsts = 0
      for _ in function.instructions { numInsts += 1 }
      cache.estimatedFunctionSize = numInsts
    }
    return 1000000 / cache.estimatedFunctionSize!
  }

  /// Returns true if the `instruction` (which in general writes to memory) is immutable in a certain scope,
  /// defined by `address`.
  ///
  /// That means that even if we don't know anything about `instruction`, we can be sure
  /// that `instruction` cannot write to `address`, if it's inside the addresse's scope.
  /// An immutable scope is for example a read-only `begin_access`/`end_access` scope.
  /// Another example is a borrow scope of an immutable copy-on-write buffer.
  private func isImmutable(instruction: Instruction, inScopeOf address: Value) -> Bool {
    guard let immutableScope = ImmutableScope(for: address, context) else {
      return false
    }
    if case .wholeFunction = immutableScope {
      // No need to check if the instruction is inside the scope if the scope is the whole function.
      return true
    }

    if !isImmutableCacheComputed(for: immutableScope) {
      computeImmutableCache(for: immutableScope)
    }
    let key = Cache.ScopeKey(beginScope: immutableScope.beginScopeInstruction, instInScope: instruction)
    return cache.immutableInstructionsInScopes.contains(key)
  }

  private func isImmutableCacheComputed(for immutableScope: ImmutableScope) -> Bool {
    let beginScopeInst = immutableScope.beginScopeInstruction

    // The special key of (beginScopeInst, beginScopeInst) is used as a marker to check if the immutable scope
    // is already computed at all.
    let key = Cache.ScopeKey(beginScope: beginScopeInst, instInScope: beginScopeInst)
    return !cache.immutableInstructionsInScopes.insert(key).inserted
  }

  private func computeImmutableCache(for immutableScope: ImmutableScope) {
    let beginScopeInst = immutableScope.beginScopeInstruction
    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }

    immutableScope.pushEndScopeInstructions(to: &worklist)

    while let inst = worklist.pop() {
      if inst.mayWriteToMemory {
        if case .modifyAccess(let beginAccessInst) = immutableScope,
           computeMemoryEffect(of: inst, on: .modifyAccessScope(beginAccessInst)).write
        {
        } else {
          cache.immutableInstructionsInScopes.insert(Cache.ScopeKey(beginScope: beginScopeInst, instInScope: inst))
        }
      }
      worklist.pushPredecessors(of: inst, ignoring: beginScopeInst)
    }
  }
}

//===--------------------------------------------------------------------===//
//                       Internal data structures
//===--------------------------------------------------------------------===//

private struct Cache {
  struct ScopeKey: Hashable {
    let beginScope: Instruction
    let instInScope: Instruction
  }

  // Caches immutable instructions inside specific scopes.
  var immutableInstructionsInScopes = Set<ScopeKey>()

  // Used to limit complexity. The size is computed lazily.
  var estimatedFunctionSize: Int? = nil
}

// A simple abstraction for the kind of address the memory effect is computed.
private enum MemoryLocation {
  // The usual case: an arbitrary address
  case memoryAddress(Value)

  // The address of an modify-access, within the access scope.
  // The difference to an arbitrary address is that we know that there are no other reads or writes to the
  // access-address within the access scope.
  // This is used when computing the immutable-scope of a `begin_access [modify]`
  case modifyAccessScope(BeginAccessInst)

  var addressWithPath: ProjectedValue {
    let addrValue = self.address
    return addrValue.at(SmallProjectionPath(.anyValueFields))
  }

  var address: Value {
    switch self {
    case .memoryAddress(let value):
      precondition(value.type.isAddress, "expected address value")
      return value
    case .modifyAccessScope(let beginAccess):
      return beginAccess
    }
  }

  var walkingDirection: EscapeUtilityTypes.WalkingDirection {
    switch self {
    case .memoryAddress:
      // We need to consider where the address comes from
      return .up
    case .modifyAccessScope:
      // We don't care where the access-address comes from because we know that all accesses to the address
      // (in the access scope) must be derived from the `begin_access`.
      return .down
    }
  }

  var isLetValue: Bool {
    switch self {
    case .memoryAddress(let addr):
      return addr.accessBase.isLet
    case .modifyAccessScope:
      return false
    }
  }

  func mayAlias(with otherAddr: Value, _ aliasAnalysis: AliasAnalysis) -> Bool {
    return aliasAnalysis.mayAlias(address, otherAddr)
  }
}

/// A scope in which certain instructions can be assumed to be immutable,
/// i.e. don't write to the scope's based address.
private enum ImmutableScope {
  // If the based address is or is derived from an indirect-in or guaranteed function argument.
  // The scope spans over the whole function and we don't need to do any scope computation.
  case wholeFunction

  // If the based address is or is derived from a begin_access with access kind "read".
  case readAccess(BeginAccessInst)

  // If the based address is or is derived from a begin_access with access kind "modify".
  case modifyAccess(BeginAccessInst)

  // If the based address is an interior pointer (e.g. the address of a class field) of a borrowed object.
  case borrow(BeginBorrowValue)

  init?(for basedAddress: Value, _ context: FunctionPassContext) {
    switch basedAddress.enclosingAccessScope {
    case .access(let beginAccess):
      if beginAccess.isUnsafe {
        return nil
      }

      // This is a workaround for a bug in the move-only checker: rdar://151841926.
      // The move-only checker sometimes inserts destroy_addr within read-only static access scopes.
      // TODO: remove this once the bug is fixed.
      if beginAccess.isStatic {
        return nil
      }

      switch beginAccess.accessKind {
      case .read:
        self = .readAccess(beginAccess)
      case .modify:
        self = .modifyAccess(beginAccess)
      case .`init`, .deinit:
        return nil
      }
    case .base(let accessBase):
      let object: Value
      switch accessBase {
      case .class(let elementAddr):
        if !elementAddr.isImmutable {
          return nil
        }
        object = elementAddr.instance
      case .tail(let tailAddr):
        if !tailAddr.isImmutable {
          return nil
        }
        object = tailAddr.instance
      case .global(let global):
        if global.isLet && !basedAddress.parentFunction.canInitializeGlobal {
          self = .wholeFunction
          return
        }
        return nil
      default:
        return nil
      }
      if !object.parentFunction.hasOwnership {
        // Special handling for non-OSSA: we can only reason about guaranteed function arguments.
        var walker = IsGuaranteedFunctionArgumentWalker()
        if walker.walkUp(value: object, path: SmallProjectionPath()) != .continueWalk {
          return nil
        }
        self = .wholeFunction
      } else {
        guard let singleBorrowIntroducer = object.getBorrowIntroducers(context).singleElement else {
          return nil
        }

        switch singleBorrowIntroducer {
        case .beginBorrow, .loadBorrow, .reborrow:
          self = .borrow(singleBorrowIntroducer)
        case .functionArgument:
          self = .wholeFunction
        case .beginApply, .uncheckOwnershipConversion:
          return nil
        }
      }
      case .dependence(let markDep):
        // ignore mark_dependence for the purpose of alias analysis.
        self.init(for: markDep.value, context)
    }
  }

  var beginScopeInstruction: SingleValueInstruction {
    switch self {
    case .wholeFunction:
      fatalError("should not request the beginScopeInstruction of a whole function")
    case .readAccess(let beginAccess), .modifyAccess(let beginAccess):
      return beginAccess
    case .borrow(let beginBorrowValue):
      switch beginBorrowValue {
        case .beginBorrow(let bbi): return bbi
        case .loadBorrow(let lbi):  return lbi
        case .reborrow(let phi):    return phi.borrowedFrom!
        default:                    fatalError("unsupported BeginBorrowValue")
      }
    }
  }

  func pushEndScopeInstructions(to worklist: inout InstructionWorklist) {
    switch self {
    case .wholeFunction:
      fatalError("should not pushEndScopeInstructions of a whole function")
    case .readAccess(let beginAccess), .modifyAccess(let beginAccess):
      for endAccess in beginAccess.endAccessInstructions {
        worklist.pushPredecessors(of: endAccess, ignoring: beginAccess)
      }
    case .borrow(let beginBorrowValue):
      let beginScopeInst = beginScopeInstruction
      for endBorrowOp in beginBorrowValue.scopeEndingOperands {
        worklist.pushPredecessors(of: endBorrowOp.instruction, ignoring: beginScopeInst)
      }
    }
  }
}

private struct FindBeginBorrowWalker : ValueUseDefWalker {
  let beginBorrow: BeginBorrowInstruction
  var walkUpCache = WalkerCache<Path>()

  mutating func walkUp(value: Value, path: SmallProjectionPath) -> WalkResult {
    if value == beginBorrow {
      return .abortWalk
    }
    if value.ownership != .guaranteed {
      // If value is owned then it cannot be the borrowed value.
      return .continueWalk
    }
    return walkUpDefault(value: value, path: path)
  }

  mutating func rootDef(value: Value, path: SmallProjectionPath) -> WalkResult {
    switch value {
    case is FunctionArgument,
         // Loading a value from memory cannot be the borrowed value.
         // Note that we exclude the "regular" `load` by checking for guaranteed ownership in `walkUp`.
         is LoadBorrowInst:
      return .continueWalk
    default:
      return .abortWalk
    }
  }
}

private struct IsGuaranteedFunctionArgumentWalker : ValueUseDefWalker {
  var walkUpCache = WalkerCache<Path>()

  mutating func rootDef(value: Value, path: SmallProjectionPath) -> WalkResult {
    if let funcArg = value as? FunctionArgument, funcArg.convention.isGuaranteed {
      return .continueWalk
    }
    return .abortWalk
  }
}

// Computes the effects which a called function (potentially) has on an address.
private struct FullApplyEffectsVisitor : EscapeVisitorWithResult {
  let apply: FullApplySite
  let calleeAnalysis: CalleeAnalysis
  let isAddress: Bool
  var result = SideEffects.GlobalEffects()

  mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
    let user = operand.instruction
    if user is ReturnInst {
      // Anything which is returned cannot escape to an instruction inside the function.
      return .ignore
    }
    if user == apply {
      if apply.isCallee(operand: operand) {
        // If the address "escapes" to the callee of the apply it means that the address was captured
        // by an inout_aliasable operand of an partial_apply.
        // Therefore assume that the called function will both, read and write, to the address.
        return .abort
      }
      let e = calleeAnalysis.getSideEffects(of: apply, operand: operand, path: path.projectionPath)
      result.merge(with: e)
    }
    return .continueWalk
  }

  var followTrivialTypes: Bool { isAddress }
  var followLoads: Bool { !isAddress }
}

// In contrast to a full apply, the effects of a partial_apply don't depend on the callee
// (a partial_apply doesn't call anything, it just creates a thick function pointer).
// The only effects come from capturing the arguments (either consuming or guaranteeed).
private struct PartialApplyEffectsVisitor : EscapeVisitorWithResult {
  let partialApply: PartialApplyInst
  var result = SideEffects.Memory.noEffects

  mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
    let user = operand.instruction
    if user is ReturnInst {
      // Anything which is returned cannot escape to an instruction inside the function.
      return .ignore
    }
    if user == partialApply,
       let convention = partialApply.convention(of: operand)
    {
      switch convention {
      case .indirectIn, .indirectInGuaranteed:
        result.read = true
        if !partialApply.isOnStack {
          result.write = true
        }
      case .indirectInout, .indirectInoutAliasable, .packInout:
        break
      case .directOwned, .directUnowned, .directGuaranteed, .packOwned, .packGuaranteed:
        break
      case .indirectOut, .packOut, .indirectInCXX:
        fatalError("invalid convention for partial_apply")
      }
    }
    return .continueWalk
  }

  var followTrivialTypes: Bool { true }
  var followLoads: Bool { false }
}

private struct AddressVisibleByBuiltinOnceVisitor : EscapeVisitor {
  var followTrivialTypes: Bool { true }
  var followLoads: Bool { false }
}

/// Checks if a value is "escaping" to the `target` instruction.
private struct EscapesToInstructionVisitor : EscapeVisitor {
  let target: Instruction
  let isAddress: Bool

  mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
    let user = operand.instruction
    if user == target {
      return .abort
    }
    if user is ReturnInst {
      // Anything which is returned cannot escape to an instruction inside the function.
      return .ignore
    }
    return .continueWalk
  }

  var followTrivialTypes: Bool { isAddress }
  var followLoads: Bool { !isAddress }
}

private extension Value {
  var isImmutableAddress: Bool {
    switch accessBase {
    case .argument(let arg):
      return arg.convention == .indirectInGuaranteed
    default:
      return false
    }
  }
}

//===--------------------------------------------------------------------===//
//                  Type-based alias analysis (TBAA)
//===--------------------------------------------------------------------===//

/// Perform type-based alias analysis (TBAA).
private func isTypeDistinct(_ address1: Value, _ address2: Value,
                            _ accessBase1: AccessBase, _ accessBase2: AccessBase
) -> Bool {
  let type1 = address1.type
  let type2 = address2.type
  if type1 == type2 {
    return false
  }
  if !accessBase1.isEligibleForTBAA || !accessBase2.isEligibleForTBAA {
    return false
  }
  if !type1.isEligibleForTBAA || !type2.isEligibleForTBAA {
    return false
  }
  let function = address1.parentFunction

  // Even if the types are different, one type can contain the other type, e.g.
  //
  // struct S { var i: Int }
  // isTypeDistinct(addressOfS, addressOfInt) -> false
  //
  if type1.aggregateIsOrContains(type2, in: function) || type2.aggregateIsOrContains(type1, in: function) {
    return false
  }
  if type1.isClass && type2.isClass {
    return false
  }
  return true
}

private extension AccessBase {
  func isIndirectResult(of apply: FullApplySite) -> Bool {
    return apply.indirectResultOperands.contains { $0.value.accessBase == self }
  }

  var isEligibleForTBAA: Bool {
    // Only access bases which cannot be the result of an not-strict pointer conversion are eligible.
    switch self {
    case .box, .class, .tail, .global:
      return true
    case .pointer(let pointerToAddress):
      return pointerToAddress.isStrict
    default:
      return false
    }
  }
}

private extension Type {
  var isEligibleForTBAA: Bool {
    if hasArchetype {
      // Two distinct types which contain archetypes can be actually the same, e.g.:
      //   SomeGenericStruct<T>   // T is a type parameter, which can potentially also be Int
      //   SomeGenericStruct<Int>
      return false
    }
    if isClass || isStruct || isEnum || isTuple {
      return true
    }
    // Only support the most important builtin types to be on the safe side.
    // Historically we assumed that Builtin.RawPointer can alias everything (but why?).
    if isBuiltinInteger || isBuiltinFloat {
      return true
    }
    return false
  }
}

private extension Function {
  var canInitializeGlobal: Bool {
    return isGlobalInitOnceFunction ||
           // In non -parse-as-library mode globals are initialized in the `main` function.
           name == "main"
  }
}

//===--------------------------------------------------------------------===//
//                              Bridging
//===--------------------------------------------------------------------===//

private extension SideEffects.Memory {
  var bridged: BridgedMemoryBehavior {
    switch (read, write) {
      case (false, false): return .None
      case (true, false):  return .MayRead
      case (false, true):  return .MayWrite
      case (true, true):   return .MayReadWrite
    }
  }
}

private extension BridgedAliasAnalysis {
  var cachePointer: UnsafeRawPointer {
    UnsafeRawPointer(aa)
  }

  var mutableCachePointer: UnsafeMutableRawPointer {
    UnsafeMutableRawPointer(aa)
  }
}
