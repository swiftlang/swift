//===--- AccessUtils.swift - Utilities for analyzing memory accesses ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This file provides a set of utilities for analyzing memory accesses.
// It defines the following concepts
// - `AccessBase`: represents the base address of a memory access.
// - `AccessPath`: a pair of an `AccessBase` and `SmallProjectionPath` with the
//   the path describing the specific address (in terms of projections) of the
//   access.
// - Access storage path (which is of type `ProjectedValue`): identifies the
//   reference (or a value which contains a reference) an address originates from.
//
// The snippet below shows the relationship between the access concepts.
// ```
// %ref = struct_extract %value, #f1                                        access storage path
// %base = ref_element_addr %ref, #f2         AccessBase       AccessPath            |
// %scope = begin_access %base                AccessScope          |                 |
// %t = tuple_element_addr %scope, 0                               |                 |
// %s = struct_element_addr %t, #f3                                v                 v
// %l = load %s                               the access
// ```
//===----------------------------------------------------------------------===//

/// AccessBase describes the base address of a memory access (e.g. of a `load` or `store``).
/// The "base address" is defined as the address which is obtained from the access address by
/// looking through all address projections.
/// This means that the base address is either the same as the access address or:
/// the access address is a chain of address projections of the base address.
/// The following snippets show examples of memory accesses and their respective bases.
///
/// ```
/// %base1 = ref_element_addr %ref, #Obj.field // A `class` base
/// %base2 = alloc_stack $S                    // A `stack` base
/// %base3 = global_addr @gaddr                // A `global` base
/// %addr1 = struct_element_addr %base1
/// %access1 = store %v1 to [trivial] %addr1   // accessed address is offset from base
/// %access2 = store %v2 to [trivial] %base2   // accessed address is base itself
/// ```
///
/// The base address is never inside an access scope.
public enum AccessBase : CustomStringConvertible, Hashable {

  /// The address of a boxed variable, i.e. a field of an `alloc_box`.
  case box(ProjectBoxInst)
  
  /// The address of a stack-allocated value, i.e. an `alloc_stack`
  case stack(AllocStackInst)
  
  /// The address of a global variable.
  ///
  /// TODO: make this payload the global address. Make AccessBase.address non-optional. Make AccessBase comparison see
  /// though things like project_box and global_addr. Then cleanup APIs like LifetimeDependence.Scope that carry extra
  /// address values around.
  case global(GlobalVariable)
  
  /// The address of a stored property of a class instance.
  case `class`(RefElementAddrInst)
  
  /// The base address of the tail allocated elements of a class instance.
  case tail(RefTailAddrInst)

  /// An indirect function argument, like `@inout`.
  case argument(FunctionArgument)
  
  /// An indirect result of a `begin_apply`.
  case yield(MultipleValueInstructionResult)

  /// store_borrow is never the base of a formal access, but calling Value.enclosingScope on an arbitrary address will
  /// return it as the accessBase. A store_borrow always stores into an alloc_stack, but it is handled separately
  /// because it may be useful for clients to know which value was stored in the temporary stack location for the
  /// duration of this borrow scope.
  case storeBorrow(StoreBorrowInst)

  /// An address which is derived from a `Builtin.RawPointer`.
  case pointer(PointerToAddressInst)

  // The result of an `index_addr` with a non-constant index.
  // This can only occur in access paths returned by `Value.constantAccessPath`.
  // In "regular" access paths such `index_addr` projections are contained in the `projectionPath` (`i*`).
  case index(IndexAddrInst)

  /// The access base is some SIL pattern which does not fit into any other case.
  /// This should be a very rare situation.
  ///
  /// TODO: unidentified should preserve its base address value, but AccessBase must be Hashable.
  case unidentified

  public init(baseAddress: Value) {
    switch baseAddress {
    case let rea as RefElementAddrInst   : self = .class(rea)
    case let rta as RefTailAddrInst      : self = .tail(rta)
    case let pbi as ProjectBoxInst       : self = .box(pbi)
    case let asi as AllocStackInst       : self = .stack(asi)
    case let arg as FunctionArgument     : self = .argument(arg)
    case let ga as GlobalAddrInst        : self = .global(ga.global)
    case let mvr as MultipleValueInstructionResult:
      if mvr.parentInstruction is BeginApplyInst && baseAddress.type.isAddress {
        self = .yield(mvr)
      } else {
        self = .unidentified
      }
    case let sb as StoreBorrowInst:
      self = .storeBorrow(sb)
    case let p2a as PointerToAddressInst:
      if let global = p2a.resultOfGlobalAddressorCall {
        self = .global(global)
      } else {
        self = .pointer(p2a)
      }
    default:
      self = .unidentified
    }
  }

  /// Return 'nil' for global variables and unidentified addresses.
  public var address: Value? {
    switch self {
      case .global, .unidentified: return nil
      case .box(let pbi):        return pbi
      case .stack(let asi):      return asi
      case .class(let rea):      return rea
      case .tail(let rta):       return rta
      case .argument(let arg):   return arg
      case .yield(let result):   return result
      case .storeBorrow(let sb): return sb
      case .pointer(let p):      return p
      case .index(let ia):       return ia
    }
  }

  public var description: String {
    switch self {
      case .unidentified:      return "?"
      case .box(let pbi):      return "box - \(pbi)"
      case .stack(let asi):    return "stack - \(asi)"
      case .global(let gl):    return "global - @\(gl.name)"
      case .class(let rea):    return "class  - \(rea)"
      case .tail(let rta):     return "tail - \(rta.instance)"
      case .argument(let arg): return "argument - \(arg)"
      case .yield(let result): return "yield - \(result)"
      case .storeBorrow(let sb): return "storeBorrow - \(sb)"
      case .pointer(let p):    return "pointer - \(p)"
      case .index(let ia):     return "index - \(ia)"
    }
  }

  /// True, if this is an access to a class instance.
  public var isObjectAccess: Bool {
    switch self {
      case .class, .tail:
        return true
      case .box, .stack, .global, .argument, .yield, .storeBorrow, .pointer, .index, .unidentified:
        return false
    }
  }

  /// The reference value if this is an access to a referenced object (class, box, tail).
  public var reference: Value? {
    switch self {
      case .box(let pbi):      return pbi.box
      case .class(let rea):    return rea.instance
      case .tail(let rta):     return rta.instance
      case .stack, .global, .argument, .yield, .storeBorrow, .pointer, .index, .unidentified:
        return nil
    }
  }

  /// True if this access base may be derived from a reference that is only valid within a locally
  /// scoped OSSA lifetime. For example:
  ///
  ///   %reference = begin_borrow %1
  ///   %base = ref_tail_addr %reference     <- %base must not be used outside the borrow scope
  ///   end_borrow %reference
  ///
  /// This is not true for scoped storage such as alloc_stack and @in arguments.
  ///
  public var hasLocalOwnershipLifetime: Bool {
    if let reference = reference {
      // Conservatively assume that everything which is a ref-counted object is within an ownership scope.
      // TODO: we could e.g. exclude guaranteed function arguments.
      return reference.ownership != .none
    }
    return false
  }

  /// True, if the baseAddress is of an immutable property or global variable
  public var isLet: Bool {
    switch self {
      case .class(let rea):    return rea.fieldIsLet
      case .global(let g):     return g.isLet
      case .box, .stack, .tail, .argument, .yield, .storeBorrow, .pointer, .index, .unidentified:
        return false
    }
  }

  /// True, if the address is produced by an allocation in its function.
  public var isLocal: Bool {
    switch self {
    case .box(let pbi):      return pbi.box.referenceRoot is AllocBoxInst
    case .class(let rea):    return rea.instance.referenceRoot is AllocRefInstBase
    case .tail(let rta):     return rta.instance.referenceRoot is AllocRefInstBase
    case .stack, .storeBorrow: return true
    case .global, .argument, .yield, .pointer, .index, .unidentified:
      return false
    }
  }

  /// True, if the kind of storage of the access is known (e.g. a class property, or global variable).
  public var hasKnownStorageKind: Bool {
    switch self {
      case .box, .class, .tail, .stack, .storeBorrow, .global:
        return true
      case .argument, .yield, .pointer, .index, .unidentified:
        return false
    }
  }

  public var storageIsLexical: Bool {
    switch self {
    case .argument(let arg):
      return arg.isLexical
    case .stack(let allocStack):
      return allocStack.isLexical
    case .global:
      return true
    case .box, .class, .tail:
      return reference!.referenceRoot.isLexical
    case .yield, .pointer, .index, .storeBorrow, .unidentified:
      return false
    }
  }

  /// Returns true if it's guaranteed that this access has the same base address as the `other` access.
  ///
  /// `isEqual` abstracts away the projection instructions that are included as part of the AccessBase:
  /// multiple `project_box` and `ref_element_addr` instructions are equivalent bases as long as they
  /// refer to the same variable or class property.
  public func isEqual(to other: AccessBase) -> Bool {
    switch (self, other) {
    case (.box(let pb1), .box(let pb2)):
      return pb1.box.referenceRoot == pb2.box.referenceRoot
    case (.class(let rea1), .class(let rea2)):
      return rea1.fieldIndex == rea2.fieldIndex &&
             rea1.instance.referenceRoot == rea2.instance.referenceRoot
    case (.tail(let rta1), .tail(let rta2)):
      return rta1.instance.referenceRoot == rta2.instance.referenceRoot &&
             rta1.type == rta2.type
    case (.stack(let as1), .stack(let as2)):
      return as1 == as2
    case (.global(let gl1), .global(let gl2)):
      return gl1 == gl2
    case (.argument(let arg1), .argument(let arg2)):
      return arg1 == arg2
    case (.yield(let baResult1), .yield(let baResult2)):
      return baResult1 == baResult2
    case (.storeBorrow(let sb1), .storeBorrow(let sb2)):
      return sb1 == sb2
    case (.pointer(let p1), .pointer(let p2)):
      return p1 == p2
    case (.index(let ia1), .index(let ia2)):
      return ia1 == ia2
    default:
      return false
    }
  }

  /// Returns `true` if the two access bases do not alias.
  public func isDistinct(from other: AccessBase) -> Bool {

    func isDifferentAllocation(_ lhs: Value, _ rhs: Value) -> Bool {
      switch (lhs, rhs) {
      case (is Allocation, is Allocation):
        return lhs != rhs
      case (is Allocation, is FunctionArgument),
           (is FunctionArgument, is Allocation):
        // A local allocation cannot alias with something passed to the function.
        return true
      default:
        return false
      }
    }

    func hasDifferentType(_ lhs: Value, _ rhs: Value) -> Bool {
      return lhs.type != rhs.type &&
             // If the types have unbound generic arguments then we don't know
             // the possible range of the type. A type such as $Array<Int> may
             // alias $Array<T>.  Right now we are conservative and we assume
             // that $UnsafeMutablePointer<T> and $Int may alias.
             !lhs.type.hasArchetype && !rhs.type.hasArchetype
    }

    func argIsDistinct(_ arg: FunctionArgument, from other: AccessBase) -> Bool {
      if arg.convention.isExclusiveIndirect {
        // Exclusive indirect arguments cannot alias with an address for which we know that it
        // is not derived from that argument (which might be the case for `pointer` and `yield`).
        return other.hasKnownStorageKind
      }
      // Non-exclusive argument still cannot alias with anything allocated locally in the function.
      return other.isLocal
    }

    switch (self, other) {
    
    // First handle all pairs of the same kind (except `yield`, `pointer` and `index`).
    case (.box(let pb), .box(let otherPb)):
      return pb.fieldIndex != otherPb.fieldIndex ||
        isDifferentAllocation(pb.box.referenceRoot, otherPb.box.referenceRoot) ||
        hasDifferentType(pb.box, otherPb.box)
    case (.stack(let asi), .stack(let otherAsi)):
      return asi != otherAsi
    case (.global(let global), .global(let otherGlobal)):
      return global != otherGlobal
    case (.class(let rea), .class(let otherRea)):
      return rea.fieldIndex != otherRea.fieldIndex ||
             isDifferentAllocation(rea.instance, otherRea.instance) ||
             hasDifferentType(rea.instance, otherRea.instance)
    case (.tail(let rta), .tail(let otherRta)):
      return isDifferentAllocation(rta.instance, otherRta.instance) ||
             hasDifferentType(rta.instance, otherRta.instance)
    case (.argument(let arg), .argument(let otherArg)):
      return (arg.convention.isExclusiveIndirect || otherArg.convention.isExclusiveIndirect) && arg != otherArg
      
    // Handle arguments vs non-arguments
    case (.argument(let arg), _):
      return argIsDistinct(arg, from: other)
    case (_, .argument(let otherArg)):
      return argIsDistinct(otherArg, from: self)

    case (.storeBorrow(let arg), .storeBorrow(let otherArg)):
      return arg.allocStack != otherArg.allocStack

    // Handle the special case of store_borrow - alloc_stack, because that would give a false result in the default case.
    case (.storeBorrow(let sbi), .stack(let asi)):
      return sbi.allocStack != asi
    case (.stack(let asi), .storeBorrow(let sbi)):
      return sbi.allocStack != asi

    default:
      // As we already handled pairs of the same kind, here we handle pairs with different kinds.
      // Different storage kinds cannot alias, regardless where the storage comes from.
      // E.g. a class property address cannot alias with a global variable address.
      return hasKnownStorageKind && other.hasKnownStorageKind
    }
  }
}

/// An `AccessPath` is a pair of a `base: AccessBase` and a `projectionPath: Path`
/// which denotes the offset of the access from the base in terms of projections.
public struct AccessPath : CustomStringConvertible, Hashable {
  public let base: AccessBase

  /// address projections only
  public let projectionPath: SmallProjectionPath

  public static func unidentified() -> AccessPath {
    return AccessPath(base: .unidentified, projectionPath: SmallProjectionPath())
  }

  public var description: String {
    "\(projectionPath): \(base)"
  }

  public func isDistinct(from other: AccessPath) -> Bool {
    if base.isDistinct(from: other.base) {
      // We can already derived from the bases that there is no alias.
      // No need to look at the projection paths.
      return true
    }
    if base == other.base ||
       (base.hasKnownStorageKind && other.base.hasKnownStorageKind) {
      if !projectionPath.mayOverlap(with: other.projectionPath) {
        return true
      }
    }
    return false
  }

  /// Returns true if this access addresses the same memory location as `other` or if `other`
  /// is a sub-field of this access.

  /// Note that this access _contains_ `other` if `other` has a _larger_ projection path than this access.
  /// For example:
  ///   `%value.s0` contains `%value.s0.s1`
  public func isEqualOrContains(_ other: AccessPath) -> Bool {
    return getProjection(to: other) != nil
  }

  public var materializableProjectionPath: SmallProjectionPath? {
    if projectionPath.isMaterializable {
      return projectionPath
    }
    return nil
  }

  /// Returns the projection path to `other` if this access path is equal or contains `other`.
  ///
  /// For example,
  ///   `%value.s0`.getProjection(to: `%value.s0.s1`)
  /// yields
  ///   `s1`
  public func getProjection(to other: AccessPath) -> SmallProjectionPath? {
    if !base.isEqual(to: other.base) {
      return nil
    }
    if let resultPath = projectionPath.subtract(from: other.projectionPath),
       // Indexing is not a projection where the base overlaps the projected address.
       !resultPath.pop().kind.isIndexedElement
    {
      return resultPath
    }
    return nil
  }

  /// Like `getProjection`, but also requires that the resulting projection path is materializable.
  public func getMaterializableProjection(to other: AccessPath) -> SmallProjectionPath? {
    if let projectionPath = getProjection(to: other),
       projectionPath.isMaterializable {
      return projectionPath
    }
    return nil
  }
}

private func canBeOperandOfIndexAddr(_ value: Value) -> Bool {
  switch value {
  case is IndexAddrInst, is RefTailAddrInst, is PointerToAddressInst, is VectorBaseAddrInst:
    return true
  default:
    return false
  }
}

/// Tries to identify from which address the pointer operand originates from.
/// This is useful to identify patterns like
/// ```
/// %orig_addr = global_addr @...
/// %ptr = address_to_pointer %orig_addr
/// %addr = pointer_to_address %ptr
/// ```
public extension PointerToAddressInst {
  var originatingAddress: Value? {

    struct Walker : ValueUseDefWalker {
      let addrType: Type
      var result: Value?
      var walkUpCache = WalkerCache<Path>()

      mutating func rootDef(value: Value, path: SmallProjectionPath) -> WalkResult {
        if let atp = value as? AddressToPointerInst {
          if let res = result, atp.address != res {
            return .abortWalk
          }

          if addrType != atp.address.type { return .abortWalk }
          if !path.isEmpty { return .abortWalk }

          self.result = atp.address
          return .continueWalk
        }
        return .abortWalk
      }

      mutating func walkUp(value: Value, path: SmallProjectionPath) -> WalkResult {
        switch value {
        case is Argument, is MarkDependenceInst, is CopyValueInst,
             is StructExtractInst, is TupleExtractInst, is StructInst, is TupleInst, is AddressToPointerInst:
          return walkUpDefault(value: value, path: path)
        default:
          return .abortWalk
        }
      }
    }

    var walker = Walker(addrType: type)
    if walker.walkUp(value: pointer, path: SmallProjectionPath()) == .abortWalk {
      return nil
    }
    return walker.result
  }

  var resultOfGlobalAddressorCall: GlobalVariable? {
    if isStrict,
       let apply = pointer as? ApplyInst,
       let callee = apply.referencedFunction,
       let global = callee.globalOfGlobalInitFunction
    {
      return global
    }
    return nil
  }
}

/// TODO: migrate AccessBase to use this instead of GlobalVariable because many utilities need to get back to a
/// representative SIL Value.
public enum GlobalAccessBase {
  case global(GlobalAddrInst)
  case initializer(PointerToAddressInst)

  public init?(address: Value) {
    switch address {
    case let ga as GlobalAddrInst:
      self = .global(ga)
    case let p2a as PointerToAddressInst where p2a.resultOfGlobalAddressorCall != nil:
      self = .initializer(p2a)
    default:
      return nil
    }
  }

  public var address: Value {
    switch self {
      case let .global(ga):
        return ga
      case let .initializer(p2a):
        return p2a
    }
  }
}

/// The `EnclosingAccessScope` of an access is the innermost `begin_access`
/// instruction that checks for exclusivity of the access.
/// If there is no `begin_access` instruction found, then the scope is
/// the base itself.
///
/// The access scopes for the snippet below are:
///   (l1, .base(%addr)), (l2, .scope(%a2)), (l3, .scope(%a3))
///
/// ````
/// %addr = ... : $*Int64
/// %l1 = load %addr : $*Int64
/// %a1 = begin_access [read] [dynamic] %addr : $*Int64
/// %a2 = begin_access [read] [dynamic] %addr : $*Int64
/// %l2   = load %a2 : $*Int64
/// end_access %a2 : $*Int64
/// end_access %a1 : $*Int64
/// %a3 = begin_access [read] [dynamic] [no_nested_conflict] %addr : $*Int64
/// %l3 = load %a3 : $*Int64
/// end_access %a3 : $*Int64
/// ```
public enum EnclosingAccessScope {
  case access(BeginAccessInst)
  case base(AccessBase)
  case dependence(MarkDependenceInst)

  // TODO: make this non-optional after fixing AccessBase.global.
  public var address: Value? {
    switch self {
    case let .access(beginAccess):
      return beginAccess
    case let .base(accessBase):
      return accessBase.address
    case let .dependence(markDep):
      return markDep
    }
  }
}

// An AccessBase with the nested enclosing scopes that contain the original address in bottom-up order.
public struct AccessBaseAndScopes {
  public let base: AccessBase
  public let scopes: SingleInlineArray<EnclosingAccessScope>

  public init(base: AccessBase, scopes: SingleInlineArray<EnclosingAccessScope>) {
    self.base = base
    self.scopes = scopes
  }

  public var enclosingAccess: EnclosingAccessScope {
    return scopes.first ?? .base(base)
  }

  public var innermostAccess: BeginAccessInst? {
    for scope in scopes {
      if case let .access(beginAccess) = scope {
        return beginAccess
      }
    }
    return nil
  }
}

extension AccessBaseAndScopes {
  // This must return false if a mark_dependence scope is present.
  public var isOnlyReadAccess: Bool {
    scopes.allSatisfy(
      {
        if case let .access(beginAccess) = $0 {
          return beginAccess.accessKind == .read
        }
        // preserve any dependence scopes.
        return false
      })
  }
}

extension BeginAccessInst {
  // Recognize an access scope for a unsafe addressor:
  // %adr = pointer_to_address
  // %md = mark_dependence %adr
  // begin_access [unsafe] %md
  public var unsafeAddressorSelf: Value? {
    guard self.isUnsafe else {
      return nil
    }
    switch self.address.enclosingAccessScope {
    case .access, .base:
      return nil
    case let .dependence(markDep):
      switch markDep.value.enclosingAccessScope {
      case .access, .dependence:
        return nil
      case let .base(accessBase):
        guard case .pointer = accessBase else {
          return nil
        }
        return markDep.base
      }
    }
  }
}

private struct EnclosingAccessWalker : AddressUseDefWalker {
  var enclosingScope: EnclosingAccessScope?

  mutating func walk(startAt address: Value, initialPath: UnusedWalkingPath = UnusedWalkingPath()) {
    if walkUp(address: address, path: UnusedWalkingPath()) == .abortWalk {
      assert(enclosingScope == nil, "shouldn't have set an enclosing scope in an aborted walk")
    }
  }

  mutating func rootDef(address: Value, path: UnusedWalkingPath) -> WalkResult {
    assert(enclosingScope == nil, "rootDef should only called once")
    // Try identifying the address a pointer originates from
    if let p2ai = address as? PointerToAddressInst, let originatingAddr = p2ai.originatingAddress {
      return walkUp(address: originatingAddr, path: path)
    }
    enclosingScope = .base(AccessBase(baseAddress: address))
    return .continueWalk
  }

  mutating func walkUp(address: Value, path: UnusedWalkingPath) -> WalkResult {
    switch address {
    case let ba as BeginAccessInst:
      enclosingScope = .access(ba)
      return .continueWalk
    case let md as MarkDependenceInst:
      enclosingScope = .dependence(md)
      return .continueWalk
    default:
      return walkUpDefault(address: address, path: path)
    }
  }
}

private struct AccessPathWalker : AddressUseDefWalker {
  var result = AccessPath.unidentified()

  // List of nested BeginAccessInst & MarkDependenceInst: inside-out order.
  var foundEnclosingScopes = SingleInlineArray<EnclosingAccessScope>()

  let enforceConstantProjectionPath: Bool

  init(enforceConstantProjectionPath: Bool = false) {
    self.enforceConstantProjectionPath = enforceConstantProjectionPath
  }

  mutating func walk(startAt address: Value, initialPath: SmallProjectionPath = SmallProjectionPath()) {
    if walkUp(address: address, path: Path(projectionPath: initialPath)) == .abortWalk {
      assert(result.base == .unidentified,
             "shouldn't have set an access base in an aborted walk")
    }
  }

  struct Path : SmallProjectionWalkingPath {
    let projectionPath: SmallProjectionPath

    // Tracks whether an `index_addr` instruction was crossed.
    // It should be (FIXME: check if it's enforced) that operands
    // of `index_addr` must be `tail_addr` or other `index_addr` results.
    let indexAddr: Bool

    init(projectionPath: SmallProjectionPath = SmallProjectionPath(), indexAddr: Bool = false) {
      self.projectionPath = projectionPath
      self.indexAddr = indexAddr
    }

    func with(projectionPath: SmallProjectionPath) -> Self {
      return Self(projectionPath: projectionPath, indexAddr: indexAddr)
    }

    func with(indexAddr: Bool) -> Self {
      return Self(projectionPath: projectionPath, indexAddr: indexAddr)
    }

    func merge(with other: Self) -> Self {
      return Self(
        projectionPath: projectionPath.merge(with: other.projectionPath),
        indexAddr: indexAddr || other.indexAddr
      )
    }
  }

  mutating func rootDef(address: Value, path: Path) -> WalkResult {
    assert(result.base == .unidentified, "rootDef should only called once")
    // Try identifying the address a pointer originates from
    if let p2ai = address as? PointerToAddressInst, let originatingAddr = p2ai.originatingAddress {
      return walkUp(address: originatingAddr, path: path)
    }
    let base = AccessBase(baseAddress: address)
    self.result = AccessPath(base: base, projectionPath: path.projectionPath)
    return .continueWalk
  }

  mutating func walkUp(address: Value, path: Path) -> WalkResult {
    if let indexAddr = address as? IndexAddrInst {
      if !(indexAddr.index is IntegerLiteralInst) && enforceConstantProjectionPath {
        self.result = AccessPath(base: .index(indexAddr), projectionPath: path.projectionPath)
        return .continueWalk
      }
      // Track that we crossed an `index_addr` during the walk-up
      return walkUpDefault(address: indexAddr, path: path.with(indexAddr: true))
    } else if path.indexAddr && !canBeOperandOfIndexAddr(address) {
      // An `index_addr` instruction cannot be derived from an address
      // projection. Bail out
      return .abortWalk
    } else if let ba = address as? BeginAccessInst {
      foundEnclosingScopes.push(.access(ba))
    } else if let md = address as? MarkDependenceInst {
      foundEnclosingScopes.push(.dependence(md))
    }
    return walkUpDefault(address: address, path: path.with(indexAddr: false))
  }
}

extension Value {
  // Convenient properties to avoid instantiating an explicit AccessPathWalker.
  //
  // Although an AccessPathWalker is created for each call of these properties,
  // it's very unlikely that this will end up in memory allocations.
  // Only in the rare case of `pointer_to_address` -> `address_to_pointer` pairs, which
  // go through phi-arguments, the AccessPathWalker will allocate memory in its cache.

  /// Computes the access base of this address value.
  public var accessBase: AccessBase { accessPath.base }

  /// Computes the access path of this address value.
  public var accessPath: AccessPath {
    var walker = AccessPathWalker()
    walker.walk(startAt: self)
    return walker.result
  }

  /// Like `accessPath`, but ensures that the projectionPath only contains "constant" elements.
  /// This means: if the access contains an `index_addr` projection with a non-constant index,
  /// the `projectionPath` does _not_ contain the `index_addr`.
  /// Instead, the `base` is an `AccessBase.index` which refers to the `index_addr`.
  /// For example:
  /// ```
  ///    %1 = ref_tail_addr %some_reference
  ///    %2 = index_addr %1, %some_non_const_value
  ///    %3 = struct_element_addr %2, #field2
  /// ```
  /// `%3.accessPath`         = base: tail(`%1`),  projectionPath: `i*.s2`
  /// `%3.constantAccessPath` = base: index(`%2`), projectionPath: `s2`
  ///
  public var constantAccessPath: AccessPath {
    var walker = AccessPathWalker(enforceConstantProjectionPath: true)
    walker.walk(startAt: self)
    return walker.result
  }

  public func getAccessPath(fromInitialPath: SmallProjectionPath) -> AccessPath {
    var walker = AccessPathWalker()
    walker.walk(startAt: self, initialPath: fromInitialPath)
    return walker.result
  }

  /// Computes the access path of this address value and also returns the scope.
  public var accessPathWithScope: (AccessPath, scope: BeginAccessInst?) {
    var walker = AccessPathWalker()
    walker.walk(startAt: self)
    let baseAndScopes = AccessBaseAndScopes(base: walker.result.base, scopes: walker.foundEnclosingScopes)
    return (walker.result, baseAndScopes.innermostAccess)
  }

  /// Computes the enclosing access scope of this address value.
  public var enclosingAccessScope: EnclosingAccessScope {
    var walker = EnclosingAccessWalker()
    walker.walk(startAt: self)
    return walker.enclosingScope ?? .base(.unidentified)
  }

  public var accessBaseWithScopes: AccessBaseAndScopes {
    var walker = AccessPathWalker()
    walker.walk(startAt: self)
    return AccessBaseAndScopes(base: walker.result.base, scopes: walker.foundEnclosingScopes)
  }

  /// The root definition of a reference, obtained by skipping ownership forwarding and ownership transition.
  public var referenceRoot: Value {
    var value: Value = self
    while true {
      if let forward = value.forwardingInstruction, forward.preservesIdentity,
         let operand = forward.singleForwardedOperand {
        value = operand.value
        continue
      }
      if let transition = value.definingInstruction as? OwnershipTransitionInstruction {
        value = transition.operand.value
        continue
      }
      return value
    }
  }
}

/// A ValueUseDef walker that that visits access storage paths of an address.
///
/// An access storage path is the reference (or a value which contains a reference)
/// an address originates from.
/// In the following example the `storage` is `contains_ref` with `path` `"s0.c0.s0"`
/// ```
///   %ref = struct_extract %contains_ref : $S, #S.l
///   %base = ref_element_addr %ref : $List, #List.x
///   %addr = struct_element_addr %base : $X, #X.e
///   store %v to [trivial] %addr : $*Int
/// ```
///
/// Warning: This does not find the correct storage root of the
/// lifetime of an object projection, such as .box or .class because
/// ValueUseDefWalker ignores ownership and, for example, walks past copies.
extension ValueUseDefWalker where Path == SmallProjectionPath {
  /// The main entry point.
  /// Given an `accessPath` where the access base is a reference (class, tail, box), call
  /// the `visit` function for all storage roots with a the corresponding path.
  /// Returns true on success.
  /// Returns false if not all storage roots could be identified or if `accessPath` has not a "reference" base.
  public mutating func visitAccessStorageRoots(of accessPath: AccessPath) -> Bool {
    walkUpCache.clear()
    let path = accessPath.projectionPath
    switch accessPath.base {
      case .box(let pbi):
        return walkUp(value: pbi.box, path: path.push(.classField, index: pbi.fieldIndex)) != .abortWalk
      case .class(let rea):
        return walkUp(value: rea.instance, path: path.push(.classField, index: rea.fieldIndex)) != .abortWalk
      case .tail(let rta):
        return walkUp(value: rta.instance, path: path.push(.tailElements, index: 0)) != .abortWalk
      case .stack, .global, .argument, .yield, .storeBorrow, .pointer, .index, .unidentified:
        return false
    }
  }
}

extension Function {
  public var globalOfGlobalInitFunction: GlobalVariable? {
    if isGlobalInitFunction,
       let ret = returnInstruction,
       let atp = ret.returnedValue as? AddressToPointerInst,
       let ga = atp.address as? GlobalAddrInst {
      return ga.global
    }
    return nil
  }
}
