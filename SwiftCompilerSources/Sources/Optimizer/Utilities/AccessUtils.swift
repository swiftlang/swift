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
// - `AccessStoragePath`: identifies the reference (or a value which contains a
//   reference) an address originates from.
//
// The snippet below shows the relationship between the access concepts.
// ```
// %ref = struct_extract %value, #f1                                         AccessStoragePath
// %base = ref_element_addr %ref, #f2         AccessBase       AccessPath            |
// %scope = begin_access %base                AccessScope          |                 |
// %t = tuple_element_addr %scope, 0                               |                 |
// %s = struct_element_addr %t, #f3                                v                 v
// %l = load %s                               the access
// ```
//===----------------------------------------------------------------------===//
import SIL

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
enum AccessBase : CustomStringConvertible, Hashable {

  /// The address of a boxed variable, i.e. a field of an `alloc_box`.
  case box(ProjectBoxInst)
  
  /// The address of a stack-allocated value, i.e. an `alloc_stack`
  case stack(AllocStackInst)
  
  /// The address of a global variable.
  case global(GlobalVariable)
  
  /// The address of a stored property of a class instance.
  case `class`(RefElementAddrInst)
  
  /// The base address of the tail allocated elements of a class instance.
  case tail(RefTailAddrInst)

  /// An indirect function argument, like `@inout`.
  case argument(FunctionArgument)
  
  /// An indirect result of a `begin_apply`.
  case yield(BeginApplyInst)

  /// An address which is derived from a `Builtin.RawPointer`.
  case pointer(PointerToAddressInst)

  init?(baseAddress: Value) {
    switch baseAddress {
    case let rea as RefElementAddrInst   : self = .class(rea)
    case let rta as RefTailAddrInst      : self = .tail(rta)
    case let pbi as ProjectBoxInst       : self = .box(pbi)
    case let asi as AllocStackInst       : self = .stack(asi)
    case let arg as FunctionArgument     : self = .argument(arg)
    case let ga as GlobalAddrInst        : self = .global(ga.global)
    case let mvr as MultipleValueInstructionResult:
      if let ba = mvr.instruction as? BeginApplyInst, baseAddress.type.isAddress {
        self = .yield(ba)
      } else {
        return nil
      }
    default:
      return nil
    }
  }

  var description: String {
    switch self {
      case .box(let pbi):      return "box - \(pbi)"
      case .stack(let asi):    return "stack - \(asi)"
      case .global(let gl):    return "global - @\(gl.name)"
      case .class(let rea):    return "class  - \(rea)"
      case .tail(let rta):     return "tail - \(rta.operand)"
      case .argument(let arg): return "argument - \(arg)"
      case .yield(let ba):     return "yield - \(ba)"
      case .pointer(let p):    return "pointer - \(p)"
    }
  }

  /// True, if this is an access to a class instance.
  var isObjectAccess: Bool {
    switch self {
      case .class, .tail:
        return true
      case .box, .stack, .global, .argument, .yield, .pointer:
        return false
    }
  }

  /// The reference value if this is an access to a referenced objecct (class, box, tail).
  var reference: Value? {
    switch self {
      case .box(let pbi):      return pbi.operand
      case .class(let rea):    return rea.operand
      case .tail(let rta):     return rta.operand
      case .stack, .global, .argument, .yield, .pointer:
        return nil
    }
  }
  /// True, if the baseAddress is of an immutable property or global variable
  var isLet: Bool {
    switch self {
      case .class(let rea):    return rea.fieldIsLet
      case .global(let g):     return g.isLet
      case .box, .stack, .tail, .argument, .yield, .pointer:
        return false
    }
  }

  /// True, if the address is immediately produced by an allocation in its function.
  var isLocal: Bool {
    switch self {
      case .box(let pbi):      return pbi.operand is AllocBoxInst
      case .class(let rea):    return rea.operand is AllocRefInstBase
      case .tail(let rta):     return rta.operand is AllocRefInstBase
      case .stack:             return true
      case .global, .argument, .yield, .pointer:
        return false
    }
  }

  /// True, if the kind of storage of the access is known (e.g. a class property, or global variable).
  var hasKnownStorageKind: Bool {
    switch self {
      case .box, .class, .tail, .stack, .global:
        return true
      case .argument, .yield, .pointer:
        return false
    }
  }

  /// Returns `true` if the two access bases do not alias.
  func isDistinct(from other: AccessBase) -> Bool {
  
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

    func argIsDistinct(_ arg: FunctionArgument, from other: AccessBase) -> Bool {
      if arg.isExclusiveIndirectParameter {
        // Exclusive indirect arguments cannot alias with an address for which we know that it
        // is not derived from that argument (which might be the case for `pointer` and `yield`).
        return other.hasKnownStorageKind
      }
      // Non-exclusive argument still cannot alias with anything allocated locally in the function.
      return other.isLocal
    }

    switch (self, other) {
    
    // First handle all pairs of the same kind (except `yield` and `pointer`).
    case (.box(let pb), .box(let otherPb)):
      return pb.fieldIndex != otherPb.fieldIndex ||
             isDifferentAllocation(pb.operand, otherPb.operand)
    case (.stack(let asi), .stack(let otherAsi)):
      return asi != otherAsi
    case (.global(let global), .global(let otherGlobal)):
      return global != otherGlobal
    case (.class(let rea), .class(let otherRea)):
      return rea.fieldIndex != otherRea.fieldIndex ||
             isDifferentAllocation(rea.operand, otherRea.operand)
    case (.tail(let rta), .tail(let otherRta)):
      return isDifferentAllocation(rta.operand, otherRta.operand)
    case (.argument(let arg), .argument(let otherArg)):
      return (arg.isExclusiveIndirectParameter || otherArg.isExclusiveIndirectParameter) && arg != otherArg
      
    // Handle arguments vs non-arguments
    case (.argument(let arg), _):
      return argIsDistinct(arg, from: other)
    case (_, .argument(let otherArg)):
      return argIsDistinct(otherArg, from: self)

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
struct AccessPath : CustomStringConvertible {
  let base: AccessBase

  /// address projections only
  let projectionPath: SmallProjectionPath

  var description: String {
    "\(projectionPath): \(base)"
  }

  func isDistinct(from other: AccessPath) -> Bool {
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
}

/// An `AccessStoragePath` is the reference (or a value which contains a reference)
/// an address originates from.
/// In the following example the `storage` is `contains_ref` with `path` `"s0.c0.s0"`
/// ```
///   %ref = struct_extract %contains_ref : $S, #S.l
///   %base = ref_element_addr %ref : $List, #List.x
///   %addr = struct_element_addr %base : $X, #X.e
///   store %v to [trivial] %addr : $*Int
/// ```
struct AccessStoragePath {
  let storage: Value

  /// Only valid paths are: `"<sequence of value projections>.<one reference projection>.<sequence of address projections>"`
  let path: SmallProjectionPath
}

private func canBeOperandOfIndexAddr(_ value: Value) -> Bool {
  switch value {
  case is IndexAddrInst, is RefTailAddrInst, is PointerToAddressInst:
    return true
  default:
    return false
  }
}

/// Given a `%addr = pointer_to_address %ptr_operand` instruction tries to identify
/// where the pointer operand `ptr_operand` originates from.
/// This is useful to identify patterns like
/// ```
/// %orig_addr = global_addr @...
/// %ptr = address_to_pointer %orig_addr
/// %addr = pointer_to_address %ptr
/// ```
struct PointerIdentification {
  private var walker = PointerIdentificationUseDefWalker()

  mutating func getOriginatingAddress(of pointerToAddr: PointerToAddressInst) -> Value? {
    defer { walker.clear() }

    walker.start(pointerToAddr.type)
    if walker.walkUp(value: pointerToAddr.operand, path: SmallProjectionPath()) == .abortWalk {
      return nil
    }
    return walker.result
  }

  private struct PointerIdentificationUseDefWalker : ValueUseDefWalker {
    private var addrType: Type!
    private(set) var result: Value?
    var walkUpCache = WalkerCache<Path>()

    mutating func start(_ addrType: Type) {
      self.addrType = addrType
      assert(result == nil)
    }

    mutating func clear() {
      result = nil
      walkUpCache.clear()
    }

    mutating func rootDef(value: Value, path: SmallProjectionPath) -> WalkResult {
      if let atp = value as? AddressToPointerInst {
        if let res = result, atp.operand != res {
          return .abortWalk
        }

        if addrType != atp.operand.type { return .abortWalk }
        if !path.isEmpty { return .abortWalk }

        self.result = atp.operand
        return .continueWalk
      }
      return .abortWalk
    }

    mutating func walkUp(value: Value, path: SmallProjectionPath) -> WalkResult {
      switch value {
      case is BlockArgument, is MarkDependenceInst, is CopyValueInst,
           is StructExtractInst, is TupleExtractInst, is StructInst, is TupleInst,
           is FunctionArgument, is AddressToPointerInst:
        return walkUpDefault(value: value, path: path)
      default:
        return .abortWalk
      }
    }
  }
}

/// The `EnclosingScope` of an access is the innermost `begin_access`
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
enum EnclosingScope {
  case scope(BeginAccessInst)
  case base(AccessBase)
}

/// A walker utility that, given an address value, computes the `AccessPath`
/// of the access (the base address and the address projections to the accessed fields) and
/// the innermost enclosing scope (`begin_access`).
struct AccessPathWalker {
  mutating func getAccessPath(of address: Value) -> AccessPath? {
    assert(address.type.isAddress, "Expected address")
    walker.start()
    if walker.walkUp(address: address, path: Walker.Path()) == .abortWalk {
      return nil
    }
    return walker.result
  }

  mutating func getAccessPathWithScope(of address: Value) -> (AccessPath?, EnclosingScope?) {
    let ap = getAccessPath(of: address)
    return (ap, walker.scope)
  }

  mutating func getAccessBase(of address: Value) -> AccessBase? {
    getAccessPath(of: address)?.base
  }

  mutating func getAccessScope(of address: Value) -> EnclosingScope? {
    getAccessPathWithScope(of: address).1
  }

  private var walker = Walker()

  private struct Walker : AddressUseDefWalker {
    private(set) var result: AccessPath? = nil
    private var foundBeginAccess: BeginAccessInst? = nil
    private var pointerId = PointerIdentification()

    var scope: EnclosingScope? {
      if let ba = foundBeginAccess {
        return .scope(ba)
      } else {
        guard let accessPath = result else { return nil }
        return .base(accessPath.base)
      }
    }

    mutating func start() {
      result = nil
      foundBeginAccess = nil
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
      assert(result == nil, "rootDef should only called once")
      // Try identifying the address a pointer originates from
      if let p2ai = address as? PointerToAddressInst {
        if let originatingAddr = pointerId.getOriginatingAddress(of: p2ai) {
          return walkUp(address: originatingAddr, path: path)
        } else {
          self.result = AccessPath(base: .pointer(p2ai), projectionPath: path.projectionPath)
          return .continueWalk
        }
      }

      // If this is a base then we're done
      if let base = AccessBase(baseAddress: address) {
        self.result = AccessPath(base: base, projectionPath: path.projectionPath)
        return .continueWalk
      }

      // The base is unidentified
      self.result = nil
      return .abortWalk
    }

    mutating func walkUp(address: Value, path: Path) -> WalkResult {
      if address is IndexAddrInst {
        // Track that we crossed an `index_addr` during the walk-up
        return walkUpDefault(address: address, path: path.with(indexAddr: true))
      } else if path.indexAddr && !canBeOperandOfIndexAddr(address) {
        // An `index_addr` instruction cannot be derived from an address
        // projection. Bail out
        self.result = nil
        return .abortWalk
      } else if let ba = address as? BeginAccessInst, foundBeginAccess == nil {
        foundBeginAccess = ba
      }
      return walkUpDefault(address: address, path: path.with(indexAddr: false))
    }
  }
}

/// A ValueUseDef walker that identifies which values a reference of an access path might
/// originate from.
protocol AccessStoragePathWalker : ValueUseDefWalker where Path == SmallProjectionPath {
  mutating func visit(access: AccessStoragePath)
}

extension AccessStoragePathWalker {
  /// The main entry point.
  /// Given an `accessPath` where the access base is a reference (class, tail, box), call
  /// the `visit` function for all storage roots with a the corresponding path.
  /// Returns true on success.
  /// Returns false if not all storage roots could be identified or if `accessPath` has not a "reference" base.
  mutating func visitAccessStorageRoots(of accessPath: AccessPath) -> Bool {
    walkUpCache.clear()
    let path = accessPath.projectionPath
    switch accessPath.base {
      case .box(let pbi):
        return walkUp(value: pbi.operand, path: path.push(.classField, index: pbi.fieldIndex)) != .abortWalk
      case .class(let rea):
        return walkUp(value: rea.operand, path: path.push(.classField, index: rea.fieldIndex)) != .abortWalk
      case .tail(let rta):
        return walkUp(value: rta.operand, path: path.push(.tailElements, index: 0)) != .abortWalk
      case .stack, .global, .argument, .yield, .pointer:
        return false
    }
  }

  mutating func rootDef(value: Value, path: SmallProjectionPath) -> WalkResult {
    visit(access: AccessStoragePath(storage: value, path: path))
    return .continueWalk
  }
}
