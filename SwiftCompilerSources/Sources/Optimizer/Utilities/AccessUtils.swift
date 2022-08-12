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

/// "AccessBase describes the base address of a memory access (e.g. of a `load` or `store``).
/// The access address is either the base address directly or an address
/// projection of it.
/// The following snippets show examples of memory accesses and their respective bases.
///
/// ```
/// %base1 = ref_element_addr %ref, #Obj.field
/// %base2 = alloc_stack $S
/// %base3 = global_addr @gaddr
/// %addr1 = struct_element_addr %base1
/// %access1 = store %v1 to [trivial] %addr1   // accessed address is offset from base
/// %access2 = store %v2 to [trivial] %base2   // accessed address is base itself
/// ```
///
/// The base address is never inside an access scope.
struct AccessBase : CustomStringConvertible {

  public enum Kind {
    case box
    case stack
    case global
    case `class`
    case tail
    case argument
    case yield
    case pointer

    var isObject: Bool { self == .class || self == .tail }
  }

  let baseAddress: Value
  let kind: Kind

  init?(baseAddress: Value) {
    switch baseAddress {
    case is RefElementAddrInst   : kind = .class
    case is RefTailAddrInst      : kind = .tail
    case is ProjectBoxInst       : kind = .box
    case is AllocStackInst       : kind = .stack
    case is FunctionArgument     : kind = .argument
    case is GlobalAddrInst       : kind = .global
    default:
      if baseAddress.definingInstruction is BeginApplyInst &&
         baseAddress.type.isAddress {
        kind = .yield
      } else {
        return nil
      }
    }

    self.baseAddress = baseAddress
  }

  init(baseAddress: Value, kind: Kind) {
    self.baseAddress = baseAddress
    self.kind = kind
  }

  var description: String {
    "\(kind) - \(baseAddress)"
  }

  /// Returns `true` if this is an access to a class instance
  var isObjectAccess: Bool {
    return kind == .class || kind == .tail
  }

  /// Returns a value of reference type if this is a reference projection (class, box, tail)
  var reference: Value? {
    switch baseAddress {
    case let rea as RefElementAddrInst:
      return rea.operand
    case let pb as ProjectBoxInst:
      return pb.operand
    case let rta as RefTailAddrInst:
      return rta.operand
    default:
      return nil
    }
  }

  /// Returns `true` if the baseAddress is of an immutable property/global variable
  var isLet: Bool {
    switch baseAddress {
    case let rea as RefElementAddrInst:
      return rea.fieldIsLet
    case let ga as GlobalAddrInst:
      return ga.global.isLet
    default:
      return false
    }
  }

  /// Returns `true` if the address is immediately produced by a stack or box allocation
  var isLocal: Bool {
    switch kind {
    case .box:
      // The operand of the projection can be an argument, in which
      // case it wouldn't be local
      return (baseAddress as! ProjectBoxInst).operand is AllocBoxInst
    case .class:
      let op = (baseAddress as! RefElementAddrInst).operand
      return op is AllocRefInst || op is AllocRefDynamicInst
    case .stack:
      return true
    default:
      return false
    }
  }

  /// Returns `true` if we can reliably compare this `AccessBase`
  /// with another `AccessBase` for equality.
  /// When comparing two uniquely identified access bases and they are not equal,
  /// it follows that the accessed memory addresses do not alias.
  /// This is e.g. not the case for class references: two different references
  /// may still point to the same object.
  var isUniquelyIdentified: Bool {
    switch kind {
    case .box:
      // The operand `%op` in `%baseAddress = project_box %op` can
      // be `alloc_box` or an argument. Only if it's a fresh allocation it is
      // uniquelyIdentified, otherwise it is aliasable, as all the other references.
      return (baseAddress as! ProjectBoxInst).operand is AllocBoxInst
    case .stack, .global:
      return true
    case .argument:
      // An argument address that is non-aliasable
      return (baseAddress as! FunctionArgument).isExclusiveIndirectParameter
    case .class:
      let op = (baseAddress as! RefElementAddrInst).operand
      return op is AllocRefInst || op is AllocRefDynamicInst
    case .tail, .yield, .pointer:
      // References (.class and .tail) may alias, and so do pointers and
      // yield results
      return false
    }
  }

  /// Returns `true` if the two access bases do not alias
  func isDistinct(from other: AccessBase) -> Bool {
    switch (baseAddress, other.baseAddress) {
    case is (AllocStackInst, AllocStackInst):
      return baseAddress != other.baseAddress
    case let (this as ProjectBoxInst, that as ProjectBoxInst)
        where this.operand is AllocBoxInst && that.operand is AllocBoxInst:
      return this.operand != that.operand
    case let (this as GlobalAddrInst, that as GlobalAddrInst):
      return this.global != that.global
    case let (this as FunctionArgument, that as FunctionArgument):
      return (this.isExclusiveIndirectParameter || that.isExclusiveIndirectParameter) && this != that
    case let (this as RefElementAddrInst, that as RefElementAddrInst):
      return (this.fieldIndex != that.fieldIndex)
    default:
      let selfIsUniquelyIdentified = isUniquelyIdentified
      let otherIsUniquelyIdentified = other.isUniquelyIdentified
      if selfIsUniquelyIdentified && otherIsUniquelyIdentified && kind != other.kind { return true }
      // property: `isUniquelyIdentified` XOR `isObject`
      if selfIsUniquelyIdentified && other.kind.isObject { return true }
      if kind.isObject && otherIsUniquelyIdentified { return true }
      return false
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
    return
      base.isDistinct(from: other.base) ||                            // The base is distinct, in which case we are done. OR
      (base.baseAddress == other.base.baseAddress &&                  // (The base is the exact same AND
      !(projectionPath.matches(pattern: other.projectionPath)         //  the projection paths do not overlap)
        || (other.projectionPath.matches(pattern: projectionPath))))
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

enum AddressOrPointerArgument {
  case address(Value)
  case pointer(FunctionArgument)
}

extension AddressOrPointerArgument : Equatable {
  static func ==(lhs: AddressOrPointerArgument, rhs: AddressOrPointerArgument) -> Bool {
    switch (lhs, rhs) {
    case let (.address(left), .address(right)):
      return left == right
    case let (.pointer(left), .pointer(right)):
      return left == right
    default:
      return false
    }
  }
}

/// Given a `%addr = pointer_to_address %ptr_operand` instruction tries to identify
/// the address the pointer operand `ptr_operand` originates from, if any exists.
/// This is useful to identify patterns like
/// ```
/// %orig_addr = global_addr @...
/// %ptr = address_to_pointer %orig_addr
/// %addr = pointer_to_address %ptr
/// ```
/// which might arise when `[global_init]` functions for global addressors are inlined.
///
/// Alternatively, if the pointer originates from a ``FunctionArgument``, the argument is returned.
///
/// This underlying use-def traversal might cross phi arguments to identify the originating address
/// to handle diamond-shaped control-flow with common originating
/// address which might arise due to transformations ([example] (https://github.com/apple/swift/blob/8f9c5339542b17af9033f51ad7a0b95a043cad1b/test/SILOptimizer/access_storage_analysis_ossa.sil#L669-L705)) .
struct PointerIdentification {
  private var walker = PointerIdentificationUseDefWalker()

  mutating func getOriginatingAddressOrArgument(_ atp: PointerToAddressInst) -> AddressOrPointerArgument? {
    walker.start(atp.type)
    if walker.walkUp(value: atp.operand, path: SmallProjectionPath()) == .abortWalk {
      return nil
    }
    return walker.result
  }

  private struct PointerIdentificationUseDefWalker : ValueUseDefWalker {
    private var addrType: Type!
    private(set) var result: AddressOrPointerArgument?

    mutating func start(_ addrType: Type) {
      self.addrType = addrType
      result = nil
      walkUpCache.clear()
    }

    mutating func rootDef(value: Value, path: SmallProjectionPath) -> WalkResult {
      switch value {
      case let arg as FunctionArgument:
        if let res = result, res != self.result {
          self.result = nil
          return .abortWalk
        }
        self.result = .pointer(arg)
        return .continueWalk
      case let atp as AddressToPointerInst:
        if case let .address(result) = result, atp.operand != result {
          self.result = nil
          return .abortWalk
        }

        if addrType == atp.operand.type, path.isEmpty {
          self.result = .address(atp.operand)
          return .continueWalk
        }
      default:
        break
      }
      self.result = nil
      return .abortWalk
    }

    mutating func walkUp(value: Value, path: SmallProjectionPath) -> WalkResult {
      switch value {
      case is BlockArgument, is MarkDependenceInst, is CopyValueInst,
           is StructExtractInst, is TupleExtractInst, is StructInst, is TupleInst,
           is FunctionArgument, is AddressToPointerInst:
        return walkUpDefault(value: value, path: path)
      default:
        self.result = nil
        return .abortWalk
      }
    }

    var walkUpCache = WalkerCache<Path>()
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
      // Try identifying the address a pointer originates from
      if let p2ai = address as? PointerToAddressInst {
        if let addrOrArg = pointerId.getOriginatingAddressOrArgument(p2ai) {
          switch addrOrArg {
          case let .address(newAddress):
            return walkUp(address: newAddress, path: path)
          case let .pointer(arg):
            self.result = AccessPath(base: AccessBase(baseAddress: arg, kind: .pointer), projectionPath: path.projectionPath)
          }
        } else {
          self.result = AccessPath(base: AccessBase(baseAddress: p2ai, kind: .pointer), projectionPath: path.projectionPath)
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

/// A ValueUseDef walker that identifies which values a reference might
/// originate from.
protocol AccessStoragePathWalker : ValueUseDefWalker where Path == SmallProjectionPath {
  mutating func visit(access: AccessStoragePath)
}

extension AccessStoragePathWalker {
  // Main entry point
  /// Given an `ap: AccessPath` where the base address is a projection
  /// from a reference type, returns the set of reference definitions
  /// that the address of the access may originate from.
  mutating func getAccessStorage(for accessPath: AccessPath) {
    walkUpCache.clear()
    let path = accessPath.projectionPath
    switch accessPath.base.baseAddress {
    case let rea as RefElementAddrInst:
      _ = walkUp(value: rea.operand, path: path.push(.classField, index: rea.fieldIndex))
    case let pb as ProjectBoxInst:
      _ = walkUp(value: pb.operand, path: path.push(.classField, index: pb.fieldIndex))
    case let rta as RefTailAddrInst:
      _ = walkUp(value: rta.operand, path: path.push(.tailElements, index: 0))
    default:
      visit(access: AccessStoragePath(storage: accessPath.base.baseAddress, path: accessPath.projectionPath))
    }
  }

  mutating func rootDef(value: Value, path: SmallProjectionPath) -> WalkResult {
    visit(access: AccessStoragePath(storage: value, path: path))
    return .continueWalk
  }
}
