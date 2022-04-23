//===--- EscapeInfo.swift - Finds escape points of a value ----------------===//
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

import SIL

/// A utility for checking if a value escapes and for finding the escape points.
///
/// The EscapeInfo starts at the initial value and alternately walks in two directions:
/// * Starting at allocations, walks down from defs to uses ("Where does the value go to?")
/// * Starting at stores, walks up from uses to defs ("Were does the value come from?")
///
/// The result of the walk indicates if the initial value "escapes" or not.
/// The value escapes if the walk reaches a point where the further flow of the
/// value cannot be tracked anymore.
/// Example:
/// \code
///   %1 = alloc_ref $X    // 1. initial value: walk down to the `store`
///   %2 = alloc_stack $X  // 3. walk down to %3
///   store %1 to %2       // 2. walk up to `%2`
///   %3 = load %2         // 4. continue walking down to the `return`
///   return %3            // 5. The value is escaping!
/// \endcode
///
/// During the walk, a projection path indicates where the initial value is
/// contained in an aggregate.
/// Example for a walk-down:
/// \code
///   %1 = alloc_ref                   // 1. initial value, path = empty
///   %2 = struct $S (%1)              // 2. path = s0
///   %3 = tuple (%other, %1)          // 3. path = t1.s0
///   %4 = tuple_extract %3, 1         // 4. path = s0
///   %5 = struct_extract %4, #field   // 5. path = empty
/// \endcode
///
/// The `followStores` flag, which is passed together with the path, indicates
/// if stored values should be included in the walk.
/// If the initial value is stored to some memory allocation, we usually don't
/// care if other values are stored to that location as well. Example:
/// \code
///   %1 = alloc_ref $X    // 1. initial value, walk down to the `store`
///   %2 = alloc_stack $X  // 3. walk down to the second `store`
///   store %1 to %2       // 2. walk up to %2
///   store %other to %2   // 4. ignore (followStores == false): %other doesn't impact the "escapeness" of %1
/// \endcode
///
/// But once the the up-walk sees a load, it has to follow stores from that point on.
/// Example:
/// \code
/// bb0(%function_arg):            // 7. escaping! %1 escapes through %function_arg
///   %1 = alloc_ref $X            // 1. initial value, walk down to the second `store`
///   %addr = alloc_stack %X       // 5. walk down to the first `store`
///   store %function_arg to %addr // 6. walk up to %function_arg (followStores == true)
///   %2 = load %addr              // 4. walk up to %addr, followStores = true
///   %3 = ref_element_addr %2, #f // 3. walk up to %2
///   store %1 to %3               // 2. walk up to %3
/// \endcode
///
/// The algorithm doesn't distinguish between addresses and values, i.e. loads
/// and stores are treated as simple forwarding instructions, like casts.
/// For escaping it doesn't make a difference if a value or an address pointing to
/// the value, escapes.
/// An exception are `isEscaping(address: Value)` and similar functions: they ignore
/// values which are loaded from the address in question.
struct EscapeInfo {

  typealias Path = SmallProjectionPath

  public enum UseCallbackResult {
    case ignore
    case markEscaping
    case continueWalking
  }

  /// Called for all operands with the current path and `followStores` when walking down.
  typealias UseCallback = (Operand, Path, Bool) -> UseCallbackResult

  public enum DefCallbackResult {
    case ignore
    case markEscaping
    case continueWalkingUp
    case continueWalkingDown
  }

  /// Called for all values with the current path and `followStores` when walking up.
  typealias DefCallback = (Value, Path, Bool) -> DefCallbackResult

  //===--------------------------------------------------------------------===//
  //                           The top-level API
  //===--------------------------------------------------------------------===//

  init(calleeAnalysis: CalleeAnalysis) {
    self.calleeAnalysis = calleeAnalysis
  }

  /// Returns true if `object`, or any sub-objects which are selected by `path`, can escape.
  ///
  /// For example, let's assume this function is called with a struct, containing a reference,
  /// and a path of `s0.c*`:
  /// \code
  ///    %value : $Struct<X>                         // path == s0.c*, the initial `object`
  ///    %ref = struct_extract %value, #field0       // path == c*
  ///    %ref1 = struct_extract %value, #field1      // ignored - not selected by path
  ///    %addr = ref_element_addr %ref, #some_field  // path is empty
  ///    %v = load %addr                             // path is empty
  ///    return %v                                   // escaping!
  /// \endcode
  ///
  /// Trivial values are ignored, even if they are selected by `path`.
  mutating func isEscaping(object: Value, path: Path = Path(),
                           visitUse: UseCallback = { _, _, _ in .continueWalking },
                           visitDef: DefCallback = { _, _, _ in .continueWalkingUp }) -> Bool {
    start(forAnalyzingAddresses: false)
    defer { cleanup() }

    return walkUpAndCache(object, path: path, followStores: false,
                          visitUse: visitUse, visitDef: visitDef)
  }

  /// Returns true if the definition of `value` is escaping.
  ///
  /// In contrast to `isEscaping`, this function starts with a walk-up instead of a walk-down from `value`.
  mutating func isEscapingWhenWalkingDown(object: Value,
                      path: Path = Path(),
                      visitUse: UseCallback = { _, _, _ in .continueWalking },
                      visitDef: DefCallback = { _, _, _ in .continueWalkingUp }) -> Bool {
    start(forAnalyzingAddresses: false)
    defer { cleanup() }

    return walkDownAndCache(object, path: path, followStores: false, knownType: nil,
                            visitUse: visitUse, visitDef: visitDef)
  }

  /// Returns true if any address of `value`, which is selected by `path`, can escape.
  ///
  /// For example, let's assume this function is called with a struct, containing a reference,
  /// and a path of `s0.c*.v**`:
  /// \code
  ///    %value : $Struct<X>                         // path == s0.c*.v**, the initial `value`
  ///    %ref = struct_extract %value, #field0       // path == c*.v**
  ///    %selected_addr = ref_element_addr %ref, #x  // path == v**,       the selected address
  ///    apply %f(%selected_addr)                    // escaping!
  /// \endcode
  ///
  /// There are two differences to `isEscaping(object:)`:
  /// * Loads from the selected address(es) are ignored. So it's really about the _address_ and
  ///   not the value stored at the address.
  /// * Addresses with trivial types are _not_ ignored.
  mutating
  func isEscaping(addressesOf value: Value, path: Path = Path(.anyValueFields),
                  visitUse: UseCallback = { _, _, _ in .continueWalking },
                  visitDef: DefCallback = { _, _, _ in .continueWalkingUp }) -> Bool {
    start(forAnalyzingAddresses: true)
    defer { cleanup() }

    return walkUpAndCache(value, path: path, followStores: false, visitUse: visitUse, visitDef: visitDef)
  }

  /// Returns true if the selected address(es) of `lhs`/`lhsPath` can reference the same field as
  /// the selected address(es) of `rhs`/`rhsPath`.
  ///
  /// Example:
  ///   %1 = struct_element_addr %s, #field1    // true for (%1, %s)
  ///   %2 = struct_element_addr %s, #field2    // true for (%2, %s), false for (%1,%2)
  ///
  mutating func canReferenceSameField(_ lhs: Value, path lhsPath: Path = Path(.anyValueFields),
                                      _ rhs: Value, path rhsPath: Path = Path(.anyValueFields)) -> Bool {
    // lhs -> rhs will succeed (= return false) if lhs is a non-escaping "local" object,
    // but not necessarily rhs.
    if !isEscaping(addressesOf: lhs, path: lhsPath,
      visitUse: { op, _, _ in
        // Note: since we are checking the vale of an operand, we are ignoring address
        // projections with no uses. This is no problem. It just requires a fix_lifetime for
        // each address to test in alias-analysis test files.
        if op.value == rhs { return .markEscaping }
        if op.instruction is ReturnInst { return .ignore }
        return .continueWalking
      }) {
      return false
    }
    // The other way round: rhs -> lhs will succeed if rhs is a non-escaping "local" object,
    // but not necessarily lhs.
    if !isEscaping(addressesOf: rhs, path: rhsPath,
      visitUse: { op, _, _ in
        if op.value == lhs { return .markEscaping }
        if op.instruction is ReturnInst { return .ignore }
        return .continueWalking
      }) {
      return false
    }
    return true
  }

  //===--------------------------------------------------------------------===//
  //                             private state
  //===--------------------------------------------------------------------===//

  private struct CacheEntry {
    private(set) var path = Path()
    private(set) var followStores = false
    private(set) var knownType: Type?
    private var valid = false

    /// Merge the entry wit a new `path`, `followStores` and `knownType` and
    /// return the resulting entry if a new walk is needed.
    mutating func needWalk(path: Path, followStores: Bool, knownType: Type?) -> CacheEntry? {
      if !valid {
        // The first time we reach the value: do the walk with `path`, `followStores` and `knownType`.
        valid = true
        self.path = path
        self.followStores = followStores
        self.knownType = knownType
        return self
      }
      // There was already a walk for the value. Merge the `path`, `followStores` and
      // `knownType`.
      var newWalkIsNeeded = false
      if self.path != path {
        let newPath = self.path.merge(with: path)
        if newPath != self.path {
          self.path = newPath
          newWalkIsNeeded = true
        }
      }
      if !self.followStores && followStores {
        self.followStores = true
        newWalkIsNeeded = true
      }
      if let ty = self.knownType, ty != knownType {
        self.knownType = nil
        newWalkIsNeeded = true
      }
      if newWalkIsNeeded {
        // Merging the parameters resulted in something new (more conservative): a new walk is needed.
        return self
      }
      // Nothing changed, no new walk is necessary.
      return nil
    }
  }

  // The caches are not only useful for performance, but are need to avoid infinite
  // recursions of walkUp-walkDown cycles.
  private var walkedDownCache = Dictionary<HashableValue, CacheEntry>()
  private var walkedUpCache = Dictionary<HashableValue, CacheEntry>()

  /// Differences when analyzing address-escapes (instead of value-escapes):
  /// * also addresses with trivial types are tracked
  /// * loads of addresses are ignored
  /// * it can be assumed that addresses cannot escape a function (e.g. indirect parameters)
  private var analyzeAddresses = false

  private let calleeAnalysis: CalleeAnalysis
  
  //===--------------------------------------------------------------------===//
  //                          private utility functions
  //===--------------------------------------------------------------------===//

  private mutating func start(forAnalyzingAddresses: Bool) {
    precondition(walkedDownCache.isEmpty && walkedUpCache.isEmpty)
    analyzeAddresses = forAnalyzingAddresses
  }

  private mutating func cleanup() {
    walkedDownCache.removeAll(keepingCapacity: true)
    walkedUpCache.removeAll(keepingCapacity: true)
  }

  /// Returns true if the type of `value` at `path` is relevant and should be tracked.
  private func hasRelevantType(_ value: Value, at path: Path) -> Bool {
    let type = value.type
    if type.isNonTrivialOrContainsRawPointer(in: value.function) { return true }
    
    // For selected addresses we also need to consider trivial types (`value`
    // is a selected address if the path does not contain any class projections).
    if analyzeAddresses && type.isAddress && !path.hasClassProjection { return true }
    return false
  }

  /// Returns true if the selected address/value at `path` can be ignored for loading from
  /// that address or for passing that address/value to a called function.
  ///
  /// Passing the selected address (or a value loaded from the selected address) directly
  /// to a function, cannot let the selected address escape:
  ///  * if it's passed as address: indirect parameters cannot escape a function
  ///  * a load from the address does not let the address escape
  ///
  /// Example (continued from the previous example):
  ///    apply %other_func1(%selected_addr)    // cannot let %selected_addr escape (path == v**)
  ///    %l = load %selected_addr
  ///    apply %other_func2(%l)                // cannot let %selected_addr escape (path == v**)
  ///    apply %other_func3(%ref)              // can let %selected_addr escape!   (path == c*.v**)
  ///
  /// Also, we can ignore loads from the selected address, because a loaded value does not
  /// let the address escape.
  private func canIgnoreForLoadOrArgument(_ path: Path) -> Bool {
    return analyzeAddresses && path.hasNoClassProjection
  }

  /// Tries to pop the given projection from path, if the projected `value` has a relevant type.
  private func pop(_ kind: Path.FieldKind, index: Int? = nil, from path: Path, yielding value: Value) -> Path? {
    if let newPath = path.popIfMatches(kind, index: index),
       hasRelevantType(value, at: newPath) {
      return newPath
    }
    return nil
  }

  // Set a breakpoint here to debug when a value is escaping.
  private var isEscaping: Bool {
    true
  }

  //===--------------------------------------------------------------------===//
  //                           walk-down functions
  //===--------------------------------------------------------------------===//
 
  /// The entry point to the down-walk.
  ///
  /// It's a no-op if the walkDown is already cached, i.e was already done before with
  /// the same/matching `path`, `followStores` and `knownType`.
  ///
  /// In addition to the other common arguments, the `knownType` - if not nil - is the exact type
  /// of `value`. This is used for analysing releases by lookup up the knownType's destructor.
  ///
  /// Returns true if the `value` escapes.
  private mutating
  func walkDownAndCache(_ value: Value, path: Path, followStores: Bool,
                        knownType: Type?,
                        visitUse: UseCallback, visitDef: DefCallback) -> Bool {
    if let entry = walkedDownCache[value.hashable, default: CacheEntry()].needWalk(path: path, followStores: followStores, knownType: knownType) {
      return walkDown(value, path: entry.path, followStores: entry.followStores, knownType: entry.knownType,
                      visitUse: visitUse, visitDef: visitDef)
    }
    return false
  }

  /// Recursively walks down defs to uses, handling all relevant instructions.
  ///
  /// Returns true if the `value` escapes.
  private mutating
  func walkDown(_ value: Value, path: Path, followStores: Bool, knownType: Type?,
                visitUse: UseCallback, visitDef: DefCallback) -> Bool {
    for use in value.uses {
      if use.isTypeDependent { continue}

      switch visitUse(use, path, followStores) {
        case .ignore:
          continue
        case .markEscaping:
          return isEscaping
        case .continueWalking:
          break
      }

      let user = use.instruction
      switch user {
        case let rta as RefTailAddrInst:
          if let newPath = pop(.tailElements, from: path, yielding: rta) {
            if walkDown(rta, path: newPath, followStores: followStores, knownType: nil,
                        visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          }
        case let rea as RefElementAddrInst:
          if let newPath = pop(.classField, index: rea.fieldIndex, from: path, yielding: rea) {
            if walkDown(rea, path: newPath, followStores: followStores, knownType: nil,
                        visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          }
        case let pb as ProjectBoxInst:
          if let newPath = pop(.classField, index: pb.fieldIndex, from: path, yielding: pb) {
            if walkDown(pb, path: newPath, followStores: followStores, knownType: nil,
                        visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          }
        case let str as StructInst:
          if walkDown(str, path: path.push(.structField, index: use.index),
                      followStores: followStores, knownType: knownType,
                      visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case let se as StructExtractInst:
          if let newPath = pop(.structField, index: se.fieldIndex, from: path, yielding: se) {
            if walkDown(se, path: newPath, followStores: followStores, knownType: knownType,
                        visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          }
        case let ds as DestructureStructInst:
          if walkDownInstructionResults(results: ds.results,
                fieldKind: .structField, path: path, followStores: followStores, knownType: knownType,
                visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case let dt as DestructureTupleInst:
          if walkDownInstructionResults(results: dt.results,
                fieldKind: .tupleField, path: path, followStores: followStores, knownType: knownType,
                visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case let sea as StructElementAddrInst:
          if let newPath = pop(.structField, index: sea.fieldIndex, from: path, yielding: sea) {
            if walkDown(sea, path: newPath, followStores: followStores, knownType: nil,
                        visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          }
        case let t as TupleInst:
          if walkDown(t, path: path.push(.tupleField, index: use.index),
                      followStores: followStores, knownType: knownType,
                      visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case let te as TupleExtractInst:
          if let newPath = pop(.tupleField, index: te.fieldIndex, from: path, yielding: te) {
            if walkDown(te, path: newPath, followStores: followStores, knownType: knownType,
                        visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          }
        case let tea as TupleElementAddrInst:
          if let newPath = pop(.tupleField, index: tea.fieldIndex, from: path, yielding: tea) {
            if walkDown(tea, path: newPath, followStores: followStores, knownType: nil,
                        visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          }
        case let e as EnumInst:
          if walkDown(e, path: path.push(.enumCase, index: e.caseIndex),
                      followStores: followStores, knownType: knownType,
                      visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case is UncheckedEnumDataInst, is InitEnumDataAddrInst, is UncheckedTakeEnumDataAddrInst:
          let svi = user as! SingleValueInstruction
          let caseIdx = (user as! EnumInstruction).caseIndex
          if let newPath = pop(.enumCase, index: caseIdx, from: path, yielding: svi) {
            if walkDown(svi, path: newPath, followStores: followStores, knownType: knownType,
                        visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          }
        case let se as SwitchEnumInst:
          if let (caseIdx, newPath) = path.pop(kind: .enumCase) {
            if let succBlock = se.getUniqueSuccessor(forCaseIndex: caseIdx) {
              if let payload = succBlock.arguments.first {
                if walkDown(payload, path: newPath, followStores: followStores, knownType: knownType,
                            visitUse: visitUse, visitDef: visitDef) {
                  return true
                }
              }
            }
          } else if path.topMatchesAnyValueField {
            // We don't know the enum case: we have to containue with _all_ cases.
            for succBlock in se.block.successors {
              if let payload = succBlock.arguments.first {
                if walkDown(payload, path: path, followStores: followStores, knownType: knownType,
                            visitUse: visitUse, visitDef: visitDef) {
                  return true
                }
              }
            }
          } else {
            return isEscaping
          }
        case is StoreInst, is StoreWeakInst, is StoreUnownedInst:
          let store = user as! StoringInstruction
          if use == store.sourceOperand {
            if walkUp(store.destination, path: path, followStores: followStores,
                      visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          } else {
            assert(use == store.destinationOperand)
            if let si = store as? StoreInst, si.destinationOwnership == .assign {
              if handleDestroy(of: value, path: path, followStores: followStores, knownType: nil) {
                return true
              }
            }
            if followStores {
              if walkUp(store.source, path: path, followStores: followStores,
                        visitUse: visitUse, visitDef: visitDef) {
                return true
              }
            }
          }
        case let copyAddr as CopyAddrInst:
          if canIgnoreForLoadOrArgument(path) { continue }
          if use == copyAddr.sourceOperand {
            if walkUp(copyAddr.destination, path: path, followStores: followStores,
                      visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          } else {
            if !copyAddr.isInitializationOfDest {
              if handleDestroy(of: value, path: path, followStores: followStores, knownType: nil) {
                return true
              }
            }
            if followStores {
              assert(use == copyAddr.destinationOperand)
              if walkUp(copyAddr.source, path: path, followStores: followStores,
                        visitUse: visitUse, visitDef: visitDef) {
                return true
              }
            }
          }
        case is DestroyValueInst, is ReleaseValueInst, is StrongReleaseInst,
             is DestroyAddrInst:
          if handleDestroy(of: value, path: path, followStores: followStores, knownType: knownType) {
            return true
          }
        case let br as BranchInst:
          // We need to check the cache to avoid cycles and/or exploding complexity in
          // case of control flow merges.
          if walkDownAndCache(br.getArgument(for: use), path: path,
                              followStores: followStores, knownType: knownType,
                              visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case let cbr as CondBranchInst:
          // Same here: we need to check the cache.
          assert(use.index != 0, "should not visit trivial non-address values")
          if walkDownAndCache(cbr.getArgument(for: use), path: path,
                              followStores: followStores, knownType: knownType,
                              visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case is ReturnInst:
          return isEscaping
        case is ApplyInst, is TryApplyInst, is BeginApplyInst:
          if walkDownCallee(argOp: use, apply: user as! FullApplySite, path: path,
                            followStores: followStores, knownType: knownType,
                            visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case let pai as PartialApplyInst:
          if walkDownCallee(argOp: use, apply: pai, path: path,
                            followStores: followStores, knownType: nil,
                            visitUse: visitUse, visitDef: visitDef) {
            return true
          }
          // We need to follow the partial_apply value for two reasons:
          // 1. the closure (with the captured values) itself can escape
          // 2. something can escape in a destructor when the context is destroyed
          if walkDown(pai, path: path, followStores: followStores, knownType: nil,
                      visitUse: visitUse, visitDef: visitDef) {
            return true
          }

        case is LoadInst, is LoadWeakInst, is LoadUnownedInst:
          if canIgnoreForLoadOrArgument(path) { continue }
          let svi = user as! SingleValueInstruction
          
          // Even when analyzing addresses, a loaded trivial value can be ignored.
          if !svi.type.isNonTrivialOrContainsRawPointer(in: svi.function) { continue }

          if walkDown(svi, path: path,
                      followStores: followStores, knownType: nil,
                      visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case is InitExistentialAddrInst, is OpenExistentialAddrInst, is BeginAccessInst,
             is PointerToAddressInst, is AddressToPointerInst, is IndexAddrInst:
          if walkDown(user as! SingleValueInstruction, path: path,
                      followStores: followStores, knownType: nil,
                      visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case is InitExistentialRefInst, is OpenExistentialRefInst,
             is BeginBorrowInst, is CopyValueInst,
             is UpcastInst, is UncheckedRefCastInst, is EndCOWMutationInst,
             is RefToBridgeObjectInst, is BridgeObjectToRefInst:
          if walkDown(user as! SingleValueInstruction, path: path,
                      followStores: followStores, knownType: knownType,
                      visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case let bcm as BeginCOWMutationInst:
          if walkDown(bcm.bufferResult, path: path, followStores: followStores, knownType: knownType,
                          visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case let mdi as MarkDependenceInst:
          if use.index == 0 {
            if walkDown(mdi, path: path, followStores: followStores, knownType: knownType,
                        visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          }
        case let bi as BuiltinInst:
          switch bi.id {
            case .DestroyArray:
              if use.index != 1 ||
                 path.popAllValueFields().popIfMatches(.anyClassField) != nil {
                return isEscaping
              }
            default:
              return isEscaping
          }
        case is DeallocStackInst, is StrongRetainInst, is RetainValueInst,
             is DebugValueInst, is ValueMetatypeInst, is InjectEnumAddrInst,
             is InitExistentialMetatypeInst, is OpenExistentialMetatypeInst,
             is ExistentialMetatypeInst, is DeallocRefInst, is SetDeallocatingInst,
             is FixLifetimeInst, is ClassifyBridgeObjectInst, is BridgeObjectToWordInst,
             is EndBorrowInst, is EndAccessInst,
             is StrongRetainInst, is RetainValueInst,
             is ClassMethodInst, is SuperMethodInst, is ObjCMethodInst,
             is ObjCSuperMethodInst, is WitnessMethodInst,is DeallocStackRefInst:
          break
        default:
          return isEscaping
      }
    }
    return false
  }
  
  /// Utility function to continue walking down multi-result instructions, like
  /// `destructure_struct` and `destructure_tuple`.
  private mutating
  func walkDownInstructionResults(results: Instruction.Results,
                                  fieldKind: Path.FieldKind,
                                  path: Path, followStores: Bool, knownType: Type?,
                                  visitUse: UseCallback,
                                  visitDef: DefCallback) -> Bool {
    if let (index, newPath) = path.pop(kind: fieldKind) {
      return walkDown(results[index], path: newPath, followStores: followStores, knownType: knownType,
                      visitUse: visitUse, visitDef: visitDef)
    }
    if path.topMatchesAnyValueField {
      // We don't know the struct/tuple field: we have to continue with _all_ fields.
      for result in results where hasRelevantType(result, at: path) {
        if walkDown(result, path: path, followStores: followStores, knownType: nil,
                    visitUse: visitUse, visitDef: visitDef) {
          return true
        }
      }
      return false
    }
    return isEscaping
  }

  /// Handle an apply (full or partial) during the walk-down.
  private mutating
  func walkDownCallee(argOp: Operand, apply: ApplySite,
                      path: Path, followStores: Bool, knownType: Type?,
                      visitUse: UseCallback, visitDef: DefCallback) -> Bool {
    guard let argIdx = apply.argumentIndex(of: argOp) else {
      // The callee or a type dependent operand of the apply does not let escape anything.
      return false
    }

    if canIgnoreForLoadOrArgument(path) { return false }

    // Argument effects do not consider any potential stores to the argument (or it's content).
    // Therefore, if we need to track stores, the argument effects do not correctly describe what we need.
    // For example, argument 0 in the following function is marked as not-escaping, although there
    // is a store to the argument:
    //
    //   sil [escapes !%0.**] @callee(@inout X, @owned X) -> () {
    //   bb0(%0 : $*X, %1 : $X):
    //     store %1 to %0 : $*X
    //   }
    if followStores { return isEscaping }

    guard let callees = calleeAnalysis.getCallees(callee: apply.callee) else {
      // The callees are not know, e.g. if the callee is a closure, class method, etc.
      return isEscaping
    }

    let calleeArgIdx = apply.calleeArgIndex(callerArgIndex: argIdx)

    for callee in callees {
      let effects = callee.effects
      if !effects.canEscape(argumentIndex: calleeArgIdx, path: path, analyzeAddresses: analyzeAddresses) {
        continue
      }
      if walkDownArgument(calleeArgIdx: calleeArgIdx, argPath: path, knownType: knownType,
                          apply: apply, effects: effects,
                          visitUse: visitUse, visitDef: visitDef) {
        return true
      }
    }
    return false
  }
  
  /// Handle `.escaping` effects for an apply argument during the walk-down.
  private mutating
  func walkDownArgument(calleeArgIdx: Int, argPath: Path, knownType: Type?,
                        apply: ApplySite, effects: FunctionEffects,
                        visitUse: UseCallback, visitDef: DefCallback) -> Bool {
    var matched = false
    for effect in effects.argumentEffects {
      guard case .escaping(let to, let exclusive) = effect.kind else {
        continue
      }
      if effect.selectedArg.matches(.argument(calleeArgIdx), argPath) {
        matched = true
        
        switch to.value {
        case .returnValue:
          guard let fas = apply as? FullApplySite, let result = fas.singleDirectResult else { return isEscaping }
          
          if walkDown(result, path: to.pathPattern, followStores: false,
                      knownType: exclusive ? knownType : nil,
                      visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        case .argument(let toArgIdx):
          guard let callerToIdx = apply.callerArgIndex(calleeArgIndex: toArgIdx) else {
            return isEscaping
          }

          // Continue at the destination of an arg-to-arg escape.
          if walkUp(apply.arguments[callerToIdx], path: to.pathPattern, followStores: false,
                    visitUse: visitUse, visitDef: visitDef) {
            return true
          }
        }
        continue
      }
      // Handle the reverse direction of an arg-to-arg escape.
      if to.matches(.argument(calleeArgIdx), argPath) {
        guard let callerArgIdx = apply.callerArgIndex(calleeArgIndex: effect.selectedArg.argumentIndex) else {
          return isEscaping
        }
        if !exclusive { return isEscaping }

        matched = true

        if walkUp(apply.arguments[callerArgIdx], path: effect.selectedArg.pathPattern, followStores: false,
                  visitUse: visitUse, visitDef: visitDef) {
          return true
        }
        continue
      }
    }
    if !matched { return isEscaping }
    return false
  }
  
  private func handleDestroy(of value: Value, path: Path, followStores: Bool, knownType: Type?) -> Bool {

    // Even if this is a destroy_value of a struct/tuple/enum, the called destructor(s) only take a
    // single class reference as parameter.
    let p = path.popAllValueFields()

    if p.isEmpty {
      // The object to destroy (= the argument of the destructor) cannot escape itself.
      return false
    }
    if analyzeAddresses && p.matches(pattern: Path(.anyValueFields).push(.anyClassField)) {
      // Any address of a class property of the object to destroy cannot esacpe the destructor.
      // (Whereas a value stored in such a property could escape.)
      return false
    }

    if followStores {
      return isEscaping
    }
    if let exactTy = knownType {
      guard let destructor = calleeAnalysis.getDestructor(ofExactType: exactTy) else {
        return isEscaping
      }
      if destructor.effects.canEscape(argumentIndex: 0, path: p, analyzeAddresses: analyzeAddresses) {
        return isEscaping
      }
    } else {
      // We don't know the exact type, so get all possible called destructure from
      // the callee analysis.
      guard let destructors = calleeAnalysis.getDestructors(of: value.type) else {
        return isEscaping
      }
      for destructor in destructors {
        if destructor.effects.canEscape(argumentIndex: 0, path: p, analyzeAddresses: analyzeAddresses) {
          return isEscaping
        }
      }
    }
    return false
  }

  //===--------------------------------------------------------------------===//
  //                           walk-up functions
  //===--------------------------------------------------------------------===//
 
  /// The entry point to the up-walk.
  ///
  /// It's a no-op if the walkUp is already cached, i.e was already done before with
  /// the same/matching `path` and `followStores`.
  ///
  /// Returns true if the `value` escapes.
  private mutating
  func walkUpAndCache(_ value: Value, path: Path, followStores: Bool,
                      visitUse: UseCallback, visitDef: DefCallback) -> Bool {
    if let entry = walkedUpCache[value.hashable, default: CacheEntry()].needWalk(path: path, followStores: followStores, knownType: nil) {
      return walkUp(value, path: entry.path, followStores: entry.followStores,
                    visitUse: visitUse, visitDef: visitDef)
    }
    return false
  }
  
  /// Recursively walks up uses to defs, handling all relevant instructions.
  ///
  /// Returns true if the `value` escapes.
  private mutating
  func walkUp(_ value: Value, path: Path, followStores: Bool,
              visitUse: UseCallback, visitDef: DefCallback) -> Bool {
    var val = value
    var p = path
    var fSt = followStores
    while true {
      switch visitDef(val, p, fSt) {
        case .ignore:            return false
        case .markEscaping:      return isEscaping
        case .continueWalkingUp: break
        case .continueWalkingDown:
          return walkDownAndCache(val, path: p, followStores: fSt, knownType: nil,
                                  visitUse: visitUse, visitDef: visitDef)
      }
      switch val {
        case is AllocRefInst, is AllocRefDynamicInst:
          return walkDownAndCache(val, path: p, followStores: fSt, knownType: val.type,
                                  visitUse: visitUse, visitDef: visitDef)
        case is AllocRefInst, is AllocRefDynamicInst, is AllocStackInst, is AllocBoxInst:
          return walkDownAndCache(val, path: p, followStores: fSt, knownType: nil,
                                  visitUse: visitUse, visitDef: visitDef)
        case let arg as FunctionArgument:
          if canIgnoreForLoadOrArgument(path) && arg.isExclusiveIndirectParameter && !fSt {
            // The address of an indirect argument passed to the current function cannot escape.
            return walkDownAndCache(val, path: p, followStores: fSt, knownType: nil,
                                    visitUse: visitUse, visitDef: visitDef)
          }
          return isEscaping
        case let arg as BlockArgument:
          if arg.isPhiArgument {
            for incoming in arg.incomingPhiValues {
              if walkUpAndCache(incoming, path: p, followStores: fSt,
                                 visitUse: visitUse, visitDef: visitDef) {
                return true
              }
            }
            return false
          }
          let block = arg.block
          switch block.singlePredecessor!.terminator {
            case let se as SwitchEnumInst:
              guard let caseIdx = se.getUniqueCase(forSuccessor: block) else {
                return isEscaping
              }
              val = se.enumOp
              p = p.push(.enumCase, index: caseIdx)
            case let ta as TryApplyInst:
              if block != ta.normalBlock { return isEscaping }
              return walkUpApplyResult(apply: ta,
                                       path: p, followStores: fSt,
                                       visitUse: visitUse, visitDef: visitDef)
            default:
              return isEscaping
          }
        case let ap as ApplyInst:
          return walkUpApplyResult(apply: ap,
                                   path: p, followStores: fSt,
                                   visitUse: visitUse, visitDef: visitDef)
        case let str as StructInst:
          if let (structField, poppedPath) = p.pop(kind: .structField) {
            val = str.operands[structField].value
            p = poppedPath
          } else if p.topMatchesAnyValueField {
            // We don't know the struct field: we have to continue with _all_ operands.
            for op in str.operands where hasRelevantType(op.value, at: p) {
              if walkUpAndCache(op.value, path: p, followStores: fSt, visitUse: visitUse, visitDef: visitDef) {
                return true
              }
            }
            return false
          } else {
            return false
          }
        case let se as StructExtractInst:
          val = se.operand
          p = p.push(.structField, index: se.fieldIndex)
        case let t as TupleInst:
          if let (tupleField, poppedPath) = p.pop(kind: .tupleField) {
            val = t.operands[tupleField].value
            p = poppedPath
          } else if p.topMatchesAnyValueField {
            // We don't know the tuple element: we have to continue with _all_ operands.
            for op in t.operands where hasRelevantType(op.value, at: p) {
              if walkUpAndCache(op.value, path: p, followStores: fSt, visitUse: visitUse, visitDef: visitDef) {
                return true
              }
            }
            return false
          } else {
            return false
          }
        case let te as TupleExtractInst:
          val = te.operand
          p = p.push(.tupleField, index: te.fieldIndex)
        case is LoadInst, is LoadWeakInst, is LoadUnownedInst:
          val = (val as! UnaryInstruction).operand
          // When the value is loaded from somewhere it also matters what's
          // stored to that memory location.
          fSt = true
        case let rta as RefTailAddrInst:
          val = rta.operand
          p = p.push(.tailElements)
        case let rea as RefElementAddrInst:
          val = rea.operand
          p = p.push(.classField, index: rea.fieldIndex)
        case let pb as ProjectBoxInst:
          val = pb.operand
          p = p.push(.classField, index: pb.fieldIndex)
        case let sea as StructElementAddrInst:
          p = p.push(.structField, index: sea.fieldIndex)
          val = sea.operand
        case let tea as TupleElementAddrInst:
          p = p.push(.tupleField, index: tea.fieldIndex)
          val = tea.operand
        case let e as EnumInst:
          guard let newPath = p.popIfMatches(.enumCase, index: e.caseIndex) else {
            return false
          }
          guard let op = e.operand else { return false }
          p = newPath
          val = op
        case is UncheckedEnumDataInst, is InitEnumDataAddrInst, is UncheckedTakeEnumDataAddrInst:
          p = p.push(.enumCase, index: (val as! EnumInstruction).caseIndex)
          val = (val as! UnaryInstruction).operand
        case is UpcastInst, is UncheckedRefCastInst,
             is InitExistentialRefInst, is OpenExistentialRefInst,
             is InitExistentialAddrInst, is OpenExistentialAddrInst,
             is BeginAccessInst, is BeginBorrowInst,
             is EndCOWMutationInst,
             is RefToBridgeObjectInst, is BridgeObjectToRefInst,
             is IndexAddrInst, is CopyValueInst, is MarkDependenceInst,
             is PointerToAddressInst, is AddressToPointerInst:
          val = (val as! Instruction).operands[0].value
        case let mvr as MultipleValueInstructionResult:
          let inst = mvr.instruction
          switch inst {
            case let dsi as DestructureStructInst:
              p = p.push(.structField, index: mvr.index)
              val = dsi.operand
            case let dti as DestructureTupleInst:
              p = p.push(.tupleField, index: mvr.index)
              val = dti.operand
            case let bcm as BeginCOWMutationInst:
              val = bcm.operand
            default:
              return isEscaping
          }
        default:
          return isEscaping
      }
    }
  }
 
  /// Walks up from the return to the source argument if there is an "exclusive"
  /// escaping effect on an argument.
  private mutating
  func walkUpApplyResult(apply: FullApplySite,
                         path: Path, followStores: Bool,
                         visitUse: UseCallback, visitDef: DefCallback) -> Bool {
    guard let callees = calleeAnalysis.getCallees(callee: apply.callee) else {
      return isEscaping
    }

    for callee in callees {
      var matched = false
      for effect in callee.effects.argumentEffects {
        switch effect.kind {
        case .notEscaping:
          break
        case .escaping(let toSelectedArg, let exclusive):
          if exclusive && toSelectedArg.matches(.returnValue, path) {
            matched = true
            if walkUp(apply.arguments[effect.selectedArg.argumentIndex],
                      path: effect.selectedArg.pathPattern, followStores: followStores,
                      visitUse: visitUse, visitDef: visitDef) {
              return true
            }
          }
        }
      }
      if !matched {
        return isEscaping
      }
    }
    return false
  }
}
