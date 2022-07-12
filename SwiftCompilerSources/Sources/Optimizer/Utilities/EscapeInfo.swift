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

/// This protocol is used to customize `EscapeInfo` by implementing `visitUse` and
/// `visitDef` which are called for all uses and definitions ("direct" and "transitive") encountered
/// during a walk.
protocol EscapeInfoVisitor {
  typealias Path = SmallProjectionPath
  typealias UseResult = EscapeInfo<Self>.UseVisitResult
  typealias DefResult = EscapeInfo<Self>.DefVisitResult
  typealias State = EscapeInfo<Self>.State
  
  /// Called during the DefUse walk for each use
  mutating func visitUse(operand: Operand, path: Path, state: State) -> UseResult
  
  /// Called during the UseDef walk for each definition
  mutating func visitDef(def: Value, path: Path, state: State) -> DefResult
}

extension EscapeInfoVisitor {
  mutating func visitUse(operand: Operand, path: Path, state: State) -> UseResult {
    return .continueWalk
  }
  mutating func visitDef(def: Value, path: Path, state: State) -> DefResult {
    return .continueWalkUp
  }
}

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
struct EscapeInfo<V: EscapeInfoVisitor> {
  struct State {
    var followStores: Bool
    var knownType: Type?
    
    func with(followStores: Bool) -> Self {
      return Self(followStores: followStores, knownType: self.knownType)
    }
    
    func with(knownType: Type?) -> Self {
      return Self(followStores: self.followStores, knownType: knownType)
    }
  }
  
  typealias Path = SmallProjectionPath

  enum DefVisitResult {
    case ignore
    case continueWalkUp
    case walkDown
    case abort
  }

  enum UseVisitResult {
    case ignore
    case continueWalk
    case abort
  }
  
  //===--------------------------------------------------------------------===//
  //                           The top-level API
  //===--------------------------------------------------------------------===//

  init(calleeAnalysis: CalleeAnalysis, visitor: V) {
    self.walker = EscapeInfoWalker(calleeAnalysis: calleeAnalysis, visitor: visitor)
  }
  private var walker: EscapeInfoWalker<V>
  
  /// Returns the visitor passed as argument during initialization.
  /// It is also possible to mutate the visitor to reuse it for performance. See ``EscapeAliasAnalysis/canReferenceSameField``.
  var visitor: V {
    get { walker.visitor }
    
    _modify { yield &walker.visitor }
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
  mutating func isEscaping(object: Value, path: Path = Path()) -> Bool {
    walker.start(forAnalyzingAddresses: false)
    defer { walker.cleanup() }
    
    let state = State(followStores: false, knownType: nil)
    if let (path, state) = walker.shouldRecomputeUp(def: object, path: path, state: state) {
      return walker.walkUp(addressOrValue: object, path: path, state: state) == .abortWalk
    }
    return false
  }

  /// Returns true if the definition of `value` is escaping.
  ///
  /// In contrast to `isEscaping`, this function starts with a walk-down instead of a walk-up from `value`.
  mutating func isEscapingWhenWalkingDown(object: Value, path: Path = Path()) -> Bool {
    walker.start(forAnalyzingAddresses: false)
    defer { walker.cleanup() }
    
    return walker.cachedWalkDown(addressOrValue: object, path: path,
                                 state: State(followStores: false, knownType: nil)) == .abortWalk
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
  func isEscaping(addressesOf value: Value, path: Path = Path(.anyValueFields)) -> Bool {
    walker.start(forAnalyzingAddresses: true)
    defer { walker.cleanup() }
    
    let state = State(followStores: false, knownType: nil)
    if let (path, state) = walker.shouldRecomputeUp(def: value, path: path, state: state) {
      return walker.walkUp(addressOrValue: value, path: path, state: state) == .abortWalk
    }
    return false
  }
}

/// A lightweight form of AliasAnalysis that checks whether given two addresses can alias
/// by checking that the addresses don't escape and that during a walk of one of
/// the values, a use does not result in the other value.
struct EscapeAliasAnalysis {
  typealias Path = SmallProjectionPath
  
  private struct Visitor : EscapeInfoVisitor {
    // TODO: maybe we can create an empty value instead of option?
    var target: Value?
    func visitUse(operand: Operand, path: Path, state: State) -> UseResult {
      // Note: since we are checking the value of an operand, we are ignoring address
      // projections with no uses. This is no problem. It just requires a fix_lifetime for
      // each address to test in alias-analysis test files.
      if operand.value == target! { return .abort }
      if operand.instruction is ReturnInst { return .ignore }
      return .continueWalk
    }
  }
  
  private var calleeAnalysis: CalleeAnalysis
  private var walker: EscapeInfo<Visitor>
  init(calleeAnalysis: CalleeAnalysis) {
    self.calleeAnalysis = calleeAnalysis
    self.walker = EscapeInfo(calleeAnalysis: calleeAnalysis, visitor: Visitor())
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
    walker.visitor.target = rhs
    if !walker.isEscaping(addressesOf: lhs, path: lhsPath) {
      return false
    }
    // The other way round: rhs -> lhs will succeed if rhs is a non-escaping "local" object,
    // but not necessarily lhs.
    walker.visitor.target = lhs
    if !walker.isEscaping(addressesOf: rhs, path: rhsPath) {
      return false
    }
    return true
  }
}


/// The walker used by `EscapeInfo` to check whether a value escapes.
/// It is both a DefUse walker and UseDef walker. If during a walkDown a store or copy
/// is reached then
fileprivate struct EscapeInfoWalker<V: EscapeInfoVisitor> : ValueDefUseWalker,
                                                            AddressDefUseWalker,
                                                            ValueUseDefWalker,
                                                            AddressUseDefWalker {
  typealias State = EscapeInfo<V>.State
  
  init(calleeAnalysis: CalleeAnalysis, visitor: V) {
    self.calleeAnalysis = calleeAnalysis
    self.visitor = visitor
  }
  
  mutating func start(forAnalyzingAddresses: Bool) {
    precondition(walkedDownCache.isEmpty && walkedUpCache.isEmpty)
    analyzeAddresses = forAnalyzingAddresses
  }

  mutating func cleanup() {
    walkedDownCache.removeAll(keepingCapacity: true)
    walkedUpCache.removeAll(keepingCapacity: true)
  }

  //===--------------------------------------------------------------------===//
  //                                   Walking down
  //===--------------------------------------------------------------------===//
  
  /// Main entry point called by ``EscapeInfo``
  mutating func cachedWalkDown(addressOrValue: Value, path: Path, state: State) -> WalkResult {
    if let (path, state) = shouldRecomputeDown(def: addressOrValue, path: path, state: state) {
      return walkDown(addressOrValue: addressOrValue, path: path, state: state)
    } else {
      return .continueWalk
    }
  }
  
  mutating func walkDown(addressOrValue: Value, path: Path, state: State) -> WalkResult {
    if addressOrValue.type.isAddress {
      return walkDownUses(ofAddress: addressOrValue, path: path, state: state)
    } else {
      return walkDownUses(ofValue: addressOrValue, path: path, state: state)
    }
  }
  
  mutating func walkDown(value: Operand, path: Path, state: State) -> WalkResult {
    if hasRelevantType(value.value, at: path) {
      switch visitor.visitUse(operand: value, path: path, state: state) {
      case .continueWalk:
        return walkDownDefault(value: value, path: path, state: state)
      case .ignore:
        return .continueWalk
      case .abort:
        return .abortWalk
      }
    }
    return .continueWalk
  }
  
  /// ``ValueDefUseWalker`` conformance: called when the value def-use walk can't continue,
  /// i.e. when the result of the use is not a value.
  mutating func leafUse(value operand: Operand, path: Path, state: State) -> WalkResult {
    let instruction = operand.instruction
    switch instruction {
    case let rta as RefTailAddrInst:
      if let path = pop(.tailElements, from: path, yielding: rta) {
        return walkDownUses(ofAddress: rta, path: path, state: state.with(knownType: nil))
      }
    case let rea as RefElementAddrInst:
      if let path = pop(.classField, index: rea.fieldIndex, from: path, yielding: rea) {
        return walkDownUses(ofAddress: rea, path: path, state: state.with(knownType: nil))
      }
    case let pb as ProjectBoxInst:
      if let path = pop(.classField, index: pb.fieldIndex, from: path, yielding: pb) {
        return walkDownUses(ofAddress: pb, path: path, state: state.with(knownType: nil))
      }
    case is StoreInst, is StoreWeakInst, is StoreUnownedInst:
      let store = instruction as! StoringInstruction
      assert(operand == store.sourceOperand )
      return walkUp(address: store.destination, path: path, state: state)
    case is DestroyValueInst, is ReleaseValueInst, is StrongReleaseInst:
      if handleDestroy(of: operand.value, path: path, followStores: state.followStores,
                       knownType: state.knownType) == .abortWalk {
        return .abortWalk
      }
    case is ReturnInst:
      return isEscaping
    case is ApplyInst, is TryApplyInst, is BeginApplyInst:
      return walkDownCallee(argOp: operand, apply: instruction as! FullApplySite, path: path, state: state)
    case let pai as PartialApplyInst:
      // This is a non-stack closure.
      // For `stack` closures, `hasRelevantType` in `walkDown` will return false
      // stopping the walk since they don't escape.
      
      // Check whether the partially applied argument can escape in the body.
      if walkDownCallee(argOp: operand, apply: pai, path: path, state: state.with(knownType: nil)) == .abortWalk {
        return .abortWalk
      }
      
      // Additionally we need to follow the partial_apply value for two reasons:
      // 1. the closure (with the captured values) itself can escape
      //    and the use "transitively" escapes
      // 2. something can escape in a destructor when the context is destroyed
      return walkDownUses(ofValue: pai, path: path, state: state.with(knownType: nil))
    case let pta as PointerToAddressInst:
      assert(operand.index == 0)
      return walkDownUses(ofAddress: pta, path: path, state: state.with(knownType: nil))
    case let bi as BuiltinInst:
      switch bi.id {
      case .DestroyArray:
        if operand.index != 1 ||
            path.popAllValueFields().popIfMatches(.anyClassField) != nil {
          return isEscaping
        }
      default:
        return isEscaping
      }
    case is StrongRetainInst, is RetainValueInst, is DebugValueInst, is ValueMetatypeInst,
      is InitExistentialMetatypeInst, is OpenExistentialMetatypeInst,
      is ExistentialMetatypeInst, is DeallocRefInst, is SetDeallocatingInst, is FixLifetimeInst,
      is ClassifyBridgeObjectInst, is BridgeObjectToWordInst, is EndBorrowInst,
      is StrongRetainInst, is RetainValueInst,
      is ClassMethodInst, is SuperMethodInst, is ObjCMethodInst,
      is ObjCSuperMethodInst, is WitnessMethodInst, is DeallocStackRefInst:
      return .continueWalk
    case is DeallocStackInst:
      // dealloc_stack %f : $@noescape @callee_guaranteed () -> ()
      // type is a value
      assert(operand.value.definingInstruction is PartialApplyInst)
      return .continueWalk
    default:
      return isEscaping
    }
    return .continueWalk
  }
  
  mutating func walkDown(address: Operand, path: Path, state: State) -> WalkResult {
    if hasRelevantType(address.value, at: path) {
      switch visitor.visitUse(operand: address, path: path, state: state) {
      case .continueWalk:
        return walkDownDefault(address: address, path: path, state: state)
      case .ignore:
        return .continueWalk
      case .abort:
        return .abortWalk
      }
    }
    return .continueWalk
  }
  
  /// ``AddressDefUseWalker`` conformance: called when the address def-use walk can't continue,
  /// i.e. when the result of the use is not an address.
  mutating func leafUse(address operand: Operand, path: Path, state: State) -> WalkResult {
    let instruction = operand.instruction
    switch instruction {
    case is StoreInst, is StoreWeakInst, is StoreUnownedInst:
      let store = instruction as! StoringInstruction
      assert(operand == store.destinationOperand)
      if let si = store as? StoreInst, si.destinationOwnership == .assign {
        if handleDestroy(of: operand.value, path: path, followStores: state.followStores, knownType: nil) == .abortWalk {
          return .abortWalk
        }
      }
      if state.followStores {
        return walkUp(value: store.source, path: path, state: state)
      }
    case let copyAddr as CopyAddrInst:
      if canIgnoreForLoadOrArgument(path) { return .continueWalk }
      if operand == copyAddr.sourceOperand {
        return walkUp(address: copyAddr.destination, path: path, state: state)
      } else {
        if !copyAddr.isInitializationOfDest {
          if handleDestroy(of: operand.value, path: path, followStores: state.followStores,
                           knownType: nil) == .abortWalk {
            return .abortWalk
          }
        }
        
        if state.followStores {
          assert(operand == copyAddr.destinationOperand)
          return walkUp(value: copyAddr.source, path: path, state: state)
        }
      }
    case is DestroyAddrInst:
      if handleDestroy(of: operand.value, path: path, followStores: state.followStores,
                       knownType: state.knownType) == .abortWalk {
        return .abortWalk
      }
    case is ReturnInst:
      return isEscaping
    case is ApplyInst, is TryApplyInst, is BeginApplyInst:
      return walkDownCallee(argOp: operand, apply: instruction as! FullApplySite, path: path, state: state)
    case let pai as PartialApplyInst:
      if walkDownCallee(argOp: operand, apply: pai, path: path, state: state.with(knownType: nil)) == .abortWalk {
        return .abortWalk
      }

      // We need to follow the partial_apply value for two reasons:
      // 1. the closure (with the captured values) itself can escape
      // 2. something can escape in a destructor when the context is destroyed
      return walkDownUses(ofValue: pai, path: path, state: state.with(knownType: nil))
    case is LoadInst, is LoadWeakInst, is LoadUnownedInst:
      if canIgnoreForLoadOrArgument(path) { return .continueWalk }
      let svi = instruction as! SingleValueInstruction
      
      // Even when analyzing addresses, a loaded trivial value can be ignored.
      if !svi.type.isNonTrivialOrContainsRawPointer(in: svi.function) { return .continueWalk }
      return walkDownUses(ofValue: svi, path: path, state: state.with(knownType: nil))
    case let atp as AddressToPointerInst:
      return walkDownUses(ofValue: atp, path: path, state: state.with(knownType: nil))
    case let ia as IndexAddrInst:
      assert(operand.index == 0)
      return walkDownUses(ofAddress: ia, path: path, state: state.with(knownType: nil))
    case is DeallocStackInst, is InjectEnumAddrInst, is FixLifetimeInst, is EndBorrowInst, is EndAccessInst:
      return .continueWalk
    default:
      return isEscaping
    }
    return .continueWalk
  }
  
  /// Check whether the value escapes through the deinitializer
  private func handleDestroy(of value: Value, path: SmallProjectionPath, followStores: Bool, knownType: Type?) -> WalkResult {

    // Even if this is a destroy_value of a struct/tuple/enum, the called destructor(s) only take a
    // single class reference as parameter.
    let p = path.popAllValueFields()

    if p.isEmpty {
      // The object to destroy (= the argument of the destructor) cannot escape itself.
      return .continueWalk
    }
    if analyzeAddresses && p.matches(pattern: SmallProjectionPath(.anyValueFields).push(.anyClassField)) {
      // Any address of a class property of the object to destroy cannot esacpe the destructor.
      // (Whereas a value stored in such a property could escape.)
      return .continueWalk
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
    return .continueWalk
  }
  
  /// Handle an apply (full or partial) during the walk-down.
  private mutating
  func walkDownCallee(argOp: Operand, apply: ApplySite,
                      path: Path, state: State) -> WalkResult {
    guard let argIdx = apply.argumentIndex(of: argOp) else {
      // The callee or a type dependent operand of the apply does not let escape anything.
      return .continueWalk
    }

    if canIgnoreForLoadOrArgument(path) { return .continueWalk }

    // Argument effects do not consider any potential stores to the argument (or it's content).
    // Therefore, if we need to track stores, the argument effects do not correctly describe what we need.
    // For example, argument 0 in the following function is marked as not-escaping, although there
    // is a store to the argument:
    //
    //   sil [escapes !%0.**] @callee(@inout X, @owned X) -> () {
    //   bb0(%0 : $*X, %1 : $X):
    //     store %1 to %0 : $*X
    //   }
    if state.followStores { return isEscaping }

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
      if walkDownArgument(calleeArgIdx: calleeArgIdx, argPath: path, state: state,
                          apply: apply, effects: effects) == .abortWalk {
        return .abortWalk
      }
    }
    return .continueWalk
  }
  
  /// Handle `.escaping` effects for an apply argument during the walk-down.
  private mutating
  func walkDownArgument(calleeArgIdx: Int, argPath: Path, state: State,
                        apply: ApplySite, effects: FunctionEffects) -> WalkResult {
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
          
          let state = (exclusive ? state : state.with(knownType: nil)).with(followStores: false)
          
          if walkDownUses(ofValue: result, path: to.pathPattern, state: state) == .abortWalk {
            return .abortWalk
          }
        case .argument(let toArgIdx):
          guard let callerToIdx = apply.callerArgIndex(calleeArgIndex: toArgIdx) else {
            return isEscaping
          }

          // Continue at the destination of an arg-to-arg escape.
          let arg = apply.arguments[callerToIdx]
          
          let state = state.with(knownType: nil).with(followStores: false)
          if walkUp(addressOrValue: arg, path: to.pathPattern, state: state) == .abortWalk {
            return .abortWalk
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
        
        let arg = apply.arguments[callerArgIdx]
        let state = state.with(followStores: false).with(knownType: nil)

        if walkUp(addressOrValue: arg, path: effect.selectedArg.pathPattern, state: state) == .abortWalk {
          return .abortWalk
        }
        continue
      }
    }
    if !matched { return isEscaping }
    return .continueWalk
  }
  
  mutating func shouldRecomputeDown(def: Value, path: Path, state: State) -> (Path, State)? {
    if let entry = walkedDownCache[def.hashable, default: CacheEntry()].needWalk(path: path, followStores: state.followStores, knownType: state.knownType) {
      return (entry.path, state.with(knownType: entry.knownType).with(followStores: entry.followStores))
    }
    return nil
  }
  
  //===--------------------------------------------------------------------===//
  //                                   Walking up
  //===--------------------------------------------------------------------===//
  
  /// Main entry point called by ``EscapeInfo``
  mutating func walkUp(addressOrValue: Value, path: Path, state: State) -> WalkResult {
    if addressOrValue.type.isAddress {
      return walkUp(address: addressOrValue, path: path, state: state)
    } else {
      return walkUp(value: addressOrValue, path: path, state: state)
    }
  }
  
  mutating func walkUp(value: Value, path: Path, state: State) -> WalkResult {
    if hasRelevantType(value, at: path) {
      switch visitor.visitDef(def: value, path: path, state: state) {
      case .continueWalkUp:
        return walkUpDefault(value: value, path: path, state: state)
      case .walkDown:
        return cachedWalkDown(addressOrValue: value, path: path, state: state.with(knownType: nil))
      case .ignore:
        return .continueWalk
      case .abort:
        return .abortWalk
      }
    }
    return .continueWalk
  }
  
  /// ``ValueUseDefWalker`` conformance: called when the value use-def walk can't continue,
  /// i.e. when the operand (if any) of the instruction of a definition is not a value.
  mutating func rootDef(value def: Value, path: Path, state: State) -> WalkResult {
    let state = state.with(knownType: nil)
    switch def {
    case is AllocRefInst, is AllocRefDynamicInst:
      return cachedWalkDown(addressOrValue: def, path: path, state: state.with(knownType: def.type))
    case is AllocBoxInst:
      return cachedWalkDown(addressOrValue: def, path: path, state: state)
    case let arg as BlockArgument:
      let block = arg.block
      switch block.singlePredecessor!.terminator {
      case let ta as TryApplyInst:
        if block != ta.normalBlock { return isEscaping }
        return walkUpApplyResult(apply: ta, path: path, state: state)
      default:
        return isEscaping
      }
    case let ap as ApplyInst:
      return walkUpApplyResult(apply: ap, path: path, state: state)
    case is LoadInst, is LoadWeakInst, is LoadUnownedInst:
      return walkUp(address: (def as! UnaryInstruction).operand,
                    path: path, state: state.with(followStores: true))
    case let atp as AddressToPointerInst:
      return walkUp(address: atp.operand, path: path, state: state)
    default:
      return isEscaping
    }
  }
  
  mutating func walkUp(address: Value, path: Path, state: State) -> WalkResult {
    if hasRelevantType(address, at: path) {
      switch visitor.visitDef(def: address, path: path, state: state) {
      case .continueWalkUp:
        return walkUpDefault(address: address, path: path, state: state)
      case .walkDown:
        return cachedWalkDown(addressOrValue: address, path: path, state: state)
      case .ignore:
        return .continueWalk
      case .abort:
        return .abortWalk
      }
    }
    return .continueWalk
  }
  
  /// ``AddressUseDefWalker`` conformance: called when the address use-def walk can't continue,
  /// i.e. when the operand (if any) of the instruction of a definition is not an address.
  mutating func rootDef(address def: Value, path: SmallProjectionPath, state: State) -> WalkResult {
    let state = state.with(knownType: nil)
    switch def {
    case is AllocStackInst:
      return cachedWalkDown(addressOrValue: def, path: path, state: state)
    case let arg as FunctionArgument:
      if canIgnoreForLoadOrArgument(path) && arg.isExclusiveIndirectParameter && !state.followStores {
        return cachedWalkDown(addressOrValue: def, path: path, state: state)
      } else {
        return isEscaping
      }
    case is PointerToAddressInst, is IndexAddrInst:
      return walkUp(value: (def as! SingleValueInstruction).operands[0].value, path: path, state: state)
    case let rta as RefTailAddrInst:
      return walkUp(value: rta.operand, path: path.push(.tailElements), state: state)
    case let rea as RefElementAddrInst:
      return walkUp(value: rea.operand, path: path.push(.classField, index: rea.fieldIndex), state: state)
    case let pb as ProjectBoxInst:
      return walkUp(value: pb.operand, path: path.push(.classField, index: pb.fieldIndex), state: state)
    default:
      return isEscaping
    }
  }
  
  /// Walks up from the return to the source argument if there is an "exclusive"
  /// escaping effect on an argument.
  private mutating
  func walkUpApplyResult(apply: FullApplySite,
                         path: Path, state: State) -> WalkResult {
    guard let callees = calleeAnalysis.getCallees(callee: apply.callee) else {
      return .abortWalk
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
            let arg = apply.arguments[effect.selectedArg.argumentIndex]
            
            if walkUp(addressOrValue: arg, path: effect.selectedArg.pathPattern, state: state.with(knownType: nil)) == .abortWalk {
              return .abortWalk
            }
          }
        }
      }
      if !matched {
        return isEscaping
      }
    }
    return .continueWalk
  }
  
  mutating func shouldRecomputeUp(def: Value, path: Path, state: State) -> (Path, State)? {
    if let entry = walkedUpCache[def.hashable, default: CacheEntry()].needWalk(path: path, followStores: state.followStores, knownType: state.knownType) {
      return (entry.path, state.with(knownType: entry.knownType).with(followStores: entry.followStores))
    }
    return nil
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

  var visitor: V

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
  private var isEscaping: WalkResult { .abortWalk }
}
