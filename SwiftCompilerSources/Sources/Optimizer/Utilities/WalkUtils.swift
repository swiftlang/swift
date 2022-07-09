//===--- WalkUtils.swift - Utilities for use-def def-use walks ------------===//
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

/// Result returned by the walker functions
enum WalkResult {
  /// Continue the walk
  case continueWalk
  /// Stop the walks of all uses, a sufficient condition has been found
  case abortWalk
}

/// - A `DefUseWalker` finds all uses of a target value.
///
/// - A target value is described by an "initial" value and a projection path.
///   1. If the projection path is empty (`""`) then the target value is the initial value itself.
///   2. If the projection path is non-empty (`"s0.1.e3"`), then the target value is the one
///     reachable from the initial value through the series of projections described by the path.
/// - A path can also contain a pattern such as `"v**"` which means any series of "value"
///   projections (excluding `ref_element_addr` and similar, i.e. `c*`) from any field.
///   In the `v**` case, the target value*s* are many, i.e. all the ones reachable from
///   the initial value through _any of the fields_ through _any number_ of value projections.
///   `c*` means values reachable through a _single_ projection of _any_ of the fields of the class.
///
/// - A walk is started with a call to `walkDownUses(initial, path: path, state: state)`.
/// - This function will call `walkDown(operand, path: path, state: state)`
///   for every use of `initial` as `operand` in an instruction.
/// - For each use, then the walk can continue with initial value the result if the result of the using
///   instruction might still reach the target value with a new projection path.
///   1. If the use is a construction such as a
///     `%res = struct $S (%f0)` (or `%res = tuple (%unk, %1)`) instruction and the path is `p`
///     then the `%res` result value reaches the target value through the new projection`s0.p` (respectively `1.p`).
///   2.  If the use is a projection such as `%res = struct_extract %s : $S, #S.field0` and the
///     path is `s0.s1` then the target value is reachable from `%res` with path `s1`.
///     If the path doesn't match `unmatchedPath` is called.
///   3. If the use is a "forwarding instruction", such as a cast, the walk continues with the same path.
///   4. If the use is an unhandled instruction then `leafUse` is called to denote that the client has to
///     handle this use.
///
/// - `State` can be defined by the implementor to track specific state of a "branch"
///   of the walk, i.e. whether a certain instruction was crossed while walking towards _this use_ of the target.
///
/// There are two types of DefUseWalkers, one for values (`ValueDefUseWalker`) and one for
/// addresses (`AddressDefUseWalker`)


/// A `ValueDefUseWalker` can only handle "value" initial values, which correspond
/// to types that are not addresses, i.e. _do not have_ an asterisk (`*`) in the textual
/// representation of their SIL type (`$T`).
/// These can be values of reference type, or struct/tuple etc.
/// A `ValueDefUseWalker.walkDownDefault` called on a use of a initial "value" which
/// yields an "address" value (such as `ref_element_addr %initial_value`) will call `leafUse`
/// since the walk can't proceed.
///
/// Example call `walkDownUses(%str, path: "s0.s1", state: state)`
/// ```
/// %fa    = struct_extract %str  : $S1, #S1.fa   // 1. field 0, walkDownUses(%fa, "s1")
/// %fb    = struct_extract %str  : $S1, #S1.fb   // 5. field 1, unmatchedPath(%str, "s0.s1")
/// %fa.ga = struct_extract %fa   : $S2, #S2.ga   // 2. field 1, walkDownUses(%fa.ga, "")
/// ...    = struct_extract %fa.ga: $S3, #S3.ha   // 3. empty path, unmatchedPath(%fa.ga, "")
/// ...    = <instruction>  %fa.ga:               // 4. unknown instruction, leafUse(%fa.ga, "")
/// ...    = <instruction>  %str:                 // 6. unknown instruction, leafUse(%str, "s0.s1")
/// ```
protocol ValueDefUseWalker {
  typealias Path = SmallProjectionPath
  associatedtype State
  
  /// Called on each use. The implementor can decide to continue the walk by calling
  /// `walkDownDefault(value: value, path: path, state: state)` or
  /// do nothing.
  mutating func walkDown(value: Operand, path: Path, state: State) -> WalkResult
  
  /// `leafUse` is called from `walkDownDefault` when the walk can't continue for this use since
  /// this is an instruction unknown to the default walker which _might_ be a "transitive use"
  /// of the target value (such as `destroy_value %initial` or a `builtin ... %initial` instruction)
  mutating func leafUse(value: Operand, path: Path, state: State) -> WalkResult
  
  /// `unmatchedPath` is called from `walkDownDefault` when this is a use
  /// of the initial value in an instruction recognized by the walker
  /// but for which the requested `path` does not allow the walk to continue.
  mutating func unmatchedPath(value: Operand, path: Path, state: State) -> WalkResult

  /// A client must implement this function to cache walking results.
  /// The function returns `nil` if the walk doesn't need to continue because
  /// the `def` was already handled before.
  /// In case the walk needs to be continued, this function returns the path and state for continuing the walk.
  ///
  /// This method is called for two cases:
  /// 1. To avoid exponential complexity during a walk down with a wildcard path `v**` or `**`
  ///   ```
  ///   (%1, %2, %3, %4) = destructure_tuple %t1
  ///   %t2 = tuple (%1, %2, %3, %4)
  ///   (%5, %6, %7, %8) = destructure_tuple %t2
  ///   %t3 = tuple (%5, %6, %7, %8)
  ///   ```
  /// 2. To handle "phi webs" of `br` instructions which would lead to an infinite
  ///   walk down. In this case the implementor must ensure that eventually
  ///   `shouldRecomputeDown` returns `nil`, i.e. a fixpoint has been reached.
  ///   - If the implementor doesn't need for the walk to cross phi webs,
  ///     it can intercept `BranchInst`/`CondBranchInst` in `walkDown` and
  ///     not call `walkDownDefault` for these cases.
  ///   - Phi webs arise only for "value"s.
  mutating func shouldRecomputeDown(def: Value, path: Path, state: State) -> (Path, State)?
}

extension ValueDefUseWalker {
  mutating func walkDown(value operand: Operand, path: Path, state: State) -> WalkResult {
    return walkDownDefault(value: operand, path: path, state: state)
  }
  
  mutating func unmatchedPath(value: Operand, path: Path, state: State) -> WalkResult {
    return .continueWalk
  }
  
  /// Given an operand to an instruction, tries to continue the walk with the uses of
  /// instruction's result if the target value is reachable from it (i.e. matches the `path`) .
  /// If the walk can't continue, it calls `leafUse` or `unmatchedPath`
  mutating func walkDownDefault(value operand: Operand, path: Path, state: State) -> WalkResult {
    let instruction = operand.instruction
    switch instruction {
    case let str as StructInst:
      return walkDownUses(ofValue: str,
                      path: path.push(.structField, index: operand.index),
                      state: state)
    case let t as TupleInst:
      return walkDownUses(ofValue: t,
                      path: path.push(.tupleField, index: operand.index),
                      state: state)
    case let e as EnumInst:
      return walkDownUses(ofValue: e,
                          path: path.push(.enumCase, index: e.caseIndex),
                          state: state)
    case let se as StructExtractInst:
      if let path = path.popIfMatches(.structField, index: se.fieldIndex) {
        return walkDownUses(ofValue: se, path: path, state: state)
      } else {
        return unmatchedPath(value: operand, path: path, state: state)
      }
    case let te as TupleExtractInst:
      if let path = path.popIfMatches(.tupleField, index: te.fieldIndex) {
        return walkDownUses(ofValue: te, path: path, state: state)
      } else {
        return unmatchedPath(value: operand, path: path, state: state)
      }
    case let ued as UncheckedEnumDataInst:
      if let path = path.popIfMatches(.enumCase, index: ued.caseIndex) {
        return walkDownUses(ofValue: ued, path: path, state: state)
      } else {
        return unmatchedPath(value: operand, path: path, state: state)
      }
    case let ds as DestructureStructInst:
      if let (index, path) = path.pop(kind: .structField) {
        return walkDownUses(ofValue: ds.results[index], path: path, state: state)
      } else if path.topMatchesAnyValueField {
        return walkDownResults(of: ds, path: path, state: state)
      } else {
        return unmatchedPath(value: operand, path: path, state: state)
      }
    case let dt as DestructureTupleInst:
      if let (index, path) = path.pop(kind: .tupleField) {
        return walkDownUses(ofValue: dt.results[index], path: path, state: state)
      } else if path.topMatchesAnyValueField {
        return walkDownResults(of: dt, path: path, state: state)
      } else {
        return unmatchedPath(value: operand, path: path, state: state)
      }
    case is InitExistentialRefInst, is OpenExistentialRefInst,
      is BeginBorrowInst, is CopyValueInst,
      is UpcastInst, is UncheckedRefCastInst, is EndCOWMutationInst,
      is RefToBridgeObjectInst, is BridgeObjectToRefInst:
      return walkDownUses(ofValue: (instruction as! SingleValueInstruction), path: path, state: state)
    case let mdi as MarkDependenceInst:
      if operand.index == 0 {
        return walkDownUses(ofValue: mdi, path: path, state: state)
      } else {
        return unmatchedPath(value: operand, path: path, state: state)
      }
    case let br as BranchInst:
      let val = br.getArgument(for: operand)
      if let (path, state) = shouldRecomputeDown(def: val, path: path, state: state) {
        return walkDownUses(ofValue: val, path: path, state: state)
      } else {
        return .continueWalk
      }
    case let cbr as CondBranchInst:
      let val = cbr.getArgument(for: operand)
      if let (path, state) = shouldRecomputeDown(def: val, path: path, state: state) {
        return walkDownUses(ofValue: val, path: path, state: state)
      } else {
        return .continueWalk
      }
    case let se as SwitchEnumInst:
      if let (caseIdx, path) = path.pop(kind: .enumCase),
         let succBlock = se.getUniqueSuccessor(forCaseIndex: caseIdx),
         let payload = succBlock.arguments.first {
        return walkDownUses(ofValue: payload, path: path, state: state)
      } else if path.topMatchesAnyValueField {
        for succBlock in se.block.successors {
          if let payload = succBlock.arguments.first,
             walkDownUses(ofValue: payload, path: path, state: state) == .abortWalk {
            return .abortWalk
          }
        }
        return .continueWalk
      } else {
        return unmatchedPath(value: operand, path: path, state: state)
      }
    case let bcm as BeginCOWMutationInst:
      return walkDownUses(ofValue: bcm.bufferResult, path: path, state: state)
    default:
      return leafUse(value: operand, path: path, state: state)
    }
  }
  
  /// Starts the walk
  mutating func walkDownUses(ofValue: Value, path: Path, state: State) -> WalkResult {
    for operand in ofValue.uses where !operand.isTypeDependent {
      if walkDown(value: operand, path: path, state: state) == .abortWalk {
        return .abortWalk
      }
    }
    return .continueWalk
  }
  
  mutating func walkDownResults(of value: MultipleValueInstruction, path: Path, state: State) -> WalkResult {
    for result in value.results {
      if let (path, state) = shouldRecomputeDown(def: result, path: path, state: state) {
        if walkDownUses(ofValue: result, path: path, state: state) == .abortWalk {
          return .abortWalk
        }
      }
    }
    return .continueWalk
  }
}

/// An `AddressDefUseWalker` can only handle initial "addresses", which correspond
/// to types that are addresses (`$*T`).
/// An `AddressDefUseWalker.walkDownDefault` called on a use of an initial "address"
/// which results in a "value" (such as `load %initial_addr`) will call `leafUse` since the walk
/// can't proceed.
/// All functions return a boolean flag which, if true, can stop the walk of the other uses
/// and the whole walk.
protocol AddressDefUseWalker {
  typealias Path = SmallProjectionPath
  associatedtype State
  
  /// Called on each use. The implementor can decide to continue the walk by calling
  /// `walkDownDefault(address: address, path: path, state: state)` or
  /// do nothing.
  mutating func walkDown(address: Operand, path: Path, state: State) -> WalkResult
  
  /// `leafUse` is called from `walkDownDefault` when the walk can't continue for this use since
  /// this is an instruction unknown to the default walker which might be a "transitive use"
  /// of the target value (such as `destroy_addr %initial_addr` or a `builtin ... %initial_addr` instruction).
  mutating func leafUse(address: Operand, path: Path, state: State) -> WalkResult
  
  /// `unmatchedPath` is called from `walkDownDefault` when this is a use
  /// of the initial address in an instruction recognized by the walker
  /// but for which the requested `path` does not allow the walk to continue.
  mutating func unmatchedPath(address: Operand, path: Path, state: State) -> WalkResult
}

extension AddressDefUseWalker {
  mutating func walkDown(address operand: Operand, path: Path, state: State) -> WalkResult {
    return walkDownDefault(address: operand, path: path, state: state)
  }
  
  mutating func unmatchedPath(address: Operand, path: Path, state: State) -> WalkResult {
    return .continueWalk
  }
  
  mutating func walkDownDefault(address operand: Operand, path: Path, state: State) -> WalkResult {
    let instruction = operand.instruction
    switch instruction {
    case let sea as StructElementAddrInst:
      if let path = path.popIfMatches(.structField, index: sea.fieldIndex) {
        return walkDownUses(ofAddress: sea, path: path, state: state)
      } else {
        return unmatchedPath(address: operand, path: path, state: state)
      }
    case let tea as TupleElementAddrInst:
      if let path = path.popIfMatches(.tupleField, index: tea.fieldIndex) {
        return walkDownUses(ofAddress: tea, path: path, state: state)
      } else {
        return unmatchedPath(address: operand, path: path, state: state)
      }
    case is InitEnumDataAddrInst, is UncheckedTakeEnumDataAddrInst:
      let ei = instruction as! SingleValueInstruction
      if let path = path.popIfMatches(.enumCase, index: (instruction as! EnumInstruction).caseIndex) {
        return walkDownUses(ofAddress: ei, path: path, state: state)
      } else {
        return unmatchedPath(address: operand, path: path, state: state)
      }
    case is InitExistentialAddrInst, is OpenExistentialAddrInst, is BeginAccessInst,
      is IndexAddrInst:
      // FIXME: for now `index_addr` is treated as a forwarding instruction since
      // SmallProjectionPath does not track indices.
      // This is ok since `index_addr` is eventually preceeded by a `tail_addr`
      // which has pushed a `"ct"` component on the path that matches any
      // `index_addr` address.
      return walkDownUses(ofAddress: instruction as! SingleValueInstruction, path: path, state: state)
    case let mdi as MarkDependenceInst:
      if operand.index == 0 {
        return walkDownUses(ofAddress: mdi, path: path, state: state)
      } else {
        return unmatchedPath(address: operand, path: path, state: state)
      }
    default:
      return leafUse(address: operand, path: path, state: state)
    }
  }
  
  mutating func walkDownUses(ofAddress: Value, path: Path, state: State) -> WalkResult {
    for operand in ofAddress.uses where !operand.isTypeDependent {
      if walkDown(address: operand, path: path, state: state) == .abortWalk {
        return .abortWalk
      }
    }
    return .continueWalk
  }
}

/// - A `UseDefWalker` can be used to find all "generating" definitions of
///   a target value.
/// - A target value is described by an "initial" value and a projection path as in a `DefUseWalker.`
///   1. If the projection path is empty (`""`) then the target value is the initial value itself.
///   2. If the projection path is non-empty (`"s0.1.e3"`), then the target value is the one
///     reachable through the series of projections described by the path, applied to the initial value.
/// - The same notes about wildcard paths in `DefUseWalker` apply here.
///
/// - A walk is started with a call to `walkUp(initial, path: path, state: state)`.
///
/// - The implementor of `walkUp` can then track the definition if needed and
///   continue the walk by calling `walkUpDefault`.
///   `walkUpDefault` will do the following:
///   1. If the instruction of the definition is a projection, then it will continue
///     the walk by calling `walkUp` on the operand definition and an adjusted (pushed) path
///     to reflect that a further projection is needed to reach the value of interest from the new initial value.
///   2. If the instruction of the definition is a value construction such as `struct` and
///     the head of the path matches the instruction type then the walk continues
///     with a call to `walkUp` with initial value the operand defintion denoted by the path
///     and the suffix path as path since the target value can now be reached with fewer projections.
///     If the defining instruction of the value does not match the head of the path as in
///     `%t = tuple ...` and `"s0.t1"` then `unmatchedPath(%t, ...)` is called.
///   3. If the instruction is a forwarding instruction, such as a cast, the walk continues with `walkUp`
///     with the operand definition as initial value and same path.
///   4. If the instruction is not handled by this walker or the path is empty, then `rootDef` is called to
///     denote that the walk can't continue and that the definition of the target has been reached.
protocol ValueUseDefWalker {
  typealias Path = SmallProjectionPath
  associatedtype State
  
  /// Starting point of the walk. The implementor can decide to continue the walk by calling
  /// `walkUpDefault(value: value, path: path, state: state)` or
  /// do nothing.
  mutating func walkUp(value: Value, path: Path, state: State) -> WalkResult
  
  /// `rootDef` is called from `walkUpDefault` when the walk can't continue for this use since
  /// either
  /// * the defining instruction is unknown to the default walker
  /// * the `path` is empty (`""`) and therefore this is the definition of the target value.
  mutating func rootDef(value: Value, path: Path, state: State) -> WalkResult
  
  /// `unmatchedPath` is called from `walkUpDefault` when the defining instruction
  /// is unrelated to the `path` the walk should follow.
  mutating func unmatchedPath(value: Value, path: Path, state: State) -> WalkResult
  
  /// A client must implement this function to cache walking results.
  /// The function returns nil if the walk doesn't need to continue because
  /// the `def` was already handled before.
  /// In case the walk needs to be continued, this function returns the path
  /// and state for continuing the walk.
  mutating func shouldRecomputeUp(def: Value, path: Path, state: State) -> (Path, State)?
}

extension ValueUseDefWalker {
  mutating func walkUp(value: Value, path: Path, state: State) -> WalkResult {
    return walkUpDefault(value: value, path: path, state: state)
  }
  
  mutating func unmatchedPath(value: Value, path: Path, state: State) -> WalkResult {
    return .continueWalk
  }
  
  mutating func walkUpDefault(value def: Value, path: Path, state: State) -> WalkResult {
    switch def {
    case let str as StructInst:
      if let (index, path) = path.pop(kind: .structField) {
        return walkUp(value: str.operands[index].value, path: path, state: state)
      } else if path.topMatchesAnyValueField {
        return walkUpOperands(of: str, path: path, state: state)
      } else {
        return unmatchedPath(value: str, path: path, state: state)
      }
    case let t as TupleInst:
      if let (index, path) = path.pop(kind: .tupleField) {
        return walkUp(value: t.operands[index].value, path: path, state: state)
      } else if path.topMatchesAnyValueField {
        return walkUpOperands(of: t, path: path, state: state)
      } else {
        return unmatchedPath(value: t, path: path, state: state)
      }
    case let e as EnumInst:
      if let path = path.popIfMatches(.enumCase, index: e.caseIndex),
         let operand = e.operand {
        return walkUp(value: operand, path: path, state: state)
      } else {
        return unmatchedPath(value: e, path: path, state: state)
      }
    case let se as StructExtractInst:
      return walkUp(value: se.operand, path: path.push(.structField, index: se.fieldIndex), state: state)
    case let te as TupleExtractInst:
      return walkUp(value: te.operand, path: path.push(.tupleField, index: te.fieldIndex), state: state)
    case let ued as UncheckedEnumDataInst:
      return walkUp(value: ued.operand, path: path.push(.enumCase, index: ued.caseIndex), state: state)
    case let mvr as MultipleValueInstructionResult:
      let instruction = mvr.instruction
      if let ds = instruction as? DestructureStructInst {
        return walkUp(value: ds.operand, path: path.push(.structField, index: mvr.index), state: state)
      } else if let dt = instruction as? DestructureTupleInst {
        return walkUp(value: dt.operand, path: path.push(.tupleField, index: mvr.index), state: state)
      } else if let bcm = instruction as? BeginCOWMutationInst {
        return walkUp(value: bcm.operand, path: path, state: state)
      } else {
        return rootDef(value: mvr, path: path, state: state)
      }
    case is InitExistentialRefInst, is OpenExistentialRefInst,
      is BeginBorrowInst, is CopyValueInst,
      is UpcastInst, is UncheckedRefCastInst, is EndCOWMutationInst,
      is RefToBridgeObjectInst, is BridgeObjectToRefInst:
      return walkUp(value: (def as! Instruction).operands[0].value, path: path, state: state)
    case let arg as BlockArgument:
      if arg.isPhiArgument {
        for incoming in arg.incomingPhiValues {
          // `shouldRecomputeUp` is called to avoid cycles in the walk
          if let (path, state) = shouldRecomputeUp(def: incoming, path: path, state: state) {
            if walkUp(value: incoming, path: path, state: state) == .abortWalk {
              return .abortWalk
            }
          }
        }
        return .continueWalk
      }
      
      let block = arg.block
      if let pred = block.singlePredecessor,
         let se = pred.terminator as? SwitchEnumInst,
         let caseIdx = se.getUniqueCase(forSuccessor: block) {
        return walkUp(value: se.enumOp, path: path.push(.enumCase, index: caseIdx), state: state)
      }
      
      return rootDef(value: def, path: path, state: state)
    default:
      return rootDef(value: def, path: path, state: state)
    }
  }
  
  mutating func walkUpOperands(of def: SingleValueInstruction, path: Path, state: State) -> WalkResult {
    for operand in def.operands {
      // `shouldRecompute` is called to avoid exponential complexity in
      // programs like
      //
      // (%1, %2) = destructure_struct %0
      // %3 = struct $Struct %1 %2
      // (%4, %5) = destructure_struct %3
      // %6 = struct $Struct %4 %5
      if let (path, state) = shouldRecomputeUp(def: operand.value, path: path, state: state) {
        if walkUp(value: operand.value, path: path, state: state) == .abortWalk {
          return .abortWalk
        }
      }
    }
    return .continueWalk
  }
}

protocol AddressUseDefWalker {
  typealias Path = SmallProjectionPath
  associatedtype State
  
  /// Starting point of the walk. The implementor can decide to continue the walk by calling
  /// `walkUpDefault(address: address, path: path, state: state)` or
  /// do nothing.
  mutating func walkUp(address: Value, path: Path, state: State) -> WalkResult
  
  /// `rootDef` is called from `walkUpDefault` when the walk can't continue for this use since
  /// either
  /// * the defining instruction is unknown to the default walker
  /// * the `path` is empty (`""`) and therefore this is the definition of the target value.
  mutating func rootDef(address: Value, path: Path, state: State) -> WalkResult
  
  /// `unmatchedPath` is called from `walkUpDefault` when the defining instruction
  /// is unrelated to the `path` the walk should follow.
  mutating func unmatchedPath(address: Value, path: Path, state: State) -> WalkResult
}

extension AddressUseDefWalker {
  
  mutating func walkUp(address: Value, path: Path, state: State) -> WalkResult {
    return walkUpDefault(address: address, path: path, state: state)
  }
  
  mutating func unmatchedPath(address: Value, path: Path, state: State) -> WalkResult {
    return .continueWalk
  }
  
  mutating func walkUpDefault(address def: Value, path: Path, state: State) -> WalkResult {
    switch def {
    case let sea as StructElementAddrInst:
      return walkUp(address: sea.operand, path: path.push(.structField, index: sea.fieldIndex), state: state)
    case let tea as TupleElementAddrInst:
      return walkUp(address: tea.operand, path: path.push(.tupleField, index: tea.fieldIndex), state: state)
    case is InitEnumDataAddrInst, is UncheckedTakeEnumDataAddrInst:
      return walkUp(address: (def as! UnaryInstruction).operand,
                    path: path.push(.enumCase, index: (def as! EnumInstruction).caseIndex), state: state)
    case is InitExistentialAddrInst, is OpenExistentialAddrInst, is BeginAccessInst, is IndexAddrInst:
      // FIXME: for now `index_addr` is treated as a forwarding instruction since
      // SmallProjectionPath does not track indices.
      // This is ok since `index_addr` is eventually preceeded by a `tail_addr`
      // which has pushed a `"ct"` component on the path that matches any
      // `index_addr` address.
      return walkUp(address: (def as! Instruction).operands[0].value, path: path, state: state)
    case let mdi as MarkDependenceInst:
      return walkUp(address: mdi.operands[0].value, path: path, state: state)
    default:
      return rootDef(address: def, path: path, state: state)
    }
  }
}
