//===--- SimplifyPointerToAddress.swift -----------------------------------===//
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

import SIL

extension PointerToAddressInst : OnoneSimplifiable, SILCombineSimplifiable {

  func simplify(_ context: SimplifyContext) {
    if removeAddressToPointerToAddressPair(of: self, context) {
      return
    }
    if simplifyIndexRawPointer(of: self, context) {
      return
    }
    _ = optimizeAlignment(of: self, context)
  }
}

/// Remove a redundant pair of pointer-address conversions:
/// ```
///   %2 = address_to_pointer %1
///   %3 = pointer_to_address %2 [strict]
/// ```
/// -> replace all uses of %3 with %1.
///
private func removeAddressToPointerToAddressPair(
  of ptr2Addr: PointerToAddressInst,
  _ context: SimplifyContext
) -> Bool {
  guard let addr2Ptr = ptr2Addr.pointer as? AddressToPointerInst,
        ptr2Addr.isStrict,
        !ptr2Addr.hasIllegalUsesAfterLifetime(of: addr2Ptr, context)
  else {
    return false
  }

  if ptr2Addr.type == addr2Ptr.address.type {
    ptr2Addr.replace(with: addr2Ptr.address, context)
  } else {
    let cast = Builder(before: ptr2Addr, context).createUncheckedAddrCast(from: addr2Ptr.address, to: ptr2Addr.type)
    ptr2Addr.replace(with: cast, context)
  }
  return true
}

/// Replace an `index_raw_pointer` with a manually computed stride with `index_addr`:
/// ```
///    %1 = metatype $T.Type
///    %2 = builtin "strideof"<T>(%1) :
///    %3 = builtin "smul_with_overflow_Int64"(%idx, %2)
///    %4 = tuple_extract %3, 0
///    %5 = index_raw_pointer %ptr, %4
///    %6 = pointer_to_address %5 to [strict] $*T
/// ```
/// ->
/// ```
///    %2 = pointer_to_address %ptr to [strict] $*T
///    %3 = index_addr %2, %idx
/// ```
///
private func simplifyIndexRawPointer(of ptr2Addr: PointerToAddressInst, _ context: SimplifyContext) -> Bool {
  guard let indexRawPtr = ptr2Addr.pointer as? IndexRawPointerInst,
        let tupleExtract = indexRawPtr.index.lookThroughIndexScalarCast as? TupleExtractInst,
        let strideMul = tupleExtract.tuple as? BuiltinInst, strideMul.id == .SMulOver,
        let (index, strideType) = strideMul.indexAndStrideOfMultiplication,
        strideType == ptr2Addr.type.objectType
  else {
    return false
  }

  let builder = Builder(before: ptr2Addr, context)
  let newPtr2Addr = builder.createPointerToAddress(pointer: indexRawPtr.base, addressType: ptr2Addr.type,
                                                   isStrict: ptr2Addr.isStrict, isInvariant: ptr2Addr.isInvariant)
  let newIndex = builder.createCastIfNeeded(of: index, toIndexTypeOf: indexRawPtr)
  let indexAddr = builder.createIndexAddr(base: newPtr2Addr, index: newIndex, needStackProtection: false)
  ptr2Addr.replace(with: indexAddr, context)
  return true
}

/// Optimize the alignment of a `pointer_to_address` based on `Builtin.assumeAlignment`
/// ```
///    %1 = builtin "assumeAlignment"(%ptr, %align)
///    %2 = pointer_to_address %1 to [align=1] $*T
/// ```
/// ->
/// ```
///    %2 = pointer_to_address %ptr to [align=8] $*T
/// ```
/// or
/// ```
///    %2 = pointer_to_address %ptr to $*T
/// ```
///
/// The goal is to increase the alignment or to remove the attribute completely, which means that
/// the resulting address is naturaly aligned to its type.
///
private func optimizeAlignment(of ptr2Addr: PointerToAddressInst, _ context: SimplifyContext) -> Bool {
  guard let assumeAlign = ptr2Addr.pointer as? BuiltinInst, assumeAlign.id == .AssumeAlignment else {
    return false
  }

  if optimizeConstantAlignment(of: ptr2Addr, assumed: assumeAlign, context) {
    return true
  }
  return optimizeTypeAlignment(of: ptr2Addr, assumed: assumeAlign, context)
}

/// Optimize the alignment based on an integer literal
/// ```
///    %align = integer_literal $Builtin.Int64, 16
///    %1 = builtin "assumeAlignment"(%ptr, %align)
///    %2 = pointer_to_address %1 to [align=1] $*T
/// ```
/// ->
/// ```
///    %2 = pointer_to_address %ptr to [align=16] $*T
/// ```
private func optimizeConstantAlignment(
  of ptr2Addr: PointerToAddressInst,
  assumed assumeAlign: BuiltinInst,
  _ context: SimplifyContext
) -> Bool {
  guard let alignLiteral = assumeAlign.arguments[1] as? IntegerLiteralInst,
        let assumedAlignment = alignLiteral.value
  else {
    return false
  }

  ptr2Addr.operand.set(to: assumeAlign.arguments[0], context)

  if assumedAlignment == 0 {
    // A zero alignment means that the pointer is aligned to the natural alignment of the address type.
    ptr2Addr.set(alignment: nil, context)
  } else {
    if let oldAlignment = ptr2Addr.alignment, assumedAlignment <= oldAlignment {
      // Avoid decreasing the alignment, which would be a pessimisation.
      return true
    }
    ptr2Addr.set(alignment: assumedAlignment, context)
  }
  return true
}

/// Remove the alignment attribute if the alignment is assumed to be the natural alignment of the address type.
/// ```
//     %align = builtin "alignof"<T>(%0 : $@thin T.Type)
///    %1 = builtin "assumeAlignment"(%ptr, %align)
///    %2 = pointer_to_address %1 to [align=1] $*T
/// ```
/// ->
/// ```
///    %2 = pointer_to_address %ptr to $*T
/// ```
private func optimizeTypeAlignment(
  of ptr2Addr: PointerToAddressInst,
  assumed assumeAlign: BuiltinInst,
  _ context: SimplifyContext
) -> Bool {
  guard let alignOf = assumeAlign.arguments[1].lookThroughIntCasts as? BuiltinInst, alignOf.id == .Alignof,
        alignOf.alignOrStrideType == ptr2Addr.type.objectType
  else {
    return false
  }
  let pointer = assumeAlign.arguments[0]
  ptr2Addr.set(alignment: nil, context)
  ptr2Addr.operand.set(to: pointer, context)
  return true
}

private extension PointerToAddressInst {

  /// Checks if the `pointer_to_address` has uses outside the scope of the `baseAddress`.
  /// In such a case removing the `address_to_pointer`-`pointer_to_address` pair would result in
  /// invalid SIL. For example:
  /// ```
  ///   %1 = alloc_stack $T
  ///   %2 = address_to_pointer %1
  ///   dealloc_stack %1
  ///   %3 = pointer_to_address %2
  ///   %4 = load %3
  /// ```
  /// or
  /// ```
  ///   %1 = begin_borrow %0
  ///   %2 = ref_element_addr %1, #C.x
  ///   %3 = address_to_pointer %2
  ///   end_borrow %1
  ///   %4 = pointer_to_address %3
  ///   %5 = load %4
  /// ```
  func hasIllegalUsesAfterLifetime(of baseAddress: AddressToPointerInst, _ context: SimplifyContext) -> Bool {
    
    var lifetimeFrontier = InstructionSet(context)
    defer { lifetimeFrontier.deinitialize() }

    switch baseAddress.address.accessBase.addEndLifetimeUses(to: &lifetimeFrontier, context) {
    case .unknownLifetime:
      return true
    case .unlimitedLifetime:
      return false
    case .limitedLifetime:
      var addressUses = AddressUses(of: self, context)
      defer { addressUses.deinitialize() }
      return addressUses.hasUsesOutside(of: lifetimeFrontier, beginInstruction: baseAddress)
    }
  }
}

private extension AccessBase {
  func addEndLifetimeUses(to frontier: inout InstructionSet, _ context: SimplifyContext) -> Result {
    switch self {
    case .stack(let allocStack):
      frontier.insert(contentsOf: allocStack.deallocations)
      return .limitedLifetime
    case .global, .argument, .pointer:
      return .unlimitedLifetime
    case .storeBorrow(let storeBorrow):
      frontier.insert(contentsOf: storeBorrow.endBorrows)
      return .limitedLifetime
    default:
      guard let ref = reference else {
        return .unknownLifetime
      }
      switch ref.ownership {
      case .owned:
        frontier.insert(contentsOf: ref.uses.endingLifetime.users)
        return .limitedLifetime
      case .guaranteed:
        for borrowIntroducer in ref.getBorrowIntroducers(context) {
          frontier.insert(contentsOf: borrowIntroducer.scopeEndingOperands.users)
        }
        return .limitedLifetime
      case .none:
        // Not in an OSSA function.
        return .unlimitedLifetime
      case .unowned:
        return .unknownLifetime
      }
    }
  }

  enum Result {
    case unknownLifetime, unlimitedLifetime, limitedLifetime
  }
}

private struct AddressUses : AddressDefUseWalker {
  var users: InstructionWorklist

  init(of address: Value, _ context: SimplifyContext) {
    users = InstructionWorklist(context)
    _ = walkDownUses(ofAddress: address, path: UnusedWalkingPath())
  }

  mutating func deinitialize() {
    users.deinitialize()
  }

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    users.pushIfNotVisited(address.instruction)
    return .continueWalk
  }

  mutating func hasUsesOutside(of lifetimeFrontier: InstructionSet, beginInstruction: Instruction) -> Bool {
    while let inst = users.pop() {
      if lifetimeFrontier.contains(inst) {
        return true
      }
      users.pushPredecessors(of: inst, ignoring: beginInstruction)
    }
    return false
  }
}

private extension Value {
  var lookThroughIntCasts: Value {
    guard let builtin = self as? BuiltinInst else {
      return self
    }
    switch builtin.id {
    case .ZExtOrBitCast, .SExtOrBitCast, .TruncOrBitCast:
      return builtin.arguments[0].lookThroughIntCasts
    default:
      return self
    }
  }
}

private extension BuiltinInst {
  var indexAndStrideOfMultiplication : (index: Value, strideType: Type)? {
    assert(id == .SMulOver)
    if let strideOf = arguments[0].lookThroughIntCasts as? BuiltinInst, strideOf.id == .Strideof {
      return (index: arguments[1], strideType: strideOf.alignOrStrideType)
    }
    if let strideOf = arguments[1].lookThroughIntCasts as? BuiltinInst, strideOf.id == .Strideof {
      return (index: arguments[0], strideType: strideOf.alignOrStrideType)
    }
    return nil
  }

  var alignOrStrideType: Type {
    substitutionMap.replacementTypes[0].loweredType(in: parentFunction)
  }
}

private extension Builder {
  func createCastIfNeeded(of index: Value, toIndexTypeOf indexRawPtr: IndexRawPointerInst) -> Value {
    if let cast = indexRawPtr.index as? BuiltinInst {
      assert(cast.id == .TruncOrBitCast || cast.id == .SExtOrBitCast)
      return createBuiltin(name: cast.name, type: cast.type, arguments: [index])
    }
    return index
  }
}
