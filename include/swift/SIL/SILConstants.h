//===--- SILConstants.h - SIL constant representation -----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This defines an interface to represent SIL level structured constants in an
// memory efficient way.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_CONSTANTS_H
#define SWIFT_SIL_CONSTANTS_H

#include "swift/SIL/SILValue.h"

namespace swift {
class SingleValueInstruction;
class SILValue;
class SILBuilder;
class SerializedSILLoader;

struct APIntSymbolicValue;
struct APFloatSymbolicValue;
struct AddressSymbolicValue;
struct AggregateSymbolicValue;

/// When we fail to constant fold a value, this captures a reason why,
/// allowing the caller to produce a specific diagnostic.  The "Unknown"
/// SymbolicValue representation also includes a pointer to the SILNode in
/// question that was problematic.
enum class UnknownReason {
  // TODO: Eliminate the default code, by making classifications for each
  // failure mode.
  Default,

  /// The constant expression was too big.  This is reported on a random
  /// instruction within the constexpr that triggered the issue.
  TooManyInstructions,
};


/// This is the symbolic value tracked for each SILValue in a scope.  We
/// support multiple representational forms for the constant node in order to
/// avoid pointless memory bloat + copying.  This is intended to be a
/// light-weight POD type we can put in hash tables and pass around by-value.
///
/// Internally, this value has multiple ways to represent the same sorts of
/// symbolic values (e.g. to save memory).  It provides a simpler public
/// interface though.
class SymbolicValue {
  enum RepresentationKind {
    /// This value is an alloc stack that is has not (yet) been initialized
    /// by flow-sensitive analysis.
    RK_UninitMemory,

    /// This symbolic value cannot be determined, carries multiple values
    /// (i.e., varies dynamically at the top level), or is of some type that
    /// we cannot analyze and propagate (e.g. NSObject).
    ///
    RK_Unknown,

    /// This value is known to be a metatype reference.  The type is stored
    /// in the "metatype" member.
    RK_Metatype,

    /// This value is known to be a function reference, e.g. through
    /// function_ref directly, or a devirtualized method reference.
    RK_Function,

    /// This value is a constant, and tracked by the "inst" member of the
    /// value union.  This could be an integer, floating point, string, or
    /// metatype value.
    RK_Inst,

    /// This value is represented with a bump pointer allocated APInt.
    /// TODO: We could store small integers into the union inline to avoid
    /// allocations if it ever matters.
    RK_Integer,

    /// This value is represented with a bump pointer allocated APFloat.
    /// TODO: We could store small floats into the union inline to avoid
    /// allocations if it ever matters.
    RK_Float,

    /// This value is a pointer to a tracked memory location, along with zero
    /// or more indices (tuple indices, struct field indices, etc) into the
    /// value if it is an aggregate.
    ///
    RK_Address,

    /// This value is an array, struct, or tuple of constants.  This is
    /// tracked by the "aggregate" member of the value union.  Note that we
    /// cheat and represent single-element structs as the value of their
    /// element (since they are so common).
    RK_Aggregate,
  };

  RepresentationKind representationKind : 8;

  union {
    UnknownReason unknown_reason;
    //unsigned integer_bitwidth;
    // ...
  } aux;

  union {
    /// When the value is Unknown, this contains the value that was the
    /// unfoldable part of the computation.
    ///
    /// TODO: make this a more rich representation.
    SILNode *unknown;

    /// This is always a SILType with an object category.  This is the value
    /// of the underlying instance type, not the MetatypeType.
    TypeBase *metatype;

    SILFunction *function;

    /// When this SymbolicValue is of "Inst" kind, this contains a
    /// pointer to the instruction whose value this holds.  This is known to
    /// be one of a closed set of constant instructions:
    ///    IntegerLiteralInst, FloatLiteralInst, StringLiteralInst
    SingleValueInstruction *inst;

    /// When this SymbolicValue is of "Integer" kind, this pointer stores
    /// information about the APInt value it holds.
    APIntSymbolicValue *integer;

    /// When this SymbolicValue is of "Float" kind, this pointer stores
    /// information about the APFloat value it holds.
    APFloatSymbolicValue *float_;

    /// When this SymbolicValue is of "Address" kind, this pointer stores
    /// info about the base and the indices for the address.
    AddressSymbolicValue *address;

    /// When this SymbolicValue is of "Aggregate" kind, this pointer stores
    /// information about the array elements, count, and element type.
    AggregateSymbolicValue *aggregate;
  } value;

public:

  /// This enum is used to indicate the sort of value held by a SymbolicValue
  /// independent of its concrete representation.  This is the public
  /// interface to SymbolicValue.
  enum Kind {
    Unknown, Metatype, Function, Integer, Float, String, Aggregate,

    // These values are generally only seen internally to the system, external
    // clients shouldn't have to deal with them.
    Address, UninitMemory
  };

  /// For constant values, return the type classification of this value.
  Kind getKind() const;

  /// Return true if this represents a constant value.
  bool isConstant() const {
    auto kind = getKind();
    return kind != Unknown && kind != UninitMemory;
  }

  static SymbolicValue getUnknown(SILNode *node, UnknownReason reason) {
    assert(node && "node must be present");
    SymbolicValue result;
    result.representationKind = RK_Unknown;
    result.value.unknown = node;
    result.aux.unknown_reason = reason;
    return result;
  }

  /// Return information about an unknown result, including the SIL node that
  /// is a problem, and the reason it is an issue.
  std::pair<SILNode *, UnknownReason> getUnknownValue() const {
    assert(representationKind == RK_Unknown);
    return { value.unknown, aux.unknown_reason };
  }

  static SymbolicValue getUninitMemory() {
    SymbolicValue result;
    result.representationKind = RK_UninitMemory;
    return result;
  }

  static SymbolicValue getMetatype(CanType type) {
    SymbolicValue result;
    result.representationKind = RK_Metatype;
    result.value.metatype = type.getPointer();
    return result;
  }

  CanType getMetatypeValue() const {
    assert(representationKind == RK_Metatype);
    return CanType(value.metatype);
  }

  static SymbolicValue getFunction(SILFunction *fn) {
    assert(fn && "Function cannot be null");
    SymbolicValue result;
    result.representationKind = RK_Function;
    result.value.function = fn;
    return result;
  }

  SILFunction *getFunctionValue() const {
    assert(getKind() == Function);
    return value.function;
  }

  static SymbolicValue getConstantInst(SingleValueInstruction *inst) {
    assert(inst && "inst value must be present");
    SymbolicValue result;
    result.representationKind = RK_Inst;
    result.value.inst = inst;
    return result;
  }

  // TODO: Remove this, it is just needed because deabstraction has no SIL
  // instruction of its own.
  SingleValueInstruction *getConstantInstIfPresent() const {
    return representationKind == RK_Inst ? value.inst : nullptr;
  }

  static SymbolicValue getInteger(const APInt &value,
                                  llvm::BumpPtrAllocator &allocator);

  APInt getIntegerValue() const;

  static SymbolicValue getFloat(const APFloat &value,
                                llvm::BumpPtrAllocator &allocator);

  APFloat getFloatValue() const;

  /// Get a SymbolicValue corresponding to a memory object with an optional
  /// list of indices into it.  This is used by (e.g.) a struct_element_addr
  /// of a stack_alloc.
  static SymbolicValue getAddress(SILValue base,
                                  ArrayRef<unsigned> indices,
                                  llvm::BumpPtrAllocator &allocator);

  /// Accessors for Address SymbolicValue's.
  bool isAddress() const {
    return getKind() == Address;
  }

  SILValue getAddressBase() const;
  ArrayRef<unsigned> getAddressIndices() const;


  /// This returns an aggregate value with the specified elements in it.  This
  /// copies the elements into the specified allocator.
  static SymbolicValue getAggregate(ArrayRef<SymbolicValue> elements,
                                    llvm::BumpPtrAllocator &allocator);

  ArrayRef<SymbolicValue> getAggregateValue() const;

  // TODO: getStringValue.


  /// Given that this is an 'Unknown' value, emit diagnostic notes providing
  /// context about what the problem is.
  void emitUnknownDiagnosticNotes();

  void print(llvm::raw_ostream &os, unsigned indent = 0) const;
  void dump() const;
};

} // end namespace swift

#endif
