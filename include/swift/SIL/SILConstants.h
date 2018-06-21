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
struct StringSymbolicValue;
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

  /// A control flow loop was found.
  Loop,

  /// Integer overflow detected.
  Overflow,

  /// Unspecified trap detected.
  Trap,
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

    /// This value is represented with a bump-pointer allocated APInt.
    /// TODO: We could store small integers into the union inline to avoid
    /// allocations if it ever matters.
    RK_Integer,

    /// This value is represented with a bump-pointer allocated APFloat.
    /// TODO: We could store small floats into the union inline to avoid
    /// allocations if it ever matters.
    RK_Float,

    /// This value is represented with a bump-pointer allocated char array
    /// representing a UTF-8 encoded string.
    RK_String,

    /// This value is an array, struct, or tuple of constants.  This is
    /// tracked by the "aggregate" member of the value union.  Note that we
    /// cheat and represent single-element structs as the value of their
    /// element (since they are so common).
    RK_Aggregate,
  };

  RepresentationKind representationKind : 8;

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
    APFloatSymbolicValue *floatingPoint;

    /// When this SymbolicValue is of "String" kind, this pointer stores
    /// information about the StringRef value it holds.
    StringSymbolicValue *string;

    /// When this SymbolicValue is of "Aggregate" kind, this pointer stores
    /// information about the array elements, count, and element type.
    AggregateSymbolicValue *aggregate;
  } value;

  union {
    UnknownReason unknown_reason;

    // FIXME: Move function_conformance out of this union.  I want to make sure
    // that SymbolicValue stays exactly equal to two words.  The witness method
    // conformance case should be handled as a separate representation and be
    // bump pointer allocated.
    // TODO: Add a static_assert about the size of SymbolicValue.
    void *function_conformance;
    //unsigned integer_bitwidth;
    // ...
  } aux;

public:

  /// This enum is used to indicate the sort of value held by a SymbolicValue
  /// independent of its concrete representation.  This is the public
  /// interface to SymbolicValue.
  enum Kind {
    /// This is a value that isn't a constant.
    Unknown,

    /// This is a known metatype value.
    Metatype,

    /// This is a function, potentially containing a conformance if the function
    /// was resolved from a witness_method lookup.
    Function,

    /// This is an integer constant.
    Integer,

    /// This is a floating point constant.
    Float,

    /// String values may have SIL type of Builtin.RawPointer or Builtin.Word
    /// type.
    String,

    /// This can be an array, struct, tuple, etc.
    Aggregate,

    /// These values are generally only seen internally to the system, external
    /// clients shouldn't have to deal with them.
    UninitMemory
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

  bool isUnknown() const {
    return getKind() == Unknown;
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
    result.aux.function_conformance = nullptr;
    return result;
  }

  static SymbolicValue getFunction(SILFunction *fn,
                                   ProtocolConformanceRef conformance) {
    assert(fn && "Function cannot be null");
    SymbolicValue result;
    result.representationKind = RK_Function;
    result.value.function = fn;
    result.aux.function_conformance = conformance.getOpaqueValue();
    return result;
  }

  std::pair<SILFunction *, Optional<ProtocolConformanceRef>>
  getFunctionValue() const {
    assert(getKind() == Function);
    Optional<ProtocolConformanceRef> conf;
    if (auto opaqueConf = aux.function_conformance)
      conf = ProtocolConformanceRef::getFromOpaqueValue(opaqueConf);

    return std::make_pair(value.function, conf);
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

  /// Returns a SymbolicValue representing a UTF-8 encoded string.
  static SymbolicValue getString(const StringRef string,
                                 llvm::BumpPtrAllocator &allocator);

  /// Returns the UTF-8 encoded string underlying a SymbolicValue.
  StringRef getStringValue() const;

  /// This returns an aggregate value with the specified elements in it.  This
  /// copies the elements into the specified allocator.
  static SymbolicValue getAggregate(ArrayRef<SymbolicValue> elements,
                                    llvm::BumpPtrAllocator &allocator);

  ArrayRef<SymbolicValue> getAggregateValue() const;

  /// Given that this is an 'Unknown' value, emit diagnostic notes providing
  /// context about what the problem is.  If there is no location for some
  /// reason, we fall back to using the specified location.
  void emitUnknownDiagnosticNotes(SILLocation fallbackLoc);

  void print(llvm::raw_ostream &os, unsigned indent = 0) const;
  void dump() const;
};

/// SWIFT_ENABLE_TENSORFLOW
/// A graph operation attribute, used by GraphOperationInst.
/// Attributes have a name and a constant value.
struct GraphOperationAttribute {
  Identifier name;
  SymbolicValue value;
};

} // end namespace swift

#endif
