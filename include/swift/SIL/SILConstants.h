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

struct APFloatSymbolicValue;
struct APIntSymbolicValue;
struct ArraySymbolicValue;
struct DerivedAddressValue;
struct EnumWithPayloadSymbolicValue;
struct SymbolicValueMemoryObject;
struct UnknownSymbolicValue;

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

/// Determines how to substitute the function's generic arguments when
/// evaluating an apply of the function.
enum class FunctionSubConvention {
  /// Use the apply instruction's subtitution map.
  Normal,

  /// Transform the apply instruction's subtitution map that is written in terms
  /// of the requirement's generic signature to a substitution map that is
  /// written in terms of the witness signature. (See
  // `getWitnessMethodSubstitutions` in Devirtualize.cpp for more information).
  Witness
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
private:
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

    /// This value is represented with a bump-pointer allocated APInt.
    RK_Integer,

    /// This value is represented with an inline integer representation.
    RK_IntegerInline,

    /// This value is represented with a bump-pointer allocated APFloat.
    RK_Float,

    /// This value is representation with an inline float representation.
    RK_Float32,

    /// This value is representation with an inline double representation.
    RK_Float64,

    /// This value is represented with a bump-pointer allocated char array
    /// representing a UTF-8 encoded string.
    RK_String,

    /// This value is an array, struct, or tuple of constants.  This is
    /// tracked by the "aggregate" member of the value union.
    RK_Aggregate,

    /// This value is an enum with no payload.
    RK_Enum,

    /// This value is an enum with a payload.
    RK_EnumWithPayload,

    /// This represents a direct reference to the address of a memory object.
    RK_DirectAddress,

    /// This represents an index *into* a memory object.
    RK_DerivedAddress,

    /// This represents an array value.
    RK_Array,

    /// This represents an address of a memory object containing an array.
    RK_ArrayAddress,
  };

  union {
    /// When the value is Unknown, this contains information about the
    /// unfoldable part of the computation.
    UnknownSymbolicValue *unknown;

    /// This is always a SILType with an object category.  This is the value
    /// of the underlying instance type, not the MetatypeType.
    TypeBase *metatype;

    SILFunction *function;

    /// When this SymbolicValue is of "Integer" kind, this pointer stores
    /// the words of the APInt value it holds.
    uint64_t *integer;

    /// This holds the bits of an integer for an inline representation.
    uint64_t integerInline;

    /// When this SymbolicValue is of "Float" kind, this pointer stores
    /// information about the APFloat value it holds.
    APFloatSymbolicValue *floatingPoint;

    /// When this is an RK_Float32, this holds the inline representation.
    float float32;

    /// When this is an RK_Float64, this holds the inline representation.
    double float64;

    /// When this SymbolicValue is of "String" kind, this pointer stores
    /// information about the StringRef value it holds.
    const char *string;

    /// When this SymbolicValue is of "Aggregate" kind, this pointer stores
    /// information about the array elements and count.
    const SymbolicValue *aggregate;

    /// When this SymbolicValue is of "Enum" kind, this pointer stores
    /// information about the enum case type.
    EnumElementDecl *enumVal;

    /// When this SymbolicValue is of "EnumWithPayload" kind, this pointer
    /// stores information about the enum case type and its payload.
    EnumWithPayloadSymbolicValue *enumValWithPayload;

    /// When the representationKind is "DirectAddress", this pointer is the
    /// memory object referenced.
    SymbolicValueMemoryObject *directAddress;

    /// When this SymbolicValue is of "DerivedAddress" kind, this pointer stores
    /// information about the memory object and access path of the access.
    DerivedAddressValue *derivedAddress;

    /// For RK_Array, this is the elements of the array.
    ArraySymbolicValue *array;

    /// For RK_ArrayAddress, this is the memory object referenced.
    SymbolicValueMemoryObject *arrayAddress;
  } value;

  RepresentationKind representationKind : 8;

  union {
    /// This is the reason code for RK_Unknown values.
    UnknownReason unknown_reason : 32;

    /// This is the number of bits in an RK_Integer or RK_IntegerInline
    /// representation, which makes the number of entries in the list derivable.
    unsigned integer_bitwidth;

    /// This is the number of bytes for an RK_String representation.
    unsigned string_numBytes;

    /// This is the number of elements for an RK_Aggregate representation.
    unsigned aggregate_numElements;

    /// This is the FunctionSubConvention for an RK_Function representation.
    FunctionSubConvention function_subConvention;
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

    /// This is a function, represented as a SILFunction.
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

    /// This is an enum without payload.
    Enum,

    /// This is an enum with payload (formally known as "associated value").
    EnumWithPayload,

    /// This value represents the address of, or into, a memory object.
    Address,

    /// This value represents an array value.
    Array,

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

  static SymbolicValue getUnknown(SILNode *node, UnknownReason reason,
                                  llvm::ArrayRef<SourceLoc> callStack,
                                  llvm::BumpPtrAllocator &allocator);

  /// Return true if this represents an unknown result.
  bool isUnknown() const { return getKind() == Unknown; }

  /// Return the call stack for an unknown result.
  ArrayRef<SourceLoc> getUnknownCallStack() const;

  /// Return the node that triggered an unknown result.
  SILNode *getUnknownNode() const;

  /// Return the reason an unknown result was generated.
  UnknownReason getUnknownReason() const;

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

  static SymbolicValue getFunction(SILFunction *fn,
                                   FunctionSubConvention fnSubConv) {
    assert(fn && "Function cannot be null");
    SymbolicValue result;
    result.representationKind = RK_Function;
    result.value.function = fn;
    result.aux.function_subConvention = fnSubConv;
    return result;
  }

  SILFunction *getFunctionValue() const {
    assert(getKind() == Function);
    return value.function;
  }

  FunctionSubConvention getFunctionSubConvention() const {
    assert(getKind() == Function);
    return aux.function_subConvention;
  }

  static SymbolicValue getInteger(int64_t value, unsigned bitWidth);
  static SymbolicValue getInteger(const APInt &value,
                                  llvm::BumpPtrAllocator &allocator);

  APInt getIntegerValue() const;
  unsigned getIntegerValueBitWidth() const;

  static SymbolicValue getFloat(const APFloat &value,
                                llvm::BumpPtrAllocator &allocator);

  APFloat getFloatValue() const;
  const llvm::fltSemantics *getFloatValueSemantics() const;

  /// Returns a SymbolicValue representing a UTF-8 encoded string.
  static SymbolicValue getString(StringRef string,
                                 llvm::BumpPtrAllocator &allocator);

  /// Returns the UTF-8 encoded string underlying a SymbolicValue.
  StringRef getStringValue() const;

  /// This returns an aggregate value with the specified elements in it.  This
  /// copies the elements into the specified allocator.
  static SymbolicValue getAggregate(ArrayRef<SymbolicValue> elements,
                                    llvm::BumpPtrAllocator &allocator);

  ArrayRef<SymbolicValue> getAggregateValue() const;

  /// This returns a constant Symbolic value for the enum case in `decl`, which
  /// must not have an associated value.
  static SymbolicValue getEnum(EnumElementDecl *decl) {
    assert(decl);
    SymbolicValue result;
    result.representationKind = RK_Enum;
    result.value.enumVal = decl;
    return result;
  }

  /// `payload` must be a constant.
  static SymbolicValue getEnumWithPayload(EnumElementDecl *decl,
                                          SymbolicValue payload,
                                          llvm::BumpPtrAllocator &allocator);

  EnumElementDecl *getEnumValue() const;

  SymbolicValue getEnumPayloadValue() const;

  /// Return a symbolic value that represents the address of a memory object.
  static SymbolicValue getAddress(SymbolicValueMemoryObject *memoryObject) {
    SymbolicValue result;
    result.representationKind = RK_DirectAddress;
    result.value.directAddress = memoryObject;
    return result;
  }

  /// Return a symbolic value that represents the address of a memory object
  /// indexed by a path.
  static SymbolicValue getAddress(SymbolicValueMemoryObject *memoryObject,
                                  ArrayRef<unsigned> indices,
                                  llvm::BumpPtrAllocator &allocator);

  /// Return the memory object of this reference along with any access path
  /// indices involved.
  SymbolicValueMemoryObject *
  getAddressValue(SmallVectorImpl<unsigned> &accessPath) const;

  /// Return just the memory object for an address value.
  SymbolicValueMemoryObject *getAddressValueMemoryObject() const;

  /// Produce an array of elements.

  static SymbolicValue getArray(ArrayRef<SymbolicValue> elements,
                                CanType elementType,
                                llvm::BumpPtrAllocator &allocator);
  static SymbolicValue
  getArrayAddress(SymbolicValueMemoryObject *memoryObject) {
    SymbolicValue result;
    result.representationKind = RK_ArrayAddress;
    result.value.directAddress = memoryObject;
    return result;
  }

  ArrayRef<SymbolicValue> getArrayValue(CanType &elementType) const;

  //===--------------------------------------------------------------------===//
  // Helpers

  /// Dig through single element aggregates, return the ultimate thing inside of
  /// it.  This is useful when dealing with integers and floats, because they
  /// are often wrapped in single-element struct wrappers.
  SymbolicValue lookThroughSingleElementAggregates() const;

  /// Given that this is an 'Unknown' value, emit diagnostic notes providing
  /// context about what the problem is.  If there is no location for some
  /// reason, we fall back to using the specified location.
  void emitUnknownDiagnosticNotes(SILLocation fallbackLoc);

  /// Clone this SymbolicValue into the specified allocator and return the new
  /// version.  This only works for valid constants.
  SymbolicValue cloneInto(llvm::BumpPtrAllocator &allocator) const;

  void print(llvm::raw_ostream &os, unsigned indent = 0) const;
  void dump() const;
};

static_assert(sizeof(SymbolicValue) == 2 * sizeof(void *),
              "SymbolicValue should stay small and POD");

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os, SymbolicValue val) {
  val.print(os);
  return os;
}

/// This is a representation of a memory object referred to be an address.
/// Memory objects may be mutated over their lifetime, but their overall type
/// remains the same.
struct SymbolicValueMemoryObject {
  Type getType() const { return type; }

  SymbolicValue getValue() const { return value; }
  void setValue(SymbolicValue newValue) { value = newValue; }

  /// Create a new memory object whose overall type is as specified.
  static SymbolicValueMemoryObject *create(Type type, SymbolicValue value,
                                           llvm::BumpPtrAllocator &allocator);

  /// Given that this memory object contains an aggregate value like
  /// {{1, 2}, 3}, and given an access path like [0,1], return the indexed
  /// element, e.g. "2" in this case.
  ///
  /// Returns uninit memory if the access path points at or into uninit memory.
  ///
  /// Precondition: The access path must be valid for this memory object's type.
  SymbolicValue getIndexedElement(ArrayRef<unsigned> accessPath);

  /// Given that this memory object contains an aggregate value like
  /// {{1, 2}, 3}, given an access path like [0,1], and given a scalar like "4",
  /// set the indexed element to the specified scalar, producing {{1, 4}, 3} in
  /// this case.
  ///
  /// Precondition: The access path must be valid for this memory object's type.
  void setIndexedElement(ArrayRef<unsigned> accessPath, SymbolicValue scalar,
                         llvm::BumpPtrAllocator &allocator);

private:
  const Type type;
  SymbolicValue value;

  SymbolicValueMemoryObject(Type type, SymbolicValue value)
      : type(type), value(value) {}
  SymbolicValueMemoryObject(const SymbolicValueMemoryObject &) = delete;
  void operator=(const SymbolicValueMemoryObject &) = delete;
};
} // end namespace swift

#endif
