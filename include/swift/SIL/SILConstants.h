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
// This defines an interface to represent SIL level structured constants in a
// memory efficient way.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_CONSTANTS_H
#define SWIFT_SIL_CONSTANTS_H

#include "swift/SIL/SILValue.h"
#include "llvm/Support/CommandLine.h"


namespace swift {
class SingleValueInstruction;
class SILValue;
class SILBuilder;
class SerializedSILLoader;

struct APIntSymbolicValue;
struct ArraySymbolicValue;
struct EnumWithPayloadSymbolicValue;
struct UnknownSymbolicValue;

extern llvm::cl::opt<unsigned> ConstExprLimit;

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
private:
  enum RepresentationKind {
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

    /// This value is a struct or tuple of constants.  This is tracked by the
    /// "aggregate" member of the value union.
    RK_Aggregate,
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

    /// When this SymbolicValue is of "Aggregate" kind, this pointer stores
    /// information about the array elements and count.
    const SymbolicValue *aggregate;
  } value;

  RepresentationKind representationKind : 8;

  union {
    /// This is the reason code for RK_Unknown values.
    UnknownReason unknown_reason : 32;

    /// This is the number of bits in an RK_Integer or RK_IntegerInline
    /// representation, which makes the number of entries in the list derivable.
    unsigned integer_bitwidth;

    /// This is the number of elements for an RK_Aggregate representation.
    unsigned aggregate_numElements;
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

    /// This can be an array, struct, tuple, etc.
    Aggregate,
  };

  /// For constant values, return the type classification of this value.
  Kind getKind() const;

  /// Return true if this represents a constant value.
  bool isConstant() const {
    auto kind = getKind();
    return kind != Unknown;
  }

  static SymbolicValue getUnknown(SILNode *node, UnknownReason reason,
                                  llvm::ArrayRef<SourceLoc> callStack,
                                  ASTContext &astContext);

  /// Return true if this represents an unknown result.
  bool isUnknown() const { return getKind() == Unknown; }

  /// Return the call stack for an unknown result.
  ArrayRef<SourceLoc> getUnknownCallStack() const;

  /// Return the node that triggered an unknown result.
  SILNode *getUnknownNode() const;

  /// Return the reason an unknown result was generated.
  UnknownReason getUnknownReason() const;

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

  static SymbolicValue getInteger(int64_t value, unsigned bitWidth);
  static SymbolicValue getInteger(const APInt &value,
                                  ASTContext &astContext);

  APInt getIntegerValue() const;
  unsigned getIntegerValueBitWidth() const;

  /// This returns an aggregate value with the specified elements in it.  This
  /// copies the elements into the specified ASTContext.
  static SymbolicValue getAggregate(ArrayRef<SymbolicValue> elements,
                                    ASTContext &astContext);

  ArrayRef<SymbolicValue> getAggregateValue() const;

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

  /// Clone this SymbolicValue into the specified ASTContext and return the new
  /// version.  This only works for valid constants.
  SymbolicValue cloneInto(ASTContext &astContext) const;

  void print(llvm::raw_ostream &os, unsigned indent = 0) const;
  void dump() const;
};

static_assert(sizeof(SymbolicValue) == 2 * sizeof(void *),
              "SymbolicValue should stay small and POD");

inline llvm::raw_ostream &operator<<(llvm::raw_ostream &os, SymbolicValue val) {
  val.print(os);
  return os;
}

} // end namespace swift

#endif
