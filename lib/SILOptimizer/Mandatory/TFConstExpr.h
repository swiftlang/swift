//===--- TFConstExpr.h - TensorFlow constant expressions --------*- C++ -*-===//
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
// This defines an interface to evaluate Swift language level constant
// expressions.  Its model is intended to be general and reasonably powerful,
// with the goal of standardization in a future version of Swift.
//
// Constant expressions are functions without side effects that take constant
// values and return constant values.  These constants may be integer, floating
// point, and string values, or arrays thereof (up to 1024 elements).  We allow
// abstractions to be built out of fragile structs and tuples.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_TF_CONSTEXPR_H
#define SWIFT_SILOPTIMIZER_TF_CONSTEXPR_H

#include "llvm/Support/Allocator.h"
#include "swift/SIL/SILValue.h"

namespace swift {
  class SingleValueInstruction;
  class SILValue;
  class SILBuilder;
  class SerializedSILLoader;

namespace tf {
  struct APIntSymbolicValue;
  struct APFloatSymbolicValue;
  struct AddressSymbolicValue;
  struct AggregateSymbolicValue;

  /// This is the symbolic value tracked for each SILValue in a scope.  We
  /// support multiple representational forms for the constant node in order to
  /// avoid pointless memory bloat + copying.  This is intended to be a
  /// light-weight POD type we can put in hashtables.
  class SymbolicValue {
    enum ValueKind {
      /// This value is an alloc stack that is has not (yet) been initialized
      /// by flow-sensitive analysis.
      UninitMemory,

      /// This symbolic value cannot be determined, carries multiple values
      /// (i.e., varies dynamically at the top level), or is of some type that
      /// we cannot analyze and propagate (e.g. NSObject).
      ///
      /// TODO: include state (e.g. the bad instruction in question) that
      /// indicates why this was unknown so clients can produce a useful
      /// diagnostic.
      Unknown,

      /// This value is known to be a metatype reference.  The type is stored
      /// in the "metatype" member.
      Metatype,

      /// This value is known to be a function reference, e.g. through
      /// function_ref directly, or a devirtualized method reference.
      Function,

      /// This value is a constant, and tracked by the "inst" member of the
      /// value union.  This could be an integer, floating point, string, or
      /// metatype value.
      Inst,

      /// This value is represented with a bump pointer allocated APInt.
      /// TODO: We could store small integers into the union inline to avoid
      /// allocations if it ever matters.
      Integer,

      /// This value is represented with a bump pointer allocated APFloat.
      /// TODO: We could store small floats into the union inline to avoid
      /// allocations if it ever matters.
      Float,

      /// This value is a pointer to a tracked memory location, along with zero
      /// or more indices (tuple indices, struct field indices, etc) into the
      /// value if it is an aggregate.
      ///
      Address,

      /// This value is an array, struct, or tuple of constants.  This is
      /// tracked by the "aggregate" member of the value union.  Note that we
      /// cheat and represent single-element structs as the value of their
      /// element (since they are so common).
      Aggregate,
    } kind;

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

    /// For constant values, this enum is used to discriminate across the kinds
    /// of constant this holds, which allows use of the accessors.
    enum TypeKind {
      TKMetatype, TKFunction, TKInteger, TKFloat, TKString, TKAddress,
      TKAggregate
    };

    /// For constant values, return the type classification of this value.
    TypeKind getTypeKind() const;

    /// Return true if this represents a constant value.
    bool isConstant() const {
      return kind != UninitMemory && kind != Unknown;
    }

    static SymbolicValue getUnknown(SILNode *node) {
      assert(node && "node must be present");
      SymbolicValue result;
      result.kind = Unknown;
      result.value.unknown = node;
      return result;
    }

    static SymbolicValue getUninitMemory() {
      SymbolicValue result;
      result.kind = UninitMemory;
      return result;
    }

    // Return true if this is an uninitialized memory buffer.
    bool isUninitMemory() const {
      return kind == UninitMemory;
    }

    static SymbolicValue getMetatype(CanType type) {
      SymbolicValue result;
      result.kind = Metatype;
      result.value.metatype = type.getPointer();
      return result;
    }

    CanType getMetatypeValue() const {
      assert(kind == Metatype);
      return CanType(value.metatype);
    }

    static SymbolicValue getFunction(SILFunction *fn) {
      assert(fn && "Function cannot be null");
      SymbolicValue result;
      result.kind = Function;
      result.value.function = fn;
      return result;
    }

    bool isFunction() const { return kind == Function; }

    SILFunction *getFunctionValue() const {
      assert(isFunction());
      return value.function;
    }

    static SymbolicValue getConstantInst(SingleValueInstruction *inst) {
      assert(inst && "inst value must be present");
      SymbolicValue result;
      result.kind = Inst;
      result.value.inst = inst;
      return result;
    }

    SingleValueInstruction *getConstantInstIfPresent() const {
      return kind == Inst ? value.inst : nullptr;
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
      return kind == Address;
    }

    SILValue getAddressBase() const;
    ArrayRef<unsigned> getAddressIndices() const;


    /// This returns an aggregate value with the specified elements in it.  This
    /// copies the elements into the specified allocator.
    static SymbolicValue getAggregate(ArrayRef<SymbolicValue> elements,
                                      llvm::BumpPtrAllocator &allocator);


    bool isAggregate() const {
      return kind == Aggregate;
    }
    ArrayRef<SymbolicValue> getAggregateValue() const;


    // TODO: getStringValue.

    /// Create and return a new constant literal instruction for the specified
    /// scalar constant value.
    SingleValueInstruction *
    emitConstantInst(SILBuilder &B, SILType type, SILLocation loc) const;

    void print(llvm::raw_ostream &os, unsigned indent = 0) const;
    void dump() const;
  };

  /// This class is the main entrypoint for evaluating constant expressions.  It
  /// also handles caching of previously computed constexpr results.
  class ConstExprEvaluator {
    /// This is a long-lived bump pointer allocator that holds the arguments and
    /// result values for the cached constexpr calls we have already analyzed.
    llvm::BumpPtrAllocator allocator;

    /// This is a handle to a loader for serialized code.
    const std::unique_ptr<SerializedSILLoader> silLoader;

    ConstExprEvaluator(const ConstExprEvaluator &) = delete;
    void operator=(const ConstExprEvaluator &) = delete;
  public:
    explicit ConstExprEvaluator(SILModule &m);
    ~ConstExprEvaluator();

    llvm::BumpPtrAllocator &getAllocator() { return allocator; }
    SerializedSILLoader &getSILLoader() const { return *silLoader; }

    /// Analyze the specified values to determine if they are constant values.
    /// This is done in code that is not necessarily itself a constexpr
    /// function.  The results are added to the results list which is a parallel
    /// structure to the input values.
    ///
    /// TODO: Return information about which callees were found to be
    /// constexprs, which would allow the caller to delete dead calls to them
    /// that occur after after folding them.
    void computeConstantValues(ArrayRef<SILValue> values,
                               SmallVectorImpl<SymbolicValue> &results);

    // Evaluate a call to the specified function as if it were a constant
    // expression, returning false and filling in `results` on success, or
    // returning true on failure.
    //
    // TODO: propagate a *good* error up, handling cases like "called a
    // non-constexpr", "constant expr is infinite or too complex", and
    // eventually things like "overflow detected for add with overflow traps".
    // This should include the full call stack for the failure, and should
    // specify the arguments passed to each call level.
    //
    bool evaluateAndCacheCall(SILFunction &fn, SubstitutionList substitutions,
                              ArrayRef<SymbolicValue> arguments,
                              SmallVectorImpl<SymbolicValue> &results);
  };
} // end namespace tf
} // end namespace swift
#endif
