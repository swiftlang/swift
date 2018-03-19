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
// abstractions to be built out of structs and tuples.
//
// TODO: Consider adding a '@constexpr' attribute.  On public APIs, this would
// be an API guarantee that the marked function is pure and always produces a
// constant value when given a constant inputs for its arguments, that ensures
// that the body of the function is serialized.  On internal APIs, it would
// provide the same checking without an API guarantee.
//
// NOTE: It is highly recommended that you have a strong grasp of the standard
// "Sparse Conditional Constant Propagation" (SCCP) compiler optimization
// algorithm in order to understand the implementation logic in this code.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_TF_CONSTEXPR_H
#define SWIFT_SILOPTIMIZER_TF_CONSTEXPR_H

#include "llvm/Support/Allocator.h"
#include "swift/SIL/SILValue.h"

namespace swift {
  class SILInstruction;
  class SILValue;

namespace tf {

  /// Constant expressions are evaluated for symbolic values in the SSA graph
  /// and SymbolicValue represents a unique identifier for these values that are
  /// used as keys in hashtables etc.
  ///
  /// We track the (recursive) elements of structs and tuples independently as
  /// in their exploded form, and each tracked element has an associated a
  /// lattice value.  For example, consider a SSA value made out of structs and
  /// tuple types written graphically as:
  ///
  ///   {{(Int, String)}, Float, NSObject}
  ///
  /// This SSA value will have four symbolic values associated with it: the
  /// Int will have index 0, the String will have index 1, the Float will have
  /// index 2, and the NSObject will have index 3.  In reality, we actually
  /// track the LLVM internal values inside of each of those.
  ///
  /// We notionally explode through fragile structs and tuple values, but treat
  /// Swift.Array and Swift.String (which are structs) as a special terminal
  /// values instead of tracking their contents.
  ///
  /// We also track SIL addresses (e.g. from an alloc_stack) as its contents.
  ///
  struct SymbolicValue {
    SILValue value;
    unsigned index;


    bool operator==(SymbolicValue rhs) const {
      return value == rhs.value && index == rhs.index;
    }
    bool operator!=(SymbolicValue rhs) const {
      return !(*this == rhs);
    }
    bool operator<(SymbolicValue rhs) const {
      return value < rhs.value || (value == rhs.value && index < rhs.index);
    }
  };

  /// This is the lattice value tracked for each SymbolicValue in a scope.  Each
  /// symbolic value may be multiple states as defined by the standard SCCP
  /// algorithm.  We support multiple representational forms for the constant
  /// node in order to avoid pointless memory bloat + copying.  This is intended
  /// to be a light-weight POD type we can put in hashtables.
  class LatticeValue {
    enum ValueKind {
      /// This value is not reachable.
      Undefined,

      /// This value is a constant, and tracked by the "inst" member of the
      /// value union.
      ConstantInst,

      /// This value is an array of constants, and is tracked by the "array"
      /// member of the value union.
      ConstantArray,

      /// This symbolic value cannot be determined, carries multiple values
      /// (i.e., varies dynamically), or is of some type that we cannot analyze
      /// and propagate (e.g. NSObject).
      Overdefined
    } kind;

    union {
      /// When this LatticeValue is of "ConstantInst" kind, this contains a
      /// pointer to the instruction whose value this holds.  This is known to
      /// be one of a closed set of constant instructions:
      ///    IntegerLiteralInst, FloatLiteralInst, or StringLiteralInst
      SILInstruction *inst;

      struct {
        /// This is a pointer to lattice elements;
        const LatticeValue *elements;

        /// This is the number of elements in the array.  We only support up to
        /// 1024 elements in our model to the possibility of compile time
        /// explosion.
        unsigned numElements;
      } array;

      /// TODO: Eventually should support bump pointer allocated APInt's and
      /// APFloat's, and string buffers to represent the product of constant
      /// folding.
    } value;

  public:
    /// LatticeValue constructors.
    static LatticeValue getUndefined() {
      LatticeValue result;
      result.kind = Undefined;
      return result;
    }
    static LatticeValue getOverdefined() {
      LatticeValue result;
      result.kind = Overdefined;
      return result;
    }
    static LatticeValue getConstant(SILInstruction *inst) {
      LatticeValue result;
      result.kind = ConstantInst;
      result.value.inst = inst;
      return result;
    }

    /// This returns a constant lattice value with the specified elements in it.
    /// This assumes that the elements lifetime has been managed for this.
    static LatticeValue getConstantArray(ArrayRef<LatticeValue> elements) {
      LatticeValue result;
      result.kind = ConstantArray;
      result.value.array.elements = elements.data();
      result.value.array.numElements = elements.size();
      return result;
    }


    /// Return true if this represents a constant value.
    bool isConstant() const {
      return kind != Undefined && kind != Overdefined;
    }
  };

  /// This class is the main entrypoint for evaluating constant expressions.  It
  /// also handles caching of previously computed constexpr results.
  class ConstExprEvaluator {
    /// This is a long-lived bump pointer allocator that holds the arguments and
    /// result values for the cached constexpr calls we have already analyzed.
    llvm::BumpPtrAllocator allocator;

    ConstExprEvaluator(const ConstExprEvaluator &) = delete;
    void operator=(const ConstExprEvaluator &) = delete;
  public:
    ConstExprEvaluator() {}

    /// Analyze the body of the specified function (which itself may not be a
    /// constexpr).  Determine whether the specified SymbolicValue's are
    /// constants, and return their LatticeValue's.
    ///
    /// TODO: Return information about which callees were found to be
    /// constexprs, which would allow the caller to delete dead calls to them
    /// that occur after
    void computeConstantValues(SILFunction &fn,
                               ArrayRef<SymbolicValue> values,
                               SmallVectorImpl<LatticeValue> &results);

  private:
    // Evaluate a call to the specified function as if it were a constant
    // expression.
    // bool evaluateAndCacheCall(SILFunction &fn,
    //   SmallVectorImpl<SILFunction*> &callStack,
    //   ArrayRef<??> substitutions,
    //   ArrayRef<SymbolicValue> arguments,
    //   SmallVectorImpl<LatticeValue> &results);
  };
} // end namespace tf
} // end namespace swift
#endif
