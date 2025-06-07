//===--- ConstExpr.h - Constant expression evaluator -----------*- C++ -*-===//
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
// values and return constant values.  These constants may be integer, and
// floating point values.   We allow abstractions to be built out of fragile
// structs and tuples.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_CONSTEXPR_H
#define SWIFT_SILOPTIMIZER_CONSTEXPR_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILBasicBlock.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace swift {
class ASTContext;
class Operand;
class SILFunction;
class SILModule;
class SILNode;
class SymbolicValue;
class SymbolicValueAllocator;
class ConstExprFunctionState;
class UnknownReason;

/// This class is the main entrypoint for evaluating constant expressions.  It
/// also handles caching of previously computed constexpr results.
class ConstExprEvaluator {
  SymbolicValueAllocator &allocator;

  // Assert configuration that must be used by the evaluator. This determines
  // the result of the builtin "assert_configuration".
  unsigned assertConfig;

  /// The current call stack, used for providing accurate diagnostics.
  llvm::SmallVector<SourceLoc, 4> callStack;

  /// When set to true, keep track of all functions called during an evaluation.
  bool trackCallees;
  /// Functions called during the evaluation. This is an auxiliary information
  /// provided to the clients.
  llvm::SmallPtrSet<SILFunction *, 2> calledFunctions;

  void operator=(const ConstExprEvaluator &) = delete;

public:
  explicit ConstExprEvaluator(SymbolicValueAllocator &alloc,
                              unsigned assertConf, bool trackCallees = false);
  ~ConstExprEvaluator();

  explicit ConstExprEvaluator(const ConstExprEvaluator &other);

  SymbolicValueAllocator &getAllocator() { return allocator; }

  unsigned getAssertConfig() { return assertConfig; }

  void pushCallStack(SourceLoc loc) { callStack.push_back(loc); }

  void popCallStack() {
    assert(!callStack.empty());
    callStack.pop_back();
  }

  const llvm::SmallVector<SourceLoc, 4> &getCallStack() { return callStack; }

  // As SymbolicValue::getUnknown(), but handles passing the call stack and
  // allocator.
  SymbolicValue getUnknown(SILNode *node, UnknownReason reason);

  /// Analyze the specified values to determine if they are constant values.
  /// This is done in code that is not necessarily itself a constexpr
  /// function.  The results are added to the results list which is a parallel
  /// structure to the input values.
  void computeConstantValues(ArrayRef<SILValue> values,
                             SmallVectorImpl<SymbolicValue> &results);

  void recordCalledFunctionIfEnabled(SILFunction *callee) {
    if (trackCallees) {
      calledFunctions.insert(callee);
    }
  }

  /// If the evaluator was initialized with \c trackCallees enabled, return the
  /// SIL functions encountered during the evaluations performed with this
  /// evaluator. The returned functions include those that were called but
  /// failed to complete successfully.
  const SmallPtrSetImpl<SILFunction *> &getFuncsCalledDuringEvaluation() const {
    assert(trackCallees && "evaluator not configured to track callees");
    return calledFunctions;
  }
};

enum class WellKnownFunction {
  // Array.init()
  ArrayInitEmpty,
  // Array._allocateUninitializedArray
  AllocateUninitializedArray,
  // Array._endMutation
  EndArrayMutation,
  // _finalizeUninitializedArray
  FinalizeUninitializedArray,
  // Array.append(_:)
  ArrayAppendElement,
  // String.init()
  StringInitEmpty,
  // String.init(_builtinStringLiteral:utf8CodeUnitCount:isASCII:)
  StringMakeUTF8,
  // static String.append (_: String, _: inout String)
  StringAppend,
  // static String.== infix(_: String)
  StringEquals,
  // String.percentEscapedString.getter
  StringEscapePercent,
  // BinaryInteger.description.getter
  BinaryIntegerDescription,
  // _assertionFailure(_: StaticString, _: StaticString, file: StaticString,...)
  AssertionFailure,
  // A function taking one argument that prints the symbolic value of the
  // argument during constant evaluation. This must only be used for debugging.
  DebugPrint
};

//===----------------------------------------------------------------------===//
// ConstExprFunctionState implementation.
//===----------------------------------------------------------------------===//

/// This type represents the state of computed values within a function
/// as evaluation happens.  A separate instance of this is made for each
/// callee in a call chain to represent the constant values given the set of
/// formal parameters that callee was invoked with.
class ConstExprFunctionState {
  /// This is the evaluator that is computing this function state.  We use it to
  /// allocate space for values and to query the call stack.
  ConstExprEvaluator &evaluator;

  /// If we are analyzing the body of a constexpr function, this is the
  /// function.  This is null for the top-level expression.
  SILFunction *fn;

  /// If we have a function being analyzed, this is the substitutionMap for
  /// the call to it.
  /// substitutionMap specifies a mapping from all of the protocol and type
  /// requirements in the generic signature down to concrete conformances and
  /// concrete types.
  SubstitutionMap substitutionMap;

  /// This keeps track of the number of instructions we've evaluated.  If this
  /// goes beyond the execution cap, then we start returning unknown values.
  unsigned &numInstEvaluated;

  /// This is a state of previously analyzed values, maintained and filled in
  /// by getConstantValue.  This does not hold the memory referred to by SIL
  /// addresses.
  llvm::DenseMap<SILValue, SymbolicValue> calculatedValues;

  /// If a SILValue is not bound to a SymbolicValue in the calculatedValues,
  /// try to compute it recursively by visiting its defining instruction.
  bool recursivelyComputeValueIfNotInState = false;

public:
  ConstExprFunctionState(ConstExprEvaluator &evaluator, SILFunction *fn,
                         SubstitutionMap substitutionMap,
                         unsigned &numInstEvaluated,
                         bool enableTopLevelEvaluation);

  /// Pretty print the state to stderr.
  void dump() const;

  void setValue(SILValue value, SymbolicValue symVal);

  /// Return the symbolic value for a SILValue if it is bound in the interpreter
  /// state. If not, return None.
  std::optional<SymbolicValue> lookupValue(SILValue value);

  /// Invariant: Before the call, `calculatedValues` must not contain `addr`
  /// as a key.
  SymbolicValue createMemoryObject(SILValue addr, SymbolicValue initialValue);

  /// Return the SymbolicValue for the specified SIL value. If the SIL value is
  /// not in \c calculatedValues, try computing the SymbolicValue recursively
  /// if \c recursivelyComputeValueIfNotInState flag is set.
  SymbolicValue getConstantValue(SILValue value);

  /// Evaluate the specified instruction in a flow sensitive way, for use by
  /// the constexpr function evaluator.  This does not handle control flow
  /// statements.
  std::optional<SymbolicValue> evaluateFlowSensitive(SILInstruction *inst);

  /// Evaluate a branch or non-branch instruction and if the evaluation was
  /// successful, return the next instruction from where the evaluation must
  /// continue.
  /// \param instI basic-block iterator pointing to the instruction to evaluate.
  /// \param visitedBlocks basic blocks already visited during evaluation.
  ///   This is used to detect loops.
  /// \returns a pair where the first and second elements are defined as
  /// follows:
  ///   If the evaluation of the instruction is successful, the first element
  ///   is the iterator to the next instruction from the where the evaluation
  ///   must continue. Otherwise, it is None.
  ///
  ///   Second element is None, if the evaluation is successful.
  ///   Otherwise, is an unknown symbolic value that contains the error.
  std::pair<std::optional<SILBasicBlock::iterator>,
            std::optional<SymbolicValue>>
  evaluateInstructionAndGetNext(
      SILBasicBlock::iterator instI,
      SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks);

  Type substituteGenericParamsAndSimplify(Type ty);
  CanType substituteGenericParamsAndSimplify(CanType ty) {
    return substituteGenericParamsAndSimplify(Type(ty))->getCanonicalType();
  }
  SymbolicValue computeConstantValue(SILValue value);
  SymbolicValue computeConstantValueBuiltin(BuiltinInst *inst);

  std::optional<SymbolicValue> computeCallResult(ApplyInst *apply);

  std::optional<SymbolicValue> computeOpaqueCallResult(ApplyInst *apply,
                                                       SILFunction *callee);

  std::optional<SymbolicValue>
  computeWellKnownCallResult(ApplyInst *apply, WellKnownFunction callee);

  /// Evaluate a closure creation instruction which is either a partial_apply
  /// instruction or a thin_to_think_function instruction. On success, this
  /// function will bind the \c closureInst parameter to its symbolic value.
  /// On failure, it returns the unknown symbolic value that captures the error.
  std::optional<SymbolicValue>
  evaluateClosureCreation(SingleValueInstruction *closureInst);

  SymbolicValue getSingleWriterAddressValue(SILValue addr);
  SymbolicValue getConstAddrAndLoadResult(SILValue addr);
  SymbolicValue loadAddrValue(SILValue addr, SymbolicValue addrVal);
  std::optional<SymbolicValue> computeFSStore(SymbolicValue storedCst,
                                              SILValue dest);

private:
  std::optional<SymbolicValue> initializeAddressFromSingleWriter(SILValue addr);
};

/// A constant-expression evaluator that can be used to step through a control
/// flow graph (SILFunction body) by evaluating one instruction at a time.
/// This evaluator can also "skip" instructions without evaluating them and
/// only track constant values of variables whose values could be computed.
class ConstExprStepEvaluator {
private:
  ConstExprEvaluator evaluator;

  ConstExprFunctionState *internalState;

  unsigned stepsEvaluated = 0;

  /// Targets of branches that were visited. This is used to detect loops during
  /// evaluation.
  SmallPtrSet<SILBasicBlock *, 8> visitedBlocks;

  ConstExprStepEvaluator(const ConstExprStepEvaluator &) = delete;
  void operator=(const ConstExprStepEvaluator &) = delete;

  /// Set all addresses that could be mutated by the instruction to an
  /// unknown symbolic value if it is not already so.
  void setMutableAddressesToUnknown(SILInstruction *inst);

public:
  /// Constructs a step evaluator given an allocator and a non-null pointer to a
  /// SILFunction.
  explicit ConstExprStepEvaluator(SymbolicValueAllocator &alloc,
                                  SILFunction *fun, unsigned assertConf,
                                  bool trackCallees = false);
  ~ConstExprStepEvaluator();

  /// Evaluate an instruction in the current interpreter state.
  /// \param instI instruction to be evaluated in the current interpreter state.
  /// \returns a pair where the first and second elements are defined as
  /// follows:
  ///   The first element is the iterator to the next instruction from where
  ///   the evaluation can continue, if the evaluation is successful.
  ///   Otherwise, it is None.
  ///
  ///   Second element is None, if the evaluation is successful.
  ///   Otherwise, is an unknown symbolic value that contains the error.
  std::pair<std::optional<SILBasicBlock::iterator>,
            std::optional<SymbolicValue>>
  evaluate(SILBasicBlock::iterator instI);

  /// Skip the instruction without evaluating it and conservatively account for
  /// the effects of the instruction on the internal state. This operation
  /// resets to an unknown symbolic value any portion of a
  /// SymbolicValueMemoryObject that could possibly be mutated by the given
  /// instruction. This function preserves the soundness of the interpretation.
  /// \param instI instruction to be skipped.
  /// \returns a pair where the first and second elements are defined as
  /// follows:
  ///   The first element, if is not None, is the iterator to the next
  ///   instruction from the where the evaluation must continue.
  ///   The first element is None if the next instruction from where the
  ///   evaluation must continue cannot be determined.
  ///   This would be the case if `instI` is a branch like a `condbr`.
  ///
  ///   Second element is None if skipping the instruction is successful.
  ///   Otherwise, it is an unknown symbolic value containing the error.
  std::pair<std::optional<SILBasicBlock::iterator>,
            std::optional<SymbolicValue>>
  skipByMakingEffectsNonConstant(SILBasicBlock::iterator instI);

  /// Try evaluating an instruction and if the evaluation fails, skip the
  /// instruction and make it effects non constant. Note that it may not always
  /// be possible to skip an instruction whose evaluation failed and
  /// continue evaluation (e.g. a conditional branch).
  /// See `evaluate` and `skipByMakingEffectsNonConstant` functions for their
  /// semantics.
  /// \param instI instruction to be evaluated in the current interpreter state.
  /// \returns a pair where the first and second elements are defined as
  /// follows:
  ///   The first element, if is not None, is the iterator to the next
  ///   instruction from the where the evaluation must continue.
  ///   The first element is None iff both `evaluate` and `skip` functions
  ///   failed to determine the next instruction to continue evaluation from.
  ///
  ///   Second element is None if the evaluation is successful.
  ///   Otherwise, it is an unknown symbolic value containing the error.
  std::pair<std::optional<SILBasicBlock::iterator>,
            std::optional<SymbolicValue>>
  tryEvaluateOrElseMakeEffectsNonConstant(SILBasicBlock::iterator instI);

  std::optional<SymbolicValue> lookupConstValue(SILValue value);

  /// Return the number of instructions evaluated for the last `evaluate`
  /// operation. This could be used by the clients to limit the number of
  /// instructions that should be evaluated by the step-wise evaluator.
  /// Note that 'skipByMakingEffectsNonConstant' operation is not considered
  /// as an evaluation.
  unsigned instructionsEvaluatedByLastEvaluation() { return stepsEvaluated; }

  /// If the evaluator was initialized with \c trackCallees enabled, return the
  /// SIL functions encountered during the evaluations performed with this
  /// evaluator. The returned functions include those that were called but
  /// failed to complete successfully. Targets of skipped apply instructions
  /// will not be included in the returned set.
  const SmallPtrSetImpl<SILFunction *> &getFuncsCalledDuringEvaluation() {
    return evaluator.getFuncsCalledDuringEvaluation();
  }

  /// Dump the internal state to standard error for debugging.
  void dumpState();
};

bool hasConstantEvaluableAnnotation(SILFunction *fun);

bool isConstantEvaluable(SILFunction *fun);

/// Return true iff the \p applySite is constant-evaluable and read-only.
bool isReadOnlyConstantEvaluableCall(FullApplySite applySite);

/// Return true if and only if the given function \p fun is specially modeled
/// by the constant evaluator. These are typically functions in the standard
/// library, such as String.+=, Array.append, whose semantics is built into the
/// evaluator.
bool isKnownConstantEvaluableFunction(SILFunction *fun);

/// Return true if and only if \p errorVal denotes an error that requires
/// aborting interpretation and returning the error. Skipping an instruction
/// that produces such errors is not a valid behavior.
bool isFailStopError(SymbolicValue errorVal);

} // end namespace swift
#endif
