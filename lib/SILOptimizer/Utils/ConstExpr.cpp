//===--- ConstExpr.cpp - Constant expression evaluator -------------------===//
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

#define DEBUG_TYPE "ConstExpr"
#include "swift/SILOptimizer/Utils/ConstExpr.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/Defer.h"
#include "swift/Demangling/Demangle.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/Support/TrailingObjects.h"

using namespace swift;

static llvm::Optional<SymbolicValue>
evaluateAndCacheCall(SILFunction &fn, SubstitutionMap substitutionMap,
                     ArrayRef<SymbolicValue> arguments,
                     SmallVectorImpl<SymbolicValue> &results,
                     unsigned &numInstEvaluated, ConstExprEvaluator &evaluator);

// TODO: ConstantTracker in the performance inliner and the
// ConstantFolding.h/cpp files should be subsumed by this, as this is a more
// general framework.

//===----------------------------------------------------------------------===//
// ConstExprFunctionState implementation.
//===----------------------------------------------------------------------===//

namespace {
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
  /// TODO(constexpr patch): I have intentionally included this even though it's
  /// unused, so that I don't have to add it back to all the function signatures
  /// when I start using it.
  SubstitutionMap substitutionMap;

  /// This keeps track of the number of instructions we've evaluated.  If this
  /// goes beyond the execution cap, then we start returning unknown values.
  unsigned &numInstEvaluated;

  /// This is a state of previously analyzed values, maintained and filled in
  /// by getConstantValue.  This does not hold SIL address values.
  llvm::DenseMap<SILValue, SymbolicValue> calculatedValues;

public:
  ConstExprFunctionState(ConstExprEvaluator &evaluator, SILFunction *fn,
                         SubstitutionMap substitutionMap,
                         unsigned &numInstEvaluated)
      : evaluator(evaluator), fn(fn), substitutionMap(substitutionMap),
        numInstEvaluated(numInstEvaluated) {}

  void setValue(SILValue value, SymbolicValue symVal) {
    // TODO(constexpr patch): Uncomment this assertion once Address kinds have
    // been added.
    // assert(symVal.getKind() != SymbolicValue::Address &&
    //        "calculatedValues does not hold addresses");
    calculatedValues.insert({value, symVal});
  }

  /// Return the SymbolicValue for the specified SIL value, lazily computing
  /// it if needed.
  SymbolicValue getConstantValue(SILValue value);

  /// Evaluate the specified instruction in a flow sensitive way, for use by
  /// the constexpr function evaluator.  This does not handle control flow
  /// statements.
  llvm::Optional<SymbolicValue> evaluateFlowSensitive(SILInstruction *inst);

  Type simplifyType(Type ty);
  CanType simplifyType(CanType ty) {
    return simplifyType(Type(ty))->getCanonicalType();
  }
  SymbolicValue computeConstantValue(SILValue value);
  SymbolicValue computeConstantValueBuiltin(BuiltinInst *inst);

  llvm::Optional<SymbolicValue> computeCallResult(ApplyInst *apply);

  llvm::Optional<SymbolicValue> computeOpaqueCallResult(ApplyInst *apply,
                                                        SILFunction *callee);

};
} // end anonymous namespace

/// Simplify the specified type based on knowledge of substitutions if we have
/// any.
Type ConstExprFunctionState::simplifyType(Type ty) {
  return substitutionMap.empty() ? ty : ty.subst(substitutionMap);
}

/// Const-evaluate `value`, which must not have been computed.
SymbolicValue ConstExprFunctionState::computeConstantValue(SILValue value) {
  assert(!calculatedValues.count(value));

  // If this a trivial constant instruction that we can handle, then fold it
  // immediately.
  if (auto *ili = dyn_cast<IntegerLiteralInst>(value))
    return SymbolicValue::getInteger(ili->getValue(), evaluator.getASTContext());

  if (auto *fri = dyn_cast<FunctionRefInst>(value))
    return SymbolicValue::getFunction(fri->getReferencedFunction());

  // If we have a reference to a metatype, constant fold any substitutable
  // types.
  if (auto *mti = dyn_cast<MetatypeInst>(value)) {
    auto metatype = mti->getType().castTo<MetatypeType>();
    auto type = simplifyType(metatype->getInstanceType())->getCanonicalType();
    return SymbolicValue::getMetatype(type);
  }

  if (auto *tei = dyn_cast<TupleExtractInst>(value)) {
    auto val = getConstantValue(tei->getOperand());
    if (!val.isConstant())
      return val;
    return val.getAggregateValue()[tei->getFieldNo()];
  }

  // If this is a struct extract from a fragile type, then we can return the
  // element being extracted.
  if (auto *sei = dyn_cast<StructExtractInst>(value)) {
    auto aggValue = sei->getOperand();
    auto val = getConstantValue(aggValue);
    if (val.isConstant()) {
      assert(val.getKind() == SymbolicValue::Aggregate);
      return val.getAggregateValue()[sei->getFieldNo()];
    }
    // Not a const.
    return val;
  }

  // TODO: If this is a single element struct, we can avoid creating an
  // aggregate to reduce # allocations.  This is extra silly in the case of zero
  // element tuples.
  if (isa<StructInst>(value) || isa<TupleInst>(value)) {
    auto inst = cast<SingleValueInstruction>(value);
    SmallVector<SymbolicValue, 4> elts;

    for (unsigned i = 0, e = inst->getNumOperands(); i != e; ++i) {
      auto val = getConstantValue(inst->getOperand(i));
      if (!val.isConstant())
        return val;
      elts.push_back(val);
    }

    return SymbolicValue::getAggregate(elts, evaluator.getASTContext());
  }

  if (auto *builtin = dyn_cast<BuiltinInst>(value))
    return computeConstantValueBuiltin(builtin);

  if (auto *apply = dyn_cast<ApplyInst>(value)) {
    auto callResult = computeCallResult(apply);

    // If this failed, return the error code.
    if (callResult.hasValue())
      return callResult.getValue();

    assert(calculatedValues.count(apply));
    return calculatedValues[apply];
  }

  LLVM_DEBUG(llvm::dbgs() << "ConstExpr Unknown simple: " << *value << "\n");

  // Otherwise, we don't know how to handle this.
  return evaluator.getUnknown(value, UnknownReason::Default);
}

SymbolicValue
ConstExprFunctionState::computeConstantValueBuiltin(BuiltinInst *inst) {
  const BuiltinInfo &builtin = inst->getBuiltinInfo();

  // Handle various cases in groups.
  auto unknownResult = [&]() -> SymbolicValue {
    return evaluator.getUnknown(SILValue(inst), UnknownReason::Default);
  };

  // Unary operations.
  if (inst->getNumOperands() == 1) {
    auto operand = getConstantValue(inst->getOperand(0));
    // TODO: Could add a "value used here" sort of diagnostic.
    if (!operand.isConstant())
      return operand;

    // Implement support for s_to_s_checked_trunc_Int2048_Int64 and other
    // checking integer truncates.  These produce a tuple of the result value
    // and an overflow bit.
    //
    // TODO: We can/should diagnose statically detectable integer overflow
    // errors and subsume the ConstantFolding.cpp mandatory SIL pass.
    auto IntCheckedTruncFn = [&](bool srcSigned,
                                 bool dstSigned) -> SymbolicValue {
      if (operand.getKind() != SymbolicValue::Integer)
        return unknownResult();

      auto operandVal = operand.getIntegerValue();
      uint32_t srcBitWidth = operandVal.getBitWidth();
      auto dstBitWidth =
          builtin.Types[1]->castTo<BuiltinIntegerType>()->getGreatestWidth();

      APInt result = operandVal.trunc(dstBitWidth);

      // Compute the overflow by re-extending the value back to its source and
      // checking for loss of value.
      APInt reextended =
          dstSigned ? result.sext(srcBitWidth) : result.zext(srcBitWidth);
      bool overflowed = (operandVal != reextended);

      if (!srcSigned && dstSigned)
        overflowed |= result.isSignBitSet();

      if (overflowed)
        return evaluator.getUnknown(SILValue(inst), UnknownReason::Overflow);

      auto &astContext = evaluator.getASTContext();
      // Build the Symbolic value result for our truncated value.
      return SymbolicValue::getAggregate(
          {SymbolicValue::getInteger(result, astContext),
           SymbolicValue::getInteger(APInt(1, overflowed), astContext)},
          astContext);
    };

    switch (builtin.ID) {
    default:
      break;
    case BuiltinValueKind::SToSCheckedTrunc:
      return IntCheckedTruncFn(true, true);
    case BuiltinValueKind::UToSCheckedTrunc:
      return IntCheckedTruncFn(false, true);
    case BuiltinValueKind::SToUCheckedTrunc:
      return IntCheckedTruncFn(true, false);
    case BuiltinValueKind::UToUCheckedTrunc:
      return IntCheckedTruncFn(false, false);

    case BuiltinValueKind::Trunc:
    case BuiltinValueKind::TruncOrBitCast:
    case BuiltinValueKind::ZExt:
    case BuiltinValueKind::ZExtOrBitCast:
    case BuiltinValueKind::SExt:
    case BuiltinValueKind::SExtOrBitCast: {
      if (operand.getKind() != SymbolicValue::Integer)
        return unknownResult();

      unsigned destBitWidth =
          inst->getType().castTo<BuiltinIntegerType>()->getGreatestWidth();

      APInt result = operand.getIntegerValue();
      if (result.getBitWidth() != destBitWidth) {
        switch (builtin.ID) {
        default:
          assert(0 && "Unknown case");
        case BuiltinValueKind::Trunc:
        case BuiltinValueKind::TruncOrBitCast:
          result = result.trunc(destBitWidth);
          break;
        case BuiltinValueKind::ZExt:
        case BuiltinValueKind::ZExtOrBitCast:
          result = result.zext(destBitWidth);
          break;
        case BuiltinValueKind::SExt:
        case BuiltinValueKind::SExtOrBitCast:
          result = result.sext(destBitWidth);
          break;
        }
      }
      return SymbolicValue::getInteger(result, evaluator.getASTContext());
    }
    }
  }

  // Binary operations.
  if (inst->getNumOperands() == 2) {
    auto operand0 = getConstantValue(inst->getOperand(0));
    auto operand1 = getConstantValue(inst->getOperand(1));
    if (!operand0.isConstant())
      return operand0;
    if (!operand1.isConstant())
      return operand1;

    auto constFoldIntCompare =
        [&](const std::function<bool(const APInt &, const APInt &)> &fn)
        -> SymbolicValue {
      if (operand0.getKind() != SymbolicValue::Integer ||
          operand1.getKind() != SymbolicValue::Integer)
        return unknownResult();

      auto result = fn(operand0.getIntegerValue(), operand1.getIntegerValue());
      return SymbolicValue::getInteger(APInt(1, result),
                                       evaluator.getASTContext());
    };

#define REQUIRE_KIND(KIND)                                                     \
  if (operand0.getKind() != SymbolicValue::KIND ||                             \
      operand1.getKind() != SymbolicValue::KIND)                               \
    return unknownResult();

    switch (builtin.ID) {
    default:
      break;
#define INT_BINOP(OPCODE, EXPR)                                                \
  case BuiltinValueKind::OPCODE: {                                             \
    REQUIRE_KIND(Integer)                                                      \
    auto l = operand0.getIntegerValue(), r = operand1.getIntegerValue();       \
    return SymbolicValue::getInteger((EXPR), evaluator.getASTContext());       \
  }
      INT_BINOP(Add, l + r)
      INT_BINOP(And, l & r)
      INT_BINOP(AShr, l.ashr(r))
      INT_BINOP(LShr, l.lshr(r))
      INT_BINOP(Or, l | r)
      INT_BINOP(Mul, l * r)
      INT_BINOP(SDiv, l.sdiv(r))
      INT_BINOP(Shl, l << r)
      INT_BINOP(SRem, l.srem(r))
      INT_BINOP(Sub, l - r)
      INT_BINOP(UDiv, l.udiv(r))
      INT_BINOP(URem, l.urem(r))
      INT_BINOP(Xor, l ^ r)
#undef INT_BINOP

#define INT_COMPARE(OPCODE, EXPR)                                              \
  case BuiltinValueKind::OPCODE:                                               \
    REQUIRE_KIND(Integer)                                                      \
    return constFoldIntCompare(                                                \
        [&](const APInt &l, const APInt &r) -> bool { return (EXPR); })
      INT_COMPARE(ICMP_EQ, l == r);
      INT_COMPARE(ICMP_NE, l != r);
      INT_COMPARE(ICMP_SLT, l.slt(r));
      INT_COMPARE(ICMP_SGT, l.sgt(r));
      INT_COMPARE(ICMP_SLE, l.sle(r));
      INT_COMPARE(ICMP_SGE, l.sge(r));
      INT_COMPARE(ICMP_ULT, l.ult(r));
      INT_COMPARE(ICMP_UGT, l.ugt(r));
      INT_COMPARE(ICMP_ULE, l.ule(r));
      INT_COMPARE(ICMP_UGE, l.uge(r));
#undef INT_COMPARE
#undef REQUIRE_KIND
    }
  }

  // Three operand builtins.
  if (inst->getNumOperands() == 3) {
    auto operand0 = getConstantValue(inst->getOperand(0));
    auto operand1 = getConstantValue(inst->getOperand(1));
    auto operand2 = getConstantValue(inst->getOperand(2));
    if (!operand0.isConstant())
      return operand0;
    if (!operand1.isConstant())
      return operand1;
    if (!operand2.isConstant())
      return operand2;

    // Overflowing integer operations like sadd_with_overflow take three
    // operands: the last one is a "should report overflow" bit.
    auto constFoldIntOverflow =
        [&](const std::function<APInt(const APInt &, const APInt &, bool &)>
                &fn) -> SymbolicValue {
      if (operand0.getKind() != SymbolicValue::Integer ||
          operand1.getKind() != SymbolicValue::Integer ||
          operand2.getKind() != SymbolicValue::Integer)
        return unknownResult();

      auto l = operand0.getIntegerValue(), r = operand1.getIntegerValue();
      bool overflowed = false;
      auto result = fn(l, r, overflowed);

      // Return a statically diagnosed overflow if the operation is supposed to
      // trap on overflow.
      if (overflowed && !operand2.getIntegerValue().isNullValue())
        return evaluator.getUnknown(SILValue(inst), UnknownReason::Overflow);

      auto &astContext = evaluator.getASTContext();
      // Build the Symbolic value result for our normal and overflow bit.
      return SymbolicValue::getAggregate(
          {SymbolicValue::getInteger(result, astContext),
           SymbolicValue::getInteger(APInt(1, overflowed), astContext)},
          astContext);
    };

    switch (builtin.ID) {
    default:
      break;

#define INT_OVERFLOW(OPCODE, METHOD)                                           \
  case BuiltinValueKind::OPCODE:                                               \
    return constFoldIntOverflow(                                               \
        [&](const APInt &l, const APInt &r, bool &overflowed) -> APInt {       \
          return l.METHOD(r, overflowed);                                      \
        })
      INT_OVERFLOW(SAddOver, sadd_ov);
      INT_OVERFLOW(UAddOver, uadd_ov);
      INT_OVERFLOW(SSubOver, ssub_ov);
      INT_OVERFLOW(USubOver, usub_ov);
      INT_OVERFLOW(SMulOver, smul_ov);
      INT_OVERFLOW(UMulOver, umul_ov);
#undef INT_OVERFLOW
    }
  }

  LLVM_DEBUG(llvm::dbgs() << "ConstExpr Unknown Builtin: " << *inst << "\n");

  // Otherwise, we don't know how to handle this builtin.
  return unknownResult();
}

// Handle calls to opaque callees, either by handling them and returning None or
// by returning with a Unknown indicating a failure.
llvm::Optional<SymbolicValue>
ConstExprFunctionState::computeOpaqueCallResult(ApplyInst *apply,
                                                SILFunction *callee) {
  LLVM_DEBUG(llvm::dbgs() << "ConstExpr Opaque Callee: " << *callee << "\n");
  return evaluator.getUnknown((SILInstruction *)apply, UnknownReason::Default);
}

/// Given a call to a function, determine whether it is a call to a constexpr
/// function.  If so, collect its arguments as constants, fold it and return
/// None.  If not, mark the results as Unknown, and return an Unknown with
/// information about the error.
llvm::Optional<SymbolicValue>
ConstExprFunctionState::computeCallResult(ApplyInst *apply) {
  auto conventions = apply->getSubstCalleeConv();

  // Determine the callee.
  auto calleeFn = getConstantValue(apply->getOperand(0));
  if (calleeFn.getKind() != SymbolicValue::Function)
    return evaluator.getUnknown((SILInstruction *)apply,
                                UnknownReason::Default);

  SILFunction *callee = calleeFn.getFunctionValue();

  // Verify that we can fold all of the arguments to the call.
  SmallVector<SymbolicValue, 4> paramConstants;
  for (unsigned i = 0, e = apply->getNumOperands() - 1; i != e; ++i) {
    // If any of the arguments is a non-constant value, then we can't fold this
    // call.
    auto op = apply->getOperand(i + 1);
    SymbolicValue argValue = getConstantValue(op);
    if (!argValue.isConstant())
      return argValue;
    paramConstants.push_back(argValue);
  }

  // TODO(constexpr patch): This is currently unused, so we don't need to
  // calculate the correct value. Eventually, include code that calculates the
  // correct value.
  SubstitutionMap calleeSubMap;

  // Now that we have successfully folded all of the parameters, we can evaluate
  // the call.
  evaluator.pushCallStack(apply->getLoc().getSourceLoc());
  SmallVector<SymbolicValue, 4> results;
  auto callResult = evaluateAndCacheCall(*callee, calleeSubMap, paramConstants,
                                         results, numInstEvaluated, evaluator);
  evaluator.popCallStack();
  if (callResult.hasValue())
    return callResult.getValue();

  unsigned nextResult = 0;

  // If evaluation was successful, remember the results we captured in our
  // current function's state.
  if (unsigned numNormalResults = conventions.getNumDirectSILResults()) {
    // TODO: unclear when this happens, is this for tuple result values?
    assert(numNormalResults == 1 && "Multiple results aren't supported?");
    setValue(apply->getResults()[0], results[nextResult]);
    ++nextResult;
  }

  assert(nextResult == results.size() && "Unexpected number of results found");

  // We have successfully folded this call!
  return None;
}

/// Return the SymbolicValue for the specified SIL value, lazily computing
/// it if needed.
SymbolicValue ConstExprFunctionState::getConstantValue(SILValue value) {
  // Check to see if we already have an answer.
  auto it = calculatedValues.find(value);
  if (it != calculatedValues.end())
    return it->second;

  // Compute the value of a normal instruction based on its operands.
  auto result = computeConstantValue(value);

  // If this is the top-level lazy interpreter, output a debug trace.
  if (!fn) {
    LLVM_DEBUG(llvm::dbgs() << "ConstExpr top level: "; value->dump());
    LLVM_DEBUG(llvm::dbgs() << "  RESULT: "; result.dump());
  }

  setValue(value, result);
  return result;
}

/// Evaluate the specified instruction in a flow sensitive way, for use by
/// the constexpr function evaluator.  This does not handle control flow
/// statements.  This returns None on success, and an Unknown SymbolicValue with
/// information about an error on failure.
llvm::Optional<SymbolicValue>
ConstExprFunctionState::evaluateFlowSensitive(SILInstruction *inst) {
  // These are just markers.
  if (isa<DebugValueInst>(inst) || isa<DebugValueAddrInst>(inst) ||
      isa<EndAccessInst>(inst) ||
      // The interpreter doesn't model these memory management instructions, so
      // skip them.
      isa<DestroyAddrInst>(inst) || isa<RetainValueInst>(inst) ||
      isa<ReleaseValueInst>(inst) || isa<StrongRetainInst>(inst) ||
      isa<StrongReleaseInst>(inst))
    return None;

  if (isa<CondFailInst>(inst)) {
    auto failed = getConstantValue(inst->getOperand(0));
    if (failed.getKind() == SymbolicValue::Integer) {
      if (failed.getIntegerValue() == 0)
        return None;
      // Conditional fail actually failed.
      return evaluator.getUnknown(inst, UnknownReason::Trap);
    }
  }

  // If this is a call, evaluate it.
  if (auto apply = dyn_cast<ApplyInst>(inst))
    return computeCallResult(apply);

  // If the instruction produces normal results, try constant folding it.
  // If this fails, then we fail.
  if (inst->getNumResults() != 0) {
    auto oneResultVal = inst->getResults()[0];
    auto result = getConstantValue(oneResultVal);
    if (!result.isConstant())
      return result;
    LLVM_DEBUG(llvm::dbgs() << "  RESULT: "; result.dump());
    return None;
  }

  LLVM_DEBUG(llvm::dbgs() << "ConstExpr Unknown FS: " << *inst << "\n");
  // If this is an unknown instruction with no results then bail out.
  return evaluator.getUnknown(inst, UnknownReason::Default);
}

/// Evaluate a call to the specified function as if it were a constant
/// expression, returning None and filling in `results` on success, or
/// returning an 'Unknown' SymbolicValue on failure carrying the error.
///
static llvm::Optional<SymbolicValue> evaluateAndCacheCall(
    SILFunction &fn, SubstitutionMap substitutionMap,
    ArrayRef<SymbolicValue> arguments, SmallVectorImpl<SymbolicValue> &results,
    unsigned &numInstEvaluated, ConstExprEvaluator &evaluator) {
  assert(!fn.isExternalDeclaration() && "Can't analyze bodyless function");
  ConstExprFunctionState state(evaluator, &fn, substitutionMap,
                               numInstEvaluated);

  // TODO: implement caching.
  // TODO: reject code that is too complex.

  // Set up all of the indirect results and argument values.
  auto conventions = fn.getConventions();
  unsigned nextBBArg = 0;
  const auto &argList = fn.front().getArguments();

  LLVM_DEBUG(llvm::dbgs().changeColor(raw_ostream::SAVEDCOLOR, /*bold*/ true)
                 << "\nConstExpr call fn: "
                 << Demangle::demangleSymbolAsString(fn.getName());
             llvm::dbgs().resetColor() << "\n");

  assert(arguments.size() == argList.size() && "incorrect # arguments passed");
  for (auto argSymVal : arguments)
    state.setValue(argList[nextBBArg++], argSymVal);

  // Keep track of which blocks we've already visited.  We don't support loops
  // and this allows us to reject them.
  SmallPtrSet<SILBasicBlock *, 8> visitedBlocks;

  // Keep track of the current "instruction pointer".
  SILBasicBlock::iterator nextInst = fn.front().begin();
  visitedBlocks.insert(&fn.front());

  while (1) {
    SILInstruction *inst = &*nextInst++;
    LLVM_DEBUG(llvm::dbgs() << "ConstExpr interpret: "; inst->dump());

    // Make sure we haven't exceeded our interpreter iteration cap.
    if (++numInstEvaluated > ConstExprLimit)
      return SymbolicValue::getUnknown(inst, UnknownReason::TooManyInstructions,
                                       {}, evaluator.getASTContext());

    // If we can evaluate this flow sensitively, then keep going.
    if (!isa<TermInst>(inst)) {
      auto fsResult = state.evaluateFlowSensitive(inst);
      if (fsResult.hasValue())
        return fsResult;
      continue;
    }

    // Otherwise, we handle terminators here.
    if (isa<ReturnInst>(inst)) {
      auto val = state.getConstantValue(inst->getOperand(0));
      if (!val.isConstant())
        return val;

      // If we got a constant value, then we're good.  Set up the normal result
      // values as well as any indirect results.
      auto numNormalResults = conventions.getNumDirectSILResults();
      if (numNormalResults == 1) {
        results.push_back(val);
      } else if (numNormalResults > 1) {
        auto elts = val.getAggregateValue();
        assert(elts.size() == numNormalResults && "result list mismatch!");
        results.append(results.begin(), results.end());
      }

      // TODO: Handle caching of results.

      LLVM_DEBUG(llvm::dbgs() << "\n");
      return None;
    }

    if (auto *br = dyn_cast<BranchInst>(inst)) {
      auto destBB = br->getDestBB();

      // If we've already visited this block then fail - we have a loop.
      if (!visitedBlocks.insert(destBB).second)
        return evaluator.getUnknown(br, UnknownReason::Loop);

      // Set up basic block arguments.
      for (unsigned i = 0, e = br->getNumArgs(); i != e; ++i) {
        auto argument = state.getConstantValue(br->getArg(i));
        if (!argument.isConstant())
          return argument;
        state.setValue(destBB->getArgument(i), argument);
      }
      // Set the instruction pointer to the first instruction of the block.
      nextInst = destBB->begin();
      continue;
    }

    if (auto *cbr = dyn_cast<CondBranchInst>(inst)) {
      auto val = state.getConstantValue(inst->getOperand(0));
      if (!val.isConstant())
        return val;

      SILBasicBlock *destBB;
      if (!val.getIntegerValue())
        destBB = cbr->getFalseBB();
      else
        destBB = cbr->getTrueBB();

      // If we've already visited this block then fail - we have a loop.
      if (!visitedBlocks.insert(destBB).second)
        return evaluator.getUnknown(cbr, UnknownReason::Loop);

      nextInst = destBB->begin();
      continue;
    }

    LLVM_DEBUG(llvm::dbgs()
               << "ConstExpr: Unknown Terminator: " << *inst << "\n");

    return evaluator.getUnknown(inst, UnknownReason::Default);
  }
}

//===----------------------------------------------------------------------===//
// ConstExprEvaluator implementation.
//===----------------------------------------------------------------------===//

ConstExprEvaluator::ConstExprEvaluator(SILModule &m)
    : astContext(m.getASTContext()) {}

ConstExprEvaluator::~ConstExprEvaluator() {}

SymbolicValue ConstExprEvaluator::getUnknown(SILNode *node,
                                             UnknownReason reason) {
  return SymbolicValue::getUnknown(node, reason, getCallStack(),
                                   getASTContext());
}

/// Analyze the specified values to determine if they are constant values.  This
/// is done in code that is not necessarily itself a constexpr function.  The
/// results are added to the results list which is a parallel structure to the
/// input values.
///
/// TODO: Return information about which callees were found to be
/// constexprs, which would allow the caller to delete dead calls to them
/// that occur after folding them.
void ConstExprEvaluator::computeConstantValues(
    ArrayRef<SILValue> values, SmallVectorImpl<SymbolicValue> &results) {
  unsigned numInstEvaluated = 0;
  ConstExprFunctionState state(*this, nullptr, {}, numInstEvaluated);
  for (auto v : values) {
    auto symVal = state.getConstantValue(v);
    results.push_back(symVal);

    // Reset the execution limit back to zero for each subexpression we look
    // at.  We don't want lots of constants folded to trigger a limit.
    numInstEvaluated = 0;
  }
}
