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
#include "swift/Basic/NullablePtr.h"
#include "swift/Demangling/Demangle.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
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

enum class WellKnownFunction {
  // String.init()
  StringInitEmpty,
  // String.init(_builtinStringLiteral:utf8CodeUnitCount:isASCII:)
  StringMakeUTF8,
  // static String.+= infix(_: inout String, _: String)
  StringAppend,
  // static String.== infix(_: String)
  StringEquals
};

static llvm::Optional<WellKnownFunction> classifyFunction(SILFunction *fn) {
  if (fn->hasSemanticsAttr("string.init_empty"))
    return WellKnownFunction::StringInitEmpty;
  // There are two string initializers in the standard library with the
  // semantics "string.makeUTF8". They are identical from the perspective of
  // the interpreter. One of those functions is probably redundant and not used.
  if (fn->hasSemanticsAttr("string.makeUTF8"))
    return WellKnownFunction::StringMakeUTF8;
  if (fn->hasSemanticsAttr("string.append"))
    return WellKnownFunction::StringAppend;
  if (fn->hasSemanticsAttr("string.equals"))
    return WellKnownFunction::StringEquals;
  return None;
}

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
  SubstitutionMap substitutionMap;

  /// This keeps track of the number of instructions we've evaluated.  If this
  /// goes beyond the execution cap, then we start returning unknown values.
  unsigned &numInstEvaluated;

  /// This is a state of previously analyzed values, maintained and filled in
  /// by getConstantValue.  This does not hold the memory referred to by SIL
  /// addresses.
  llvm::DenseMap<SILValue, SymbolicValue> calculatedValues;

public:
  ConstExprFunctionState(ConstExprEvaluator &evaluator, SILFunction *fn,
                         SubstitutionMap substitutionMap,
                         unsigned &numInstEvaluated)
      : evaluator(evaluator), fn(fn), substitutionMap(substitutionMap),
        numInstEvaluated(numInstEvaluated) {}

  void setValue(SILValue value, SymbolicValue symVal) {
    calculatedValues.insert({value, symVal});
  }

  /// Invariant: Before the call, `calculatedValues` must not contain `addr`
  /// as a key.
  SymbolicValue createMemoryObject(SILValue addr, SymbolicValue initialValue) {
    assert(!calculatedValues.count(addr));
    auto type = substituteGenericParamsAndSimpify(addr->getType().getASTType());
    auto *memObject = SymbolicValueMemoryObject::create(
        type, initialValue, evaluator.getAllocator());
    auto result = SymbolicValue::getAddress(memObject);
    setValue(addr, result);
    return result;
  }

  /// Return the SymbolicValue for the specified SIL value, lazily computing
  /// it if needed.
  SymbolicValue getConstantValue(SILValue value);

  /// Evaluate the specified instruction in a flow sensitive way, for use by
  /// the constexpr function evaluator.  This does not handle control flow
  /// statements.
  llvm::Optional<SymbolicValue> evaluateFlowSensitive(SILInstruction *inst);

  Type substituteGenericParamsAndSimpify(Type ty);
  CanType substituteGenericParamsAndSimpify(CanType ty) {
    return substituteGenericParamsAndSimpify(Type(ty))->getCanonicalType();
  }
  SymbolicValue computeConstantValue(SILValue value);
  SymbolicValue computeConstantValueBuiltin(BuiltinInst *inst);

  llvm::Optional<SymbolicValue> computeCallResult(ApplyInst *apply);

  llvm::Optional<SymbolicValue> computeOpaqueCallResult(ApplyInst *apply,
                                                        SILFunction *callee);

  llvm::Optional<SymbolicValue>
  computeWellKnownCallResult(ApplyInst *apply, WellKnownFunction callee);

  SymbolicValue getSingleWriterAddressValue(SILValue addr);
  SymbolicValue getConstAddrAndLoadResult(SILValue addr);
  SymbolicValue loadAddrValue(SILValue addr, SymbolicValue addrVal);
  llvm::Optional<SymbolicValue> computeFSStore(SymbolicValue storedCst,
                                               SILValue dest);

private:
  llvm::Optional<SymbolicValue>
  initializeAddressFromSingleWriter(SILValue addr);
};
} // end anonymous namespace

/// Simplify the specified type based on knowledge of substitutions if we have
/// any.
Type ConstExprFunctionState::substituteGenericParamsAndSimpify(Type ty) {
  return substitutionMap.empty() ? ty : ty.subst(substitutionMap);
}

/// Const-evaluate `value`, which must not have been computed.
SymbolicValue ConstExprFunctionState::computeConstantValue(SILValue value) {
  assert(!calculatedValues.count(value));

  // If the client is asking for the value of a stack object that hasn't been
  // computed, and if fn is null, then we are in top level code, and the
  // stack object must be a single store value.  Since this is a very different
  // computation, split it out to its own path.
  if (!fn && value->getType().isAddress() && isa<AllocStackInst>(value)) {
    return getSingleWriterAddressValue(value);
  }

  // If this a trivial constant instruction that we can handle, then fold it
  // immediately.
  if (auto *ili = dyn_cast<IntegerLiteralInst>(value))
    return SymbolicValue::getInteger(ili->getValue(), evaluator.getAllocator());
  if (auto *sli = dyn_cast<StringLiteralInst>(value))
    return SymbolicValue::getString(sli->getValue(), evaluator.getAllocator());

  if (auto *fri = dyn_cast<FunctionRefInst>(value))
    return SymbolicValue::getFunction(fri->getReferencedFunction());

  // If we have a reference to a metatype, constant fold any substitutable
  // types.
  if (auto *mti = dyn_cast<MetatypeInst>(value)) {
    auto metatype = mti->getType().castTo<MetatypeType>();
    auto type = substituteGenericParamsAndSimpify(metatype->getInstanceType())
        ->getCanonicalType();
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

  // If this is an unchecked_enum_data from a fragile type, then we can return
  // the enum case value.
  if (auto *uedi = dyn_cast<UncheckedEnumDataInst>(value)) {
    auto aggValue = uedi->getOperand();
    auto val = getConstantValue(aggValue);
    if (val.isConstant()) {
      assert(val.getKind() == SymbolicValue::EnumWithPayload);
      return val.getEnumPayloadValue();
    }
    // Not a const.
    return val;
  }

  // If this is a destructure_result, then we can return the element being
  // extracted.
  if (isa<DestructureStructResult>(value) ||
      isa<DestructureTupleResult>(value)) {
    auto *result = cast<MultipleValueInstructionResult>(value);
    SILValue aggValue = result->getParent()->getOperand(0);
    auto val = getConstantValue(aggValue);
    if (val.isConstant()) {
      assert(val.getKind() == SymbolicValue::Aggregate);
      return val.getAggregateValue()[result->getIndex()];
    }
    // Not a const.
    return val;
  }

  // TODO: If this is a single element struct, we can avoid creating an
  // aggregate to reduce # allocations.  This is extra silly in the case of zero
  // element tuples.
  if (isa<StructInst>(value) || isa<TupleInst>(value)) {
    auto *inst = cast<SingleValueInstruction>(value);
    SmallVector<SymbolicValue, 4> elts;

    for (unsigned i = 0, e = inst->getNumOperands(); i != e; ++i) {
      auto val = getConstantValue(inst->getOperand(i));
      if (!val.isConstant())
        return val;
      elts.push_back(val);
    }

    return SymbolicValue::getAggregate(elts, evaluator.getAllocator());
  }

  // If this is a struct or tuple element addressor, compute a more derived
  // address.
  if (isa<StructElementAddrInst>(value) || isa<TupleElementAddrInst>(value)) {
    auto inst = cast<SingleValueInstruction>(value);
    auto baseAddr = getConstantValue(inst->getOperand(0));
    if (!baseAddr.isConstant())
      return baseAddr;

    SmallVector<unsigned, 4> accessPath;
    auto *memObject = baseAddr.getAddressValue(accessPath);

    // Add our index onto the next of the list.
    unsigned index;
    if (auto sea = dyn_cast<StructElementAddrInst>(inst))
      index = sea->getFieldNo();
    else
      index = cast<TupleElementAddrInst>(inst)->getFieldNo();
    accessPath.push_back(index);
    return SymbolicValue::getAddress(memObject, accessPath,
                                     evaluator.getAllocator());
  }

  // If this is a load, then we either have computed the value of the memory
  // already (when analyzing the body of a function in a flow-sensitive
  // fashion), or this is the indirect result of a call.  Either way, we ask for
  // the value of the pointer.  In the former case, this will be the latest
  // value of the memory.  In the latter case, the call must be the only
  // store to the address so that the memory object can be computed by
  // recursively processing the allocation and call instructions in a
  // demand-driven fashion.
  if (auto li = dyn_cast<LoadInst>(value))
    return getConstAddrAndLoadResult(li->getOperand());

  // Try to resolve a witness method against our known conformances.
  if (auto *wmi = dyn_cast<WitnessMethodInst>(value)) {
    auto confResult = substitutionMap.lookupConformance(
        wmi->getLookupType(), wmi->getConformance().getRequirement());
    if (!confResult)
      return evaluator.getUnknown(value, UnknownReason::Default);
    auto conf = confResult.getValue();
    auto &module = wmi->getModule();

    // Look up the conformance's witness table and the member out of it.
    SILFunction *fn =
        module.lookUpFunctionInWitnessTable(conf, wmi->getMember()).first;
    // If we were able to resolve it, then we can proceed.
    if (fn)
      return SymbolicValue::getFunction(fn);

    LLVM_DEBUG(llvm::dbgs()
               << "ConstExpr Unresolved witness: " << *value << "\n");
    return evaluator.getUnknown(value, UnknownReason::Default);
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

  if (auto *enumVal = dyn_cast<EnumInst>(value)) {
    if (!enumVal->hasOperand())
      return SymbolicValue::getEnum(enumVal->getElement());

    auto payload = getConstantValue(enumVal->getOperand());
    if (!payload.isConstant())
      return payload;
    return SymbolicValue::getEnumWithPayload(enumVal->getElement(), payload,
                                             evaluator.getAllocator());
  }

  // This one returns the address of its enum payload.
  if (auto *dai = dyn_cast<UncheckedTakeEnumDataAddrInst>(value)) {
    auto enumVal = getConstAddrAndLoadResult(dai->getOperand());
    if (!enumVal.isConstant())
      return enumVal;
    return createMemoryObject(value, enumVal.getEnumPayloadValue());
  }

  // This instruction is a marker that returns its first operand.
  if (auto *bai = dyn_cast<BeginAccessInst>(value))
    return getConstantValue(bai->getOperand());

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

      auto &allocator = evaluator.getAllocator();
      // Build the Symbolic value result for our truncated value.
      return SymbolicValue::getAggregate(
          {SymbolicValue::getInteger(result, allocator),
           SymbolicValue::getInteger(APInt(1, overflowed), allocator)},
          allocator);
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
      return SymbolicValue::getInteger(result, evaluator.getAllocator());
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
                                       evaluator.getAllocator());
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
    return SymbolicValue::getInteger((EXPR), evaluator.getAllocator());        \
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

      auto &allocator = evaluator.getAllocator();
      // Build the Symbolic value result for our normal and overflow bit.
      return SymbolicValue::getAggregate(
          {SymbolicValue::getInteger(result, allocator),
           SymbolicValue::getInteger(APInt(1, overflowed), allocator)},
          allocator);
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

/// Given a call to a well known function, collect its arguments as constants,
/// fold it, and return None.  If any of the arguments are not constants, marks
/// the call's results as Unknown, and return an Unknown with information about
/// the error.
llvm::Optional<SymbolicValue>
ConstExprFunctionState::computeWellKnownCallResult(ApplyInst *apply,
                                                   WellKnownFunction callee) {
  auto conventions = apply->getSubstCalleeConv();
  switch (callee) {
  case WellKnownFunction::StringInitEmpty: { // String.init()
    assert(conventions.getNumDirectSILResults() == 1 &&
           conventions.getNumIndirectSILResults() == 0 &&
           "unexpected String.init() signature");
    auto result = SymbolicValue::getString("", evaluator.getAllocator());
    setValue(apply, result);
    return None;
  }
  case WellKnownFunction::StringMakeUTF8: {
    // String.init(_builtinStringLiteral start: Builtin.RawPointer,
    //             utf8CodeUnitCount: Builtin.Word,
    //             isASCII: Builtin.Int1)
    assert(conventions.getNumDirectSILResults() == 1 &&
           conventions.getNumIndirectSILResults() == 0 &&
           conventions.getNumParameters() == 4 && "unexpected signature");
    auto literal = getConstantValue(apply->getOperand(1));
    if (literal.getKind() != SymbolicValue::String) {
      return evaluator.getUnknown((SILInstruction *)apply,
                                  UnknownReason::Default);
    }
    auto literalVal = literal.getStringValue();

    auto byteCount = getConstantValue(apply->getOperand(2));
    if (byteCount.getKind() != SymbolicValue::Integer ||
        byteCount.getIntegerValue().getLimitedValue() != literalVal.size()) {
      return evaluator.getUnknown((SILInstruction *)apply,
                                  UnknownReason::Default);
    }
    setValue(apply, literal);
    return None;
  }
  case WellKnownFunction::StringAppend: {
    // static String.+= infix(_: inout String, _: String)
    assert(conventions.getNumDirectSILResults() == 0 &&
           conventions.getNumIndirectSILResults() == 0 &&
           conventions.getNumParameters() == 3 &&
           "unexpected String.+=() signature");

    auto firstOperand = apply->getOperand(1);
    auto firstString = getConstAddrAndLoadResult(firstOperand);
    if (firstString.getKind() != SymbolicValue::String) {
      return evaluator.getUnknown((SILInstruction *)apply,
                                  UnknownReason::Default);
    }

    auto otherString = getConstantValue(apply->getOperand(2));
    if (otherString.getKind() != SymbolicValue::String) {
      return evaluator.getUnknown((SILInstruction *)apply,
                                  UnknownReason::Default);
    }

    auto result = SmallString<8>(firstString.getStringValue());
    result.append(otherString.getStringValue());
    auto resultVal = SymbolicValue::getString(result, evaluator.getAllocator());
    computeFSStore(resultVal, firstOperand);
    return None;
  }
  case WellKnownFunction::StringEquals: {
    // static String.== infix(_: String, _: String)
    assert(conventions.getNumDirectSILResults() == 1 &&
           conventions.getNumIndirectSILResults() == 0 &&
           conventions.getNumParameters() == 3 &&
           "unexpected String.==() signature");

    auto firstString = getConstantValue(apply->getOperand(1));
    if (firstString.getKind() != SymbolicValue::String) {
      return evaluator.getUnknown((SILInstruction *)apply,
                                  UnknownReason::Default);
    }

    auto otherString = getConstantValue(apply->getOperand(2));
    if (otherString.getKind() != SymbolicValue::String) {
      return evaluator.getUnknown((SILInstruction *)apply,
                                  UnknownReason::Default);
    }

    // The result is a Swift.Bool which is a struct that wraps an Int1.
    int isEqual = firstString.getStringValue() == otherString.getStringValue();
    auto intVal =
        SymbolicValue::getInteger(APInt(1, isEqual), evaluator.getAllocator());
    auto result = SymbolicValue::getAggregate(ArrayRef<SymbolicValue>(intVal),
                                              evaluator.getAllocator());
    setValue(apply, result);
    return None;
  }
  }
  llvm_unreachable("unhandled WellKnownFunction");
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

  // If this is a well-known function, do not step into it.
  if (auto wellKnownFunction = classifyFunction(callee))
    return computeWellKnownCallResult(apply, *wellKnownFunction);

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

  // If we reached an external function that hasn't been deserialized yet, make
  // sure to pull it in so we can see its body.  If that fails, then we can't
  // analyze the function.
  if (callee->isExternalDeclaration()) {
    callee->getModule().loadFunction(callee);
    if (callee->isExternalDeclaration())
      return computeOpaqueCallResult(apply, callee);
  }

  // Compute the substitution map for the callee, which maps from all of its
  // generic requirements to concrete conformances and concrete types.
  SubstitutionMap calleeSubMap;

  auto calleeFnType = callee->getLoweredFunctionType();
  assert(
      !calleeFnType->hasSelfParam() ||
      !calleeFnType->getSelfInstanceType()->getClassOrBoundGenericClass() &&
      "class methods are not supported");
  if (calleeFnType->getGenericSignature()) {
    // Get the substitution map of the call.  This maps from the callee's space
    // into the caller's world. Witness methods require additional work to
    // compute a mapping that is valid for the callee.
    SubstitutionMap callSubMap;

    if (calleeFnType->getRepresentation() ==
        SILFunctionType::Representation::WitnessMethod) {
      auto protocol =
          calleeFnType->getWitnessMethodConformance().getRequirement();
      // Compute a mapping that maps the Self type of the protocol given by
      // 'requirement' to the concrete type available in the substitutionMap.
      auto protoSelfToConcreteType =
          apply->getSubstitutionMap().subst(substitutionMap);
      // Get a concrete protocol conformance by using the mapping for the
      // Self type of the requirement.
      auto conf = protoSelfToConcreteType.lookupConformance(
          protocol->getSelfInterfaceType()->getCanonicalType(), protocol);
      if (!conf.hasValue())
        return evaluator.getUnknown((SILInstruction *)apply,
                                    UnknownReason::Default);

      callSubMap = getWitnessMethodSubstitutions(
          apply->getModule(), ApplySite(apply), callee, conf.getValue());

      /// Remark: If we ever start to care about evaluating classes,
      /// getSubstitutionsForCallee() is the analogous mapping function we
      /// should use to get correct mapping from caller to callee namespace.
      /// Ideally, the function must be renamed as
      /// getClassMethodSubstitutions().
    } else {
      callSubMap = apply->getSubstitutionMap();
    }

    // The substitution map for the callee is the composition of the callers
    // substitution map, which is always type/conformance to a concrete type
    // or conformance, with the mapping introduced by the call itself.  This
    // ensures that the callee's substitution map can map from its type
    // namespace back to concrete types and conformances.
    calleeSubMap = callSubMap.subst(substitutionMap);
  }

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

/// This is a helper function for `getSingleWriterAddressValue`. Callers should
/// use `getSingleWriterAddressValue`.
///
/// If `addr` has no writing uses, returns None.
///
/// If the following conditions hold:
///   * `addr` points at uninitialized memory;
///   * there are write(s) to `addr` that, taken together, set the memory
///     exactly once (e.g. a single "store" to `addr` OR multiple "store"s to
///     different "tuple_element_addr"s of `addr`); and
///   * the writes' value(s) can be const-evaluated;
/// Then: initializes the memory at `addr` and returns None.
///
/// Otherwise, sets the memory at `addr` to an unknown SymbolicValue, and
/// returns the unknown SymbolicValue.
///
/// Additional side effects: In all cases, this function might cache address
/// values for `addr` and for addresses derived from `addr`.
///
/// Precondition: An address for `addr`, or an address that `addr` is derived
/// from, must be cached in `computedValues`.
llvm::Optional<SymbolicValue>
ConstExprFunctionState::initializeAddressFromSingleWriter(SILValue addr) {
  LLVM_DEBUG(llvm::dbgs() << "ConstExpr: initializeAddressFromSingleWriter "
             << addr);

  SmallVector<unsigned, 4> accessPath;
  auto *memoryObject = getConstantValue(addr).getAddressValue(accessPath);

  // If we detect instructions that initialize an aggregate piecewise, then we
  // set this flag, which tells us to verify that the entire aggregate has been
  // initialized.
  bool mustCheckAggregateInitialized = false;

  // Sets the pointed-at memory to `value`.
  auto setMemoryValue = [&](SymbolicValue value) {
    memoryObject->setIndexedElement(accessPath, value,
                                    evaluator.getAllocator());
  };

  // Gets the pointed-at memory value.
  auto getMemoryValue = [&]() -> SymbolicValue {
    return memoryObject->getIndexedElement(accessPath);
  };

  // Does all error-condition side-effects, and returns the appropriate error
  // result.
  // Precondition: `unknown` must be an unknown SymbolicValue.
  auto error = [&](SymbolicValue unknown) -> SymbolicValue {
    assert(unknown.getKind() == SymbolicValue::Unknown);
    setMemoryValue(unknown);
    return unknown;
  };

  // Checks that the pointed-at aggregate is fully initialized.
  // Precondition: The pointed-at memory value is uninit memory or an
  // aggregate.
  auto checkAggregateInitialized = [&]() -> bool {
    auto memoryValue = getMemoryValue();
    return memoryValue.getKind() != SymbolicValue::UninitMemory &&
           llvm::all_of(memoryValue.getAggregateValue(),
                        [](SymbolicValue v) { return v.isConstant(); });
  };

  // Okay, check out all of the users of this value looking for semantic stores
  // into the address.  If we find more than one, then this was a var or
  // something else we can't handle.
  // We must iterate over all uses, to make sure there is a single initializer.
  // The only permitted early exit is when we know for sure that we have failed.
  for (auto *use : addr->getUses()) {
    auto user = use->getUser();

    // Ignore markers, loads, and other things that aren't stores to this stack
    // value.
    if (isa<LoadInst>(user) || isa<DeallocStackInst>(user) ||
        isa<DestroyAddrInst>(user) || isa<DebugValueAddrInst>(user))
      continue;

    // TODO: Allow BeginAccess/EndAccess users.

    // If this is a store *to* the memory, analyze the input value.
    if (auto *si = dyn_cast<StoreInst>(user)) {
      if (use->getOperandNumber() == 1) {
        // Forbid multiple assignment.
        if (getMemoryValue().getKind() != SymbolicValue::UninitMemory)
          return error(evaluator.getUnknown(addr, UnknownReason::Default));

        auto result = getConstantValue(si->getOperand(0));
        if (!result.isConstant())
          return error(evaluator.getUnknown(addr, UnknownReason::Default));

        setMemoryValue(result);
        continue;
      }
    }

    if (auto *cai = dyn_cast<CopyAddrInst>(user)) {
      // If this is a copy_addr *from* the memory, then it is a load, ignore it.
      if (use->getOperandNumber() == 0)
        continue;

      // If this is a copy_addr *to* the memory, analyze the input value.
      assert(use->getOperandNumber() == 1 && "copy_addr has two operands");

      // Forbid multiple assignment.
      if (getMemoryValue().getKind() != SymbolicValue::UninitMemory)
        return error(evaluator.getUnknown(addr, UnknownReason::Default));

      auto result = getConstAddrAndLoadResult(cai->getOperand(0));
      if (!result.isConstant())
        return error(evaluator.getUnknown(addr, UnknownReason::Default));

      setMemoryValue(result);
      continue;
    }

    // If this is an apply_inst passing the memory address as an indirect
    // result operand, then we have a call that fills in this result.
    if (auto *apply = dyn_cast<ApplyInst>(user)) {
      auto conventions = apply->getSubstCalleeConv();

      // If this is an out-parameter, it is like a store.  If not, this is an
      // indirect read which is ok.
      unsigned numIndirectResults = conventions.getNumIndirectSILResults();
      unsigned opNum = use->getOperandNumber() - 1;
      if (opNum >= numIndirectResults)
        continue;

      // Forbid multiple assignment.
      if (getMemoryValue().getKind() != SymbolicValue::UninitMemory)
        return error(evaluator.getUnknown(addr, UnknownReason::Default));

      // The callee needs to be a direct call to a constant expression.
      auto callResult = computeCallResult(apply);

      // If the call failed, we're done.
      if (callResult.hasValue())
        return error(*callResult);

      // computeCallResult will have figured out the result and cached it for
      // us.
      assert(getMemoryValue().isConstant());
      continue;
    }

    // If it is an index_addr, make sure it is a different address from base.
    if (auto *iai = dyn_cast<IndexAddrInst>(user)) {
      assert(use->get() == iai->getBase());
      if (auto *ili = dyn_cast<IntegerLiteralInst>(iai->getIndex())) {
        if (ili->getValue().getLimitedValue() != 0)
          continue;
      }
      return error(evaluator.getUnknown(addr, UnknownReason::Default));
    }

    if (auto *teai = dyn_cast<TupleElementAddrInst>(user)) {
      // Try finding a writer among the users of `teai`. For example:
      //   %179 = alloc_stack $(Int32, Int32, Int32, Int32)
      //   %183 = tuple_element_addr %179 : $*(Int32, Int32, Int32, Int32), 3
      //   copy_addr %114 to [initialization] %183 : $*Int32
      //   %191 = tuple_element_addr %179 : $*(Int32, Int32, Int32, Int32), 3
      //   copy_addr [take] %191 to [initialization] %178 : $*Int32
      //
      // The workflow is: when const-evaluating %178, we const-evaluate %191,
      // which in turn triggers const-evaluating %179, thereby enter this
      // function, where `addrInst` being %179. Among its users, %191 is not an
      // initializer, so we skip it (`initializeAddressFromSingleWriter(teai)`
      // below will act as a no-op on it). %183 is a good initializer and can
      // be const-evaluated (by const-evaluating %114).

      // We can't forbid multiple assignment here by checking for uninit memory,
      // because previous TupleElementAddrInsts may have already partially
      // initialized the memory. However, the recursive call to
      // `initializeAddressFromSingleWriter` below detects and forbids multiple
      // assignment, so we don't need to do it here.

      if (auto failure = initializeAddressFromSingleWriter(teai))
        return error(*failure);

      // If this instruction partially initialized the memory, then we must
      // remember to check later that the memory has been fully initialized.
      if (getMemoryValue().getKind() != SymbolicValue::UninitMemory)
        mustCheckAggregateInitialized = true;

#ifndef NDEBUG
      // If all aggregate elements are const, we have successfully
      // const-evaluated the entire tuple!
      if (checkAggregateInitialized())
        LLVM_DEBUG(llvm::dbgs() << "Const-evaluated the entire tuple: ";
                   getMemoryValue().dump());
#endif // NDEBUG
      continue;
    }

    LLVM_DEBUG(llvm::dbgs()
               << "Unknown SingleStore ConstExpr user: " << *user << "\n");

    // If this is some other user that we don't know about, then we should
    // treat it conservatively, because it could store into the address.
    return error(evaluator.getUnknown(addr, UnknownReason::Default));
  }

  if (mustCheckAggregateInitialized && !checkAggregateInitialized())
    return error(evaluator.getUnknown(addr, UnknownReason::Default));

  return None;
}

/// Find the initializer (single writer) of `addr` among it users,
/// const-evaluate it and store the result into a memory object.
///
/// Side effects: Creates a fully-initialized memory object (on success), or a
/// memory object containing an unknown (on failure). Inserts the address of
/// that memory object into `calculatedValues`, with key `addr`.
///
/// Returns the address of the memory object on success. Returns the unknown on
/// failure.
///
/// Some use cases are:
/// 1. When analyzing the top-level code involved in a constant expression, we
/// can end up demanding values that are returned by address.  Handle this by
/// finding the temporary stack value (an alloc_stack inst), and calling this
/// method on it.
/// 2. When const-evaluating an array via decodeAllocUninitializedArray(),
/// do that by const-evaluating the writers of individual array elements.
///
///  There are a few forms of writers, such as:
/// - store %3 to %4 ...
/// - %8 = pointer_to_address %7 : $Builtin.RawPointer to [strict] $*Int32
/// - %14 = index_addr %9 : $*Int32, %13 : $Builtin.Word
/// - %180 = tuple_element_addr %179 : $*(Int32, Int32, Int32, Int32), 3
///
///  Note unlike getConstAddrAndLoadResult(), this method does *not*
///  const-evaluate the input `addr` by evaluating its operand first, such as %7
///  above. Instead, it finds a user of %8 who is the initializer, and uses that
///  to set the const value for %7. In other words, this method propagates const
///  info from result to operand (e.g. from %8 to %7), while
///  getConstAddrAndLoadResult() propagates const info from operand to result.
///
///  As such, when const-evaluating an address-typed inst such as
///  pointer_to_address, if the address is to be written to, caller should call
///  this method (e.g. a[3] = 17). If the address is to be read (e.g. let v =
///  a[3]), call getConstAddrAndLoadResult().
SymbolicValue
ConstExprFunctionState::getSingleWriterAddressValue(SILValue addr) {
  // Check to see if we already have an answer.
  auto it = calculatedValues.find(addr);
  if (it != calculatedValues.end())
    return it->second;

  assert(addr->getType().isAddress());
  auto *addrInst = dyn_cast<SingleValueInstruction>(addr);
  if (!addrInst)
    return evaluator.getUnknown(addr, UnknownReason::Default);

  // Create a memory object to initialize, and point `addr` at it.
  auto memoryAddress =
      createMemoryObject(addr, SymbolicValue::getUninitMemory());
  auto *memoryObject = memoryAddress.getAddressValueMemoryObject();

  if (auto failure = initializeAddressFromSingleWriter(addr)) {
    assert(failure->getKind() == SymbolicValue::Unknown);
    memoryObject->setValue(*failure);
    return *failure;
  }
  if (!memoryObject->getValue().isConstant()) {
    auto unknown = evaluator.getUnknown(addr, UnknownReason::Default);
    memoryObject->setValue(unknown);
    return unknown;
  }

  return memoryAddress;
}

/// Given the operand to a load, resolve it to a constant if possible.
/// Also see the comments on getSingleWriterAddressValue() to contrast these 2
/// APIs.
SymbolicValue ConstExprFunctionState::getConstAddrAndLoadResult(SILValue addr) {
  auto addrVal = getConstantValue(addr);
  if (!addrVal.isConstant())
    return addrVal;

  return loadAddrValue(addr, addrVal);
}

/// Load and return the underlying (const) object whose address is given by
/// `addrVal`. On error, return a message based on `addr`.
SymbolicValue ConstExprFunctionState::loadAddrValue(SILValue addr,
                                                    SymbolicValue addrVal) {
  SmallVector<unsigned, 4> accessPath;
  auto *memoryObject = addrVal.getAddressValue(accessPath);

  // If this is a derived address, then we are digging into an aggregate
  // value.
  auto objectVal = memoryObject->getValue();

  // Try digging through the aggregate to get to our value.
  unsigned idx = 0, end = accessPath.size();
  while (idx != end && objectVal.getKind() == SymbolicValue::Aggregate) {
    objectVal = objectVal.getAggregateValue()[accessPath[idx]];
    ++idx;
  }

  // If we successfully indexed down to our value, then we're done.
  if (idx == end)
    return objectVal;

  // If the memory object had a reason, return it.
  if (objectVal.isUnknown())
    return objectVal;

  // Otherwise, return a generic failure.
  return evaluator.getUnknown(addr, UnknownReason::Default);
}

/// Evaluate a flow sensitive store to the specified pointer address.
llvm::Optional<SymbolicValue>
ConstExprFunctionState::computeFSStore(SymbolicValue storedCst, SILValue dest) {
  // Only update existing memory locations that we're tracking.
  auto it = calculatedValues.find(dest);
  if (it == calculatedValues.end() || !it->second.isConstant())
    return evaluator.getUnknown(dest, UnknownReason::Default);

  SmallVector<unsigned, 4> accessPath;
  auto *memoryObject = it->second.getAddressValue(accessPath);
  memoryObject->setIndexedElement(accessPath, storedCst,
                                  evaluator.getAllocator());
  return None;
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

  // If this is a special flow-sensitive instruction like a stack allocation,
  // store, copy_addr, etc, we handle it specially here.
  if (auto asi = dyn_cast<AllocStackInst>(inst)) {
    createMemoryObject(asi, SymbolicValue::getUninitMemory());
    return None;
  }

  // If this is a deallocation of a memory object that we are tracking, then
  // don't do anything.  The memory is allocated in a BumpPtrAllocator so there
  // is no useful way to free it.
  if (isa<DeallocStackInst>(inst))
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

  if (isa<StoreInst>(inst)) {
    auto stored = getConstantValue(inst->getOperand(0));
    if (!stored.isConstant())
      return stored;

    return computeFSStore(stored, inst->getOperand(1));
  }

  // Copy addr is a load + store combination.
  if (auto *copy = dyn_cast<CopyAddrInst>(inst)) {
    auto value = getConstAddrAndLoadResult(copy->getOperand(0));
    if (!value.isConstant())
      return value;

    return computeFSStore(value, copy->getOperand(1));
  }

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
                                       {}, evaluator.getAllocator());

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

    if (isa<SwitchEnumAddrInst>(inst) || isa<SwitchEnumInst>(inst)) {
      SymbolicValue value;
      SwitchEnumInstBase *switchInst = dyn_cast<SwitchEnumInst>(inst);
      if (switchInst) {
        value = state.getConstantValue(switchInst->getOperand());
      } else {
        switchInst = cast<SwitchEnumAddrInst>(inst);
        value = state.getConstAddrAndLoadResult(switchInst->getOperand());
      }
      if (!value.isConstant())
        return value;

      assert(value.getKind() == SymbolicValue::Enum ||
             value.getKind() == SymbolicValue::EnumWithPayload);

      auto *caseBB = switchInst->getCaseDestination(value.getEnumValue());

      // Prepare to subsequently visit the case blocks instructions.
      nextInst = caseBB->begin();
      // Then set up the arguments.
      if (caseBB->getParent()->hasOwnership() &&
          switchInst->getDefaultBBOrNull() == caseBB) {
        // If we are visiting the default block and we are in ossa, then we may
        // have uses of the failure parameter. That means we need to map the
        // original value to the argument.
        state.setValue(caseBB->getArgument(0), value);
        continue;
      }

      if (caseBB->getNumArguments() == 0)
        continue;

      assert(value.getKind() == SymbolicValue::EnumWithPayload);
      // When there are multiple payload components, they form a single
      // tuple-typed argument.
      assert(caseBB->getNumArguments() == 1);
      auto argument = value.getEnumPayloadValue();
      assert(argument.isConstant());
      state.setValue(caseBB->getArgument(0), argument);
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

ConstExprEvaluator::ConstExprEvaluator(SymbolicValueAllocator &alloc)
    : allocator(alloc) {}

ConstExprEvaluator::~ConstExprEvaluator() {}

SymbolicValue ConstExprEvaluator::getUnknown(SILNode *node,
                                             UnknownReason reason) {
  return SymbolicValue::getUnknown(node, reason, getCallStack(),
                                   getAllocator());
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
