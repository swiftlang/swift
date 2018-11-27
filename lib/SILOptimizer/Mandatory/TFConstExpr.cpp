//===--- TFConstExpr.cpp - TensorFlow constant expressions ----------------===//
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

#define DEBUG_TYPE "TFConstExpr"
#include "TFConstExpr.h"
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
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TrailingObjects.h"

using namespace swift;

static llvm::cl::opt<unsigned>
    ConstExprLimit("constexpr-limit", llvm::cl::init(512),
                   llvm::cl::desc("Number of instructions interpreted in a"
                                  " constexpr function"));

static llvm::Optional<SymbolicValue>
evaluateAndCacheCall(SILFunction &fn, SubstitutionMap substitutionMap,
                     ArrayRef<SymbolicValue> arguments,
                     SmallVectorImpl<SymbolicValue> &results,
                     unsigned &numInstEvaluated, ConstExprEvaluator &evaluator);

// TODO: ConstantTracker in the performance inliner and the
// ConstantFolding.h/cpp files should be subsumed by this, as this is a more
// general framework.

// We have a list of functions that we hackily special case.  In order to keep
// this localized, we use this classifier function.  This should be replaced
// with an attribute put on the standard library that captures this info.
enum class WellKnownFunction {
  Unknown,
  // String.init()
  StringInitEmpty,
  // String.init(_builtinStringLiteral:utf8CodeUnitCount:isASCII:)
  StringMakeUTF8,
  // _assertionFailure(_:_:file:line:flags:)
  AssertionFailure,
  // Array._allocateUninitializedArray
  AllocateUninitializedArray,
};

static WellKnownFunction classifyFunction(SILFunction *fn) {
  if (fn->hasSemanticsAttr("string.makeUTF8"))
    return WellKnownFunction::StringMakeUTF8;

  StringRef mangledName = fn->getName();
  if (mangledName == "$sS2SycfC")
    return WellKnownFunction::StringInitEmpty;

  if (mangledName.contains("_assertionFailure"))
    return WellKnownFunction::AssertionFailure;

  if (mangledName.contains("_allocateUninitializedArray"))
    return WellKnownFunction::AllocateUninitializedArray;
  return WellKnownFunction::Unknown;
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
  /// This is the evaluator we put bump pointer allocated values into.
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
  /// by getConstantValue.  This does not hold SIL address values.
  llvm::DenseMap<SILValue, SymbolicValue> calculatedValues;

public:
  ConstExprFunctionState(ConstExprEvaluator &evaluator, SILFunction *fn,
                         SubstitutionMap substitutionMap,
                         unsigned &numInstEvaluated)
      : evaluator(evaluator), fn(fn), substitutionMap(substitutionMap),
        numInstEvaluated(numInstEvaluated) {}

  void setValue(SILValue value, SymbolicValue symVal) {
    LLVM_DEBUG(llvm::dbgs() << "For sil value " << value << ", setting it to: ";
               symVal.dump());
    calculatedValues.insert({value, symVal});
  }

  /// Invariant: Before the call, `calculatedValues` must not contain `addr`
  /// as a key.
  SymbolicValue createMemoryObject(SILValue addr, SymbolicValue initialValue) {
    assert(!calculatedValues.count(addr));
    auto type = simplifyType(addr->getType().getASTType());
    auto *memObject = SymbolicValueMemoryObject::create(
        type, initialValue, evaluator.getAllocator());
    LLVM_DEBUG(llvm::dbgs()
                   << "For sil addr " << addr << ", creating memory obj: "
                   << memObject << ", with init value " << initialValue
                   << ", mem obj value: " << memObject->getValue(););
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

  Type simplifyType(Type ty);
  CanType simplifyType(CanType ty) {
    return simplifyType(Type(ty))->getCanonicalType();
  }
  SymbolicValue computeConstantValue(SILValue value);
  SymbolicValue computeConstantValueBuiltin(BuiltinInst *inst);

  llvm::Optional<SymbolicValue> computeCallResult(ApplyInst *apply);

  llvm::Optional<SymbolicValue> computeOpaqueCallResult(ApplyInst *apply,
                                                        SILFunction *callee);

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
Type ConstExprFunctionState::simplifyType(Type ty) {
  return substitutionMap.empty() ? ty : ty.subst(substitutionMap);
}

// TODO: refactor this out somewhere sharable between autodiff and this code.
static void lookupOrLinkWitnessTable(ProtocolConformanceRef confRef,
                                     SILModule &module) {
  // Cannot resolve abstract conformances.
  if (!confRef.isConcrete())
    return;

  auto *conf = confRef.getConcrete();
  auto wtable = module.lookUpWitnessTable(conf);
  if (wtable)
    return;

  auto *decl =
      conf->getDeclContext()->getSelfNominalTypeDecl();
  auto linkage = getSILLinkage(getDeclLinkage(decl), NotForDefinition);
  auto *newTable = module.createWitnessTableDeclaration(conf, linkage);
  newTable = module.getSILLoader()->lookupWitnessTable(newTable);

  // Update linkage for witness methods.
  // FIXME: Figure out why witnesses have shared linkage by default.
  for (auto &entry : newTable->getEntries())
    if (entry.getKind() == SILWitnessTable::WitnessKind::Method)
      entry.getMethodWitness().Witness->setLinkage(linkage);
}

/// Const-evaluate `value`, which must not have been computed.
SymbolicValue ConstExprFunctionState::computeConstantValue(SILValue value) {
  assert(!calculatedValues.count(value));

  // If the client is asking for the value of a stack object that hasn't been
  // computed, then we are in top level code, and the stack object must be a
  // single store value.  Since this is a very different computation, split it
  // out to its own path.
  if (!fn && value->getType().isAddress() && isa<AllocStackInst>(value)) {
    return getSingleWriterAddressValue(value);
  }

  // If this a trivial constant instruction that we can handle, then fold it
  // immediately.
  if (auto *ili = dyn_cast<IntegerLiteralInst>(value))
    return SymbolicValue::getInteger(ili->getValue(), evaluator.getAllocator());
  if (auto *fli = dyn_cast<FloatLiteralInst>(value))
    return SymbolicValue::getFloat(fli->getValue(), evaluator.getAllocator());
  if (auto *sli = dyn_cast<StringLiteralInst>(value))
    return SymbolicValue::getString(sli->getValue(), evaluator.getAllocator());

  if (auto *fri = dyn_cast<FunctionRefInst>(value))
    return SymbolicValue::getFunction(fri->getReferencedFunction(),
                                      FunctionSubstitutionConvention::Normal);
  
  if (auto *tttfi = dyn_cast<ThinToThickFunctionInst>(value))
    return getConstantValue(tttfi->getOperand());

  if (auto *cfi = dyn_cast<ConvertFunctionInst>(value))
    return getConstantValue(cfi->getOperand());

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
      if (val.getKind() == SymbolicValue::Aggregate)
        return val.getAggregateValue()[sei->getFieldNo()];
      else {
        // `val` can be an array. e.g. It can have SIL type $Array<Int32> and be
        // produce by an inst such as:
        //   %133 = struct_extract %132 : $TensorShape, #TensorShape.dimensions
        assert(sei->getFieldNo() == 0);
        return val;
      }
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
  // already (when analyzing the body of a constexpr) or this should be a by-ref
  // result of a call.  Either way, we ask for the value of the pointer: in the
  // former case this will be the latest value for this, in the later case, this
  // must be a single-def value for us to analyze it.
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

    // Look up the conformance's withness table and the member out of it.
    SILFunction *fn =
        module.lookUpFunctionInWitnessTable(conf, wmi->getMember()).first;
    if (!fn) {
      // If that failed, try force loading it, and try again.
      lookupOrLinkWitnessTable(conf, wmi->getModule());
      fn = module.lookUpFunctionInWitnessTable(conf, wmi->getMember()).first;
    }

    // If we were able to resolve it, then we can proceed.
    if (fn)
      return SymbolicValue::getFunction(
          fn, FunctionSubstitutionConvention::Witness);

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

  // Builtin.RawPointer and addresses have the same representation.
  if (auto *p2ai = dyn_cast<PointerToAddressInst>(value))
    return getConstantValue(p2ai->getOperand());

  // Indexing a pointer moves its deepest index.
  if (auto *ia = dyn_cast<IndexAddrInst>(value)) {
    auto index = getConstantValue(ia->getOperand(1));
    if (!index.isConstant())
      return index;
    auto basePtr = getConstantValue(ia->getOperand(0));
    if (basePtr.getKind() != SymbolicValue::Address)
      return basePtr;

    SmallVector<unsigned, 4> accessPath;
    auto *memObject = basePtr.getAddressValue(accessPath);
    assert(!accessPath.empty() && "Can't index a non-indexed address");
    accessPath.back() += index.getIntegerValue().getLimitedValue();
    return SymbolicValue::getAddress(memObject, accessPath,
                                     evaluator.getAllocator());
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

  // Nullary operations.
  if (inst->getNumOperands() == 0) {
    switch (builtin.ID) {
    default:
      break;
    case BuiltinValueKind::AssertConf: {
      // assert_configuration builtin gets replaces with debug/release/etc
      // constants.
      auto config = inst->getModule().getOptions().AssertConfig;
      // Don't replace assert_configuration if we're not supposed to.
      if (config == SILOptions::DisableReplacement)
        break;
      auto resultBitWidth =
          inst->getType().castTo<BuiltinIntegerType>()->getGreatestWidth();
      auto result = APInt(resultBitWidth, config);
      return SymbolicValue::getInteger(result, evaluator.getAllocator());
    }
    }
  }

  // Unary operations.
  if (inst->getNumOperands() == 1) {
    auto operand = getConstantValue(inst->getOperand(0));
    // TODO: Could add a "value used here" sort of diagnostic.
    if (!operand.isConstant())
      return operand;

    // TODO: SUCheckedConversion/USCheckedConversion

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

      // Get source type and bit width.
      auto SrcTy = builtin.Types[0]->castTo<AnyBuiltinIntegerType>();
      assert((srcSigned || !isa<BuiltinIntegerLiteralType>(SrcTy)) &&
             "only the signed intrinsics can be used with integer literals");

      assert((dstBitWidth < srcBitWidth || !SrcTy->getWidth().isFixedWidth()) &&
             "preconditions on builtin trunc operations should prevent"
             "fixed-width truncations that actually extend");
      APInt result;
      bool overflowed;
      if (dstBitWidth > srcBitWidth) {
        // The only way a true extension can overflow is if the value is
        // negative and the result is unsigned.
        overflowed = (srcSigned && !dstSigned && operandVal.isNegative());
        result =
            srcSigned ? operandVal.sext(dstBitWidth) : operandVal.zext(dstBitWidth);
      } else if (dstBitWidth == srcBitWidth) {
        // A same-width change can overflow if the top bit disagrees.
        overflowed = (srcSigned != dstSigned && operandVal.isNegative());
      } else {
        // Truncate the result and check for overflow.
        result = operandVal.trunc(dstBitWidth);

        // Compute the overflow by re-extending the value back to its source and
        // checking for loss of value.
        APInt reextended =
          dstSigned ? result.sext(srcBitWidth) : result.zext(srcBitWidth);
        overflowed = (operandVal != reextended);

        if (builtin.ID == BuiltinValueKind::UToSCheckedTrunc)
          overflowed |= result.isSignBitSet();
      }
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
    case BuiltinValueKind::PtrToInt:
    case BuiltinValueKind::IntToPtr:
      // These are semantically just copies, they exist to change SIL types.
      return operand;
    case BuiltinValueKind::SToSCheckedTrunc:
      return IntCheckedTruncFn(true, true);
    case BuiltinValueKind::UToSCheckedTrunc:
      return IntCheckedTruncFn(false, true);
    case BuiltinValueKind::SToUCheckedTrunc:
      return IntCheckedTruncFn(true, false);
    case BuiltinValueKind::UToUCheckedTrunc:
      return IntCheckedTruncFn(false, false);
    case BuiltinValueKind::SIToFP:
    case BuiltinValueKind::UIToFP: {
      if (operand.getKind() != SymbolicValue::Integer)
        return unknownResult();

      auto operandVal = operand.getIntegerValue();
      auto &semantics =
          inst->getType().castTo<BuiltinFloatType>()->getAPFloatSemantics();
      APFloat apf(semantics,
                  APInt::getNullValue(APFloat::semanticsSizeInBits(semantics)));
      apf.convertFromAPInt(operandVal, builtin.ID == BuiltinValueKind::SIToFP,
                           APFloat::rmNearestTiesToEven);
      return SymbolicValue::getFloat(apf, evaluator.getAllocator());
    }

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
    auto constFoldFPCompare =
        [&](const std::function<bool(APFloat::cmpResult result)> &fn)
        -> SymbolicValue {
      if (operand0.getKind() != SymbolicValue::Float ||
          operand1.getKind() != SymbolicValue::Float)
        return unknownResult();

      auto comparison =
          operand0.getFloatValue().compare(operand1.getFloatValue());
      return SymbolicValue::getInteger(APInt(1, fn(comparison)),
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
#define FP_BINOP(OPCODE, EXPR)                                                 \
  case BuiltinValueKind::OPCODE: {                                             \
    REQUIRE_KIND(Float)                                                        \
    auto l = operand0.getFloatValue(), r = operand1.getFloatValue();           \
    return SymbolicValue::getFloat((EXPR), evaluator.getAllocator());          \
  }
      FP_BINOP(FAdd, l + r)
      FP_BINOP(FSub, l - r)
      FP_BINOP(FMul, l * r)
      FP_BINOP(FDiv, l / r)
      FP_BINOP(FRem, (l.mod(r), l))
#undef FP_BINOP

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
#define FP_COMPARE(OPCODE, EXPR)                                               \
  case BuiltinValueKind::OPCODE:                                               \
    REQUIRE_KIND(Float)                                                        \
    return constFoldFPCompare(                                                 \
        [&](APFloat::cmpResult result) -> bool { return (EXPR); })
      FP_COMPARE(FCMP_OEQ, result == APFloat::cmpEqual);
      FP_COMPARE(FCMP_OGT, result == APFloat::cmpGreaterThan);
      FP_COMPARE(FCMP_OGE, result == APFloat::cmpGreaterThan ||
                               result == APFloat::cmpEqual);
      FP_COMPARE(FCMP_OLT, result == APFloat::cmpLessThan);
      FP_COMPARE(FCMP_OLE,
                 result == APFloat::cmpLessThan || result == APFloat::cmpEqual);
      FP_COMPARE(FCMP_ONE, result == APFloat::cmpLessThan ||
                               result == APFloat::cmpGreaterThan);
      FP_COMPARE(FCMP_ORD, result != APFloat::cmpUnordered);
      FP_COMPARE(FCMP_UEQ, result == APFloat::cmpUnordered ||
                               result == APFloat::cmpEqual);
      FP_COMPARE(FCMP_UGT, result == APFloat::cmpUnordered ||
                               result == APFloat::cmpGreaterThan);
      FP_COMPARE(FCMP_UGE, result != APFloat::cmpLessThan);
      FP_COMPARE(FCMP_ULT, result == APFloat::cmpUnordered ||
                               result == APFloat::cmpLessThan);
      FP_COMPARE(FCMP_ULE, result != APFloat::cmpGreaterThan);
      FP_COMPARE(FCMP_UNE, result != APFloat::cmpEqual);
      FP_COMPARE(FCMP_UNO, result == APFloat::cmpUnordered);
#undef FP_COMPARE
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

  // Handle LLVM intrinsics.
  auto &intrinsic = inst->getIntrinsicInfo();
  switch (intrinsic.ID) {
  default:
    break;
  case llvm::Intrinsic::expect:
    // llvm.expect(x, y) always lowers to x.
    return getConstantValue(inst->getOperand(0));
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
  // This is a termination point like fatalError.
  if (callee->hasSemanticsAttr("arc.programtermination_point")) {
    // TODO: Actually get the message out of fatalError.  We can literally get
    // its string and file/line/col info and propagate it up!
    return evaluator.getUnknown((SILInstruction *)apply, UnknownReason::Trap);
  }

  if (classifyFunction(callee) == WellKnownFunction::AssertionFailure) {
    // TODO: Actually get the message out of fatalError.  We can literally get
    // its string and file/line/col info and propagate it up!
    return evaluator.getUnknown((SILInstruction *)apply, UnknownReason::Trap);
  }

  LLVM_DEBUG(llvm::dbgs() << "ConstExpr Opaque Callee: " << *callee << "\n");
  return evaluator.getUnknown((SILInstruction *)apply, UnknownReason::Default);
}

/// If the specified type is a Swift.Array of some element type, then return the
/// element type.  Otherwise, return a null Type.
static Type getArrayElementType(Type ty) {
  if (auto bgst = ty->getAs<BoundGenericStructType>())
    if (bgst->getDecl() == bgst->getASTContext().getArrayDecl())
      return bgst->getGenericArgs()[0];
  return Type();
}

// TODO: Refactor this to someplace common, this is defined in Devirtualize.cpp.
SubstitutionMap getWitnessMethodSubstitutions(SILModule &Module, ApplySite AI,
                                              SILFunction *F,
                                              ProtocolConformanceRef CRef);

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

  // TODO: Verify that the callee was defined as a @constexpr function.

  // If this is a well-known function, do not step into it.
  //
  // FIXME: This should be based on the SILFunction carrying a
  // @constexprSemantics sort of attribute that indicates it is well known,
  // just like the existing SemanticsAttr thing.
  switch (classifyFunction(callee)) {
  default:
    break;
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
    if (literal.getKind() != SymbolicValue::String)
      break;
    auto literalVal = literal.getStringValue();

    auto byteCount = getConstantValue(apply->getOperand(2));
    if (byteCount.getKind() != SymbolicValue::Integer ||
        byteCount.getIntegerValue().getLimitedValue() != literalVal.size())
      break;
    setValue(apply, literal);
    return None;
  }
  case WellKnownFunction::AllocateUninitializedArray: {
    // This function has this signature:
    //   func _allocateUninitializedArray<Element>(_ builtinCount: Builtin.Word)
    //     -> (Array<Element>, Builtin.RawPointer)

    // Figure out the allocation size.
    auto numElementsSV = getConstantValue(apply->getOperand(1));
    if (!numElementsSV.isConstant())
      return numElementsSV;

    unsigned numElements = numElementsSV.getIntegerValue().getLimitedValue();

    SmallVector<SymbolicValue, 8> elementConstants;
    if (fn) {
      // In the flow sensitive case, we can analyze this as an allocation of
      // uninitialized array memory, and allow the stores to the pointer result
      // to initialize the elements in a normal flow sensitive way.
      elementConstants.assign(numElements, SymbolicValue::getUninitMemory());
    } else {
      // We handle the flow-insensitive case specially in order to find the
      // stores/apply's to initialize the array elements.  Collect them here
      // and pretend that the array was initialized atomically.
      SmallVector<Operand*, 8> elementsAtInit;
      if (ConstExprEvaluator::decodeAllocUninitializedArray(
              apply, numElements, elementsAtInit,
              /*arrayInsts*/ nullptr))
        return evaluator.getUnknown((SILInstruction *)apply,
                                    UnknownReason::Default);

      // Okay, we were able to decode the array.  See if we can fold all of the
      // elements we found.
      for (auto *use : elementsAtInit) {
        auto addr = use->get();
        assert(addr->getType().isAddress());
        SymbolicValue addrVal = getSingleWriterAddressValue(addr);
        if (!addrVal.isConstant())
          return addrVal;
        SymbolicValue eltCst = loadAddrValue(addr, addrVal);
        if (!eltCst.isConstant())
          return eltCst;
        elementConstants.push_back(eltCst);
      }
    }

    auto arrayType = apply->getType().castTo<TupleType>()->getElementType(0);
    auto arrayEltType = getArrayElementType(arrayType);
    assert(arrayEltType && "Couldn't understand Swift.Array type?");

    // Build this value as an array of elements.  Wrap it up into a memory
    // object with an address refering to it.
    auto arrayVal = SymbolicValue::getArray(elementConstants,
                                            arrayEltType->getCanonicalType(),
                                            evaluator.getAllocator());

    auto *memObject = SymbolicValueMemoryObject::create(
        arrayType, arrayVal, evaluator.getAllocator());

    // Okay, now we have the array memory object, return the indirect array
    // value and the pointer object we need for the tuple result.
    auto indirectArr = SymbolicValue::getArrayAddress(memObject);

    // The address notationally points to the first element of the array.
    auto address =
        SymbolicValue::getAddress(memObject, {0}, evaluator.getAllocator());

    setValue(apply, SymbolicValue::getAggregate({indirectArr, address},
                                                evaluator.getAllocator()));
    return None;
  }
  }

  // If we reached an external function that hasn't been deserialized yet, make
  // sure to pull it in so we can see its body.  If that fails, then we can't
  // analyze the function.
  if (callee->isExternalDeclaration()) {
    callee->getModule().loadFunction(callee);
    if (callee->isExternalDeclaration())
      return computeOpaqueCallResult(apply, callee);
  }

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

  // Compute the substitution map for the callee, which maps from all of its
  // generic requirements to concrete conformances and concrete types.
  SubstitutionMap calleeSubMap;

  auto calleeFnType = callee->getLoweredFunctionType();
  if (calleeFnType->getGenericSignature()) {
    ApplySite AI(apply);

    // Get the substitution map of the call.  This maps from the callee's space
    // into the caller's world.
    SubstitutionMap callSubMap;
    if (calleeFn.getFunctionSubstitutionConvention() ==
        FunctionSubstitutionConvention::Witness) {
      // Get the conformance out of the SILFunctionType and map it into our
      // current type space.
      auto conformance = calleeFnType->getWitnessMethodConformance();
      auto selfTy = conformance.getRequirement()->getSelfInterfaceType();
      auto conf = substitutionMap.lookupConformance(
          selfTy->getCanonicalType(), conformance.getRequirement());
      if (!conf.hasValue())
        return evaluator.getUnknown((SILInstruction *)apply,
                                    UnknownReason::Default);

      // Witness methods have special magic that is required to resolve them.
      callSubMap = getWitnessMethodSubstitutions(apply->getModule(), AI, callee,
                                                 conf.getValue());
    } else {
      auto requirementSig = AI.getOrigCalleeType()->getGenericSignature();
      callSubMap =
          SubstitutionMap::get(requirementSig, apply->getSubstitutionMap());
    }

    // The substitution map for the callee is the composition of the callers
    // substitution map (which is always type/conformance to a concrete type
    // or conformance, with the mapping introduced by the call itself.  This
    // ensures that the callee's substitution map can map from its type
    // namespace back to concrete types and conformances.
    calleeSubMap = callSubMap.subst(substitutionMap);
  }

  // Now that have successfully folded all of the parameters, we can evaluate
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

    // TODO: BeginAccess/EndAccess.

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
    //
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
      //   copy_addr %114 to [initialization] %183 : $*Int32
      //   %179 = alloc_stack $(Int32, Int32, Int32, Int32)
      //   %183 = tuple_element_addr %179 : $*(Int32, Int32, Int32, Int32), 3
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
      // Constant have no important state.
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

  // If this is a deallocation of a memory object that we may be tracking,
  // remove the memory from the set.  We don't *have* to do this, but it seems
  // useful for hygiene.
  if (isa<DeallocStackInst>(inst)) {
    calculatedValues.erase(inst->getOperand(0));
    return None;
  }

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
      // values as well any indirect results.
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
      // Set up basic block arguments.
      auto *caseBB = switchInst->getCaseDestination(value.getEnumValue());
      if (caseBB->getNumArguments() > 0) {
        assert(value.getKind() == SymbolicValue::EnumWithPayload);
        // When there are multiple payload components, they form a single
        // tuple-typed argument.
        assert(caseBB->getNumArguments() == 1);
        auto argument = value.getEnumPayloadValue();
        assert(argument.isConstant());
        state.setValue(caseBB->getArgument(0), argument);
      }
      nextInst = caseBB->begin();
      continue;
    }

    LLVM_DEBUG(llvm::dbgs()
               << "ConstExpr: Unknown Terminator: " << *inst << "\n");

    // TODO: Enum switches when we support enums?
    return evaluator.getUnknown(inst, UnknownReason::Default);
  }
}

//===----------------------------------------------------------------------===//
// ConstExprEvaluator implementation.
//===----------------------------------------------------------------------===//

ConstExprEvaluator::ConstExprEvaluator(SILModule &m)
    : allocator(m.getASTContext().getAllocator()) {}

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
/// that occur after after folding them.
void ConstExprEvaluator::computeConstantValues(
    ArrayRef<SILValue> values, SmallVectorImpl<SymbolicValue> &results) {
  unsigned numInstEvaluated = 0;
  ConstExprFunctionState state(*this, nullptr, {}, numInstEvaluated);
  for (auto v : values) {
    auto symVal = state.getConstantValue(v);
    results.push_back(symVal);

    // Reset the execution limit back to zero for each subsexpression we look
    // at.  We don't want lots of constants folded to trigger a limit.
    numInstEvaluated = 0;
  }
}

/// Analyze the array users of an _allocateUninitialized call, to see if they
/// are all simple things we can remove.  If so, add them all to arrayInsts and
/// return false.  If not, return true.
static bool analyzeArrayInitUses(SILValue v,
                                 SmallPtrSet<SILInstruction *, 8> *arrayInsts) {
  for (auto *use : v->getUses()) {
    auto *user = use->getUser();

    // We can always remove retain/release instructions and debug_value.
    if (isa<StrongRetainInst>(user) || isa<StrongReleaseInst>(user) ||
        isa<RetainValueInst>(user) || isa<ReleaseValueInst>(user) ||
        isa<DebugValueInst>(user)) {
      if (arrayInsts)
        arrayInsts->insert(user);
      continue;
    }

    // We can look through unpacking and repacking of structs that eventually
    // turn into a retain or release.
    if (isa<StructExtractInst>(user) || isa<StructInst>(user)) {
      if (arrayInsts)
        arrayInsts->insert(user);
      if (analyzeArrayInitUses(user->getResults()[0], arrayInsts))
        return true;
      continue;
    }

    // Oops we found an unknown use!
    return true;
  }
  return false;
}

/// Try to decode the specified apply of the _allocateUninitializedArray
/// function in the standard library.  This attempts to figure out how the
/// resulting elements will be initialized.  This fills in the result with a
/// lists of insts used to pass element addresses for initialization, and
/// returns false on success.
///
/// Specifically, elementsAtInit[i] is an operand, where the associated
/// instruction returns the address for array element i, and the user of that
/// operand is a writer for that array element. For example, the user can be a
/// store inst into that array element.
///
/// If arrayInsts is non-null and if decoding succeeds, this function adds
/// all of the instructions relevant to the definition of this array into
/// the set.  If decoding fails, then the contents of this set is undefined.
///
/// Some example array element initializers that we can handle:
/// 1. %78 below
///   %78 = index_addr %73 : $*Int32, %77 : $Builtin.Word // user: %86
///   function_ref SignedInteger<>.init<A>(_:)
///   %85 = function_ref @... // user: %86
///   // Here the func call writes to %78.
///   %86 = apply %85<Int32, Int>(%78, %83, %80) : $@convention(method)
///
/// 2. %132 below, and similar insts that initialize other tuple elts of %131.
///   %132 = tuple_element_addr %131 : $*(Int32, Int32, Int32, Int32), 0
///
bool ConstExprEvaluator::decodeAllocUninitializedArray(
    ApplyInst *apply, uint64_t numElements,
    SmallVectorImpl<Operand*> &elementsAtInit,
    SmallPtrSet<SILInstruction *, 8> *arrayInsts) {
  elementsAtInit.resize(numElements);

  // The apply is part of the call.
  if (arrayInsts)
    arrayInsts->insert(apply);

  // Keep track of whether we have any unknown users.  If so, the caller cannot
  // remove the initializer.
  bool hadUnknownUsers = false;

  // The call has a tuple return type, see if we can walk all uses of them to
  // analyze them and find the stores/apply's to the elements.
  for (auto *use : apply->getUses()) {
    auto *user = use->getUser();

    // Ignore these users.
    if (isa<RetainValueInst>(user) || isa<ReleaseValueInst>(user) ||
        isa<DebugValueInst>(user)) {
      if (arrayInsts)
        arrayInsts->insert(user);
      continue;
    }

    auto *tupleExtract = dyn_cast<TupleExtractInst>(user);
    if (!tupleExtract)
      return true;
    if (arrayInsts)
      arrayInsts->insert(tupleExtract);

    // If this is the array result of _allocateUninitialized, try to determine
    // whether there is anything that would prevent removing the allocation.
    if (tupleExtract->getFieldNo() == 0) {
      hadUnknownUsers |= analyzeArrayInitUses(tupleExtract, arrayInsts);
      continue;
    }

    // Otherwise, it must be the pointer result.
    assert(tupleExtract->getFieldNo() == 1 && "allocUninit has two results");

    // Look through pointer_to_address
    auto pointer2addr =
        tupleExtract->getSingleUserOfType<PointerToAddressInst>();
    if (!pointer2addr)
      return true;
    if (arrayInsts)
      arrayInsts->insert(pointer2addr);

    // Okay, process the use list of the pointer_to_address, each user of
    // interest is either an index_addr inst that specifies an array element
    // (see `index` below), or a writer to a specific array element. When we
    // find such a use/user pair, set `elementsAtInit[index]` to it.
    for (auto *use : pointer2addr->getUses()) {
      auto *user = use->getUser();

      uint64_t index = 0;
      if (auto *iai = dyn_cast<IndexAddrInst>(user)) {
        if (arrayInsts)
          arrayInsts->insert(iai);

        auto *ili = dyn_cast<IntegerLiteralInst>(iai->getOperand(1));
        if (!ili)
          return true;

        index = ili->getValue().getLimitedValue();
        use = iai->getSingleUse();
        if (!use)
          return true;
        user = use->getUser();
      }

      // We handle the cases that the element is either set by a store or
      // filled via an apply.
      if (auto *store = dyn_cast<StoreInst>(user)) {
        if (store->getDest() != use->get())
          return true;
        if (arrayInsts)
          arrayInsts->insert(store);
      } else if (auto *applyInst = dyn_cast<ApplyInst>(user)) {
        // In this case, the element's address is passed to an apply where
        // the initialization happens in-place.  For example, the SIL snippet
        // may look like below:
        //
        //   %input_value_addr = something $*Int
        //   %elt_ty = metatype $@thick Int32.Type
        //   %elt_addr = something $*Int32
        //   %func = function_ref SignedInteger<>.init<A>(_:)
        //   %user = apply %func<Int32, Int>(%elt_addr, %input_value_addr, %elt_ty) : $@convention(method) <U, V> (@in V, @thick U.Type) -> @out U
        //
        // Here, %elt_addr is used as a @out parameter by the apply, and gets
        // filled with the value in %input_value_addr.

        // Check to see if this is an out-parameter (i.e., like a store).
        auto conventions = applyInst->getSubstCalleeConv();
        unsigned numIndirectResults = conventions.getNumIndirectSILResults();
        unsigned argIndex = use->getOperandNumber() - 1;
        if (argIndex >= numIndirectResults)
          return true;
        // An apply can have arbitrary side effects, so let's be conservative.
        hadUnknownUsers = true;
      } else if (auto *cai = dyn_cast<CopyAddrInst>(user)) {
        // In this case, the element's address is passed to a copy_addr, which
        // sets the value. For example, let %178 be the index_addr inst, the SIL
        // snippet that initializes the value given by index_addr might look
        // like the following, where we set `elementsAtInit[index]` to
        // %178. Caller will then const-evaluate %191 (the user of %178), which
        // const-evaluates %179, which involves const-evaluating %183, where the
        // writer to that tuple elt address is "copy_addr %114". Eventually it
        // const-evaluates %101 to fill in the value for %183. The continues
        // until the writer insts of all the other tuple elements of %179 are
        // const-evaluated, yielding a const tuple for %179. This finally gives
        // us the const value at address %178.
        //
        //   %101 = tuple_extract %96 : $(Int32, Int32, Int32, Int32), 3
        //   %108 = alloc_stack $Int32
        //   store %101 to %108 : $*Int32
        //   %114 = tuple_element_addr %110 : $*(Int32, Int32, Int32, Int32), 3
        //   %121 = tuple_element_addr %110 : $*(Int32, Int32, Int32, Int32), 3
        //   copy_addr [take] %108 to [initialization] %121 : $*Int32
        //   copy_addr %114 to [initialization] %183 : $*Int32
        //   %177 = integer_literal $Builtin.Word, 3
        //   %178 = index_addr %130 : $*Int32, %177
        //   %179 = alloc_stack $(Int32, Int32, Int32, Int32)
        //   %183 = tuple_element_addr %179 : $*(Int32, Int32, Int32, Int32), 3
        //   %191 = tuple_element_addr %179 : $*(Int32, Int32, Int32, Int32), 3
        //   copy_addr [take] %191 to [initialization] %178 : $*Int32

        if (cai->getOperand(1) != use->get())
          return true;
        if (arrayInsts)
          arrayInsts->insert(cai);
      } else
        return true;

      // Check to see if we have a valid index that hasn't been recorded yet.
      if (index >= elementsAtInit.size() || elementsAtInit[index])
        return true;

      // If we got a store/apply to a valid index, it must be our element.
      elementsAtInit[index] = use;

      // Track how many elements we see so we can know if we got them all.
      --numElements;
    }
  }

  // Make sure that all of the elements were found.
  if (numElements != 0)
    return true;

  if (hadUnknownUsers && arrayInsts)
    arrayInsts->clear();

  return false;
}
