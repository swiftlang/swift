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
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILConstants.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TrailingObjects.h"

using namespace swift;
using namespace tf;

static llvm::cl::opt<unsigned>
ConstExprLimit("constexpr-limit", llvm::cl::init(512),
               llvm::cl::desc("Number of instructions interpreted in a"
                              " constexpr function"));

static llvm::Optional<SymbolicValue>
evaluateAndCacheCall(SILFunction &fn, SubstitutionMap substitutionMap,
                     ArrayRef<SymbolicValue> arguments,
                     SmallVectorImpl<SymbolicValue> &results,
                     unsigned &numInstEvaluated,
                     ConstExprEvaluator &evaluator);

// TODO: ConstantTracker in the performance inliner and the
// ConstantFolding.h/cpp files should be subsumed by this, as this is a more
// general framework.


// We have a list of functions that we hackily special case.  In order to keep
// this localized, we use this classifier function.  This should be replaced
// with an attribute put on the standard library that captures this info.
enum class WellKnownFunction {
  Unknown,
  StringInitEmpty,  // String.init()
  AssertionFailure, // _assertionFailure(_:_:file:line:flags:)
};


static WellKnownFunction classifyFunction(StringRef mangledName) {
  if (mangledName == "$SS2SycfC")
    return WellKnownFunction::StringInitEmpty;

  if (mangledName.contains("_assertionFailure"))
    return WellKnownFunction::AssertionFailure;
  return WellKnownFunction::Unknown;
}



//===----------------------------------------------------------------------===//
// AddressValue implementation.
//===----------------------------------------------------------------------===//

namespace {
  /// This is a representation of an address value, stored as a base ID plus
  /// trailing array of indices.  Elements of this value are bump-pointer
  /// allocated.
  struct alignas(unsigned) DerivedAddressValue final
  : private llvm::TrailingObjects<DerivedAddressValue, unsigned> {
    friend class llvm::TrailingObjects<DerivedAddressValue, unsigned>;

    const unsigned objectID;
    const unsigned numIndices;

    static DerivedAddressValue *create(unsigned objectID,
                                        ArrayRef<unsigned> indices,
                                        llvm::BumpPtrAllocator &allocator) {
      auto byteSize =
        DerivedAddressValue::totalSizeToAlloc<unsigned>(indices.size());
      auto rawMem = allocator.Allocate(byteSize, alignof(DerivedAddressValue));

      //  Placement initialize the AddressSymbolicValue.
      auto alv = ::new (rawMem) DerivedAddressValue(objectID, indices.size());
      std::uninitialized_copy(indices.begin(), indices.end(),
                              alv->getTrailingObjects<unsigned>());
      return alv;
    }

    ArrayRef<unsigned> getIndices() const {
      return { getTrailingObjects<unsigned>(), numIndices };
    }

    // This is used by the llvm::TrailingObjects base class.
    size_t numTrailingObjects(OverloadToken<unsigned>) const {
      return numIndices;
    }
  private:
    DerivedAddressValue() = delete;
    DerivedAddressValue(const DerivedAddressValue &) = delete;
    DerivedAddressValue(unsigned objectID, unsigned numIndices)
      : objectID(objectID), numIndices(numIndices) {}
  };

  /// AddressValue is our model for (possibly derived) SILValue pointers.  Given
  /// a SILValue, we need to identify which slot in the MemoryObjects list they
  /// reference, along with the accesspath (a sequence of struct_element_addr
  /// and tuple_element_addr's) into the memory object.
  ///
  /// This is a small POD value that is intended to be stored in the value of a
  /// DenseMap.
  class AddressValue {
    typedef llvm::PointerEmbeddedInt<unsigned, 31> InlineRepresentationTy;
    // An address value is either a directly stored integer, or a pointer that
    // holds an access path.
    PointerUnion<InlineRepresentationTy, DerivedAddressValue*> value;

    AddressValue(PointerUnion<InlineRepresentationTy,
                              DerivedAddressValue*> value) : value(value) {}
  public:

    static AddressValue get(unsigned objectID) {
      auto val = InlineRepresentationTy(objectID);
      return AddressValue(val);
    }

    static AddressValue get(unsigned objectID, ArrayRef<unsigned> indices,
                            llvm::BumpPtrAllocator &allocator) {
      if (indices.empty())
        return get(objectID);
      auto val = DerivedAddressValue::create(objectID, indices, allocator);
      return AddressValue(val);
    }

    /// Return the ID of the memory object being referenced.
    unsigned getObjectID() const {
      if (value.is<InlineRepresentationTy>())
        return value.get<InlineRepresentationTy>();
      return value.get<DerivedAddressValue*>()->objectID;
    }

    /// Return the memory object of this reference along with any access path
    /// indices involved.
    unsigned getState(SmallVectorImpl<unsigned> &accessPath) const;

    void dump() const;
  };
} // end anonymous namespace

unsigned AddressValue::getState(SmallVectorImpl<unsigned> &accessPath) const {
  if (value.is<InlineRepresentationTy>())
    return value.get<InlineRepresentationTy>();

  auto derived = value.get<DerivedAddressValue*>();
  auto indices = derived->getIndices();

  accessPath.clear();
  accessPath.reserve(indices.size());
  accessPath.append(indices.begin(), indices.end());
  return derived->objectID;
}

void AddressValue::dump() const {
  SmallVector<unsigned, 4> accessPath;
  unsigned objectID = getState(accessPath);
  llvm::errs() << "Address[#" << objectID << "] ";
  interleave(accessPath.begin(), accessPath.end(),
             [&](unsigned idx) { llvm::errs() << idx; },
             [&]() { llvm::errs() << ", "; });
  llvm::errs() << "\n";
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

    /// Memory objects tracked by the evaluator each get an entry in this array.
    /// In addition to the current value, we track the type of the whole object.
    SmallVector<std::pair<SymbolicValue, Type>, 8> memoryObjects;

    /// Each SIL address gets an entry in this mapping, indicating which memory
    /// object it is addressing and any indices into that object.
    llvm::DenseMap<SILValue, AddressValue> addressValues;

  public:
    ConstExprFunctionState(ConstExprEvaluator &evaluator, SILFunction *fn,
                           SubstitutionMap substitutionMap,
                           unsigned &numInstEvaluated)
      : evaluator(evaluator), fn(fn), substitutionMap(substitutionMap),
        numInstEvaluated(numInstEvaluated) {
    }

    void setValue(SILValue value, SymbolicValue symVal) {
      assert(!value->getType().isAddress() &&
             "addresses are tracked separately");
      calculatedValues.insert({ value, symVal });
    }

    void setAddress(SILValue addr, AddressValue addrVal) {
      assert(addr->getType().isAddress() && "values are tracked separately");
      addressValues.insert({ addr, addrVal });
    }

    AddressValue createMemoryObject(SILValue addr, SymbolicValue initialValue) {
      unsigned objectID = memoryObjects.size();
      auto objectType = simplifyType(addr->getType().getSwiftRValueType());
      memoryObjects.push_back({initialValue, objectType});

      auto result = AddressValue::get(objectID);
      setAddress(addr, result);
      return result;
    }

    /// In failure cases computing addresses, we want to return an address
    /// pointing to a reason address computation failed.
    AddressValue getUnknownAddress(SILValue addr, UnknownReason reason) {
      return createMemoryObject(addr, SymbolicValue::getUnknown(addr, reason));
    }

    /// Return the SymbolicValue for the specified SIL value, lazily computing
    /// it if needed.
    SymbolicValue getConstantValue(SILValue value);

    /// Return the AddressValue for the specified SIL address, lazily computing
    /// it if needed.
    AddressValue getAddressValue(SILValue addr);

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

    AddressValue computeSingleStoreAddressValue(SILValue addr);
    llvm::Optional<SymbolicValue> computeCallResult(ApplyInst *apply);

    llvm::Optional<SymbolicValue>
    computeOpaqueCallResult(ApplyInst *apply, SILFunction *callee);

    SymbolicValue computeLoadResult(SILValue addr);
    llvm::Optional<SymbolicValue> computeFSStore(SymbolicValue storedCst,
                                                 SILValue dest);

    AddressValue computeAddressValue(SILValue addr);
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
    conf->getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext();
  auto linkage = getSILLinkage(getDeclLinkage(decl), NotForDefinition);
  auto *newTable = module.createWitnessTableDeclaration(conf, linkage);
  newTable = module.getSILLoader()->lookupWitnessTable(newTable);

  // Update linkage for witness methods.
  // FIXME: Figure out why witnesses have shared linkage by default.
  for (auto &entry : newTable->getEntries())
    if (entry.getKind() == SILWitnessTable::WitnessKind::Method)
      entry.getMethodWitness().Witness->setLinkage(linkage);
}


SymbolicValue ConstExprFunctionState::computeConstantValue(SILValue value) {
  // If this a trivial constant instruction that we can handle, then fold it
  // immediately.
  if (isa<IntegerLiteralInst>(value) || isa<FloatLiteralInst>(value) ||
      isa<StringLiteralInst>(value))
    return SymbolicValue::getConstantInst(cast<SingleValueInstruction>(value));

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
    if (!val.isConstant()) return val;
    return val.getAggregateValue()[tei->getFieldNo()];
  }

  // If this is a struct extract from a fragile type, then we can return the
  // element being extracted.
  if (auto *sei = dyn_cast<StructExtractInst>(value)) {
    auto val = getConstantValue(sei->getOperand());
    if (!val.isConstant()) return val;
    return val.getAggregateValue()[sei->getFieldNo()];
  }

  // TODO: If this is a single element struct, we can avoid creating an
  // aggregate to reduce # allocations.  This is extra silly in the case of zero
  // element tuples.
  if (isa<StructInst>(value) || isa<TupleInst>(value)) {
    auto inst = cast<SingleValueInstruction>(value);
    SmallVector<SymbolicValue, 4> elts;

    for (unsigned i = 0, e = inst->getNumOperands(); i != e; ++i) {
      auto val = getConstantValue(inst->getOperand(i));
      if (!val.isConstant()) return val;
      elts.push_back(val);
    }

    return SymbolicValue::getAggregate(elts, evaluator.getAllocator());
  }

  // If this is a load, then we either have computed the value of the memory
  // already (when analyzing the body of a constexpr) or this should be a by-ref
  // result of a call.  Either way, we ask for the value of the pointer: in the
  // former case this will be the latest value for this, in the later case, this
  // must be a single-def value for us to analyze it.
  if (auto li = dyn_cast<LoadInst>(value))
    return computeLoadResult(li->getOperand());

  // Try to resolve a witness method against our known conformances.
  if (auto *wmi = dyn_cast<WitnessMethodInst>(value)) {
    auto confResult = substitutionMap.lookupConformance(wmi->getLookupType(),
                         wmi->getConformance().getRequirement());
    if (!confResult)
      return SymbolicValue::getUnknown(value, UnknownReason::Default);
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
      return SymbolicValue::getFunction(fn, conf);

    DEBUG(llvm::dbgs() << "ConstExpr Unresolved witness: " << *value << "\n");
    return SymbolicValue::getUnknown(value, UnknownReason::Default);
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

  DEBUG(llvm::dbgs() << "ConstExpr Unknown simple: " << *value << "\n");

  // Otherwise, we don't know how to handle this.
  return SymbolicValue::getUnknown(value, UnknownReason::Default);
}

SymbolicValue
ConstExprFunctionState::computeConstantValueBuiltin(BuiltinInst *inst) {
  const BuiltinInfo &builtin = inst->getBuiltinInfo();

  // Handle various cases in groups.
  auto unknownResult = [&]() -> SymbolicValue {
    return SymbolicValue::getUnknown(SILValue(inst), UnknownReason::Default);
  };

  // Nullary operations.
  if (inst->getNumOperands() == 0) {
    switch (builtin.ID) {
    default: break;
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
    auto IntCheckedTruncFn = [&](bool srcSigned, bool dstSigned)->SymbolicValue{
      if (operand.getKind() != SymbolicValue::Integer)
        return unknownResult();

      auto operandVal = operand.getIntegerValue();
      uint32_t srcBitWidth = operandVal.getBitWidth();
      auto dstBitWidth =
        builtin.Types[1]->castTo<BuiltinIntegerType>()->getGreatestWidth();

      APInt result = operandVal.trunc(dstBitWidth);

      // Compute the overflow by re-extending the value back to its source and
      // checking for loss of value.
      APInt reextended = dstSigned ? result.sext(srcBitWidth)
                                   : result.zext(srcBitWidth);
      bool overflowed = (operandVal != reextended);

      if (builtin.ID == BuiltinValueKind::UToSCheckedTrunc)
        overflowed |= result.isSignBitSet();

      if (overflowed)
        return SymbolicValue::getUnknown(SILValue(inst),
                                         UnknownReason::Overflow);

      auto &allocator = evaluator.getAllocator();
      // Build the Symbolic value result for our truncated value.
      return SymbolicValue::getAggregate({
          SymbolicValue::getInteger(result, allocator),
          SymbolicValue::getInteger(APInt(1, overflowed), allocator)
      }, allocator);
    };

    switch (builtin.ID) {
    default: break;
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
        default: assert(0 && "Unknown case");
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

    case BuiltinValueKind::PtrToInt: {
      auto resultType = inst->getType().castTo<BuiltinIntegerType>();
      // The only ptrtoint case we handle is from a string value to Word type,
      // which happens in the wrapping up of strings.
      if (operand.getKind() != SymbolicValue::String ||
          !resultType->getWidth().isPointerWidth())
        break;
      return operand;
    }
    }
  }

  // Binary operations.
  if (inst->getNumOperands() == 2) {
    auto operand0 = getConstantValue(inst->getOperand(0));
    auto operand1 = getConstantValue(inst->getOperand(1));
    if (!operand0.isConstant()) return operand0;
    if (!operand1.isConstant()) return operand1;

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

#define REQUIRE_KIND(KIND)                          \
  if (operand0.getKind() != SymbolicValue::KIND ||  \
      operand1.getKind() != SymbolicValue::KIND)    \
      return unknownResult();

    switch (builtin.ID) {
    default: break;
#define INT_BINOP(OPCODE, EXPR)                                            \
    case BuiltinValueKind::OPCODE: {                                       \
      REQUIRE_KIND(Integer)                                                \
      auto l = operand0.getIntegerValue(), r = operand1.getIntegerValue(); \
      return SymbolicValue::getInteger((EXPR), evaluator.getAllocator());  \
    }
    INT_BINOP(Add,  l+r)
    INT_BINOP(And,  l&r)
    INT_BINOP(AShr, l.ashr(r))
    INT_BINOP(LShr, l.lshr(r))
    INT_BINOP(Or,   l|r)
    INT_BINOP(Mul,  l*r)
    INT_BINOP(SDiv, l.sdiv(r))
    INT_BINOP(Shl,  l << r)
    INT_BINOP(SRem, l.srem(r))
    INT_BINOP(Sub,  l-r)
    INT_BINOP(UDiv, l.udiv(r))
    INT_BINOP(URem, l.urem(r))
    INT_BINOP(Xor,  l^r)
#undef INT_BINOP
#define FP_BINOP(OPCODE, EXPR)                                           \
    case BuiltinValueKind::OPCODE: {                                     \
      REQUIRE_KIND(Float)                                                \
      auto l = operand0.getFloatValue(), r = operand1.getFloatValue();   \
      return SymbolicValue::getFloat((EXPR), evaluator.getAllocator());  \
    }
    FP_BINOP(FAdd, l+r)
    FP_BINOP(FSub, l-r)
    FP_BINOP(FMul, l*r)
    FP_BINOP(FDiv, l/r)
    FP_BINOP(FRem, (l.mod(r), l))
#undef FP_BINOP

#define INT_COMPARE(OPCODE, EXPR)                                              \
    case BuiltinValueKind::OPCODE:                                             \
      REQUIRE_KIND(Integer)                                                    \
      return constFoldIntCompare([&](const APInt &l, const APInt &r) -> bool { \
        return (EXPR);                                                         \
      })
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
#define FP_COMPARE(OPCODE, EXPR)                                         \
    case BuiltinValueKind::OPCODE:                                       \
      REQUIRE_KIND(Float)                                                \
      return constFoldFPCompare([&](APFloat::cmpResult result) -> bool { \
        return (EXPR);                                                   \
      })
    FP_COMPARE(FCMP_OEQ, result == APFloat::cmpEqual);
    FP_COMPARE(FCMP_OGT, result == APFloat::cmpGreaterThan);
    FP_COMPARE(FCMP_OGE, result == APFloat::cmpGreaterThan ||
                         result == APFloat::cmpEqual);
    FP_COMPARE(FCMP_OLT, result == APFloat::cmpLessThan);
    FP_COMPARE(FCMP_OLE, result == APFloat::cmpLessThan ||
                         result == APFloat::cmpEqual);
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
    if (!operand0.isConstant()) return operand0;
    if (!operand1.isConstant()) return operand1;
    if (!operand2.isConstant()) return operand2;

    // Overflowing integer operations like sadd_with_overflow take three
    // operands: the last one is a "should report overflow" bit.
    auto constFoldIntOverflow =
      [&](const std::function<APInt(const APInt &, const APInt &, bool &)> &fn)
            -> SymbolicValue {
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
        return SymbolicValue::getUnknown(SILValue(inst),
                                         UnknownReason::Overflow);

      auto &allocator = evaluator.getAllocator();
      // Build the Symbolic value result for our normal and overflow bit.
      return SymbolicValue::getAggregate({
        SymbolicValue::getInteger(result, allocator),
        SymbolicValue::getInteger(APInt(1, overflowed), allocator)
      }, allocator);
    };

    switch (builtin.ID) {
    default: break;

#define INT_OVERFLOW(OPCODE, METHOD)                                   \
    case BuiltinValueKind::OPCODE:                                     \
      return constFoldIntOverflow([&](const APInt &l, const APInt &r,  \
                                      bool &overflowed) -> APInt {     \
        return l.METHOD(r, overflowed);                                \
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
  default: break;
  case llvm::Intrinsic::expect:
    // llvm.expect(x, y) always lowers to x.
    return getConstantValue(inst->getOperand(0));
  }


  DEBUG(llvm::dbgs() << "ConstExpr Unknown Builtin: " << *inst << "\n");

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
    return SymbolicValue::getUnknown((SILInstruction*)apply,
                                     UnknownReason::Trap);
  }
  
  if (classifyFunction(callee->getName()) ==
      WellKnownFunction::AssertionFailure) {
    // TODO: Actually get the message out of fatalError.  We can literally get
    // its string and file/line/col info and propagate it up!
    return SymbolicValue::getUnknown((SILInstruction*)apply,
                                     UnknownReason::Trap);
  }

  DEBUG(llvm::dbgs() << "ConstExpr Opaque Callee: " << *callee << "\n");
  return SymbolicValue::getUnknown((SILInstruction*)apply,
                                   UnknownReason::Default);
}


// TODO: Refactor this to someplace common, this is defined in Devirtualize.cpp.
SubstitutionMap
getWitnessMethodSubstitutions(SILModule &Module, ApplySite AI, SILFunction *F,
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
  if (!calleeFn.isConstant())
    return SymbolicValue::getUnknown((SILInstruction*)apply,
                                     UnknownReason::Default);

  SILFunction *callee;
  Optional<ProtocolConformanceRef> conformance;
  std::tie(callee, conformance) = calleeFn.getFunctionValue();


  // If we reached an external function that hasn't been deserialized yet, make
  // sure to pull it in so we can see its body.  If that fails, then we can't
  // analyze the function.
  if (callee->isExternalDeclaration()) {
    callee->getModule().loadFunction(callee);
    if (callee->isExternalDeclaration())
      return computeOpaqueCallResult(apply, callee);
  }

  // TODO: Verify that the callee was defined as a @constexpr function.

  // If this is a well-known function, do not step into it.
  //
  // FIXME: This should be based on the SILFunction carrying a
  // @constexprSemantics sort of attribute that indicates it is well known,
  // just like the existing SemanticsAttr thing.
  switch (classifyFunction(callee->getName())) {
  default: break;
  case WellKnownFunction::StringInitEmpty: {  // String.init()
    assert(conventions.getNumDirectSILResults() == 1 &&
           conventions.getNumIndirectSILResults() == 0 &&
           "unexpected String.init() signature");
    auto result = SymbolicValue::getString("", evaluator.getAllocator());
    setValue(apply->getResults()[0], result);
    return None;
  }
  }

  // Verify that we can fold all of the arguments to the call.
  SmallVector<SymbolicValue, 4> paramConstants;
  unsigned applyParamBaseIndex = 1+conventions.getNumIndirectSILResults();
  auto paramInfos = conventions.getParameters();
  for (unsigned i = 0, e = paramInfos.size(); i != e; ++i) {
    // If any of the arguments is a non-constant value, then we can't fold this
    // call.
    auto op = apply->getOperand(applyParamBaseIndex+i);
    SymbolicValue argValue;
    if (op->getType().isObject())
      argValue = getConstantValue(op);
    else
      argValue = computeLoadResult(op);

    if (!argValue.isConstant())
      return argValue;

    paramConstants.push_back(argValue);
  }

  // Compute the substitution map for the callee, which maps from all of its
  // generic requirements to concrete conformances and concrete types.
  SubstitutionMap calleeSubMap;

  auto calleeFnType = callee->getLoweredFunctionType();
  if (auto signature = calleeFnType->getGenericSignature()) {
    ApplySite AI(apply);

    // Get the substitution map of the call.  This maps from the callee's space
    // into the caller's world.
    SubstitutionMap callSubMap;
    if (conformance.hasValue()) {
      // Witness methods have special magic that is required to resolve them.
      callSubMap = getWitnessMethodSubstitutions(apply->getModule(), AI, callee,
                                                 conformance.getValue());
    } else {
      auto requirementSig = AI.getOrigCalleeType()->getGenericSignature();
      callSubMap =
        requirementSig->getSubstitutionMap(apply->getSubstitutions());
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
  SmallVector<SymbolicValue, 4> results;
  auto callResult =
    evaluateAndCacheCall(*callee, calleeSubMap, paramConstants, results,
                         numInstEvaluated, evaluator);
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

  // Handle indirect results as well.
  for (unsigned i = 0, e = conventions.getNumIndirectSILResults(); i != e; ++i){
    auto error = computeFSStore(results[nextResult], apply->getOperand(1+i));
    if (error.hasValue())
      return error;
    ++nextResult;
  }

  assert(nextResult == results.size() && "Unexpected number of results found");

  // We have successfully folded this call!
  return None;
}

/// Return the SymbolicValue for the specified SIL value, lazily computing
/// it if needed.
SymbolicValue ConstExprFunctionState::getConstantValue(SILValue value) {
  assert(!value->getType().isAddress() && "Shouldn't be used for addresses");

  // Check to see if we already have an answer.
  auto it = calculatedValues.find(value);
  if (it != calculatedValues.end()) return it->second;

  // Compute the value of a normal instruction based on its operands.
  auto result = computeConstantValue(value);

  // If this is the top-level lazy interpreter, output a debug trace.
  if (!fn) {
    DEBUG(llvm::dbgs() << "ConstExpr top level: "; value->dump());
    DEBUG(llvm::dbgs() << "  RESULT: ";  result.dump());
  }

  return calculatedValues[value] = result;
}




/// Given an aggregate value like {{1, 2}, 3} and an access path like [0,1], and
/// a scalar like 4, return the aggregate value with the indexed element
/// replaced with its specified scalar, producing {{1, 4}, 3} in this case.
///
/// This returns true on failure and false on success.
///
static bool updateIndexedElement(SymbolicValue &aggregate,
                                 ArrayRef<unsigned> indices,
                                 SymbolicValue scalar, Type type,
                                 llvm::BumpPtrAllocator &allocator) {
  // We're done if we've run out of indices.
  if (indices.empty()) {
    aggregate = scalar;
    return false;
  }

  // If we have a non-aggregate then fail.  If we have an uninit memory, then
  // scalarize it into an aggregate to continue.  This happens when memory
  // objects are initialized piecewise.
  if (aggregate.getKind() != SymbolicValue::Aggregate) {
    if (aggregate.getKind() != SymbolicValue::UninitMemory)
      return true;

    unsigned numMembers;
    // We need to have either a struct or a tuple type.
    if (auto *decl = type->getStructOrBoundGenericStruct()) {
      numMembers = std::distance(decl->getStoredProperties().begin(),
                                 decl->getStoredProperties().end());
    } else if (auto tuple = type->getAs<TupleType>()) {
      numMembers = tuple->getNumElements();
    } else {
      return true;
    }

    SmallVector<SymbolicValue, 4> newElts(numMembers,
                                          SymbolicValue::getUninitMemory());
    aggregate = SymbolicValue::getAggregate(newElts, allocator);
  }

  unsigned elementNo = indices.front();
  Type eltType;

  // We need to have either a struct or a tuple type.
  if (auto *decl = type->getStructOrBoundGenericStruct()) {
    auto it = decl->getStoredProperties().begin();
    std::advance(it, elementNo);
    eltType = (*it)->getType();
  } else if (auto tuple = type->getAs<TupleType>()) {
    assert(elementNo < tuple->getNumElements() && "invalid index");
    eltType = tuple->getElement(elementNo).getType();
  } else {
    return true;
  }

  // Update the indexed element of the aggregate.
  auto oldElts = aggregate.getAggregateValue();
  SmallVector<SymbolicValue, 4> newElts(oldElts.begin(), oldElts.end());
  if (updateIndexedElement(newElts[elementNo], indices.drop_front(),
                           scalar, eltType, allocator))
    return true;

  aggregate = SymbolicValue::getAggregate(newElts, allocator);
  return false;
}

/// When analyzing the top-level code involved in a constant expression, we can
/// end up demanding values that are returned by address.  Handle this by
/// finding the temporary stack value that they were stored into and analyzing
/// the single store that should exist into that memory (there are a few forms).
AddressValue
ConstExprFunctionState::computeSingleStoreAddressValue(SILValue addr) {
  // The only value we can otherwise handle is an alloc_stack instruction.
  auto alloc = dyn_cast<AllocStackInst>(addr);
  if (!alloc)
    return getUnknownAddress(addr, UnknownReason::Default);

  // Keep track of the value found for the first constant store.
  unsigned objectID =
    createMemoryObject(alloc, SymbolicValue::getUninitMemory()).getObjectID();

  // Okay, check out all of the users of this value looking for semantic stores
  // into the address.  If we find more than one, then this was a var or
  // something else we can't handle.
  for (auto *use : alloc->getUses()) {
    auto user = use->getUser();

    // Ignore markers, loads, and other things that aren't stores to this stack
    // value.
    if (isa<LoadInst>(user) ||
        isa<DeallocStackInst>(user) ||
        isa<DebugValueAddrInst>(user))
      continue;

    // TODO: BeginAccess/EndAccess.

    // TODO: CopyAddr.

    // If this is a store *to* the memory, analyze the input value.
    if (auto *si = dyn_cast<StoreInst>(user)) {
      if (use->getOperandNumber() == 1) {

        // If we have already found a value for this stack slot then we're done:
        // we don't support multiple assignment.
        if (memoryObjects[objectID].first.getKind()
            != SymbolicValue::UninitMemory)
          return getUnknownAddress(alloc, UnknownReason::Default);

        auto result = getConstantValue(si->getOperand(0));
        memoryObjects[objectID].first = result;
        if (result.isUnknown())
          return AddressValue::get(objectID);
        continue;
      }
    }

    // If this is an apply_inst passing the memory address as an indirect
    // result operand, then we have a call that fills in this result.
    //
    if (auto *apply = dyn_cast<ApplyInst>(user)) {
      auto conventions = apply->getSubstCalleeConv();

      // If this is an out-parameter, it is like a store.  If not, this is an
      // indirect read which is ok.
      unsigned numIndirectResults = conventions.getNumIndirectSILResults();
      unsigned opNum = use->getOperandNumber()-1;
      if (opNum >= numIndirectResults)
        continue;

      // Otherwise this is a write.  If we have already found a value for this
      // stack slot then we're done: we don't support multiple assignment.
      if (memoryObjects[objectID].first.getKind() !=SymbolicValue::UninitMemory)
        return getUnknownAddress(alloc, UnknownReason::Default);

      // The callee needs to be a direct call to a constant expression.
      auto callResult = computeCallResult(apply);

      // If the call failed, we're done.
      if (callResult.hasValue()) {
        memoryObjects[objectID].first = callResult.getValue();
        return AddressValue::get(objectID);
      }

      // computeCallResult will have figured out the result and cached it for
      // us.
      assert(memoryObjects[objectID].first.isConstant() &&
             "Should have found a constant result value");
      continue;
    }


    DEBUG(llvm::dbgs() << "Unknown SingleStore ConstExpr user: "
                       << *user << "\n");

    // If this is some other user that we don't know about, then we should
    // treat it conservatively, because it could store into the address.
    return getUnknownAddress(addr, UnknownReason::Default);
  }

  // If we found a store of a constant, then return that value!
  if (memoryObjects[objectID].first.isConstant())
    return AddressValue::get(objectID);

  // Otherwise, return unknown.
  return getUnknownAddress(addr, UnknownReason::Default);
}


/// Given the operand to a load, resolve it to a constant if possible.
SymbolicValue ConstExprFunctionState::computeLoadResult(SILValue addrVal) {
  auto addr = getAddressValue(addrVal);

  SmallVector<unsigned, 4> accessPath;
  unsigned objectID = addr.getState(accessPath);

  // If this is a derived address, then we are digging into an aggregate
  // value.
  auto objectVal = memoryObjects[objectID].first;

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
  return SymbolicValue::getUnknown(addrVal, UnknownReason::Default);
}

/// Evaluate a flow sensitive store to the specified pointer address.
llvm::Optional<SymbolicValue>
ConstExprFunctionState::computeFSStore(SymbolicValue storedCst, SILValue dest) {
  // Only update existing memory locations that we're tracking.
  auto it = addressValues.find(dest);
  if (it == addressValues.end())
    return SymbolicValue::getUnknown(dest, UnknownReason::Default);

  SmallVector<unsigned, 4> accessPath;
  unsigned objectID = it->second.getState(accessPath);

  // If this is a direct store to tracked memory object, just update it.
  if (accessPath.empty()) {
    memoryObjects[objectID].first = storedCst;
    return None;
  }

  // Otherwise, this is a store to a derived address, update the element of
  // the base value.
  auto objectVal = memoryObjects[objectID].first;
  auto objectType = memoryObjects[objectID].second;

  if (updateIndexedElement(objectVal, accessPath, storedCst, objectType,
                           evaluator.getAllocator()))
    return SymbolicValue::getUnknown(dest, UnknownReason::Default);

  memoryObjects[objectID].first = objectVal;
  return None;
}


AddressValue ConstExprFunctionState::computeAddressValue(SILValue addr) {
  // If this is a struct or tuple element addressor, compute a more derived
  // address.
  if (isa<StructElementAddrInst>(addr) || isa<TupleElementAddrInst>(addr)) {
    auto inst = cast<SingleValueInstruction>(addr);
    auto baseAddr = getAddressValue(inst->getOperand(0));

    SmallVector<unsigned, 4> accessPath;
    unsigned objectID = baseAddr.getState(accessPath);

    // Add our index onto the next of the list.
    unsigned index;
    if (auto sea = dyn_cast<StructElementAddrInst>(inst))
      index = sea->getFieldNo();
    else
      index = cast<TupleElementAddrInst>(inst)->getFieldNo();
    accessPath.push_back(index);
    return AddressValue::get(objectID, accessPath, evaluator.getAllocator());
  }

  // These instructions are markers that return their first operand.
  if (auto *bai = dyn_cast<BeginAccessInst>(addr))
    return getAddressValue(bai->getOperand());

  DEBUG(llvm::dbgs() << "ConstExpr Unknown simple addr: " << *addr << "\n");

  // Otherwise, we don't know how to handle this.
  return getUnknownAddress(addr, UnknownReason::Default);
}


/// Return the AddressValue for the specified SIL address, lazily computing
/// it if needed.
AddressValue ConstExprFunctionState::getAddressValue(SILValue addr) {
  assert(addr->getType().isAddress() && "Should be used for addresses");

  // Check to see if we already have an answer.
  auto it = addressValues.find(addr);
  if (it != addressValues.end())
    return it->second;

  // If the client is asking for the value of a stack object that hasn't been
  // computed, then we are in top level code, and the stack object must be a
  // single store value.  Since this is a very different computation, split it
  // out to its own path.
  if (!fn) {
    AddressValue result = computeSingleStoreAddressValue(addr);
    addressValues.insert({addr, result});
    return result;
  }

  // Compute the value of a normal instruction based on its operands.
  AddressValue result = computeAddressValue(addr);
  addressValues.insert({addr, result});
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
      // Constant have no important state.
      isa<DestroyAddrInst>(inst))
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
    addressValues.erase(inst->getOperand(0));
    return None;
  }

  if (isa<CondFailInst>(inst)) {
    auto failed = getConstantValue(inst->getOperand(0));
    if (failed.getKind() == SymbolicValue::Integer) {
      if (failed.getIntegerValue() == 0)
        return None;
      // Conditional fail actually failed.
      return SymbolicValue::getUnknown(inst, UnknownReason::Trap);
    }
  }

  // If this is a call, evaluate it.
  if (auto apply = dyn_cast<ApplyInst>(inst))
    return computeCallResult(apply);

  if (auto *store = dyn_cast<StoreInst>(inst)) {
    auto stored = getConstantValue(inst->getOperand(0));
    if (!stored.isConstant())
      return stored;

    return computeFSStore(stored, inst->getOperand(1));
  }

  // Copy addr is a load + store combination.
  if (auto *copy = dyn_cast<CopyAddrInst>(inst)) {
    auto value = computeLoadResult(copy->getOperand(0));
    if (!value.isConstant())
      return value;

    return computeFSStore(value, copy->getOperand(1));
  }

  // If the instruction produces normal results, try constant folding it.
  // If this fails, then we fail.
  if (inst->getNumResults() != 0) {
    auto oneResultVal = inst->getResults()[0];
    if (oneResultVal->getType().isObject()) {
      auto result = getConstantValue(oneResultVal);
      if (!result.isConstant())
        return result;
      DEBUG(llvm::dbgs() << "  RESULT: ";  result.dump());
    } else {
      auto result = getAddressValue(oneResultVal);
      // If the address could not be resolved, puts the 'unknown' code into
      // the referenced memory object.
      auto memVal = memoryObjects[result.getObjectID()].first;
      if (memVal.isUnknown())
        return memVal;
      DEBUG(llvm::dbgs() << "  RESULT: ";  result.dump());
    }
    return None;
  }

  DEBUG(llvm::dbgs() << "ConstExpr Unknown FS: " << *inst << "\n");
  // If this is an unknown instruction with no results then bail out.
  return SymbolicValue::getUnknown(inst, UnknownReason::Default);
}


/// Evaluate a call to the specified function as if it were a constant
/// expression, returning None and filling in `results` on success, or
/// returning an 'Unknown' SymbolicValue on failure carrying the error.
///
static llvm::Optional<SymbolicValue>
evaluateAndCacheCall(SILFunction &fn, SubstitutionMap substitutionMap,
                     ArrayRef<SymbolicValue> arguments,
                     SmallVectorImpl<SymbolicValue> &results,
                     unsigned &numInstEvaluated,
                     ConstExprEvaluator &evaluator) {
  assert(!fn.isExternalDeclaration() && "Can't analyze bodyless function");
  ConstExprFunctionState state(evaluator, &fn, substitutionMap,
                               numInstEvaluated);

  // TODO: implement caching.
  // TODO: reject code that is too complex.

  // Set up all of the indirect results and argument values.
  auto conventions = fn.getConventions();
  unsigned nextBBArg = 0;
  const auto &argList = fn.front().getArguments();

  DEBUG(llvm::dbgs().changeColor(raw_ostream::SAVEDCOLOR, /*bold*/true)
        << "\nConstExpr call fn: "
        << Demangle::demangleSymbolAsString(fn.getName());
        llvm::dbgs().resetColor()
        << "\n");

  for (unsigned i = 0, e = conventions.getNumIndirectSILResults(); i != e; ++i)
    state.createMemoryObject(argList[nextBBArg++],
                             SymbolicValue::getUninitMemory());

  for (auto argSymVal : arguments) {
    auto argVal = argList[nextBBArg++];
    if (argVal->getType().isAddress())
      state.createMemoryObject(argVal, argSymVal);
    else
      state.setValue(argVal, argSymVal);
  }

  assert(fn.front().getNumArguments() == nextBBArg &&
         "argument count mismatch");

  // Keep track of which blocks we've already visited.  We don't support loops
  // and this allows us to reject them.
  SmallPtrSet<SILBasicBlock*, 8> visitedBlocks;

  // Keep track of the current "instruction pointer".
  SILBasicBlock::iterator nextInst = fn.front().begin();
  visitedBlocks.insert(&fn.front());

  while (1) {
    SILInstruction *inst = &*nextInst++;
    DEBUG(llvm::dbgs() << "ConstExpr interpret: "; inst->dump());

    // Make sure we haven't exceeded our interpreter iteration cap.
    if (++numInstEvaluated > ConstExprLimit)
      return SymbolicValue::getUnknown(inst,
                                       UnknownReason::TooManyInstructions);

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

      for (unsigned i = 0, e = conventions.getNumIndirectSILResults();
           i != e; ++i) {
        auto result = state.computeLoadResult(argList[i]);
        if (!result.isConstant())
          return result;
        results.push_back(result);
      }

      // TODO: Handle caching of results.

      DEBUG(llvm::dbgs() << "\n");
      return None;
    }

    if (auto *br = dyn_cast<BranchInst>(inst)) {
      auto destBB = br->getDestBB();

      // If we've already visited this block then fail - we have a loop.
      if (!visitedBlocks.insert(destBB).second)
        return SymbolicValue::getUnknown(br, UnknownReason::Loop);

      // Set up basic block arguments.
      for (unsigned i = 0, e = br->getNumArgs(); i != e; ++i) {
        auto argument = state.getConstantValue(br->getArg(i));
        if (!argument.isConstant()) return argument;
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
        return SymbolicValue::getUnknown(cbr, UnknownReason::Loop);

      nextInst = destBB->begin();
      continue;
    }

    DEBUG(llvm::dbgs() << "ConstExpr: Unknown Terminator: " << *inst << "\n");

    // TODO: Enum switches when we support enums?
    return SymbolicValue::getUnknown(inst, UnknownReason::Default);
  }
}

//===----------------------------------------------------------------------===//
// ConstExprEvaluator implementation.
//===----------------------------------------------------------------------===//

ConstExprEvaluator::ConstExprEvaluator(SILModule &m) {
}

ConstExprEvaluator::~ConstExprEvaluator() {
}

/// Analyze the specified values to determine if they are constant values.  This
/// is done in code that is not necessarily itself a constexpr function.  The
/// results are added to the results list which is a parallel structure to the
/// input values.
///
/// TODO: Return information about which callees were found to be
/// constexprs, which would allow the caller to delete dead calls to them
/// that occur after after folding them.
void ConstExprEvaluator::
computeConstantValues(ArrayRef<SILValue> values,
                      SmallVectorImpl<SymbolicValue> &results) {
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
