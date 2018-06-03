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
#include "swift/SIL/SILBuilder.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/Defer.h"
#include "swift/Demangling/Demangle.h"
#include "llvm/Support/TrailingObjects.h"

using namespace swift;
using namespace tf;

namespace {
  /// Lots of APIs here need to return a succeeded/failed status of some sort.
  /// instead of using a bool, we use this enum to make things more explicit.
  enum class ResultCode {
    Success, Fail
  };
} // end anonymous namespace.

//===----------------------------------------------------------------------===//
// SymbolicValue implementation
//===----------------------------------------------------------------------===//

void SymbolicValue::print(llvm::raw_ostream &os, unsigned indent) const {
  os.indent(indent);
  switch (kind) {
  case UninitMemory: os << "uninit\n"; return;
  case Unknown:
    os << "unknown: ";
    value.unknown->dump();
    return;
  case Metatype:
    os << "metatype: ";
    getMetatypeValue()->print(os);
    os << "\n";
    return;
  case Function:
    os << "fn: " << getFunctionValue()->getName() << ": ";
    os << Demangle::demangleSymbolAsString(getFunctionValue()->getName());
    os << "\n";
    return;
  case Inst:
    os << "inst: ";
    value.inst->dump();
    return;
  case Integer:
    os << "int: " << getIntegerValue() << "\n";
    return;
  case Float:
    os << "float: ";
    getFloatValue().print(os);
    os << "\n";
    return;
  case Address: {
    os << "address indices = [";
    interleave(getAddressIndices(), [&](unsigned idx) { os << idx; },
               [&]() { os << ", "; });
    os << "]:  " << getAddressBase();
    return;
  }
  case Aggregate: {
    ArrayRef<SymbolicValue> elements = getAggregateValue();
    os << "agg: " << elements.size() << " element" << "s"[elements.size() == 1]
       << " [\n";
    for (auto elt : elements)
      elt.print(os, indent+2);
    os.indent(indent) << "]\n";
    return;
  }
  }
}

void SymbolicValue::dump() const {
  print(llvm::errs());
}

/// For constant values, return the type classification of this value.
auto SymbolicValue::getTypeKind() const -> TypeKind {
  switch (kind) {
  case UninitMemory:
  case Unknown: assert(0 && "Not a constant value");
  case Metatype: return TKMetatype;
  case Function: return TKFunction;
  case Address: return TKAddress;
  case Aggregate: return TKAggregate;
  case Integer: return TKInteger;
  case Float: return TKFloat;
  case Inst:
    auto *inst = value.inst;
    if (isa<IntegerLiteralInst>(inst))
      return TKInteger;
    if (isa<FloatLiteralInst>(inst))
      return TKFloat;
    assert(isa<StringLiteralInst>(inst) && "Unknown ConstantInst kind");
    return TKString;
  }
}

/// Create and return a new constant literal instruction for the specified
/// scalar constant value.
///
/// TODO: this should eventually go away when we stop using literal instructions
/// and builtin instructions to represent #tfop.  We should switch to a more
/// principled design when we have a custom SIL instruction for graph ops.
SingleValueInstruction *SymbolicValue::
emitConstantInst(SILBuilder &B, SILType type, SILLocation loc) const {
  assert(isConstant() && "Not a constant value");

  switch (getTypeKind()) {
  case SymbolicValue::TKAggregate:
  case SymbolicValue::TKString:
  case SymbolicValue::TKFunction:
  case SymbolicValue::TKAddress:
    // TODO: Unsupported right now.
    return nullptr;

  case SymbolicValue::TKMetatype: {
    auto mt = MetatypeType::get(getMetatypeValue())->getCanonicalType();
    return B.createMetatype(loc, SILType::getPrimitiveObjectType(mt));
  }

  case SymbolicValue::TKInteger:
    return B.createIntegerLiteral(loc, type, getIntegerValue());
  case SymbolicValue::TKFloat:
    return B.createFloatLiteral(loc, type, getFloatValue());
  }
}


//===----------------------------------------------------------------------===//
// Integers
//===----------------------------------------------------------------------===//

namespace swift {
namespace tf {
/// This is a representation of an integer value, stored as a trailing array
/// of words.  Elements of this value are bump pointer allocated.
struct alignas(uint64_t) APIntSymbolicValue final
  : private llvm::TrailingObjects<APIntSymbolicValue, uint64_t> {
    friend class llvm::TrailingObjects<APIntSymbolicValue, uint64_t>;

  /// The number of words in the trailing array and # bits of the value.
  const unsigned numWords, numBits;

  static APIntSymbolicValue *create(unsigned numBits,
                                    ArrayRef<uint64_t> elements,
                                    llvm::BumpPtrAllocator &allocator) {
    auto byteSize =
      APIntSymbolicValue::totalSizeToAlloc<uint64_t>(elements.size());
    auto rawMem = allocator.Allocate(byteSize, alignof(APIntSymbolicValue));

    //  Placement initialize the APIntSymbolicValue.
    auto ilv = ::new (rawMem) APIntSymbolicValue(numBits, elements.size());
    std::uninitialized_copy(elements.begin(), elements.end(),
                            ilv->getTrailingObjects<uint64_t>());
    return ilv;
  }

  APInt getValue() const {
    return APInt(numBits, { getTrailingObjects<uint64_t>(), numWords });
  }

  // This is used by the llvm::TrailingObjects base class.
  size_t numTrailingObjects(OverloadToken<uint64_t>) const {
    return numWords;
  }
private:
  APIntSymbolicValue() = delete;
  APIntSymbolicValue(const APIntSymbolicValue &) = delete;
  APIntSymbolicValue(unsigned numBits, unsigned numWords)
    : numWords(numWords), numBits(numBits) {}
};
} // end namespace tf
} // end namespace swift


SymbolicValue SymbolicValue::getInteger(const APInt &value,
                                        llvm::BumpPtrAllocator &allocator) {
  // TODO: Could store these inline in the union in the common case.
  auto intValue =
    APIntSymbolicValue::create(value.getBitWidth(),
                               { value.getRawData(), value.getNumWords()},
                               allocator);
  assert(intValue && "aggregate value must be present");
  SymbolicValue result;
  result.kind = Integer;
  result.value.integer = intValue;
  return result;
}

APInt SymbolicValue::getIntegerValue() const {
  assert(getTypeKind() == TKInteger);
  if (kind == Integer)
    return value.integer->getValue();

  assert(kind == Inst);
  // TODO: Will eventually support the bump-pointer allocated folded int value.
  return cast<IntegerLiteralInst>(value.inst)->getValue();
}

//===----------------------------------------------------------------------===//
// Floats
//===----------------------------------------------------------------------===//

namespace swift {
namespace tf {
/// This is a representation of an integer value, stored as a trailing array
/// of words.  Elements of this value are bump pointer allocated.
struct alignas(uint64_t) APFloatSymbolicValue final
  : private llvm::TrailingObjects<APFloatSymbolicValue, uint64_t> {
    friend class llvm::TrailingObjects<APFloatSymbolicValue, uint64_t>;

  const llvm::fltSemantics &semantics;

  static APFloatSymbolicValue *create(const llvm::fltSemantics &semantics,
                                     ArrayRef<uint64_t> elements,
                                     llvm::BumpPtrAllocator &allocator) {
    assert((APFloat::getSizeInBits(semantics)+63)/64 == elements.size());

    auto byteSize =
      APFloatSymbolicValue::totalSizeToAlloc<uint64_t>(elements.size());
    auto rawMem = allocator.Allocate(byteSize, alignof(APFloatSymbolicValue));

    //  Placement initialize the APFloatSymbolicValue.
    auto ilv = ::new (rawMem) APFloatSymbolicValue(semantics);
    std::uninitialized_copy(elements.begin(), elements.end(),
                            ilv->getTrailingObjects<uint64_t>());
    return ilv;
  }

  APFloat getValue() const {
    auto val = APInt(APFloat::getSizeInBits(semantics),
                     { getTrailingObjects<uint64_t>(),
                       numTrailingObjects(OverloadToken<uint64_t>())
                     });
    return APFloat(semantics, val);
  }

  // This is used by the llvm::TrailingObjects base class.
  size_t numTrailingObjects(OverloadToken<uint64_t>) const {
    return (APFloat::getSizeInBits(semantics)+63)/64;
  }
private:
  APFloatSymbolicValue() = delete;
  APFloatSymbolicValue(const APFloatSymbolicValue &) = delete;
  APFloatSymbolicValue(const llvm::fltSemantics &semantics)
    : semantics(semantics) {}
};
} // end namespace tf
} // end namespace swift


SymbolicValue SymbolicValue::getFloat(const APFloat &value,
                                      llvm::BumpPtrAllocator &allocator) {
  APInt val = value.bitcastToAPInt();

  // TODO: Could store these inline in the union in the common case.
  auto fpValue =
    APFloatSymbolicValue::create(value.getSemantics(),
                                 { val.getRawData(), val.getNumWords()},
                                 allocator);
  assert(fpValue && "aggregate value must be present");
  SymbolicValue result;
  result.kind = Float;
  result.value.float_ = fpValue;
  return result;
}


APFloat SymbolicValue::getFloatValue() const {
  assert(getTypeKind() == TKFloat);

  if (kind == Float)
    return value.float_->getValue();

  assert(kind == Inst);
  return cast<FloatLiteralInst>(value.inst)->getValue();
}

//===----------------------------------------------------------------------===//
// Addresses
//===----------------------------------------------------------------------===//

namespace swift {
namespace tf {
/// This is a representation of an address value, stored as a base pointer plus
/// trailing array of indices.  Elements of this value are bump pointer
/// allocated.
struct alignas(SILValue) AddressSymbolicValue final
  : private llvm::TrailingObjects<AddressSymbolicValue, unsigned> {
    friend class llvm::TrailingObjects<AddressSymbolicValue, unsigned>;

  /// The number of words in the trailing array and # bits of the value.
  const SILValue base;
  const unsigned numIndices;

  static AddressSymbolicValue *create(SILValue base, ArrayRef<unsigned> indices,
                                      llvm::BumpPtrAllocator &allocator) {
    auto byteSize =
      AddressSymbolicValue::totalSizeToAlloc<unsigned>(indices.size());
    auto rawMem = allocator.Allocate(byteSize, alignof(AddressSymbolicValue));

    //  Placement initialize the AddressSymbolicValue.
    auto alv = ::new (rawMem) AddressSymbolicValue(base, indices.size());
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
  AddressSymbolicValue() = delete;
  AddressSymbolicValue(const AddressSymbolicValue &) = delete;
  AddressSymbolicValue(SILValue base, unsigned numIndices)
    : base(base), numIndices(numIndices) {}
};
} // end namespace tf
} // end namespace swift


SymbolicValue
SymbolicValue::getAddress(SILValue base, ArrayRef<unsigned> indices,
                          llvm::BumpPtrAllocator &allocator) {
  auto alv = AddressSymbolicValue::create(base, indices, allocator);
  assert(alv && "aggregate value must be present");
  SymbolicValue result;
  result.kind = Address;
  result.value.address = alv;
  return result;
}

SILValue SymbolicValue::getAddressBase() const {
  assert(kind == Address);
  return value.address->base;
}

ArrayRef<unsigned> SymbolicValue::getAddressIndices() const {
  assert(kind == Address);
  return value.address->getIndices();
}


//===----------------------------------------------------------------------===//
// Aggregates
//===----------------------------------------------------------------------===//

namespace swift {
namespace tf {
/// This is the representation of a constant aggregate value.  It maintains
/// the elements as a trailing array of SymbolicValue's.  Note that single
/// element structs do not use this (as a performance optimization to reduce
/// allocations).
struct alignas(SymbolicValue) AggregateSymbolicValue final
: private llvm::TrailingObjects<AggregateSymbolicValue, SymbolicValue> {
  friend class llvm::TrailingObjects<AggregateSymbolicValue, SymbolicValue>;

  /// This is the number of elements in the aggregate.
  const unsigned numElements;

  static AggregateSymbolicValue *create(ArrayRef<SymbolicValue> elements,
                                       llvm::BumpPtrAllocator &allocator) {
    auto byteSize =
      AggregateSymbolicValue::totalSizeToAlloc<SymbolicValue>(elements.size());
    auto rawMem = allocator.Allocate(byteSize, alignof(AggregateSymbolicValue));

    //  Placement initialize the AggregateSymbolicValue.
    auto alv = ::new (rawMem) AggregateSymbolicValue(elements.size());
    std::uninitialized_copy(elements.begin(), elements.end(),
                            alv->getTrailingObjects<SymbolicValue>());
    return alv;
  }

  /// Return the element constants for this aggregate constant.  These are
  /// known to all be constants.
  ArrayRef<SymbolicValue> getElements() const {
    return { getTrailingObjects<SymbolicValue>(), numElements };
  }

  // This is used by the llvm::TrailingObjects base class.
  size_t numTrailingObjects(OverloadToken<SymbolicValue>) const {
    return numElements;
  }
private:
  AggregateSymbolicValue() = delete;
  AggregateSymbolicValue(const AggregateSymbolicValue &) = delete;
  AggregateSymbolicValue(unsigned numElements) : numElements(numElements) {}
};
} // end namespace tf
} // end namespace swift


/// This returns a constant Symbolic value with the specified elements in it.
/// This assumes that the elements lifetime has been managed for this.
SymbolicValue SymbolicValue::getAggregate(ArrayRef<SymbolicValue> elements,
                                          llvm::BumpPtrAllocator &allocator) {
  auto aggregate = AggregateSymbolicValue::create(elements, allocator);
  assert(aggregate && "aggregate value must be present");
  SymbolicValue result;
  result.kind = Aggregate;
  result.value.aggregate = aggregate;
  return result;
}

ArrayRef<SymbolicValue> SymbolicValue::getAggregateValue() const {
  assert(getTypeKind() == TKAggregate);
  return value.aggregate->getElements();
}



//===----------------------------------------------------------------------===//
// ConstExprFunctionCache implementation.
//===----------------------------------------------------------------------===//

namespace {
  /// This type represents a cache of computed values within a specific function
  /// as evaluation happens.  A separate instance of this is made for each
  /// callee in a call chain to represent the constant values given the set of
  /// formal parameters that callee was invoked with.
  class ConstExprFunctionCache {
    /// This is the allocator we put temporarly values into.
    ConstExprEvaluator &evaluator;

    /// If we are analyzing the body of a constexpr function, this is the
    /// function.  This is null for the top-level expression.
    SILFunction *fn;

    /// If we have a function being analyzed, this is the substitution list for
    /// the call to it.
    SubstitutionList substitutions;

    /// This is a mapping of substitutions.
    SubstitutionMap substitutionMap;

    /// This is a cache of previously analyzed values, maintained and filled in
    /// by getConstantValue.
    llvm::DenseMap<SILValue, SymbolicValue> calculatedValues;

  public:
    ConstExprFunctionCache(ConstExprEvaluator &evaluator, SILFunction *fn,
                           SubstitutionList substitutions)
      : evaluator(evaluator), fn(fn), substitutions(substitutions) {

      if (fn && !substitutions.empty()) {
        auto signature = fn->getLoweredFunctionType()->getGenericSignature();
        if (signature)
          substitutionMap = signature->getSubstitutionMap(substitutions);
      }
    }


    void setValue(SILValue value, SymbolicValue symVal) {
      calculatedValues.insert({ value, symVal });
    }

    /// Return the Symbolic value for the specified SIL value.
    SymbolicValue getConstantValue(SILValue value);


    /// Evaluate the specified instruction in a flow sensitive way, for use by
    /// the constexpr function evaluator.  This does not handle control flow
    /// statements.  This returns true if the instruction could not be
    /// evaluated.  TODO: Return a more useful error message.
    ResultCode evaluateFlowSensitive(SILInstruction *inst);
  private:
    Type simplifyType(Type ty);
    SymbolicValue computeConstantValue(SILValue value);
    SymbolicValue computeConstantValueBuiltin(BuiltinInst *inst);

    SymbolicValue computeSingleStoreAddressValue(SILValue addr);
    ResultCode computeCallResult(ApplyInst *apply);
  };
} // end anonymous namespace


/// Simplify the specified type based on knowledge of substitutions if we have
/// any.
Type ConstExprFunctionCache::simplifyType(Type ty) {
  return substitutionMap.empty() ? ty : ty.subst(substitutionMap);
}

// TODO: refactor this out somewhere sharable between autodiff and this code.
static SILWitnessTable *
lookupOrLinkWitnessTable(ProtocolConformanceRef confRef,
                         SILModule &module,
                         SerializedSILLoader &silLoader) {
  auto *conf = confRef.getConcrete();
  auto wtable = module.lookUpWitnessTable(conf);
  if (wtable) return wtable;

  auto *decl =
    conf->getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext();
  auto linkage = getSILLinkage(getDeclLinkage(decl), NotForDefinition);
  auto *newTable = module.createWitnessTableDeclaration(conf, linkage);
  newTable = silLoader.lookupWitnessTable(newTable);
  // Update linkage for witness methods.
  // FIXME: Figure out why witnesses have shared linkage by default.
  for (auto &entry : newTable->getEntries())
    if (entry.getKind() == SILWitnessTable::WitnessKind::Method)
      entry.getMethodWitness().Witness->setLinkage(linkage);
  return newTable;
}

SymbolicValue ConstExprFunctionCache::computeConstantValue(SILValue value) {
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
  // aggregate to reduce # allocations.
  // TODO: This is extra silly in the case of zero element tuples.
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

  // If this is a struct or tuple element addressor, compute a more derived
  // address.
  if (isa<StructElementAddrInst>(value) || isa<TupleElementAddrInst>(value)) {
    unsigned index;
    if (auto sea = dyn_cast<StructElementAddrInst>(value))
      index = sea->getFieldNo();
    else
      index = cast<TupleElementAddrInst>(value)->getFieldNo();

    auto inst = cast<SingleValueInstruction>(value);
    SILValue base = inst->getOperand(0);
    auto baseVal = getConstantValue(base);
    SmallVector<unsigned, 4> indices;
    // If the base is an address object, then this is adding indices onto the
    // list.  Otherwise, this is the first reference to some memory value.
    if (baseVal.isAddress()) {
      auto baseIndices = baseVal.getAddressIndices();
      base = baseVal.getAddressBase();
      indices.append(baseIndices.begin(), baseIndices.end());
    }
    indices.push_back(index);
    return SymbolicValue::getAddress(base, indices, evaluator.getAllocator());
  }

  // If this is a load, then we either have computed the value of the memory
  // already (when analyzing the body of a constexpr) or this should be a by-ref
  // result of a call.  Either way, we ask for the value of the pointer: in the
  // former case this will be the latest value for this, in the later case, this
  // must be a single-def value for us to analyze it.
  if (auto li = dyn_cast<LoadInst>(value)) {
    auto result = getConstantValue(li->getOperand());
    // If it is some non-address value, then this is a direct reference to
    // memory.
    if (result.isConstant() && !result.isAddress())
      return result;

    // If this is a derived address, then we are digging into an aggregate
    // value.
    if (result.isAddress()) {
      auto baseVal = getConstantValue(result.getAddressBase());
      auto indices = result.getAddressIndices();
      // Try digging through the aggregate to get to our value.
      while (!indices.empty() && baseVal.isAggregate()) {
        baseVal = baseVal.getAggregateValue()[indices.front()];
        indices = indices.drop_front();
      }

      // If we successfully indexed down to our value, then we're done.
      if (indices.empty())
        return baseVal;
    }

    // When accessing a var in top level code, we want to report the error at
    // the site of the load, not the site of the memory definition.  Remap an
    // unknown result to be the load if present.
    return SymbolicValue::getUnknown(value);
  }

  // Try to resolve a witness method against our known conformances.
  if (auto *wmi = dyn_cast<WitnessMethodInst>(value)) {
    auto confResult = substitutionMap.lookupConformance(wmi->getLookupType(),
                         wmi->getConformance().getRequirement());
    if (!confResult)
      return SymbolicValue::getUnknown(value);
    auto conf = confResult.getValue();
    auto &module = wmi->getModule();

    // Look up the conformance's withness table and the member out of it.
    SILFunction *fn =
      module.lookUpFunctionInWitnessTable(conf, wmi->getMember()).first;
    if (!fn) {
      // If that failed, try force loading it, and try again.
      (void)lookupOrLinkWitnessTable(conf, wmi->getModule(),
                                     evaluator.getSILLoader());
      fn = module.lookUpFunctionInWitnessTable(conf, wmi->getMember()).first;
    }

    // If we were able to resolve it, then we can proceed.
    if (fn)
      return SymbolicValue::getFunction(fn);
  }

  if (auto *builtin = dyn_cast<BuiltinInst>(value))
    return computeConstantValueBuiltin(builtin);

  if (auto *apply = dyn_cast<ApplyInst>(value))
    if (computeCallResult(apply) == ResultCode::Success) {
      assert(calculatedValues.count(apply));
      return calculatedValues[apply];
    }


  DEBUG(llvm::errs() << "ConstExpr Unknown simple: " << *value << "\n");

  // Otherwise, we don't know how to handle this.
  return SymbolicValue::getUnknown(value);
}

SymbolicValue
ConstExprFunctionCache::computeConstantValueBuiltin(BuiltinInst *inst) {
  const BuiltinInfo &builtin = inst->getBuiltinInfo();

  // Handle various cases in groups.

  // Unary operations first.
  if (inst->getNumOperands() == 1) {
    auto operand = getConstantValue(inst->getOperand(0));
    if (!operand.isConstant())
      return SymbolicValue::getUnknown(SILValue(inst));

    // TODO: SUCheckedConversion/USCheckedConversion

    // Implement support for s_to_s_checked_trunc_Int2048_Int64 and other
    // checking integer truncates.  These produce a tuple of the result value
    // and an overflow bit.
    //
    // TODO: We can/should diagnose statically detectable integer overflow
    // errors and subsume the ConstantFolding.cpp mandatory SIL pass.
    auto IntCheckedTruncFn = [&](bool srcSigned, bool dstSigned)->SymbolicValue{
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
    }
  }

  // Binary operations.
  if (inst->getNumOperands() == 2) {
    auto operand0 = getConstantValue(inst->getOperand(0));
    auto operand1 = getConstantValue(inst->getOperand(1));
    if (!operand0.isConstant() || !operand1.isConstant())
      return SymbolicValue::getUnknown(SILValue(inst));

    auto constFoldIntCompare =
      [&](const std::function<bool(const APInt &, const APInt &)> &fn)
        -> SymbolicValue {
      auto result = fn(operand0.getIntegerValue(), operand1.getIntegerValue());
      return SymbolicValue::getInteger(APInt(1, result),
                                       evaluator.getAllocator());
    };
    auto constFoldFPCompare =
      [&](const std::function<bool(APFloat::cmpResult result)> &fn)
        -> SymbolicValue {
      auto comparison =
          operand0.getFloatValue().compare(operand1.getFloatValue());
      return SymbolicValue::getInteger(APInt(1, fn(comparison)),
                                       evaluator.getAllocator());
    };

    switch (builtin.ID) {
    default: break;
#define INT_BINOP(OPCODE, EXPR)                                            \
    case BuiltinValueKind::OPCODE: {                                       \
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
    }
  }


  // Three operand builtins.
  if (inst->getNumOperands() == 3) {
    auto operand0 = getConstantValue(inst->getOperand(0));
    auto operand1 = getConstantValue(inst->getOperand(1));
    auto operand2 = getConstantValue(inst->getOperand(2));
    if (!operand0.isConstant() || !operand1.isConstant() ||
        !operand2.isConstant())
      return SymbolicValue::getUnknown(SILValue(inst));

    // Overflowing integer operations like sadd_with_overflow take three
    // operands: the last one is a "should report overflow" bit.
    auto constFoldIntOverflow =
      [&](const std::function<APInt(const APInt &, const APInt &, bool &)> &fn)
            -> SymbolicValue {
      // TODO: We can/should diagnose statically detectable integer overflow
      // errors and subsume the ConstantFolding.cpp mandatory SIL pass.
      auto l = operand0.getIntegerValue(), r = operand1.getIntegerValue();
      bool overflowed = false;
      auto result = fn(l, r, overflowed);
      auto &allocator = evaluator.getAllocator();
      // Build the Symbolic value result for our truncated value.
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

  DEBUG(llvm::errs() << "ConstExpr Unknown Builtin: " << *inst << "\n");

  // Otherwise, we don't know how to handle this builtin.
  return SymbolicValue::getUnknown(SILValue(inst));
}


/// Given a call to a function, determine whether it is a call to a constexpr
/// function.  If so, collect its arguments as constants, fold it and return
/// false.  If not, return true and mark the results as unknown.
/// 
ResultCode ConstExprFunctionCache::computeCallResult(ApplyInst *apply) {
  auto conventions = apply->getSubstCalleeConv();

  // There are many failure paths through this code, so we write the failure
  // code once up front.  In the failure case, we associate the normal result
  // and any indirect results of the call with unknown.
  bool isConstantCall = false;

  SWIFT_DEFER {
    // On success, don't do anything!
    if (isConstantCall) return;

    // Otherwise, remember that this call produced unknown as well as any
    // indirect results.
    auto unknown = SymbolicValue::getUnknown((SILInstruction*)apply);
    calculatedValues[apply] = unknown;

    for (unsigned i = 0, e = conventions.getNumIndirectSILResults();
         i != e; ++i) {
      auto resultOperand = apply->getOperand(i+1);
      assert(resultOperand->getType().isAddress() &&
             "Indirect results should be by-address");
      calculatedValues[resultOperand] = unknown;
    }
  };

  // Determine the callee.
  auto calleeLV = getConstantValue(apply->getOperand(0));
  if (!calleeLV.isFunction())
    return ResultCode::Fail;

  SILFunction *callee = calleeLV.getFunctionValue();

  // If we reached an external function that hasn't been deserialized yet, make
  // sure to pull it in so we can see its body.  If that fails, then we can't
  // analyze the function.
  if (callee->isExternalDeclaration()) {
    callee = evaluator.getSILLoader().lookupSILFunction(callee);
    if (!callee || callee->isExternalDeclaration()) {
      DEBUG(llvm::errs() << "ConstExpr Opaque Callee: "
                         << *calleeLV.getFunctionValue() << "\n");
      return ResultCode::Fail;
    }
  }

  // TODO: Verify that the callee was defined as a @constexpr function.

  // Verify that we can fold all of the arguments to the call.
  SmallVector<SymbolicValue, 4> paramConstants;
  unsigned applyParamBaseIndex = 1+conventions.getNumIndirectSILResults();
  auto paramInfos = conventions.getParameters();
  for (unsigned i = 0, e = paramInfos.size(); i != e; ++i) {
    // If any of the arguments is a non-constant value, then we can't fold this
    // call.
    auto cst = getConstantValue(apply->getOperand(applyParamBaseIndex+i));
    if (!cst.isConstant())
      return ResultCode::Fail;

    paramConstants.push_back(cst);
  }

  // Now that have successfully folded all of the parameters, we can evaluate
  // the call.
  SmallVector<SymbolicValue, 4> results;
  if (evaluator.evaluateAndCacheCall(*callee, apply->getSubstitutions(),
                                     paramConstants, results))
    return ResultCode::Fail;

  unsigned nextResult = 0;

  // If evaluation was successful, remember the results we captured in our
  // current function's cache.
  if (unsigned numNormalResults = conventions.getNumDirectSILResults()) {
    // TODO: unclear when this happens, is this for tuple result values?
    assert(numNormalResults == 1 && "Multiple results aren't supported?");
    calculatedValues[apply->getResults()[0]] = results[nextResult];
    ++nextResult;
  }

  // Handle indirect results as well.
  for (unsigned i = 0, e = conventions.getNumIndirectSILResults(); i != e; ++i){
    calculatedValues[apply->getOperand(1+i)] = results[nextResult];
    ++nextResult;
  }

  assert(nextResult == results.size() && "Unexpected number of results found");

  // We have successfully folded this call!  Disable the SWIFT_DEFER up top.
  isConstantCall = true;
  return ResultCode::Success;
}

/// When analyzing the top-level code involved in a constant expression, we can
/// end up demanding values that are returned by address.  Handle this by
/// finding the temporary stack value that they were stored into and analyzing
/// the single store that should exist into that memory (there are a few forms).
SymbolicValue
ConstExprFunctionCache::computeSingleStoreAddressValue(SILValue addr) {
  // The only value we can otherwise handle is an alloc_stack instruction.
  auto alloc = dyn_cast<AllocStackInst>(addr);
  if (!alloc) return SymbolicValue::getUnknown(addr);

  // Keep track of the value found for the first constant store.
  SymbolicValue result = SymbolicValue::getUninitMemory();

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

#if 0
    // If this is a store *to* the memory, analyze the input value.
    if (auto *si = dyn_cast<StoreInst>(user)) {
      if (use->getOperandNumber() == 1) {
      TODO: implement;
        continue;
      }
    }
#endif
    // TODO: CopyAddr.

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
      // stack slot then we're done - we don't support multiple assignment.
      if (!result.isUninitMemory())
        return SymbolicValue::getUnknown(addr);

      // The callee needs to be a direct call to a constant expression.
      assert(!calculatedValues.count(addr) &&
             "Shouldn't already have an entry");
      computeCallResult(apply);

      // computeCallResult will have figured out the result and cached it for
      // us.
      assert(calculatedValues.count(addr) &&
             "Should have found a result value");
      result = calculatedValues[addr];

      // If it wasn't a constant, then we're done.
      if (!result.isConstant())
        return result;

      continue;
    }


    DEBUG(llvm::errs() << "Unknown SingleStore ConstExpr user: "
                       << *user << "\n");

    // If this is some other user that we don't know about, then we should
    // treat it conservatively, because it could store into the address.
    return SymbolicValue::getUnknown(addr);
  }

  // If we found a store of a constant, then return that value!
  if (result.isConstant())
    return result;

  // Otherwise, return unknown.
  return SymbolicValue::getUnknown(addr);
}


/// Return the symbolic value for the specified SIL value.
SymbolicValue ConstExprFunctionCache::getConstantValue(SILValue value) {
  // Check to see if we already have an answer.
  auto it = calculatedValues.find(value);
  if (it != calculatedValues.end()) return it->second;

  // If the client is asking for the value of a stack object that hasn't been
  // computed, then we are in top level code, and the stack object must be a
  // single store value.  Since this is a very different computation, split it
  // out to its own path.
  if (value->getType().isAddress() && !fn) {
    auto result = computeSingleStoreAddressValue(value);
    return calculatedValues[value] = result;
  }

  // Compute the value of a normal instruction based on its operands.
  auto result = computeConstantValue(value);
  return calculatedValues[value] = result;
}

/// Given an aggregate value like {{1, 2}, 3} and an access path like [0,1], and
/// a scalar like 4, return the aggregate value with the indexed element
/// replaced with its specified scalar, producing {{1, 4}, 3} in this case.
///
/// This returns true on failures and false on success.
///
static ResultCode updateIndexedElement(SymbolicValue &aggregate,
                                       ArrayRef<unsigned> indices,
                                       SymbolicValue scalar,
                                       llvm::BumpPtrAllocator &allocator) {
  // We're done if we've run out of indices.
  if (indices.empty())
    return ResultCode::Success;

  // TODO: We should handle updates into uninit memory as well.  TODO: we need
  // to know something about its shape/type to do that because we need to turn
  // it into an aggregate.  Maybe uninit should only be for scalar values?

  if (!aggregate.isAggregate())
    return ResultCode::Fail;

  // Update the indexed element of the aggregate.
  auto oldElts = aggregate.getAggregateValue();
  SmallVector<SymbolicValue, 4> newElts(oldElts.begin(), oldElts.end());
  if (updateIndexedElement(newElts[indices.front()], indices.drop_front(),
                           scalar, allocator) == ResultCode::Fail)
    return ResultCode::Fail;

  aggregate = SymbolicValue::getAggregate(newElts, allocator);
  return ResultCode::Success;
}

/// Evaluate the specified instruction in a flow sensitive way, for use by
/// the constexpr function evaluator.  This does not handle control flow
/// statements.  This returns true if the instruction could not be
/// evaluated.  TODO: Return a more useful error message.
ResultCode ConstExprFunctionCache::evaluateFlowSensitive(SILInstruction *inst) {
  if (isa<DebugValueInst>(inst))
    return ResultCode::Success;

  // If this is a special flow-sensitive instruction like a stack allocation,
  // store, copy_addr, etc, we handle it specially here.
  if (auto asi = dyn_cast<AllocStackInst>(inst)) {
    calculatedValues[asi] = SymbolicValue::getUninitMemory();
    return ResultCode::Success;
  }

  // If this is a deallocation of a memory object that we may be tracking,
  // remove the memory from the set.  We don't *have* to do this, but it seems
  // useful for hygiene.
  if (isa<DeallocStackInst>(inst)) {
    calculatedValues.erase(inst->getOperand(0));
    return ResultCode::Success;
  }

  if (isa<CondFailInst>(inst)) {
    auto failed = getConstantValue(inst->getOperand(0));
    // TODO: Emit a diagnostic if this cond_fail actually fails under constant
    // folding.
    if (!failed.isConstant() || failed.getIntegerValue() != 0)
      return ResultCode::Fail;

    return ResultCode::Success;
  }

  // If this is a call, evaluate it.
  if (auto apply = dyn_cast<ApplyInst>(inst))
    return computeCallResult(apply);

  if (auto *store = dyn_cast<StoreInst>(inst)) {
    auto stored = getConstantValue(inst->getOperand(0));
    if (!stored.isConstant()) return ResultCode::Fail;

    // Only update existing memory locations that we're tracking.
    auto it = calculatedValues.find(inst->getOperand(1));
    if (it == calculatedValues.end()) return ResultCode::Fail;

    // If this is a store to an address, update the element of the base value.
    if (it->second.isAddress()) {
      auto baseVal = getConstantValue(it->second.getAddressBase());
      auto indices = it->second.getAddressIndices();

      if (updateIndexedElement(baseVal, indices, stored,
                               evaluator.getAllocator()) == ResultCode::Fail)
        return ResultCode::Fail;
      stored = baseVal;
    }

    it->second = stored;
    return ResultCode::Success;
  }

  // If the instruction produces normal results, try constant folding it.
  // If this fails, then we fail.
  if (inst->getNumResults() != 0) {
    auto result = getConstantValue(inst->getResults()[0]);
    return result.isConstant() ? ResultCode::Success : ResultCode::Fail;
  }

  DEBUG(llvm::errs() << "ConstExpr Unknown FS: " << *inst << "\n");
  // If this is an unknown instruction with no results then bail out.
  return ResultCode::Fail;
}

//===----------------------------------------------------------------------===//
// ConstExprEvaluator implementation.
//===----------------------------------------------------------------------===//

ConstExprEvaluator::ConstExprEvaluator(SILModule &m)
  : silLoader(SerializedSILLoader::create(m.getASTContext(), &m, nullptr)) {
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
  ConstExprFunctionCache cache(*this, nullptr, {});
  for (auto v : values) {
    auto symVal = cache.getConstantValue(v);
    results.push_back(symVal);
  }
}

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
bool ConstExprEvaluator::
evaluateAndCacheCall(SILFunction &fn, SubstitutionList substitutions,
                     ArrayRef<SymbolicValue> arguments,
                     SmallVectorImpl<SymbolicValue> &results) {
  ConstExprFunctionCache cache(*this, &fn, substitutions);

  // If this function has no SIL code available, we can't analyze it.
  if (fn.isExternalDeclaration())
    return true;

  // TODO: implement caching.
  // TODO: reject code that is too complex.

  // Set up all of the indirect results and argument values.
  auto conventions = fn.getConventions();
  unsigned nextBBArg = 0;
  const auto &argList = fn.front().getArguments();
  for (unsigned i = 0, e = conventions.getNumIndirectSILResults(); i != e; ++i){
    cache.setValue(argList[nextBBArg++], SymbolicValue::getUninitMemory());
  }

  for (auto argument : arguments) {
    cache.setValue(argList[nextBBArg++], argument);
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

    // If we can evaluate this flow sensitively, then keep going.
    if (!isa<TermInst>(inst)) {
      if (cache.evaluateFlowSensitive(inst) == ResultCode::Fail)
        return true;
      continue;
    }

    // Otherwise, we handle terminators here.
    if (isa<ReturnInst>(inst)) {
      auto val = cache.getConstantValue(inst->getOperand(0));
      if (!val.isConstant()) return true;

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
        auto result = cache.getConstantValue(argList[i]);
        if (!result.isConstant())
          return true;
        results.push_back(result);
      }

      // TODO: Handle caching of results.
      return false;
    }


    if (auto *br = dyn_cast<BranchInst>(inst)) {
      auto destBB = br->getDestBB();

      // If we've already visited this block then fail - we have a loop.
      if (!visitedBlocks.insert(destBB).second)
        return true;

      // Set up basic block arguments.
      for (unsigned i = 0, e = br->getNumArgs(); i != e; ++i) {
        auto argument = cache.getConstantValue(destBB->getArgument(i));
        if (!argument.isConstant()) return true;
        cache.setValue(br->getArg(i), argument);
      }
      // Set the instruction pointer to the first instruction of the block.
      nextInst = destBB->begin();
      continue;
    }

    if (auto *cbr = dyn_cast<CondBranchInst>(inst)) {
      auto val = cache.getConstantValue(inst->getOperand(0));
      if (!val.isConstant()) return true;

      SILBasicBlock *destBB;
      if (!val.getIntegerValue())
        destBB = cbr->getFalseBB();
      else
        destBB = cbr->getTrueBB();

      // If we've already visited this block then fail - we have a loop.
      if (!visitedBlocks.insert(destBB).second)
        return true;

      nextInst = destBB->begin();
      continue;
    }

    DEBUG(llvm::errs() << "ConstExpr: Unknown Terminator: " << *inst << "\n");

    // TODO: Enum switches when we support enums?
    return true;
  }
}

