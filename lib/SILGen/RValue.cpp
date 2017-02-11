//===--- RValue.cpp - Exploded RValue Representation ----------------------===//
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
// A storage structure for holding a destructured rvalue with an optional
// cleanup(s).
// Ownership of the rvalue can be "forwarded" to disable the associated
// cleanup(s).
//
//===----------------------------------------------------------------------===//

#include "Initialization.h"
#include "RValue.h"
#include "swift/SIL/AbstractionPattern.h"
#include "swift/SIL/SILArgument.h"
#include "swift/AST/CanTypeVisitor.h"

using namespace swift;
using namespace Lowering;


static unsigned getTupleSize(CanType t) {
  if (TupleType *tt = dyn_cast<TupleType>(t))
    return tt->getNumElements();
  return 1;
}

static unsigned getRValueSize(AbstractionPattern pattern, CanType formalType) {
  if (pattern.isTuple()) {
    unsigned count = 0;
    auto formalTupleType = cast<TupleType>(formalType);
    for (auto i : indices(formalTupleType.getElementTypes())) {
      count += getRValueSize(pattern.getTupleElementType(i),
                             formalTupleType.getElementType(i));
    }
    return count;
  }

  return 1;
}

/// Return the number of rvalue elements in the given canonical type.
static unsigned getRValueSize(CanType type) {
  if (auto tupleType = dyn_cast<TupleType>(type)) {
    unsigned count = 0;
    for (auto eltType : tupleType.getElementTypes())
      count += getRValueSize(eltType);
    return count;
  }

  return 1;
}

namespace {
class ExplodeTupleValue
  : public CanTypeVisitor<ExplodeTupleValue,
                          /*RetTy=*/ void,
                          /*Args...=*/ ManagedValue>
{
public:
  std::vector<ManagedValue> &values;
  SILGenFunction &gen;
  SILLocation loc;

  ExplodeTupleValue(std::vector<ManagedValue> &values,
                    SILGenFunction &gen, SILLocation loc)
    : values(values), gen(gen), loc(loc)
  {
  }

  void visitType(CanType formalType, ManagedValue v) {
    values.push_back(v);
  }

  void visitTupleType(CanTupleType tupleFormalType, ManagedValue tupleMV) {
    bool isPlusZero = tupleMV.isPlusZeroRValueOrTrivial();
    SILValue tuple = tupleMV.forward(gen);

    for (auto i : indices(tupleFormalType->getElements())) {
      CanType eltFormalType = tupleFormalType.getElementType(i);
      assert(eltFormalType->isMaterializable());

      auto eltTy = tuple->getType().getTupleElementType(i);
      assert(eltTy.isAddress() == tuple->getType().isAddress());
      auto &eltTI = gen.getTypeLowering(eltTy);

      // Project the element.
      SILValue elt;
      if (tuple->getType().isObject()) {
        assert(eltTI.isLoadable());
        elt = gen.B.createTupleExtract(loc, tuple, i, eltTy);
      } else {
        elt = gen.B.createTupleElementAddr(loc, tuple, i, eltTy);

        // RValue has an invariant that loadable values have been
        // loaded.  Except it's not really an invariant, because
        // argument emission likes to lie sometimes.
        if (eltTI.isLoadable()) {
          elt = eltTI.emitLoad(gen.B, loc, elt, LoadOwnershipQualifier::Take);
        }
      }

      // If we're returning a +1 value, emit a cleanup for the member
      // to cover for the cleanup we disabled for the tuple aggregate.
      auto eltMV = isPlusZero ? ManagedValue::forUnmanaged(elt)
                              : gen.emitManagedRValueWithCleanup(elt, eltTI);

      visit(eltFormalType, eltMV);
    }
  }
};

enum class ImplodeKind { Unmanaged, Forward, Copy };

template<ImplodeKind KIND>
class ImplodeLoadableTupleValue
  : public CanTypeVisitor<ImplodeLoadableTupleValue<KIND>,
                          /*RetTy=*/ SILValue,
                          /*Args...=*/ SILLocation>
{
public:
  ArrayRef<ManagedValue> values;
  SILGenFunction &gen;

  static SILValue getValue(SILGenFunction &gen, ManagedValue v, SILLocation l) {
    switch (KIND) {
    case ImplodeKind::Unmanaged:
      return v.getUnmanagedValue();
    case ImplodeKind::Forward:
      return v.forward(gen);
    case ImplodeKind::Copy:
      return v.copyUnmanaged(gen, l).forward(gen);
    }

    llvm_unreachable("Unhandled ImplodeKind in switch.");
  }

  ImplodeLoadableTupleValue(ArrayRef<ManagedValue> values,
                            SILGenFunction &gen)
    : values(values), gen(gen)
  {}

  SILValue visitType(CanType t, SILLocation l) {
    SILValue result = getValue(gen, values[0], l);
    values = values.slice(1);
    return result;
  }

  SILValue visitTupleType(CanTupleType t, SILLocation l) {
    SmallVector<SILValue, 4> elts;
    for (auto fieldTy : t.getElementTypes())
      elts.push_back(this->visit(fieldTy, l));
    SILType ty = gen.getLoweredLoadableType(t);
    return gen.B.createTuple(l, ty, elts);
  }

  ~ImplodeLoadableTupleValue() {
  }
};

template<ImplodeKind KIND>
class ImplodeAddressOnlyTuple
  : public CanTypeVisitor<ImplodeAddressOnlyTuple<KIND>,
                          /*RetTy=*/ void,
                          /*Args...=*/ SILValue, SILLocation>
{
public:
  ArrayRef<ManagedValue> values;
  SILGenFunction &gen;

  ImplodeAddressOnlyTuple(ArrayRef<ManagedValue> values,
                          SILGenFunction &gen)
    : values(values), gen(gen)
  {}

  void visitType(CanType t, SILValue address, SILLocation l) {
    ManagedValue v = values[0];
    switch (KIND) {
    case ImplodeKind::Unmanaged:
      llvm_unreachable("address-only types always managed!");

    case ImplodeKind::Forward:
      v.forwardInto(gen, l, address);
      break;

    case ImplodeKind::Copy:
      v.copyInto(gen, address, l);
      break;
    }
    values = values.slice(1);
  }

  void visitTupleType(CanTupleType t, SILValue address, SILLocation l) {
    for (unsigned n = 0, size = t->getNumElements(); n < size; ++n) {
      CanType fieldCanTy = t.getElementType(n);
      SILType fieldTy = gen.getLoweredType(fieldCanTy);
      SILValue fieldAddr = gen.B.createTupleElementAddr(l,
                                                      address, n,
                                                      fieldTy.getAddressType());
      this->visit(fieldCanTy, fieldAddr, l);
    }
  }

  ~ImplodeAddressOnlyTuple() {
    assert(values.empty() && "values not exhausted imploding tuple?!");
  }
};

template<ImplodeKind KIND>
static SILValue implodeTupleValues(ArrayRef<ManagedValue> values,
                                   SILGenFunction &gen,
                                   CanType tupleType, SILLocation l) {
  // Non-tuples don't need to be imploded.
  if (!isa<TupleType>(tupleType)) {
    assert(values.size() == 1 && "exploded non-tuple value?!");
    return ImplodeLoadableTupleValue<KIND>::getValue(gen, values[0], l);
  }

  SILType loweredType = gen.getLoweredType(tupleType);

  // To implode an address-only tuple, we need to create a buffer to hold the
  // result tuple.
  if (loweredType.isAddressOnly(gen.F.getModule())) {
    assert(KIND != ImplodeKind::Unmanaged &&
           "address-only values are always managed!");
    SILValue buffer = gen.emitTemporaryAllocation(l, loweredType);
    ImplodeAddressOnlyTuple<KIND>(values, gen).visit(tupleType, buffer, l);
    return buffer;
  }

  // To implode loadable tuples, we just need to combine the elements with
  // TupleInsts.
  return ImplodeLoadableTupleValue<KIND>(values, gen).visit(tupleType, l);
}

class EmitBBArguments : public CanTypeVisitor<EmitBBArguments,
                                              /*RetTy*/ RValue>
{
public:
  SILGenFunction &gen;
  SILBasicBlock *parent;
  SILLocation loc;
  bool functionArgs;

  EmitBBArguments(SILGenFunction &gen, SILBasicBlock *parent,
                  SILLocation l, bool functionArgs)
    : gen(gen), parent(parent), loc(l), functionArgs(functionArgs) {}

  RValue visitType(CanType t) {
    SILValue arg;
    if (functionArgs) {
      arg = parent->createFunctionArgument(gen.getLoweredType(t),
                                           loc.getAsASTNode<ValueDecl>());
    } else {
      SILType lty = gen.getLoweredType(t);
      ValueOwnershipKind ownershipkind = lty.isAddress()
                                             ? ValueOwnershipKind::Trivial
                                             : ValueOwnershipKind::Owned;
      arg = parent->createPHIArgument(lty, ownershipkind,
                                      loc.getAsASTNode<ValueDecl>());
    }
    ManagedValue mv = isa<InOutType>(t)
      ? ManagedValue::forLValue(arg)
      : gen.emitManagedRValueWithCleanup(arg);

    // If the value is a (possibly optional) ObjC block passed into the entry
    // point of the function, then copy it so we can treat the value reliably
    // as a heap object. Escape analysis can eliminate this copy if it's
    // unneeded during optimization.
    CanType objectType = t;
    if (auto theObjTy = t.getAnyOptionalObjectType())
      objectType = theObjTy;
    if (functionArgs
        && isa<FunctionType>(objectType)
        && cast<FunctionType>(objectType)->getRepresentation()
              == FunctionType::Representation::Block) {
      SILValue blockCopy = gen.B.createCopyBlock(loc, mv.getValue());
      mv = gen.emitManagedRValueWithCleanup(blockCopy);
    }
    return RValue(gen, loc, t, mv);
  }

  RValue visitTupleType(CanTupleType t) {
    RValue rv{t};

    for (auto fieldType : t.getElementTypes())
      rv.addElement(visit(fieldType));

    return rv;
  }
};

} // end anonymous namespace


/// Perform a copy or init operation from an array of ManagedValue (from an
/// RValue) into an initialization. The RValue will have one scalar ManagedValue
/// for each exploded tuple element in the RValue, so this needs to make the
/// shape of the initialization match the available elements.  This can be done
/// one of two ways:
///
///  1) recursively scalarize down the initialization on demand if the type of
///     the RValue is tuple type and the initialization supports it.
///  2) implode the corresponding values in the RValue to a scalar value of
///     tuple type and process them as a unit.
///
/// We prefer to use approach #1 since it generates better code.
///
template <ImplodeKind KIND>
static void copyOrInitValuesInto(Initialization *init,
                                 ArrayRef<ManagedValue> &values, CanType type,
                                 SILLocation loc, SILGenFunction &gen) {
  static_assert(KIND == ImplodeKind::Forward ||
                KIND == ImplodeKind::Copy, "Not handled by init");
  bool isInit = (KIND == ImplodeKind::Forward);
  
  // If the element has non-tuple type, just serve it up to the initialization.
  auto tupleType = dyn_cast<TupleType>(type);
  if (!tupleType) {
    // We take the first value.
    ManagedValue result = values[0];
    values = values.slice(1);
    init->copyOrInitValueInto(gen, loc, result, isInit);
    init->finishInitialization(gen);
    return;
  }
  
  bool implodeTuple = false;

  if (auto Address = init->getAddressOrNull()) {
    if (isa<GlobalAddrInst>(Address) &&
        gen.getTypeLowering(type).getLoweredType().isTrivial(gen.SGM.M)) {
      // Implode tuples in initialization of globals if they are
      // of trivial types.
      implodeTuple = true;
    }
  }
  
  // If we can satisfy the tuple type by breaking up the aggregate
  // initialization, do so.
  if (!implodeTuple && init->canSplitIntoTupleElements()) {
    SmallVector<InitializationPtr, 4> subInitBuf;
    auto subInits = init->splitIntoTupleElements(gen, loc, type, subInitBuf);
    
    assert(subInits.size() == tupleType->getNumElements() &&
           "initialization does not match tuple?!");
    
    for (unsigned i = 0, e = subInits.size(); i < e; ++i)
      copyOrInitValuesInto<KIND>(subInits[i].get(), values,
                                 tupleType.getElementType(i), loc, gen);

    init->finishInitialization(gen);
    return;
  }
  
  // Otherwise, process this by turning the values corresponding to the tuple
  // into a single value (through an implosion) and then binding that value to
  // our initialization.
  SILValue scalar = implodeTupleValues<KIND>(values, gen, type, loc);
  
  // This will have just used up the first values in the list, pop them off.
  values = values.slice(getRValueSize(type));
  
  init->copyOrInitValueInto(gen, loc, ManagedValue::forUnmanaged(scalar),
                            isInit);
  init->finishInitialization(gen);
}

static unsigned
expectedExplosionSize(CanType type) {
  auto tuple = dyn_cast<TupleType>(type);
  if (!tuple)
    return 1;
  unsigned total = 0;
  for (unsigned i = 0; i < tuple->getNumElements(); ++i) {
    total += expectedExplosionSize(tuple.getElementType(i));
  }
  return total;
}

RValue::RValue(ArrayRef<ManagedValue> values, CanType type)
  : values(values.begin(), values.end()), type(type), elementsToBeAdded(0) {
  
  assert(values.size() == expectedExplosionSize(type)
         && "creating rvalue with wrong number of pre-exploded elements");
  
  if (values.size() == 1 && values[0].isInContext()) {
    values = ArrayRef<ManagedValue>();
    type = CanType();
    elementsToBeAdded = Used;
    return;
  }

}

RValue RValue::withPreExplodedElements(ArrayRef<ManagedValue> values,
                                       CanType type) {
  return RValue(values, type);
}

RValue::RValue(SILGenFunction &gen, SILLocation l, CanType formalType,
               ManagedValue v)
  : type(formalType), elementsToBeAdded(0)
{
  assert(v && "creating r-value with consumed value");

  if (v.isInContext()) {
    type = CanType();
    elementsToBeAdded = Used;
    return;
  }

  ExplodeTupleValue(values, gen, l).visit(formalType, v);
  assert(values.size() == getRValueSize(type));
}

RValue::RValue(SILGenFunction &gen, Expr *expr, ManagedValue v)
  : type(expr->getType()->getCanonicalType()), elementsToBeAdded(0) {

  if (v.isInContext()) {
    type = CanType();
    elementsToBeAdded = Used;
    return;
  }

  assert(v && "creating r-value with consumed value");
  ExplodeTupleValue(values, gen, expr).visit(type, v);
  assert(values.size() == getRValueSize(type));
}

RValue::RValue(CanType type)
  : type(type), elementsToBeAdded(getTupleSize(type)) {
}

RValue::RValue(AbstractionPattern pattern, CanType type)
  : type(type), elementsToBeAdded(getRValueSize(pattern, type)) {
}

void RValue::addElement(RValue &&element) & {
  assert(!element.isUsed() && "adding consumed value to r-value");
  assert(!isComplete() && "rvalue already complete");
  assert(!isUsed() && "rvalue already used");
  --elementsToBeAdded;
  values.insert(values.end(),
                element.values.begin(), element.values.end());
  element.makeUsed();

  assert(!isComplete() || values.size() == getRValueSize(type));
}

void RValue::addElement(SILGenFunction &gen, ManagedValue element,
                        CanType formalType, SILLocation l) & {
  assert(element && "adding consumed value to r-value");
  assert(!isComplete() && "rvalue already complete");
  assert(!isUsed() && "rvalue already used");
  --elementsToBeAdded;

  ExplodeTupleValue(values, gen, l).visit(formalType, element);

  assert(!isComplete() || values.size() == getRValueSize(type));
}

SILValue RValue::forwardAsSingleValue(SILGenFunction &gen, SILLocation l) && {
  assert(isComplete() && "rvalue is not complete");
  SILValue result
    = implodeTupleValues<ImplodeKind::Forward>(values, gen, type, l);

  makeUsed();
  return result;
}

SILValue RValue::forwardAsSingleStorageValue(SILGenFunction &gen,
                                             SILType storageType,
                                             SILLocation l) && {
  assert(isComplete() && "rvalue is not complete");
  SILValue result = std::move(*this).forwardAsSingleValue(gen, l);
  return gen.emitConversionFromSemanticValue(l, result, storageType);
}

void RValue::forwardInto(SILGenFunction &gen, SILLocation loc, 
                         Initialization *I) && {
  assert(isComplete() && "rvalue is not complete");
  ArrayRef<ManagedValue> elts = values;
  copyOrInitValuesInto<ImplodeKind::Forward>(I, elts, type, loc, gen);
}

void RValue::copyInto(SILGenFunction &gen, SILLocation loc,
                      Initialization *I) const & {
  assert(isComplete() && "rvalue is not complete");
  ArrayRef<ManagedValue> elts = values;
  copyOrInitValuesInto<ImplodeKind::Copy>(I, elts, type, loc, gen);
}

static void assignRecursive(SILGenFunction &gen, SILLocation loc,
                            CanType type, ArrayRef<ManagedValue> &srcValues,
                            SILValue destAddr) {
  // Recurse into tuples.
  if (auto srcTupleType = dyn_cast<TupleType>(type)) {
    assert(destAddr->getType().castTo<TupleType>()->getNumElements()
             == srcTupleType->getNumElements());
    for (auto eltIndex : indices(srcTupleType.getElementTypes())) {
      auto eltDestAddr = gen.B.createTupleElementAddr(loc, destAddr, eltIndex);
      assignRecursive(gen, loc, srcTupleType.getElementType(eltIndex),
                      srcValues, eltDestAddr);
    }
    return;
  }

  // Otherwise, pull the front value off the list.
  auto srcValue = srcValues.front();
  srcValues = srcValues.slice(1);

  srcValue.assignInto(gen, loc, destAddr);
}

void RValue::assignInto(SILGenFunction &gen, SILLocation loc,
                        SILValue destAddr) && {
  assert(isComplete() && "rvalue is not complete");
  ArrayRef<ManagedValue> srcValues = values;
  assignRecursive(gen, loc, type, srcValues, destAddr);
  assert(srcValues.empty() && "didn't claim all elements!");
}

ManagedValue RValue::getAsSingleValue(SILGenFunction &gen, SILLocation l) && {
  // Avoid killing and re-emitting the cleanup if the enclosed value isn't a
  // tuple.
  if (!isa<TupleType>(type)) {
    assert(values.size() == 1 && "exploded non-tuple?!");
    ManagedValue result = values[0];
    makeUsed();
    return result;
  }

  // Forward into a single value, then install a cleanup on the resulting
  // imploded value.
  return gen.emitManagedRValueWithCleanup(
                                std::move(*this).forwardAsSingleValue(gen, l));
}

SILValue RValue::getUnmanagedSingleValue(SILGenFunction &gen,
                                         SILLocation l) const & {
  assert(isComplete() && "rvalue is not complete");
  return implodeTupleValues<ImplodeKind::Unmanaged>(values, gen, type, l);
}

void RValue::forwardAll(SILGenFunction &gen,
                        SmallVectorImpl<SILValue> &dest) && {
  assert(isComplete() && "rvalue is not complete");

  for (auto value : values)
    dest.push_back(value.forward(gen));

  makeUsed();
}

void RValue::getAll(SmallVectorImpl<ManagedValue> &dest) && {
  assert(isComplete() && "rvalue is not complete");

  dest.append(values.begin(), values.end());
  makeUsed();
}

void RValue::getAllUnmanaged(SmallVectorImpl<SILValue> &dest) const & {
  assert(isComplete() && "rvalue is not complete");

  for (auto value : values)
    dest.push_back(value.getUnmanagedValue());
}

/// Return the range of indexes for the given tuple type element.
static std::pair<unsigned,unsigned>
getElementRange(CanTupleType tupleType, unsigned eltIndex) {
  assert(eltIndex < tupleType->getNumElements());
  unsigned begin = 0;
  for (unsigned i = 0; i < eltIndex; ++i) {
    begin += getRValueSize(tupleType.getElementType(i));
  }
  unsigned end = begin + getRValueSize(tupleType.getElementType(eltIndex));
  return { begin, end };
}

RValue RValue::extractElement(unsigned n) && {
  assert(isComplete() && "rvalue is not complete");

  CanTupleType tupleTy = dyn_cast<TupleType>(type);
  if (!tupleTy) {
    assert(n == 0);
    unsigned to = getRValueSize(type);
    assert(to == values.size());
    RValue element({llvm::makeArrayRef(values).slice(0, to), type});
    makeUsed();
    return element;
  }

  auto range = getElementRange(tupleTy, n);
  unsigned from = range.first, to = range.second;

  CanType eltType = cast<TupleType>(type).getElementType(n);
  RValue element(llvm::makeArrayRef(values).slice(from, to - from), eltType);
  makeUsed();
  return element;
}

void RValue::extractElements(SmallVectorImpl<RValue> &elements) && {
  assert(isComplete() && "rvalue is not complete");

  CanTupleType tupleTy = dyn_cast<TupleType>(type);
  if (!tupleTy) {
    unsigned to = getRValueSize(type);
    assert(to == values.size());
    elements.push_back({llvm::makeArrayRef(values).slice(0, to), type});
    makeUsed();
    return;
  }

  unsigned from = 0;
  for (auto eltType : tupleTy.getElementTypes()) {
    unsigned to = from + getRValueSize(eltType);
    elements.push_back({llvm::makeArrayRef(values).slice(from, to - from),
                        eltType});
    from = to;
  }
  assert(from == values.size());

  makeUsed();
}

RValue::RValue(const RValue &copied, SILGenFunction &gen, SILLocation l)
  : type(copied.type),
    elementsToBeAdded(copied.elementsToBeAdded)
{
  assert((copied.isComplete() || copied.isUsed())
         && "can't copy incomplete rvalue");
  values.reserve(copied.values.size());
  for (ManagedValue value : copied.values) {
    values.push_back(value.copy(gen, l));
  }
}

ManagedValue RValue::materialize(SILGenFunction &gen, SILLocation loc) && {
  auto &paramTL = gen.getTypeLowering(getType());

  // If we're already materialized, we're done.
  if (values.size() == 1 &&
      values[0].getType() == paramTL.getLoweredType().getAddressType()) {
    auto value = values[0];
    makeUsed();
    return value;
  }

  // Otherwise, emit to a temporary.
  auto temp = gen.emitTemporary(loc, paramTL);
  std::move(*this).forwardInto(gen, loc, temp.get());
  return temp->getManagedAddress();
}
