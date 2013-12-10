//===--- RValue.cpp - Exploded RValue Representation ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "SILGen.h"
#include "RValue.h"
#include "swift/SIL/SILArgument.h"
#include "swift/AST/CanTypeVisitor.h"
#include <deque>

using namespace swift;
using namespace Lowering;

namespace {

static unsigned getTupleSize(CanType t) {
  if (TupleType *tt = dyn_cast<TupleType>(t))
    return tt->getNumElements();
  return 1;
}

class ExplodeTupleValue
  : public CanTypeVisitor<ExplodeTupleValue,
                          /*RetTy=*/ void,
                          /*Args...=*/ ManagedValue, SILLocation>
{
public:
  std::vector<ManagedValue> &values;
  SILGenFunction &gen;
  
  ExplodeTupleValue(std::vector<ManagedValue> &values,
                    SILGenFunction &gen)
    : values(values), gen(gen)
  {
  }
  
  void visitType(CanType t, ManagedValue v, SILLocation l) {
    values.push_back(v);
  }
  
  void visitTupleType(CanTupleType t, ManagedValue mv, SILLocation l) {
    SILValue v = mv.forward(gen);
    if (v.getType().isAddressOnly(gen.F.getModule())) {
      // Destructure address-only types by addressing the individual members.
      for (unsigned i = 0, size = t->getNumElements(); i < size; ++i) {
        CanType fieldCanTy = t.getElementType(i);
        auto &fieldTI = gen.getTypeLowering(fieldCanTy);
        SILType fieldTy = fieldTI.getLoweredType();
        SILValue member = gen.B.createTupleElementAddr(l, v, i,
                                                     fieldTy.getAddressType());
        assert(fieldTI.getSemanticType() == fieldTy);
        if (fieldTI.isLoadable() && !isa<LValueType>(fieldCanTy))
          member = gen.B.createLoad(l, member);
        visit(fieldCanTy, gen.emitManagedRValueWithCleanup(member, fieldTI), l);
      }
    } else {
      // Extract the elements from loadable tuples.
      for (unsigned i = 0, size = t->getNumElements(); i < size; ++i) {
        CanType fieldCanTy = t.getElementType(i);
        auto &fieldTI = gen.getTypeLowering(fieldCanTy);
        assert(fieldTI.isLoadable());
        SILValue member = gen.B.createTupleExtract(l, v, i,
                                                   fieldTI.getLoweredType());
        visit(fieldCanTy, gen.emitManagedRValueWithCleanup(member, fieldTI), l);
      }
    }
  }
};

enum class ImplodeKind { Unmanaged, Forward };

template<ImplodeKind KIND>
class ImplodeLoadableTupleValue
  : public CanTypeVisitor<ImplodeLoadableTupleValue<KIND>,
                          /*RetTy=*/ SILValue,
                          /*Args...=*/ SILLocation>
{
public:
  ArrayRef<ManagedValue> values;
  SILGenFunction &gen;

  static SILValue getValue(SILGenFunction &gen, ManagedValue v) {
    switch (KIND) {
    case ImplodeKind::Unmanaged:
      return v.getUnmanagedValue();
    case ImplodeKind::Forward:
      return v.forward(gen);
    }
  }
  
  ImplodeLoadableTupleValue(ArrayRef<ManagedValue> values,
                            SILGenFunction &gen)
    : values(values), gen(gen)
  {}
  
  SILValue visitType(CanType t, SILLocation l) {
    SILValue result = getValue(gen, values[0]);
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
    assert(values.empty() && "values not exhausted imploding tuple?!");
  }
};
  
class ImplodeAddressOnlyTuple
  : public CanTypeVisitor<ImplodeAddressOnlyTuple,
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
    v.forwardInto(gen, l, address);
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
    return ImplodeLoadableTupleValue<KIND>::getValue(gen, values[0]);
  }
  
  SILType loweredType = gen.getLoweredType(tupleType);
  
  // To implode an address-only tuple, we need to create a buffer to hold the
  // result tuple.
  if (loweredType.isAddressOnly(gen.F.getModule())) {
    assert(KIND != ImplodeKind::Unmanaged &&
           "address-only values are always managed!");
    SILValue buffer = gen.emitTemporaryAllocation(l, loweredType);
    ImplodeAddressOnlyTuple(values, gen).visit(tupleType, buffer, l);
    return buffer;
  }
  
  // To implode loadable tuples, we just need to combine the elements with
  // TupleInsts.
  return ImplodeLoadableTupleValue<KIND>(values, gen).visit(tupleType, l);
}
  
class InitializeTupleValues
  : public CanTypeVisitor<InitializeTupleValues,
                          /*RetTy=*/ void,
                          /*Args...=*/ Initialization*>
{
public:
  ArrayRef<ManagedValue> values;
  SILGenFunction &gen;
  SILLocation loc;
  
  InitializeTupleValues(ArrayRef<ManagedValue> values, SILGenFunction &gen,
                        SILLocation l)
    : values(values), gen(gen), loc(l)
  {}
  
  void visitType(CanType t, Initialization *I) {
    // Pop a result off.
    ManagedValue result = values[0];
    values = values.slice(1);

    switch (I->kind) {
    case Initialization::Kind::AddressBinding:
      llvm_unreachable("cannot emit into a inout binding");
    case Initialization::Kind::Tuple:
      llvm_unreachable("tuple initialization not destructured?!");

    case Initialization::Kind::Ignored:
      // Throw out the value without storing it.
      return;

    case Initialization::Kind::Translating:
      I->translateValue(gen, loc, result);
      I->finishInitialization(gen);
      return;
        
    case Initialization::Kind::SingleBuffer: {
      // If we didn't evaluate into the initialization buffer, do so now.
      if (result.getValue() != I->getAddress()) {
        result.forwardInto(gen, loc, I->getAddress());
      } else {
        // If we did evaluate into the initialization buffer, disable the
        // cleanup.
        result.forwardCleanup(gen);
      }
      
      I->finishInitialization(gen);
      return;
    }
    }
  }
  
  void visitTupleType(CanTupleType t, Initialization *I) {
    // Break up the aggregate initialization.
    SmallVector<InitializationPtr, 4> subInitBuf;
    auto subInits = I->getSubInitializationsForTuple(gen, t, subInitBuf, loc);
    
    assert(subInits.size() == t->getNumElements() &&
           "initialization does not match tuple?!");
    
    for (unsigned i = 0, e = subInits.size(); i < e; ++i)
      visit(t.getElementType(i), subInits[i].get());
  }
};
  
class EmitBBArguments : public CanTypeVisitor<EmitBBArguments,
                                              /*RetTy*/ RValue>
{
public:
  SILGenFunction &gen;
  SILBasicBlock *parent;
  SILLocation loc;
  
  EmitBBArguments(SILGenFunction &gen, SILBasicBlock *parent,
                  SILLocation l)
    : gen(gen), parent(parent), loc(l) {}
  
  RValue visitType(CanType t) {
    SILValue arg = new (gen.SGM.M)
      SILArgument(gen.getLoweredType(t), parent, loc.getAsASTNode<ValueDecl>());
    ManagedValue mv = isa<LValueType>(t)
      ? ManagedValue(arg, ManagedValue::LValue)
      : gen.emitManagedRValueWithCleanup(arg);
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

RValue::RValue(ArrayRef<ManagedValue> values, CanType type)
  : values(values.begin(), values.end()), type(type), elementsToBeAdded(0)
{
}

RValue::RValue(SILGenFunction &gen, SILLocation l, CanType formalType,
               ManagedValue v)
  : type(formalType), elementsToBeAdded(0)
{
  ExplodeTupleValue(values, gen).visit(type, v, l);
  assert(values.size() == getRValueSize(type));
}

RValue::RValue(SILGenFunction &gen, Expr *expr, ManagedValue v)
  : type(expr->getType()->getCanonicalType()), elementsToBeAdded(0)
{
  ExplodeTupleValue(values, gen).visit(type, v, expr);
  assert(values.size() == getRValueSize(type));
}

RValue::RValue(CanType type)
  : type(type), elementsToBeAdded(getTupleSize(type)) {
}

RValue RValue::emitBBArguments(CanType type,
                               SILGenFunction &gen,
                               SILBasicBlock *parent,
                               SILLocation l) {
  return EmitBBArguments(gen, parent, l).visit(type);
}

void RValue::addElement(RValue &&element) & {
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
  assert(!isComplete() && "rvalue already complete");
  assert(!isUsed() && "rvalue already used");
  --elementsToBeAdded;

  ExplodeTupleValue(values, gen).visit(formalType, element, l);

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

void RValue::forwardInto(SILGenFunction &gen, Initialization *I,
                         SILLocation loc) && {
  assert(isComplete() && "rvalue is not complete");
  InitializeTupleValues(values, gen, loc).visit(type, I);
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

  auto tupleTy = cast<TupleType>(type);
  auto range = getElementRange(tupleTy, n);
  unsigned from = range.first, to = range.second;

  CanType eltType = cast<TupleType>(type).getElementType(n);  
  RValue element(llvm::makeArrayRef(values).slice(from, to - from), eltType);
  makeUsed();
  return element;
}

void RValue::extractElements(SmallVectorImpl<RValue> &elements) && {
  assert(isComplete() && "rvalue is not complete");

  unsigned from = 0;  
  for (auto eltType : cast<TupleType>(type).getElementTypes()) {
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

void ManagedValue::forwardInto(SILGenFunction &gen, SILLocation loc,
                               SILValue address) {
  if (hasCleanup())
    forwardCleanup(gen);
  auto &addrTL = gen.getTypeLowering(address.getType());
  gen.emitSemanticStore(loc, getValue(), address, addrTL, IsInitialization);
}

void ManagedValue::assignInto(SILGenFunction &gen, SILLocation loc,
                              SILValue address) {
  if (hasCleanup())
    forwardCleanup(gen);

  auto &addrTL = gen.getTypeLowering(address.getType());
  gen.emitSemanticStore(loc, getValue(), address, addrTL,
                        IsNotInitialization);
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
  std::move(*this).forwardInto(gen, temp.get(), loc);
  return temp->getManagedAddress();
}

RValue &RValueSource::forceAndPeekRValue(SILGenFunction &gen) & {
  if (isRValue()) {
    return Storage.TheRV.Value;
  }

  auto expr = asKnownExpr();
  IsRValue = true;
  new (&Storage.TheRV.Value) RValue(gen.emitRValue(expr));
  Storage.TheRV.Loc = expr;
  return Storage.TheRV.Value;
}

void RValueSource::rewriteType(CanType newType) & {
  if (isRValue()) {
    Storage.TheRV.Value.rewriteType(newType);
  } else {
    Expr *expr = Storage.TheExpr;
    if (expr->getType()->isEqual(newType)) return;
    assert(0 && "unimplemented! hope it doesn't happen");
  }
}

RValue RValueSource::getAsRValue(SILGenFunction &gen) && {
  if (isRValue()) {
    return std::move(*this).asKnownRValue();
  } else {
    return gen.emitRValue(std::move(*this).asKnownExpr());
  }
}

ManagedValue RValueSource::getAsSingleValue(SILGenFunction &gen) && {
  if (isRValue()) {
    auto loc = getKnownRValueLocation();
    return std::move(*this).asKnownRValue().getAsSingleValue(gen, loc);
  }

  auto e = std::move(*this).asKnownExpr();
  return gen.emitRValue(e).getAsSingleValue(gen, e);
}

void RValueSource::forwardInto(SILGenFunction &gen, Initialization *dest) && {
  if (isRValue()) {
    auto loc = getKnownRValueLocation();
    return std::move(*this).asKnownRValue().forwardInto(gen, dest, loc);
  }

  auto e = std::move(*this).asKnownExpr();
  return gen.emitExprInto(e, dest);
}

ManagedValue RValueSource::materialize(SILGenFunction &gen) && {
  if (isRValue()) {
    auto loc = getKnownRValueLocation();
    return std::move(*this).asKnownRValue().materialize(gen, loc);
  }

  auto expr = std::move(*this).asKnownExpr();
  auto temp = gen.emitTemporary(expr, gen.getTypeLowering(expr->getType()));
  gen.emitExprInto(expr, temp.get());
  return temp->getManagedAddress();
}
