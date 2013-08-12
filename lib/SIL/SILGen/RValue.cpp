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
                          /*Args...=*/ ManagedValue>
{
public:
  std::vector<ManagedValue> &values;
  SILGenFunction &gen;
  
  ExplodeTupleValue(std::vector<ManagedValue> &values,
                    SILGenFunction &gen)
    : values(values), gen(gen)
  {
  }
  
  void visitType(CanType t, ManagedValue v) {
    values.push_back(v);
  }
  
  void visitTupleType(CanTupleType t, ManagedValue mv) {
    SILValue v = mv.forward(gen);
    if (v.getType().isAddressOnly(gen.F.getModule())) {
      // Destructure address-only types by addressing the individual members.
      for (unsigned i = 0, size = t->getNumElements(); i < size; ++i) {
        CanType fieldCanTy = t.getElementType(i);
        auto &fieldTI = gen.getTypeLowering(fieldCanTy);
        SILType fieldTy = fieldTI.getLoweredType();
        SILValue member = gen.B.createTupleElementAddr(SILLocation(),
                                                     v, i,
                                                     fieldTy.getAddressType());
        assert(fieldTI.getSemanticType() == fieldTy);
        if (fieldTI.isLoadable() && !isa<LValueType>(fieldCanTy))
          member = gen.B.createLoad(SILLocation(), member);
        visit(fieldCanTy, gen.emitManagedRValueWithCleanup(member, fieldTI));
      }
    } else {
      // Extract the elements from loadable tuples.
      for (unsigned i = 0, size = t->getNumElements(); i < size; ++i) {
        CanType fieldCanTy = t.getElementType(i);
        auto &fieldTI = gen.getTypeLowering(fieldCanTy);
        assert(fieldTI.isLoadable());
        SILValue member = gen.B.createTupleExtract(SILLocation(), v, i,
                                                   fieldTI.getLoweredType());
        visit(fieldCanTy, gen.emitManagedRValueWithCleanup(member, fieldTI));
      }
    }
  }
};

enum class ImplodeKind { Unmanaged, Forward };

template<ImplodeKind KIND>
class ImplodeLoadableTupleValue
  : public CanTypeVisitor<ImplodeLoadableTupleValue<KIND>,
                          /*RetTy=*/ SILValue>
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
  
  SILValue visitType(CanType t) {
    SILValue result = getValue(gen, values[0]);
    values = values.slice(1);
    return result;
  }
  
  SILValue visitTupleType(CanTupleType t) {
    SmallVector<SILValue, 4> elts;
    for (auto fieldTy : t.getElementTypes())
      elts.push_back(this->visit(fieldTy));
    SILType ty = gen.getLoweredLoadableType(t);
    return gen.B.createTuple(SILLocation(), ty, elts);
  }
  
  ~ImplodeLoadableTupleValue() {
    assert(values.empty() && "values not exhausted imploding tuple?!");
  }
};
  
class ImplodeAddressOnlyTuple
  : public CanTypeVisitor<ImplodeAddressOnlyTuple,
                          /*RetTy=*/ void,
                          /*Args...=*/ SILValue>
{
public:
  ArrayRef<ManagedValue> values;
  SILGenFunction &gen;
  
  ImplodeAddressOnlyTuple(ArrayRef<ManagedValue> values,
                          SILGenFunction &gen)
    : values(values), gen(gen)
  {}
  
  void visitType(CanType t, SILValue address) {
    ManagedValue v = values[0];
    v.forwardInto(gen, SILLocation(), address);
    values = values.slice(1);
  }
  
  void visitTupleType(CanTupleType t, SILValue address) {
    for (unsigned n = 0, size = t->getNumElements(); n < size; ++n) {
      CanType fieldCanTy = t.getElementType(n);
      SILType fieldTy = gen.getLoweredType(fieldCanTy);
      SILValue fieldAddr = gen.B.createTupleElementAddr(SILLocation(),
                                                      address, n,
                                                      fieldTy.getAddressType());
      this->visit(fieldCanTy, fieldAddr);
    }
  }
  
  ~ImplodeAddressOnlyTuple() {
    assert(values.empty() && "values not exhausted imploding tuple?!");
  }
};
  
template<ImplodeKind KIND>
static SILValue implodeTupleValues(ArrayRef<ManagedValue> values,
                                   SILGenFunction &gen,
                                   CanType tupleType) {
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
    SILValue buffer = gen.emitTemporaryAllocation(SILLocation(), loweredType);
    ImplodeAddressOnlyTuple(values, gen).visit(tupleType, buffer);
    return buffer;
  }
  
  // To implode loadable tuples, we just need to combine the elements with
  // TupleInsts.
  return ImplodeLoadableTupleValue<KIND>(values, gen).visit(tupleType);
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
      llvm_unreachable("cannot emit into a byref binding");
    case Initialization::Kind::Tuple:
      llvm_unreachable("tuple initialization not destructured?!");

    case Initialization::Kind::Ignored:
      // Throw out the value without storing it.
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
    auto subInits = I->getSubInitializations(gen, subInitBuf);
    
    assert(subInits.size() == t->getNumElements() &&
           "initialization does not match tuple?!");
    
    for (unsigned i = 0, e = subInits.size(); i < e; ++i)
      visit(t.getElementType(i), subInits[i].get());
    
    I->finishInitialization(gen);
  }
};
  
class EmitBBArguments : public CanTypeVisitor<EmitBBArguments,
                                              /*RetTy*/ RValue>
{
public:
  SILGenFunction &gen;
  SILBasicBlock *parent;
  
  EmitBBArguments(SILGenFunction &gen, SILBasicBlock *parent)
    : gen(gen), parent(parent) {}
  
  RValue visitType(CanType t) {
    SILValue arg = new (gen.SGM.M) SILArgument(gen.getLoweredType(t), parent);
    ManagedValue mv = isa<LValueType>(t)
      ? ManagedValue(arg, ManagedValue::LValue)
      : gen.emitManagedRValueWithCleanup(arg);
    return RValue(gen, mv);
  }
  
  RValue visitTupleType(CanTupleType t) {
    RValue rv{t};
    
    for (auto fieldType : t.getElementTypes())
      rv.addElement(visit(fieldType));
    
    return rv;
  }
};
  
} // end anonymous namespace

RValue::RValue(ArrayRef<ManagedValue> values, CanType type)
  : values(values.begin(), values.end()), type(type), elementsToBeAdded(0)
{
}

RValue::RValue(SILGenFunction &gen, ManagedValue v)
  : type(v.getSwiftType()), elementsToBeAdded(0)
{
  ExplodeTupleValue(values, gen).visit(type, v);
}

RValue::RValue(CanType type)
  : type(type), elementsToBeAdded(getTupleSize(type)) {
}

RValue RValue::emitBBArguments(CanType type,
                               SILGenFunction &gen,
                               SILBasicBlock *parent) {
  return EmitBBArguments(gen, parent).visit(type);
}

void RValue::addElement(RValue &&element) & {
  assert(!isComplete() && "rvalue already complete");
  assert(!isUsed() && "rvalue already used");
  --elementsToBeAdded;
  values.insert(values.end(),
                element.values.begin(), element.values.end());
  element.makeUsed();
}

void RValue::addElement(SILGenFunction &gen, ManagedValue element) & {
  assert(!isComplete() && "rvalue already complete");
  assert(!isUsed() && "rvalue already used");
  --elementsToBeAdded;

  ExplodeTupleValue(values, gen).visit(element.getSwiftType(), element);
}

SILValue RValue::forwardAsSingleValue(SILGenFunction &gen) && {
  assert(isComplete() && "rvalue is not complete");
  SILValue result
    = implodeTupleValues<ImplodeKind::Forward>(values, gen, type);

  makeUsed();
  return result;
}

void RValue::forwardInto(SILGenFunction &gen, Initialization *I,
                         SILLocation loc) && {
  assert(isComplete() && "rvalue is not complete");
  InitializeTupleValues(values, gen, loc).visit(type, I);
}

ManagedValue RValue::getAsSingleValue(SILGenFunction &gen) && {
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
                                    std::move(*this).forwardAsSingleValue(gen));
}

SILValue RValue::getUnmanagedSingleValue(SILGenFunction &gen) const & {
  assert(isComplete() && "rvalue is not complete");
  return implodeTupleValues<ImplodeKind::Unmanaged>(values, gen, type);
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

RValue::RValue(const RValue &copied, SILGenFunction &gen)
  : type(copied.type),
    elementsToBeAdded(copied.elementsToBeAdded)
{
  assert((copied.isComplete() || copied.isUsed())
         && "can't copy incomplete rvalue");
  values.reserve(copied.values.size());
  for (ManagedValue value : copied.values) {
    values.push_back(value.copy(gen));
  }
}

void ManagedValue::forwardInto(SILGenFunction &gen, SILLocation loc,
                               SILValue address) {
  if (hasCleanup())
    forwardCleanup(gen);
  auto &lowering = gen.getTypeLowering(address.getType().getSwiftRValueType());
  lowering.emitSemanticInitialize(gen.B, loc, getValue(), address);
}

void ManagedValue::assignInto(SILGenFunction &gen, SILLocation loc,
                              SILValue address) {
  if (hasCleanup())
    forwardCleanup(gen);
  auto &lowering = gen.getTypeLowering(address.getType().getSwiftRValueType());

  if (gen.SGM.M.getASTContext().LangOpts.UseDefiniteInit)
    lowering.emitSemanticAssignOrInitialize(gen.B, loc, getValue(), address);
  else
    lowering.emitSemanticAssign(gen.B, loc, getValue(), address);
}
