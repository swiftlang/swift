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
#include "swift/SIL/TypeVisitor.h"
#include <deque>

using namespace swift;
using namespace Lowering;

namespace {

static unsigned getTupleSize(CanType t) {
  if (TupleType *tt = dyn_cast<TupleType>(t))
    return tt->getFields().size();
  return 1;
}

class ExplodeTupleValue
  : public Lowering::TypeVisitor<ExplodeTupleValue,
                                 /*RetTy=*/ void,
                                 /*Args...=*/ ManagedValue>
{
public:
  std::vector<ManagedValue> &values;
  std::vector<unsigned> &elementOffsets;
  SILGenFunction &gen;
  unsigned depth;
  
  ExplodeTupleValue(std::vector<ManagedValue> &values,
                    std::vector<unsigned> &elementOffsets,
                    SILGenFunction &gen,
                    unsigned depth)
    : values(values), elementOffsets(elementOffsets), gen(gen), depth(depth)
  {
    // The first element is always at offset 0.
    elementOffsets.push_back(0);
  }
  
  void visitType(TypeBase *t, ManagedValue v) {
    values.push_back(v);
  }
  
  void visitTupleType(TupleType *t, ManagedValue v) {
    ++depth;
    if (v.getType().isAddressOnly()) {
      // Destructure address-only types by addressing the individual members.
      for (unsigned i = 0; i < t->getFields().size(); ++i) {
        auto &field = t->getFields()[i];
        SILType fieldTy = gen.getLoweredType(field.getType());
        SILValue member = gen.B.createElementAddr(SILLocation(),
                                                  v.getValue(),
                                                  i,
                                                  fieldTy.getAddressType());
        if (!fieldTy.isAddressOnly() && !field.getType()->is<LValueType>())
          member = gen.B.createLoad(SILLocation(), member);
        visit(CanType(field.getType()),
              gen.emitManagedRValueWithCleanup(member));
        if (depth == 1)
          elementOffsets.push_back(values.size());
      }
    } else {
      // Extract the elements from loadable tuples.
      for (unsigned i = 0; i < t->getFields().size(); ++i) {
        auto &field = t->getFields()[i];
        SILType fieldTy = gen.getLoweredType(field.getType());
        SILValue member = gen.B.createExtract(SILLocation(),
                                              v.getValue(),
                                              i,
                                              fieldTy);
        visit(CanType(field.getType()),
              gen.emitManagedRValueWithCleanup(member));
        if (depth == 1)
          elementOffsets.push_back(values.size());
      }
    }
    --depth;
  }
};

enum class ImplodeKind { Unmanaged, Forward };

template<ImplodeKind KIND>
class ImplodeLoadableTupleValue
  : public Lowering::TypeVisitor<ImplodeLoadableTupleValue<KIND>,
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
  
  SILValue visitType(TypeBase *t) {
    SILValue result = getValue(gen, values[0]);
    values = values.slice(1);
    return result;
  }
  
  SILValue visitTupleType(TupleType *t) {
    SmallVector<SILValue, 4> elts;
    for (auto &field : t->getFields())
      elts.push_back(this->visit(CanType(field.getType())));
    SILType ty = gen.getLoweredLoadableType(t);
    return gen.B.createTuple(SILLocation(), ty, elts);
  }
  
  ~ImplodeLoadableTupleValue() {
    assert(values.empty() && "values not exhausted imploding tuple?!");
  }
};
  
class ImplodeAddressOnlyTuple
  : public Lowering::TypeVisitor<ImplodeAddressOnlyTuple,
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
  
  void visitType(TypeBase *t, SILValue address) {
    ManagedValue v = values[0];
    if (v.getType().isAddressOnly())
      v.forwardInto(gen, SILLocation(), address, /*isInitialize=*/true);
    else
      gen.emitStore(SILLocation(), v, address);
    values = values.slice(1);
  }
  
  void visitTupleType(TupleType *t, SILValue address) {
    for (unsigned n = 0; n < t->getFields().size(); ++n) {
      auto &field = t->getFields()[n];
      SILType fieldTy = gen.getLoweredType(field.getType());
      SILValue fieldAddr = gen.B.createElementAddr(SILLocation(),
                                                   address, n,
                                                   fieldTy.getAddressType());
      this->visit(CanType(field.getType()), fieldAddr);
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
  if (loweredType.isAddressOnly()) {
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
  
class ComputeElementOffsets
  : public Lowering::TypeVisitor<ComputeElementOffsets>
{
public:
  unsigned offset = 0;
  
  void visitType(TypeBase *t) {
    ++offset;
  }
  
  void visitTupleType(TupleType *t) {
    for (auto &field : t->getFields())
      visit(CanType(field.getType()));
  }
};
  
static void computeElementOffsets(std::vector<unsigned> &offsets,
                                  CanType type) {
  // The first element is always at offset zero.
  offsets.push_back(0);
  TupleType *tuple = dyn_cast<TupleType>(type);
  // Non-tuples always have a single value.
  if (!tuple) {
    offsets.push_back(1);
    return;
  }
  
  // Visit each field and record its ending value offset.
  ComputeElementOffsets visitor;
  for (auto &field : tuple->getFields()) {
    visitor.visit(CanType(field.getType()));
    offsets.push_back(visitor.offset);
  }
}
  
class InitializeTupleValues
  : public Lowering::TypeVisitor<InitializeTupleValues,
                                 /*RetTy=*/ void,
                                 /*Args...=*/ Initialization*>
{
public:
  ArrayRef<ManagedValue> values;
  SILGenFunction &gen;
  
  InitializeTupleValues(ArrayRef<ManagedValue> values, SILGenFunction &gen)
    : values(values), gen(gen)
  {}
  
  void visitType(TypeBase *t, Initialization *I) {
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
        if (result.getType().isAddressOnly())
          result.forwardInto(gen, SILLocation(), I->getAddress(),
                             /*isInitialize=*/true);
        else
          gen.emitStore(SILLocation(), result, I->getAddress());
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
  
  void visitTupleType(TupleType *t, Initialization *I) {
    // Break up the aggregate initialization.
    SmallVector<InitializationPtr, 4> subInitBuf;
    auto subInits = I->getSubInitializations(gen, subInitBuf);
    
    assert(subInits.size() == t->getFields().size() &&
           "initialization does not match tuple?!");
    
    for (unsigned i = 0, e = subInits.size(); i < e; ++i)
      visit(CanType(t->getFields()[i].getType()), subInits[i].get());
    
    I->finishInitialization(gen);
  }
};
  
} // end anonymous namespace

RValue::RValue(ArrayRef<ManagedValue> values, CanType type)
  : values(values.begin(), values.end()), type(type), elementsToBeAdded(0)
{
  computeElementOffsets(elementOffsets, type);
}

RValue::RValue(SILGenFunction &gen, ManagedValue v)
  : type(v.getSwiftType()), elementsToBeAdded(0)
{
  ExplodeTupleValue(values, elementOffsets, gen, 0).visit(type, v);
}

RValue::RValue(CanType type)
  : type(type), elementsToBeAdded(getTupleSize(type)) {
  elementOffsets.push_back(0);
}

void RValue::addElement(RValue &&element) & {
  assert(!isComplete() && "rvalue already complete");
  assert(!isUsed() && "rvalue already used");
  --elementsToBeAdded;
  values.insert(values.end(),
                element.values.begin(), element.values.end());
  elementOffsets.push_back(values.size());
  element.makeUsed();
}

void RValue::addElement(SILGenFunction &gen, ManagedValue element) & {
  assert(!isComplete() && "rvalue already complete");
  assert(!isUsed() && "rvalue already used");
  --elementsToBeAdded;

  ExplodeTupleValue(values, elementOffsets, gen, 1)
    .visit(element.getSwiftType(), element);
  elementOffsets.push_back(values.size());
}

SILValue RValue::forwardAsSingleValue(SILGenFunction &gen) && {
  assert(isComplete() && "rvalue is not complete");
  SILValue result
    = implodeTupleValues<ImplodeKind::Forward>(values, gen, type);

  makeUsed();
  return result;
}

void RValue::forwardInto(SILGenFunction &gen, Initialization *I) && {
  assert(isComplete() && "rvalue is not complete");
  InitializeTupleValues(values, gen).visit(type, I);
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

RValue RValue::extractElement(unsigned n) && {
  assert(isComplete() && "rvalue is not complete");
  
  unsigned from = elementOffsets[n], to = elementOffsets[n+1];
  CanType eltTy(cast<TupleType>(type)->getFields()[n].getType());
  
  RValue element(llvm::makeArrayRef(values).slice(from, to - from), eltTy);
  makeUsed();
  return element;
}

void RValue::extractElements(SmallVectorImpl<RValue> &elements) && {
  assert(isComplete() && "rvalue is not complete");

  auto fields = cast<TupleType>(type)->getFields();
  
  for (unsigned n = 0; n < fields.size(); ++n) {
    unsigned from = elementOffsets[n], to = elementOffsets[n+1];
    elements.push_back({llvm::makeArrayRef(values).slice(from, to - from),
                        CanType(fields[n].getType())});
  }
  makeUsed();
}
