//===--- ArgumentSource.cpp - Latent value representation -----------------===//
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
// A structure for holding a r-value or l-value
//
//===----------------------------------------------------------------------===//

#include "ArgumentSource.h"
#include "Conversion.h"
#include "Initialization.h"

using namespace swift;
using namespace Lowering;

RValue &ArgumentSource::peekRValue() & {
  assert(isRValue() && "Undefined behavior to call this method without the "
         "ArgumentSource actually being an RValue");
  return Storage.get<RValueStorage>(StoredKind).Value;
}

void ArgumentSource::rewriteType(CanType newType) & {
  switch (StoredKind) {
  case Kind::Invalid:
    llvm_unreachable("argument source is invalid");
  case Kind::LValue:
    llvm_unreachable("cannot rewrite type of l-value");
  case Kind::Tuple:
    llvm_unreachable("cannot rewrite type of tuple");
  case Kind::RValue:
    Storage.get<RValueStorage>(StoredKind).Value.rewriteType(newType);
    return;
  case Kind::Expr:
    Expr *&expr = Storage.get<Expr*>(StoredKind);
    CanType oldType = expr->getType()->getCanonicalType();

    // Usually nothing is required.
    if (oldType == newType) return;

    // Sometimes we need to wrap the expression in a single-element tuple.
    // This is only necessary because we don't break down the argument list
    // when dealing with SILGenApply.
    if (auto newTuple = dyn_cast<TupleType>(newType)) {
      if (newTuple->getNumElements() == 1 &&
          newTuple.getElementType(0) == oldType) {
        expr = TupleExpr::create(newType->getASTContext(),
                                 SourceLoc(), expr, {}, {}, SourceLoc(),
                                 /*trailing closure*/ false,
                                 /*implicit*/ true, newType);
        return;
      }
    }

    llvm_unreachable("unimplemented! hope it doesn't happen");
  }
  llvm_unreachable("bad kind");
}

bool ArgumentSource::requiresCalleeToEvaluate() const {
  switch (StoredKind) {
  case Kind::Invalid:
    llvm_unreachable("argument source is invalid");
  case Kind::RValue:
  case Kind::LValue:
    return false;
  case Kind::Expr:
    // FIXME: TupleShuffleExprs come in two flavors:
    //
    // 1) as apply arguments, where they're used to insert default
    // argument value and collect varargs
    //
    // 2) as tuple conversions, where they can introduce, eliminate
    // and re-order fields
    //
    // Case 1) must be emitted by ArgEmitter, and Case 2) must be
    // emitted by RValueEmitter.
    //
    // It would be good to split up TupleShuffleExpr into these two
    // cases, and simplify ArgEmitter since it no longer has to deal
    // with re-ordering. However for now, SubscriptExpr emits the
    // index argument via the RValueEmitter, so the RValueEmitter has
    // to know about varargs, duplicating some of the logic in
    // ArgEmitter.
    //
    // Once this is fixed, we can also consider allowing subscripts
    // to have default arguments.
    if (auto *shuffleExpr = dyn_cast<TupleShuffleExpr>(asKnownExpr())) {
      for (auto index : shuffleExpr->getElementMapping()) {
        if (index == TupleShuffleExpr::DefaultInitialize ||
            index == TupleShuffleExpr::CallerDefaultInitialize ||
            index == TupleShuffleExpr::Variadic)
          return true;
      }
    }
    return false;
  case Kind::Tuple:
    for (auto &source : Storage.get<TupleStorage>(StoredKind).Elements) {
      if (source.requiresCalleeToEvaluate())
        return true;
    }
    return false;
  }
  llvm_unreachable("bad kind");
}

RValue ArgumentSource::getAsRValue(SILGenFunction &SGF, SGFContext C) && {
  switch (StoredKind) {
  case Kind::Invalid:
    llvm_unreachable("argument source is invalid");
  case Kind::LValue:
    llvm_unreachable("cannot get l-value as r-value");
  case Kind::RValue:
    return std::move(*this).asKnownRValue(SGF);
  case Kind::Expr:
    return SGF.emitRValue(std::move(*this).asKnownExpr(), C);
  case Kind::Tuple:
    return std::move(*this).getKnownTupleAsRValue(SGF, C);
  }
  llvm_unreachable("bad kind");
}

RValue
ArgumentSource::getKnownTupleAsRValue(SILGenFunction &SGF, SGFContext C) && {

  return std::move(*this).withKnownTupleElementSources<RValue>(
                            [&](SILLocation loc, CanTupleType type,
                                MutableArrayRef<ArgumentSource> elements) {
    // If there's a target initialization, and we can split it, do so.
    if (auto init = C.getEmitInto()) {
      if (init->canSplitIntoTupleElements()) {
        // Split the tuple.
        SmallVector<InitializationPtr, 4> scratch;
        auto eltInits = init->splitIntoTupleElements(SGF, loc, type, scratch);

        // Emit each element into the corresponding element initialization.
        for (auto i : indices(eltInits)) {
          std::move(elements[i]).forwardInto(SGF, eltInits[i].get());
        }

        // Finish initialization.
        init->finishInitialization(SGF);

        return RValue::forInContext();
      }
    }

    // Otherwise, emit all of the elements into a single big r-value.
    RValue result(type);
    for (auto &element : elements) {
      result.addElement(std::move(element).getAsRValue(SGF));
    }
    return result;
  });
}

ManagedValue ArgumentSource::getAsSingleValue(SILGenFunction &SGF,
                                              SGFContext C) && {
  switch (StoredKind) {
  case Kind::Invalid:
    llvm_unreachable("argument source is invalid");
  case Kind::LValue: {
    auto loc = getKnownLValueLocation();
    return SGF.emitAddressOfLValue(loc, std::move(*this).asKnownLValue(),
                                   AccessKind::ReadWrite);
  }
  case Kind::RValue: {
    auto loc = getKnownRValueLocation();
    if (auto init = C.getEmitInto()) {
      std::move(*this).asKnownRValue(SGF)
                      .ensurePlusOne(SGF, loc)
                      .forwardInto(SGF, loc, init);
      return ManagedValue::forInContext();
    } else {
      return std::move(*this).asKnownRValue(SGF).getAsSingleValue(SGF, loc);
    }
  }
  case Kind::Expr: {
    auto e = std::move(*this).asKnownExpr();
    if (e->isSemanticallyInOutExpr()) {
      return SGF.emitAddressOfLValue(e, SGF.emitLValue(e, AccessKind::ReadWrite),
                                     AccessKind::ReadWrite);
    } else {
      return SGF.emitRValueAsSingleValue(e, C);
    }
  }
  case Kind::Tuple: {
    auto loc = getKnownTupleLocation();
    auto rvalue = std::move(*this).getKnownTupleAsRValue(SGF, C);
    if (rvalue.isInContext())
      return ManagedValue::forInContext();
    return std::move(rvalue).getAsSingleValue(SGF, loc);
  }
  }
  llvm_unreachable("bad kind");
}


ManagedValue ArgumentSource::getAsSingleValue(SILGenFunction &SGF,
                                              AbstractionPattern origFormalType,
                                              SGFContext C) && {
  auto substFormalType = getSubstType();
  auto conversion = Conversion::getSubstToOrig(origFormalType, substFormalType);
  return std::move(*this).getConverted(SGF, conversion, C);
}

ManagedValue ArgumentSource::getConverted(SILGenFunction &SGF,
                                          const Conversion &conversion,
                                          SGFContext C) && {
  switch (StoredKind) {
  case Kind::Invalid:
    llvm_unreachable("argument source is invalid");
  case Kind::LValue:
    llvm_unreachable("cannot get converted l-value");
  case Kind::RValue:
  case Kind::Expr:
  case Kind::Tuple:
    return SGF.emitConvertedRValue(getLocation(), conversion, C,
                [&](SILGenFunction &SGF, SILLocation loc, SGFContext C) {
      return std::move(*this).getAsSingleValue(SGF, C);
    });
  }
  llvm_unreachable("bad kind");
}

void ArgumentSource::forwardInto(SILGenFunction &SGF, Initialization *dest) && {
  switch (StoredKind) {
  case Kind::Invalid:
    llvm_unreachable("argument source is invalid");
  case Kind::LValue:
    llvm_unreachable("cannot forward an l-value");
  case Kind::RValue: {
    auto loc = getKnownRValueLocation();
    std::move(*this).asKnownRValue(SGF).ensurePlusOne(SGF, loc).forwardInto(SGF, loc, dest);
    return;
  }
  case Kind::Expr: {
    auto e = std::move(*this).asKnownExpr();
    SGF.emitExprInto(e, dest);
    return;
  }
  case Kind::Tuple: {
    auto loc = getKnownTupleLocation();
    auto rvalue = std::move(*this).getKnownTupleAsRValue(SGF, SGFContext(dest));
    if (!rvalue.isInContext())
      std::move(rvalue).ensurePlusOne(SGF, loc).forwardInto(SGF, loc, dest);
    return;
  }
  }
  llvm_unreachable("bad kind");
}

// FIXME: Once uncurrying is removed, get rid of this constructor.
ArgumentSource::ArgumentSource(SILLocation loc, RValue &&rv, Kind kind)
    : Storage(), StoredKind(kind) {
  Storage.emplaceAggregate<RValueStorage>(StoredKind, std::move(rv), loc);
}

ArgumentSource ArgumentSource::borrow(SILGenFunction &SGF) const & {
  switch (StoredKind) {
  case Kind::Invalid:
    llvm_unreachable("argument source is invalid");
  case Kind::LValue:
    llvm_unreachable("cannot borrow an l-value");
  case Kind::RValue: {
    auto loc = getKnownRValueLocation();
    return ArgumentSource(loc, asKnownRValue().borrow(SGF, loc));
  }
  case Kind::Expr: {
    llvm_unreachable("cannot borrow an expression");
  }
  case Kind::Tuple: {
    // FIXME: We can if we check the sub argument sources.
    llvm_unreachable("cannot borrow a tuple");
  }
  }
  llvm_unreachable("bad kind");
}

ManagedValue ArgumentSource::materialize(SILGenFunction &SGF) && {
  if (isRValue()) {
    auto loc = getKnownRValueLocation();
    return std::move(*this).asKnownRValue(SGF).materialize(SGF, loc);
  }

  auto loc = getLocation();
  auto temp = SGF.emitTemporary(loc, SGF.getTypeLowering(getSubstType()));
  std::move(*this).forwardInto(SGF, temp.get());
  return temp->getManagedAddress();
}

ManagedValue ArgumentSource::materialize(SILGenFunction &SGF,
                                         AbstractionPattern origFormalType,
                                         SILType destType) && {
  auto substFormalType = CanType(getSubstType()->getInOutObjectType());
  assert(!destType || destType.getObjectType() ==
               SGF.SGM.Types.getLoweredType(origFormalType,
                                            substFormalType).getObjectType());

  // Fast path: if the types match exactly, no abstraction difference
  // is possible and we can just materialize as normal.
  if (origFormalType.isExactType(substFormalType))
    return std::move(*this).materialize(SGF);

  auto &destTL =
    (destType ? SGF.getTypeLowering(destType)
              : SGF.getTypeLowering(origFormalType, substFormalType));
  if (!destType) destType = destTL.getLoweredType();

  // If there's no abstraction difference, we can just materialize as normal.
  if (destTL.getLoweredType() == SGF.getLoweredType(substFormalType)) {
    return std::move(*this).materialize(SGF);
  }

  // Emit a temporary at the given address.
  auto temp = SGF.emitTemporary(getLocation(), destTL);

  // Forward into it.
  std::move(*this).forwardInto(SGF, origFormalType, temp.get(), destTL);

  return temp->getManagedAddress();
}

void ArgumentSource::forwardInto(SILGenFunction &SGF,
                                 AbstractionPattern origFormalType,
                                 Initialization *dest,
                                 const TypeLowering &destTL) && {
  auto substFormalType = getSubstType();
  assert(destTL.getLoweredType() ==
                        SGF.getLoweredType(origFormalType, substFormalType));

  // If there are no abstraction changes, we can just forward
  // normally.
  if (origFormalType.isExactType(substFormalType) ||
      destTL.getLoweredType() == SGF.getLoweredType(substFormalType)) {
    std::move(*this).forwardInto(SGF, dest);
    return;
  }

  // Otherwise, emit as a single independent value.
  SILLocation loc = getLocation();
  ManagedValue outputValue =
      std::move(*this).getAsSingleValue(SGF, origFormalType,
                                        SGFContext(dest));

  if (outputValue.isInContext()) return;

  // Use RValue's forward-into-initialization code.  We have to lie to
  // RValue about the formal type (by using the lowered type) because
  // we're emitting into an abstracted value, which RValue doesn't
  // really handle.
  auto substLoweredType = destTL.getLoweredType().getASTType();
  RValue(SGF, loc, substLoweredType, outputValue).forwardInto(SGF, loc, dest);
}

SILType ArgumentSource::getSILSubstRValueType(SILGenFunction &SGF) const & {
  CanSILFunctionType funcType = SGF.F.getLoweredFunctionType();
  CanType substType = getSubstType();
  AbstractionPattern origType(funcType->getGenericSignature(), substType);
  return SGF.getLoweredType(origType, substType);
}

SILType ArgumentSource::getSILSubstType(SILGenFunction &SGF) const & {
  CanSILFunctionType funcType = SGF.F.getLoweredFunctionType();
  CanType substType = getSubstType();
  AbstractionPattern origType(funcType->getGenericSignature(), substType);
  return SGF.getLoweredType(origType, substType);
}

void ArgumentSource::dump() const {
  dump(llvm::errs());
}

void ArgumentSource::dump(raw_ostream &out, unsigned indent) const {
  out.indent(indent) << "ArgumentSource::";
  switch (StoredKind) {
  case Kind::Invalid:
    out << "Invalid\n";
    return;
  case Kind::LValue:
    out << "LValue\n";
    Storage.get<LValueStorage>(StoredKind).Value.dump(out, indent + 2);
    return;
  case Kind::Tuple: {
    out << "Tuple\n";
    auto &storage = Storage.get<TupleStorage>(StoredKind);
    storage.SubstType.dump(out, indent + 2);
    for (auto &elt : storage.Elements) {
      elt.dump(out, indent + 2);
      out << '\n';
    }
    return;
  }
  case Kind::RValue:
    out << "RValue\n";
    Storage.get<RValueStorage>(StoredKind).Value.dump(out, indent + 2);
    return;
  case Kind::Expr:
    out << "Expr\n";
    Storage.get<Expr*>(StoredKind)->dump(out); // FIXME: indent
    return;
  }
  llvm_unreachable("bad kind");
}

void PreparedArguments::emplaceEmptyArgumentList(SILGenFunction &SGF) {
  emplace(CanType(TupleType::getEmpty(SGF.getASTContext())), /*scalar*/ false);
  assert(isValid());
}

PreparedArguments
PreparedArguments::copy(SILGenFunction &SGF, SILLocation loc) const {
  if (isNull()) return PreparedArguments();

  assert(isValid());
  PreparedArguments result(getFormalType(), isScalar());
  for (auto &elt : Arguments) {
    assert(elt.isRValue());
    result.add(elt.getKnownRValueLocation(),
               elt.asKnownRValue().copy(SGF, loc));
  }
  assert(isValid());
  return result;
}

bool PreparedArguments::isObviouslyEqual(const PreparedArguments &other) const {
  if (isNull() != other.isNull())
    return false;
  if (isNull())
    return true;

  assert(isValid() && other.isValid());
  if (Arguments.size() != other.Arguments.size())
    return false;
  for (auto i : indices(Arguments)) {
    if (!Arguments[i].isObviouslyEqual(other.Arguments[i]))
      return false;
  }
  return true;
}

bool ArgumentSource::isObviouslyEqual(const ArgumentSource &other) const {
  if (StoredKind != other.StoredKind)
    return false;

  switch (StoredKind) {
  case Kind::Invalid:
    llvm_unreachable("argument source is invalid");
  case Kind::RValue:
    return asKnownRValue().isObviouslyEqual(other.asKnownRValue());
  case Kind::LValue:
    return false; // TODO?
  case Kind::Expr:
    return false; // TODO?
  case Kind::Tuple: {
    auto &selfTuple = Storage.get<TupleStorage>(StoredKind);
    auto &otherTuple = other.Storage.get<TupleStorage>(other.StoredKind);
    if (selfTuple.Elements.size() != otherTuple.Elements.size())
      return false;
    for (auto i : indices(selfTuple.Elements)) {
      if (!selfTuple.Elements[i].isObviouslyEqual(otherTuple.Elements[i]))
        return false;
    }
    return true;
  }
  }
  llvm_unreachable("bad kind");
}
