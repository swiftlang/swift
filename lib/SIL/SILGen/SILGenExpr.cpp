//===--- SILGenExpr.cpp - Implements Lowering of ASTs -> SIL for Exprs ----===//
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

#include "SILGen.h"
#include "swift/AST/AST.h"
#include "Explosion.h"
#include "TypeInfo.h"
using namespace swift;
using namespace Lowering;

namespace {
  class CleanupRValue : public Cleanup {
    Value rv;
  public:
    CleanupRValue(Value rv) : rv(rv) {}
    
    void emit(SILGen &gen) override {
      gen.emitReleaseRValue(SILLocation(), rv);
    }
  };
}

static ManagedValue managedRValueWithCleanup(SILGen &gen, Value v) {
  if (v.getType()->is<LValueType>() || TypeInfo::get(v.getType()).isTrivial()) {
    return ManagedValue(v);
  } else {
    gen.Cleanups.pushCleanup<CleanupRValue>(v);
    return ManagedValue(v, gen.getCleanupsDepth());
  }
}

ManagedValue SILGen::visitExpr(Expr *E) {
  E->dump();
  llvm_unreachable("Not yet implemented");
}

ManagedValue SILGen::visitApplyExpr(ApplyExpr *E) {
  // FIXME: This assumes that all Swift arguments and returns lower one-to-one
  // to SIL arguments and returns, which won't hold up in the face of
  // address-only types.
  Value FnV = visit(E->getFn()).forward(*this);
  llvm::SmallVector<Value, 10> ArgsV;
  
  Expr *argExpr = E->getArg();
  if (ParenExpr *pe = dyn_cast<ParenExpr>(argExpr))
    argExpr = pe->getSubExpr();
  
  // Special case Arg being a TupleExpr or ScalarToTupleExpr to inline the
  // arguments and not create a tuple instruction.
  if (TupleExpr *te = dyn_cast<TupleExpr>(argExpr)) {
    for (auto arg : te->getElements())
      ArgsV.push_back(visit(arg).forward(*this));
  } else if (ScalarToTupleExpr *se = dyn_cast<ScalarToTupleExpr>(argExpr)) {
    ArgsV.push_back(visit(se->getSubExpr()).forward(*this));
  } else {
    ArgsV.push_back(visit(argExpr).forward(*this));
  }
  
  return managedRValueWithCleanup(*this, B.createApply(E, FnV, ArgsV));
}

ManagedValue SILGen::visitDeclRefExpr(DeclRefExpr *E) {
  // If this is a reference to a mutable decl, produce an lvalue.
  if (E->getType()->is<LValueType>()) {
    assert(VarLocs.count(E->getDecl()) && "VarDecl location not generated?");
    return ManagedValue(VarLocs[E->getDecl()]);
  }
  
  // Otherwise, we can only produce its value, so use a ConstantRefInst.
  return ManagedValue(B.createConstantRef(E));
}

ManagedValue SILGen::visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
  return ManagedValue(B.createIntegerLiteral(E));
}
ManagedValue SILGen::visitFloatLiteralExpr(FloatLiteralExpr *E) {
  return ManagedValue(B.createFloatLiteral(E));
}
ManagedValue SILGen::visitCharacterLiteralExpr(CharacterLiteralExpr *E) {
  return ManagedValue(B.createIntegerLiteral(E));
}
ManagedValue SILGen::visitStringLiteralExpr(StringLiteralExpr *E) {
  return ManagedValue(B.createStringLiteral(E));
}

ManagedValue SILGen::visitLoadExpr(LoadExpr *E) {
  ManagedValue SubV = visit(E->getSubExpr());
  Value loadedV = B.createLoad(E, SubV.getUnmanagedValue());
  emitRetainRValue(E, loadedV);
  return managedRValueWithCleanup(*this, loadedV);
}

ManagedValue SILGen::visitMaterializeExpr(MaterializeExpr *E) {
  // Evaluate the value, use it to initialize a new temporary and return the
  // temp's address.
  Value V = visit(E->getSubExpr()).forward(*this);
  // FIXME: eliminate AllocTmp and generate this as an AllocVar with
  // DeallocVar cleanup.
  Value TmpMem = B.createAllocTmp(E);
  B.createStore(E, V, TmpMem);
  // The DeallocVar cleanup's ownership will not be forwarded to a calling
  // function, so this ManagedValue for the temporary allocation does not
  // reference its cleanup.
  return ManagedValue(TmpMem);
}

ManagedValue SILGen::visitRequalifyExpr(RequalifyExpr *E) {
  return ManagedValue(B.createConvert(E,
                                      visit(E->getSubExpr()).getValue(),
                                      E->getType()));
}

ManagedValue SILGen::visitFunctionConversionExpr(FunctionConversionExpr *E) {
  return ManagedValue(B.createConvert(E,
                                      visit(E->getSubExpr()).getValue(),
                                      E->getType()));
}

ManagedValue SILGen::visitParenExpr(ParenExpr *E) {
  return visit(E->getSubExpr());
}

ManagedValue SILGen::visitTupleExpr(TupleExpr *E) {
  llvm::SmallVector<Value, 10> ArgsV;
  for (auto &I : E->getElements())
    ArgsV.push_back(visit(I).forward(*this));
  return managedRValueWithCleanup(*this,
                                  B.createTuple(E, E->getType(), ArgsV));
}

ManagedValue SILGen::visitGetMetatypeExpr(GetMetatypeExpr *E) {
  return visit(E->getSubExpr());
}

ManagedValue SILGen::visitSpecializeExpr(SpecializeExpr *E) {
  return ManagedValue(B.createSpecialize(E,
                                    visit(E->getSubExpr()).getUnmanagedValue(),
                                    E->getType()));
}

ManagedValue SILGen::visitAddressOfExpr(AddressOfExpr *E) {
  return visit(E->getSubExpr());
}


ManagedValue SILGen::visitTupleElementExpr(TupleElementExpr *E) {
  Value elt = B.createTupleElement(E,
                                   visit(E->getBase()).getValue(),
                                   E->getFieldNumber(),
                                   E->getType());
  emitRetainRValue(E, elt);
  return managedRValueWithCleanup(*this, elt);
}


/// emitArrayInjectionCall - Form an array "Slice" out of an ObjectPointer
/// (which represents the retain count) a base pointer to some elements, and a
/// length
ManagedValue SILGen::emitArrayInjectionCall(Value ObjectPtr,
                                            Value BasePtr,
                                            Value Length,
                                            Expr *ArrayInjectionFunction) {
  // Bitcast the BasePtr (an lvalue) to Builtin.RawPointer if it isn't already.
  if (!BasePtr.getType()->isEqual(F.getContext().TheRawPointerType))
    BasePtr = B.createConvert(SILLocation(),
                              BasePtr,
                              F.getContext().TheRawPointerType);

  Value InjectionFn = visit(ArrayInjectionFunction).getUnmanagedValue();
  Value InjectionArgs[] = { BasePtr, ObjectPtr, Length };
  return managedRValueWithCleanup(*this,
                                  B.createApply(SILLocation(),
                                                InjectionFn, InjectionArgs));
}


ManagedValue SILGen::emitTupleShuffle(Expr *E,
                                      ArrayRef<Value> InOps,
                                      ArrayRef<int> ElementMapping,
                                      Expr *VarargsInjectionFunction) {
  // Collect the new elements.
  SmallVector<Value, 8> ResultElements;

  // Loop over each result element to compute it.
  ArrayRef<TupleTypeElt> outerFields =
    E->getType()->castTo<TupleType>()->getFields();

  auto shuffleIndexIterator = ElementMapping.begin();
  for (const TupleTypeElt &outerField : outerFields) {
    int shuffleIndex = *shuffleIndexIterator++;

    // If the shuffle index is -1, we're supposed to use the default value.
    if (shuffleIndex == -1) {
      assert(outerField.hasInit() && "no default initializer for field!");
      ResultElements.push_back(visit(outerField.getInit()->getExpr())
                                 .forward(*this));
      continue;
    }

    // If the shuffle index is -2, it is the beginning of the list of
    // varargs inputs.  Save this case for last.
    if (shuffleIndex != -2) {
      // Map from a different tuple element.
      ResultElements.push_back(InOps[shuffleIndex]);
      continue;
    }

    assert(outerField.isVararg() && "Cannot initialize nonvariadic element");

    // Okay, we have a varargs tuple element.  All the remaining elements feed
    // into the varargs portion of this, which is then constructed into a Slice
    // through an informal protocol captured by the InjectionFn in the
    // TupleShuffleExpr.
    auto shuffleIndexIteratorEnd = ElementMapping.end();
    unsigned NumArrayElts = shuffleIndexIteratorEnd - shuffleIndexIterator;
    Value NumEltsVal = B.createIntegerValueInst(NumArrayElts,
                                   BuiltinIntegerType::get(64, F.getContext()));
    AllocArrayInst *AllocArray =
      B.createAllocArray(E, outerField.getVarargBaseTy(), NumEltsVal);

    Value ObjectPtr(AllocArray, 0);
    Value BasePtr(AllocArray, 1);

    unsigned CurElem = 0;
    while (shuffleIndexIterator != shuffleIndexIteratorEnd) {
      unsigned SourceField = *shuffleIndexIterator++;
      
      Value EltLoc = BasePtr;
      if (CurElem) EltLoc = B.createIndexAddr(E, EltLoc, CurElem);

      B.createStore(E, InOps[SourceField], EltLoc);
      ++CurElem;
    }

    ResultElements.push_back(emitArrayInjectionCall(ObjectPtr, BasePtr,
                                        NumEltsVal, VarargsInjectionFunction)
                               .forward(*this));
    break;
  }

  return managedRValueWithCleanup(*this, B.createTuple(E, E->getType(),
                                                       ResultElements));
}

ManagedValue SILGen::visitTupleShuffleExpr(TupleShuffleExpr *E) {
  // TupleShuffle expands out to extracts+inserts.  Start by emitting the base
  // expression that we'll shuffle.
  Value Op = visit(E->getSubExpr()).getValue();
  SmallVector<Value, 8> InElts;
  unsigned EltNo = 0;
  for (auto &InField : Op.getType()->castTo<TupleType>()->getFields()) {
    Value elt = B.createTupleElement(SILLocation(), Op, EltNo++,
                                     InField.getType());
    emitRetainRValue(E, elt);
    InElts.push_back(elt);
  }

  return emitTupleShuffle(E, InElts, E->getElementMapping(),
                          E->getVarargsInjectionFunctionOrNull());
}

ManagedValue SILGen::visitScalarToTupleExpr(ScalarToTupleExpr *E) {
  // Emit the argument and turn it into a trivial tuple.
  Value Arg = visit(E->getSubExpr()).getValue();

  // If we don't have exactly the same tuple, perform a shuffle to create
  // default arguments etc.
  SmallVector<int, 8> ShuffleMask;

  auto outerFields = E->getType()->castTo<TupleType>()->getFields();
  for (unsigned i = 0, e = outerFields.size(); i != e; ++i) {
    // If we get to the last argument and it is a varargs list, make sure to
    // mark it with a "-2" entry.
    if (outerFields[i].isVararg())
      ShuffleMask.push_back(-2);

    // If we have a field with a default value, emit that value.  Otherwise, use
    // the tuple we have as input.
    if (i == E->getScalarField())
      ShuffleMask.push_back(0);
    else if (!outerFields[i].isVararg())
      ShuffleMask.push_back(-1);
  }

  return emitTupleShuffle(E, Arg, ShuffleMask,E->getVarargsInjectionFunction());
}

ManagedValue SILGen::visitNewArrayExpr(NewArrayExpr *E) {
  Value NumElements = visit(E->getBounds()[0].Value).getValue();

  // Allocate the array.
  AllocArrayInst *AllocArray = B.createAllocArray(E, E->getElementType(),
                                                  NumElements);

  Value ObjectPtr(AllocArray, 0), BasePtr(AllocArray, 1);

  // FIXME: We need to initialize the elements of the array that are now
  // allocated.

  // Finally, build and return a Slice instance using the object
  // header/base/count.
  return emitArrayInjectionCall(ObjectPtr, BasePtr, NumElements,
                                E->getInjectionFunction());
}



ManagedValue SILGen::visitMetatypeExpr(MetatypeExpr *E) {
  return ManagedValue(B.createMetatype(E));
}

