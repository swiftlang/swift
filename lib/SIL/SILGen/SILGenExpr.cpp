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
using namespace swift;
using namespace Lowering;

Value SILGen::visitApplyExpr(ApplyExpr *E) {
  Value FnV = visit(E->getFn());
  llvm::SmallVector<Value, 10> ArgsV;
  
  // Special case Arg being a TupleExpr, to inline the arguments and
  // not create another instruction.
  if (TupleExpr *TU = dyn_cast<TupleExpr>(E->getArg())) {
    for (auto arg : TU->getElements())
      ArgsV.push_back(visit(arg));
  } else {
    ArgsV.push_back(visit(E->getArg()));
  }
  
  return B.createApply(E, FnV, ArgsV);
}

Value SILGen::visitDeclRefExpr(DeclRefExpr *E) {
  // If this is a reference to a mutable decl, produce an lvalue.
  if (E->getType()->is<LValueType>()) {
    assert(VarLocs.count(E->getDecl()) && "VarDecl location not generated?");
    return VarLocs[E->getDecl()];
  }
  
  // Otherwise, we can only produce its value, use a ConstantRefInst.
  return B.createConstantRef(E);
}

Value SILGen::visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
  return B.createIntegerLiteral(E);
}
Value SILGen::visitFloatLiteralExpr(FloatLiteralExpr *E) {
  return B.createFloatLiteral(E);
}
Value SILGen::visitCharacterLiteralExpr(CharacterLiteralExpr *E) {
  return B.createCharacterLiteral(E);
}
Value SILGen::visitStringLiteralExpr(StringLiteralExpr *E) {
  return B.createStringLiteral(E);
}

Value SILGen::visitLoadExpr(LoadExpr *E) {
  Value SubV = visit(E->getSubExpr());
  return B.createLoad(E, SubV);
}

Value SILGen::visitMaterializeExpr(MaterializeExpr *E) {
  // Evaluate the value, use it to initialize a new temporary and return the
  // temp's address.
  Value V = visit(E->getSubExpr());
  Value TmpMem = B.createAllocTmp(E);
  B.createInitialization(E, V, TmpMem);
  return TmpMem;
}


Value SILGen::visitRequalifyExpr(RequalifyExpr *E) {
  return B.createTypeConversion(E, visit(E->getSubExpr()));
}

Value SILGen::visitFunctionConversionExpr(FunctionConversionExpr *E) {
  return B.createTypeConversion(E, visit(E->getSubExpr()));
}

Value SILGen::visitParenExpr(ParenExpr *E) {
  return visit(E->getSubExpr());
}

Value SILGen::visitTupleExpr(TupleExpr *E) {
  llvm::SmallVector<Value, 10> ArgsV;
  for (auto &I : E->getElements())
    ArgsV.push_back(visit(I));
  return B.createTuple(E, ArgsV);
}

Value SILGen::visitGetMetatypeExpr(GetMetatypeExpr *E) {
  return visit(E->getSubExpr());
}

Value SILGen::visitSpecializeExpr(SpecializeExpr *E) {
  return B.createSpecialize(E, visit(E->getSubExpr()), E->getType());
}

Value SILGen::visitAddressOfExpr(AddressOfExpr *E) {
  return visit(E->getSubExpr());
}


Value SILGen::visitTupleElementExpr(TupleElementExpr *E) {
  return B.createTupleElement(E, visit(E->getBase()), E->getFieldNumber());
}


/// emitArrayInjectionCall - Form an array "Slice" out of an ObjectPointer
/// (which represents the retain count) a base pointer to some elements, and a
/// length
Value SILGen::emitArrayInjectionCall(Value ObjectPtr, Value BasePtr,
                                     Value Length,
                                     Expr *ArrayInjectionFunction) {
  // Bitcast the BasePtr (an lvalue) to Builtin.RawPointer if it isn't already.
  if (!BasePtr.getType()->isEqual(F.getContext().TheRawPointerType))
    BasePtr = B.createTypeConversion(F.getContext().TheRawPointerType, BasePtr);

  Value InjectionFn = visit(ArrayInjectionFunction);
  Value InjectionArgs[] = { BasePtr, ObjectPtr, Length };
  return B.createApply(InjectionFn, InjectionArgs);
}


Value SILGen::emitTupleShuffle(Expr *E, ArrayRef<Value> InOps,
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
      ResultElements.push_back(visit(outerField.getInit()->getExpr()));
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
    Value AllocArray = B.createAllocArray(E, outerField.getVarargBaseTy(),
                                          NumEltsVal);

    Type BaseLValue =
      AllocArray.getType()->castTo<TupleType>()->getElementType(1);
    Value BasePtr = B.createTupleElement(BaseLValue, AllocArray, 1);

    unsigned CurElem = 0;
    while (shuffleIndexIterator != shuffleIndexIteratorEnd) {
      unsigned SourceField = *shuffleIndexIterator++;
      
      Value EltLoc = BasePtr;
      if (CurElem) EltLoc = B.createIndexLValue(E, EltLoc, CurElem);

      B.createInitialization(E, InOps[SourceField], EltLoc);
      ++CurElem;
    }

    Value ObjectPtr =
      B.createTupleElement(F.getContext().TheObjectPointerType, AllocArray, 0);

    ResultElements.push_back(emitArrayInjectionCall(ObjectPtr, BasePtr,
                                        NumEltsVal, VarargsInjectionFunction));
    break;
  }

  return B.createTuple(E, ResultElements);
}

Value SILGen::visitTupleShuffleExpr(TupleShuffleExpr *E) {
  // TupleShuffle expands out to extracts+inserts.  Start by emitting the base
  // expression that we'll shuffle.
  Value Op = visit(E->getSubExpr());
  SmallVector<Value, 8> InElts;
  unsigned EltNo = 0;
  for (auto &InField : Op.getType()->castTo<TupleType>()->getFields())
    InElts.push_back(B.createTupleElement(InField.getType(), Op, EltNo++));

  return emitTupleShuffle(E, InElts, E->getElementMapping(),
                          E->getVarargsInjectionFunctionOrNull());
}

Value SILGen::visitScalarToTupleExpr(ScalarToTupleExpr *E) {
  // Emit the argument and turn it into a trivial tuple.
  Value Arg = visit(E->getSubExpr());

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

Value SILGen::visitNewArrayExpr(NewArrayExpr *E) {
  Value NumElements = visit(E->getBounds()[0].Value);

  // Allocate the array.
  Value AllocArray = B.createAllocArray(E, E->getElementType(), NumElements);

  Type BaseLValue =
    AllocArray.getType()->castTo<TupleType>()->getElementType(1);
  Value BasePtr = B.createTupleElement(BaseLValue, AllocArray, 1);
  Value ObjectPtr =
    B.createTupleElement(F.getContext().TheObjectPointerType, AllocArray, 0);

  // FIXME: We need to initialize the elements of the array that are now
  // allocated.

  // Finally, build and return a Slice instance using the object
  // header/base/count.
  return emitArrayInjectionCall(ObjectPtr, BasePtr, NumElements,
                                E->getInjectionFunction());
}



Value SILGen::visitMetatypeExpr(MetatypeExpr *E) {
  return B.createMetatype(E);
}

