//===--- CFGGenExpr.cpp - Implements Lowering of ASTs -> CFGs for Exprs ---===//
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

#include "CFGGen.h"
#include "swift/AST/AST.h"
using namespace swift;
using namespace Lowering;

Value *CFGGen::visitApplyExpr(ApplyExpr *E) {
  Value *FnV = visit(E->getFn());
  llvm::SmallVector<Value*, 10> ArgsV;
  
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

Value *CFGGen::visitDeclRefExpr(DeclRefExpr *E) {
  // If this is a reference to a mutable decl, produce an lvalue.
  if (E->getType()->is<LValueType>()) {
    assert(VarLocs.count(E->getDecl()) && "VarDecl location not generated?");
    return VarLocs[E->getDecl()];
  }
  
  // Otherwise, we can only produce its value, use a ConstantRefInst.
  return B.createConstantRef(E);
}

Value *CFGGen::visitIntegerLiteralExpr(IntegerLiteralExpr *E) {
  return B.createIntegerLiteral(E);
}
Value *CFGGen::visitFloatLiteralExpr(FloatLiteralExpr *E) {
  return B.createFloatLiteral(E);
}
Value *CFGGen::visitCharacterLiteralExpr(CharacterLiteralExpr *E) {
  return B.createCharacterLiteral(E);
}
Value *CFGGen::visitStringLiteralExpr(StringLiteralExpr *E) {
  return B.createStringLiteral(E);
}

Value *CFGGen::visitLoadExpr(LoadExpr *E) {
  Value *SubV = visit(E->getSubExpr());
  return B.createLoad(E, SubV);
}

Value *CFGGen::visitMaterializeExpr(MaterializeExpr *E) {
  // Evaluate the value, use it to initialize a new temporary and return the
  // temp's address.
  Value *V = visit(E->getSubExpr());
  Value *TmpMem = B.createAllocTmp(E);
  B.createInitialization(E, V, TmpMem);
  return TmpMem;
}


Value *CFGGen::visitRequalifyExpr(RequalifyExpr *E) {
  return B.createTypeConversion(E, visit(E->getSubExpr()));
}

Value *CFGGen::visitFunctionConversionExpr(FunctionConversionExpr *E) {
  return B.createTypeConversion(E, visit(E->getSubExpr()));
}

Value *CFGGen::visitParenExpr(ParenExpr *E) {
  return visit(E->getSubExpr());
}

Value *CFGGen::visitTupleExpr(TupleExpr *E) {
  llvm::SmallVector<Value*, 10> ArgsV;
  for (auto &I : E->getElements())
    ArgsV.push_back(visit(I));
  return B.createTuple(E, ArgsV);
}

Value *CFGGen::visitSpecializeExpr(SpecializeExpr *E) {
  return B.createSpecialize(E, visit(E->getSubExpr()), E->getType());
}

Value *CFGGen::visitAddressOfExpr(AddressOfExpr *E) {
  return visit(E->getSubExpr());
}


Value *CFGGen::visitTupleElementExpr(TupleElementExpr *E) {
  return B.createTupleElement(E, visit(E->getBase()), E->getFieldNumber());
}

Value *CFGGen::emitTupleShuffle(Expr *E, ArrayRef<Value *> InOps,
                                ArrayRef<int> ElementMapping,
                                Expr *VarargsInjectionFunction) {
  // Collect the new elements.
  SmallVector<Value*, 8> ResultElements;

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
    Value *NumEltsVal = B.createIntegerValueInst(NumArrayElts,
                                   BuiltinIntegerType::get(64, C.getContext()));
    Value *AllocArray = B.createAllocArray(E, outerField.getVarargBaseTy(),
                                           NumEltsVal);

    Type BaseLValue =
      AllocArray->getType()->castTo<TupleType>()->getElementType(1);
    Value *BasePtr = B.createTupleElement(BaseLValue, AllocArray, 1);

    unsigned CurElem = 0;
    while (shuffleIndexIterator != shuffleIndexIteratorEnd) {
      unsigned SourceField = *shuffleIndexIterator++;
      
      Value *EltLoc = BasePtr;
      if (CurElem) EltLoc = B.createIndexLValue(E, EltLoc, CurElem);

      B.createInitialization(E, InOps[SourceField], EltLoc);
      ++CurElem;
    }

    Value *ObjectPtr =
      B.createTupleElement(C.getContext().TheObjectPointerType, AllocArray, 0);

    // Bitcast the BasePtr (an lvalue) to Builtin.RawPointer.
    BasePtr = B.createTypeConversion(C.getContext().TheRawPointerType, BasePtr);

    Value *InjectionFn = visit(VarargsInjectionFunction);
    Value *InjectionArgs[] = { BasePtr, ObjectPtr, NumEltsVal };
    ResultElements.push_back(B.createApply(InjectionFn, InjectionArgs));
    break;
  }

  return B.createTuple(E, ResultElements);
}

Value *CFGGen::visitTupleShuffleExpr(TupleShuffleExpr *E) {
  // TupleShuffle expands out to extracts+inserts.  Start by emitting the base
  // expression that we'll shuffle.
  Value *Op = visit(E->getSubExpr());
  SmallVector<Value*, 8> InElts;
  unsigned EltNo = 0;
  for (auto &InField : Op->getType()->castTo<TupleType>()->getFields())
    InElts.push_back(B.createTupleElement(InField.getType(), Op, EltNo++));

  return emitTupleShuffle(E, InElts, E->getElementMapping(),
                          E->getVarargsInjectionFunctionOrNull());
}

Value *CFGGen::visitScalarToTupleExpr(ScalarToTupleExpr *E) {
  // Emit the argument and turn it into a trivial tuple.
  Value *Arg = visit(E->getSubExpr());

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

Value *CFGGen::visitTypeOfExpr(TypeOfExpr *E) {
  return B.createTypeOf(E);
}

