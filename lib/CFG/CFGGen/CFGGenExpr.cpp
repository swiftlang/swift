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

Value *CFGGen::visitScalarToTupleExpr(ScalarToTupleExpr *E) {
  return B.createScalarToTuple(E, visit(E->getSubExpr()));
}

Value *CFGGen::visitTupleElementExpr(TupleElementExpr *E) {
  return B.createTupleElement(E, visit(E->getBase()), E->getFieldNumber());
}

Value *CFGGen::visitTupleShuffleExpr(TupleShuffleExpr *E) {
  // TupleShuffle expands out to extracts+inserts.  Start by emitting the base
  // expression that we'll shuffle.
  Value *Op = visit(E->getSubExpr());

  // Then collect the new elements.
  SmallVector<Value*, 8> ResultElements;

  // Loop over each result element to compute it.
  llvm::ArrayRef<TupleTypeElt> outerFields =
    E->getType()->getAs<TupleType>()->getFields();

  auto shuffleIndexIterator = E->getElementMapping().begin();
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
      assert(shuffleIndex >= 0 && (unsigned)shuffleIndex < outerFields.size());
      Type EltTy = outerField.getType();
      ResultElements.push_back(B.createTupleElement(EltTy, Op, shuffleIndex));
      continue;
    }

    // Okay, we have a varargs tuple element.  All the remaining elements feed
    // into the varargs portion of this, which is then constructed into a Slice
    // through an informal protocol captured by the InjectionFn in the
    // TupleShuffleExpr.
    auto shuffleIndexIteratorEnd = E->getElementMapping().end();
    llvm::ArrayRef<TupleTypeElt> InnerFields =
      E->getSubExpr()->getType()->getAs<TupleType>()->getFields();

    unsigned NumArrayElts = shuffleIndexIteratorEnd - shuffleIndexIterator;
    Value *AllocArray = B.createAllocArray(E, outerField.getVarargBaseTy(),
                                             NumArrayElts);

    Type BaseLValue =
      AllocArray->getType()->getAs<TupleType>()->getElementType(1);

    Value *BasePtr = B.createTupleElement(BaseLValue, AllocArray, 1);

    unsigned CurElem = 0;
    while (shuffleIndexIterator != shuffleIndexIteratorEnd) {
      unsigned SourceField = *shuffleIndexIterator++;

      Value *EltLoc = BasePtr;
      if (CurElem) EltLoc = B.createIndexLValue(E, EltLoc, CurElem);

      Type EltTy = InnerFields[SourceField].getType();
      Value *EltVal = B.createTupleElement(EltTy, Op, SourceField);
      B.createInitialization(E, EltVal, EltLoc);
      ++CurElem;
    }

    Value *ObjectPtr =
      B.createTupleElement(C.getContext().TheObjectPointerType, AllocArray, 0);
    (void)ObjectPtr;

    // FIXME: Need to bitcast the BasePtr (an lvalue) to Builtin.RawPtr.

   // visit(E->getVarargsInjectionFunction());
    // call Injection(Builtin.RawPointer, Builtin.ObjectPointer, typeof(length))
    //ResElement = Call Injection(BasePtr, Object, #elements)
    // ResultElements.push_back(ResElement);
    
    assert(0 && "FIXME: Varargs tuple shuffles not supported yet");
    break;
  }

  return B.createTuple(E, ResultElements);
}

Value *CFGGen::visitTypeOfExpr(TypeOfExpr *E) {
  return B.createTypeOf(E);
}

