//===--- SILGen.cpp - Implements Lowering of ASTs -> SIL ------------------===//
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

//===--------------------------------------------------------------------===//
// SILGen Class implementation
//===--------------------------------------------------------------------===//

// TODO: more accurately port the result schema logic from
// IRGenFunction::emitEpilogue to handle all cases where a default void return
// is needed
static bool isVoidableType(Type type) {
  if (TupleType *tt = type->getAs<TupleType>()) {
    return tt->getFields().empty();
  } else
    return false;
}

SILGen::SILGen(Function &F, FuncExpr *FE)
  : F(F), B(new (F) BasicBlock(&F), F),
    Cleanups(*this), ArgumentScope(Cleanups.getCleanupsDepth()),
    hasVoidReturn(isVoidableType(FE->getResultType(F.getContext()))) {

  emitProlog(FE);
}

/// SILGen destructor - called when the entire AST has been visited.  This
/// handles "falling off the end of the function" logic.
SILGen::~SILGen() {
  // If the end of the function isn't reachable (e.g. it ended in an explicit
  // return), then we're done.
  if (!B.hasValidInsertionPoint())
    return;
  
  // Clean up arguments.
  Cleanups.endScope(ArgumentScope);

  // If we have an unterminated block, it is either an implicit return of an
  // empty tuple, or a dynamically unreachable location.
  if (hasVoidReturn) {
    auto EmptyTuple = B.createTuple(SILLocation(),
                                    TupleType::getEmpty(F.getContext()),
                                    ArrayRef<Value>());
    B.createReturn(SILLocation(), EmptyTuple);
  } else {
    B.createUnreachable();
  }
}

Function *Function::constructSIL(FuncExpr *FE) {
  Function *C = new Function(FE->getASTContext());

  SILGen(*C, FE).visit(FE->getBody());

  C->verify();
  return C;
}


