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
// SILGenFunction Class implementation
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

SILGenFunction::SILGenFunction(SILGenModule &SGM, Function &F, FuncExpr *FE)
  : SGM(SGM), F(F), B(new (F.getModule()) BasicBlock(&F), F),
    Cleanups(*this),
    hasVoidReturn(isVoidableType(FE->getResultType(F.getContext()))) {

  emitProlog(FE);
}

TupleInst *SILBuilder::createEmptyTuple(SILLocation Loc) {
  return createTuple(Loc, TupleType::getEmpty(F.getContext()),
                     ArrayRef<Value>());
}

/// SILGenFunction destructor - called after the entire function's AST has been
/// visited.  This handles "falling off the end of the function" logic.
SILGenFunction::~SILGenFunction() {
  // If the end of the function isn't reachable (e.g. it ended in an explicit
  // return), then we're done.
  if (!B.hasValidInsertionPoint())
    return;
  
  // If we have an unterminated block, it is either an implicit return of an
  // empty tuple, or a dynamically unreachable location.
  if (hasVoidReturn) {
    Value emptyTuple = B.createEmptyTuple(SILLocation());
    Cleanups.emitReturnAndCleanups(SILLocation(), emptyTuple);
  } else {
    B.createUnreachable();
  }
}

//===--------------------------------------------------------------------===//
// SILGenModule Class implementation
//===--------------------------------------------------------------------===//

SILGenModule::SILGenModule(SILModule &M)
  : M(M), Types(*this) {
}

void SILGenModule::visitFuncDecl(FuncDecl *FD) {
  FuncExpr *FE = FD->getBody();
  
  // Ignore function prototypes.
  if (FE->getBody() == nullptr) return;
  
  assert(M.functions.find(FD) == M.functions.end() &&
         "already generated function for decl!");

  Function *C = new (M) Function(M);
  SILGenFunction(*this, *C, FE).visit(FE->getBody());
  
  C->verify();
  M.functions[FD] = C;
}

//===--------------------------------------------------------------------===//
// SILModule::constructSIL method implementation
//===--------------------------------------------------------------------===//


SILModule *SILModule::constructSIL(TranslationUnit *tu) {
  SILModule *m = new SILModule(tu->getASTContext());
  SILGenModule sgm(*m);
  for (Decl *D : tu->Decls)
    sgm.visit(D);
  return m;
}