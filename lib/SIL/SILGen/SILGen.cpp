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
#include "llvm/ADT/Optional.h"
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

SILGenFunction::SILGenFunction(SILGenModule &SGM, Function &F,
                               bool hasVoidReturn)
  : SGM(SGM), F(F), B(new (F.getModule()) BasicBlock(&F), F),
    Cleanups(*this),
    hasVoidReturn(hasVoidReturn) {
}

SILGenFunction::SILGenFunction(SILGenModule &SGM, Function &F, FuncExpr *FE)
  : SILGenFunction(SGM, F,
          /*hasVoidReturn=*/isVoidableType(FE->getResultType(F.getContext()))) {

  emitProlog(FE, FE->getBodyParamPatterns());
}

SILGenFunction::SILGenFunction(SILGenModule &SGM, Function &F, ClosureExpr *CE)
  : SILGenFunction(SGM, F, /*hasVoidReturn=*/false) {

  emitProlog(CE, CE->getParamPatterns());
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

SILGenModule::SILGenModule(SILModule &M, bool verbose)
  : M(M), Types(*this), TopLevelSGF(nullptr), Verbose(verbose) {
  if (M.toplevel)
    TopLevelSGF = new SILGenFunction(*this, *M.toplevel,
                                     /*hasVoidReturn=*/true);
}

SILGenModule::~SILGenModule() {
  delete TopLevelSGF;
}

void SILGenModule::visitFuncDecl(FuncDecl *fd) {
  emitFunction(fd, fd->getBody());
}

Function *SILGenModule::emitFunction(SILConstant::Loc decl, FuncExpr *fe) {
  // Ignore prototypes.
  if (fe->getBody() == nullptr) return nullptr;
  
  SILConstant constant(decl);
  assert(!M.hasFunction(constant) &&
         "already generated function for decl!");

  if (Verbose) {
    constant.dump();
    fe->dump();
  }
  
  Function *f = new (M) Function(M);
  SILGenFunction(*this, *f, fe).visit(fe->getBody());
  
  if (Verbose) {
    f->dump();
  }
  f->verify();
  M.functions[constant] = f;
  
  return f;
}

Function *SILGenModule::emitClosure(ClosureExpr *ce) {
  SILConstant constant(ce);
  assert(!M.hasFunction(constant) &&
         "already generated function for closure!");
  
  if (Verbose) {
    constant.dump();
    ce->dump();
  }
  
  Function *f = new (M) Function(M);
  SILGenFunction(*this, *f, ce).emitClosureBody(ce->getBody());

  if (Verbose) {
    f->dump();
  }
  f->verify();
  M.functions[constant] = f;
  
  return f;
}

void SILGenModule::visitPatternBindingDecl(PatternBindingDecl *pd) {
  // FIXME: generate accessor functions for global variables
}

//===--------------------------------------------------------------------===//
// SILModule::constructSIL method implementation
//===--------------------------------------------------------------------===//


SILModule *SILModule::constructSIL(TranslationUnit *tu, bool verbose) {
  bool hasTopLevel = true;
  switch (tu->Kind) {
  case TranslationUnit::Library:
    hasTopLevel = false;
    break;
  case TranslationUnit::Main:
  case TranslationUnit::Repl:
    hasTopLevel = true;
    break;
  }
  SILModule *m = new SILModule(tu->getASTContext(), hasTopLevel);
  SILGenModule sgm(*m, verbose);
  for (Decl *D : tu->Decls)
    sgm.visit(D);
  return m;
}

//===--------------------------------------------------------------------===//
// SILGenType Class implementation
//===--------------------------------------------------------------------===//

SILGenType::SILGenType(SILGenModule &SGM) : SGM(SGM) {}
SILGenType::~SILGenType() {}