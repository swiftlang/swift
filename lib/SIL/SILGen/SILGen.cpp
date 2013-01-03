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
#include "swift/SIL/BBArgument.h"
using namespace swift;
using namespace Lowering;

//===--------------------------------------------------------------------===//
// SILGenFunction Class implementation
//===--------------------------------------------------------------------===//

// TODO: more accurately port the result schema logic from
// IRGenFunction::emitEpilogue to handle all cases where a default void return
// is needed
static bool isVoidableType(Type type) {
  return type->isEqual(type->getASTContext().TheEmptyTupleType);
}

SILGenFunction::SILGenFunction(SILGenModule &SGM, Function &F,
                               bool hasVoidReturn)
  : SGM(SGM), F(F), B(new (F.getModule()) BasicBlock(&F), F),
    Cleanups(*this),
    hasVoidReturn(hasVoidReturn),
    epilogBB(nullptr) {
}

SILGenFunction::SILGenFunction(SILGenModule &SGM, Function &F, FuncExpr *FE)
  : SILGenFunction(SGM, F,
          /*hasVoidReturn=*/isVoidableType(FE->getResultType(F.getContext())))
{
  emitProlog(FE, FE->getBodyParamPatterns(), FE->getResultType(F.getContext()));
}

SILGenFunction::SILGenFunction(SILGenModule &SGM, Function &F, ClosureExpr *CE)
  : SILGenFunction(SGM, F, /*hasVoidReturn=*/false) {

  emitProlog(CE, CE->getParamPatterns(),
             CE->getType()->castTo<FunctionType>()->getResult());
}

SILGenFunction::SILGenFunction(SILGenModule &SGM, Function &F,
                               ClassDecl *CD, DestructorDecl *DD)
  : SILGenFunction(SGM, F, /*hasVoidReturn=*/true) {
  emitDestructorProlog(CD, DD);
}

SILGenFunction::SILGenFunction(SILGenModule &SGM, Function &F,
                               ConstructorDecl *CD)
  // Although a constructor implicitly returns 'this', from the body's
  // perspective it looks like a void function, hence the hasVoidReturn flag.
  : SILGenFunction(SGM, F, /*hasVoidReturn=*/true)
{
  // In addition to the declared arguments, the constructor implicitly takes
  // the metatype as its first argument, like a static function.
  Type metatype = CD->getType()->castTo<AnyFunctionType>()->getInput();
  new (F.getModule()) BBArgument(getLoweredType(metatype), F.begin());
                                
  emitProlog(CD->getArguments(), CD->getImplicitThisDecl()->getType());
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
    assert(!epilogBB && "epilog bb not terminated?!");
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

SILType SILGenModule::getConstantType(SILConstant constant) {
  return Types.getConstantType(constant);
}

void SILGenModule::visitFuncDecl(FuncDecl *fd) {
  emitFunction(fd, fd->getBody());
}

template<typename T>
Function *SILGenModule::preEmitFunction(SILConstant constant,
                                        T *astNode) {
  assert(!M.hasFunction(constant) &&
         "already generated function for constant!");
  
  if (Verbose) {
    constant.print(llvm::errs());
    llvm::errs() << " : $";
    getConstantType(constant).dump();
    if (astNode)
      astNode->dump();
  }

  return new (M) Function(M, getConstantType(constant));
}

void SILGenModule::postEmitFunction(SILConstant constant,
                                    Function *F) {
  if (Verbose) {
    F->dump();
  }
  F->verify();
  M.functions[constant] = F;
}

Function *SILGenModule::emitFunction(SILConstant::Loc decl, FuncExpr *fe) {
  // Ignore prototypes.
  if (fe->getBody() == nullptr) return nullptr;
  
  SILConstant constant(decl);
  Function *f = preEmitFunction(constant, fe);
  SILGenFunction(*this, *f, fe).visit(fe->getBody());
  postEmitFunction(constant, f);
  
  return f;
}

Function *SILGenModule::emitConstructor(ConstructorDecl *decl) {
  // Ignore prototypes.
  // FIXME: generate default constructor, which appears in the AST as a
  // prototype
  if (decl->getBody() == nullptr) return nullptr;
  
  SILConstant constant(decl);
  Function *f = preEmitFunction(constant, decl);
  SILGenFunction(*this, *f, decl).emitConstructorBody(decl);
  postEmitFunction(constant, f);
  
  return f;
}

Function *SILGenModule::emitClosure(ClosureExpr *ce) {
  SILConstant constant(ce);
  Function *f = preEmitFunction(constant, ce);
  SILGenFunction(*this, *f, ce).emitClosureBody(ce->getBody());
  postEmitFunction(constant, f);
  
  return f;
}

Function *SILGenModule::emitDestructor(ClassDecl *cd,
                                       DestructorDecl /*nullable*/ *dd) {
  SILConstant constant(cd, SILConstant::Destructor);
  
  Function *f = preEmitFunction(constant, dd);
  SILGenFunction(*this, *f, cd, dd).emitDestructorBody(cd, dd);
  postEmitFunction(constant, f);
  
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

SILGenType::SILGenType(SILGenModule &SGM, NominalTypeDecl *theType)
  : SGM(SGM), theType(theType), explicitDestructor(nullptr) {}

SILGenType::~SILGenType() {
  // Emit the destructor for a class type.
  if (ClassDecl *theClass = dyn_cast<ClassDecl>(theType)) {
    SGM.emitDestructor(theClass, explicitDestructor);
  } else {
    assert(!explicitDestructor && "destructor in non-class type?!");
  }
}
