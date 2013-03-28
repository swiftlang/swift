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
#include "swift/Subsystems.h"
#include "llvm/Support/Debug.h"
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

SILGenModule::SILGenModule(SILModule &M)
  : M(M), Types(*this), TopLevelSGF(nullptr) {
  if (M.toplevel) {
    TopLevelSGF = new SILGenFunction(*this, *M.toplevel,
                                     /*hasVoidReturn=*/true);
    // Ensure that we generate type info for () -> ().
    getLoweredType(M.toplevel->getLoweredType().getSwiftType());
  }
}

SILGenModule::~SILGenModule() {
  delete TopLevelSGF;
  if (M.toplevel)
    M.toplevel->verify();
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
  
  DEBUG(llvm::dbgs() << "lowering ";
        constant.print(llvm::dbgs());
        llvm::dbgs() << " : $";
        getConstantType(constant).print(llvm::dbgs());
        llvm::dbgs() << '\n';
        if (astNode) {
          astNode->print(llvm::dbgs());
          llvm::dbgs() << '\n';
        });
  
  return new (M) Function(M, getConstantType(constant));
}

void SILGenModule::postEmitFunction(SILConstant constant,
                                    Function *F) {

  DEBUG(llvm::dbgs() << "lowered sil:\n";
        F->print(llvm::dbgs()));
  F->verify();
  M.functions[constant] = F;
}

Function *SILGenModule::emitFunction(SILConstant::Loc decl, FuncExpr *fe) {
  // Ignore prototypes.
  if (fe->getBody() == nullptr) return nullptr;
  
  SILConstant constant(decl);
  Function *f = preEmitFunction(constant, fe);
  bool hasVoidReturn = isVoidableType(fe->getResultType(f->getContext()));
  SILGenFunction(*this, *f, hasVoidReturn).emitFunction(fe);
  postEmitFunction(constant, f);
  
  return f;
}

void SILGenModule::addGlobalVariable(VarDecl *global) {
  M.globals.insert(global);
}

Function *SILGenModule::emitConstructor(ConstructorDecl *decl) {
  // Ignore prototypes.
  // FIXME: generate default constructor, which appears in the AST as a
  // prototype
  if (decl->getBody() == nullptr) return nullptr;

  SILConstant constant(decl);
  Function *f = preEmitFunction(constant, decl);
  
  if (decl->getImplicitThisDecl()->getType()->hasReferenceSemantics()) {
    // Class constructors have separate entry points for allocation and
    // initialization.
    SILGenFunction(*this, *f, /*hasVoidReturn=*/true)
      .emitClassConstructorAllocator(decl);
    postEmitFunction(constant, f);
    
    SILConstant initConstant(decl, SILConstant::Kind::Initializer);
    Function *initF = preEmitFunction(initConstant, decl);
    SILGenFunction(*this, *initF, /*hasVoidReturn=*/true)
      .emitClassConstructorInitializer(decl);
    postEmitFunction(initConstant, initF);
  } else {
    // Struct constructors do everything in a single function.
    SILGenFunction(*this, *f, /*hasVoidReturn=*/true)
      .emitValueConstructor(decl);
    postEmitFunction(constant, f);
  }
  
  return f;
}

Function *SILGenModule::emitClosure(ClosureExpr *ce) {
  SILConstant constant(ce);
  Function *f = preEmitFunction(constant, ce);
  SILGenFunction(*this, *f, /*hasVoidReturn=*/false).emitClosure(ce);
  postEmitFunction(constant, f);
  
  return f;
}

Function *SILGenModule::emitDestructor(ClassDecl *cd,
                                       DestructorDecl /*nullable*/ *dd) {
  SILConstant constant(cd, SILConstant::Kind::Destructor);
  
  Function *f = preEmitFunction(constant, dd);
  SILGenFunction(*this, *f, /*hasVoidReturn=*/true).emitDestructor(cd, dd);
  postEmitFunction(constant, f);
  
  return f;
}

void SILGenModule::visitPatternBindingDecl(PatternBindingDecl *pd) {
  // Emit initializers for variables in top-level code.
  if (TopLevelSGF) {
    if (!TopLevelSGF->B.hasValidInsertionPoint())
      return;
    
    TopLevelSGF->visit(pd);
  }
  
  // FIXME: generate accessor functions for global variables
}

//===--------------------------------------------------------------------===//
// SILModule::constructSIL method implementation
//===--------------------------------------------------------------------===//


SILModule *SILModule::constructSIL(TranslationUnit *tu, unsigned startElem) {
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
  SILGenModule sgm(*m);
  for (Decl *D : llvm::makeArrayRef(tu->Decls).slice(startElem))
    sgm.visit(D);
  return m;
}

SILModule *swift::performSILGeneration(TranslationUnit *tu,
                                       unsigned startElem) {
  return SILModule::constructSIL(tu, startElem);
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
