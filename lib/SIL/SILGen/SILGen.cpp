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
#include "swift/SIL/SILArgument.h"
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

SILGenFunction::SILGenFunction(SILGenModule &SGM, SILFunction &F,
                               bool hasVoidReturn)
  : SGM(SGM), F(F), B(new (F.getModule()) SILBasicBlock(&F), F),
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
    SILValue emptyTuple = emitEmptyTuple(SILLocation());
    Cleanups.emitReturnAndCleanups(SILLocation(), emptyTuple);
  } else {
    B.createUnreachable();
  }
}

//===--------------------------------------------------------------------===//
// SILGenModule Class implementation
//===--------------------------------------------------------------------===//

SILGenModule::SILGenModule(SILModule &M)
  : M(M), Types(M.Types), TopLevelSGF(nullptr) {
  TopLevelSGF = new SILGenFunction(*this, *M.toplevel,
                                   /*hasVoidReturn=*/true);
}

SILGenModule::~SILGenModule() {
  delete TopLevelSGF;
  DEBUG(llvm::dbgs() << "lowered toplevel sil:\n";
        M.toplevel->print(llvm::dbgs()));
  M.toplevel->verify();
}

SILType SILGenModule::getConstantType(SILConstant constant) {
  return Types.getConstantType(constant);
}

void SILGenModule::visitFuncDecl(FuncDecl *fd) {
  emitFunction(fd, fd->getBody());
}

template<typename T>
SILFunction *SILGenModule::preEmitFunction(SILConstant constant, T *astNode) {
  assert(!M.hasFunction(constant) &&
         "already generated function for constant!");
  
  SILType constantType = getConstantType(constant);
  
  DEBUG(llvm::dbgs() << "lowering ";
        constant.print(llvm::dbgs());
        llvm::dbgs() << " : $";
        constantType.print(llvm::dbgs());
        llvm::dbgs() << '\n';
        if (astNode) {
          astNode->print(llvm::dbgs());
          llvm::dbgs() << '\n';
        });
  
  return new (M) SILFunction(M, constantType);
}

void SILGenModule::postEmitFunction(SILConstant constant,
                                    SILFunction *F) {

  DEBUG(llvm::dbgs() << "lowered sil:\n";
        F->print(llvm::dbgs()));
  F->verify();
  M.functions[constant] = F;
}

SILFunction *SILGenModule::emitFunction(SILConstant::Loc decl, FuncExpr *fe) {
  // Ignore prototypes.
  if (fe->getBody() == nullptr) return nullptr;
  
  SILConstant constant(decl);
  SILFunction *f = preEmitFunction(constant, fe);
  bool hasVoidReturn = isVoidableType(fe->getResultType(f->getContext()));
  SILGenFunction(*this, *f, hasVoidReturn).emitFunction(fe);
  postEmitFunction(constant, f);

  // If the function is a standalone function and is curried, emit the thunks
  // for the intermediate curry levels.
  // FIXME: It might make more sense to do this lazily and emit curry thunks
  // with internal linkage in IRGen.
  
  // FIXME: Should we emit thunks for getters and setters? IRGen doesn't know
  // how to mangle them currently.
  ValueDecl *vd = decl.dyn_cast<ValueDecl*>();
  FuncDecl *fd = dyn_cast_or_null<FuncDecl>(vd);
  if (fd && fd->isGetterOrSetter())
    return f;
  
  // FIXME: Thunks for instance methods.
  if (fd && fd->isInstanceMember())
    return f;
  
  // FIXME: Curry thunks for generic functions don't work right yet, so skip
  // emitting thunks for generic functions for now.
  for (AnyFunctionType *ft = fe->getType()->getAs<AnyFunctionType>();
       ft;
       ft = ft->getResult()->getAs<AnyFunctionType>()) {
    if (ft->is<PolymorphicFunctionType>())
      return f;
  }
  
  // Generate the curry thunks.
  unsigned level = constant.uncurryLevel;
  while (level-- > 0) {
    SILConstant curryConstant = constant.atUncurryLevel(level);
    
    emitCurryThunk(curryConstant, constant, fe);
    constant = curryConstant;
  }
  
  return f;
}

void SILGenModule::emitCurryThunk(SILConstant entryPoint,
                                  SILConstant nextEntryPoint,
                                  FuncExpr *fe) {
  SILFunction *f = preEmitFunction(entryPoint, fe);
  bool hasVoidReturn = isVoidableType(fe->getResultType(f->getContext()));
  SILGenFunction(*this, *f, hasVoidReturn)
    .emitCurryThunk(fe, entryPoint, nextEntryPoint);
  postEmitFunction(entryPoint, f);
}

void SILGenModule::addGlobalVariable(VarDecl *global) {
  M.globals.insert(global);
}

SILFunction *SILGenModule::emitConstructor(ConstructorDecl *decl) {
  SILConstant constant(decl);
  SILFunction *f = preEmitFunction(constant, decl);

  if (decl->getImplicitThisDecl()->getType()->getClassOrBoundGenericClass()) {
    // Class constructors have separate entry points for allocation and
    // initialization.
    SILGenFunction(*this, *f, /*hasVoidReturn=*/true)
      .emitClassConstructorAllocator(decl);
    postEmitFunction(constant, f);
    
    SILConstant initConstant(decl, SILConstant::Kind::Initializer);
    SILFunction *initF = preEmitFunction(initConstant, decl);
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

SILFunction *SILGenModule::emitClosure(ClosureExpr *ce) {
  SILConstant constant(ce);
  SILFunction *f = preEmitFunction(constant, ce);
  SILGenFunction(*this, *f, /*hasVoidReturn=*/false).emitClosure(ce);
  postEmitFunction(constant, f);
  
  return f;
}

SILFunction *SILGenModule::emitDestructor(ClassDecl *cd,
                                       DestructorDecl /*nullable*/ *dd) {
  SILConstant constant(cd, SILConstant::Kind::Destructor);
  
  SILFunction *f = preEmitFunction(constant, dd);
  SILGenFunction(*this, *f, /*hasVoidReturn=*/true).emitDestructor(cd, dd);
  postEmitFunction(constant, f);
  
  return f;
}

void SILGenModule::visitPatternBindingDecl(PatternBindingDecl *pd) {
  // Emit initializers for variables in top-level code.
  // FIXME: Global initialization order?!
  if (TopLevelSGF) {
    if (!TopLevelSGF->B.hasValidInsertionPoint())
      return;
    
    TopLevelSGF->visit(pd);
  }
  
  // FIXME: generate accessor functions for global variables
}

void SILGenModule::visitVarDecl(VarDecl *vd) {
  if (!vd->isProperty())
    addGlobalVariable(vd);
}

//===--------------------------------------------------------------------===//
// SILModule::constructSIL method implementation
//===--------------------------------------------------------------------===//


SILModule *SILModule::constructSIL(TranslationUnit *tu, unsigned startElem) {
  SILModule *m = new SILModule(tu->getASTContext());
  SILGenModule sgm(*m);
  for (Decl *D : llvm::makeArrayRef(tu->Decls).slice(startElem))
    sgm.visit(D);
  
  // Emit external definitions from Clang modules.
  // FIXME: O(n^2), since the same Clang module gets seen through multiple TUs
  for (auto mod : tu->getASTContext().LoadedClangModules) {
    for (auto &def : mod->getExternalDefinitions()) {
      switch (def.getStage()) {
        case ExternalDefinition::NameBound:
          llvm_unreachable("external definition not type-checked");
          
        case ExternalDefinition::TypeChecked:
          // FIXME: We should emit this definition only if it's actually needed.
          sgm.emitExternalDefinition(def.getDecl());
          break;
      }
    }
  }

  return m;
}

SILModule *swift::performSILGeneration(TranslationUnit *tu,
                                       unsigned startElem) {
  return SILModule::constructSIL(tu, startElem);
}
