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
#include "swift/SIL/Mangle.h"
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
  : SGM(SGM), F(F), B(new (F.getModule()) SILBasicBlock(&F)),
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
  
  SILFunction *toplevel = emitTopLevelFunction();
  TopLevelSGF = new SILGenFunction(*this, *toplevel,
                                   /*hasVoidReturn=*/true);
}

SILGenModule::~SILGenModule() {
  SILFunction *toplevel = &TopLevelSGF->getFunction();
  delete TopLevelSGF;
  DEBUG(llvm::dbgs() << "lowered toplevel sil:\n";
        toplevel->print(llvm::dbgs()));
  toplevel->verify();
  M.verify();
}

SILFunction *SILGenModule::emitTopLevelFunction() {
  ASTContext &C = M.getASTContext();
  Type topLevelType = FunctionType::get(TupleType::getEmpty(C),
                                        TupleType::getEmpty(C), C);
  SILType loweredType = getLoweredType(topLevelType);
  return new (M) SILFunction(M, SILLinkage::Internal,
                             "top_level_code", loweredType);
}

SILType SILGenModule::getConstantType(SILConstant constant) {
  return Types.getConstantType(constant);
}

SILLinkage SILGenModule::getConstantLinkage(SILConstant constant) {
  /// Anonymous functions always have internal linkage.
  if (!constant.hasDecl())
    return SILLinkage::Internal;
    
  ValueDecl *d = constant.getDecl();
  DeclContext *dc = d->getDeclContext();
  while (!dc->isModuleContext()) {
    if (dc->isLocalContext())
      return SILLinkage::Internal;
    dc = dc->getParent();
  }
  
  if (isa<ClangModule>(dc) &&
      (isa<ConstructorDecl>(d) ||
       isa<SubscriptDecl>(d) ||
       (isa<VarDecl>(d) && cast<VarDecl>(d)->isProperty())))
    return SILLinkage::ClangThunk;
  
  return SILLinkage::External;
}

SILFunction *SILGenModule::getFunction(SILConstant constant) {
  auto found = emittedFunctions.find(constant);
  if (found != emittedFunctions.end())
    return found->second;
  
  SILType constantType = getConstantType(constant);
  SILLinkage linkage = getConstantLinkage(constant);
  
  SILFunction *F = new (M) SILFunction(M, linkage, "", constantType);
  mangleConstant(constant, F);
  emittedFunctions[constant] = F;

  return F;
}

void SILGenModule::visitFuncDecl(FuncDecl *fd) {
  emitFunction(fd, fd->getBody());
}

template<typename T>
SILFunction *SILGenModule::preEmitFunction(SILConstant constant, T *astNode) {
  SILFunction *f = getFunction(constant);
  
  assert(f->empty() && "already emitted function?!");
  
  DEBUG(llvm::dbgs() << "lowering ";
        f->printName(llvm::dbgs());
        llvm::dbgs() << " : $";
        f->getLoweredType().print(llvm::dbgs());
        llvm::dbgs() << '\n';
        if (astNode) {
          astNode->print(llvm::dbgs());
          llvm::dbgs() << '\n';
        });
  
  return f;
}

void SILGenModule::postEmitFunction(SILConstant constant,
                                    SILFunction *F) {
  assert(!F->isExternalDeclaration() && "did not emit any function body?!");
  DEBUG(llvm::dbgs() << "lowered sil:\n";
        F->print(llvm::dbgs()));
  F->verify();
}

void SILGenModule::emitFunction(SILConstant::Loc decl, FuncExpr *fe) {
  // Ignore prototypes.
  if (fe->getBody() == nullptr) return;
  
  SILConstant constant(decl);
  SILFunction *f = preEmitFunction(constant, fe);
  bool hasVoidReturn = isVoidableType(fe->getResultType(f->getASTContext()));
  SILGenFunction(*this, *f, hasVoidReturn).emitFunction(fe);
  postEmitFunction(constant, f);

  // If the function is a standalone function and is curried, emit the thunks
  // for the intermediate curry levels.
  // FIXME: It might make more sense to do this lazily and emit curry thunks
  // with internal linkage.
  
  // Getters and setters can't be referenced uncurried, so skip thunking them.
  ValueDecl *vd = decl.dyn_cast<ValueDecl*>();
  FuncDecl *fd = dyn_cast_or_null<FuncDecl>(vd);
  if (fd && fd->isGetterOrSetter())
    return;
  
  // FIXME: Thunks for instance methods.
  if (fd && fd->isInstanceMember())
    return;
  
  // FIXME: Curry thunks for generic functions don't work right yet, so skip
  // emitting thunks for generic functions for now.
  for (AnyFunctionType *ft = fe->getType()->getAs<AnyFunctionType>();
       ft;
       ft = ft->getResult()->getAs<AnyFunctionType>()) {
    if (ft->is<PolymorphicFunctionType>())
      return;
  }
  
  // Generate the curry thunks.
  unsigned level = constant.uncurryLevel;
  while (level-- > 0) {
    SILConstant curryConstant = constant.atUncurryLevel(level);
    
    emitCurryThunk(curryConstant, constant, fe);
    constant = curryConstant;
  }
}

void SILGenModule::emitCurryThunk(SILConstant entryPoint,
                                  SILConstant nextEntryPoint,
                                  FuncExpr *fe) {
  SILFunction *f = preEmitFunction(entryPoint, fe);
  bool hasVoidReturn = isVoidableType(fe->getResultType(f->getASTContext()));
  SILGenFunction(*this, *f, hasVoidReturn)
    .emitCurryThunk(fe, entryPoint, nextEntryPoint);
  postEmitFunction(entryPoint, f);
}

void SILGenModule::addGlobalVariable(VarDecl *global) {
  M.globals.insert(global);
}

void SILGenModule::emitConstructor(ConstructorDecl *decl) {
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
}

void SILGenModule::emitClosure(PipeClosureExpr *ce) {
  SILConstant constant(ce);
  SILFunction *f = preEmitFunction(constant, ce);
  bool hasVoidReturn = isVoidableType(ce->getResultType());
  SILGenFunction(*this, *f, hasVoidReturn).emitClosure(ce);
  postEmitFunction(constant, f);
}

void SILGenModule::emitClosure(ClosureExpr *ce) {
  SILConstant constant(ce);
  SILFunction *f = preEmitFunction(constant, ce);
  SILGenFunction(*this, *f, /*hasVoidReturn=*/false).emitClosure(ce);
  postEmitFunction(constant, f);
}

void SILGenModule::emitDestructor(ClassDecl *cd,
                                  DestructorDecl /*nullable*/ *dd) {
  // Emit the destroying destructor.
  SILConstant destroyer(cd, SILConstant::Kind::Destroyer);
  SILFunction *f = preEmitFunction(destroyer, dd);
  SILGenFunction(*this, *f, /*hasVoidReturn=*/true).emitDestructor(cd, dd);
  postEmitFunction(destroyer, f);
}

void SILGenModule::emitObjCMethodThunk(FuncDecl *method) {
  SILConstant thunk(method, SILConstant::ConstructAtNaturalUncurryLevel,
                    /*isObjC*/ true);
                    
  SILFunction *f = preEmitFunction(thunk, method->getBody());
  SILGenFunction(*this, *f, false).emitObjCMethodThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::emitObjCPropertyMethodThunks(VarDecl *prop) {
  SILConstant getter(prop, SILConstant::Kind::Getter,
                     SILConstant::ConstructAtNaturalUncurryLevel,
                     /*isObjC*/ true);
                     
  SILFunction *f = preEmitFunction(getter, prop);
  SILGenFunction(*this, *f, false).emitObjCPropertyGetter(getter);
  postEmitFunction(getter, f);

  if (!prop->isSettable())
    return;

  SILConstant setter(prop, SILConstant::Kind::Setter,
                     SILConstant::ConstructAtNaturalUncurryLevel,
                     /*isObjC*/ true);

  f = preEmitFunction(setter, prop);
  SILGenFunction(*this, *f, false).emitObjCPropertySetter(setter);
  postEmitFunction(setter, f);
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
  
  // Emit external definitions used by this translation unit.
  for (auto def : tu->getASTContext().ExternalDefinitions) {
    sgm.emitExternalDefinition(def);
  }
  
  return m;
}

SILModule *swift::performSILGeneration(TranslationUnit *tu,
                                       unsigned startElem) {
  return SILModule::constructSIL(tu, startElem);
}
