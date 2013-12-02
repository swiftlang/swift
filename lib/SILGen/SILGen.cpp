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
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Mangle.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/Subsystems.h"
#include "llvm/Support/Debug.h"
using namespace swift;
using namespace Lowering;

//===--------------------------------------------------------------------===//
// SILGenFunction Class implementation
//===--------------------------------------------------------------------===//

SILGenFunction::SILGenFunction(SILGenModule &SGM, SILFunction &F)
  : SGM(SGM), F(F), LastInsnWithoutScope(0),
    B(createBasicBlock(), &InsertedInstrs),
    ReturnDest(CleanupLocation::getCleanupLocation(F.getLocation())),
    CurrentSILLoc(F.getLocation()), Cleanups(*this)
{
}

/// SILGenFunction destructor - called after the entire function's AST has been
/// visited.  This handles "falling off the end of the function" logic.
SILGenFunction::~SILGenFunction() {
  // If the end of the function isn't terminated, we screwed up somewhere.
  assert(!B.hasValidInsertionPoint() &&
         "SILGenFunction did not terminate function?!");
}

//===--------------------------------------------------------------------===//
// SILGenModule Class implementation
//===--------------------------------------------------------------------===//

SILGenModule::SILGenModule(SILModule &M, Module *SM)
  : M(M), Types(M.Types), SwiftModule(SM), TopLevelSGF(nullptr) {

  RegularLocation TopLevelLoc = RegularLocation::getModuleLocation();
  SILFunction *toplevel = emitTopLevelFunction(TopLevelLoc);

  // Assign a debug scope pointing into the void to the top level function.
  toplevel->setDebugScope(new (M) SILDebugScope(TopLevelLoc));

  TopLevelSGF = new SILGenFunction(*this, *toplevel);
  TopLevelSGF->prepareEpilog(Type(),
                             CleanupLocation::getModuleCleanupLocation());
}

SILGenModule::~SILGenModule() {
  TopLevelSGF->emitEpilog(RegularLocation::getModuleLocation(),
                          /* AutoGen */ true);
  SILFunction *toplevel = &TopLevelSGF->getFunction();
  delete TopLevelSGF;
  DEBUG(llvm::dbgs() << "lowered toplevel sil:\n";
        toplevel->print(llvm::dbgs()));
  toplevel->verify();
  M.verify();
}

static SILDeclRef getBridgingFn(Optional<SILDeclRef> &cacheSlot,
                                 SILGenModule &SGM,
                                 StringRef moduleName,
                                 StringRef functionName,
                                 std::initializer_list<SILType> inputTypes,
                                 SILType outputType) {
  SILDeclRef fn = cacheSlot.cache([&] {
    Optional<UnqualifiedLookup> lookup
      = UnqualifiedLookup::forModuleAndName(SGM.M.getASTContext(),
                                            moduleName,
                                            functionName);
    // Check that we can find the module and function.
    // FIXME: Can we recover more gracefully?
    if (!lookup) {
      SGM.diagnose(SourceLoc(), diag::bridging_module_missing,
                   moduleName, functionName);
      exit(1);
    }
    if (lookup->Results.size() == 0) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_missing,
                   moduleName, functionName);
      exit(1);
    }
    // FIXME: Resolve overloads.
    if (lookup->Results.size() > 1) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_overloaded,
                   moduleName, functionName);
      exit(1);
    }
    auto &result = lookup->Results[0];
    // Check that the bridging function is actually a function.
    if (!result.hasValueDecl()) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_not_function,
                   moduleName, functionName);
      exit(1);
    }
    FuncDecl *fd = dyn_cast<FuncDecl>(result.getValueDecl());
    if (!fd) {
      SGM.diagnose(result.getValueDecl()->getLoc(),
                   diag::bridging_function_not_function,
                   moduleName, functionName);
      exit(1);
    }

    assert(fd->hasType() && "bridging functions must be type-checked");

    // Check that the function takes the expected arguments and returns the
    // expected result type.
    SILDeclRef c(fd);
    auto funcInfo = SGM.getConstantType(c).castTo<SILFunctionType>();
    
    if (funcInfo->getParameters().size() != inputTypes.size()
        || !std::equal(funcInfo->getParameterSILTypes().begin(),
                       funcInfo->getParameterSILTypes().end(),
                       inputTypes.begin())) {
      SGM.diagnose(fd->getLoc(), diag::bridging_function_not_correct_type,
                   moduleName, functionName);
      exit(1);
    }
    
    if (funcInfo->getResult().getSILType() != outputType) {
      SGM.diagnose(fd->getLoc(), diag::bridging_function_not_correct_type,
                   moduleName, functionName);
      exit(1);
    }
    
    return c;
  });
  
  DEBUG(llvm::dbgs() << "bridging function "
          << moduleName << '.' << functionName
          << " mapped to ";
        fn.print(llvm::dbgs()));
  
  return fn;
}

static SILType getInOutStringTy(SILGenModule &SGM) {
  return SGM.getLoweredType(LValueType::get(SGM.Types.getStringType(),
                                            LValueType::Qual::DefaultForType,
                                            SGM.M.getASTContext()));
}

static SILType getNSStringTy(SILGenModule &SGM) {
  return SGM.getLoweredType(SGM.Types.getNSStringType());
}

static SILType getBoolTy(SILGenModule &SGM) {
  return SGM.getLoweredType(SGM.Types.getBoolType());
}

static SILType getObjCBoolTy(SILGenModule &SGM) {
  return SGM.getLoweredType(SGM.Types.getObjCBoolType());
}

SILDeclRef SILGenModule::getNSStringToStringFn() {
  return getBridgingFn(NSStringToStringFn, *this,
                       "Foundation", "convertNSStringToString",
                       {getNSStringTy(*this), getInOutStringTy(*this)},
                       Types.getEmptyTupleType());
}

SILDeclRef SILGenModule::getStringToNSStringFn() {
  return getBridgingFn(StringToNSStringFn, *this,
                       "Foundation", "convertStringToNSString",
                       {getInOutStringTy(*this)},
                       getNSStringTy(*this));
}

SILDeclRef SILGenModule::getBoolToObjCBoolFn() {
  return getBridgingFn(BoolToObjCBoolFn, *this,
                       "ObjectiveC", "convertBoolToObjCBool",
                       {getBoolTy(*this)},
                       getObjCBoolTy(*this));
}

SILDeclRef SILGenModule::getObjCBoolToBoolFn() {
  return getBridgingFn(ObjCBoolToBoolFn, *this,
                       "ObjectiveC", "convertObjCBoolToBool",
                       {getObjCBoolTy(*this)},
                       getBoolTy(*this));
}

SILFunction *SILGenModule::emitTopLevelFunction(SILLocation Loc) {
  ASTContext &C = M.getASTContext();
  auto extInfo = FunctionType::ExtInfo().withIsThin(true);
  Type topLevelType = FunctionType::get(TupleType::getEmpty(C),
                                        TupleType::getEmpty(C),
                                        extInfo, C);
  auto loweredType = getLoweredType(topLevelType).castTo<SILFunctionType>();
  return new (M) SILFunction(M, SILLinkage::Internal,
                             "top_level_code", loweredType, Loc);
}

SILType SILGenModule::getConstantType(SILDeclRef constant) {
  return Types.getConstantType(constant);
}

SILLinkage SILGenModule::getConstantLinkage(SILDeclRef constant) {
  // Anonymous functions always have internal linkage.
  if (!constant.hasDecl())
    return SILLinkage::Internal;
  
  // Function-local declarations always have internal linkage.
  ValueDecl *d = constant.getDecl();
  DeclContext *dc = d->getDeclContext();
  while (!dc->isModuleContext()) {
    if (dc->isLocalContext())
      return SILLinkage::Internal;
    dc = dc->getParent();
  }
  
  // Currying and calling convention thunks have thunk linkage.
  if (constant.isCurried || constant.isForeignThunk())
    return SILLinkage::Thunk;
  
  // Declarations imported from Clang modules have thunk linkage.
  // FIXME: They shouldn't.
  if(isa<ClangModule>(dc) &&
     (isa<ConstructorDecl>(d) ||
      isa<SubscriptDecl>(d) ||
      isa<EnumElementDecl>(d) ||
      (isa<VarDecl>(d) && cast<VarDecl>(d)->isComputed()) ||
      (isa<FuncDecl>(d) && isa<EnumDecl>(d->getDeclContext()))))
    return SILLinkage::Thunk;
  
  // Otherwise, we have external linkage.
  // FIXME: access control
  return SILLinkage::External;
}

SILFunction *SILGenModule::getFunction(SILDeclRef constant) {
  auto found = emittedFunctions.find(constant);
  if (found != emittedFunctions.end())
    return found->second;
  
  auto constantType = getConstantType(constant).castTo<SILFunctionType>();
  SILLinkage linkage = getConstantLinkage(constant);

  IsTransparent_t IsTrans = constant.isTransparent()?
                              IsTransparent : IsNotTransparent;

  auto *F = new (M) SILFunction(M, linkage, "", constantType, Nothing, IsTrans);
  
  ValueDecl *VD = nullptr;
  if (constant.hasDecl())
    VD = constant.getDecl();

  F->setDeclContext(VD);

  mangleConstant(constant, F);
  emittedFunctions[constant] = F;

  return F;
}

bool SILGenModule::hasFunction(SILDeclRef constant) {
  return emittedFunctions.count(constant);
}

void SILGenModule::visitFuncDecl(FuncDecl *fd) {
  emitFunction(fd);
}

template<typename T>
SILFunction *SILGenModule::preEmitFunction(SILDeclRef constant, T *astNode,
                                           SILLocation Loc) {
  // By default, use the astNode to create the location.
  if (Loc.isNull())
    Loc = RegularLocation(astNode);

  SILFunction *f = getFunction(constant);
  assert(f->empty() && "already emitted function?!");

  // Create a debug scope for the function using astNode as source location.
  f->setDebugScope(new (M) SILDebugScope(RegularLocation(astNode)));

  f->setLocation(Loc);

  f->setDeclContext(astNode);

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

void SILGenModule::postEmitFunction(SILDeclRef constant,
                                    SILFunction *F) {
  assert(!F->isExternalDeclaration() && "did not emit any function body?!");
  DEBUG(llvm::dbgs() << "lowered sil:\n";
        F->print(llvm::dbgs()));
  F->verify();
}

void SILGenModule::emitAbstractFuncDecl(AbstractFunctionDecl *AFD) {
  // If this is a function at global scope, it may close over a global variable.
  // If we're emitting top-level code, then emit a "mark_function_escape" that
  // lists the captured global variables so that definite initialization can
  // reason about this escape point.
  if (!AFD->getDeclContext()->isLocalContext() &&
      TopLevelSGF && TopLevelSGF->B.hasValidInsertionPoint()) {
    SmallVector<SILValue, 4> Captures;
    
    for (auto Capture : AFD->getCaptureInfo().getCaptures()) {
      auto It = TopLevelSGF->VarLocs.find(Capture);
      if (It == TopLevelSGF->VarLocs.end()) continue;
      Captures.push_back(It->second.address);
    }
    
    if (!Captures.empty())
      TopLevelSGF->B.createMarkFunctionEscape(AFD, Captures);
  }
}

void SILGenModule::emitFunction(FuncDecl *fd) {
  SILDeclRef::Loc decl = fd;

  // Emit any default argument generators.
  {
    auto patterns = fd->getArgParamPatterns();
    if (fd->getDeclContext()->isTypeContext())
      patterns = patterns.slice(1);
    emitDefaultArgGenerators(decl, patterns);
  }

  emitAbstractFuncDecl(fd);
  
  // Emit the actual body of the function to a new SILFunction.  Ignore
  // prototypes.
  if (fd->getBody()) {
    PrettyStackTraceDecl stackTrace("emitting SIL for", fd);

    SILDeclRef constant(decl);
    SILFunction *f = preEmitFunction(constant, fd, fd);
    SILGenFunction(*this, *f).emitFunction(fd);
    postEmitFunction(constant, f);
  }
}

void SILGenModule::emitCurryThunk(SILDeclRef entryPoint,
                                  SILDeclRef nextEntryPoint,
                                  FuncDecl *fd) {
  SILFunction *f = preEmitFunction(entryPoint, fd, fd);
  SILGenFunction(*this, *f)
    .emitCurryThunk(fd, entryPoint, nextEntryPoint);
  postEmitFunction(entryPoint, f);
}

void SILGenModule::emitForeignThunk(SILDeclRef thunk) {
  assert(!thunk.isForeign && "native-to-foreign thunk not implemented");
  SILFunction *f = preEmitFunction(thunk, thunk.getDecl(), thunk.getDecl());
  SILGenFunction(*this, *f)
    .emitForeignThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::addGlobalVariable(VarDecl *global) {
  M.globals.insert(global);
}

void SILGenModule::emitConstructor(ConstructorDecl *decl) {
  // Emit any default argument getter functions.
  emitDefaultArgGenerators(decl, decl->getBodyParams());

  emitAbstractFuncDecl(decl);

  SILDeclRef constant(decl);
  SILFunction *f = preEmitFunction(constant, decl, decl);

  if (decl->getImplicitSelfDecl()->getType()->getClassOrBoundGenericClass()) {
    // Class constructors have separate entry points for allocation and
    // initialization.
    SILGenFunction(*this, *f)
      .emitClassConstructorAllocator(decl);
    postEmitFunction(constant, f);

    // If this constructor was imported, we don't need the initializing
    // constructor to be emitted.
    if (!decl->hasClangNode()) {
      SILDeclRef initConstant(decl, SILDeclRef::Kind::Initializer);
      SILFunction *initF = preEmitFunction(initConstant, decl, decl);
      SILGenFunction(*this, *initF)
        .emitClassConstructorInitializer(decl);
      postEmitFunction(initConstant, initF);
    }
  } else {
    // Struct and enum constructors do everything in a single function.
    SILGenFunction(*this, *f).emitValueConstructor(decl);
    postEmitFunction(constant, f);
  }  
}

void SILGenModule::emitEnumConstructor(EnumElementDecl *decl) {
  SILDeclRef constant(decl);
  SILFunction *f = preEmitFunction(constant, decl, decl);
  SILGenFunction(*this, *f).emitEnumConstructor(decl);
  postEmitFunction(constant, f);
}

void SILGenModule::emitClosure(ClosureExpr *ce) {
  SILDeclRef constant(ce);
  SILFunction *f = preEmitFunction(constant, ce, ce);
  SILGenFunction(*this, *f).emitClosure(ce);
  postEmitFunction(constant, f);
}

void SILGenModule::emitClosure(AutoClosureExpr *ce) {
  SILDeclRef constant(ce);
  SILFunction *f = preEmitFunction(constant, ce, ce);
  SILGenFunction(*this, *f).emitClosure(ce);
  postEmitFunction(constant, f);
}

void SILGenModule::emitDestructor(ClassDecl *cd,
                                  DestructorDecl /*nullable*/ *dd) {
  if (dd) emitAbstractFuncDecl(dd);

  // Emit the destroying destructor.
  SILDeclRef destroyer(cd, SILDeclRef::Kind::Destroyer);
  SILFunction *f = preEmitFunction(destroyer, dd, dd);
  SILGenFunction(*this, *f).emitDestructor(cd, dd);
  postEmitFunction(destroyer, f);
}

void SILGenModule::emitDefaultArgGenerator(SILDeclRef constant, Expr *arg) {
  SILFunction *f = preEmitFunction(constant, arg, arg);
  SILGenFunction(*this, *f)
    .emitGeneratorFunction(constant, arg);
  postEmitFunction(constant, f);
}

SILFunction *SILGenModule::emitLazyGlobalInitializer(StringRef funcName,
                                                 PatternBindingDecl *binding) {
  ASTContext &C = M.getASTContext();
  Type initType = FunctionType::get(TupleType::getEmpty(C),
                                    TupleType::getEmpty(C),
                                    FunctionType::ExtInfo()
                                      .withIsThin(true),
                                    C);
  auto initSILType = getLoweredType(initType).castTo<SILFunctionType>();
  
  auto *f = new (M) SILFunction(M, SILLinkage::Internal, funcName,
                                initSILType, binding, IsNotTransparent);
  // TODO: Debug scope? Decl context?
  
  SILGenFunction(*this, *f)
    .emitLazyGlobalInitializer(binding);
  
  f->verify();
  
  return f;
}

void SILGenModule::emitGlobalAccessor(VarDecl *global,
                                      FuncDecl *builtinOnceDecl,
                                      SILGlobalVariable *onceToken,
                                      SILFunction *onceFunc) {
  SILDeclRef accessor(global, SILDeclRef::Kind::GlobalAccessor);
  SILFunction *f = preEmitFunction(accessor, global, global);
  SILGenFunction(*this, *f)
    .emitGlobalAccessor(global, builtinOnceDecl, onceToken, onceFunc);
  postEmitFunction(accessor, f);
}
  
void SILGenModule::emitDefaultArgGenerators(SILDeclRef::Loc decl,
                                            ArrayRef<Pattern*> patterns) {
  unsigned index = 0;
  for (auto pattern : patterns) {
    pattern = pattern->getSemanticsProvidingPattern();
    auto tuplePattern = dyn_cast<TuplePattern>(pattern);
    if (!tuplePattern) {
      ++index;
      continue;
    }

    for (auto &elt : tuplePattern->getFields()) {
      if (auto handle = elt.getInit()) {
        emitDefaultArgGenerator(SILDeclRef::getDefaultArgGenerator(decl,index),
                                handle->getExpr());
      }
      ++index;
    }
  }
}

void SILGenModule::emitObjCMethodThunk(FuncDecl *method) {
  SILDeclRef thunk(method, SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*isObjC*/ true);
  
  // Don't emit the thunk if it already exists.
  if (hasFunction(thunk))
    return;
  SILFunction *f = preEmitFunction(thunk, method, method);
  SILGenFunction(*this, *f).emitObjCMethodThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::emitObjCPropertyMethodThunks(VarDecl *prop) {
  SILDeclRef getter(prop, SILDeclRef::Kind::Getter,
                     SILDeclRef::ConstructAtNaturalUncurryLevel,
                     /*isObjC*/ true);
                     
  // Don't emit the thunks if they already exist.
  if (hasFunction(getter))
    return;

  RegularLocation ThunkBodyLoc(prop);
  ThunkBodyLoc.markAutoGenerated();
  SILFunction *f = preEmitFunction(getter, prop, ThunkBodyLoc);
  SILGenFunction(*this, *f).emitObjCPropertyGetter(getter);
  postEmitFunction(getter, f);

  if (!prop->isSettable())
    return;

  // FIXME: Add proper location.
  SILDeclRef setter(prop, SILDeclRef::Kind::Setter,
                     SILDeclRef::ConstructAtNaturalUncurryLevel,
                     /*isObjC*/ true);

  f = preEmitFunction(setter, prop, ThunkBodyLoc);
  SILGenFunction(*this, *f).emitObjCPropertySetter(setter);
  postEmitFunction(setter, f);
}

void SILGenModule::emitObjCSubscriptMethodThunks(SubscriptDecl *subscript) {
  SILDeclRef getter(subscript, SILDeclRef::Kind::Getter,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*isObjC*/ true);
                     
  // Don't emit the thunks if they already exist.
  if (hasFunction(getter))
    return;

  RegularLocation ThunkBodyLoc(subscript);
  ThunkBodyLoc.markAutoGenerated();
  SILFunction *f = preEmitFunction(getter, subscript, ThunkBodyLoc);
  SILGenFunction(*this, *f).emitObjCSubscriptGetter(getter);
  postEmitFunction(getter, f);

  if (!subscript->isSettable())
    return;

  // FIXME: Add proper location.
  SILDeclRef setter(subscript, SILDeclRef::Kind::Setter,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*isObjC*/ true);

  f = preEmitFunction(setter, subscript, ThunkBodyLoc);
  SILGenFunction(*this, *f).emitObjCSubscriptSetter(setter);
  postEmitFunction(setter, f);
}

void SILGenModule::emitObjCConstructorThunk(ConstructorDecl *constructor) {
  SILDeclRef thunk(constructor,
                   SILDeclRef::Kind::Initializer,
                   SILDeclRef::ConstructAtNaturalUncurryLevel,
                   /*isObjC*/ true);

  // Don't emit the thunk if it already exists.
  if (hasFunction(thunk))
    return;
  SILFunction *f = preEmitFunction(thunk, constructor, constructor);
  SILGenFunction(*this, *f).emitObjCMethodThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::visitPatternBindingDecl(PatternBindingDecl *pd) {
  // Emit initializers for variables in top-level code.
  emitGlobalInitialization(pd);
}

void SILGenModule::visitVarDecl(VarDecl *vd) {
  if (!vd->isComputed())
    addGlobalVariable(vd);
}

//===--------------------------------------------------------------------===//
// SILModule::constructSIL method implementation
//===--------------------------------------------------------------------===//

std::unique_ptr<SILModule> SILModule::constructSIL(TranslationUnit *tu,
                                                   SourceFile *sf,
                                                   unsigned startElem) {
  std::unique_ptr<SILModule> m(new SILModule(tu));
  SILGenModule sgm(*m, tu);

  if (sf) {
    for (Decl *D : llvm::makeArrayRef(sf->Decls).slice(startElem))
      sgm.visit(D);
  } else {
    assert(startElem == 0 && "no explicit source file");
    for (auto nextSF : tu->getSourceFiles()) {
      if (nextSF->ASTStage != SourceFile::TypeChecked)
        continue;
      for (Decl *D : llvm::makeArrayRef(nextSF->Decls).slice(startElem))
        sgm.visit(D);
    }
  }
    
  // Emit external definitions used by this translation unit.
  for (auto def : tu->getASTContext().ExternalDefinitions) {
    sgm.emitExternalDefinition(def);
  }

  return m;
}

std::unique_ptr<SILModule> swift::performSILGeneration(TranslationUnit *tu) {
  return SILModule::constructSIL(tu);
}

std::unique_ptr<SILModule> swift::performSILGeneration(SourceFile &sf,
                                       unsigned startElem) {
  return SILModule::constructSIL(&sf.TU, &sf, startElem);
}
