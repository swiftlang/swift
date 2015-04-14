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

#define DEBUG_TYPE "silgen"
#include "SILGenFunction.h"
#include "llvm/ADT/Optional.h"
#include "swift/Strings.h"
#include "swift/AST/AST.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/Subsystems.h"
#include "llvm/Support/Debug.h"
#include "ManagedValue.h"
using namespace swift;
using namespace Lowering;

//===--------------------------------------------------------------------===//
// SILGenModule Class implementation
//===--------------------------------------------------------------------===//

SILGenModule::SILGenModule(SILModule &M, Module *SM, bool makeModuleFragile)
  : M(M), Types(M.Types), SwiftModule(SM), TopLevelSGF(nullptr),
    Profiler(nullptr), makeModuleFragile(makeModuleFragile) {
}

SILGenModule::~SILGenModule() {
  assert(!TopLevelSGF && "active source file lowering!?");
  M.verify();
}

static SILDeclRef
getBridgingFn(Optional<SILDeclRef> &cacheSlot,
              SILGenModule &SGM,
              StringRef moduleName,
              StringRef functionName,
              Optional<std::initializer_list<Type>> inputTypes,
              Optional<Type> outputType) {
  // FIXME: the optionality of outputType and the presence of trustInputTypes
  // are hacks for cases where coming up with those types is complicated, i.e.,
  // when dealing with generic bridging functions.

  if (!cacheSlot) {
    ASTContext &ctx = SGM.M.getASTContext();
    Module *mod = ctx.getLoadedModule(ctx.getIdentifier(moduleName));
    if (!mod) {
      SGM.diagnose(SourceLoc(), diag::bridging_module_missing,
                   moduleName, functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }

    SmallVector<ValueDecl *, 2> decls;
    mod->lookupValue(/*accessPath=*/{}, ctx.getIdentifier(functionName),
                     NLKind::QualifiedLookup, decls);
    if (decls.empty()) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_missing,
                   moduleName, functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }
    if (decls.size() != 1) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_overloaded,
                   moduleName, functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }

    auto *fd = dyn_cast<FuncDecl>(decls.front());
    if (!fd) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_not_function,
                   moduleName, functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }

    assert(fd->hasType() && "bridging functions must be type-checked");

    // Check that the function takes the expected arguments and returns the
    // expected result type.
    SILDeclRef c(fd);
    auto funcInfo = SGM.getConstantType(c).castTo<SILFunctionType>();

    if (inputTypes) {
      auto toSILType = [&SGM](Type ty) { return SGM.getLoweredType(ty); };
      if (funcInfo->getParameters().size() != inputTypes->size()
          || !std::equal(funcInfo->getParameterSILTypes().begin(),
                         funcInfo->getParameterSILTypes().end(),
                         makeTransformIterator(inputTypes->begin(),
                                               toSILType))) {
        SGM.diagnose(fd->getLoc(), diag::bridging_function_not_correct_type,
                     moduleName, functionName);
        llvm::report_fatal_error("unable to set up the ObjC bridge!");
      }
    }

    if (outputType &&
        funcInfo->getResult().getSILType() != SGM.getLoweredType(*outputType)){
      SGM.diagnose(fd->getLoc(), diag::bridging_function_not_correct_type,
                   moduleName, functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }

    cacheSlot = c;
  }

  DEBUG(llvm::dbgs() << "bridging function "
          << moduleName << '.' << functionName
          << " mapped to ";
        cacheSlot->print(llvm::dbgs()));

  return *cacheSlot;
}

#define REQUIRED(X) { Types.get##X##Type() }
#define OPTIONAL(X) { OptionalType::get(Types.get##X##Type()) }
#define GENERIC(X) None

#define GET_BRIDGING_FN(Module, FromKind, FromTy, ToKind, ToTy) \
  SILDeclRef SILGenModule::get##FromTy##To##ToTy##Fn() { \
    return getBridgingFn(FromTy##To##ToTy##Fn, *this, \
                         Module, "_convert" #FromTy "To" #ToTy, \
                         FromKind(FromTy), \
                         ToKind(ToTy)); \
  }

GET_BRIDGING_FN(OBJC_MODULE_NAME, REQUIRED, Bool, REQUIRED, ObjCBool)
GET_BRIDGING_FN(OBJC_MODULE_NAME, REQUIRED, ObjCBool, REQUIRED, Bool)
GET_BRIDGING_FN(FOUNDATION_MODULE_NAME, OPTIONAL, NSError, REQUIRED, ErrorType)
GET_BRIDGING_FN(FOUNDATION_MODULE_NAME, REQUIRED, String, REQUIRED, NSString)
GET_BRIDGING_FN(FOUNDATION_MODULE_NAME, OPTIONAL, NSString, REQUIRED, String)
GET_BRIDGING_FN(FOUNDATION_MODULE_NAME, GENERIC, Array, REQUIRED, NSArray)
GET_BRIDGING_FN(FOUNDATION_MODULE_NAME, OPTIONAL, NSArray, GENERIC, Array)
GET_BRIDGING_FN(FOUNDATION_MODULE_NAME, GENERIC, Set, REQUIRED, NSSet)
GET_BRIDGING_FN(FOUNDATION_MODULE_NAME, OPTIONAL, NSSet, GENERIC, Set)
GET_BRIDGING_FN(FOUNDATION_MODULE_NAME,
                GENERIC, Dictionary,
                REQUIRED, NSDictionary)
GET_BRIDGING_FN(FOUNDATION_MODULE_NAME,
                OPTIONAL, NSDictionary,
                GENERIC, Dictionary)

#undef GET_BRIDGING_FN
#undef REQURIED
#undef OPTIONAL
#undef GENERIC

SILFunction *SILGenModule::emitTopLevelFunction(SILLocation Loc) {
  ASTContext &C = M.getASTContext();
  auto extInfo = SILFunctionType::ExtInfo()
    .withRepresentation(SILFunctionType::Representation::CFunctionPointer);

  auto findStdlibDecl = [&](StringRef name) -> ValueDecl* {
    if (!getASTContext().getStdlibModule())
      return nullptr;
    SmallVector<ValueDecl*, 1> lookupBuffer;
    getASTContext().getStdlibModule()->lookupValue({},
                                       getASTContext().getIdentifier(name),
                                       NLKind::QualifiedLookup,
                                       lookupBuffer);
    if (lookupBuffer.size() == 1)
      return lookupBuffer[0];
    return nullptr;
  };

  // Use standard library types if we have them; otherwise, fall back to
  // builtins.
  CanType Int32Ty;
  if (auto Int32Decl = dyn_cast_or_null<TypeDecl>(findStdlibDecl("Int32"))) {
    Int32Ty = Int32Decl->getDeclaredType()->getCanonicalType();
  } else {
    Int32Ty = CanType(BuiltinIntegerType::get(32, C));
  }

  CanType PtrPtrInt8Ty = C.TheRawPointerType;
  if (auto PointerDecl = C.getUnsafeMutablePointerDecl()) {
    if (auto Int8Decl = cast<TypeDecl>(findStdlibDecl("Int8"))) {
      Type PointerInt8Ty = BoundGenericType::get(PointerDecl,
                                                 nullptr,
                                                 Int8Decl->getDeclaredType());
      PtrPtrInt8Ty = BoundGenericType::get(PointerDecl,
                                           nullptr,
                                           PointerInt8Ty)
        ->getCanonicalType();
    }
  }

  SILParameterInfo params[] = {
    SILParameterInfo(Int32Ty, ParameterConvention::Direct_Unowned),
    SILParameterInfo(PtrPtrInt8Ty, ParameterConvention::Direct_Unowned),
  };

  CanSILFunctionType topLevelType = SILFunctionType::get(nullptr, extInfo,
                                   ParameterConvention::Direct_Unowned,
                                   params,
                                   SILResultInfo(Int32Ty,
                                                 ResultConvention::Unowned),
                                   None,
                                   C);

  return SILFunction::create(M, SILLinkage::Public,
                             SWIFT_ENTRY_POINT_FUNCTION, topLevelType, nullptr,
                             Loc, IsBare, IsNotTransparent, IsNotFragile,
                             IsNotThunk, SILFunction::NotRelevant);
}

SILType SILGenModule::getConstantType(SILDeclRef constant) {
  return Types.getConstantType(constant);
}

SILFunction *SILGenModule::getFunction(SILDeclRef constant,
                                       ForDefinition_t forDefinition) {
  auto found = emittedFunctions.find(constant);
  if (found != emittedFunctions.end()) {
    SILFunction *F = found->second;
    if (forDefinition) {
      // In all the cases where getConstantLinkage returns something
      // different for ForDefinition, it returns an available-externally
      // linkage.
      if (isAvailableExternally(F->getLinkage())) {
        F->setLinkage(constant.getLinkage(ForDefinition));
      }
      if (makeModuleFragile) {
        F->setFragile(IsFragile);
      }
    }
    return F;
  }

  // Note: Do not provide any SILLocation. You can set it afterwards.
  auto *F = M.getOrCreateFunction((Decl*)nullptr, constant, forDefinition);

  assert(F && "SILFunction should have been defined");

  if (makeModuleFragile) {
    SILLinkage linkage = constant.getLinkage(forDefinition);
    if (linkage != SILLinkage::PublicExternal) {
      F->setFragile(IsFragile);
    }
  }

  emittedFunctions[constant] = F;

  return F;
}

bool SILGenModule::hasFunction(SILDeclRef constant) {
  return emittedFunctions.count(constant);
}

void SILGenModule::visitFuncDecl(FuncDecl *fd) {
  ProfilerRAII Profiler(*this, fd);
  emitFunction(fd);
}

template<typename T>
SILFunction *SILGenModule::preEmitFunction(SILDeclRef constant, T *astNode,
                                           SILLocation Loc) {
  // By default, use the astNode to create the location.
  if (Loc.isNull())
    Loc = RegularLocation(astNode);

  SILFunction *f = getFunction(constant, ForDefinition);
  assert(f->empty() && "already emitted function?!");

  f->setContextGenericParams(
                         Types.getConstantInfo(constant).ContextGenericParams);

  // Create a debug scope for the function using astNode as source location.
  f->setDebugScope(new (M) SILDebugScope(RegularLocation(astNode), *f));

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
  // Emit any default argument generators.
  {
    auto patterns = AFD->getBodyParamPatterns();
    if (AFD->getDeclContext()->isTypeContext())
      patterns = patterns.slice(1);
    emitDefaultArgGenerators(AFD, patterns);
  }

  // If this is a function at global scope, it may close over a global variable.
  // If we're emitting top-level code, then emit a "mark_function_escape" that
  // lists the captured global variables so that definite initialization can
  // reason about this escape point.
  if (!AFD->getDeclContext()->isLocalContext() &&
      TopLevelSGF && TopLevelSGF->B.hasValidInsertionPoint()) {
    SmallVector<SILValue, 4> Captures;

    for (auto capture : AFD->getCaptureInfo().getCaptures()) {
      // Decls captured by value don't escape.
      if (auto *CaptureVD = dyn_cast<VarDecl>(capture.getDecl()))
        if (CaptureVD->isLet())
          continue;

      auto It = TopLevelSGF->VarLocs.find(capture.getDecl());
      if (It == TopLevelSGF->VarLocs.end())
        continue;

      Captures.push_back(It->second.value);
    }

    if (!Captures.empty())
      TopLevelSGF->B.createMarkFunctionEscape(AFD, Captures);
  }
}

void SILGenModule::emitFunction(FuncDecl *fd) {
  SILDeclRef::Loc decl = fd;

  emitAbstractFuncDecl(fd);

  // Emit the actual body of the function to a new SILFunction.  Ignore
  // prototypes and methods whose bodies weren't synthesized by now.
  if (fd->getBody(/*canSynthesize=*/false)) {
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
  PrettyStackTraceSILFunction X("silgen emitCurryThunk", f);

  SILGenFunction(*this, *f)
    .emitCurryThunk(fd, entryPoint, nextEntryPoint);
  postEmitFunction(entryPoint, f);
}

void SILGenModule::emitForeignToNativeThunk(SILDeclRef thunk) {
  assert(!thunk.isForeign && "foreign-to-native thunks only");
  SILFunction *f = preEmitFunction(thunk, thunk.getDecl(), thunk.getDecl());
  PrettyStackTraceSILFunction X("silgen emitForeignToNativeThunk", f);
  SILGenFunction(*this, *f).emitForeignToNativeThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::emitNativeToForeignThunk(SILDeclRef thunk) {
  assert(thunk.isForeign && "native-to-foreign thunks only");
  
  SILFunction *f;
  if (thunk.hasDecl())
    f = preEmitFunction(thunk, thunk.getDecl(), thunk.getDecl());
  else
    f = preEmitFunction(thunk, thunk.getAbstractClosureExpr(),
                        thunk.getAbstractClosureExpr());
  PrettyStackTraceSILFunction X("silgen emitNativeToForeignThunk", f);
  f->setBare(IsBare);
  SILGenFunction(*this, *f).emitNativeToForeignThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::addGlobalVariable(VarDecl *global) {
  // We create SILGlobalVariable here.
  getSILGlobalVariable(global, ForDefinition);
}

void SILGenModule::emitConstructor(ConstructorDecl *decl) {
  // FIXME: Handle 'self' like any other argument here.
  // Emit any default argument getter functions.
  emitAbstractFuncDecl(decl);

  // We never emit constructors in protocols.
  if (isa<ProtocolDecl>(decl->getDeclContext()))
    return;

  // Always-unavailable imported constructors are factory methods
  // that have been imported as constructors and then hidden by an
  // imported init method.
  if (decl->hasClangNode() &&
      decl->getAttrs().isUnavailable(decl->getASTContext()))
    return;

  SILDeclRef constant(decl);
  SILFunction *f = preEmitFunction(constant, decl, decl);
  PrettyStackTraceSILFunction X("silgen emitConstructor", f);

  if (decl->getImplicitSelfDecl()->getType()->getInOutObjectType()
        ->getClassOrBoundGenericClass()) {
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
      PrettyStackTraceSILFunction X("silgen constructor initializer", initF);
      SILGenFunction(*this, *initF).emitClassConstructorInitializer(decl);
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
  PrettyStackTraceSILFunction X("silgen enum constructor", f);
  SILGenFunction(*this, *f).emitEnumConstructor(decl);
  postEmitFunction(constant, f);
}

SILFunction *SILGenModule::emitClosure(AbstractClosureExpr *ce) {
  SILDeclRef constant(ce);
  SILFunction *f = preEmitFunction(constant, ce, ce);
  PrettyStackTraceSILFunction X("silgen closureexpr", f);
  SILGenFunction(*this, *f).emitClosure(ce);
  postEmitFunction(constant, f);
  return f;
}

/// Determine whether the given class requires a separate instance
/// variable initialization method.
static bool requiresIVarInitialization(SILGenModule &SGM, ClassDecl *cd) {
  if (!cd->requiresStoredPropertyInits())
    return false;

  for (Decl *member : cd->getMembers()) {
    auto pbd = dyn_cast<PatternBindingDecl>(member);
    if (!pbd) continue;

    for (auto entry : pbd->getPatternList())
      if (entry.Init)
        return true;
  }

  return false;
}

/// Determine whether the given class has any instance variables that
/// need to be destroyed.
static bool requiresIVarDestruction(SILGenModule &SGM, ClassDecl *cd) {
  for (Decl *member : cd->getMembers()) {
    VarDecl *vd = dyn_cast<VarDecl>(member);
    if (!vd || !vd->hasStorage()) continue;

    const TypeLowering &ti = SGM.Types.getTypeLowering(vd->getType());
    if (!ti.isTrivial())
      return true;
  }

  return false;
}

/// TODO: This needs a better name.
void SILGenModule::emitObjCAllocatorDestructor(ClassDecl *cd,
                                               DestructorDecl *dd) {
  // Emit the native deallocating destructor for -dealloc.
  {
    SILDeclRef dealloc(dd, SILDeclRef::Kind::Deallocator);
    SILFunction *f = preEmitFunction(dealloc, dd, dd);
    PrettyStackTraceSILFunction X("silgen emitDestructor -dealloc", f);
    SILGenFunction(*this, *f).emitObjCDestructor(dealloc);
    postEmitFunction(dealloc, f);
  }

  // Emit the Objective-C -dealloc entry point if it has
  // something to do beyond messaging the superclass's -dealloc.
  if (dd->getBody()->getNumElements() != 0)
    emitObjCDestructorThunk(dd);

  // Emit the ivar initializer, if needed.
  if (requiresIVarInitialization(*this, cd)) {
    SILDeclRef ivarInitializer(cd, SILDeclRef::Kind::IVarInitializer,
                               SILDeclRef::ConstructAtBestResilienceExpansion,
                               SILDeclRef::ConstructAtNaturalUncurryLevel,
                               /*isForeign=*/true);
    SILFunction *f = preEmitFunction(ivarInitializer, dd, dd);
    PrettyStackTraceSILFunction X("silgen emitDestructor ivar initializer", f);
    SILGenFunction(*this, *f).emitIVarInitializer(ivarInitializer);
    postEmitFunction(ivarInitializer, f);
  }

  // Emit the ivar destroyer, if needed.
  if (requiresIVarDestruction(*this, cd)) {
    SILDeclRef ivarDestroyer(cd, SILDeclRef::Kind::IVarDestroyer,
                             SILDeclRef::ConstructAtBestResilienceExpansion,
                             SILDeclRef::ConstructAtNaturalUncurryLevel,
                             /*isForeign=*/true);
    SILFunction *f = preEmitFunction(ivarDestroyer, dd, dd);
    PrettyStackTraceSILFunction X("silgen emitDestructor ivar destroyer", f);
    SILGenFunction(*this, *f).emitIVarDestroyer(ivarDestroyer);
    postEmitFunction(ivarDestroyer, f);
  }
}

void SILGenModule::emitDestructor(ClassDecl *cd, DestructorDecl *dd) {
  emitAbstractFuncDecl(dd);

  // If the class would use the Objective-C allocator, only emit -dealloc.
  if (usesObjCAllocator(cd)) {
    emitObjCAllocatorDestructor(cd, dd);
    return;
  }

  // Emit the destroying destructor.
  {
    SILDeclRef destroyer(dd, SILDeclRef::Kind::Destroyer);
    SILFunction *f = preEmitFunction(destroyer, dd, dd);
    PrettyStackTraceSILFunction X("silgen emitDestroyingDestructor", f);
    SILGenFunction(*this, *f).emitDestroyingDestructor(dd);
    postEmitFunction(destroyer, f);
  }

  // Emit the deallocating destructor.
  {
    SILDeclRef deallocator(dd, SILDeclRef::Kind::Deallocator);
    SILFunction *f = preEmitFunction(deallocator, dd, dd);
    PrettyStackTraceSILFunction X("silgen emitDeallocatingDestructor", f);
    SILGenFunction(*this, *f).emitDeallocatingDestructor(dd);
    postEmitFunction(deallocator, f);
  }
}

void SILGenModule::emitDefaultArgGenerator(SILDeclRef constant, Expr *arg) {
  SILFunction *f = preEmitFunction(constant, arg, arg);
  PrettyStackTraceSILFunction X("silgen emitDefaultArgGenerator ", f);
  SILGenFunction(*this, *f).emitGeneratorFunction(constant, arg);
  postEmitFunction(constant, f);
}

SILFunction *SILGenModule::emitLazyGlobalInitializer(StringRef funcName,
                                                 PatternBindingDecl *binding,
                                                     unsigned pbdEntry) {
  ASTContext &C = M.getASTContext();
  Type initType = FunctionType::get(
                    TupleType::getEmpty(C), TupleType::getEmpty(C),
                    FunctionType::ExtInfo()
                      .withRepresentation(FunctionType::Representation::Thin));
  auto initSILType = getLoweredType(initType).castTo<SILFunctionType>();

  auto *f =
    SILFunction::create(M, SILLinkage::Private, funcName,
                        initSILType, nullptr, SILLocation(binding),
                        IsNotBare, IsNotTransparent,
                        makeModuleFragile ? IsFragile : IsNotFragile);
  f->setDebugScope(new (M)
                   SILDebugScope(RegularLocation(binding->getInit(pbdEntry)),
                                 *f));
  f->setLocation(binding);

  SILGenFunction(*this, *f).emitLazyGlobalInitializer(binding);

  f->verify();

  return f;
}

void SILGenModule::emitGlobalAccessor(VarDecl *global,
                                      SILGlobalVariable *onceToken,
                                      SILFunction *onceFunc) {
  SILDeclRef accessor(global, SILDeclRef::Kind::GlobalAccessor);
  SILFunction *f = preEmitFunction(accessor, global, global);
  PrettyStackTraceSILFunction X("silgen emitGlobalAccessor", f);
  SILGenFunction(*this, *f)
    .emitGlobalAccessor(global, onceToken, onceFunc);
  postEmitFunction(accessor, f);
}

void SILGenModule::emitGlobalGetter(VarDecl *global,
                                    SILGlobalVariable *onceToken,
                                    SILFunction *onceFunc) {
  SILDeclRef accessor(global, SILDeclRef::Kind::GlobalGetter);
  SILFunction *f = preEmitFunction(accessor, global, global);
  PrettyStackTraceSILFunction X("silgen emitGlobalGetter", f);
  SILGenFunction(*this, *f)
    .emitGlobalGetter(global, onceToken, onceFunc);
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

    for (auto &elt : tuplePattern->getElements()) {
      if (auto handle = elt.getInit()) {
        emitDefaultArgGenerator(SILDeclRef::getDefaultArgGenerator(decl,index),
                                handle->getExpr());
      }
      ++index;
    }
  }
}

void SILGenModule::emitObjCMethodThunk(FuncDecl *method) {
  SILDeclRef thunk(method,
                   SILDeclRef::ConstructAtBestResilienceExpansion,
                   SILDeclRef::ConstructAtNaturalUncurryLevel,
                   /*isObjC*/ true);

  // Don't emit the thunk if it already exists.
  if (hasFunction(thunk))
    return;
  SILFunction *f = preEmitFunction(thunk, method, method);
  PrettyStackTraceSILFunction X("silgen emitObjCMethodThunk", f);
  f->setBare(IsBare);
  SILGenFunction(*this, *f).emitNativeToForeignThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::emitObjCPropertyMethodThunks(AbstractStorageDecl *prop) {
  // If we don't actually need an entry point for the getter, do nothing.
  if (!prop->getGetter() || !requiresObjCMethodEntryPoint(prop->getGetter()))
    return;

  SILDeclRef getter(prop->getGetter(), SILDeclRef::Kind::Func,
                    SILDeclRef::ConstructAtBestResilienceExpansion,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*isObjC*/ true);

  // Don't emit the thunks if they already exist.
  if (hasFunction(getter))
    return;

  RegularLocation ThunkBodyLoc(prop);
  ThunkBodyLoc.markAutoGenerated();
  {
  SILFunction *f = preEmitFunction(getter, prop, ThunkBodyLoc);
  PrettyStackTraceSILFunction X("silgen objc property getter thunk", f);
  f->setBare(IsBare);
  SILGenFunction(*this, *f).emitNativeToForeignThunk(getter);
  postEmitFunction(getter, f);
  }

  if (!prop->isSettable(prop->getDeclContext()))
    return;

  // FIXME: Add proper location.
  SILDeclRef setter(prop->getSetter(), SILDeclRef::Kind::Func,
                    SILDeclRef::ConstructAtBestResilienceExpansion,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*isObjC*/ true);

  SILFunction *f = preEmitFunction(setter, prop, ThunkBodyLoc);
  PrettyStackTraceSILFunction X("silgen objc property setter thunk", f);
  f->setBare(IsBare);
  SILGenFunction(*this, *f).emitNativeToForeignThunk(setter);
  postEmitFunction(setter, f);
}

void SILGenModule::emitObjCConstructorThunk(ConstructorDecl *constructor) {
  SILDeclRef thunk(constructor,
                   SILDeclRef::Kind::Initializer,
                   SILDeclRef::ConstructAtBestResilienceExpansion,
                   SILDeclRef::ConstructAtNaturalUncurryLevel,
                   /*isObjC*/ true);

  // Don't emit the thunk if it already exists.
  if (hasFunction(thunk))
    return;
  SILFunction *f = preEmitFunction(thunk, constructor, constructor);
  PrettyStackTraceSILFunction X("silgen objc constructor thunk", f);
  f->setBare(IsBare);
  SILGenFunction(*this, *f).emitNativeToForeignThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::emitObjCDestructorThunk(DestructorDecl *destructor) {
  SILDeclRef thunk(destructor,
                   SILDeclRef::Kind::Deallocator,
                   SILDeclRef::ConstructAtBestResilienceExpansion,
                   SILDeclRef::ConstructAtNaturalUncurryLevel,
                   /*isObjC*/ true);

  // Don't emit the thunk if it already exists.
  if (hasFunction(thunk))
    return;
  SILFunction *f = preEmitFunction(thunk, destructor, destructor);
  PrettyStackTraceSILFunction X("silgen objc destructor thunk", f);
  f->setBare(IsBare);
  SILGenFunction(*this, *f).emitNativeToForeignThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::visitPatternBindingDecl(PatternBindingDecl *pd) {
  // If we're in a script mode context, emit the pattern binding as top-level
  // code.
  if (auto sf = dyn_cast<SourceFile>(pd->getDeclContext())) {
    if (sf->isScriptMode()) {
      assert(TopLevelSGF && "no top-level code for script mode?!");
      if (TopLevelSGF->B.hasValidInsertionPoint())
        TopLevelSGF->visit(pd);
      return;
    }
  }

  // Otherwise, emit the initializer for library global variables.
  for (unsigned i = 0, e = pd->getNumPatternEntries(); i != e; ++i)
    if (pd->getInit(i))
      emitGlobalInitialization(pd, i);
}

void SILGenModule::visitVarDecl(VarDecl *vd) {
  if (vd->hasStorage())
    addGlobalVariable(vd);
}

void emitTopLevelProlog(SILGenFunction &gen, SILLocation loc) {
  assert(gen.B.getInsertionBB() == gen.F.begin()
         && "not at entry point?!");

  SILBasicBlock *entry = gen.B.getInsertionBB();
  // Create the argc and argv arguments.
  auto &C = gen.getASTContext();
  auto FnTy = gen.F.getLoweredFunctionType();
  auto argc = new (gen.F.getModule()) SILArgument(
                                  entry, FnTy->getParameters()[0].getSILType());
  auto argv = new (gen.F.getModule()) SILArgument(
                                  entry, FnTy->getParameters()[1].getSILType());

  // If the standard library provides a _didEnterMain intrinsic, call it first
  // thing.
  if (auto didEnterMain = C.getDidEnterMain(nullptr)) {
    ManagedValue params[] = {
      ManagedValue::forUnmanaged(argc),
      ManagedValue::forUnmanaged(argv),
    };
    gen.emitApplyOfLibraryIntrinsic(loc, didEnterMain, {}, params,
                                    SGFContext());
  }
}

namespace {

/// An RAII class to scope source file codegen.
class SourceFileScope {
  SILGenModule &sgm;
  SourceFile *sf;
public:
  SourceFileScope(SILGenModule &sgm, SourceFile *sf) : sgm(sgm), sf(sf) {
    // If this is the script-mode file for the module, create a toplevel.
    if (sf->isScriptMode()) {
      assert(!sgm.TopLevelSGF && "already emitted toplevel?!");
      assert(!sgm.M.lookUpFunction(SWIFT_ENTRY_POINT_FUNCTION)
             && "already emitted toplevel?!");

      RegularLocation TopLevelLoc = RegularLocation::getModuleLocation();
      SILFunction *toplevel = sgm.emitTopLevelFunction(TopLevelLoc);

      // Assign a debug scope pointing into the void to the top level function.
      toplevel->setDebugScope(new (sgm.M) SILDebugScope(TopLevelLoc,*toplevel));

      sgm.TopLevelSGF = new SILGenFunction(sgm, *toplevel);
      sgm.TopLevelSGF->MagicFunctionName = sgm.SwiftModule->Name;
      sgm.TopLevelSGF->prepareEpilog(Type(), false,
                                 CleanupLocation::getModuleCleanupLocation());

      auto PrologueLoc = RegularLocation::getModuleLocation();
      PrologueLoc.markAsPrologue();
      emitTopLevelProlog(*sgm.TopLevelSGF, PrologueLoc);
    }
  }

  ~SourceFileScope() {
    if (sgm.TopLevelSGF) {
      // Write out the epilog.
      auto moduleLoc = RegularLocation::getModuleLocation();
      auto returnInfo
        = sgm.TopLevelSGF->emitEpilogBB(moduleLoc);
      auto returnLoc = returnInfo.second;

      // Signal a normal return to the OS by returning 0.
      auto &B = sgm.TopLevelSGF->B;
      auto &F = sgm.TopLevelSGF->F;
      auto &C = sgm.getASTContext();
      SILValue zero = B.createIntegerLiteral(moduleLoc,
                                      SILType::getBuiltinIntegerType(32, C), 0);
      if (zero.getType() != F.getLoweredFunctionType()->getResult().getSILType())
        zero = B.createStruct(moduleLoc,
                          F.getLoweredFunctionType()->getResult().getSILType(),
                          zero);
      sgm.TopLevelSGF->B.createReturn(returnLoc, zero);

      SILFunction *toplevel = &sgm.TopLevelSGF->getFunction();
      delete sgm.TopLevelSGF;

      DEBUG(llvm::dbgs() << "lowered toplevel sil:\n";
            toplevel->print(llvm::dbgs()));
      toplevel->verify();
      sgm.TopLevelSGF = nullptr;
    }

    // If the source file contains an artificial main, emit the implicit
    // toplevel code.
    if (auto mainClass = sf->getMainClass()) {
      assert(!sgm.M.lookUpFunction(SWIFT_ENTRY_POINT_FUNCTION)
             && "already emitted toplevel before main class?!");

      RegularLocation TopLevelLoc = RegularLocation::getModuleLocation();
      SILFunction *toplevel = sgm.emitTopLevelFunction(TopLevelLoc);

      // Assign a debug scope pointing into the void to the top level function.
      toplevel->setDebugScope(new (sgm.M) SILDebugScope(TopLevelLoc,*toplevel));

      SILGenFunction gen(sgm, *toplevel);
      emitTopLevelProlog(gen, mainClass);
      gen.emitArtificialTopLevel(mainClass);
    }
  }
};

} // end anonymous namespace

void SILGenModule::emitSourceFile(SourceFile *sf, unsigned startElem) {
  SourceFileScope scope(*this, sf);
  for (Decl *D : llvm::makeArrayRef(sf->Decls).slice(startElem))
    visit(D);
}

//===--------------------------------------------------------------------===//
// SILModule::constructSIL method implementation
//===--------------------------------------------------------------------===//

std::unique_ptr<SILModule>
SILModule::constructSIL(Module *mod, SILOptions &options, FileUnit *sf,
                        Optional<unsigned> startElem, bool makeModuleFragile,
                        bool isWholeModule) {
  const DeclContext *DC;
  if (startElem) {
    assert(sf && "cannot have a start element without a source file");
    // Because more decls may be added to the SourceFile, we can't assume
    // anything about the compilation context.
    DC = nullptr;
  } else if (sf) {
    DC = sf;
  } else {
    DC = mod;
  }

  std::unique_ptr<SILModule> m(new SILModule(mod, options, DC, isWholeModule));
  SILGenModule sgm(*m, mod, makeModuleFragile);

  if (sf) {
    if (auto *file = dyn_cast<SourceFile>(sf)) {
      sgm.emitSourceFile(file, startElem.getValueOr(0));
    } else if (auto *file = dyn_cast<SerializedASTFile>(sf)) {
      if (file->isSIB())
        m->getSILLoader()->getAllForModule(mod->getName(), file);
    }
  } else {
    for (auto file : mod->getFiles()) {
      auto nextSF = dyn_cast<SourceFile>(file);
      if (!nextSF || nextSF->ASTStage != SourceFile::TypeChecked)
        continue;
      sgm.emitSourceFile(nextSF, 0);
    }

    // Also make sure to process any intermediate files that may contain SIL
    bool hasSIB = std::any_of(mod->getFiles().begin(),
                              mod->getFiles().end(),
                              [](const FileUnit *File) -> bool {
      auto *SASTF = dyn_cast<SerializedASTFile>(File);
      return SASTF && SASTF->isSIB();
    });
    if (hasSIB)
      m->getSILLoader()->getAllForModule(mod->getName(), nullptr);
  }

  // Emit external definitions used by this module.
  for (size_t i = 0, e = mod->Ctx.LastCheckedExternalDefinition; i != e; ++i) {
    auto def = mod->Ctx.ExternalDefinitions[i];
    sgm.emitExternalDefinition(def);
  }

  return m;
}

std::unique_ptr<SILModule>
swift::performSILGeneration(Module *mod,
                            SILOptions &options,
                            bool makeModuleFragile,
                            bool wholeModuleCompilation) {
  return SILModule::constructSIL(mod, options, nullptr, None, makeModuleFragile,
                                 wholeModuleCompilation);
}

std::unique_ptr<SILModule>
swift::performSILGeneration(FileUnit &sf, SILOptions &options,
                            Optional<unsigned> startElem,
                            bool makeModuleFragile) {
  return SILModule::constructSIL(sf.getParentModule(), options, &sf, startElem,
                                 makeModuleFragile, false);
}
