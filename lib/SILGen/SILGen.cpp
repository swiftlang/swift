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

#include "SILGenFunction.h"
#include "llvm/ADT/Optional.h"
#include "swift/Strings.h"
#include "swift/AST/AST.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/Subsystems.h"
#include "llvm/Support/Debug.h"
#define DEBUG_TYPE "silgen"
using namespace swift;
using namespace Lowering;

//===--------------------------------------------------------------------===//
// SILGenFunction Class implementation
//===--------------------------------------------------------------------===//

SILGenFunction::SILGenFunction(SILGenModule &SGM, SILFunction &F)
  : SGM(SGM), F(F), LastInsnWithoutScope(0),
    B(createBasicBlock(), &InsertedInstrs),
    ReturnDest(CleanupLocation::getCleanupLocation(F.getLocation())),
    NeedsReturn(false), CurrentSILLoc(F.getLocation()), Cleanups(*this)
{
}

/// SILGenFunction destructor - called after the entire function's AST has been
/// visited.  This handles "falling off the end of the function" logic.
SILGenFunction::~SILGenFunction() {
  // If the end of the function isn't terminated, we screwed up somewhere.
  assert(!B.hasValidInsertionPoint() &&
         "SILGenFunction did not terminate function?!");
  freeWritebackStack();
}

//===--------------------------------------------------------------------===//
// SILGenModule Class implementation
//===--------------------------------------------------------------------===//

SILGenModule::SILGenModule(SILModule &M, Module *SM)
  : M(M), Types(M.Types), SwiftModule(SM), TopLevelSGF(nullptr) {
}

SILGenModule::~SILGenModule() {
  assert(!TopLevelSGF && "active source file lowering!?");
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
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }
    if (lookup->Results.size() == 0) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_missing,
                   moduleName, functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }
    // FIXME: Resolve overloads.
    if (lookup->Results.size() > 1) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_overloaded,
                   moduleName, functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }
    auto &result = lookup->Results[0];
    // Check that the bridging function is actually a function.
    if (!result.hasValueDecl()) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_not_function,
                   moduleName, functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }
    FuncDecl *fd = dyn_cast<FuncDecl>(result.getValueDecl());
    if (!fd) {
      SGM.diagnose(result.getValueDecl()->getLoc(),
                   diag::bridging_function_not_function,
                   moduleName, functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }

    assert(fd->hasType() && "bridging functions must be type-checked");

    // Check that the function takes the expected arguments and returns the
    // expected result type.
    SILDeclRef c(fd);
    auto funcInfo = SGM.getConstantType(c).castTo<SILFunctionType>();
    
    if (funcInfo->getInterfaceParameters().size() != inputTypes.size()
        || !std::equal(funcInfo->getInterfaceParameterSILTypes().begin(),
                       funcInfo->getInterfaceParameterSILTypes().end(),
                       inputTypes.begin())) {
      SGM.diagnose(fd->getLoc(), diag::bridging_function_not_correct_type,
                   moduleName, functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }
    
    if (funcInfo->getInterfaceResult().getSILType() != outputType) {
      SGM.diagnose(fd->getLoc(), diag::bridging_function_not_correct_type,
                   moduleName, functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
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
  return SGM.getLoweredType(InOutType::get(SGM.Types.getStringType()));
}

static SILType getNSStringTy(SILGenModule &SGM) {
  return SGM.getLoweredType(SGM.Types.getNSStringType());
}

SILDeclRef SILGenModule::getNSStringToStringFn() {
  return getBridgingFn(NSStringToStringFn, *this,
                       FOUNDATION_MODULE_NAME, "_convertNSStringToString",
                       {getNSStringTy(*this), getInOutStringTy(*this)},
                       Types.getEmptyTupleType());
}

/// Find the default initializer for the given type.
static ConstructorDecl *getDefaultInitFn(SILGenModule &sgm, CanType type) {
  auto &C = sgm.getASTContext();
  auto decl = type->getNominalOrBoundGenericNominal();
  auto constructors = decl->lookupDirect(C.Id_init);
  ConstructorDecl *defaultCtor = nullptr;
  for (auto decl : constructors) {
    auto ctor = dyn_cast<ConstructorDecl>(decl);
    if (!ctor) continue;
    auto argTy = ctor->getType()->castTo<AnyFunctionType>()
      ->getResult()->castTo<AnyFunctionType>()->getInput();
    if (!argTy->isEqual(C.TheEmptyTupleType))
      continue;
      
    defaultCtor = ctor;
    break;
  }
    
  if (!defaultCtor) {
    sgm.diagnose(SourceLoc(), diag::bridging_function_missing,
                 STDLIB_NAME, type->getString() + ".init()");
    llvm::report_fatal_error("unable to set up the ObjC bridge!");
  }
    
  return defaultCtor;

}

SILDeclRef SILGenModule::getStringDefaultInitFn() {
  return StringDefaultInitFn.cache([&] {
      return SILDeclRef(getDefaultInitFn(*this, Types.getStringType()),
                        SILDeclRef::Kind::Allocator); 
  });
}

SILDeclRef SILGenModule::getStringToNSStringFn() {
  return getBridgingFn(StringToNSStringFn, *this,
                       FOUNDATION_MODULE_NAME, "_convertStringToNSString",
                       {getInOutStringTy(*this)},
                       getNSStringTy(*this));
}

static SILType getNSArrayTy(SILGenModule &SGM) {
  return SGM.getLoweredType(SGM.Types.getNSArrayType());
}

static SILType getAnyObjectArrayTy(SILGenModule &SGM) {
  return SGM.getLoweredType(SGM.Types.getAnyObjectArrayType());
}

SILDeclRef SILGenModule::getAnyObjectArrayToNSArrayFn() {
  return getBridgingFn(AnyObjectArrayToNSArrayFn, *this,
                       FOUNDATION_MODULE_NAME, "_convertAnyObjectArrayToNSArray",
                       {getAnyObjectArrayTy(*this)},
                       getNSArrayTy(*this));
}

SILDeclRef SILGenModule::getNSArrayToAnyObjectArrayFn() {
  return getBridgingFn(NSArrayToAnyObjectArrayFn, *this,
                       FOUNDATION_MODULE_NAME, "_convertNSArrayToAnyObjectArray",
                       {getNSArrayTy(*this)},
                       getAnyObjectArrayTy(*this));
}

#define STANDARD_GET_BRIDGING_FN(Module, FromTy, ToTy) \
  SILDeclRef SILGenModule::get##FromTy##To##ToTy##Fn() { \
    return getBridgingFn(FromTy##To##ToTy##Fn, *this, \
                         Module, "_convert" #FromTy "To" #ToTy, \
                         {getLoweredType(Types.get##FromTy##Type())}, \
                         getLoweredType(Types.get##ToTy##Type())); \
  }

STANDARD_GET_BRIDGING_FN(OBJC_MODULE_NAME, Bool, ObjCBool)
STANDARD_GET_BRIDGING_FN(OBJC_MODULE_NAME, ObjCBool, Bool)
STANDARD_GET_BRIDGING_FN(STDLIB_NAME, CMutableVoidPointer, COpaquePointer)
STANDARD_GET_BRIDGING_FN(STDLIB_NAME, CConstVoidPointer, COpaquePointer)
STANDARD_GET_BRIDGING_FN(STDLIB_NAME, COpaquePointer, CMutableVoidPointer)
STANDARD_GET_BRIDGING_FN(STDLIB_NAME, COpaquePointer, CConstVoidPointer)

#undef STANDARD_GET_BRIDGING_FN

static SILType getPointerInterfaceType(NominalTypeDecl *pointerDecl) {
  return SILType::getPrimitiveObjectType(pointerDecl->getDeclaredInterfaceType()
                                                    ->getCanonicalType());
}

static SILType getCConstPointerInterfaceType(SILGenModule &SGM) {
  return getPointerInterfaceType(SGM.Types.getCConstPointerDecl());
}

static SILType getCMutablePointerInterfaceType(SILGenModule &SGM) {
  return getPointerInterfaceType(SGM.Types.getCMutablePointerDecl());
}

static SILType getObjCMutablePointerInterfaceType(SILGenModule &SGM) {
  return getPointerInterfaceType(SGM.Types.getObjCMutablePointerDecl());
}

static SILType getUnsafePointerInterfaceType(SILGenModule &SGM) {
  return getPointerInterfaceType(SGM.Types.getUnsafePointerDecl());
}

SILDeclRef SILGenModule::getCConstPointerToUnsafePointerFn() {
  return getBridgingFn(CConstPointerToUnsafePointerFn, *this,
                       STDLIB_NAME, "_convertCConstPointerToUnsafePointer",
                       {getCConstPointerInterfaceType(*this)},
                       getUnsafePointerInterfaceType(*this));
}

SILDeclRef SILGenModule::getCMutablePointerToUnsafePointerFn() {
  return getBridgingFn(CMutablePointerToUnsafePointerFn, *this,
                       STDLIB_NAME, "_convertCMutablePointerToUnsafePointer",
                       {getCMutablePointerInterfaceType(*this)},
                       getUnsafePointerInterfaceType(*this));
}

SILDeclRef SILGenModule::getObjCMutablePointerToUnsafePointerFn() {
  return getBridgingFn(ObjCMutablePointerToUnsafePointerFn, *this,
                       STDLIB_NAME, "_convertObjCMutablePointerToUnsafePointer",
                       {getObjCMutablePointerInterfaceType(*this)},
                       getUnsafePointerInterfaceType(*this));
}

SILDeclRef SILGenModule::getUnsafePointerToCConstPointerFn() {
  return getBridgingFn(UnsafePointerToCConstPointerFn, *this,
                       STDLIB_NAME, "_convertUnsafePointerToCConstPointer",
                       {getUnsafePointerInterfaceType(*this)},
                       getCConstPointerInterfaceType(*this));
}

SILDeclRef SILGenModule::getUnsafePointerToCMutablePointerFn() {
  return getBridgingFn(UnsafePointerToCMutablePointerFn, *this,
                       STDLIB_NAME, "_convertUnsafePointerToCMutablePointer",
                       {getUnsafePointerInterfaceType(*this)},
                       getCMutablePointerInterfaceType(*this));
}

SILDeclRef SILGenModule::getUnsafePointerToObjCMutablePointerFn() {
  return getBridgingFn(UnsafePointerToObjCMutablePointerFn, *this,
                       STDLIB_NAME, "_convertUnsafePointerToObjCMutablePointer",
                       {getUnsafePointerInterfaceType(*this)},
                       getObjCMutablePointerInterfaceType(*this));
}

SILFunction *SILGenModule::emitTopLevelFunction(SILLocation Loc) {
  ASTContext &C = M.getASTContext();
  auto extInfo = FunctionType::ExtInfo()
    .withRepresentation(FunctionType::Representation::Thin);
  Type topLevelType = FunctionType::get(TupleType::getEmpty(C),
                                        TupleType::getEmpty(C),
                                        extInfo);
  auto loweredType = getLoweredType(topLevelType).castTo<SILFunctionType>();
  return SILFunction::create(M, SILLinkage::Private,
                             SWIFT_ENTRY_POINT_FUNCTION, loweredType, nullptr,
                             Loc);
}

SILType SILGenModule::getConstantType(SILDeclRef constant) {
  return Types.getConstantType(constant);
}

static void updateLinkageForDefinition(SILGenModule &SGM,
                                       SILFunction *fn, SILDeclRef constant) {
  // In all the cases where getConstantLinkage returns something
  // different for ForDefinition, it returns an available-externally
  // linkage.
  if (!isAvailableExternally(fn->getLinkage())) return;
  fn->setLinkage(constant.getLinkage(ForDefinition));
}

SILFunction *SILGenModule::getFunction(SILDeclRef constant,
                                       ForDefinition_t forDefinition) {
  auto found = emittedFunctions.find(constant);
  if (found != emittedFunctions.end()) {
    if (forDefinition)
      updateLinkageForDefinition(*this, found->second, constant);
    return found->second;
  }
  
  auto constantType = getConstantType(constant).castTo<SILFunctionType>();
  SILLinkage linkage = constant.getLinkage(forDefinition);

  IsTransparent_t IsTrans = constant.isTransparent()?
                              IsTransparent : IsNotTransparent;

  SmallVector<char, 128> buffer;
  auto *F = SILFunction::create(M, linkage, constant.mangle(buffer),
                                constantType, nullptr,
                                Nothing, IsNotBare, IsTrans);

  F->setGlobalInit(constant.isGlobal());
  
  ValueDecl *VD = nullptr;
  if (constant.hasDecl())
    VD = constant.getDecl();

  F->setDeclContext(VD);
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

  SILFunction *f = getFunction(constant, ForDefinition);
  assert(f->empty() && "already emitted function?!");

  f->setContextGenericParams(
                         Types.getConstantInfo(constant).ContextGenericParams);
  
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
    
    for (auto Capture : AFD->getCaptureInfo().getCaptures()) {
      auto It = TopLevelSGF->VarLocs.find(Capture);
      if (It == TopLevelSGF->VarLocs.end() ||
          // Decls captured by value don't escape.
          It->second.isConstant())
        continue;

      Captures.push_back(It->second.getAddress());
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

void SILGenModule::emitForeignThunk(SILDeclRef thunk) {
  assert(!thunk.isForeign && "native-to-foreign thunk not implemented");
  SILFunction *f = preEmitFunction(thunk, thunk.getDecl(), thunk.getDecl());
  PrettyStackTraceSILFunction X("silgen emitForeignThunk", f);
  SILGenFunction(*this, *f).emitForeignThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::addGlobalVariable(VarDecl *global) {
  M.globals.insert(global);
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
  if (decl->hasClangNode() && decl->getAttrs().isUnavailable())
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

void SILGenModule::emitClosure(AbstractClosureExpr *ce) {
  SILDeclRef constant(ce);
  SILFunction *f = preEmitFunction(constant, ce, ce);
  PrettyStackTraceSILFunction X("silgen closureexpr", f);
  SILGenFunction(*this, *f).emitClosure(ce);
  postEmitFunction(constant, f);
}

/// Determine whether the given class requires a separate instance
/// variable initialization method.
static bool requiresIVarInitialization(SILGenModule &SGM, ClassDecl *cd) {
  if (!cd->requiresStoredPropertyInits())
    return false;

  for (Decl *member : cd->getMembers()) {
    auto pbd = dyn_cast<PatternBindingDecl>(member);
    if (!pbd) continue;

    if (pbd->getInit())
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

void SILGenModule::emitDestructor(ClassDecl *cd, DestructorDecl *dd) {
  emitAbstractFuncDecl(dd);

  // If the class would use the Objective-C allocator, only emit -dealloc.
  if (usesObjCAllocator(cd)) {
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
    if (!dd->getBody()->getElements().empty())
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
                                                 PatternBindingDecl *binding) {
  ASTContext &C = M.getASTContext();
  Type initType = FunctionType::get(
                    TupleType::getEmpty(C), TupleType::getEmpty(C),
                    FunctionType::ExtInfo()
                      .withRepresentation(FunctionType::Representation::Thin));
  auto initSILType = getLoweredType(initType).castTo<SILFunctionType>();
  
  auto *f = 
    SILFunction::create(M, SILLinkage::Private, funcName,
                        initSILType, nullptr,
                        binding, IsNotBare, IsNotTransparent);
  f->setDebugScope(new (M) SILDebugScope(RegularLocation(binding->getInit())));
  f->setLocation(binding);
  
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
  PrettyStackTraceSILFunction X("silgen emitGlobalAccessor", f);
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
  SILGenFunction(*this, *f).emitObjCMethodThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::emitObjCPropertyMethodThunks(AbstractStorageDecl *prop) {
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
  SILGenFunction(*this, *f).emitObjCGetter(getter);
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
  SILGenFunction(*this, *f).emitObjCSetter(setter);
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
  SILGenFunction(*this, *f).emitObjCMethodThunk(thunk);
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
  SILGenFunction(*this, *f).emitObjCMethodThunk(thunk);
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
  if (pd->hasInit())
    emitGlobalInitialization(pd);
}

void SILGenModule::visitVarDecl(VarDecl *vd) {
  if (vd->hasStorage())
    addGlobalVariable(vd);
}

namespace {

/// An RAII class to scope source file codegen.
class SourceFileScope {
  SILGenModule &sgm;
public:
  SourceFileScope(SILGenModule &sgm, SourceFile *sf) : sgm(sgm) {
    // If this is the script-mode file for the module, create a toplevel.
    if (sf->isScriptMode()) {
      assert(!sgm.TopLevelSGF && "already emitted toplevel?!");
      assert(!sgm.M.lookUpFunction(SWIFT_ENTRY_POINT_FUNCTION)
             && "already emitted toplevel?!");
      
      RegularLocation TopLevelLoc = RegularLocation::getModuleLocation();
      SILFunction *toplevel = sgm.emitTopLevelFunction(TopLevelLoc);
      
      // Assign a debug scope pointing into the void to the top level function.
      toplevel->setDebugScope(new (sgm.M) SILDebugScope(TopLevelLoc));
      
      sgm.TopLevelSGF = new SILGenFunction(sgm, *toplevel);
      sgm.TopLevelSGF->MagicFunctionName = sgm.SwiftModule->Name;
      sgm.TopLevelSGF->prepareEpilog(Type(),
                                 CleanupLocation::getModuleCleanupLocation());
    }
  }
  
  ~SourceFileScope() {
    if (sgm.TopLevelSGF) {
      sgm.TopLevelSGF->emitEpilog(RegularLocation::getModuleLocation(),
                                  /* AutoGen */ true);
      SILFunction *toplevel = &sgm.TopLevelSGF->getFunction();
      delete sgm.TopLevelSGF;
      
      DEBUG(llvm::dbgs() << "lowered toplevel sil:\n";
            toplevel->print(llvm::dbgs()));
      toplevel->verify();
      sgm.TopLevelSGF = nullptr;
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

std::unique_ptr<SILModule> SILModule::constructSIL(Module *mod,
                                                   SourceFile *sf,
                                                   unsigned startElem) {
  std::unique_ptr<SILModule> m(new SILModule(mod));
  SILGenModule sgm(*m, mod);

  if (sf) {
    sgm.emitSourceFile(sf, startElem);
  } else {
    assert(startElem == 0 && "no explicit source file");
    for (auto file : mod->getFiles()) {
      auto nextSF = dyn_cast<SourceFile>(file);
      if (!nextSF || nextSF->ASTStage != SourceFile::TypeChecked)
        continue;
      sgm.emitSourceFile(nextSF, startElem);
    }
  }
    
  // Emit external definitions used by this module.
  for (auto def : mod->Ctx.ExternalDefinitions) {
    sgm.emitExternalDefinition(def);
  }

  return m;
}

std::unique_ptr<SILModule> swift::performSILGeneration(Module *mod) {
  return SILModule::constructSIL(mod);
}

std::unique_ptr<SILModule> swift::performSILGeneration(SourceFile &sf,
                                       unsigned startElem) {
  return SILModule::constructSIL(sf.getParentModule(), &sf, startElem);
}
