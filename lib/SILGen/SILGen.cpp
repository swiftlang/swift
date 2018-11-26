//===--- SILGen.cpp - Implements Lowering of ASTs -> SIL ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "silgen"
#include "SILGenFunction.h"
#include "Scope.h"
#include "swift/Strings.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/Timer.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/Subsystems.h"
#include "llvm/ProfileData/InstrProfReader.h"
#include "llvm/Support/Debug.h"
#include "ManagedValue.h"
#include "RValue.h"
using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
// SILGenModule Class implementation
//===----------------------------------------------------------------------===//

SILGenModule::SILGenModule(SILModule &M, ModuleDecl *SM)
    : M(M), Types(M.Types), SwiftModule(SM), TopLevelSGF(nullptr) {
  SILOptions &Opts = M.getOptions();
  if (!Opts.UseProfile.empty()) {
    auto ReaderOrErr = llvm::IndexedInstrProfReader::create(Opts.UseProfile);
    if (auto E = ReaderOrErr.takeError()) {
      diagnose(SourceLoc(), diag::profile_read_error, Opts.UseProfile,
               llvm::toString(std::move(E)));
      Opts.UseProfile.erase();
    }
    M.setPGOReader(std::move(ReaderOrErr.get()));
  }
}

SILGenModule::~SILGenModule() {
  assert(!TopLevelSGF && "active source file lowering!?");
  M.verify();
}

static SILDeclRef
getBridgingFn(Optional<SILDeclRef> &cacheSlot,
              SILGenModule &SGM,
              Identifier moduleName,
              StringRef functionName,
              Optional<std::initializer_list<Type>> inputTypes,
              Optional<Type> outputType) {
  // FIXME: the optionality of outputType and the presence of trustInputTypes
  // are hacks for cases where coming up with those types is complicated, i.e.,
  // when dealing with generic bridging functions.

  if (!cacheSlot) {
    ASTContext &ctx = SGM.M.getASTContext();
    ModuleDecl *mod = ctx.getLoadedModule(moduleName);
    if (!mod) {
      SGM.diagnose(SourceLoc(), diag::bridging_module_missing,
                   moduleName.str(), functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }

    SmallVector<ValueDecl *, 2> decls;
    mod->lookupValue(/*AccessPath=*/{}, ctx.getIdentifier(functionName),
                     NLKind::QualifiedLookup, decls);
    if (decls.empty()) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_missing,
                   moduleName.str(), functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }
    if (decls.size() != 1) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_overloaded,
                   moduleName.str(), functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }

    auto *fd = dyn_cast<FuncDecl>(decls.front());
    if (!fd) {
      SGM.diagnose(SourceLoc(), diag::bridging_function_not_function,
                   moduleName.str(), functionName);
      llvm::report_fatal_error("unable to set up the ObjC bridge!");
    }

    assert(fd->hasInterfaceType() && "bridging functions must be type-checked");

    // Check that the function takes the expected arguments and returns the
    // expected result type.
    SILDeclRef c(fd);
    auto funcTy = SGM.Types.getConstantFunctionType(c);
    SILFunctionConventions fnConv(funcTy, SGM.M);

    if (inputTypes) {
      auto toSILType = [&SGM](Type ty) { return SGM.getLoweredType(ty); };
      if (fnConv.hasIndirectSILResults()
          || funcTy->getNumParameters() != inputTypes->size()
          || !std::equal(
                 fnConv.getParameterSILTypes().begin(),
                 fnConv.getParameterSILTypes().end(),
                 makeTransformIterator(inputTypes->begin(), toSILType))) {
        SGM.diagnose(fd->getLoc(), diag::bridging_function_not_correct_type,
                     moduleName.str(), functionName);
        llvm::report_fatal_error("unable to set up the ObjC bridge!");
      }
    }

    if (outputType
        && fnConv.getSingleSILResultType() != SGM.getLoweredType(*outputType)) {
      SGM.diagnose(fd->getLoc(), diag::bridging_function_not_correct_type,
                   moduleName.str(), functionName);
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
                         getASTContext().Id_##Module, \
                         "_convert" #FromTy "To" #ToTy, \
                         FromKind(FromTy), \
                         ToKind(ToTy)); \
  }

GET_BRIDGING_FN(Darwin, REQUIRED, Bool, REQUIRED, DarwinBoolean)
GET_BRIDGING_FN(Darwin, REQUIRED, DarwinBoolean, REQUIRED, Bool)
GET_BRIDGING_FN(ObjectiveC, REQUIRED, Bool, REQUIRED, ObjCBool)
GET_BRIDGING_FN(ObjectiveC, REQUIRED, ObjCBool, REQUIRED, Bool)
GET_BRIDGING_FN(Foundation, OPTIONAL, NSError, REQUIRED, Error)
GET_BRIDGING_FN(Foundation, REQUIRED, Error, REQUIRED, NSError)

#undef GET_BRIDGING_FN
#undef REQUIRED
#undef OPTIONAL
#undef GENERIC

static FuncDecl *diagnoseMissingIntrinsic(SILGenModule &sgm,
                                          SILLocation loc,
                                          const char *name) {
  sgm.diagnose(loc, diag::bridging_function_missing,
               sgm.getASTContext().StdlibModuleName.str(), name);
  return nullptr;
}

#define FUNC_DECL(NAME, ID)                             \
  FuncDecl *SILGenModule::get##NAME(SILLocation loc) {  \
    if (auto fn = getASTContext().get##NAME(nullptr))   \
      return fn;                                        \
    return diagnoseMissingIntrinsic(*this, loc, ID);    \
  }
#include "swift/AST/KnownDecls.def"

ProtocolDecl *SILGenModule::getObjectiveCBridgeable(SILLocation loc) {
  if (ObjectiveCBridgeable)
    return *ObjectiveCBridgeable;

  // Find the _ObjectiveCBridgeable protocol.
  auto &ctx = getASTContext();
  auto proto = ctx.getProtocol(KnownProtocolKind::ObjectiveCBridgeable);
  if (!proto)
    diagnose(loc, diag::bridging_objcbridgeable_missing);

  ObjectiveCBridgeable = proto;
  return proto;
}

FuncDecl *SILGenModule::getBridgeToObjectiveCRequirement(SILLocation loc) {
  if (BridgeToObjectiveCRequirement)
    return *BridgeToObjectiveCRequirement;

  // Find the _ObjectiveCBridgeable protocol.
  auto proto = getObjectiveCBridgeable(loc);
  if (!proto) {
    BridgeToObjectiveCRequirement = nullptr;
    return nullptr;
  }

  // Look for _bridgeToObjectiveC().
  auto &ctx = getASTContext();
  FuncDecl *found = nullptr;
  DeclName name(ctx, ctx.Id_bridgeToObjectiveC, llvm::ArrayRef<Identifier>());
  for (auto member : proto->lookupDirect(name, true)) {
    if (auto func = dyn_cast<FuncDecl>(member)) {
      found = func;
      break;
    }
  }

  if (!found)
    diagnose(loc, diag::bridging_objcbridgeable_broken, name);

  BridgeToObjectiveCRequirement = found;
  return found;
}

FuncDecl *SILGenModule::getUnconditionallyBridgeFromObjectiveCRequirement(
    SILLocation loc) {
  if (UnconditionallyBridgeFromObjectiveCRequirement)
    return *UnconditionallyBridgeFromObjectiveCRequirement;

  // Find the _ObjectiveCBridgeable protocol.
  auto proto = getObjectiveCBridgeable(loc);
  if (!proto) {
    UnconditionallyBridgeFromObjectiveCRequirement = nullptr;
    return nullptr;
  }

  // Look for _bridgeToObjectiveC().
  auto &ctx = getASTContext();
  FuncDecl *found = nullptr;
  DeclName name(ctx, ctx.getIdentifier("_unconditionallyBridgeFromObjectiveC"),
                llvm::makeArrayRef(Identifier()));
  for (auto member : proto->lookupDirect(name, true)) {
    if (auto func = dyn_cast<FuncDecl>(member)) {
      found = func;
      break;
    }
  }

  if (!found)
    diagnose(loc, diag::bridging_objcbridgeable_broken, name);

  UnconditionallyBridgeFromObjectiveCRequirement = found;
  return found;
}

AssociatedTypeDecl *
SILGenModule::getBridgedObjectiveCTypeRequirement(SILLocation loc) {
  if (BridgedObjectiveCType)
    return *BridgedObjectiveCType;

  // Find the _ObjectiveCBridgeable protocol.
  auto proto = getObjectiveCBridgeable(loc);
  if (!proto) {
    BridgeToObjectiveCRequirement = nullptr;
    return nullptr;
  }

  // Look for _bridgeToObjectiveC().
  auto &ctx = getASTContext();
  AssociatedTypeDecl *found = nullptr;
  DeclName name(ctx.Id_ObjectiveCType);
  for (auto member : proto->lookupDirect(name, true)) {
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(member)) {
      found = assocType;
      break;
    }
  }

  if (!found)
    diagnose(loc, diag::bridging_objcbridgeable_broken, name);

  BridgedObjectiveCType = found;
  return found;
}

ProtocolConformance *
SILGenModule::getConformanceToObjectiveCBridgeable(SILLocation loc, Type type) {
  auto proto = getObjectiveCBridgeable(loc);
  if (!proto) return nullptr;

  // Find the conformance to _ObjectiveCBridgeable.
  auto result = SwiftModule->lookupConformance(type, proto);
  if (result) return result->getConcrete();
  return nullptr;
}

ProtocolDecl *SILGenModule::getBridgedStoredNSError(SILLocation loc) {
  if (BridgedStoredNSError)
    return *BridgedStoredNSError;

  // Find the _BridgedStoredNSError protocol.
  auto &ctx = getASTContext();
  auto proto = ctx.getProtocol(KnownProtocolKind::BridgedStoredNSError);
  BridgedStoredNSError = proto;
  return proto;
}

VarDecl *SILGenModule::getNSErrorRequirement(SILLocation loc) {
  if (NSErrorRequirement)
    return *NSErrorRequirement;

  // Find the _BridgedStoredNSError protocol.
  auto proto = getBridgedStoredNSError(loc);
  if (!proto) {
    NSErrorRequirement = nullptr;
    return nullptr;
  }

  // Look for _nsError.
  auto &ctx = getASTContext();
  VarDecl *found = nullptr;
  for (auto member : proto->lookupDirect(ctx.Id_nsError, true)) {
    if (auto var = dyn_cast<VarDecl>(member)) {
      found = var;
      break;
    }
  }

  NSErrorRequirement = found;
  return found;
}

Optional<ProtocolConformanceRef>
SILGenModule::getConformanceToBridgedStoredNSError(SILLocation loc, Type type) {
  auto proto = getBridgedStoredNSError(loc);
  if (!proto) return None;

  // Find the conformance to _BridgedStoredNSError.
  return SwiftModule->lookupConformance(type, proto);
}

ProtocolConformance *SILGenModule::getNSErrorConformanceToError() {
  if (NSErrorConformanceToError)
    return *NSErrorConformanceToError;

  auto &ctx = getASTContext();
  auto nsError = ctx.getNSErrorDecl();
  if (!nsError) {
    NSErrorConformanceToError = nullptr;
    return nullptr;
  }

  auto error = ctx.getErrorDecl();
  if (!error) {
    NSErrorConformanceToError = nullptr;
    return nullptr;
  }

  auto conformance =
    SwiftModule->lookupConformance(nsError->getDeclaredInterfaceType(),
                                   cast<ProtocolDecl>(error));

  if (conformance && conformance->isConcrete())
    NSErrorConformanceToError = conformance->getConcrete();
  else
    NSErrorConformanceToError = nullptr;
  return *NSErrorConformanceToError;
}


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
    Int32Ty = Int32Decl->getDeclaredInterfaceType()->getCanonicalType();
  } else {
    Int32Ty = CanType(BuiltinIntegerType::get(32, C));
  }

  CanType PtrPtrInt8Ty = C.TheRawPointerType;
  if (auto PointerDecl = C.getUnsafeMutablePointerDecl()) {
    if (auto Int8Decl = cast<TypeDecl>(findStdlibDecl("Int8"))) {
      Type Int8Ty = Int8Decl->getDeclaredInterfaceType();
      Type PointerInt8Ty = BoundGenericType::get(PointerDecl,
                                                 nullptr,
                                                 Int8Ty);
      Type OptPointerInt8Ty = OptionalType::get(PointerInt8Ty);
      PtrPtrInt8Ty = BoundGenericType::get(PointerDecl,
                                           nullptr,
                                           OptPointerInt8Ty)
        ->getCanonicalType();
    }
  }

  SILParameterInfo params[] = {
    SILParameterInfo(Int32Ty, ParameterConvention::Direct_Unowned),
    SILParameterInfo(PtrPtrInt8Ty, ParameterConvention::Direct_Unowned),
  };

  CanSILFunctionType topLevelType = SILFunctionType::get(nullptr, extInfo,
                                   SILCoroutineKind::None,
                                   ParameterConvention::Direct_Unowned,
                                   params, /*yields*/ {},
                                   SILResultInfo(Int32Ty,
                                                 ResultConvention::Unowned),
                                   None,
                                   C);

  return M.createFunction(SILLinkage::Public, SWIFT_ENTRY_POINT_FUNCTION,
                          topLevelType, nullptr, Loc, IsBare, IsNotTransparent,
                          IsNotSerialized, ProfileCounter(), IsNotThunk,
                          SubclassScope::NotApplicable);
}

SILFunction *SILGenModule::getEmittedFunction(SILDeclRef constant,
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
    }
    return F;
  }

  return nullptr;
}

static SILFunction *getFunctionToInsertAfter(SILGenModule &SGM,
                                             SILDeclRef insertAfter) {
  // If the decl ref was emitted, emit after its function.
  while (insertAfter) {
    auto found = SGM.emittedFunctions.find(insertAfter);
    if (found != SGM.emittedFunctions.end()) {
      return found->second;
    }

    // Otherwise, try to insert after the function we would be transitively
    // be inserted after.
    auto foundDelayed = SGM.delayedFunctions.find(insertAfter);
    if (foundDelayed != SGM.delayedFunctions.end()) {
      insertAfter = foundDelayed->second.insertAfter;
    } else {
      break;
    }
  }

  // If the decl ref is nil, just insert at the beginning.
  return nullptr;
}

static bool hasSILBody(FuncDecl *fd) {
  if (auto accessor = dyn_cast<AccessorDecl>(fd))
    if (accessor->isMaterializeForSet())
      return !isa<ProtocolDecl>(accessor->getDeclContext());

  return fd->getBody(/*canSynthesize=*/false);
}

static bool haveProfiledAssociatedFunction(SILDeclRef constant) {
  return constant.isDefaultArgGenerator() || constant.isForeign ||
         constant.isCurried;
}

SILProfiler *
SILGenModule::getOrCreateProfilerForConstructors(DeclContext *ctx,
                                                 ConstructorDecl *cd) {
  const auto &Opts = M.getOptions();
  if (!Opts.GenerateProfile && Opts.UseProfile.empty())
    return nullptr;

  // Profile nominal types and extensions separately, as they may live in
  // distinct files. For extensions, just pass in the constructor, because
  // there are no stored property initializers to visit.
  Decl *decl = nullptr;
  if (ctx->isExtensionContext())
    decl = cd;
  else
    decl = ctx->getAsNominalTypeOrNominalTypeExtensionContext();
  assert(decl && "No decl available for profiling in this context");

  SILProfiler *&profiler = constructorProfilers[decl];
  if (!profiler)
    profiler = SILProfiler::create(M, ForDefinition, decl);
  return profiler;
}

SILFunction *SILGenModule::getFunction(SILDeclRef constant,
                                       ForDefinition_t forDefinition) {
  // If we already emitted the function, return it (potentially preparing it
  // for definition).
  if (auto emitted = getEmittedFunction(constant, forDefinition))
    return emitted;

  // Note: Do not provide any SILLocation. You can set it afterwards.
  auto *F = M.getOrCreateFunction(constant.hasDecl() ? constant.getDecl()
                                                     : (Decl *)nullptr,
                                  constant, forDefinition);

  // Set up the function for profiling instrumentation.
  ASTNode profiledNode;
  if (!haveProfiledAssociatedFunction(constant)) {
    if (constant.hasDecl()) {
      if (auto *fd = constant.getFuncDecl()) {
        if (hasSILBody(fd)) {
          F->createProfiler(fd, forDefinition);
          profiledNode = fd->getBody(/*canSynthesize=*/false);
        }
      }
    } else if (auto *ace = constant.getAbstractClosureExpr()) {
      F->createProfiler(ace, forDefinition);
      profiledNode = ace;
    }
    // Set the function entry count for PGO.
    if (SILProfiler *SP = F->getProfiler())
      F->setEntryCount(SP->getExecutionCount(profiledNode));
  }

  assert(F && "SILFunction should have been defined");

  emittedFunctions[constant] = F;

  // If we delayed emitting this function previously, we need it now.
  auto foundDelayed = delayedFunctions.find(constant);
  if (foundDelayed != delayedFunctions.end()) {
    // Move the function to its proper place within the module.
    M.functions.remove(F);
    SILFunction *insertAfter = getFunctionToInsertAfter(*this,
                                              foundDelayed->second.insertAfter);
    if (!insertAfter) {
      M.functions.push_front(F);
    } else {
      M.functions.insertAfter(insertAfter->getIterator(), F);
    }

    forcedFunctions.push_back(*foundDelayed);
    delayedFunctions.erase(foundDelayed);
  } else {
    // We would have registered a delayed function as "last emitted" when we
    // enqueued. If the function wasn't delayed, then we're emitting it now.
    lastEmittedFunction = constant;
  }

  return F;
}

bool SILGenModule::hasFunction(SILDeclRef constant) {
  return emittedFunctions.count(constant);
}

void SILGenModule::visitFuncDecl(FuncDecl *fd) { emitFunction(fd); }

/// Emit a function now, if it's externally usable or has been referenced in
/// the current TU, or remember how to emit it later if not.
template<typename /*void (SILFunction*)*/ Fn>
void emitOrDelayFunction(SILGenModule &SGM,
                         SILDeclRef constant,
                         Fn &&emitter,
                         bool forceEmission = false) {
  auto emitAfter = SGM.lastEmittedFunction;

  SILFunction *f = nullptr;

  // If the function is explicit or may be externally referenced, or if we're
  // forcing emission, we must emit it.
  bool mayDelay;
  // Shared thunks and Clang-imported definitions can always be delayed.
  if (constant.isThunk() || constant.isClangImported()) {
    mayDelay = !forceEmission;
  // Implicit decls may be delayed if they can't be used externally.
  } else {
    auto linkage = constant.getLinkage(ForDefinition);
    mayDelay = !forceEmission &&
               (constant.isImplicit() &&
                !isPossiblyUsedExternally(linkage, SGM.M.isWholeModule()));
  }

  // Avoid emitting a delayable definition if it hasn't already been referenced.
  if (mayDelay)
    f = SGM.getEmittedFunction(constant, ForDefinition);
  else
    f = SGM.getFunction(constant, ForDefinition);

  // If we don't want to emit now, remember how for later.
  if (!f) {
    SGM.delayedFunctions.insert({constant, {emitAfter,
                                            std::forward<Fn>(emitter)}});
    // Even though we didn't emit the function now, update the
    // lastEmittedFunction so that we preserve the original ordering that
    // the symbols would have been emitted in.
    SGM.lastEmittedFunction = constant;
    return;
  }

  emitter(f);
}

void SILGenModule::preEmitFunction(SILDeclRef constant,
                              llvm::PointerUnion<ValueDecl *,
                                                 Expr *> astNode,
                                   SILFunction *F,
                                   SILLocation Loc) {
  // By default, use the astNode to create the location.
  if (Loc.isNull()) {
    if (auto *decl = astNode.get<ValueDecl *>())
      Loc = RegularLocation(decl);
    else
      Loc = RegularLocation(astNode.get<Expr *>());
  }

  assert(F->empty() && "already emitted function?!");

  if (F->getLoweredFunctionType()->isPolymorphic())
    F->setGenericEnvironment(Types.getConstantInfo(constant).GenericEnv);

  // Create a debug scope for the function using astNode as source location.
  F->setDebugScope(new (M) SILDebugScope(Loc, F));

  DEBUG(llvm::dbgs() << "lowering ";
        F->printName(llvm::dbgs());
        llvm::dbgs() << " : ";
        F->getLoweredType().print(llvm::dbgs());
        llvm::dbgs() << '\n';
        if (astNode) {
          if (auto *decl = astNode.dyn_cast<ValueDecl *>())
            decl->dump(llvm::dbgs());
          else
            astNode.get<Expr *>()->dump(llvm::dbgs());
          llvm::dbgs() << '\n';
        });
}

void SILGenModule::postEmitFunction(SILDeclRef constant,
                                    SILFunction *F) {
  assert(!F->isExternalDeclaration() && "did not emit any function body?!");
  DEBUG(llvm::dbgs() << "lowered sil:\n";
        F->print(llvm::dbgs()));
  F->verify();
}

void SILGenModule::
emitMarkFunctionEscapeForTopLevelCodeGlobals(SILLocation loc,
                                             const CaptureInfo &captureInfo) {
  assert(TopLevelSGF && TopLevelSGF->B.hasValidInsertionPoint()
         && "no valid code generator for top-level function?!");

  SmallVector<SILValue, 4> Captures;
  
  for (auto capture : captureInfo.getCaptures()) {
    // Decls captured by value don't escape.
    auto It = TopLevelSGF->VarLocs.find(capture.getDecl());
    if (It == TopLevelSGF->VarLocs.end() ||
        !It->getSecond().value->getType().isAddress())
      continue;
    
    Captures.push_back(It->second.value);
  }
  
  if (!Captures.empty())
    TopLevelSGF->B.createMarkFunctionEscape(loc, Captures);
}

void SILGenModule::emitAbstractFuncDecl(AbstractFunctionDecl *AFD) {
  // Emit any default argument generators.
  if (!isa<DestructorDecl>(AFD)) {
    unsigned paramListIndex = AFD->getDeclContext()->isTypeContext() ? 1 : 0;
    auto *paramList = AFD->getParameterLists()[paramListIndex];
    emitDefaultArgGenerators(AFD, paramList);
  }

  // If this is a function at global scope, it may close over a global variable.
  // If we're emitting top-level code, then emit a "mark_function_escape" that
  // lists the captured global variables so that definite initialization can
  // reason about this escape point.
  if (!AFD->getDeclContext()->isLocalContext() &&
      TopLevelSGF && TopLevelSGF->B.hasValidInsertionPoint()) {
    emitMarkFunctionEscapeForTopLevelCodeGlobals(AFD, AFD->getCaptureInfo());
  }
  
  // If the declaration is exported as a C function, emit its native-to-foreign
  // thunk too, if it wasn't already forced.
  if (AFD->getAttrs().hasAttribute<CDeclAttr>()) {
    auto thunk = SILDeclRef(AFD).asForeign();
    if (!hasFunction(thunk))
      emitNativeToForeignThunk(thunk);
  }
}

void SILGenModule::emitFunction(FuncDecl *fd) {
  SILDeclRef::Loc decl = fd;

  emitAbstractFuncDecl(fd);

  if (hasSILBody(fd)) {
    FrontendStatsTracer Tracer(getASTContext().Stats, "emit-SIL", fd);
    PrettyStackTraceDecl stackTrace("emitting SIL for", fd);

    SILDeclRef constant(decl);

    bool ForCoverageMapping = doesASTRequireProfiling(M, fd);

    emitOrDelayFunction(*this, constant, [this,constant,fd](SILFunction *f){
      preEmitFunction(constant, fd, f, fd);
      PrettyStackTraceSILFunction X("silgen emitFunction", f);
      auto accessor = dyn_cast<AccessorDecl>(fd);
      if (accessor && accessor->isMaterializeForSet())
        SILGenFunction(*this, *f).emitMaterializeForSet(accessor);
      else
        SILGenFunction(*this, *f).emitFunction(fd);
      postEmitFunction(constant, f);
    }, /*forceEmission=*/ForCoverageMapping);
  }
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
  DeclContext *declCtx = decl->getDeclContext();

  bool ForCoverageMapping = doesASTRequireProfiling(M, decl);

  if (declCtx->getAsClassOrClassExtensionContext()) {
    // Class constructors have separate entry points for allocation and
    // initialization.
    emitOrDelayFunction(
        *this, constant, [this, constant, decl](SILFunction *f) {
          preEmitFunction(constant, decl, f, decl);
          PrettyStackTraceSILFunction X("silgen emitConstructor", f);
          SILGenFunction(*this, *f).emitClassConstructorAllocator(decl);
          postEmitFunction(constant, f);
        });

    // If this constructor was imported, we don't need the initializing
    // constructor to be emitted.
    if (!decl->hasClangNode()) {
      SILDeclRef initConstant(decl, SILDeclRef::Kind::Initializer);
      emitOrDelayFunction(
          *this, initConstant,
          [this, initConstant, decl, declCtx](SILFunction *initF) {
            preEmitFunction(initConstant, decl, initF, decl);
            PrettyStackTraceSILFunction X("silgen constructor initializer",
                                          initF);
            initF->setProfiler(
                getOrCreateProfilerForConstructors(declCtx, decl));
            SILGenFunction(*this, *initF).emitClassConstructorInitializer(decl);
            postEmitFunction(initConstant, initF);
          },
          /*forceEmission=*/ForCoverageMapping);
    }
  } else {
    // Struct and enum constructors do everything in a single function.
    emitOrDelayFunction(
        *this, constant, [this, constant, decl, declCtx](SILFunction *f) {
          preEmitFunction(constant, decl, f, decl);
          PrettyStackTraceSILFunction X("silgen emitConstructor", f);
          f->setProfiler(getOrCreateProfilerForConstructors(declCtx, decl));
          SILGenFunction(*this, *f).emitValueConstructor(decl);
          postEmitFunction(constant, f);
        });
  }
}

void SILGenModule::emitEnumConstructor(EnumElementDecl *decl) {
  // Enum element constructors are always emitted by need, so don't need
  // delayed emission.
  SILDeclRef constant(decl);
  SILFunction *f = getFunction(constant, ForDefinition);
  preEmitFunction(constant, decl, f, decl);
  PrettyStackTraceSILFunction X("silgen enum constructor", f);
  SILGenFunction(*this, *f).emitEnumConstructor(decl);
  postEmitFunction(constant, f);
}

SILFunction *SILGenModule::emitClosure(AbstractClosureExpr *ce) {
  SILDeclRef constant(ce);
  SILFunction *f = getFunction(constant, ForDefinition);

  // Generate the closure function, if we haven't already.
  //
  // We may visit the same closure expr multiple times in some cases,
  // for instance, when closures appear as in-line initializers of stored
  // properties. In these cases the closure will be emitted into every
  // initializer of the containing type.
  if (!f->isExternalDeclaration())
    return f;

  preEmitFunction(constant, ce, f, ce);
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
      if (entry.getInit())
        return true;
  }

  return false;
}

bool SILGenModule::hasNonTrivialIVars(ClassDecl *cd) {
  for (Decl *member : cd->getMembers()) {
    auto *vd = dyn_cast<VarDecl>(member);
    if (!vd || !vd->hasStorage()) continue;

    const TypeLowering &ti = Types.getTypeLowering(vd->getType());
    if (!ti.isTrivial())
      return true;
  }

  return false;
}

bool SILGenModule::requiresIVarDestroyer(ClassDecl *cd) {
  // Only needed if we have non-trivial ivars, we're not a root class, and
  // the superclass is not @objc.
  return (hasNonTrivialIVars(cd) &&
          cd->getSuperclass() &&
          !cd->getSuperclass()->getClassOrBoundGenericClass()->hasClangNode());
}

/// TODO: This needs a better name.
void SILGenModule::emitObjCAllocatorDestructor(ClassDecl *cd,
                                               DestructorDecl *dd) {
  // Emit the native deallocating destructor for -dealloc.
  // Destructors are a necessary part of class metadata, so can't be delayed.
  {
    SILDeclRef dealloc(dd, SILDeclRef::Kind::Deallocator);
    SILFunction *f = getFunction(dealloc, ForDefinition);
    preEmitFunction(dealloc, dd, f, dd);
    PrettyStackTraceSILFunction X("silgen emitDestructor -dealloc", f);
    f->createProfiler(dd, ForDefinition);
    SILGenFunction(*this, *f).emitObjCDestructor(dealloc);
    postEmitFunction(dealloc, f);
  }

  // Emit the Objective-C -dealloc entry point if it has
  // something to do beyond messaging the superclass's -dealloc.
  if (dd->getBody()->getNumElements() != 0)
    emitObjCDestructorThunk(dd);

  // Emit the ivar initializer, if needed.
  if (requiresIVarInitialization(*this, cd)) {
    auto ivarInitializer = SILDeclRef(cd, SILDeclRef::Kind::IVarInitializer)
      .asForeign();
    SILFunction *f = getFunction(ivarInitializer, ForDefinition);
    preEmitFunction(ivarInitializer, dd, f, dd);
    PrettyStackTraceSILFunction X("silgen emitDestructor ivar initializer", f);
    SILGenFunction(*this, *f).emitIVarInitializer(ivarInitializer);
    postEmitFunction(ivarInitializer, f);
  }

  // Emit the ivar destroyer, if needed.
  if (hasNonTrivialIVars(cd)) {
    auto ivarDestroyer = SILDeclRef(cd, SILDeclRef::Kind::IVarDestroyer)
      .asForeign();
    SILFunction *f = getFunction(ivarDestroyer, ForDefinition);
    preEmitFunction(ivarDestroyer, dd, f, dd);
    PrettyStackTraceSILFunction X("silgen emitDestructor ivar destroyer", f);
    SILGenFunction(*this, *f).emitIVarDestroyer(ivarDestroyer);
    postEmitFunction(ivarDestroyer, f);
  }
}

void SILGenModule::emitDestructor(ClassDecl *cd, DestructorDecl *dd) {
  emitAbstractFuncDecl(dd);
  
  // Emit the ivar destroyer, if needed.
  if (requiresIVarDestroyer(cd)) {
    SILDeclRef ivarDestroyer(cd, SILDeclRef::Kind::IVarDestroyer);
    SILFunction *f = getFunction(ivarDestroyer, ForDefinition);
    preEmitFunction(ivarDestroyer, dd, f, dd);
    PrettyStackTraceSILFunction X("silgen emitDestructor ivar destroyer", f);
    SILGenFunction(*this, *f).emitIVarDestroyer(ivarDestroyer);
    postEmitFunction(ivarDestroyer, f);
  }

  // If the class would use the Objective-C allocator, only emit -dealloc.
  if (usesObjCAllocator(cd)) {
    emitObjCAllocatorDestructor(cd, dd);
    return;
  }

  // Emit the destroying destructor.
  // Destructors are a necessary part of class metadata, so can't be delayed.
  {
    SILDeclRef destroyer(dd, SILDeclRef::Kind::Destroyer);
    SILFunction *f = getFunction(destroyer, ForDefinition);
    preEmitFunction(destroyer, dd, f, dd);
    PrettyStackTraceSILFunction X("silgen emitDestroyingDestructor", f);
    SILGenFunction(*this, *f).emitDestroyingDestructor(dd);
    f->setDebugScope(new (M) SILDebugScope(dd, f));
    postEmitFunction(destroyer, f);
  }

  // Emit the deallocating destructor.
  {
    SILDeclRef deallocator(dd, SILDeclRef::Kind::Deallocator);
    SILFunction *f = getFunction(deallocator, ForDefinition);
    preEmitFunction(deallocator, dd, f, dd);
    PrettyStackTraceSILFunction X("silgen emitDeallocatingDestructor", f);
    f->createProfiler(dd, ForDefinition);
    SILGenFunction(*this, *f).emitDeallocatingDestructor(dd);
    f->setDebugScope(new (M) SILDebugScope(dd, f));
    postEmitFunction(deallocator, f);
  }
}

void SILGenModule::emitDefaultArgGenerator(SILDeclRef constant, Expr *arg,
                                           DefaultArgumentKind kind) {
  switch (kind) {
  case DefaultArgumentKind::None:
    llvm_unreachable("No default argument here?");

  case DefaultArgumentKind::Normal:
    break;

  case DefaultArgumentKind::Inherited:
    return;

  case DefaultArgumentKind::Column:
  case DefaultArgumentKind::File:
  case DefaultArgumentKind::Line:
  case DefaultArgumentKind::Function:
  case DefaultArgumentKind::DSOHandle:
  case DefaultArgumentKind::NilLiteral:
  case DefaultArgumentKind::EmptyArray:
  case DefaultArgumentKind::EmptyDictionary:
    return;
  }

  emitOrDelayFunction(*this, constant, [this,constant,arg](SILFunction *f) {
    preEmitFunction(constant, arg, f, arg);
    PrettyStackTraceSILFunction X("silgen emitDefaultArgGenerator ", f);
    SILGenFunction SGF(*this, *f);
    SGF.emitGeneratorFunction(constant, arg);
    postEmitFunction(constant, f);
  });
}

void SILGenModule::
emitStoredPropertyInitialization(PatternBindingDecl *pbd, unsigned i) {
  const PatternBindingEntry &pbdEntry = pbd->getPatternList()[i];
  auto *var = pbdEntry.getAnchoringVarDecl();
  auto *init = pbdEntry.getInit();

  SILDeclRef constant(var, SILDeclRef::Kind::StoredPropertyInitializer);
  emitOrDelayFunction(*this, constant, [this,constant,init,var](SILFunction *f) {
    preEmitFunction(constant, init, f, init);
    PrettyStackTraceSILFunction X("silgen emitStoredPropertyInitialization", f);

    // Inherit a profiler instance from the constructor.
    f->setProfiler(
        getOrCreateProfilerForConstructors(var->getDeclContext(), nullptr));

    SILGenFunction(*this, *f).emitGeneratorFunction(constant, init);
    postEmitFunction(constant, f);
  });
}

SILFunction *SILGenModule::emitLazyGlobalInitializer(StringRef funcName,
                                                 PatternBindingDecl *binding,
                                                     unsigned pbdEntry) {
  ASTContext &C = M.getASTContext();
  auto *onceBuiltin =
      cast<FuncDecl>(getBuiltinValueDecl(C, C.getIdentifier("once")));
  auto blockParam = onceBuiltin->getParameterLists()[0]->get(1);
  auto *type = blockParam->getType()->castTo<FunctionType>();
  Type initType = FunctionType::get(TupleType::getEmpty(C),
                                    TupleType::getEmpty(C), type->getExtInfo());
  auto initSILType = getLoweredType(initType).castTo<SILFunctionType>();

  auto *f =
      M.createFunction(SILLinkage::Private,
                       funcName, initSILType, nullptr,
                       SILLocation(binding), IsNotBare, IsNotTransparent,
                       IsNotSerialized);
  f->setDebugScope(new (M) SILDebugScope(RegularLocation(binding), f));
  SILGenFunction(*this, *f).emitLazyGlobalInitializer(binding, pbdEntry);
  f->verify();

  return f;
}

void SILGenModule::emitGlobalAccessor(VarDecl *global,
                                      SILGlobalVariable *onceToken,
                                      SILFunction *onceFunc) {
  SILDeclRef accessor(global, SILDeclRef::Kind::GlobalAccessor);
  emitOrDelayFunction(*this, accessor,
                      [this,accessor,global,onceToken,onceFunc](SILFunction *f){
    preEmitFunction(accessor, global, f, global);
    PrettyStackTraceSILFunction X("silgen emitGlobalAccessor", f);
    SILGenFunction(*this, *f)
      .emitGlobalAccessor(global, onceToken, onceFunc);
    postEmitFunction(accessor, f);
  });
}

void SILGenModule::emitDefaultArgGenerators(SILDeclRef::Loc decl,
                                            ParameterList *paramList) {
  unsigned index = 0;
  for (auto param : *paramList) {
    if (auto defaultArg = param->getDefaultValue())
      emitDefaultArgGenerator(SILDeclRef::getDefaultArgGenerator(decl, index),
                              defaultArg, param->getDefaultArgumentKind());
    ++index;
  }
}

void SILGenModule::emitObjCMethodThunk(FuncDecl *method) {
  auto thunk = SILDeclRef(method).asForeign();

  // Don't emit the thunk if it already exists.
  if (hasFunction(thunk))
    return;

  // ObjC entry points are always externally usable, so can't be delay-emitted.

  SILFunction *f = getFunction(thunk, ForDefinition);
  preEmitFunction(thunk, method, f, method);
  PrettyStackTraceSILFunction X("silgen emitObjCMethodThunk", f);
  f->setBare(IsBare);
  f->setThunk(IsThunk);
  SILGenFunction(*this, *f).emitNativeToForeignThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::emitObjCPropertyMethodThunks(AbstractStorageDecl *prop) {
  // If we don't actually need an entry point for the getter, do nothing.
  if (!prop->getGetter() || !requiresObjCMethodEntryPoint(prop->getGetter()))
    return;

  auto getter = SILDeclRef(prop->getGetter(), SILDeclRef::Kind::Func)
    .asForeign();

  // Don't emit the thunks if they already exist.
  if (hasFunction(getter))
    return;

  RegularLocation ThunkBodyLoc(prop);
  ThunkBodyLoc.markAutoGenerated();
  // ObjC entry points are always externally usable, so emitting can't be
  // delayed.
  {
    SILFunction *f = getFunction(getter, ForDefinition);
    preEmitFunction(getter, prop, f, ThunkBodyLoc);
    PrettyStackTraceSILFunction X("silgen objc property getter thunk", f);
    f->setBare(IsBare);
    f->setThunk(IsThunk);
    SILGenFunction(*this, *f).emitNativeToForeignThunk(getter);
    postEmitFunction(getter, f);
  }

  if (!prop->isSettable(prop->getDeclContext()))
    return;

  // FIXME: Add proper location.
  auto setter = SILDeclRef(prop->getSetter(), SILDeclRef::Kind::Func)
    .asForeign();

  SILFunction *f = getFunction(setter, ForDefinition);
  preEmitFunction(setter, prop, f, ThunkBodyLoc);
  PrettyStackTraceSILFunction X("silgen objc property setter thunk", f);
  f->setBare(IsBare);
  f->setThunk(IsThunk);
  SILGenFunction(*this, *f).emitNativeToForeignThunk(setter);
  postEmitFunction(setter, f);
}

void SILGenModule::emitObjCConstructorThunk(ConstructorDecl *constructor) {
  auto thunk = SILDeclRef(constructor, SILDeclRef::Kind::Initializer)
    .asForeign();

  // Don't emit the thunk if it already exists.
  if (hasFunction(thunk))
    return;
  // ObjC entry points are always externally usable, so emitting can't be
  // delayed.

  SILFunction *f = getFunction(thunk, ForDefinition);
  preEmitFunction(thunk, constructor, f, constructor);
  PrettyStackTraceSILFunction X("silgen objc constructor thunk", f);
  f->setBare(IsBare);
  f->setThunk(IsThunk);
  SILGenFunction(*this, *f).emitNativeToForeignThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::emitObjCDestructorThunk(DestructorDecl *destructor) {
  auto thunk = SILDeclRef(destructor, SILDeclRef::Kind::Deallocator)
    .asForeign();

  // Don't emit the thunk if it already exists.
  if (hasFunction(thunk))
    return;
  SILFunction *f = getFunction(thunk, ForDefinition);
  preEmitFunction(thunk, destructor, f, destructor);
  PrettyStackTraceSILFunction X("silgen objc destructor thunk", f);
  f->setBare(IsBare);
  f->setThunk(IsThunk);
  SILGenFunction(*this, *f).emitNativeToForeignThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::visitPatternBindingDecl(PatternBindingDecl *pd) {
  assert(!TopLevelSGF && "script mode PBDs should be in TopLevelCodeDecls");
  for (unsigned i = 0, e = pd->getNumPatternEntries(); i != e; ++i)
    if (pd->getInit(i))
      emitGlobalInitialization(pd, i);
}

void SILGenModule::visitVarDecl(VarDecl *vd) {
  if (vd->hasBehavior())
    emitPropertyBehavior(vd);

  if (vd->hasStorage())
    addGlobalVariable(vd);

  if (vd->getStorageKind() == AbstractStorageDecl::StoredWithTrivialAccessors) {
    // If the global variable has storage, it might also have synthesized
    // accessors. Emit them here, since they won't appear anywhere else.
    if (auto getter = vd->getGetter())
      emitFunction(getter);
    if (auto setter = vd->getSetter())
      emitFunction(setter);
  }
  
  tryEmitPropertyDescriptor(vd);
}

static bool doesPropertyNeedDescriptor(AbstractStorageDecl *decl) {
  // The storage needs a descriptor if it sits at a module's ABI boundary,
  // meaning it has public linkage.
  
  // Any property that's potentially resilient should have accessors
  // synthesized.
  if (!decl->getGetter())
    return false;

  // If the getter is mutating, we cannot form a keypath to it at all.
  if (decl->isGetterMutating())
    return false;

  // TODO: If previous versions of an ABI-stable binary needed the descriptor,
  // then we still do.

  auto getter = SILDeclRef(decl->getGetter());
  auto getterLinkage = getter.getLinkage(ForDefinition);
  
  switch (getterLinkage) {
  case SILLinkage::Public:
  case SILLinkage::PublicNonABI:
    // We may need a descriptor.
    break;
    
  case SILLinkage::Shared:
  case SILLinkage::Private:
  case SILLinkage::Hidden:
    // Don't need a public descriptor.
    return false;
    
  case SILLinkage::HiddenExternal:
  case SILLinkage::PrivateExternal:
  case SILLinkage::PublicExternal:
  case SILLinkage::SharedExternal:
    llvm_unreachable("should be definition linkage?");
  }
  
  // TODO: We might be able to avoid a descriptor if the property is committed
  // to being implemented a certain way, such as if it's promised to remain
  // stored, or is computed with inlinable accessors, and can't change its
  // mutability (because it's already promised to be mutable or fully immutable).
  return true;
}

void SILGenModule::tryEmitPropertyDescriptor(AbstractStorageDecl *decl) {
  if (!M.getASTContext().LangOpts.EnableKeyPathResilience)
    return;
  
  // TODO: Key path code emission doesn't handle opaque values properly yet.
  if (!SILModuleConventions(M).useLoweredAddresses())
    return;
  
  if (!doesPropertyNeedDescriptor(decl))
    return;
  
  auto genericEnv = decl->getInnermostDeclContext()
                        ->getGenericEnvironmentOfContext();
  unsigned baseOperand = 0;
  bool needsGenericContext = true;
  
  Type baseTy;
  if (decl->getDeclContext()->isTypeContext()) {
    baseTy = decl->getDeclContext()->getSelfInterfaceType()
                 ->getCanonicalType(decl->getInnermostDeclContext()
                                        ->getGenericSignatureOfContext());
    if (decl->isStatic()) {
      // TODO: Static properties should eventually be referenceable as
      // keypaths from T.Type -> Element
      //baseTy = MetatypeType::get(baseTy);
      return;
    }
  } else {
    // TODO: Global variables should eventually be referenceable as
    // key paths from ()
    //baseTy = TupleType::getEmpty(getASTContext());
    return;
  }
  
  SubstitutionList subs = {};
  if (genericEnv)
    subs = genericEnv->getForwardingSubstitutions();
  
  if (auto sub = dyn_cast<SubscriptDecl>(decl)) {
    for (auto *index : *sub->getIndices()) {
      // Keypaths can't capture inout indices.
      if (index->isInOut())
        return;

      // TODO: Handle reabstraction and tuple explosion in thunk generation.
      // This wasn't previously a concern because anything that was Hashable
      // had only one abstraction level and no explosion.
      auto indexTy = index->getInterfaceType();
      
      if (isa<TupleType>(indexTy->getCanonicalType(sub->getGenericSignature())))
        return;
      
      if (genericEnv)
        indexTy = genericEnv->mapTypeIntoContext(indexTy);
      
      auto indexLoweredTy = Types.getLoweredType(indexTy);
      auto indexOpaqueLoweredTy =
        Types.getLoweredType(AbstractionPattern::getOpaque(), indexTy);
      
      if (indexOpaqueLoweredTy.getAddressType()
             != indexLoweredTy.getAddressType())
        return;
    }
  }

  auto component = emitKeyPathComponentForDecl(SILLocation(decl),
                                               genericEnv,
                                               baseOperand, needsGenericContext,
                                               subs, decl, {},
                                               baseTy->getCanonicalType(),
                                               /*property descriptor*/ true);
  
  (void)SILProperty::create(M, /*serialized*/ false, decl, component);
}

void SILGenModule::emitPropertyBehavior(VarDecl *vd) {
  assert(vd->hasBehavior());
  // Emit the protocol conformance to the behavior.
  getWitnessTable(*vd->getBehavior()->Conformance);
}

void SILGenModule::visitIfConfigDecl(IfConfigDecl *ICD) {
  // Nothing to do for these kinds of decls - anything active has been added
  // to the enclosing declaration.
}

void SILGenModule::visitPoundDiagnosticDecl(PoundDiagnosticDecl *PDD) {
  // Nothing to do for #error/#warning; they've already been emitted.
}

void SILGenModule::visitTopLevelCodeDecl(TopLevelCodeDecl *td) {
  assert(TopLevelSGF && "top-level code in a non-main source file!");

  if (!TopLevelSGF->B.hasValidInsertionPoint())
    return;

  // A single SILFunction may be used to lower multiple top-level decls. When
  // this happens, fresh profile counters must be assigned to the new decl.
  TopLevelSGF->F.discardProfiler();
  TopLevelSGF->F.createProfiler(td, ForDefinition);

  TopLevelSGF->emitProfilerIncrement(td->getBody());
 
  for (auto &ESD : td->getBody()->getElements()) {
    if (!TopLevelSGF->B.hasValidInsertionPoint()) {
      if (auto *S = ESD.dyn_cast<Stmt*>()) {
        if (S->isImplicit())
          continue;
      } else if (auto *E = ESD.dyn_cast<Expr*>()) {
        if (E->isImplicit())
          continue;
      }

      diagnose(ESD.getStartLoc(), diag::unreachable_code);
      // There's no point in trying to emit anything else.
      return;
    }

    if (auto *S = ESD.dyn_cast<Stmt*>()) {
      TopLevelSGF->emitStmt(S);
    } else if (auto *E = ESD.dyn_cast<Expr*>()) {
      TopLevelSGF->emitIgnoredExpr(E);
    } else {
      TopLevelSGF->visit(ESD.get<Decl*>());
    }
  }
}

void SILGenModule::useConformance(ProtocolConformanceRef conformanceRef) {
  // We don't need to emit dependent conformances.
  if (conformanceRef.isAbstract()) 
    return;

  auto conformance = conformanceRef.getConcrete();
  auto root = conformance->getRootNormalConformance();

  // If we already emitted this witness table, we don't need to track the fact
  // we need it.
  if (emittedWitnessTables.count(root))
    return;

  // If we delayed emitting this witness table, force it.
  auto foundDelayed = delayedConformances.find(root);
  if (foundDelayed != delayedConformances.end()) {
    forcedConformances.push_back(*foundDelayed);
    delayedConformances.erase(foundDelayed);
    return;
  }

  // Otherwise, just remember the fact we used this conformance.
  usedConformances.insert(root);
}

void
SILGenModule::useConformancesFromSubstitutions(SubstitutionList subs) {
  for (auto &sub : subs) {
    for (auto conformance : sub.getConformances())
      useConformance(conformance);
  }
}

namespace {

/// An RAII class to scope source file codegen.
class SourceFileScope {
  SILGenModule &sgm;
  SourceFile *sf;
  Optional<Scope> scope;
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
      toplevel->setDebugScope(new (sgm.M) SILDebugScope(TopLevelLoc, toplevel));

      sgm.TopLevelSGF = new SILGenFunction(sgm, *toplevel);
      sgm.TopLevelSGF->MagicFunctionName = sgm.SwiftModule->getName();
      auto moduleCleanupLoc = CleanupLocation::getModuleCleanupLocation();
      sgm.TopLevelSGF->prepareEpilog(Type(), true, moduleCleanupLoc);

      // Create the argc and argv arguments.
      auto prologueLoc = RegularLocation::getModuleLocation();
      prologueLoc.markAsPrologue();
      auto entry = sgm.TopLevelSGF->B.getInsertionBB();
      auto paramTypeIter =
          sgm.TopLevelSGF->F.getConventions().getParameterSILTypes().begin();
      entry->createFunctionArgument(*paramTypeIter);
      entry->createFunctionArgument(*std::next(paramTypeIter));

      scope.emplace(sgm.TopLevelSGF->Cleanups, moduleCleanupLoc);
    }
  }

  ~SourceFileScope() {
    if (sgm.TopLevelSGF) {
      scope.reset();

      // Unregister the top-level function emitter.
      auto &SGF = *sgm.TopLevelSGF;
      sgm.TopLevelSGF = nullptr;

      // Write out the epilog.
      auto moduleLoc = RegularLocation::getModuleLocation();
      moduleLoc.markAutoGenerated();
      auto returnInfo = SGF.emitEpilogBB(moduleLoc);
      auto returnLoc = returnInfo.second;
      returnLoc.markAutoGenerated();

      SILType returnType = SGF.F.getConventions().getSingleSILResultType();
      auto emitTopLevelReturnValue = [&](unsigned value) -> SILValue {
        // Create an integer literal for the value.
        auto litType = SILType::getBuiltinIntegerType(32, sgm.getASTContext());
        SILValue retValue =
          SGF.B.createIntegerLiteral(moduleLoc, litType, value);

        // Wrap that in a struct if necessary.
        if (litType != returnType) {
          retValue = SGF.B.createStruct(moduleLoc, returnType, retValue);
        }
        return retValue;
      };

      // Fallthrough should signal a normal exit by returning 0.
      SILValue returnValue;
      if (SGF.B.hasValidInsertionPoint())
        returnValue = emitTopLevelReturnValue(0);

      // Handle the implicit rethrow block.
      auto rethrowBB = SGF.ThrowDest.getBlock();
      SGF.ThrowDest = JumpDest::invalid();

      // If the rethrow block wasn't actually used, just remove it.
      if (rethrowBB->pred_empty()) {
        SGF.eraseBasicBlock(rethrowBB);

      // Otherwise, we need to produce a unified return block.
      } else {
        auto returnBB = SGF.createBasicBlock();
        if (SGF.B.hasValidInsertionPoint())
          SGF.B.createBranch(returnLoc, returnBB, returnValue);
        returnValue =
            returnBB->createPHIArgument(returnType, ValueOwnershipKind::Owned);
        SGF.B.emitBlock(returnBB);

        // Emit the rethrow block.
        SILGenSavedInsertionPoint savedIP(SGF, rethrowBB,
                                          FunctionSection::Postmatter);

        // Log the error.
        SILValue error = rethrowBB->getArgument(0);
        SGF.B.createBuiltin(moduleLoc,
                            sgm.getASTContext().getIdentifier("errorInMain"),
                            sgm.Types.getEmptyTupleType(), {}, {error});

        // Signal an abnormal exit by returning 1.
        SGF.Cleanups.emitCleanupsForReturn(CleanupLocation::get(moduleLoc));
        SGF.B.createBranch(returnLoc, returnBB, emitTopLevelReturnValue(1));
      }

      // Return.
      if (SGF.B.hasValidInsertionPoint())
        SGF.B.createReturn(returnLoc, returnValue);

      // Okay, we're done emitting the top-level function; destroy the
      // emitter and verify the result.
      SILFunction *toplevel = &SGF.getFunction();
      delete &SGF;

      DEBUG(llvm::dbgs() << "lowered toplevel sil:\n";
            toplevel->print(llvm::dbgs()));
      toplevel->verify();
    }

    // If the source file contains an artificial main, emit the implicit
    // toplevel code.
    if (auto mainClass = sf->getMainClass()) {
      assert(!sgm.M.lookUpFunction(SWIFT_ENTRY_POINT_FUNCTION)
             && "already emitted toplevel before main class?!");

      RegularLocation TopLevelLoc = RegularLocation::getModuleLocation();
      SILFunction *toplevel = sgm.emitTopLevelFunction(TopLevelLoc);

      // Assign a debug scope pointing into the void to the top level function.
      toplevel->setDebugScope(new (sgm.M) SILDebugScope(TopLevelLoc, toplevel));

      // Create the argc and argv arguments.
      SILGenFunction SGF(sgm, *toplevel);
      auto entry = SGF.B.getInsertionBB();
      auto paramTypeIter =
          SGF.F.getConventions().getParameterSILTypes().begin();
      entry->createFunctionArgument(*paramTypeIter);
      entry->createFunctionArgument(*std::next(paramTypeIter));
      SGF.emitArtificialTopLevel(mainClass);
    }
  }
};

} // end anonymous namespace

void SILGenModule::emitSourceFile(SourceFile *sf, unsigned startElem) {
  SourceFileScope scope(*this, sf);
  for (Decl *D : llvm::makeArrayRef(sf->Decls).slice(startElem))
    visit(D);

  for (Decl *D : sf->LocalTypeDecls)
    visit(D);

  // Mark any conformances as "used".
  for (auto conformance : sf->getUsedConformances())
    useConformance(ProtocolConformanceRef(conformance));
}

//===----------------------------------------------------------------------===//
// SILModule::constructSIL method implementation
//===----------------------------------------------------------------------===//

std::unique_ptr<SILModule>
SILModule::constructSIL(ModuleDecl *mod, SILOptions &options, FileUnit *SF,
                        Optional<unsigned> startElem,
                        bool isWholeModule) {
  SharedTimer timer("SILGen");
  const DeclContext *DC;
  if (startElem) {
    assert(SF && "cannot have a start element without a source file");
    // Because more decls may be added to the SourceFile, we can't assume
    // anything about the compilation context.
    DC = nullptr;
  } else if (SF) {
    DC = SF;
  } else {
    DC = mod;
  }

  std::unique_ptr<SILModule> M(
      new SILModule(mod, options, DC, isWholeModule));
  SILGenModule SGM(*M, mod);

  if (SF) {
    if (auto *file = dyn_cast<SourceFile>(SF)) {
      SGM.emitSourceFile(file, startElem.getValueOr(0));
    } else if (auto *file = dyn_cast<SerializedASTFile>(SF)) {
      if (file->isSIB())
        M->getSILLoader()->getAllForModule(mod->getName(), file);
    }
  } else {
    for (auto file : mod->getFiles()) {
      auto nextSF = dyn_cast<SourceFile>(file);
      if (!nextSF || nextSF->ASTStage != SourceFile::TypeChecked)
        continue;
      SGM.emitSourceFile(nextSF, 0);
    }

    // Also make sure to process any intermediate files that may contain SIL
    bool hasSIB = std::any_of(mod->getFiles().begin(),
                              mod->getFiles().end(),
                              [](const FileUnit *File) -> bool {
      auto *SASTF = dyn_cast<SerializedASTFile>(File);
      return SASTF && SASTF->isSIB();
    });
    if (hasSIB)
      M->getSILLoader()->getAllForModule(mod->getName(), nullptr);
  }

  // Emit external definitions used by this module.
  for (size_t i = 0, e = mod->getASTContext().LastCheckedExternalDefinition;
       i != e; ++i) {
    auto def = mod->getASTContext().ExternalDefinitions[i];
    SGM.emitExternalDefinition(def);
  }

  // Emit any delayed definitions that were forced.
  // Emitting these may in turn force more definitions, so we have to take care
  // to keep pumping the queues.
  while (!SGM.forcedFunctions.empty()
         || !SGM.forcedConformances.empty()) {
    while (!SGM.forcedFunctions.empty()) {
      auto &front = SGM.forcedFunctions.front();
      front.second.emitter(SGM.getFunction(front.first, ForDefinition));
      SGM.forcedFunctions.pop_front();
    }
    while (!SGM.forcedConformances.empty()) {
      auto &front = SGM.forcedConformances.front();
      SGM.getWitnessTable(front.first);
      SGM.forcedConformances.pop_front();
    }
  }

  return M;
}

std::unique_ptr<SILModule>
swift::performSILGeneration(ModuleDecl *mod,
                            SILOptions &options,
                            bool wholeModuleCompilation) {
  return SILModule::constructSIL(mod, options, nullptr, None,
                                 wholeModuleCompilation);
}

std::unique_ptr<SILModule>
swift::performSILGeneration(FileUnit &sf, SILOptions &options,
                            Optional<unsigned> startElem) {
  return SILModule::constructSIL(sf.getParentModule(), options, &sf, startElem,
                                 false);
}
