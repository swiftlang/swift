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
#include "swift/AST/NameLookup.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ResilienceExpansion.h"
#include "swift/Basic/Timer.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/Serialization/SerializedModuleLoader.h"
#include "swift/Serialization/SerializedSILLoader.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILDebugScope.h"
#include "swift/Subsystems.h"
#include "llvm/Support/Debug.h"
#include "ManagedValue.h"
#include "RValue.h"
using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
// SILGenModule Class implementation
//===----------------------------------------------------------------------===//

SILGenModule::SILGenModule(SILModule &M, ModuleDecl *SM, bool makeModuleFragile)
  : M(M), Types(M.Types), SwiftModule(SM), TopLevelSGF(nullptr),
    Profiler(nullptr), makeModuleFragile(makeModuleFragile) {
}

SILGenModule::~SILGenModule() {
  assert(!TopLevelSGF && "active source file lowering!?");
  M.verify();
}

EnumElementDecl *SILGenModule::getLoweredEnumElementDecl(EnumElementDecl *elt) {
  auto &ctx = getASTContext();
  if (elt->getParentEnum()->classifyAsOptionalType()
        != OTK_ImplicitlyUnwrappedOptional)
    return elt;

  if (elt == ctx.getImplicitlyUnwrappedOptionalSomeDecl()) {
    return ctx.getOptionalSomeDecl();
  } else {
    assert(elt == ctx.getImplicitlyUnwrappedOptionalNoneDecl());
    return ctx.getOptionalNoneDecl();
  }
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
    auto funcInfo = SGM.getConstantType(c).castTo<SILFunctionType>();
    SILFunctionConventions fnConv(funcInfo, SGM.M);

    if (inputTypes) {
      auto toSILType = [&SGM](Type ty) { return SGM.getLoweredType(ty); };
      if (fnConv.hasIndirectSILResults()
          || funcInfo->getNumParameters() != inputTypes->size()
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
  auto result = SwiftModule->lookupConformance(type, proto, nullptr);
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
  return SwiftModule->lookupConformance(type, proto, nullptr);
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
                                   cast<ProtocolDecl>(error),
                                   nullptr);

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
                                   ParameterConvention::Direct_Unowned,
                                   params,
                                   SILResultInfo(Int32Ty,
                                                 ResultConvention::Unowned),
                                   None,
                                   C);

  return M.createFunction(SILLinkage::Public, SWIFT_ENTRY_POINT_FUNCTION,
                          topLevelType, nullptr, Loc, IsBare,
                          IsNotTransparent, IsNotSerialized, IsNotThunk,
                          SILFunction::NotRelevant);
}

SILType SILGenModule::getConstantType(SILDeclRef constant) {
  return Types.getConstantType(constant);
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
      if (makeModuleFragile) {
        F->setSerialized(IsSerialized);
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

  assert(F && "SILFunction should have been defined");

  if (makeModuleFragile) {
    SILLinkage linkage = constant.getLinkage(forDefinition);
    if (linkage != SILLinkage::PublicExternal) {
      F->setSerialized(IsSerialized);
    }
  }

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

void SILGenModule::visitFuncDecl(FuncDecl *fd) {
  ProfilerRAII Profiler(*this, fd);
  emitFunction(fd);
}

/// Emit a function now, if it's externally usable or has been referenced in
/// the current TU, or remember how to emit it later if not.
template<typename /*void (SILFunction*)*/ Fn>
void emitOrDelayFunction(SILGenModule &SGM,
                         SILDeclRef constant,
                         Fn &&emitter) {
  auto emitAfter = SGM.lastEmittedFunction;

  SILFunction *f = nullptr;

  // If the function is explicit or may be externally referenced, we must emit
  // it.
  bool mayDelay;
  // Shared thunks and Clang-imported definitions can always be delayed.
  if (constant.isThunk() || constant.isClangImported()) {
    mayDelay = true;
  // Implicit decls may be delayed if they can't be used externally.
  } else {
    auto linkage = constant.getLinkage(ForDefinition);
    mayDelay = constant.isImplicit()
      && !isPossiblyUsedExternally(linkage, SGM.M.isWholeModule());
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
          if (auto *decl = astNode.get<ValueDecl *>())
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
  {
    auto paramLists = AFD->getParameterLists();
    if (AFD->getDeclContext()->isTypeContext())
      paramLists = paramLists.slice(1);
    emitDefaultArgGenerators(AFD, paramLists);
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

static bool hasSILBody(FuncDecl *fd) {
  if (fd->getAccessorKind() == AccessorKind::IsMaterializeForSet)
    return !isa<ProtocolDecl>(fd->getDeclContext());

  return fd->getBody(/*canSynthesize=*/false);
}

void SILGenModule::emitFunction(FuncDecl *fd) {
  SILDeclRef::Loc decl = fd;

  emitAbstractFuncDecl(fd);

  if (hasSILBody(fd)) {
    PrettyStackTraceDecl stackTrace("emitting SIL for", fd);

    SILDeclRef constant(decl);

    emitOrDelayFunction(*this, constant, [this,constant,fd](SILFunction *f){
      preEmitFunction(constant, fd, f, fd);
      if (fd->getAccessorKind() == AccessorKind::IsMaterializeForSet)
        SILGenFunction(*this, *f).emitMaterializeForSet(fd);
      else
        SILGenFunction(*this, *f).emitFunction(fd);
      postEmitFunction(constant, f);
    });
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

  if (decl->getImplicitSelfDecl()->getType()->getInOutObjectType()
        ->getClassOrBoundGenericClass()) {
    // Class constructors have separate entry points for allocation and
    // initialization.
    emitOrDelayFunction(*this, constant, [this,constant,decl](SILFunction *f){
      preEmitFunction(constant, decl, f, decl);
      PrettyStackTraceSILFunction X("silgen emitConstructor", f);
      SILGenFunction(*this, *f)
        .emitClassConstructorAllocator(decl);
      postEmitFunction(constant, f);
    });

    // If this constructor was imported, we don't need the initializing
    // constructor to be emitted.
    if (!decl->hasClangNode()) {
      SILDeclRef initConstant(decl, SILDeclRef::Kind::Initializer);
      emitOrDelayFunction(*this, initConstant,
                          [this,initConstant,decl](SILFunction *initF){
        preEmitFunction(initConstant, decl, initF, decl);
        PrettyStackTraceSILFunction X("silgen constructor initializer", initF);
        SILGenFunction(*this, *initF).emitClassConstructorInitializer(decl);
        postEmitFunction(initConstant, initF);
      });
    }
  } else {
    // Struct and enum constructors do everything in a single function.
    emitOrDelayFunction(*this, constant, [this,constant,decl](SILFunction *f) {
      preEmitFunction(constant, decl, f, decl);
      PrettyStackTraceSILFunction X("silgen emitConstructor", f);
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
    VarDecl *vd = dyn_cast<VarDecl>(member);
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
    SILFunction *f = getFunction(ivarInitializer, ForDefinition);
    preEmitFunction(ivarInitializer, dd, f, dd);
    PrettyStackTraceSILFunction X("silgen emitDestructor ivar initializer", f);
    SILGenFunction(*this, *f).emitIVarInitializer(ivarInitializer);
    postEmitFunction(ivarInitializer, f);
  }

  // Emit the ivar destroyer, if needed.
  if (hasNonTrivialIVars(cd)) {
    SILDeclRef ivarDestroyer(cd, SILDeclRef::Kind::IVarDestroyer,
                             SILDeclRef::ConstructAtBestResilienceExpansion,
                             SILDeclRef::ConstructAtNaturalUncurryLevel,
                             /*isForeign=*/true);
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
    SILDeclRef ivarDestroyer(cd, SILDeclRef::Kind::IVarDestroyer,
                             SILDeclRef::ConstructAtBestResilienceExpansion,
                             SILDeclRef::ConstructAtNaturalUncurryLevel,
                             /*isForeign=*/false);
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
    SILGenFunction(*this, *f).emitDeallocatingDestructor(dd);
    f->setDebugScope(new (M) SILDebugScope(dd, f));
    postEmitFunction(deallocator, f);
  }
}

void SILGenModule::emitDefaultArgGenerator(SILDeclRef constant, Expr *arg) {
  emitOrDelayFunction(*this, constant, [this,constant,arg](SILFunction *f) {
    preEmitFunction(constant, arg, f, arg);
    PrettyStackTraceSILFunction X("silgen emitDefaultArgGenerator ", f);
    SILGenFunction(*this, *f).emitGeneratorFunction(constant, arg);
    postEmitFunction(constant, f);
  });
}

void SILGenModule::
emitStoredPropertyInitialization(PatternBindingDecl *pbd, unsigned i) {
  const PatternBindingEntry &pbdEntry = pbd->getPatternList()[i];
  auto *var = pbdEntry.getAnchoringVarDecl();
  auto *init = pbdEntry.getInit();

  SILDeclRef constant(var, SILDeclRef::Kind::StoredPropertyInitializer);
  emitOrDelayFunction(*this, constant, [this,constant,init](SILFunction *f) {
    preEmitFunction(constant, init, f, init);
    PrettyStackTraceSILFunction X("silgen emitStoredPropertyInitialization", f);
    SILGenFunction(*this, *f).emitGeneratorFunction(constant, init);
    postEmitFunction(constant, f);
  });
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
      M.createFunction(SILLinkage::Private,
                       funcName, initSILType, nullptr,
                       SILLocation(binding), IsNotBare, IsNotTransparent,
                       makeModuleFragile
                           ? IsSerialized
                           : IsNotSerialized);
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

void SILGenModule::emitGlobalGetter(VarDecl *global,
                                    SILGlobalVariable *onceToken,
                                    SILFunction *onceFunc) {
  SILDeclRef accessor(global, SILDeclRef::Kind::GlobalGetter);
  emitOrDelayFunction(*this, accessor,
                      [this,accessor,global,onceToken,onceFunc](SILFunction *f){
    preEmitFunction(accessor, global, f, global);
    PrettyStackTraceSILFunction X("silgen emitGlobalGetter", f);
    SILGenFunction(*this, *f)
      .emitGlobalGetter(global, onceToken, onceFunc);
    postEmitFunction(accessor, f);
  });
}

void SILGenModule::emitDefaultArgGenerators(SILDeclRef::Loc decl,
                                        ArrayRef<ParameterList*> paramLists) {
  unsigned index = 0;
  for (auto paramList : paramLists) {
    for (auto param : *paramList) {
      if (auto defaultArg = param->getDefaultValue())
        emitDefaultArgGenerator(SILDeclRef::getDefaultArgGenerator(decl, index),
                                defaultArg);
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

  SILDeclRef getter(prop->getGetter(), SILDeclRef::Kind::Func,
                    SILDeclRef::ConstructAtBestResilienceExpansion,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*isObjC*/ true);

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
  SILDeclRef setter(prop->getSetter(), SILDeclRef::Kind::Func,
                    SILDeclRef::ConstructAtBestResilienceExpansion,
                    SILDeclRef::ConstructAtNaturalUncurryLevel,
                    /*isObjC*/ true);

  SILFunction *f = getFunction(setter, ForDefinition);
  preEmitFunction(setter, prop, f, ThunkBodyLoc);
  PrettyStackTraceSILFunction X("silgen objc property setter thunk", f);
  f->setBare(IsBare);
  f->setThunk(IsThunk);
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
  SILDeclRef thunk(destructor,
                   SILDeclRef::Kind::Deallocator,
                   SILDeclRef::ConstructAtBestResilienceExpansion,
                   SILDeclRef::ConstructAtNaturalUncurryLevel,
                   /*isObjC*/ true);

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

void SILGenModule::visitTopLevelCodeDecl(TopLevelCodeDecl *td) {
  assert(TopLevelSGF && "top-level code in a non-main source file!");

  if (!TopLevelSGF->B.hasValidInsertionPoint())
    return;

  ProfilerRAII Profiler(*this, td);
  TopLevelSGF->emitProfilerIncrement(td->getBody());
 
  for (auto &ESD : td->getBody()->getElements()) {
    if (!TopLevelSGF->B.hasValidInsertionPoint()) {
      if (Stmt *S = ESD.dyn_cast<Stmt*>()) {
        if (S->isImplicit())
          continue;
      } else if (Expr *E = ESD.dyn_cast<Expr*>()) {
        if (E->isImplicit())
          continue;
      }

      diagnose(ESD.getStartLoc(), diag::unreachable_code);
      // There's no point in trying to emit anything else.
      return;
    }

    if (Stmt *S = ESD.dyn_cast<Stmt*>()) {
      TopLevelSGF->emitStmt(S);
    } else if (Expr *E = ESD.dyn_cast<Expr*>()) {
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
      sgm.TopLevelSGF->prepareEpilog(Type(), false,
                                 CleanupLocation::getModuleCleanupLocation());

      sgm.TopLevelSGF->prepareRethrowEpilog(
                                 CleanupLocation::getModuleCleanupLocation());

      // Create the argc and argv arguments.
      auto PrologueLoc = RegularLocation::getModuleLocation();
      PrologueLoc.markAsPrologue();
      auto entry = sgm.TopLevelSGF->B.getInsertionBB();
      auto paramTypeIter =
          sgm.TopLevelSGF->F.getConventions().getParameterSILTypes().begin();
      entry->createFunctionArgument(*paramTypeIter);
      entry->createFunctionArgument(*std::next(paramTypeIter));

      scope.emplace(sgm.TopLevelSGF->Cleanups,
                    CleanupLocation::getModuleCleanupLocation());
    }
  }

  ~SourceFileScope() {
    if (sgm.TopLevelSGF) {
      scope.reset();

      // Unregister the top-level function emitter.
      auto &gen = *sgm.TopLevelSGF;
      sgm.TopLevelSGF = nullptr;

      // Write out the epilog.
      auto moduleLoc = RegularLocation::getModuleLocation();
      moduleLoc.markAutoGenerated();
      auto returnInfo = gen.emitEpilogBB(moduleLoc);
      auto returnLoc = returnInfo.second;
      returnLoc.markAutoGenerated();

      SILType returnType = gen.F.getConventions().getSingleSILResultType();
      auto emitTopLevelReturnValue = [&](unsigned value) -> SILValue {
        // Create an integer literal for the value.
        auto litType = SILType::getBuiltinIntegerType(32, sgm.getASTContext());
        SILValue retValue =
          gen.B.createIntegerLiteral(moduleLoc, litType, value);

        // Wrap that in a struct if necessary.
        if (litType != returnType) {
          retValue = gen.B.createStruct(moduleLoc, returnType, retValue);
        }
        return retValue;
      };

      // Fallthrough should signal a normal exit by returning 0.
      SILValue returnValue;
      if (gen.B.hasValidInsertionPoint())
        returnValue = emitTopLevelReturnValue(0);

      // Handle the implicit rethrow block.
      auto rethrowBB = gen.ThrowDest.getBlock();
      gen.ThrowDest = JumpDest::invalid();

      // If the rethrow block wasn't actually used, just remove it.
      if (rethrowBB->pred_empty()) {
        gen.eraseBasicBlock(rethrowBB);

      // Otherwise, we need to produce a unified return block.
      } else {
        auto returnBB = gen.createBasicBlock();
        if (gen.B.hasValidInsertionPoint())
          gen.B.createBranch(returnLoc, returnBB, returnValue);
        returnValue =
            returnBB->createPHIArgument(returnType, ValueOwnershipKind::Owned);
        gen.B.emitBlock(returnBB);

        // Emit the rethrow block.
        SavedInsertionPoint savedIP(gen, rethrowBB,
                                    FunctionSection::Postmatter);

        // Log the error.
        SILValue error = rethrowBB->getArgument(0);
        gen.B.createBuiltin(moduleLoc,
                            sgm.getASTContext().getIdentifier("errorInMain"),
                            sgm.Types.getEmptyTupleType(), {}, {error});

        // Signal an abnormal exit by returning 1.
        gen.Cleanups.emitCleanupsForReturn(CleanupLocation::get(moduleLoc));
        gen.B.createBranch(returnLoc, returnBB, emitTopLevelReturnValue(1));
      }

      // Return.
      if (gen.B.hasValidInsertionPoint())
        gen.B.createReturn(returnLoc, returnValue);

      // Okay, we're done emitting the top-level function; destroy the
      // emitter and verify the result.
      SILFunction *toplevel = &gen.getFunction();
      delete &gen;

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
      SILGenFunction gen(sgm, *toplevel);
      auto entry = gen.B.getInsertionBB();
      auto paramTypeIter =
          gen.F.getConventions().getParameterSILTypes().begin();
      entry->createFunctionArgument(*paramTypeIter);
      entry->createFunctionArgument(*std::next(paramTypeIter));
      gen.emitArtificialTopLevel(mainClass);
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
                        Optional<unsigned> startElem, bool makeModuleFragile,
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

  std::unique_ptr<SILModule> M(new SILModule(mod, options, DC, isWholeModule));
  SILGenModule SGM(*M, mod, makeModuleFragile);

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
