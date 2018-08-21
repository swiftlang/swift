//===--- SILGenFunction.cpp - Top-level lowering for functions ------------===//
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
//
//  This file defines the primary routines for creating and emitting
//  functions.
//
//===----------------------------------------------------------------------===//

#include "SILGenFunction.h"
#include "RValue.h"
#include "SILGenFunctionBuilder.h"
#include "Scope.h"
#include "swift/AST/Initializer.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILProfiler.h"
#include "swift/SIL/SILUndef.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
// SILGenFunction Class implementation
//===----------------------------------------------------------------------===//

SILGenFunction::SILGenFunction(SILGenModule &SGM, SILFunction &F,
                               DeclContext *DC)
    : SGM(SGM), F(F), silConv(SGM.M), FunctionDC(DC),
      StartOfPostmatter(F.end()), B(*this), OpenedArchetypesTracker(&F),
      CurrentSILLoc(F.getLocation()), Cleanups(*this),
      StatsTracer(SGM.M.getASTContext().Stats, "SILGen-function", &F) {
  assert(DC && "creating SGF without a DeclContext?");
  B.setInsertionPoint(createBasicBlock());
  B.setCurrentDebugScope(F.getDebugScope());
  B.setOpenedArchetypesTracker(&OpenedArchetypesTracker);
}

/// SILGenFunction destructor - called after the entire function's AST has been
/// visited.  This handles "falling off the end of the function" logic.
SILGenFunction::~SILGenFunction() {
  // If the end of the function isn't terminated, we screwed up somewhere.
  assert(!B.hasValidInsertionPoint() &&
         "SILGenFunction did not terminate function?!");

  // If we didn't clean up the rethrow destination, we screwed up somewhere.
  assert(!ThrowDest.isValid() &&
         "SILGenFunction did not emit throw destination");
}

//===----------------------------------------------------------------------===//
// Function emission
//===----------------------------------------------------------------------===//

// Get the #function name for a declaration.
DeclName SILGenModule::getMagicFunctionName(DeclContext *dc) {
  // For closures, use the parent name.
  if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
    return getMagicFunctionName(closure->getParent());
  }
  if (auto absFunc = dyn_cast<AbstractFunctionDecl>(dc)) {
    // If this is an accessor, use the name of the storage.
    if (auto accessor = dyn_cast<AccessorDecl>(absFunc))
      return accessor->getStorage()->getFullName();
    if (auto func = dyn_cast<FuncDecl>(absFunc)) {
      // If this is a defer body, use the parent name.
      if (func->isDeferBody()) {
        return getMagicFunctionName(func->getParent());
      }
    }

    return absFunc->getFullName();
  }
  if (auto init = dyn_cast<Initializer>(dc)) {
    return getMagicFunctionName(init->getParent());
  }
  if (auto nominal = dyn_cast<NominalTypeDecl>(dc)) {
    return nominal->getName();
  }
  if (auto tl = dyn_cast<TopLevelCodeDecl>(dc)) {
    return tl->getModuleContext()->getName();
  }
  if (auto fu = dyn_cast<FileUnit>(dc)) {
    return fu->getParentModule()->getName();
  }
  if (auto m = dyn_cast<ModuleDecl>(dc)) {
    return m->getName();
  }
  if (auto e = dyn_cast<ExtensionDecl>(dc)) {
    assert(e->getExtendedNominal() && "extension for nonnominal");
    return e->getExtendedNominal()->getName();
  }
  llvm_unreachable("unexpected #function context");
}

DeclName SILGenModule::getMagicFunctionName(SILDeclRef ref) {
  switch (ref.kind) {
  case SILDeclRef::Kind::Func:
    if (auto closure = ref.getAbstractClosureExpr())
      return getMagicFunctionName(closure);
    return getMagicFunctionName(cast<FuncDecl>(ref.getDecl()));
  case SILDeclRef::Kind::Initializer:
  case SILDeclRef::Kind::Allocator:
    return getMagicFunctionName(cast<ConstructorDecl>(ref.getDecl()));
  case SILDeclRef::Kind::Deallocator:
  case SILDeclRef::Kind::Destroyer:
    return getMagicFunctionName(cast<DestructorDecl>(ref.getDecl()));
  case SILDeclRef::Kind::GlobalAccessor:
    return getMagicFunctionName(cast<VarDecl>(ref.getDecl())->getDeclContext());
  case SILDeclRef::Kind::DefaultArgGenerator:
    return getMagicFunctionName(cast<AbstractFunctionDecl>(ref.getDecl()));
  case SILDeclRef::Kind::StoredPropertyInitializer:
    return getMagicFunctionName(cast<VarDecl>(ref.getDecl())->getDeclContext());
  case SILDeclRef::Kind::IVarInitializer:
    return getMagicFunctionName(cast<ClassDecl>(ref.getDecl()));
  case SILDeclRef::Kind::IVarDestroyer:
    return getMagicFunctionName(cast<ClassDecl>(ref.getDecl()));
  case SILDeclRef::Kind::EnumElement:
    return getMagicFunctionName(cast<EnumElementDecl>(ref.getDecl())
                                  ->getDeclContext());
  }

  llvm_unreachable("Unhandled SILDeclRefKind in switch.");
}

std::tuple<ManagedValue, SILType>
SILGenFunction::emitSiblingMethodRef(SILLocation loc,
                                     SILValue selfValue,
                                     SILDeclRef methodConstant,
                                     SubstitutionMap subMap) {
  SILValue methodValue;

  // If the method is dynamic, access it through runtime-hookable virtual
  // dispatch (viz. objc_msgSend for now).
  if (methodConstant.hasDecl()
      && methodConstant.getDecl()->isObjCDynamic()) {
    methodValue = emitDynamicMethodRef(
                      loc, methodConstant,
                      SGM.Types.getConstantInfo(methodConstant).SILFnType)
                      .getValue();
  } else {
    methodValue = emitGlobalFunctionRef(loc, methodConstant);
  }

  SILType methodTy = methodValue->getType();

  // Specialize the generic method.
  methodTy = methodTy.substGenericArgs(SGM.M, subMap);

  return std::make_tuple(ManagedValue::forUnmanaged(methodValue),
                         methodTy);
}

void SILGenFunction::emitCaptures(SILLocation loc,
                                  AnyFunctionRef closure,
                                  CaptureEmission purpose,
                                  SmallVectorImpl<ManagedValue> &capturedArgs) {
  auto captureInfo = SGM.Types.getLoweredLocalCaptures(closure);
  // For boxed captures, we need to mark the contained variables as having
  // escaped for DI diagnostics.
  SmallVector<SILValue, 2> escapesToMark;
  
  // Partial applications take ownership of the context parameters, so we'll
  // need to pass ownership rather than merely guaranteeing parameters.
  bool canGuarantee;
  switch (purpose) {
  case CaptureEmission::PartialApplication:
    canGuarantee = false;
    break;
  case CaptureEmission::ImmediateApplication:
    canGuarantee = true;
    break;
  }
  
  for (auto capture : captureInfo.getCaptures()) {
    if (capture.isDynamicSelfMetadata()) {
      // The parameter type is the static Self type, but the value we
      // want to pass is the dynamic Self type, so upcast it.
      auto dynamicSelfMetatype = MetatypeType::get(
        captureInfo.getDynamicSelfType());
      SILType dynamicSILType = getLoweredType(dynamicSelfMetatype);

      SILValue value = B.createMetatype(loc, dynamicSILType);
      capturedArgs.push_back(ManagedValue::forUnmanaged(value));
      continue;
    }

    auto *vd = capture.getDecl();

    switch (SGM.Types.getDeclCaptureKind(capture)) {
    case CaptureKind::None:
      break;

    case CaptureKind::Constant: {
      // let declarations.
      auto found = VarLocs.find(vd);
      assert(found != VarLocs.end());
      auto Entry = found->second;

      auto *var = cast<VarDecl>(vd);
      auto &tl = getTypeLowering(var->getType()->getReferenceStorageReferent());
      SILValue Val = Entry.value;

      if (!Val->getType().isAddress()) {
        // Our 'let' binding can guarantee the lifetime for the callee,
        // if we don't need to do anything more to it.
        if (canGuarantee && !var->getType()->is<ReferenceStorageType>()) {
          auto guaranteed = ManagedValue::forUnmanaged(Val).borrow(*this, loc);
          capturedArgs.push_back(guaranteed);
          break;
        }
      
        // Just retain a by-val let.
        Val = B.emitCopyValueOperation(loc, Val);
      } else {
        // If we have a mutable binding for a 'let', such as 'self' in an
        // 'init' method, load it.
        Val = emitLoad(loc, Val, tl, SGFContext(), IsNotTake).forward(*this);
      }

      // If we're capturing an unowned pointer by value, we will have just
      // loaded it into a normal retained class pointer, but we capture it as
      // an unowned pointer.  Convert back now.
      if (var->getType()->is<ReferenceStorageType>()) {
        auto type = getLoweredType(var->getType());
        Val = emitConversionFromSemanticValue(loc, Val, type);
      }

      capturedArgs.push_back(emitManagedRValueWithCleanup(Val));
      break;
    }

    case CaptureKind::StorageAddress: {
      // No-escaping stored declarations are captured as the
      // address of the value.
      assert(VarLocs.count(vd) && "no location for captured var!");
      VarLoc vl = VarLocs[vd];
      assert(vl.value->getType().isAddress() && "no address for captured var!");
      capturedArgs.push_back(ManagedValue::forLValue(vl.value));
      break;
    }

    case CaptureKind::Box: {
      // LValues are captured as both the box owning the value and the
      // address of the value.
      assert(VarLocs.count(vd) && "no location for captured var!");
      VarLoc vl = VarLocs[vd];
      assert(vl.value->getType().isAddress() && "no address for captured var!");

      // If this is a boxed variable, we can use it directly.
      if (vl.box) {
        // We can guarantee our own box to the callee.
        if (canGuarantee) {
          capturedArgs.push_back(
              ManagedValue::forUnmanaged(vl.box).borrow(*this, loc));
        } else {
          capturedArgs.push_back(emitManagedRetain(loc, vl.box));
        }
        escapesToMark.push_back(vl.value);
      } else {
        // Address only 'let' values are passed by box.  This isn't great, in
        // that a variable captured by multiple closures will be boxed for each
        // one.  This could be improved by doing an "isCaptured" analysis when
        // emitting address-only let constants, and emit them into an alloc_box
        // like a variable instead of into an alloc_stack.
        //
        // TODO: This might not be profitable anymore with guaranteed captures,
        // since we could conceivably forward the copied value into the
        // closure context and pass it down to the partially applied function
        // in-place.
        // TODO: Use immutable box for immutable captures.
        auto boxTy = SGM.Types.getContextBoxTypeForCapture(vd,
                                       vl.value->getType().getASTType(),
                                       F.getGenericEnvironment(),
                                       /*mutable*/ true);
        
        AllocBoxInst *allocBox = B.createAllocBox(loc, boxTy);
        ProjectBoxInst *boxAddress = B.createProjectBox(loc, allocBox, 0);
        B.createCopyAddr(loc, vl.value, boxAddress, IsNotTake,
                         IsInitialization);
        if (canGuarantee)
          capturedArgs.push_back(
              emitManagedRValueWithCleanup(allocBox).borrow(*this, loc));
        else
          capturedArgs.push_back(emitManagedRValueWithCleanup(allocBox));
      }

      break;
    }
    }
  }
  
  // Mark box addresses as captured for DI purposes. The values must have
  // been fully initialized before we close over them.
  if (!escapesToMark.empty()) {
    B.createMarkFunctionEscape(loc, escapesToMark);
  }
}

ManagedValue
SILGenFunction::emitClosureValue(SILLocation loc, SILDeclRef constant,
                                 CanType expectedType,
                                 SubstitutionMap subs) {
  auto closure = *constant.getAnyFunctionRef();
  auto captureInfo = closure.getCaptureInfo();
  auto loweredCaptureInfo = SGM.Types.getLoweredLocalCaptures(closure);
  auto hasCaptures = SGM.Types.hasLoweredLocalCaptures(closure);

  auto constantInfo = getConstantInfo(constant);
  SILValue functionRef = emitGlobalFunctionRef(loc, constant, constantInfo);
  SILType functionTy = functionRef->getType();

  // Apply substitutions.
  auto pft = constantInfo.SILFnType;

  auto *dc = closure.getAsDeclContext()->getParent();
  if (dc->isLocalContext() && !loweredCaptureInfo.hasGenericParamCaptures()) {
    // If the lowered function type is not polymorphic but we were given
    // substitutions, we have a closure in a generic context which does not
    // capture generic parameters. Just drop the substitutions.
    subs = { };
  } else if (closure.getAbstractClosureExpr()) {
    // If we have a closure expression in generic context, Sema won't give
    // us substitutions, so we just use the forwarding substitutions from
    // context.
    subs = getForwardingSubstitutionMap();
  }

  bool wasSpecialized = false;
  if (!subs.empty()) {
    auto specialized = pft->substGenericArgs(F.getModule(), subs);
    functionTy = SILType::getPrimitiveObjectType(specialized);
    wasSpecialized = true;
  }

  // If we're in top-level code, we don't need to physically capture script
  // globals, but we still need to mark them as escaping so that DI can flag
  // uninitialized uses.
  if (this == SGM.TopLevelSGF) {
    SGM.emitMarkFunctionEscapeForTopLevelCodeGlobals(
        loc, captureInfo);
  }

  if (!hasCaptures && !wasSpecialized) {
    auto result = ManagedValue::forUnmanaged(functionRef);
    return emitOrigToSubstValue(loc, result,
                                AbstractionPattern(expectedType),
                                expectedType);
  }

  SmallVector<ManagedValue, 4> capturedArgs;
  emitCaptures(loc, closure, CaptureEmission::PartialApplication,
               capturedArgs);

  // The partial application takes ownership of the context parameters.
  SmallVector<SILValue, 4> forwardedArgs;
  for (auto capture : capturedArgs)
    forwardedArgs.push_back(capture.forward(*this));

  auto calleeConvention = ParameterConvention::Direct_Guaranteed;

  SILType closureTy = SILGenBuilder::getPartialApplyResultType(
      functionRef->getType(), capturedArgs.size(), SGM.M, subs,
      calleeConvention);

  auto toClosure =
    B.createPartialApply(loc, functionRef, functionTy,
                         subs, forwardedArgs, closureTy);
  auto result = emitManagedRValueWithCleanup(toClosure);

  // Get the lowered AST types:
  //  - the original type
  auto origFormalType = AbstractionPattern(constantInfo.LoweredType);

  // - the substituted type
  auto substFormalType = expectedType;

  // Generalize if necessary.
  result = emitOrigToSubstValue(loc, result, origFormalType,
                                substFormalType);

  return result;
}

void SILGenFunction::emitFunction(FuncDecl *fd) {
  MagicFunctionName = SILGenModule::getMagicFunctionName(fd);

  emitProlog(fd, fd->getParameters(), fd->getImplicitSelfDecl(),
             fd->getResultInterfaceType(), fd->hasThrows());
  Type resultTy = fd->mapTypeIntoContext(fd->getResultInterfaceType());
  prepareEpilog(resultTy, fd->hasThrows(), CleanupLocation(fd));

  emitProfilerIncrement(fd->getBody());
  emitStmt(fd->getBody());

  emitEpilog(fd);

  mergeCleanupBlocks();
}

void SILGenFunction::emitClosure(AbstractClosureExpr *ace) {
  MagicFunctionName = SILGenModule::getMagicFunctionName(ace);

  auto resultIfaceTy = ace->getResultType()->mapTypeOutOfContext();
  emitProlog(ace, ace->getParameters(), /*selfParam=*/nullptr,
             resultIfaceTy, ace->isBodyThrowing());
  prepareEpilog(ace->getResultType(), ace->isBodyThrowing(),
                CleanupLocation(ace));
  emitProfilerIncrement(ace);
  if (auto *ce = dyn_cast<ClosureExpr>(ace)) {
    emitStmt(ce->getBody());
  } else {
    auto *autoclosure = cast<AutoClosureExpr>(ace);
    // Closure expressions implicitly return the result of their body
    // expression.
    emitReturnExpr(ImplicitReturnLocation(ace),
                   autoclosure->getSingleExpressionBody());
  }
  emitEpilog(ace);
}

void SILGenFunction::emitArtificialTopLevel(ClassDecl *mainClass) {
  // Load argc and argv from the entry point arguments.
  SILValue argc = F.begin()->getArgument(0);
  SILValue argv = F.begin()->getArgument(1);

  switch (mainClass->getArtificialMainKind()) {
  case ArtificialMainKind::UIApplicationMain: {
    // Emit a UIKit main.
    // return UIApplicationMain(C_ARGC, C_ARGV, nil, ClassName);

    CanType NSStringTy = SGM.Types.getNSStringType();
    CanType OptNSStringTy
      = OptionalType::get(NSStringTy)->getCanonicalType();

    // Look up UIApplicationMain.
    // FIXME: Doing an AST lookup here is gross and not entirely sound;
    // we're getting away with it because the types are guaranteed to already
    // be imported.
    ASTContext &ctx = getASTContext();
    
    std::pair<Identifier, SourceLoc> UIKitName =
      {ctx.getIdentifier("UIKit"), SourceLoc()};
    
    ModuleDecl *UIKit = ctx
      .getClangModuleLoader()
      ->loadModule(SourceLoc(), UIKitName);
    assert(UIKit && "couldn't find UIKit objc module?!");
    SmallVector<ValueDecl *, 1> results;
    UIKit->lookupQualified(UIKit,
                           ctx.getIdentifier("UIApplicationMain"),
                           NL_QualifiedDefault,
                           results);
    assert(results.size() == 1
           && "couldn't find a unique UIApplicationMain in the UIKit ObjC "
              "module?!");

    ValueDecl *UIApplicationMainDecl = results.front();

    auto mainRef = SILDeclRef(UIApplicationMainDecl).asForeign();
    SILGenFunctionBuilder builder(SGM);
    auto UIApplicationMainFn =
        builder.getOrCreateFunction(mainClass, mainRef, NotForDefinition);
    auto fnTy = UIApplicationMainFn->getLoweredFunctionType();
    SILFunctionConventions fnConv(fnTy, SGM.M);

    // Get the class name as a string using NSStringFromClass.
    CanType mainClassTy = mainClass->getDeclaredInterfaceType()
        ->getCanonicalType();
    CanType mainClassMetaty = CanMetatypeType::get(mainClassTy,
                                                   MetatypeRepresentation::ObjC);
    CanType anyObjectTy = ctx.getAnyObjectType();
    CanType anyObjectMetaTy = CanExistentialMetatypeType::get(anyObjectTy,
                                                  MetatypeRepresentation::ObjC);

    auto NSStringFromClassType = SILFunctionType::get(nullptr,
                  SILFunctionType::ExtInfo()
                    .withRepresentation(SILFunctionType::Representation::
                                        CFunctionPointer),
                  SILCoroutineKind::None,
                  ParameterConvention::Direct_Unowned,
                  SILParameterInfo(anyObjectMetaTy,
                                   ParameterConvention::Direct_Unowned),
                  /*yields*/ {},
                  SILResultInfo(OptNSStringTy,
                                ResultConvention::Autoreleased),
                  /*error result*/ None,
                  ctx);
    auto NSStringFromClassFn = builder.getOrCreateFunction(
        mainClass, "NSStringFromClass", SILLinkage::PublicExternal,
        NSStringFromClassType, IsBare, IsTransparent, IsNotSerialized);
    auto NSStringFromClass = B.createFunctionRef(mainClass, NSStringFromClassFn);
    SILValue metaTy = B.createMetatype(mainClass,
                             SILType::getPrimitiveObjectType(mainClassMetaty));
    metaTy = B.createInitExistentialMetatype(mainClass, metaTy,
                          SILType::getPrimitiveObjectType(anyObjectMetaTy), {});
    SILValue optNameValue = B.createApply(
        mainClass, NSStringFromClass, NSStringFromClass->getType(),
        SILType::getPrimitiveObjectType(OptNSStringTy), {}, metaTy);
    ManagedValue optName = emitManagedRValueWithCleanup(optNameValue);

    // Fix up the string parameters to have the right type.
    SILType nameArgTy = fnConv.getSILArgumentType(3);
    assert(nameArgTy == fnConv.getSILArgumentType(2));
    (void)nameArgTy;
    assert(optName.getType() == nameArgTy);
    SILValue nilValue =
        getOptionalNoneValue(mainClass, getTypeLowering(OptNSStringTy));

    // Fix up argv to have the right type.
    auto argvTy = fnConv.getSILArgumentType(1);

    SILType unwrappedTy = argvTy;
    if (Type innerTy = argvTy.getASTType()->getOptionalObjectType()) {
      auto canInnerTy = innerTy->getCanonicalType();
      unwrappedTy = SILType::getPrimitiveObjectType(canInnerTy);
    }

    auto managedArgv = ManagedValue::forUnmanaged(argv);

    if (unwrappedTy != argv->getType()) {
      auto converted =
          emitPointerToPointer(mainClass, managedArgv,
                               argv->getType().getASTType(),
                               unwrappedTy.getASTType());
      managedArgv = std::move(converted).getAsSingleValue(*this, mainClass);
    }

    if (unwrappedTy != argvTy) {
      managedArgv = getOptionalSomeValue(mainClass, managedArgv,
                                         getTypeLowering(argvTy));
    }

    auto UIApplicationMain = B.createFunctionRef(mainClass, UIApplicationMainFn);

    SILValue args[] = {argc, managedArgv.getValue(), nilValue,
                       optName.getValue()};

    B.createApply(mainClass, UIApplicationMain,
                  UIApplicationMain->getType(),
                  argc->getType(), {}, args);
    SILValue r = B.createIntegerLiteral(mainClass,
                        SILType::getBuiltinIntegerType(32, ctx), 0);
    auto rType = F.getConventions().getSingleSILResultType();
    if (r->getType() != rType)
      r = B.createStruct(mainClass, rType, r);

    Cleanups.emitCleanupsForReturn(mainClass, NotForUnwind);
    B.createReturn(mainClass, r);
    return;
  }

  case ArtificialMainKind::NSApplicationMain: {
    // Emit an AppKit main.
    // return NSApplicationMain(C_ARGC, C_ARGV);

    SILParameterInfo argTypes[] = {
      SILParameterInfo(argc->getType().getASTType(),
                       ParameterConvention::Direct_Unowned),
      SILParameterInfo(argv->getType().getASTType(),
                       ParameterConvention::Direct_Unowned),
    };
    auto NSApplicationMainType = SILFunctionType::get(nullptr,
                  SILFunctionType::ExtInfo()
                    // Should be C calling convention, but NSApplicationMain
                    // has an overlay to fix the type of argv.
                    .withRepresentation(SILFunctionType::Representation::Thin),
                  SILCoroutineKind::None,
                  ParameterConvention::Direct_Unowned,
                  argTypes,
                  /*yields*/ {},
                  SILResultInfo(argc->getType().getASTType(),
                                ResultConvention::Unowned),
                  /*error result*/ None,
                  getASTContext());

    SILGenFunctionBuilder builder(SGM);
    auto NSApplicationMainFn = builder.getOrCreateFunction(
        mainClass, "NSApplicationMain", SILLinkage::PublicExternal,
        NSApplicationMainType, IsBare, IsTransparent, IsNotSerialized);

    auto NSApplicationMain = B.createFunctionRef(mainClass, NSApplicationMainFn);
    SILValue args[] = { argc, argv };

    B.createApply(mainClass, NSApplicationMain,
                  NSApplicationMain->getType(),
                  argc->getType(), {}, args);
    SILValue r = B.createIntegerLiteral(mainClass,
                        SILType::getBuiltinIntegerType(32, getASTContext()), 0);
    auto rType = F.getConventions().getSingleSILResultType();
    if (r->getType() != rType)
      r = B.createStruct(mainClass, rType, r);
    B.createReturn(mainClass, r);
    return;
  }
  }
}

void SILGenFunction::emitGeneratorFunction(SILDeclRef function, Expr *value) {
  MagicFunctionName = SILGenModule::getMagicFunctionName(function);

  RegularLocation Loc(value);
  Loc.markAutoGenerated();

  // Default argument generators of function typed values return noescape
  // functions. Strip the escape to noescape function conversion.
  if (function.kind == SILDeclRef::Kind::DefaultArgGenerator) {
    if (auto funType = value->getType()->getAs<AnyFunctionType>()) {
      if (funType->getExtInfo().isNoEscape()) {
        auto conv = cast<FunctionConversionExpr>(value);
        value = conv->getSubExpr();
        assert(funType->withExtInfo(funType->getExtInfo().withNoEscape(false))
                   ->isEqual(value->getType()));
      }
    }
  }

  auto *dc = function.getDecl()->getInnermostDeclContext();
  auto interfaceType = value->getType()->mapTypeOutOfContext();
  emitProlog(/*paramList=*/nullptr, /*selfParam=*/nullptr, interfaceType,
             dc, false);
  prepareEpilog(value->getType(), false, CleanupLocation::get(Loc));
  emitReturnExpr(Loc, value);
  emitEpilog(Loc);
}

static SILLocation getLocation(ASTNode Node) {
  if (auto *E = Node.dyn_cast<Expr *>())
    return E;
  else if (auto *S = Node.dyn_cast<Stmt *>())
    return S;
  else if (auto *D = Node.dyn_cast<Decl *>())
    return D;
  else
    llvm_unreachable("unsupported ASTNode");
}

void SILGenFunction::emitProfilerIncrement(ASTNode N) {
  // Ignore functions which aren't set up for instrumentation.
  SILProfiler *SP = F.getProfiler();
  if (!SP)
    return;
  if (!SP->hasRegionCounters() || !getModule().getOptions().UseProfile.empty())
    return;

  auto &C = B.getASTContext();
  const auto &RegionCounterMap = SP->getRegionCounterMap();
  auto CounterIt = RegionCounterMap.find(N);

  // TODO: Assert that this cannot happen (rdar://42792053).
  if (CounterIt == RegionCounterMap.end())
    return;

  auto Int32Ty = getLoweredType(BuiltinIntegerType::get(32, C));
  auto Int64Ty = getLoweredType(BuiltinIntegerType::get(64, C));

  SILLocation Loc = getLocation(N);
  SILValue Args[] = {
      // The intrinsic must refer to the function profiling name var, which is
      // inaccessible during SILGen. Rely on irgen to rewrite the function name.
      B.createStringLiteral(Loc, SP->getPGOFuncName(),
                            StringLiteralInst::Encoding::UTF8),
      B.createIntegerLiteral(Loc, Int64Ty, SP->getPGOFuncHash()),
      B.createIntegerLiteral(Loc, Int32Ty, SP->getNumRegionCounters()),
      B.createIntegerLiteral(Loc, Int32Ty, CounterIt->second)};
  B.createBuiltin(Loc, C.getIdentifier("int_instrprof_increment"),
                  SGM.Types.getEmptyTupleType(), {}, Args);
}

ProfileCounter SILGenFunction::loadProfilerCount(ASTNode Node) const {
  if (SILProfiler *SP = F.getProfiler())
    return SP->getExecutionCount(Node);
  return ProfileCounter();
}

Optional<ASTNode> SILGenFunction::getPGOParent(ASTNode Node) const {
  if (SILProfiler *SP = F.getProfiler())
    return SP->getPGOParent(Node);
  return None;
}
