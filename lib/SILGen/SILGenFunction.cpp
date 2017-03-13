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
#include "Scope.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"

#include "swift/AST/DiagnosticsSIL.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
// SILGenFunction Class implementation
//===----------------------------------------------------------------------===//

SILGenFunction::SILGenFunction(SILGenModule &SGM, SILFunction &F)
    : SGM(SGM), F(F), silConv(SGM.M), StartOfPostmatter(F.end()),
      B(*this, createBasicBlock()), OpenedArchetypesTracker(F),
      CurrentSILLoc(F.getLocation()), Cleanups(*this) {
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
    if (auto func = dyn_cast<FuncDecl>(absFunc)) {
      // If this is an accessor, use the name of the storage.
      if (auto storage = func->getAccessorStorageDecl()) {
        return storage->getFullName();
      }
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
    assert(e->getExtendedType()->getAnyNominal() && "extension for nonnominal");
    return e->getExtendedType()->getAnyNominal()->getName();
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
  case SILDeclRef::Kind::GlobalGetter:
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

SILValue SILGenFunction::emitGlobalFunctionRef(SILLocation loc,
                                               SILDeclRef constant,
                                               SILConstantInfo constantInfo) {
  assert(constantInfo == getConstantInfo(constant));

  // Builtins must be fully applied at the point of reference.
  if (constant.hasDecl() &&
      isa<BuiltinUnit>(constant.getDecl()->getDeclContext())) {
    SGM.diagnose(loc.getSourceLoc(), diag::not_implemented,
                 "delayed application of builtin");
    return SILUndef::get(constantInfo.getSILType(), SGM.M);
  }
  
  // If the constant is a thunk we haven't emitted yet, emit it.
  if (!SGM.hasFunction(constant)) {
    if (constant.isCurried) {
      auto vd = constant.getDecl();
      // Reference the next uncurrying level of the function.
      SILDeclRef next = SILDeclRef(vd, constant.kind,
                                 SILDeclRef::ConstructAtBestResilienceExpansion,
                                 constant.uncurryLevel + 1);
      // If the function is fully uncurried and natively foreign, reference its
      // foreign entry point.
      if (!next.isCurried) {
        if (requiresForeignToNativeThunk(vd))
          next = next.asForeign();
      }
      
      // Preserve whether the curry thunks lead to a direct reference to the
      // method implementation.
      next = next.asDirectReference(constant.isDirectReference);

      SGM.emitCurryThunk(vd, constant, next);
    }
    // Otherwise, if this is a calling convention thunk we haven't emitted yet,
    // emit it.
    else if (constant.isForeignToNativeThunk()) {
      SGM.emitForeignToNativeThunk(constant);
    } else if (constant.isNativeToForeignThunk()) {
      SGM.emitNativeToForeignThunk(constant);
    } else if (constant.kind == SILDeclRef::Kind::EnumElement) {
      SGM.emitEnumConstructor(cast<EnumElementDecl>(constant.getDecl()));
    }
  }

  auto f = SGM.getFunction(constant, NotForDefinition);
  assert(f->getLoweredFunctionType() == constantInfo.SILFnType);
  return B.createFunctionRef(loc, f);
}

std::tuple<ManagedValue, SILType>
SILGenFunction::emitSiblingMethodRef(SILLocation loc,
                                     SILValue selfValue,
                                     SILDeclRef methodConstant,
                                     const SubstitutionMap &subMap) {
  SILValue methodValue;

  // If the method is dynamic, access it through runtime-hookable virtual
  // dispatch (viz. objc_msgSend for now).
  if (methodConstant.hasDecl()
      && methodConstant.getDecl()->getAttrs().hasAttribute<DynamicAttr>())
    methodValue = emitDynamicMethodRef(loc, methodConstant,
                                     SGM.Types.getConstantInfo(methodConstant));
  else
    methodValue = emitGlobalFunctionRef(loc, methodConstant);

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
  // TODO: Or we always retain them when guaranteed contexts aren't enabled.
  if (!SGM.M.getOptions().EnableGuaranteedClosureContexts)
    canGuarantee = false;
  
  for (auto capture : captureInfo.getCaptures()) {
    if (capture.isDynamicSelfMetadata()) {
      // The parameter type is the static Self type, but the value we
      // want to pass is the dynamic Self type, so upcast it.
      auto dynamicSelfMetatype = MetatypeType::get(
          captureInfo.getDynamicSelfType(),
          MetatypeRepresentation::Thick)
              ->getCanonicalType();
      auto staticSelfMetatype = MetatypeType::get(
          captureInfo.getDynamicSelfType()->getSelfType(),
          MetatypeRepresentation::Thick)
              ->getCanonicalType();
      SILType dynamicSILType = SILType::getPrimitiveObjectType(
          dynamicSelfMetatype);
      SILType staticSILType = SILType::getPrimitiveObjectType(
          staticSelfMetatype);

      SILValue value = B.createMetatype(loc, dynamicSILType);
      value = B.createUpcast(loc, value, staticSILType);
      capturedArgs.push_back(ManagedValue::forUnmanaged(value));
      continue;
    }

    auto *vd = capture.getDecl();

    switch (SGM.Types.getDeclCaptureKind(capture)) {
    case CaptureKind::None:
      break;

    case CaptureKind::Constant: {
      // let declarations.
      auto Entry = VarLocs[vd];

      auto *var = cast<VarDecl>(vd);
      auto &tl = getTypeLowering(var->getType()->getReferenceStorageReferent());
      SILValue Val = Entry.value;

      if (!Val->getType().isAddress()) {
        // Our 'let' binding can guarantee the lifetime for the callee,
        // if we don't need to do anything more to it.
        if (canGuarantee && !var->getType()->is<ReferenceStorageType>()) {
          auto guaranteed = ManagedValue::forUnmanaged(Val);
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
        auto type = getTypeLowering(var->getType()).getLoweredType();
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
          capturedArgs.push_back(ManagedValue::forUnmanaged(vl.box));
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
                                       vl.value->getType().getSwiftRValueType(),
                                       F.getGenericEnvironment(),
                                       /*mutable*/ true);
        
        AllocBoxInst *allocBox = B.createAllocBox(loc, boxTy);
        ProjectBoxInst *boxAddress = B.createProjectBox(loc, allocBox, 0);
        B.createCopyAddr(loc, vl.value, boxAddress, IsNotTake,IsInitialization);
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
                                 SubstitutionList subs) {
  auto closure = *constant.getAnyFunctionRef();
  auto captureInfo = closure.getCaptureInfo();
  auto loweredCaptureInfo = SGM.Types.getLoweredLocalCaptures(closure);
  auto hasCaptures = SGM.Types.hasLoweredLocalCaptures(closure);
  assert(((constant.uncurryLevel == 1 && hasCaptures) ||
          (constant.uncurryLevel == 0 && !hasCaptures)) &&
         "curried local functions not yet supported");

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
    subs = getForwardingSubstitutions();
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

  SILType closureTy =
    SILGenBuilder::getPartialApplyResultType(functionRef->getType(),
                                             capturedArgs.size(), SGM.M,
                                             subs,
                                             ParameterConvention::Direct_Owned);
  auto toClosure =
    B.createPartialApply(loc, functionRef, functionTy,
                         subs, forwardedArgs, closureTy);
  auto result = emitManagedRValueWithCleanup(toClosure);

  // Get the lowered AST types:
  //  - the original type
  auto origLoweredFormalType =
      AbstractionPattern(constantInfo.LoweredInterfaceType);
  if (hasCaptures) {
    // Get the unlowered formal type of the constant, stripping off
    // the first level of function application, which applies captures.
    origLoweredFormalType =
      AbstractionPattern(constantInfo.FormalInterfaceType)
          .getFunctionResultType();

    // Lower it, being careful to use the right generic signature.
    origLoweredFormalType =
      AbstractionPattern(
          origLoweredFormalType.getGenericSignature(),
          SGM.Types.getLoweredASTFunctionType(
              cast<FunctionType>(origLoweredFormalType.getType()),
              0, constant));
  }

  // - the substituted type
  auto substFormalType = cast<FunctionType>(expectedType);
  auto substLoweredFormalType =
    SGM.Types.getLoweredASTFunctionType(substFormalType, 0, constant);

  // Generalize if necessary.
  result = emitOrigToSubstValue(loc, result, origLoweredFormalType,
                                substLoweredFormalType);

  return result;
}

void SILGenFunction::emitFunction(FuncDecl *fd) {
  MagicFunctionName = SILGenModule::getMagicFunctionName(fd);

  emitProlog(fd, fd->getParameterLists(), fd->getResultInterfaceType(),
             fd->hasThrows());
  Type resultTy = fd->mapTypeIntoContext(fd->getResultInterfaceType());
  prepareEpilog(resultTy, fd->hasThrows(), CleanupLocation(fd));

  emitProfilerIncrement(fd->getBody());
  emitStmt(fd->getBody());

  emitEpilog(fd);
}

void SILGenFunction::emitClosure(AbstractClosureExpr *ace) {
  MagicFunctionName = SILGenModule::getMagicFunctionName(ace);

  auto resultIfaceTy = ace->mapTypeOutOfContext(ace->getResultType());
  emitProlog(ace, ace->getParameters(), resultIfaceTy,
             ace->isBodyThrowing());
  prepareEpilog(ace->getResultType(), ace->isBodyThrowing(),
                CleanupLocation(ace));
  if (auto *ce = dyn_cast<ClosureExpr>(ace)) {
    emitProfilerIncrement(ce);
    emitStmt(ce->getBody());
  } else {
    auto *autoclosure = cast<AutoClosureExpr>(ace);
    // Closure expressions implicitly return the result of their body
    // expression.
    emitProfilerIncrement(autoclosure);
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
    CanType IUOptNSStringTy
      = ImplicitlyUnwrappedOptionalType::get(NSStringTy)->getCanonicalType();

    // Look up UIApplicationMain.
    // FIXME: Doing an AST lookup here is gross and not entirely sound;
    // we're getting away with it because the types are guaranteed to already
    // be imported.
    ASTContext &ctx = getASTContext();
    ModuleDecl *UIKit = ctx.getLoadedModule(ctx.getIdentifier("UIKit"));
    SmallVector<ValueDecl *, 1> results;
    UIKit->lookupQualified(UIKit->getInterfaceType(),
                           ctx.getIdentifier("UIApplicationMain"),
                           NL_QualifiedDefault,
                           /*resolver*/nullptr,
                           results);
    assert(!results.empty() && "couldn't find UIApplicationMain in UIKit");
    assert(results.size() == 1 && "more than one UIApplicationMain?");

    SILDeclRef mainRef{results.front(), ResilienceExpansion::Minimal,
                       SILDeclRef::ConstructAtNaturalUncurryLevel,
                       /*isForeign*/true};
    auto UIApplicationMainFn = SGM.M.getOrCreateFunction(mainClass, mainRef,
                                                         NotForDefinition);
    auto fnTy = UIApplicationMainFn->getLoweredFunctionType();
    SILFunctionConventions fnConv(fnTy, SGM.M);

    // Get the class name as a string using NSStringFromClass.
    CanType mainClassTy = mainClass->getDeclaredInterfaceType()
        ->getCanonicalType();
    CanType mainClassMetaty = CanMetatypeType::get(mainClassTy,
                                                   MetatypeRepresentation::ObjC);
    ProtocolDecl *anyObjectProtocol =
      ctx.getProtocol(KnownProtocolKind::AnyObject);
    auto mainClassAnyObjectConformance = ProtocolConformanceRef(
      *SGM.M.getSwiftModule()->lookupConformance(mainClassTy, anyObjectProtocol,
                                                nullptr));
    CanType anyObjectTy = anyObjectProtocol
      ->getDeclaredInterfaceType()
      ->getCanonicalType();
    CanType anyObjectMetaTy = CanExistentialMetatypeType::get(anyObjectTy,
                                                  MetatypeRepresentation::ObjC);

    auto NSStringFromClassType = SILFunctionType::get(nullptr,
                  SILFunctionType::ExtInfo()
                    .withRepresentation(SILFunctionType::Representation::
                                        CFunctionPointer),
                  ParameterConvention::Direct_Unowned,
                  SILParameterInfo(anyObjectMetaTy,
                                   ParameterConvention::Direct_Unowned),
                  SILResultInfo(OptNSStringTy,
                                ResultConvention::Autoreleased),
                  /*error result*/ None,
                  ctx);
    auto NSStringFromClassFn
      = SGM.M.getOrCreateFunction(mainClass, "NSStringFromClass",
                                  SILLinkage::PublicExternal,
                                  NSStringFromClassType,
                                  IsBare, IsTransparent, IsNotFragile);
    auto NSStringFromClass = B.createFunctionRef(mainClass, NSStringFromClassFn);
    SILValue metaTy = B.createMetatype(mainClass,
                             SILType::getPrimitiveObjectType(mainClassMetaty));
    metaTy = B.createInitExistentialMetatype(mainClass, metaTy,
                          SILType::getPrimitiveObjectType(anyObjectMetaTy),
                          ctx.AllocateCopy(
                            llvm::makeArrayRef(mainClassAnyObjectConformance)));
    SILValue optName = B.createApply(mainClass,
                               NSStringFromClass,
                               NSStringFromClass->getType(),
                               SILType::getPrimitiveObjectType(OptNSStringTy),
                               {}, metaTy);

    // Fix up the string parameters to have the right type.
    SILType nameArgTy = fnConv.getSILArgumentType(3);
    assert(nameArgTy == fnConv.getSILArgumentType(2));
    auto managedName = ManagedValue::forUnmanaged(optName);
    SILValue nilValue;
    if (optName->getType() == nameArgTy) {
      nilValue = getOptionalNoneValue(mainClass,
                                      getTypeLowering(OptNSStringTy));
    } else {
      assert(nameArgTy.getSwiftRValueType() == IUOptNSStringTy);
      nilValue = getOptionalNoneValue(mainClass,
                                      getTypeLowering(IUOptNSStringTy));
      managedName = emitOptionalToOptional(
          mainClass, managedName,
          SILType::getPrimitiveObjectType(IUOptNSStringTy),
          [](SILGenFunction &, SILLocation, ManagedValue input, SILType) {
        return input;
      });
    }

    // Fix up argv to have the right type.
    auto argvTy = fnConv.getSILArgumentType(1);

    SILType unwrappedTy = argvTy;
    if (Type innerTy = argvTy.getSwiftRValueType()->getAnyOptionalObjectType()){
      auto canInnerTy = innerTy->getCanonicalType();
      unwrappedTy = SILType::getPrimitiveObjectType(canInnerTy);
    }

    auto managedArgv = ManagedValue::forUnmanaged(argv);

    if (unwrappedTy != argv->getType()) {
      auto converted =
          emitPointerToPointer(mainClass, managedArgv,
                               argv->getType().getSwiftRValueType(),
                               unwrappedTy.getSwiftRValueType());
      managedArgv = std::move(converted).getAsSingleValue(*this, mainClass);
    }

    if (unwrappedTy != argvTy) {
      managedArgv = getOptionalSomeValue(mainClass, managedArgv,
                                         getTypeLowering(argvTy));
    }

    auto UIApplicationMain = B.createFunctionRef(mainClass, UIApplicationMainFn);

    SILValue args[] = {argc, managedArgv.getValue(), nilValue,
                       managedName.getValue()};

    B.createApply(mainClass, UIApplicationMain,
                  UIApplicationMain->getType(),
                  argc->getType(), {}, args);
    SILValue r = B.createIntegerLiteral(mainClass,
                        SILType::getBuiltinIntegerType(32, ctx), 0);
    auto rType = F.getConventions().getSingleSILResultType();
    if (r->getType() != rType)
      r = B.createStruct(mainClass, rType, r);

    Cleanups.emitCleanupsForReturn(mainClass);
    B.createReturn(mainClass, r);
    return;
  }

  case ArtificialMainKind::NSApplicationMain: {
    // Emit an AppKit main.
    // return NSApplicationMain(C_ARGC, C_ARGV);

    SILParameterInfo argTypes[] = {
      SILParameterInfo(argc->getType().getSwiftRValueType(),
                       ParameterConvention::Direct_Unowned),
      SILParameterInfo(argv->getType().getSwiftRValueType(),
                       ParameterConvention::Direct_Unowned),
    };
    auto NSApplicationMainType = SILFunctionType::get(nullptr,
                  SILFunctionType::ExtInfo()
                    // Should be C calling convention, but NSApplicationMain
                    // has an overlay to fix the type of argv.
                    .withRepresentation(SILFunctionType::Representation::Thin),
                  ParameterConvention::Direct_Unowned,
                  argTypes,
                  SILResultInfo(argc->getType().getSwiftRValueType(),
                                ResultConvention::Unowned),
                  /*error result*/ None,
                  getASTContext());

    auto NSApplicationMainFn
      = SGM.M.getOrCreateFunction(mainClass, "NSApplicationMain",
                                  SILLinkage::PublicExternal,
                                  NSApplicationMainType,
                                  IsBare, IsTransparent, IsNotFragile);

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

static void forwardCaptureArgs(SILGenFunction &gen,
                               SmallVectorImpl<SILValue> &args,
                               CapturedValue capture) {
  auto addSILArgument = [&](SILType t, ValueDecl *d) {
    args.push_back(gen.F.begin()->createFunctionArgument(t, d));
  };

  auto *vd = capture.getDecl();

  switch (gen.SGM.Types.getDeclCaptureKind(capture)) {
  case CaptureKind::None:
    break;

  case CaptureKind::Constant: {
    auto *var = dyn_cast<VarDecl>(vd);
    addSILArgument(gen.getLoweredType(var->getType()), vd);
    break;
  }

  case CaptureKind::Box: {
    // Forward the captured owning box.
    auto *var = cast<VarDecl>(vd);
    auto boxTy = gen.SGM.Types
      .getInterfaceBoxTypeForCapture(vd,
                                     gen.getLoweredType(var->getType())
                                        .getSwiftRValueType(),
                                     /*mutable*/ true);
    addSILArgument(SILType::getPrimitiveObjectType(boxTy), vd);
    break;
  }

  case CaptureKind::StorageAddress: {
    auto *var = dyn_cast<VarDecl>(vd);
    SILType ty = gen.getLoweredType(var->getType()->getRValueType())
      .getAddressType();
    // Forward the captured value address.
    addSILArgument(ty, vd);
    break;
  }
  }
}

static SILValue getNextUncurryLevelRef(SILGenFunction &gen,
                                       SILLocation loc,
                                       SILDeclRef next,
                                       bool direct,
                                       ArrayRef<SILValue> curriedArgs,
                                       SubstitutionList curriedSubs) {
  if (next.isForeign || next.isCurried || !next.hasDecl() || direct)
    return gen.emitGlobalFunctionRef(loc, next.asForeign(false));

  auto constantInfo = gen.SGM.Types.getConstantInfo(next);
  SILValue thisArg;
  if (!curriedArgs.empty())
      thisArg = curriedArgs.back();

  if (isa<AbstractFunctionDecl>(next.getDecl()) &&
      getMethodDispatch(cast<AbstractFunctionDecl>(next.getDecl()))
        == MethodDispatch::Class) {
    SILValue thisArg = curriedArgs.back();

    // Use the dynamic thunk if dynamic.
    if (next.getDecl()->isDynamic()) {
      auto dynamicThunk = gen.SGM.getDynamicThunk(next, constantInfo);
      return gen.B.createFunctionRef(loc, dynamicThunk);
    }

    return gen.B.createClassMethod(loc, thisArg, next);
  }

  // If the fully-uncurried reference is to a generic method, look up the
  // witness.
  if (constantInfo.SILFnType->getRepresentation()
        == SILFunctionTypeRepresentation::WitnessMethod) {
    auto thisType = curriedSubs[0].getReplacement()->getCanonicalType();
    assert(isa<ArchetypeType>(thisType) && "no archetype for witness?!");
    SILValue OpenedExistential;
    if (!cast<ArchetypeType>(thisType)->getOpenedExistentialType().isNull())
      OpenedExistential = thisArg;
    auto protocol =
      next.getDecl()->getDeclContext()->getAsProtocolOrProtocolExtensionContext();
    auto conformance = ProtocolConformanceRef(protocol);
    return gen.B.createWitnessMethod(loc, thisType, conformance, next,
                                     constantInfo.getSILType(),
                                     OpenedExistential);
  }

  // Otherwise, emit a direct call.
  return gen.emitGlobalFunctionRef(loc, next);
}

void SILGenFunction::emitCurryThunk(ValueDecl *vd,
                                    SILDeclRef from, SILDeclRef to) {
  SmallVector<SILValue, 8> curriedArgs;

  unsigned paramCount = from.uncurryLevel + 1;

  if (isa<ConstructorDecl>(vd) || isa<EnumElementDecl>(vd)) {
    // The first body parameter pattern for a constructor specifies the
    // "self" instance, but the constructor is invoked from outside on a
    // metatype.
    assert(from.uncurryLevel == 0 && to.uncurryLevel == 1
           && "currying constructor at level other than one?!");
    F.setBare(IsBare);
    auto selfMetaTy = vd->getInterfaceType()->getAs<AnyFunctionType>()
        ->getInput();
    selfMetaTy = vd->getInnermostDeclContext()->mapTypeIntoContext(selfMetaTy);
    auto metatypeVal =
        F.begin()->createFunctionArgument(getLoweredLoadableType(selfMetaTy));
    curriedArgs.push_back(metatypeVal);

  } else if (auto fd = dyn_cast<AbstractFunctionDecl>(vd)) {
    // Forward implicit closure context arguments.
    bool hasCaptures = SGM.M.Types.hasLoweredLocalCaptures(fd);
    if (hasCaptures)
      --paramCount;

    // Forward the curried formal arguments.
    auto forwardedPatterns = fd->getParameterLists().slice(0, paramCount);
    for (auto *paramPattern : reversed(forwardedPatterns))
      bindParametersForForwarding(paramPattern, curriedArgs);

    // Forward captures.
    if (hasCaptures) {
      auto captureInfo = SGM.Types.getLoweredLocalCaptures(fd);
      for (auto capture : captureInfo.getCaptures())
        forwardCaptureArgs(*this, curriedArgs, capture);
    }
  } else {
    llvm_unreachable("don't know how to curry this decl");
  }

  // Forward substitutions.
  SubstitutionList subs;
  auto constantInfo = getConstantInfo(to);
  if (auto *env = constantInfo.GenericEnv)
    subs = env->getForwardingSubstitutions();

  SILValue toFn = getNextUncurryLevelRef(*this, vd, to, from.isDirectReference,
                                         curriedArgs, subs);
  SILFunctionConventions fromConv(
      SGM.getConstantType(from).castTo<SILFunctionType>(), SGM.M);
  SILType resultTy = fromConv.getSingleSILResultType();
  resultTy = F.mapTypeIntoContext(resultTy);
  auto toTy = toFn->getType();

  // Forward archetypes and specialize if the function is generic.
  if (!subs.empty()) {
    auto toFnTy = toFn->getType().castTo<SILFunctionType>();
    toTy = getLoweredLoadableType(toFnTy->substGenericArgs(SGM.M,  subs));
  }

  // Partially apply the next uncurry level and return the result closure.
  auto closureTy =
    SILGenBuilder::getPartialApplyResultType(toFn->getType(), curriedArgs.size(),
                                             SGM.M, subs,
                                             ParameterConvention::Direct_Owned);
  SILInstruction *toClosure =
    B.createPartialApply(vd, toFn, toTy, subs, curriedArgs, closureTy);
  if (resultTy != closureTy)
    toClosure = B.createConvertFunction(vd, toClosure, resultTy);
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(vd), toClosure);
}

void SILGenFunction::emitGeneratorFunction(SILDeclRef function, Expr *value) {
  MagicFunctionName = SILGenModule::getMagicFunctionName(function);

  RegularLocation Loc(value);
  Loc.markAutoGenerated();

  // Override location for #file, #line etc. to an invalid one so that we
  // don't put extra strings into the default argument generator function that
  // is not going to be ever used anyway.
  overrideLocationForMagicIdentifiers = SourceLoc();

  auto *dc = function.getDecl()->getInnermostDeclContext();
  auto interfaceType = dc->mapTypeOutOfContext(value->getType());
  emitProlog({}, interfaceType, dc, false);
  prepareEpilog(value->getType(), false, CleanupLocation::get(Loc));
  emitReturnExpr(Loc, value);
  emitEpilog(Loc);
}
