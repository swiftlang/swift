//===--- SILGenFunction.cpp - Top-level lowering for functions ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
#include "swift/Basic/Fallthrough.h"
#include "swift/AST/AST.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"

#include "swift/AST/DiagnosticsSIL.h"

using namespace swift;
using namespace Lowering;

//===----------------------------------------------------------------------===//
// SILGenFunction Class implementation
//===----------------------------------------------------------------------===//

SILGenFunction::SILGenFunction(SILGenModule &SGM, SILFunction &F)
  : SGM(SGM), F(F),
    B(*this, createBasicBlock()),
    OpenedArchetypesTracker(F),
    CurrentSILLoc(F.getLocation()),
    Cleanups(*this)
{
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

  freeWritebackStack();
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
  if (auto m = dyn_cast<Module>(dc)) {
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

std::tuple<ManagedValue, SILType, ArrayRef<Substitution>>
SILGenFunction::emitSiblingMethodRef(SILLocation loc,
                                     SILValue selfValue,
                                     SILDeclRef methodConstant,
                                     ArrayRef<Substitution> subs) {
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

  if (!subs.empty()) {
    // Specialize the generic method.
    methodTy = getLoweredLoadableType(
                    methodTy.castTo<SILFunctionType>()
                      ->substGenericArgs(SGM.M, SGM.SwiftModule, subs));
  }

  return std::make_tuple(ManagedValue::forUnmanaged(methodValue),
                         methodTy, subs);
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

      auto &tl = getTypeLowering(vd->getType()->getReferenceStorageReferent());
      SILValue Val = Entry.value;

      if (!Val->getType().isAddress()) {
        // Our 'let' binding can guarantee the lifetime for the callee,
        // if we don't need to do anything more to it.
        if (canGuarantee && !vd->getType()->is<ReferenceStorageType>()) {
          auto guaranteed = ManagedValue::forUnmanaged(Val);
          capturedArgs.push_back(guaranteed);
          break;
        }
      
        // Just retain a by-val let.
        B.emitRetainValueOperation(loc, Val);
      } else {
        // If we have a mutable binding for a 'let', such as 'self' in an
        // 'init' method, load it.
        Val = emitLoad(loc, Val, tl, SGFContext(), IsNotTake).forward(*this);
      }

      // If we're capturing an unowned pointer by value, we will have just
      // loaded it into a normal retained class pointer, but we capture it as
      // an unowned pointer.  Convert back now.
      if (vd->getType()->is<ReferenceStorageType>()) {
        auto type = getTypeLowering(vd->getType()).getLoweredType();
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
          B.createStrongRetain(loc, vl.box, Atomicity::Atomic);
          capturedArgs.push_back(emitManagedRValueWithCleanup(vl.box));
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
        AllocBoxInst *allocBox =
          B.createAllocBox(loc, vl.value->getType().getObjectType());
        ProjectBoxInst *boxAddress = B.createProjectBox(loc, allocBox);
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
                                 ArrayRef<Substitution> subs) {
  auto closure = *constant.getAnyFunctionRef();
  auto captureInfo = closure.getCaptureInfo();
  auto loweredCaptureInfo = SGM.Types.getLoweredLocalCaptures(closure);

  assert(((constant.uncurryLevel == 1 &&
           captureInfo.hasLocalCaptures()) ||
          (constant.uncurryLevel == 0 &&
           !captureInfo.hasLocalCaptures())) &&
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
    auto specialized = pft->substGenericArgs(F.getModule(),
                                             F.getModule().getSwiftModule(),
                                             subs);
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

  if (!captureInfo.hasLocalCaptures() && !wasSpecialized) {
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
                                             subs);
  auto toClosure =
    B.createPartialApply(loc, functionRef, functionTy,
                         subs, forwardedArgs, closureTy);
  auto result = emitManagedRValueWithCleanup(toClosure);

  // Get the lowered AST types:
  //  - the original type
  auto origLoweredFormalType =
      AbstractionPattern(constantInfo.LoweredInterfaceType);
  if (captureInfo.hasLocalCaptures()) {
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

  Type resultTy = fd->getResultType();
  emitProlog(fd, fd->getParameterLists(), resultTy);
  prepareEpilog(resultTy, fd->hasThrows(), CleanupLocation(fd));

  emitProfilerIncrement(fd->getBody());
  emitStmt(fd->getBody());

  emitEpilog(fd);
}

void SILGenFunction::emitClosure(AbstractClosureExpr *ace) {
  MagicFunctionName = SILGenModule::getMagicFunctionName(ace);

  emitProlog(ace, ace->getParameters(), ace->getResultType());
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
  SILValue argc = F.begin()->getBBArg(0);
  SILValue argv = F.begin()->getBBArg(1);

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
    Module *UIKit = ctx.getLoadedModule(ctx.getIdentifier("UIKit"));
    SmallVector<ValueDecl *, 1> results;
    UIKit->lookupQualified(UIKit->getDeclaredType(),
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

    // Get the class name as a string using NSStringFromClass.
    CanType mainClassTy = mainClass->getDeclaredTypeInContext()->getCanonicalType();
    CanType mainClassMetaty = CanMetatypeType::get(mainClassTy,
                                                   MetatypeRepresentation::ObjC);
    ProtocolDecl *anyObjectProtocol =
      ctx.getProtocol(KnownProtocolKind::AnyObject);
    auto mainClassAnyObjectConformance = ProtocolConformanceRef(
      *SGM.M.getSwiftModule()->lookupConformance(mainClassTy, anyObjectProtocol,
                                                nullptr));
    CanType anyObjectTy = anyObjectProtocol
      ->getDeclaredTypeInContext()
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
    SILType nameArgTy = fnTy->getSILArgumentType(3);
    assert(nameArgTy == fnTy->getSILArgumentType(2));
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
    auto argvTy = fnTy->getSILArgumentType(1);

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
    auto rType = F.getLoweredFunctionType()->getSingleResult().getSILType();
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
    auto rType = F.getLoweredFunctionType()->getSingleResult().getSILType();
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
    args.push_back(new (gen.SGM.M) SILArgument(gen.F.begin(), t, d));
  };

  auto *vd = capture.getDecl();

  switch (gen.SGM.Types.getDeclCaptureKind(capture)) {
  case CaptureKind::None:
    break;

  case CaptureKind::Constant:
    addSILArgument(gen.getLoweredType(vd->getType()), vd);
    break;

  case CaptureKind::Box: {
    SILType ty = gen.getLoweredType(vd->getType()->getRValueType())
      .getAddressType();
    // Forward the captured owning box.
    SILType boxTy = SILType::getPrimitiveObjectType(
        SILBoxType::get(ty.getSwiftRValueType()));
    addSILArgument(boxTy, vd);
    break;
  }

  case CaptureKind::StorageAddress: {
    SILType ty = gen.getLoweredType(vd->getType()->getRValueType())
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
                                       ArrayRef<Substitution> curriedSubs) {
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
    auto selfMetaTy = vd->getType()->getAs<AnyFunctionType>()->getInput();
    auto metatypeVal = new (F.getModule()) SILArgument(F.begin(),
                                            getLoweredLoadableType(selfMetaTy));
    curriedArgs.push_back(metatypeVal);

  } else if (auto fd = dyn_cast<AbstractFunctionDecl>(vd)) {
    // Forward implicit closure context arguments.
    bool hasCaptures = fd->getCaptureInfo().hasLocalCaptures();
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
  ArrayRef<Substitution> subs;
  if (auto gp = getConstantInfo(to).ContextGenericParams) {
    subs = gp->getForwardingSubstitutions(getASTContext());
  }

  SILValue toFn = getNextUncurryLevelRef(*this, vd, to, from.isDirectReference,
                                         curriedArgs, subs);
  SILType resultTy
    = SGM.getConstantType(from).castTo<SILFunctionType>()
         ->getSingleResult().getSILType();
  resultTy = F.mapTypeIntoContext(resultTy);
  auto toTy = toFn->getType();

  // Forward archetypes and specialize if the function is generic.
  if (!subs.empty()) {
    auto toFnTy = toFn->getType().castTo<SILFunctionType>();
    toTy = getLoweredLoadableType(
              toFnTy->substGenericArgs(SGM.M, SGM.SwiftModule, subs));
  }

  // Partially apply the next uncurry level and return the result closure.
  auto closureTy =
    SILGenBuilder::getPartialApplyResultType(toFn->getType(), curriedArgs.size(),
                                             SGM.M, subs);
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

  emitProlog({ }, value->getType(), function.getDecl()->getDeclContext());
  prepareEpilog(value->getType(), false, CleanupLocation::get(Loc));
  emitReturnExpr(Loc, value);
  emitEpilog(Loc);
}

SILGenBuilder::SILGenBuilder(SILGenFunction &gen)
  : SILBuilder(gen.F), SGM(gen.SGM) {}
SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB)
  : SILBuilder(insertBB), SGM(gen.SGM) {}
SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB,
                             SmallVectorImpl<SILInstruction *> *insertedInsts)
  : SILBuilder(insertBB, insertedInsts), SGM(gen.SGM) {}
SILGenBuilder::SILGenBuilder(SILGenFunction &gen, SILBasicBlock *insertBB,
                             SILInstruction *insertInst)
    : SILBuilder(insertBB, insertInst->getIterator()), SGM(gen.SGM) {}

MetatypeInst *SILGenBuilder::createMetatype(SILLocation loc, SILType metatype) {
  auto theMetatype = metatype.castTo<MetatypeType>();
  // Getting a nontrivial metatype requires forcing any conformances necessary
  // to instantiate the type.
  switch (theMetatype->getRepresentation()) {
  case MetatypeRepresentation::Thin:
    break;
  case MetatypeRepresentation::Thick:
  case MetatypeRepresentation::ObjC: {
    // Walk the type recursively to look for substitutions we may need.
    theMetatype.getInstanceType().findIf([&](Type t) -> bool {
      if (!t->getAnyNominal())
        return false;

      auto subs = t->gatherAllSubstitutions(SGM.SwiftModule, nullptr);
      SGM.useConformancesFromSubstitutions(subs);
      return false;
    });

    break;
  }
  }

  return SILBuilder::createMetatype(loc, metatype);
}

ApplyInst *SILGenBuilder::createApply(SILLocation Loc, SILValue Fn,
                                      SILType SubstFnTy,
                                      SILType Result,
                                      ArrayRef<Substitution> Subs,
                                      ArrayRef<SILValue> Args) {
  SGM.useConformancesFromSubstitutions(Subs);
  return SILBuilder::createApply(Loc, Fn, SubstFnTy, Result, Subs, Args, false);
}

TryApplyInst *SILGenBuilder::createTryApply(SILLocation loc, SILValue Fn,
                                            SILType substFnTy,
                                            ArrayRef<Substitution> subs,
                                            ArrayRef<SILValue> args,
                                            SILBasicBlock *normalBB,
                                            SILBasicBlock *errorBB) {
  SGM.useConformancesFromSubstitutions(subs);
  return SILBuilder::createTryApply(loc, Fn, substFnTy, subs, args, normalBB,
                                    errorBB);
}

PartialApplyInst *SILGenBuilder::createPartialApply(SILLocation Loc,
                                                    SILValue Fn,
                                                    SILType SubstFnTy,
                                                    ArrayRef<Substitution> Subs,
                                                    ArrayRef<SILValue> Args,
                                                    SILType ClosureTy) {
  SGM.useConformancesFromSubstitutions(Subs);
  return SILBuilder::createPartialApply(Loc, Fn, SubstFnTy, Subs, Args,
                                        ClosureTy);
}

BuiltinInst *SILGenBuilder::createBuiltin(SILLocation Loc, Identifier Name,
                                          SILType ResultTy,
                                          ArrayRef<Substitution> Subs,
                                          ArrayRef<SILValue> Args) {
  SGM.useConformancesFromSubstitutions(Subs);
  return SILBuilder::createBuiltin(Loc, Name, ResultTy, Subs, Args);
}

InitExistentialAddrInst *
SILGenBuilder::createInitExistentialAddr(SILLocation Loc,
                                   SILValue Existential,
                                   CanType FormalConcreteType,
                                   SILType LoweredConcreteType,
                                ArrayRef<ProtocolConformanceRef> Conformances) {
  for (auto conformance : Conformances)
    SGM.useConformance(conformance);

  return SILBuilder::createInitExistentialAddr(Loc, Existential,
                                               FormalConcreteType,
                                               LoweredConcreteType,
                                               Conformances);
}

InitExistentialMetatypeInst *
SILGenBuilder::createInitExistentialMetatype(SILLocation loc,
                                             SILValue metatype,
                                             SILType existentialType,
                                ArrayRef<ProtocolConformanceRef> conformances) {
  for (auto conformance : conformances)
    SGM.useConformance(conformance);

  return SILBuilder::createInitExistentialMetatype(loc, metatype,
                                                   existentialType,
                                                   conformances);
}

InitExistentialRefInst *
SILGenBuilder::createInitExistentialRef(SILLocation Loc,
                                        SILType ExistentialType,
                                        CanType FormalConcreteType,
                                        SILValue Concrete,
                                ArrayRef<ProtocolConformanceRef> Conformances) {
  for (auto conformance : Conformances)
    SGM.useConformance(conformance);

  return SILBuilder::createInitExistentialRef(Loc, ExistentialType,
                                              FormalConcreteType, Concrete,
                                              Conformances);
}

AllocExistentialBoxInst *
SILGenBuilder::createAllocExistentialBox(SILLocation Loc,
                                         SILType ExistentialType,
                                         CanType ConcreteType,
                                ArrayRef<ProtocolConformanceRef> Conformances) {
  for (auto conformance : Conformances)
    SGM.useConformance(conformance);

  return SILBuilder::createAllocExistentialBox(Loc, ExistentialType,
                                               ConcreteType,
                                               Conformances);
}
