//===--- SILGenFunction.cpp - Top-level lowering for functions ------------===//
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

//===--------------------------------------------------------------------===//
// SILGenFunction Class implementation
//===--------------------------------------------------------------------===//

SILGenFunction::SILGenFunction(SILGenModule &SGM, SILFunction &F)
  : SGM(SGM), F(F),
    B(createBasicBlock(), &InsertedInstrs),
    CurrentSILLoc(F.getLocation()),
    Cleanups(*this)
{
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

//===--------------------------------------------------------------------===//
// Function emission
//===--------------------------------------------------------------------===//

// Get the __FUNCTION__ name for a declaration.
DeclName SILGenModule::getMagicFunctionName(DeclContext *dc) {
  // For closures, use the parent name.
  if (auto closure = dyn_cast<AbstractClosureExpr>(dc)) {
    return getMagicFunctionName(closure->getParent());
  }
  if (auto absFunc = dyn_cast<AbstractFunctionDecl>(dc)) {
    // If this is an accessor, use the name of the storage.
    if (auto func = dyn_cast<FuncDecl>(absFunc)) {
      if (auto storage = func->getAccessorStorageDecl())
        return storage->getFullName();
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
    return tl->getModuleContext()->Name;
  }
  if (auto fu = dyn_cast<FileUnit>(dc)) {
    return fu->getParentModule()->Name;
  }
  if (auto m = dyn_cast<Module>(dc)) {
    return m->Name;
  }
  if (auto e = dyn_cast<ExtensionDecl>(dc)) {
    assert(e->getExtendedType()->getAnyNominal() && "extension for nonnominal");
    return e->getExtendedType()->getAnyNominal()->getName();
  }
  llvm_unreachable("unexpected __FUNCTION__ context");
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
      // Non-functions can't be referenced uncurried.
      FuncDecl *fd = cast<FuncDecl>(constant.getDecl());

      // Getters and setters can't be referenced uncurried.
      assert(!fd->isAccessor());

      // FIXME: Thunks for instance methods of generics.
      assert(!(fd->isInstanceMember() &&
               isa<ProtocolDecl>(fd->getDeclContext()))
             && "currying generic method not yet supported");

      // FIXME: Curry thunks for generic methods don't work right yet, so skip
      // emitting thunks for them
      assert(!(fd->getType()->is<AnyFunctionType>() &&
               fd->getType()->castTo<AnyFunctionType>()->getResult()
                 ->is<PolymorphicFunctionType>()));

      // Reference the next uncurrying level of the function.
      SILDeclRef next = SILDeclRef(fd, SILDeclRef::Kind::Func,
                                 SILDeclRef::ConstructAtBestResilienceExpansion,
                                 constant.uncurryLevel + 1);
      // If the function is fully uncurried and natively foreign, reference its
      // foreign entry point.
      if (!next.isCurried && fd->hasClangNode())
        next = next.asForeign();
      
      // Preserve whether the curry thunks lead to a direct reference to the
      // method implementation.
      next = next.asDirectReference(constant.isDirectReference);

      SGM.emitCurryThunk(constant, next, fd);
    }
    // Otherwise, if this is a calling convention thunk we haven't emitted yet,
    // emit it.
    else if (constant.isForeignToNativeThunk()) {
      SGM.emitForeignToNativeThunk(constant);
    } else if (constant.isNativeToForeignThunk()) {
      SGM.emitNativeToForeignThunk(constant);
    }
  }

  return B.createFunctionRef(loc, SGM.getFunction(constant, NotForDefinition));
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

  SILType methodTy = methodValue.getType();

  if (!subs.empty()) {
    // Specialize the generic method.
    methodTy = getLoweredLoadableType(
                    methodTy.castTo<SILFunctionType>()
                      ->substGenericArgs(SGM.M, SGM.SwiftModule, subs));
  }

  return std::make_tuple(ManagedValue::forUnmanaged(methodValue),
                         methodTy, subs);
}

ManagedValue SILGenFunction::emitFunctionRef(SILLocation loc,
                                             SILDeclRef constant) {
  return emitFunctionRef(loc, constant, getConstantInfo(constant));
}

ManagedValue SILGenFunction::emitFunctionRef(SILLocation loc,
                                             SILDeclRef constant,
                                             SILConstantInfo constantInfo) {
  // If the function has captures, apply them.
  if (auto fn = constant.getAnyFunctionRef()) {
    if (fn->getCaptureInfo().hasLocalCaptures()
        || (fn->getAsDeclContext()->getParent()->isLocalContext()
            && fn->getAsDeclContext()->isGenericContext())) {
      return emitClosureValue(loc, constant, getForwardingSubstitutions(), *fn);
    }
  }

  // Otherwise, use a global FunctionRefInst.
  SILValue c = emitGlobalFunctionRef(loc, constant, constantInfo);
  return ManagedValue::forUnmanaged(c);
}

void SILGenFunction::emitCaptures(SILLocation loc,
                                  AnyFunctionRef TheClosure,
                                  SmallVectorImpl<ManagedValue> &capturedArgs) {
  for (auto capture : SGM.Types.getLoweredLocalCaptures(TheClosure)) {
    auto *vd = capture.getDecl();

    switch (SGM.Types.getDeclCaptureKind(capture)) {
    case CaptureKind::None:
      break;

    case CaptureKind::Constant: {
      // let declarations.
      auto Entry = VarLocs[vd];

      // Non-address-only constants are passed at +1.
      auto &tl = getTypeLowering(vd->getType()->getReferenceStorageReferent());
      SILValue Val = Entry.value;

      if (!Val.getType().isAddress()) {
        // Just retain a by-val let.
        B.emitRetainValueOperation(loc, Val);
      } else {
        // If we have a mutable binding for a 'let', such as 'self' in an
        // 'init' method, load it.
        Val = emitLoad(loc, Val, tl, SGFContext(), IsNotTake).forward(*this);
      }

      // Use an RValue to explode Val if it is a tuple.
      RValue RV(*this, loc, vd->getType()->getCanonicalType(),
                ManagedValue::forUnmanaged(Val));

      // If we're capturing an unowned pointer by value, we will have just
      // loaded it into a normal retained class pointer, but we capture it as
      // an unowned pointer.  Convert back now.
      if (vd->getType()->is<ReferenceStorageType>()) {
        auto type = getTypeLowering(vd->getType()).getLoweredType();
        auto val = std::move(RV).forwardAsSingleStorageValue(*this, type,loc);
        capturedArgs.push_back(emitManagedRValueWithCleanup(val));
      } else {
        std::move(RV).getAll(capturedArgs);
      }
      break;
    }

    case CaptureKind::StorageAddress: {
      // No-escaping stored declarations are captured as the
      // address of the value.
      assert(VarLocs.count(vd) && "no location for captured var!");
      VarLoc vl = VarLocs[vd];
      assert(vl.value.getType().isAddress() && "no address for captured var!");
      capturedArgs.push_back(ManagedValue::forLValue(vl.value));
      break;
    }

    case CaptureKind::Box: {
      // LValues are captured as both the box owning the value and the
      // address of the value.
      assert(VarLocs.count(vd) && "no location for captured var!");
      VarLoc vl = VarLocs[vd];
      assert(vl.value.getType().isAddress() && "no address for captured var!");

      // If this is a boxed variable, we can use it directly.
      if (vl.box) {
        B.createStrongRetain(loc, vl.box);
        capturedArgs.push_back(emitManagedRValueWithCleanup(vl.box));
        capturedArgs.push_back(ManagedValue::forLValue(vl.value));
      } else {
        // Address only 'let' values are passed by box.  This isn't great, in
        // that a variable captured by multiple closures will be boxed for each
        // one.  This could be improved by doing an "isCaptured" analysis when
        // emitting address-only let constants, and emit them into a alloc_box
        // like a variable instead of into an alloc_stack.
        AllocBoxInst *allocBox =
          B.createAllocBox(loc, vl.value.getType().getObjectType());
        auto boxAddress = SILValue(allocBox, 1);
        B.createCopyAddr(loc, vl.value, boxAddress, IsNotTake,IsInitialization);
        capturedArgs.push_back(emitManagedRValueWithCleanup(SILValue(allocBox, 0)));
        capturedArgs.push_back(ManagedValue::forLValue(boxAddress));
      }

      break;
    }
    }
  }
}

ManagedValue
SILGenFunction::emitClosureValue(SILLocation loc, SILDeclRef constant,
                                 ArrayRef<Substitution> forwardSubs,
                                 AnyFunctionRef TheClosure) {
  // FIXME: Stash the capture args somewhere and curry them on demand rather
  // than here.
  assert(((constant.uncurryLevel == 1 &&
           TheClosure.getCaptureInfo().hasLocalCaptures()) ||
          (constant.uncurryLevel == 0 &&
           !TheClosure.getCaptureInfo().hasLocalCaptures())) &&
         "curried local functions not yet supported");

  auto constantInfo = getConstantInfo(constant);
  SILValue functionRef = emitGlobalFunctionRef(loc, constant, constantInfo);
  SILType functionTy = functionRef.getType();

  auto expectedType =
    cast<FunctionType>(TheClosure.getType()->getCanonicalType());

  // Forward substitutions from the outer scope.

  auto pft = constantInfo.SILFnType;

  bool wasSpecialized = false;
  if (pft->isPolymorphic() && !forwardSubs.empty()) {
    auto specialized = pft->substGenericArgs(F.getModule(),
                                                F.getModule().getSwiftModule(),
                                                forwardSubs);
    functionTy = SILType::getPrimitiveObjectType(specialized);
    wasSpecialized = true;
  }

  if (!TheClosure.getCaptureInfo().hasLocalCaptures() && !wasSpecialized) {
    auto result = ManagedValue::forUnmanaged(functionRef);
    return emitGeneralizedFunctionValue(loc, result,
                             AbstractionPattern(expectedType), expectedType);
  }

  SmallVector<ManagedValue, 4> capturedArgs;
  emitCaptures(loc, TheClosure, capturedArgs);

  // Currently all capture arguments are captured at +1.
  // TODO: Ideally this would be +0.
  SmallVector<SILValue, 4> forwardedArgs;
  for (auto capture : capturedArgs)
    forwardedArgs.push_back(capture.forward(*this));

  SILType closureTy =
    SILBuilder::getPartialApplyResultType(functionRef.getType(),
                                          capturedArgs.size(), SGM.M,
                                          forwardSubs);
  auto toClosure =
    B.createPartialApply(loc, functionRef, functionTy,
                         forwardSubs, forwardedArgs, closureTy);
  auto result = emitManagedRValueWithCleanup(toClosure);

  return emitGeneralizedFunctionValue(loc, result,
                                      AbstractionPattern(expectedType),
                                      expectedType);
}

void SILGenFunction::emitFunction(FuncDecl *fd) {
  MagicFunctionName = SILGenModule::getMagicFunctionName(fd);

  Type resultTy = fd->getResultType();
  emitProlog(fd, fd->getBodyParamPatterns(), resultTy);
  prepareEpilog(resultTy, fd->isBodyThrowing(), CleanupLocation(fd));

  emitProfilerIncrement(fd->getBody());
  emitStmt(fd->getBody());

  emitEpilog(fd);
}

void SILGenFunction::emitClosure(AbstractClosureExpr *ace) {
  MagicFunctionName = SILGenModule::getMagicFunctionName(ace);

  emitProlog(ace, ace->getParams(), ace->getResultType());
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

    // Get the class name as a string using NSStringFromClass.
    CanType mainClassTy = mainClass->getDeclaredTypeInContext()->getCanonicalType();
    CanType mainClassMetaty = CanMetatypeType::get(mainClassTy,
                                                   MetatypeRepresentation::ObjC);
    ProtocolDecl *anyObjectProtocol =
      getASTContext().getProtocol(KnownProtocolKind::AnyObject);
    auto mainClassAnyObjectConformance =
      SGM.M.getSwiftModule()->lookupConformance(mainClassTy, anyObjectProtocol,
                                                nullptr)
        .getPointer();
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
                  getASTContext());
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
                          getASTContext().AllocateCopy(
                            llvm::makeArrayRef(mainClassAnyObjectConformance)));
    SILValue optName = B.createApply(mainClass,
                               NSStringFromClass,
                               NSStringFromClass->getType(),
                               SILType::getPrimitiveObjectType(OptNSStringTy),
                               {}, metaTy);
    SILValue iuoptName = B.createUncheckedRefBitCast(mainClass, optName,
                              SILType::getPrimitiveObjectType(IUOptNSStringTy));

    // Call UIApplicationMain.
    SILParameterInfo argTypes[] = {
      SILParameterInfo(argc.getType().getSwiftRValueType(),
                       ParameterConvention::Direct_Unowned),
      SILParameterInfo(argv.getType().getSwiftRValueType(),
                       ParameterConvention::Direct_Unowned),
      SILParameterInfo(IUOptNSStringTy, ParameterConvention::Direct_Unowned),
      SILParameterInfo(IUOptNSStringTy, ParameterConvention::Direct_Unowned),
    };
    auto UIApplicationMainType = SILFunctionType::get(nullptr,
                  SILFunctionType::ExtInfo()
                    .withRepresentation(SILFunctionType::Representation::
                                        CFunctionPointer),
                  ParameterConvention::Direct_Unowned,
                  argTypes,
                  SILResultInfo(argc.getType().getSwiftRValueType(),
                                ResultConvention::Unowned),
                  /*error result*/ None,
                  getASTContext());

    auto UIApplicationMainFn
      = SGM.M.getOrCreateFunction(mainClass, "UIApplicationMain",
                                  SILLinkage::PublicExternal,
                                  UIApplicationMainType,
                                  IsBare, IsTransparent, IsNotFragile);

    auto UIApplicationMain = B.createFunctionRef(mainClass, UIApplicationMainFn);
    auto nil = B.createEnum(mainClass, SILValue(),
                      getASTContext().getImplicitlyUnwrappedOptionalNoneDecl(),
                      SILType::getPrimitiveObjectType(IUOptNSStringTy));

    SILValue args[] = { argc, argv, nil, iuoptName };

    B.createApply(mainClass, UIApplicationMain,
                  UIApplicationMain->getType(),
                  argc.getType(), {}, args);
    SILValue r = B.createIntegerLiteral(mainClass,
                        SILType::getBuiltinIntegerType(32, getASTContext()), 0);
    if (r.getType() != F.getLoweredFunctionType()->getResult().getSILType())
      r = B.createStruct(mainClass,
                       F.getLoweredFunctionType()->getResult().getSILType(), r);

    B.createReturn(mainClass, r);
    return;
  }

  case ArtificialMainKind::NSApplicationMain: {
    // Emit an AppKit main.
    // return NSApplicationMain(C_ARGC, C_ARGV);

    SILParameterInfo argTypes[] = {
      SILParameterInfo(argc.getType().getSwiftRValueType(),
                       ParameterConvention::Direct_Unowned),
      SILParameterInfo(argv.getType().getSwiftRValueType(),
                       ParameterConvention::Direct_Unowned),
    };
    auto NSApplicationMainType = SILFunctionType::get(nullptr,
                  SILFunctionType::ExtInfo()
                    // Should be C calling convention, but NSApplicationMain
                    // has an overlay to fix the type of argv.
                    .withRepresentation(SILFunctionType::Representation::Thin),
                  ParameterConvention::Direct_Unowned,
                  argTypes,
                  SILResultInfo(argc.getType().getSwiftRValueType(),
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
                  argc.getType(), {}, args);
    SILValue r = B.createIntegerLiteral(mainClass,
                        SILType::getBuiltinIntegerType(32, getASTContext()), 0);
    if (r.getType() != F.getLoweredFunctionType()->getResult().getSILType())
      r = B.createStruct(mainClass,
                       F.getLoweredFunctionType()->getResult().getSILType(), r);
    B.createReturn(mainClass, r);
    return;
  }
  }
}

static void forwardCaptureArgs(SILGenFunction &gen,
                               SmallVectorImpl<SILValue> &args,
                               CapturedValue capture) {
  ASTContext &c = gen.getASTContext();

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
    // Forward the captured owning NativeObject.
    addSILArgument(SILType::getNativeObjectType(c), vd);
    // Forward the captured value address.
    addSILArgument(ty, vd);
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
  // For a foreign function, reference the native thunk.
  if (next.isForeign)
    return gen.emitGlobalFunctionRef(loc, next.asForeign(false));

  // If the fully-uncurried reference is to a native dynamic class method, emit
  // the dynamic dispatch.
  auto fullyAppliedMethod = !next.isCurried && !next.isForeign && !direct &&
    next.kind == SILDeclRef::Kind::Func &&
    next.hasDecl();

  auto constantInfo = gen.SGM.Types.getConstantInfo(next);
  SILValue thisArg;
  if (!curriedArgs.empty())
      thisArg = curriedArgs.back();

  if (fullyAppliedMethod &&
      gen.getMethodDispatch(cast<AbstractFunctionDecl>(next.getDecl()))
        == MethodDispatch::Class) {
    SILValue thisArg = curriedArgs.back();

    // Use the dynamic thunk if dynamic.
    if (next.getDecl()->isDynamic()) {
      auto dynamicThunk = gen.SGM.getDynamicThunk(next, constantInfo);
      return gen.B.createFunctionRef(loc, dynamicThunk);
    }

    return gen.B.createClassMethod(loc, thisArg, next,
                                   constantInfo.getSILType());
  }

  // If the fully-uncurried reference is to a generic method, look up the
  // witness.
  if (fullyAppliedMethod &&
      constantInfo.SILFnType->getRepresentation()
        == SILFunctionTypeRepresentation::WitnessMethod) {
    auto thisType = curriedSubs[0].getReplacement()->getCanonicalType();
    assert(isa<ArchetypeType>(thisType) && "no archetype for witness?!");
    SILValue OpenedExistential;
    if (!cast<ArchetypeType>(thisType)->getOpenedExistentialType().isNull())
      OpenedExistential = thisArg;
    return gen.B.createWitnessMethod(loc, thisType, nullptr, next,
                                     constantInfo.getSILType(),
                                     OpenedExistential);
  }

  // Otherwise, emit a direct call.
  return gen.emitGlobalFunctionRef(loc, next);
}

void SILGenFunction::emitCurryThunk(FuncDecl *fd,
                                    SILDeclRef from, SILDeclRef to) {
  SmallVector<SILValue, 8> curriedArgs;

  unsigned paramCount = from.uncurryLevel + 1;

  // Forward implicit closure context arguments.
  bool hasCaptures = fd->getCaptureInfo().hasLocalCaptures();
  if (hasCaptures)
    --paramCount;

  // Forward the curried formal arguments.
  auto forwardedPatterns = fd->getBodyParamPatterns().slice(0, paramCount);
  for (auto *paramPattern : reversed(forwardedPatterns))
    bindParametersForForwarding(paramPattern, curriedArgs);

  // Forward captures.
  if (hasCaptures) {
    ArrayRef<CapturedValue> LocalCaptures
      = SGM.Types.getLoweredLocalCaptures(fd);
    for (auto capture : LocalCaptures)
      forwardCaptureArgs(*this, curriedArgs, capture);
  }

  // Forward substitutions.
  ArrayRef<Substitution> subs;
  if (auto gp = getConstantInfo(to).ContextGenericParams) {
    subs = gp->getForwardingSubstitutions(getASTContext());
  }

  SILValue toFn = getNextUncurryLevelRef(*this, fd, to, from.isDirectReference,
                                         curriedArgs, subs);
  SILType resultTy
    = SGM.getConstantType(from).castTo<SILFunctionType>()
         ->getResult().getSILType();
  resultTy = F.mapTypeIntoContext(resultTy);
  auto toTy = toFn.getType();

  // Forward archetypes and specialize if the function is generic.
  if (!subs.empty()) {
    auto toFnTy = toFn.getType().castTo<SILFunctionType>();
    toTy = getLoweredLoadableType(
              toFnTy->substGenericArgs(SGM.M, SGM.SwiftModule, subs));
  }

  // Partially apply the next uncurry level and return the result closure.
  auto closureTy =
    SILBuilder::getPartialApplyResultType(toFn.getType(), curriedArgs.size(),
                                          SGM.M, subs);
  SILInstruction *toClosure =
    B.createPartialApply(fd, toFn, toTy, subs, curriedArgs, closureTy);
  if (resultTy != closureTy)
    toClosure = B.createConvertFunction(fd, toClosure, resultTy);
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(fd), toClosure);
}

void SILGenFunction::emitGeneratorFunction(SILDeclRef function, Expr *value) {
  MagicFunctionName = SILGenModule::getMagicFunctionName(function);

  RegularLocation Loc(value);
  Loc.markAutoGenerated();

  // Override location for __FILE__ __LINE__ etc. to an invalid one so that we
  // don't put extra strings into the defaut argument generator function that
  // is not going to be ever used anyway.
  overrideLocationForMagicIdentifiers = SourceLoc();

  emitProlog({ }, value->getType(), function.getDecl()->getDeclContext());
  prepareEpilog(value->getType(), false, CleanupLocation::get(Loc));
  emitReturnExpr(Loc, value);
  emitEpilog(Loc);
}

