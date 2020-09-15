//===--- SILGenThunk.cpp - SILGen for thunks ------------------------------===//
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
// This file contains code for emitting various types of thunks that can be
// referenced from code, such as dynamic thunks, curry thunks, native to foreign
// thunks and foreign to native thunks.
//
// VTable thunks and witness thunks can be found in SILGenType.cpp, and the
// meat of the bridging thunk implementation is in SILGenBridging.cpp, and
// re-abstraction thunks are in SILGenPoly.cpp.
//
//===----------------------------------------------------------------------===//

#include "ManagedValue.h"
#include "SILGenFunction.h"
#include "SILGenFunctionBuilder.h"
#include "Scope.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/FileUnit.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;

SILValue SILGenFunction::emitClassMethodRef(SILLocation loc,
                                            SILValue selfPtr,
                                            SILDeclRef constant,
                                            CanSILFunctionType constantTy) {
  assert(!constant.isForeign);
  return B.createClassMethod(loc, selfPtr, constant,
                             SILType::getPrimitiveObjectType(constantTy));
}

SILFunction *SILGenModule::getDynamicThunk(SILDeclRef constant,
                                           CanSILFunctionType constantTy) {
  assert(constant.kind != SILDeclRef::Kind::Allocator &&
         "allocating entry point for constructor is never dynamic");
  // Mangle the constant with a TD suffix.
  auto nameTmp = constant.mangle(SILDeclRef::ManglingKind::DynamicThunk);
  auto name = M.allocateCopy(nameTmp);

  SILGenFunctionBuilder builder(*this);
  auto F = builder.getOrCreateFunction(
      constant.getDecl(), name, SILLinkage::Shared, constantTy, IsBare,
      IsTransparent, IsSerializable, IsNotDynamic, ProfileCounter(), IsThunk);

  if (F->empty()) {
    // Emit the thunk if we haven't yet.
    // Currently a dynamic thunk looks just like a foreign-to-native thunk around
    // an ObjC method. This would change if we introduced a native
    // runtime-hookable mechanism.
    SILGenFunction SGF(*this, *F, SwiftModule);
    SGF.emitForeignToNativeThunk(constant);
    emitLazyConformancesForFunction(F);
  }

  return F;
}

ManagedValue
SILGenFunction::emitDynamicMethodRef(SILLocation loc, SILDeclRef constant,
                                     CanSILFunctionType constantTy) {
  // If the method is foreign, its foreign thunk will handle the dynamic
  // dispatch for us.
  if (constant.isForeignToNativeThunk()) {
    if (!SGM.hasFunction(constant))
      SGM.emitForeignToNativeThunk(constant);
    return ManagedValue::forUnmanaged(B.createFunctionRefFor(
        loc, SGM.getFunction(constant, NotForDefinition)));
  }

  // Otherwise, we need a dynamic dispatch thunk.
  SILFunction *F = SGM.getDynamicThunk(constant, constantTy);

  return ManagedValue::forUnmanaged(B.createFunctionRefFor(loc, F));
}

void SILGenModule::emitForeignToNativeThunk(SILDeclRef thunk) {
  // Thunks are always emitted by need, so don't need delayed emission.
  assert(!thunk.isForeign && "foreign-to-native thunks only");
  SILFunction *f = getFunction(thunk, ForDefinition);
  f->setThunk(IsThunk);
  if (thunk.asForeign().isClangGenerated())
    f->setSerialized(IsSerializable);
  preEmitFunction(thunk, thunk.getDecl(), f, thunk.getDecl());
  PrettyStackTraceSILFunction X("silgen emitForeignToNativeThunk", f);
  SILGenFunction(*this, *f, SwiftModule).emitForeignToNativeThunk(thunk);
  postEmitFunction(thunk, f);
}

void SILGenModule::emitNativeToForeignThunk(SILDeclRef thunk) {
  // Thunks are always emitted by need, so don't need delayed emission.
  assert(thunk.isForeign && "native-to-foreign thunks only");
  
  SILFunction *f = getFunction(thunk, ForDefinition);
  if (thunk.hasDecl())
    preEmitFunction(thunk, thunk.getDecl(), f, thunk.getDecl());
  else
    preEmitFunction(thunk, thunk.getAbstractClosureExpr(), f,
                    thunk.getAbstractClosureExpr());
  PrettyStackTraceSILFunction X("silgen emitNativeToForeignThunk", f);
  f->setBare(IsBare);
  f->setThunk(IsThunk);
  SILGenFunction(*this, *f, SwiftModule).emitNativeToForeignThunk(thunk);
  postEmitFunction(thunk, f);
}

SILValue
SILGenFunction::emitGlobalFunctionRef(SILLocation loc, SILDeclRef constant,
                                      SILConstantInfo constantInfo,
                                      bool callPreviousDynamicReplaceableImpl) {
  assert(constantInfo == getConstantInfo(getTypeExpansionContext(), constant));

  // Builtins must be fully applied at the point of reference.
  if (constant.hasDecl() &&
      isa<BuiltinUnit>(constant.getDecl()->getDeclContext())) {
    SGM.diagnose(loc.getSourceLoc(), diag::not_implemented,
                 "delayed application of builtin");
    return SILUndef::get(constantInfo.getSILType(), F);
  }
  
  // If the constant is a thunk we haven't emitted yet, emit it.
  if (!SGM.hasFunction(constant)) {
    if (constant.isForeignToNativeThunk()) {
      SGM.emitForeignToNativeThunk(constant);
    } else if (constant.isNativeToForeignThunk()) {
      SGM.emitNativeToForeignThunk(constant);
    } else if (constant.kind == SILDeclRef::Kind::EnumElement) {
      SGM.emitEnumConstructor(cast<EnumElementDecl>(constant.getDecl()));
    }
  }

  auto f = SGM.getFunction(constant, NotForDefinition);
#ifndef NDEBUG
  auto constantFnTypeInContext =
    SGM.Types.getLoweredType(constantInfo.SILFnType,
                             B.getTypeExpansionContext())
             .castTo<SILFunctionType>();
  assert(f->getLoweredFunctionTypeInContext(B.getTypeExpansionContext())
          == constantFnTypeInContext);
#endif
  if (callPreviousDynamicReplaceableImpl)
    return B.createPreviousDynamicFunctionRef(loc, f);
  else
    return B.createFunctionRefFor(loc, f);
}

SILFunction *SILGenModule::
getOrCreateReabstractionThunk(CanSILFunctionType thunkType,
                              CanSILFunctionType fromType,
                              CanSILFunctionType toType,
                              CanType dynamicSelfType) {
  // The reference to the thunk is likely @noescape, but declarations are always
  // escaping.
  auto thunkDeclType =
      thunkType->getWithExtInfo(thunkType->getExtInfo().withNoEscape(false));

  // Mangle the reabstraction thunk.
  // Substitute context parameters out of the "from" and "to" types.
  auto fromInterfaceType = fromType->mapTypeOutOfContext()
    ->getCanonicalType();
  auto toInterfaceType = toType->mapTypeOutOfContext()
    ->getCanonicalType();
  CanType dynamicSelfInterfaceType;
  if (dynamicSelfType)
    dynamicSelfInterfaceType = dynamicSelfType->mapTypeOutOfContext()
      ->getCanonicalType();

  Mangle::ASTMangler NewMangler;
  std::string name = NewMangler.mangleReabstractionThunkHelper(thunkType,
                       fromInterfaceType, toInterfaceType,
                       dynamicSelfInterfaceType,
                       M.getSwiftModule());
  
  auto loc = RegularLocation::getAutoGeneratedLocation();

  SILGenFunctionBuilder builder(*this);
  return builder.getOrCreateSharedFunction(
      loc, name, thunkDeclType, IsBare, IsTransparent, IsSerializable,
      ProfileCounter(), IsReabstractionThunk, IsNotDynamic);
}

SILFunction *SILGenModule::getOrCreateAutoDiffClassMethodThunk(
    SILDeclRef derivativeFnDeclRef, CanSILFunctionType constantTy) {
  auto *derivativeId = derivativeFnDeclRef.derivativeFunctionIdentifier;
  assert(derivativeId);
  auto *derivativeFnDecl = derivativeFnDeclRef.getDecl();

  SILGenFunctionBuilder builder(*this);
  auto originalFnDeclRef = derivativeFnDeclRef.asAutoDiffOriginalFunction();
  // TODO(TF-685): Use principled thunk mangling.
  // Do not simply reuse reabstraction thunk mangling.
  auto name = derivativeFnDeclRef.mangle() + "_vtable_entry_thunk";
  auto *thunk = builder.getOrCreateFunction(
      derivativeFnDecl, name, originalFnDeclRef.getLinkage(ForDefinition),
      constantTy, IsBare, IsTransparent, derivativeFnDeclRef.isSerialized(),
      IsNotDynamic, ProfileCounter(), IsThunk);
  if (!thunk->empty())
    return thunk;

  if (auto genSig = constantTy->getSubstGenericSignature())
    thunk->setGenericEnvironment(genSig->getGenericEnvironment());
  SILGenFunction SGF(*this, *thunk, SwiftModule);
  SmallVector<ManagedValue, 4> params;
  auto loc = derivativeFnDeclRef.getAsRegularLocation();
  SGF.collectThunkParams(loc, params);

  auto originalFn = SGF.emitGlobalFunctionRef(loc, originalFnDeclRef);
  auto *loweredParamIndices = autodiff::getLoweredParameterIndices(
      derivativeId->getParameterIndices(),
      derivativeFnDecl->getInterfaceType()->castTo<AnyFunctionType>());
  auto diffFn =
      SGF.B.createDifferentiableFunction(loc, loweredParamIndices, originalFn);
  auto derivativeFn = SGF.B.createDifferentiableFunctionExtract(
      loc, NormalDifferentiableFunctionTypeComponent(derivativeId->getKind()),
      diffFn);
  auto derivativeFnSILTy = SILType::getPrimitiveObjectType(constantTy);
  SmallVector<SILValue, 4> args(thunk->getArguments().begin(),
                                thunk->getArguments().end());
  auto apply =
      SGF.emitApplyWithRethrow(loc, derivativeFn, derivativeFnSILTy,
                               SGF.getForwardingSubstitutionMap(), args);
  SGF.B.createReturn(loc, apply);

  return thunk;
}
