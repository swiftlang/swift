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
#include "Scope.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILFunctionBuilder.h"
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
  auto name = constant.mangle(SILDeclRef::ManglingKind::DynamicThunk);

  SILFunctionBuilder builder(M);
  auto F = builder.getOrCreateFunction(
      constant.getDecl(), name, SILLinkage::Shared, constantTy, IsBare,
      IsTransparent, IsSerializable, ProfileCounter(), IsThunk);

  if (F->empty()) {
    // Emit the thunk if we haven't yet.
    // Currently a dynamic thunk looks just like a foreign-to-native thunk around
    // an ObjC method. This would change if we introduced a native
    // runtime-hookable mechanism.
    SILGenFunction SGF(*this, *F, SwiftModule);
    SGF.emitForeignToNativeThunk(constant);
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
    return ManagedValue::forUnmanaged(
        B.createFunctionRef(loc, SGM.getFunction(constant, NotForDefinition)));
  }

  // Otherwise, we need a dynamic dispatch thunk.
  SILFunction *F = SGM.getDynamicThunk(constant, constantTy);

  return ManagedValue::forUnmanaged(B.createFunctionRef(loc, F));
}

static ManagedValue getNextUncurryLevelRef(SILGenFunction &SGF, SILLocation loc,
                                           SILDeclRef thunk,
                                           ManagedValue selfArg,
                                           SubstitutionMap curriedSubs) {
  auto *vd = thunk.getDecl();

  // Reference the next uncurrying level of the function.
  SILDeclRef next = SILDeclRef(vd, thunk.kind);
  assert(!next.isCurried);

  // If the function is natively foreign, reference its foreign entry point.
  if (requiresForeignToNativeThunk(vd))
    return ManagedValue::forUnmanaged(SGF.emitGlobalFunctionRef(loc, next));

  // If the thunk is a curry thunk for a direct method reference, we are
  // doing a direct dispatch (eg, a fragile 'super.foo()' call).
  if (thunk.isDirectReference)
    return ManagedValue::forUnmanaged(SGF.emitGlobalFunctionRef(loc, next));

  auto constantInfo = SGF.SGM.Types.getConstantInfo(next);

  if (auto *func = dyn_cast<AbstractFunctionDecl>(vd)) {
    if (getMethodDispatch(func) == MethodDispatch::Class) {
      // Use the dynamic thunk if dynamic.
      if (vd->isDynamic())
        return SGF.emitDynamicMethodRef(loc, next, constantInfo.SILFnType);

      auto methodTy = SGF.SGM.Types.getConstantOverrideType(next);
      SILValue result =
          SGF.emitClassMethodRef(loc, selfArg.getValue(), next, methodTy);
      return ManagedValue::forUnmanaged(result);
    }

    // If the fully-uncurried reference is to a generic method, look up the
    // witness.
    if (constantInfo.SILFnType->getRepresentation()
          == SILFunctionTypeRepresentation::WitnessMethod) {
      auto protocol =
        func->getDeclContext()->getAsProtocolOrProtocolExtensionContext();
      auto origSelfType = protocol->getSelfInterfaceType()->getCanonicalType();
      auto substSelfType = origSelfType.subst(curriedSubs)->getCanonicalType();
      auto conformance = curriedSubs.lookupConformance(origSelfType, protocol);
      auto result = SGF.B.createWitnessMethod(loc, substSelfType, *conformance,
                                              next, constantInfo.getSILType());
      return ManagedValue::forUnmanaged(result);
    }
  }

  // Otherwise, emit a direct call.
  return ManagedValue::forUnmanaged(SGF.emitGlobalFunctionRef(loc, next));
}

void SILGenFunction::emitCurryThunk(SILDeclRef thunk) {
  assert(thunk.isCurried);

  auto *vd = thunk.getDecl();

  if (auto *fd = dyn_cast<AbstractFunctionDecl>(vd)) {
    assert(!SGM.M.Types.hasLoweredLocalCaptures(fd) &&
           "methods cannot have captures");
    (void) fd;
  }

  SILLocation loc(vd);
  Scope S(*this, vd);

  auto thunkFnTy = SGM.Types.getConstantInfo(thunk).SILFnType;
  SILFunctionConventions fromConv(thunkFnTy, SGM.M);

  auto selfTy = fromConv.getSILType(thunkFnTy->getSelfParameter());
  selfTy = F.mapTypeIntoContext(selfTy);
  ManagedValue selfArg = B.createInputFunctionArgument(selfTy, loc);

  // Forward substitutions.
  auto subs = F.getForwardingSubstitutionMap();

  ManagedValue toFn = getNextUncurryLevelRef(*this, loc, thunk, selfArg, subs);

  // FIXME: Using the type from the ConstantInfo instead of looking at
  // getConstantOverrideInfo() for methods looks suspect in the presence
  // of covariant overrides and multiple vtable entries.
  SILType resultTy = fromConv.getSingleSILResultType();
  resultTy = F.mapTypeIntoContext(resultTy);
  auto substTy = toFn.getType().substGenericArgs(SGM.M, subs);

  // Partially apply the next uncurry level and return the result closure.
  selfArg = selfArg.ensurePlusOne(*this, loc);
  auto calleeConvention = ParameterConvention::Direct_Guaranteed;
  auto closureTy = SILGenBuilder::getPartialApplyResultType(
      toFn.getType(), /*appliedParams=*/1, SGM.M, subs, calleeConvention);
  ManagedValue toClosure =
      B.createPartialApply(loc, toFn, substTy, subs, {selfArg}, closureTy);
  if (resultTy != closureTy) {
    CanSILFunctionType resultFnTy = resultTy.castTo<SILFunctionType>();
    CanSILFunctionType closureFnTy = closureTy.castTo<SILFunctionType>();
    if (resultFnTy->isABICompatibleWith(closureFnTy).isCompatible()) {
      toClosure = B.createConvertFunction(loc, toClosure, resultTy);
    } else {
      toClosure =
          emitCanonicalFunctionThunk(loc, toClosure, closureFnTy, resultFnTy);
    }
  }
  toClosure = S.popPreservingValue(toClosure);
  B.createReturn(ImplicitReturnLocation::getImplicitReturnLoc(loc), toClosure);
}

void SILGenModule::emitCurryThunk(SILDeclRef constant) {
  assert(constant.isCurried);

  // Thunks are always emitted by need, so don't need delayed emission.
  SILFunction *f = getFunction(constant, ForDefinition);
  f->setThunk(IsThunk);
  f->setBare(IsBare);

  auto *fd = constant.getDecl();
  preEmitFunction(constant, fd, f, fd);
  PrettyStackTraceSILFunction X("silgen emitCurryThunk", f);

  SILGenFunction(*this, *f, SwiftModule).emitCurryThunk(constant);
  postEmitFunction(constant, f);
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
      SGM.emitCurryThunk(constant);
    } else if (constant.isForeignToNativeThunk()) {
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

SILFunction *SILGenModule::
getOrCreateReabstractionThunk(CanSILFunctionType thunkType,
                              CanSILFunctionType fromType,
                              CanSILFunctionType toType,
                              IsSerialized_t Serialized) {
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

  Mangle::ASTMangler NewMangler;
  std::string name = NewMangler.mangleReabstractionThunkHelper(thunkType,
                       fromInterfaceType, toInterfaceType, M.getSwiftModule());
  
  auto loc = RegularLocation::getAutoGeneratedLocation();

  SILFunctionBuilder builder(M);
  return builder.getOrCreateSharedFunction(
      loc, name, thunkDeclType, IsBare, IsTransparent, IsSerializable,
      ProfileCounter(), IsReabstractionThunk);
}
