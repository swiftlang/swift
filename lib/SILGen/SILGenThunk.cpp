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
// VTable thunks, witness thunks and the meat of the bridging thunk
// implementation can be found elsewhere.
//
//===----------------------------------------------------------------------===//

#include "SILGenFunction.h"
#include "Scope.h"
#include "ManagedValue.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;

SILFunction *SILGenModule::getDynamicThunk(SILDeclRef constant,
                                           SILConstantInfo constantInfo) {
  // Mangle the constant with a _TTD header.
  auto name = constant.mangle(SILDeclRef::ManglingKind::DynamicThunk);

  IsFragile_t isFragile = IsNotFragile;
  if (makeModuleFragile)
    isFragile = IsFragile;
  if (constant.isFragile())
    isFragile = IsFragile;
  auto F = M.getOrCreateFunction(constant.getDecl(), name, SILLinkage::Shared,
                            constantInfo.getSILType().castTo<SILFunctionType>(),
                            IsBare, IsTransparent, isFragile, IsThunk);

  if (F->empty()) {
    // Emit the thunk if we haven't yet.
    // Currently a dynamic thunk looks just like a foreign-to-native thunk around
    // an ObjC method. This would change if we introduced a native
    // runtime-hookable mechanism.
    SILGenFunction SGF(*this, *F);
    SGF.emitForeignToNativeThunk(constant);
  }

  return F;
}

SILValue SILGenFunction::emitDynamicMethodRef(SILLocation loc,
                                              SILDeclRef constant,
                                              SILConstantInfo constantInfo) {
  // If the method is foreign, its foreign thunk will handle the dynamic
  // dispatch for us.
  if (constant.isForeignToNativeThunk()) {
    if (!SGM.hasFunction(constant))
      SGM.emitForeignToNativeThunk(constant);
    return B.createFunctionRef(loc, SGM.getFunction(constant, NotForDefinition));
  }

  // Otherwise, we need a dynamic dispatch thunk.
  SILFunction *F = SGM.getDynamicThunk(constant, constantInfo);

  return B.createFunctionRef(loc, F);
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

void SILGenModule::emitCurryThunk(ValueDecl *fd,
                                  SILDeclRef entryPoint,
                                  SILDeclRef nextEntryPoint) {
  // Thunks are always emitted by need, so don't need delayed emission.
  SILFunction *f = getFunction(entryPoint, ForDefinition);
  f->setThunk(IsThunk);

  preEmitFunction(entryPoint, fd, f, fd);
  PrettyStackTraceSILFunction X("silgen emitCurryThunk", f);

  SILGenFunction(*this, *f)
    .emitCurryThunk(fd, entryPoint, nextEntryPoint);
  postEmitFunction(entryPoint, f);
}

void SILGenModule::emitForeignToNativeThunk(SILDeclRef thunk) {
  // Thunks are always emitted by need, so don't need delayed emission.
  assert(!thunk.isForeign && "foreign-to-native thunks only");
  SILFunction *f = getFunction(thunk, ForDefinition);
  f->setThunk(IsThunk);
  if (thunk.asForeign().isClangGenerated())
    f->setFragile(IsFragile);
  preEmitFunction(thunk, thunk.getDecl(), f, thunk.getDecl());
  PrettyStackTraceSILFunction X("silgen emitForeignToNativeThunk", f);
  SILGenFunction(*this, *f).emitForeignToNativeThunk(thunk);
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
  SILGenFunction(*this, *f).emitNativeToForeignThunk(thunk);
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
