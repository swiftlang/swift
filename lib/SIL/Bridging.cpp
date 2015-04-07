//===--- Bridging.cpp - Bridging imported Clang types to Swift ------------===//
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
// This file defines routines relating to bridging Swift types to C types,
// working in concert with the Clang importer.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "libsil"
#include "swift/SIL/SILType.h"
#include "swift/SIL/SILModule.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/Basic/Fallthrough.h"
#include "clang/AST/DeclObjC.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
using namespace swift;
using namespace swift::Lowering;


SILType TypeConverter::getLoweredTypeOfGlobal(VarDecl *var) {
  AbstractionPattern origType = getAbstractionPattern(var);
  CanType swiftType = (origType.isOpaque() ? var->getType()->getCanonicalType()
                                           : origType.getType());
  return getLoweredType(origType, swiftType).getObjectType();
}

static CanType getBridgedInputType(TypeConverter &tc,
                                   SILFunctionTypeRepresentation rep,
                                   CanType input,
                                   const clang::Decl *clangDecl) {

  auto getClangParamType = [&](unsigned i) -> const clang::Type * {
    if (!clangDecl)
      return nullptr;
    if (auto methodDecl = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
      if (i < methodDecl->param_size())
        return methodDecl->param_begin()[i]->getType()
          ->getUnqualifiedDesugaredType();
      return nullptr;
    } else if (auto fnTy = dyn_cast_or_null<clang::FunctionProtoType>
                 (clangDecl->getFunctionType())) {
      return fnTy->getParamType(i)->getUnqualifiedDesugaredType();
    } else {
      return nullptr;
    }
  };

  if (auto tuple = dyn_cast<TupleType>(input)) {
    SmallVector<TupleTypeElt, 4> bridgedFields;
    bool changed = false;

    for (unsigned i : indices(tuple->getElements())) {
      auto &elt = tuple->getElement(i);

      auto clangInputTy = getClangParamType(i);

      Type bridged = tc.getLoweredBridgedType(elt.getType(), rep, clangInputTy,
                                              TypeConverter::ForArgument);
      if (!bridged) {
        tc.Context.Diags.diagnose(SourceLoc(), diag::could_not_find_bridge_type,
                                  elt.getType());

        llvm::report_fatal_error("unable to set up the ObjC bridge!");
      }

      CanType canBridged = bridged->getCanonicalType();
      if (canBridged != CanType(elt.getType())) {
        changed = true;
        bridgedFields.push_back(elt.getWithType(canBridged));
      } else {
        bridgedFields.push_back(elt);
      }
    }

    if (!changed)
      return input;
    return CanType(TupleType::get(bridgedFields, input->getASTContext()));
  }

  auto clangInputTy = getClangParamType(0);
  auto loweredBridgedType = tc.getLoweredBridgedType(input, rep, clangInputTy,
                                                    TypeConverter::ForArgument);

  if (!loweredBridgedType) {
    tc.Context.Diags.diagnose(SourceLoc(), diag::could_not_find_bridge_type,
                              input);

    llvm::report_fatal_error("unable to set up the ObjC bridge!");
  }

  return loweredBridgedType->getCanonicalType();
}

/// Bridge a result type.
static CanType getBridgedResultType(TypeConverter &tc,
                                    SILFunctionTypeRepresentation rep,
                                    CanType result,
                                    const clang::Decl *clangDecl) {
  const clang::Type *clangResultTy = nullptr;
  if (clangDecl) {
    if (auto methodDecl = dyn_cast<clang::ObjCMethodDecl>(clangDecl)) {
      clangResultTy = methodDecl->getReturnType()->getUnqualifiedDesugaredType();
    } else if (auto fnTy = clangDecl->getFunctionType()) {
      clangResultTy = fnTy->getReturnType()->getUnqualifiedDesugaredType();
    }
  }

  auto loweredType = tc.getLoweredBridgedType(result, rep, clangResultTy,
                                              TypeConverter::ForResult);

  if (!loweredType) {
    tc.Context.Diags.diagnose(SourceLoc(), diag::could_not_find_bridge_type,
                              result);

    llvm::report_fatal_error("unable to set up the ObjC bridge!");
  }

  return loweredType->getCanonicalType();
}

/// Fast path for bridging types in a function type without uncurrying.
static CanAnyFunctionType getBridgedFunctionType(TypeConverter &tc,
                                            CanAnyFunctionType t,
                                            AnyFunctionType::ExtInfo extInfo,
                                            const clang::Decl *decl) {
  // Pull out the generic signature.
  CanGenericSignature genericSig;
  if (auto gft = dyn_cast<GenericFunctionType>(t)) {
    genericSig = gft.getGenericSignature();
  }

  // Pull the innermost generic parameter list in the type out.
  Optional<GenericParamList *> genericParams;
  {
    CanAnyFunctionType innerTy = t;
    while (innerTy) {
      if (auto pft = dyn_cast<PolymorphicFunctionType>(innerTy)) {
        assert(!genericParams
           || pft->getGenericParams().getOuterParameters() == *genericParams);
        genericParams = &pft->getGenericParams();
      }
      innerTy = dyn_cast<AnyFunctionType>(innerTy.getResult());
    }
  }
  GenericParamList *innerGenericParams
    = genericParams ? *genericParams : nullptr;

  auto rebuild = [&](CanType input, CanType result) -> CanAnyFunctionType {
    if (genericParams) {
      assert(!genericSig && "got mix of poly/generic function type?!");
      return CanPolymorphicFunctionType::get(input, result,
                                             innerGenericParams,
                                             extInfo);
    } else if (genericSig) {
      return CanGenericFunctionType::get(genericSig, input, result, extInfo);
    } else {
      return CanFunctionType::get(input, result, extInfo);
    }
  };

  switch (auto rep = t->getExtInfo().getSILRepresentation()) {
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
    // No bridging needed for native functions.
    if (t->getExtInfo() == extInfo && !innerGenericParams)
      return t;
    return rebuild(t.getInput(), t.getResult());

  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
  case SILFunctionTypeRepresentation::ObjCMethod:
    // FIXME: Should consider the originating Clang types of e.g. blocks.
    return rebuild(getBridgedInputType(tc, rep, t.getInput(), decl),
                   getBridgedResultType(tc, rep, t.getResult(), decl));
  }
  llvm_unreachable("bad calling convention");
}

CanAnyFunctionType
TypeConverter::getLoweredASTFunctionType(CanAnyFunctionType t,
                                         unsigned uncurryLevel,
                                         AnyFunctionType::ExtInfo extInfo,
                                         Optional<SILDeclRef> constant) {
  // Get the original Clang type of a node, if we have one.
  const clang::Decl *clangDecl = nullptr;
  if (constant && constant->hasDecl())
    clangDecl = constant->getDecl()->getClangDecl();

  // Fast path: no uncurrying required.
  if (uncurryLevel == 0)
    return getBridgedFunctionType(*this, t, extInfo, clangDecl);

  SILFunctionTypeRepresentation rep = extInfo.getSILRepresentation();
  assert(!extInfo.isAutoClosure() && "autoclosures cannot be curried");
  assert(rep != SILFunctionType::Representation::Block
         && "objc blocks cannot be curried");

  // The uncurried input types.
  SmallVector<TupleTypeElt, 4> inputs;

  // The innermost generic parameter list.
  // FIXME: Interface types make this unnecessary.
  Optional<GenericParamList *> genericParams;

  // The dependent generic signature.
  CanGenericSignature genericSig;
  if (auto gft = dyn_cast<GenericFunctionType>(t)) {
    genericSig = gft.getGenericSignature();
  }

  // Merge inputs and generic parameters from the uncurry levels.
  for (;;) {
    inputs.push_back(TupleTypeElt(t->getInput()));

    if (auto pft = dyn_cast<PolymorphicFunctionType>(t)) {
      assert(!genericParams
             || pft->getGenericParams().getOuterParameters() == *genericParams);
      genericParams = &pft->getGenericParams();
    }

    if (uncurryLevel-- == 0)
      break;
    t = cast<AnyFunctionType>(t.getResult());
  }

  CanType resultType = t.getResult();

  // Bridge input and result types.
  switch (rep) {
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
    // Native functions don't need bridging.
    break;

  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
    for (auto &input : inputs)
      input = input.getWithType(
               getBridgedInputType(*this, rep, CanType(input.getType()),
                                   clangDecl));
    resultType = getBridgedResultType(*this, rep, resultType,
                                      clangDecl);
    break;
  case SILFunctionTypeRepresentation::ObjCMethod: {
    // The "self" parameter should not get bridged unless it's a metatype.
    unsigned skip = 1;
    if (inputs.front().getType()->is<AnyMetatypeType>())
      skip = 0;

    for (auto &input : make_range(inputs.begin() + skip, inputs.end()))
      input = input.getWithType(
                getBridgedInputType(*this, rep, CanType(input.getType()),
                                    clangDecl));
    resultType = getBridgedResultType(*this, rep, resultType, clangDecl);
    break;
  }
  }

  // Put the inputs in the order expected by the calling convention.
  std::reverse(inputs.begin(), inputs.end());

  // Create the new function type.
  CanType inputType = CanType(TupleType::get(inputs, Context));
  GenericParamList *innerGenericParams
    = genericParams ? *genericParams : nullptr;
  if (genericSig) {
    assert(!innerGenericParams && "got mix of Polymorphic/Generic FunctionType?!");
    return CanGenericFunctionType::get(genericSig,
                                       inputType, resultType, extInfo);
  }

  if (innerGenericParams) {
    return CanPolymorphicFunctionType::get(inputType, resultType,
                                           innerGenericParams,
                                           extInfo);
  } else {
    return CanFunctionType::get(inputType, resultType, extInfo);
  }
}

Type TypeConverter::getLoweredBridgedType(Type t,
                                          SILFunctionTypeRepresentation rep,
                                          const clang::Type *clangTy,
                                          BridgedTypePurpose purpose) {
  switch (rep) {
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
    // No bridging needed for native CCs.
    return t;
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::Block:
    // Map native types back to bridged types.

    // Look through optional types.
    if (auto valueTy = t->getOptionalObjectType()) {
      auto Ty = getLoweredCBridgedType(valueTy, clangTy, false);
      return Ty ? OptionalType::get(Ty) : Ty;
    }
    if (auto valueTy = t->getImplicitlyUnwrappedOptionalObjectType()) {
      auto Ty = getLoweredCBridgedType(valueTy, clangTy, false);
      return Ty ? ImplicitlyUnwrappedOptionalType::get(Ty) : Ty;
    }
    return getLoweredCBridgedType(t, clangTy, purpose == ForResult);
  }
};

Type TypeConverter::getLoweredCBridgedType(Type t,
                                           const clang::Type *clangTy,
                                           bool bridgedCollectionsAreOptional) {
  // Bridge String back to NSString.
  auto nativeStringTy = getStringType();
  if (nativeStringTy && t->isEqual(nativeStringTy)) {
    Type bridgedTy = getNSStringType();
    if (bridgedCollectionsAreOptional && clangTy)
      bridgedTy = OptionalType::get(bridgedTy);
    return bridgedTy;
  }

  // Bridge Bool back to ObjC bool, unless the original Clang type was _Bool.
  auto nativeBoolTy = getBoolType();
  if (nativeBoolTy && t->isEqual(nativeBoolTy)) {
    if (clangTy && clangTy->isBooleanType())
      return t;
    return getObjCBoolType();
  }

  // Class metatypes bridge to ObjC metatypes.
  if (auto metaTy = t->getAs<MetatypeType>()) {
    if (metaTy->getInstanceType()->getClassOrBoundGenericClass()) {
      return MetatypeType::get(metaTy->getInstanceType(),
                               MetatypeRepresentation::ObjC);
    }
  }

  // ObjC-compatible existential metatypes.
  if (auto metaTy = t->getAs<ExistentialMetatypeType>()) {
    if (metaTy->getInstanceType()->isObjCExistentialType()) {
      return ExistentialMetatypeType::get(metaTy->getInstanceType(),
                                          MetatypeRepresentation::ObjC);
    }
  }
  
  if (auto funTy = t->getAs<FunctionType>()) {
    switch (funTy->getExtInfo().getSILRepresentation()) {
    // Functions that are already represented as blocks or C function pointers
    // don't need bridging.
    case SILFunctionType::Representation::Block:
    case SILFunctionType::Representation::CFunctionPointer:
    case SILFunctionType::Representation::Thin:
    case SILFunctionType::Representation::Method:
    case SILFunctionType::Representation::ObjCMethod:
    case SILFunctionType::Representation::WitnessMethod:
      return t;
    case SILFunctionType::Representation::Thick:
      // Thick functions (TODO: conditionally) get bridged to blocks.
      return FunctionType::get(funTy->getInput(), funTy->getResult(),
                               funTy->getExtInfo().withSILRepresentation(
                                       SILFunctionType::Representation::Block));
    }
  }

  // Array bridging.
  if (auto arrayDecl = Context.getArrayDecl()) {
    if (t->getAnyNominal() == arrayDecl) {
      Type bridgedTy = getNSArrayType();
      if (bridgedCollectionsAreOptional && clangTy)
        bridgedTy = OptionalType::get(bridgedTy);
      return bridgedTy;
    }
  }

  // Dictionary bridging.
  if (auto dictDecl = Context.getDictionaryDecl()) {
    if (t->getAnyNominal() == dictDecl) {
      Type bridgedTy = getNSDictionaryType();
      if (bridgedCollectionsAreOptional && clangTy)
        bridgedTy = OptionalType::get(bridgedTy);
      return bridgedTy;
    }
  }

  // Set bridging.
  if (auto setDecl = Context.getSetDecl()) {
    if (t->getAnyNominal() == setDecl) {
      Type bridgedTy = getNSSetType();
      if (bridgedCollectionsAreOptional && clangTy)
        bridgedTy = OptionalType::get(bridgedTy);
      return bridgedTy;
    }
  }

  return t;
}
