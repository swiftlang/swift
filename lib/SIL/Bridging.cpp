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

CanType TypeConverter::getBridgedInputType(SILFunctionTypeRepresentation rep,
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

      Type bridged = getLoweredBridgedType(elt.getType(), rep, clangInputTy,
                                           TypeConverter::ForArgument);
      if (!bridged) {
        Context.Diags.diagnose(SourceLoc(), diag::could_not_find_bridge_type,
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
  auto loweredBridgedType = getLoweredBridgedType(input, rep, clangInputTy,
                                                  TypeConverter::ForArgument);

  if (!loweredBridgedType) {
    Context.Diags.diagnose(SourceLoc(), diag::could_not_find_bridge_type,
                           input);

    llvm::report_fatal_error("unable to set up the ObjC bridge!");
  }

  return loweredBridgedType->getCanonicalType();
}

/// Bridge a result type.
CanType TypeConverter::getBridgedResultType(SILFunctionTypeRepresentation rep,
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

  auto loweredType = getLoweredBridgedType(result, rep, clangResultTy,
                                           TypeConverter::ForResult);

  if (!loweredType) {
    Context.Diags.diagnose(SourceLoc(), diag::could_not_find_bridge_type,
                           result);

    llvm::report_fatal_error("unable to set up the ObjC bridge!");
  }

  return loweredType->getCanonicalType();
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
