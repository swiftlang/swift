//===--- TypeCheckRequests.cpp - Type Checking Requests ------------------===//
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
#include "TypeChecker.h"
#include "TypeCheckType.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/Attr.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/Types.h"
#include "swift/Subsystems.h"

using namespace swift;

llvm::Expected<Type>
InheritedTypeRequest::evaluate(
    Evaluator &evaluator, llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
    unsigned index,
    TypeResolutionStage stage) const {
  // Figure out how to resolve types.
  TypeResolutionOptions options = None;
  DeclContext *dc;
  if (auto typeDecl = decl.dyn_cast<TypeDecl *>()) {
    if (auto nominal = dyn_cast<NominalTypeDecl>(typeDecl)) {
      dc = nominal;

      options |= TypeResolutionFlags::AllowUnavailableProtocol;
    } else {
      dc = typeDecl->getDeclContext();
    }
  } else {
    auto ext = decl.get<ExtensionDecl *>();
    dc = ext;
    options |= TypeResolutionFlags::AllowUnavailableProtocol;
  }

  Optional<TypeResolution> resolution;
  switch (stage) {
  case TypeResolutionStage::Structural:
    resolution = TypeResolution::forStructural(dc);
    break;

  case TypeResolutionStage::Interface:
    resolution = TypeResolution::forInterface(dc);
    break;

  case TypeResolutionStage::Contextual: {
    // Compute the contextual type by mapping the interface type into context.
    auto result =
      evaluator(InheritedTypeRequest{decl, index,
                                     TypeResolutionStage::Interface});
    if (!result)
      return result;

    return dc->mapTypeIntoContext(*result);
  }
  }

  TypeLoc &typeLoc = getTypeLoc(decl, index);

  Type inheritedType;
  if (typeLoc.getTypeRepr())
    inheritedType = resolution->resolveType(typeLoc.getTypeRepr(), options);
  else
    inheritedType = typeLoc.getType();

  return inheritedType ? inheritedType : ErrorType::get(dc->getASTContext());
}

llvm::Expected<Type>
SuperclassTypeRequest::evaluate(Evaluator &evaluator,
                                NominalTypeDecl *nominalDecl,
                                TypeResolutionStage stage) const {
  assert(isa<ClassDecl>(nominalDecl) || isa<ProtocolDecl>(nominalDecl));

  // If this is a protocol that came from a serialized module, compute the
  // superclass via its generic signature.
  if (auto *proto = dyn_cast<ProtocolDecl>(nominalDecl)) {
    if (proto->wasDeserialized()) {
      return proto->getGenericSignature()
          ->getSuperclassBound(proto->getSelfInterfaceType());
    }
  }

  for (unsigned int idx : indices(nominalDecl->getInherited())) {
    auto result = evaluator(InheritedTypeRequest{nominalDecl, idx, stage});

    if (auto err = result.takeError()) {
      // FIXME: Should this just return once a cycle is detected?
      llvm::handleAllErrors(std::move(err),
        [](const CyclicalRequestError<InheritedTypeRequest> &E) {
          /* cycle detected */
        });
      continue;
    }

    Type inheritedType = *result;
    if (!inheritedType) continue;

    // If we found a class, return it.
    if (inheritedType->getClassOrBoundGenericClass()) {
      return inheritedType;
    }

    // If we found an existential with a superclass bound, return it.
    if (inheritedType->isExistentialType()) {
      if (auto superclassType =
            inheritedType->getExistentialLayout().explicitSuperclass) {
        if (superclassType->getClassOrBoundGenericClass()) {
          return superclassType;
        }
      }
    }
  }

  // No superclass.
  return Type();
}

llvm::Expected<Type>
EnumRawTypeRequest::evaluate(Evaluator &evaluator, EnumDecl *enumDecl,
                             TypeResolutionStage stage) const {
  for (unsigned int idx : indices(enumDecl->getInherited())) {
    auto inheritedTypeResult =
      evaluator(InheritedTypeRequest{enumDecl, idx, stage});
    
    if (auto err = inheritedTypeResult.takeError()) {
      llvm::handleAllErrors(std::move(err),
        [](const CyclicalRequestError<InheritedTypeRequest> &E) {
          // cycle detected
        });
      continue;
    }

    auto &inheritedType = *inheritedTypeResult;
    if (!inheritedType) continue;

    // Skip existential types.
    if (inheritedType->isExistentialType()) continue;

    // We found a raw type; return it.
    return inheritedType;
  }

  // No raw type.
  return Type();
}

llvm::Expected<CustomAttr *>
AttachedFunctionBuilderRequest::evaluate(Evaluator &evaluator,
                                         ValueDecl *decl) const {
  ASTContext &ctx = decl->getASTContext();
  auto dc = decl->getDeclContext();
  for (auto attr : decl->getAttrs().getAttributes<CustomAttr>()) {
    auto mutableAttr = const_cast<CustomAttr *>(attr);
    // Figure out which nominal declaration this custom attribute refers to.
    auto nominal = evaluateOrDefault(ctx.evaluator,
                                     CustomAttrNominalRequest{mutableAttr, dc},
                                     nullptr);

    // Ignore unresolvable custom attributes.
    if (!nominal)
      continue;

    // Return the first custom attribute that is a function builder type.
    if (nominal->getAttrs().hasAttribute<FunctionBuilderAttr>())
      return mutableAttr;
  }

  return nullptr;
}

llvm::Expected<Type>
FunctionBuilderTypeRequest::evaluate(Evaluator &evaluator,
                                     ValueDecl *decl) const {
  // Look for a function-builder custom attribute.
  auto attr = decl->getAttachedFunctionBuilder();
  if (!attr) return Type();

  // Resolve a type for the attribute.
  auto mutableAttr = const_cast<CustomAttr*>(attr);
  auto dc = decl->getDeclContext();
  auto &ctx = dc->getASTContext();
  Type type = resolveCustomAttrType(mutableAttr, dc,
                                    CustomAttrTypeKind::NonGeneric);
  if (!type) return Type();

  auto nominal = type->getAnyNominal();
  if (!nominal) {
    assert(ctx.Diags.hadAnyError());
    return Type();
  }

  // Do some additional checking on parameters.
  if (auto param = dyn_cast<ParamDecl>(decl)) {
    // The parameter had better already have an interface type.
    Type paramType = param->getInterfaceType();
    assert(paramType);
    auto paramFnType = paramType->getAs<FunctionType>();

    // Require the parameter to be an interface type.
    if (!paramFnType) {
      ctx.Diags.diagnose(attr->getLocation(),
                         diag::function_builder_parameter_not_of_function_type,
                         nominal->getFullName());
      mutableAttr->setInvalid();
      return Type();
    }

    // Forbid the parameter to be an autoclosure.
    if (param->isAutoClosure()) {
      ctx.Diags.diagnose(attr->getLocation(),
                         diag::function_builder_parameter_autoclosure,
                         nominal->getFullName());
      mutableAttr->setInvalid();
      return Type();
    }
  }

  return type->mapTypeOutOfContext();
}

// Define request evaluation functions for each of the type checker requests.
static AbstractRequestFunction *typeCheckerRequestFunctions[] = {
#define SWIFT_REQUEST(Zone, Name)                      \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/AST/TypeCheckerTypeIDZone.def"
#undef SWIFT_REQUEST
};

void swift::registerTypeCheckerRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(Zone::TypeChecker,
                                     typeCheckerRequestFunctions);
}
