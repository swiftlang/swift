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
#include "GenericTypeResolver.h"
#include "TypeChecker.h"
#include "swift/Sema/TypeCheckRequests.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/TypeLoc.h"
#include "swift/AST/Types.h"
using namespace swift;

namespace swift {
// Implement the type checker type zone (zone 10).
#define SWIFT_TYPEID_ZONE 10
#define SWIFT_TYPEID_HEADER "swift/Sema/TypeCheckerTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"

}

void swift::simple_display(
       llvm::raw_ostream &out,
       const llvm::PointerUnion<TypeDecl *, ExtensionDecl *> &value) {
  if (auto type = value.dyn_cast<TypeDecl *>()) {
    type->dumpRef(out);
    return;
  }

  auto ext = value.get<ExtensionDecl *>();
  out << "extension of ";
  ext->getAsNominalTypeOrNominalTypeExtensionContext()->dumpRef(out);
}

//----------------------------------------------------------------------------//
// Inherited type computation.
//----------------------------------------------------------------------------//

TypeLoc &InheritedTypeRequest::getTypeLoc(
                        llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
                        unsigned index) const {
  if (auto typeDecl = decl.dyn_cast<TypeDecl *>())
    return typeDecl->getInherited()[index];

  return decl.get<ExtensionDecl *>()->getInherited()[index];
}

Type InheritedTypeRequest::operator()(
                        Evaluator &evaluator,
                        llvm::PointerUnion<TypeDecl *, ExtensionDecl *> decl,
                        unsigned index) const {
  // Figure out how to resolve types.
  TypeResolutionOptions options;
  DeclContext *dc;
  if (auto typeDecl = decl.dyn_cast<TypeDecl *>()) {
    if (auto nominal = dyn_cast<NominalTypeDecl>(typeDecl)) {
      dc = nominal;
      options |= TypeResolutionFlags::GenericSignature;
      options |= TypeResolutionFlags::InheritanceClause;
      options |= TypeResolutionFlags::AllowUnavailableProtocol;
    } else {
      dc = typeDecl->getDeclContext();

      if (isa<GenericTypeParamDecl>(typeDecl)) {
        // For generic parameters, we want name lookup to look at just the
        // signature of the enclosing entity.
        if (auto nominal = dyn_cast<NominalTypeDecl>(dc)) {
          dc = nominal;
          options |= TypeResolutionFlags::GenericSignature;
        } else if (auto ext = dyn_cast<ExtensionDecl>(dc)) {
          dc = ext;
          options |= TypeResolutionFlags::GenericSignature;
        } else if (auto func = dyn_cast<AbstractFunctionDecl>(dc)) {
          dc = func;
          options |= TypeResolutionFlags::GenericSignature;
        } else if (!dc->isModuleScopeContext()) {
          // Skip the generic parameter's context entirely.
          dc = dc->getParent();
        }
      }
    }
  } else {
    auto ext = decl.get<ExtensionDecl *>();
    dc = ext;
    options |= TypeResolutionFlags::GenericSignature;
    options |= TypeResolutionFlags::InheritanceClause;
    options |= TypeResolutionFlags::AllowUnavailableProtocol;
  }

  ProtocolRequirementTypeResolver protoResolver;
  GenericTypeToArchetypeResolver archetypeResolver(dc);
  GenericTypeResolver *resolver;
  if (isa<ProtocolDecl>(dc)) {
    resolver = &protoResolver;
  } else {
    resolver = &archetypeResolver;
  }

  // FIXME: Hack for calls through here when we have no type checker.
  auto lazyResolver = dc->getASTContext().getLazyResolver();
  if (!lazyResolver) return ErrorType::get(dc->getASTContext());

  TypeChecker &tc = *static_cast<TypeChecker *>(lazyResolver);
  TypeLoc &typeLoc = getTypeLoc(decl, index);

  Type inheritedType =
    tc.resolveType(typeLoc.getTypeRepr(), dc, options, resolver);
  if (inheritedType && !isa<ProtocolDecl>(dc))
    inheritedType = inheritedType->mapTypeOutOfContext();
  return inheritedType ? inheritedType : ErrorType::get(tc.Context);
}

Optional<Type> InheritedTypeRequest::getCachedResult() const {
  const auto &storage = getStorage();
  auto &typeLoc = getTypeLoc(std::get<0>(storage), std::get<1>(storage));
  if (typeLoc.wasValidated())
    return typeLoc.getType();

  return None;
}

void InheritedTypeRequest::cacheResult(Type value) const {
  const auto &storage = getStorage();
  auto &typeLoc = getTypeLoc(std::get<0>(storage), std::get<1>(storage));
  typeLoc.setType(value, true);
}

//----------------------------------------------------------------------------//
// Superclass computation.
//----------------------------------------------------------------------------//
Type SuperclassTypeRequest::operator()(Evaluator &evaluator,
                                       ClassDecl *classDecl) const {
  for (unsigned int idx : indices(classDecl->getInherited())) {
    Type inheritedType = evaluator(InheritedTypeRequest{classDecl, idx});
    if (!inheritedType) continue;

    // If we found a class, return it.
    if (inheritedType->getClassOrBoundGenericClass()) {
      if (inheritedType->hasArchetype())
        return inheritedType->mapTypeOutOfContext();

      return inheritedType;
    }

    // If we found an existential with a superclass bound, return it.
    if (inheritedType->isExistentialType()) {
      if (auto superclassType =
            inheritedType->getExistentialLayout().superclass) {
        if (superclassType->getClassOrBoundGenericClass()) {
          if (superclassType->hasArchetype())
            return superclassType->mapTypeOutOfContext();

          return superclassType;
        }
      }
    }
  }

  // No superclass.
  return Type();
}

void SuperclassTypeRequest::diagnoseCycle(DiagnosticEngine &diags) const {
  // FIXME: Improve this diagnostic.
  auto classDecl = std::get<0>(getStorage());
  std::string className = "'" + std::string(classDecl->getNameStr()) + "'";
  diags.diagnose(classDecl, diag::circular_class_inheritance, className);
}

void SuperclassTypeRequest::noteCycleStep(DiagnosticEngine &diags) const {
  auto classDecl = std::get<0>(getStorage());
  // FIXME: Customize this further.
  diags.diagnose(classDecl, diag::circular_reference_through);
}

Optional<Type> SuperclassTypeRequest::getCachedResult() const {
  auto classDecl = std::get<0>(getStorage());
  if (classDecl->LazySemanticInfo.Superclass.getInt())
    return classDecl->LazySemanticInfo.Superclass.getPointer();

  return None;
}

void SuperclassTypeRequest::cacheResult(Type value) const {
  auto classDecl = std::get<0>(getStorage());
  classDecl->LazySemanticInfo.Superclass.setPointerAndInt(value, true);
}
