//===--- TypeCheckInvertible.cpp -  Type checking invertible protocols ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements semantic analysis for evaluating whether a type
// conforms to an invertible protocol. An invertible protocol is a known
// protocol KP for which the type ~KP exists.
//
//===----------------------------------------------------------------------===//

#include "TypeCheckInvertible.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericEnvironment.h"
#include "TypeChecker.h"

namespace swift {

/// MARK: diagnostic utilities

/// Finds a SourceRange to remove an inverse of \c kp applied to the type decl.
static
SourceRange findInverseRemovalRange(const TypeDecl *typeDecl,
                                    KnownProtocolKind targetProto) {
  auto inheritedTypes = typeDecl->getInherited();

  // Check inheritance clause.
  auto entries = inheritedTypes.getEntries();
  for (size_t i = 0; i < entries.size(); i++) {
    auto entry = entries[i];

    if (auto inverse = entry.getType()->getAs<InverseType>())
      if (auto kp = inverse->getInvertedProtocol()->getKnownProtocol())
        if (*kp == targetProto)
          return inheritedTypes.getRemovalRange(i);
  }

  // TODO: just ask HasNoncopyableAnnotationRequest for the annotation?
  // so we can handle where clauses.

  return SourceRange();
}

/// Adds the appropriate fix-it to make the given nominal conform to \c proto.
static void addConformanceFixIt(const NominalTypeDecl *nominal,
                                InFlightDiagnostic &diag,
                                KnownProtocolKind proto,
                                bool inverse) {
  SmallString<64> text;
  if (nominal->getInherited().empty()) {
    SourceLoc fixItLoc = nominal->getBraces().Start;
    text.append(": ");
    if (inverse) text.append("~");
    text.append(getProtocolName(proto));
    diag.fixItInsert(fixItLoc, text);
  } else {
    auto fixItLoc = nominal->getInherited().getEndLoc();
    text.append(", ");
    if (inverse) text.append("~");
    text.append(getProtocolName(proto));
    diag.fixItInsertAfter(fixItLoc, text);
  }
}

/// Emit fix-it's to help the user resolve a containment issue where the
/// \c nonConformingTy needs to be made to conform to \c kp to resolve a
/// containment issue.
/// \param enclosingNom is the nominal type containing a nonconforming value
/// \param nonConformingTy is the type of the nonconforming value
static void tryEmitContainmentFixits(NominalTypeDecl *enclosingNom,
                                     Type nonConformingTy,
                                     KnownProtocolKind kp) {
  auto *module = enclosingNom->getParentModule();

  // First and most universal suggestion, add the inverse to the enclosing type.
  {
    auto diag = enclosingNom->diagnose(diag::add_inverse_for_containment,
                                       enclosingNom,
                                       getProtocolName(kp));
    addConformanceFixIt(enclosingNom, diag, kp, /*inverse=*/true);
  }

  // If it's a generic parameter defined in the same module, suggest removing
  // the ~KP constraint
  if (auto genericArchetype = nonConformingTy->getAs<ArchetypeType>()) {
    auto interfaceType = genericArchetype->getInterfaceType();
    if (auto genericParamType =
        interfaceType->getAs<GenericTypeParamType>()) {
      auto *genericParamTypeDecl = genericParamType->getDecl();
      if (genericParamTypeDecl &&
          genericParamTypeDecl->getModuleContext() == module) {
        auto diag = genericParamTypeDecl->diagnose(
            diag::remove_inverse_on_generic_parameter_for_conformance,
            nonConformingTy, getProtocolName(kp));

        if (auto range = findInverseRemovalRange(genericParamTypeDecl, kp))
          diag.fixItRemove(range);
      }
    }

    // If the offending type is a nominal defined in the same module...
  } else if (auto nominal = nonConformingTy->getAnyNominal()) {
    if (nominal->getModuleContext() == module) {
      // if it has a ~KP on it explicitly, suggest removing it.
      if (auto range = findInverseRemovalRange(nominal, kp)) {
        nominal->diagnose(
                diag::remove_inverse_on_nominal_for_conformance,
                nominal, getProtocolName(kp))
            .fixItRemove(range);
      } else if (kp == KnownProtocolKind::Copyable) {
        // Otherwise, it became noncopyable due to a ~Copyable on one of its
        // generic params, so suggest adding an explicit Copyable in the
        // inheritance clause.
        auto diag = nominal->diagnose(
            diag::add_explicit_protocol_for_conformance,
            nominal, getProtocolName(kp));
        addConformanceFixIt(nominal, diag, kp, /*inverse=*/false);
      }
    }
  }
}

/// MARK: conformance queries

bool IsNoncopyableRequest::evaluate(Evaluator &evaluator,
                                    CanType type) const {
  assert(!type->hasTypeParameter() && "forgot to mapTypeIntoContext first");
  auto &ctx = type->getASTContext();

  // Pack expansions such as `repeat T` themselves do not have conformances,
  // so check its pattern type for conformance.
  if (auto *pet = type->getAs<PackExpansionType>()) {
    type = pet->getPatternType()->getCanonicalType();
  }

  auto *copyable = ctx.getProtocol(KnownProtocolKind::Copyable);
  if (!copyable)
    llvm_unreachable("missing Copyable protocol!");

  const bool conforms =
      (bool)TypeChecker::conformsToProtocol(type,
                                            copyable,
                                            copyable->getParentModule(),
                                            /*allowMissing=*/false);

  return !conforms;
}

/// MARK: conformance checking

bool checkCopyableConformance(ProtocolConformance *conformance) {
  auto *proto = conformance->getProtocol();
  assert(proto->isSpecificProtocol(KnownProtocolKind::Copyable));

  auto *nom = conformance->getType()->getAnyNominal();
  assert(nom && "non-nominal with conformance?");
  if (!nom)
    return false;

  // All classes can store noncopyable values.
  if (isa<ClassDecl>(nom))
    return true;

  // Protocols do not directly define any storage.
  if (isa<ProtocolDecl>(nom))
    return true;

  if (isa<BuiltinTupleDecl>(nom))
    llvm_unreachable("TODO: BuiltinTupleDecl");

  // NOTE: A deinit prevents a struct or enum from conforming to Copyable, but
  // we will emit an error for that elsewhere already.

  // Otherwise, we have to check its storage to ensure it is all Copyable.

  class HasNoncopyable: public StorageVisitor {
    NominalTypeDecl *Nominal;
    DeclContext *DC;
    bool Diagnosing;
  public:
    HasNoncopyable(NominalTypeDecl *nom, DeclContext *dc, bool diagnose)
        : Nominal(nom), DC(dc), Diagnosing(diagnose) {}

    bool visit() { return StorageVisitor::visit(Nominal, DC); }

    bool check(ValueDecl *storage, Type type, bool isEnum) {
      // ignore invalid storage.
      if (type->hasError())
        return false;

      if (!type->isNoncopyable(DC))
        return false;

      if (!Diagnosing)
        return true; // it's noncopyable

      storage->diagnose(diag::noncopyable_type_member_in_copyable, type,
                        isEnum, storage->getName(), Nominal);

      tryEmitContainmentFixits(Nominal, type, KnownProtocolKind::Copyable);
      return true;
    }

    /// Handle a stored property.
    /// \returns true iff this visitor should stop its walk over the nominal.
    bool operator()(VarDecl *property, Type propertyType) override {
      return check(property, propertyType, /*isEnum=*/false);
    }

    /// Handle an enum associated value.
    /// \returns true iff this visitor should stop its walk over the nominal.
    bool operator()(EnumElementDecl *element, Type elementType) override {
      return check(element, elementType, /*isEnum=*/true);
    }
  };

  // This nominal cannot be Copyable if it contains noncopyable storage.
  return !HasNoncopyable(nom, conformance->getDeclContext(),
                         /*diagnose=*/true).visit();
}

/// Visit the instance storage of the given nominal type as seen through
/// the given declaration context.
bool StorageVisitor::visit(NominalTypeDecl *nominal, DeclContext *dc) {
  // Walk the stored properties of classes and structs.
  if (isa<StructDecl>(nominal) || isa<ClassDecl>(nominal)) {
    for (auto property : nominal->getStoredProperties()) {
      auto propertyType = dc->mapTypeIntoContext(property->getInterfaceType())
          ->getRValueType()->getReferenceStorageReferent();
      if ((*this)(property, propertyType))
        return true;
    }

    return false;
  }

  // Walk the enum elements that have associated values.
  if (auto enumDecl = dyn_cast<EnumDecl>(nominal)) {
    for (auto caseDecl : enumDecl->getAllCases()) {
      for (auto element : caseDecl->getElements()) {
        if (!element->hasAssociatedValues())
          continue;

        // Check that the associated value type is Sendable.
        auto elementType = dc->mapTypeIntoContext(
            element->getArgumentInterfaceType());
        if ((*this)(element, elementType))
          return true;
      }
    }

    return false;
  }

  assert(!isa<ProtocolDecl>(nominal) || !isa<BuiltinTupleDecl>(nominal));
  return false;
}

/// Produces implicit ProtocolConformances for known protocols. Does _not_ check
/// whether the conformance is valid. That's done in a different step
/// (see `TypeChecker::checkConformancesInContext`).
ProtocolConformance *deriveConformanceForInvertible(Evaluator &evaluator,
                                                    NominalTypeDecl *nominal,
                                                    KnownProtocolKind kp) {
  auto &ctx = nominal->getASTContext();
  auto *proto = ctx.getProtocol(kp);

  switch (kp) {
  case KnownProtocolKind::Copyable: {
    if (evaluateOrDefault(evaluator, HasNoncopyableAnnotationRequest{nominal}, false))
      return nullptr; // it's not Copyable.

    // TODO: generate conditional conformances implied by ~Copyable on generic params.

    // form a full unconditional conformance to Copyable.
    auto conformance = ctx.getNormalConformance(
        nominal->getDeclaredInterfaceType(), proto, nominal->getLoc(),
        nominal, ProtocolConformanceState::Complete,
        /*isUnchecked=*/false);
    conformance->setSourceKindAndImplyingConformance(
        ConformanceEntryKind::Synthesized, nullptr);

    nominal->registerProtocolConformance(conformance, /*synthesized=*/true);
    return conformance;
  }
  default:
    llvm_unreachable("not invertible");
  }
}

}
