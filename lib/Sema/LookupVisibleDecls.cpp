//===--- LookupVisibleDecls - Swift Name Lookup Routines ------------------===//
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
// This file implements the lookupVisibleDecls interface for visiting named
// declarations.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/ImportCache.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/ModuleNameLookup.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Sema/IDETypeCheckingRequests.h"
#include "swift/Sema/IDETypeChecking.h"
#include "llvm/ADT/SetVector.h"
#include <set>

using namespace swift;

namespace {
struct LookupState {
private:
  /// If \c false, an unqualified lookup of all visible decls in a
  /// DeclContext.
  ///
  /// If \c true, lookup of all visible members of a given object (possibly of
  /// metatype type).
  unsigned IsQualified : 1;

  /// Is this a qualified lookup on a metatype?
  unsigned IsOnMetatype : 1;

  /// Did we recurse into a superclass?
  unsigned IsOnSuperclass : 1;

  unsigned InheritsSuperclassInitializers : 1;

  /// Should instance members be included even if lookup is performed on a type?
  unsigned IncludeInstanceMembers : 1;

  LookupState()
      : IsQualified(0), IsOnMetatype(0), IsOnSuperclass(0),
        InheritsSuperclassInitializers(0), IncludeInstanceMembers(0) {}

public:
  LookupState(const LookupState &) = default;

  static LookupState makeQualified() {
    LookupState Result;
    Result.IsQualified = 1;
    return Result;
  }

  static LookupState makeUnqualified() {
    LookupState Result;
    Result.IsQualified = 0;
    return Result;
  }

  bool isQualified() const { return IsQualified; }
  bool isOnMetatype() const { return IsOnMetatype; }
  bool isOnSuperclass() const { return IsOnSuperclass; }
  bool isInheritsSuperclassInitializers() const {
    return InheritsSuperclassInitializers;
  }
  bool isIncludingInstanceMembers() const { return IncludeInstanceMembers; }

  LookupState withOnMetatype() const {
    auto Result = *this;
    Result.IsOnMetatype = 1;
    return Result;
  }

  LookupState withOnSuperclass() const {
    auto Result = *this;
    Result.IsOnSuperclass = 1;
    return Result;
  }

  LookupState withInheritsSuperclassInitializers() const {
    auto Result = *this;
    Result.InheritsSuperclassInitializers = 1;
    return Result;
  }

  LookupState withoutInheritsSuperclassInitializers() const {
    auto Result = *this;
    Result.InheritsSuperclassInitializers = 0;
    return Result;
  }

  LookupState withIncludedInstanceMembers() const {
    auto Result = *this;
    Result.IncludeInstanceMembers = 1;
    return Result;
  }
};
} // end anonymous namespace

static bool areTypeDeclsVisibleInLookupMode(LookupState LS) {
  // Nested type declarations can be accessed only with unqualified lookup or
  // on metatypes.
  return !LS.isQualified() || LS.isOnMetatype();
}

static bool isDeclVisibleInLookupMode(ValueDecl *Member, LookupState LS,
                                      const DeclContext *FromContext) {
  // Accessors are never visible directly in the source language.
  if (isa<AccessorDecl>(Member))
    return false;
  
  // Check access when relevant.
  if (!Member->getDeclContext()->isLocalContext() &&
      !isa<GenericTypeParamDecl>(Member) && !isa<ParamDecl>(Member)) {
    if (!Member->isAccessibleFrom(FromContext))
      return false;
  }

  if (auto *FD = dyn_cast<FuncDecl>(Member)) {
    // Cannot call static functions on non-metatypes.
    if (!LS.isOnMetatype() && FD->isStatic())
      return false;

    // Otherwise, either call a function or curry it.
    return true;
  }
  if (auto *SD = dyn_cast<SubscriptDecl>(Member)) {
    // Cannot use static subscripts on non-metatypes.
    if (!LS.isOnMetatype() && SD->isStatic())
      return false;

    // Cannot use instance subscript on metatypes.
    if (LS.isOnMetatype() && !SD->isStatic() && !LS.isIncludingInstanceMembers())
      return false;

    return true;
  }
  if (auto *VD = dyn_cast<VarDecl>(Member)) {
    // Cannot use static properties on non-metatypes.
    if (!(LS.isQualified() && LS.isOnMetatype()) && VD->isStatic())
      return false;

    // Cannot use instance properties on metatypes.
    if (LS.isOnMetatype() && !VD->isStatic() && !LS.isIncludingInstanceMembers())
      return false;

    return true;
  }
  if (isa<EnumElementDecl>(Member)) {
    // Cannot reference enum elements on non-metatypes.
    if (!(LS.isQualified() && LS.isOnMetatype()))
      return false;
  }
  if (auto CD = dyn_cast<ConstructorDecl>(Member)) {
    // Constructors with stub implementations cannot be called in Swift.
    if (CD->hasStubImplementation())
      return false;
    if (LS.isQualified() && LS.isOnSuperclass()) {
      // Cannot call initializers from a superclass, except for inherited
      // convenience initializers.
      return LS.isInheritsSuperclassInitializers() && CD->isInheritable();
    }
  }
  if (isa<TypeDecl>(Member))
    return areTypeDeclsVisibleInLookupMode(LS);

  return true;
}

/// Collect visble members from \p Parent into \p FoundDecls .
static void collectVisibleMemberDecls(const DeclContext *CurrDC, LookupState LS,
                                      Type BaseType,
                                      IterableDeclContext *Parent,
                                      SmallVectorImpl<ValueDecl *> &FoundDecls) {
  for (auto Member : Parent->getMembers()) {
    auto *VD = dyn_cast<ValueDecl>(Member);
    if (!VD)
      continue;
    if (!isDeclVisibleInLookupMode(VD, LS, CurrDC))
      continue;
    if (!evaluateOrDefault(CurrDC->getASTContext().evaluator,
        IsDeclApplicableRequest(DeclApplicabilityOwner(CurrDC, BaseType, VD)),
                           false))
      continue;
    FoundDecls.push_back(VD);
  }
}

static void
synthesizePropertyWrapperStorageWrapperProperties(IterableDeclContext *IDC);

/// Lookup members in extensions of \p LookupType, using \p BaseType as the
/// underlying type when checking any constraints on the extensions.
static void doGlobalExtensionLookup(Type BaseType,
                                    Type LookupType,
                                    SmallVectorImpl<ValueDecl *> &FoundDecls,
                                    const DeclContext *CurrDC,
                                    LookupState LS,
                                    DeclVisibilityKind Reason) {
  auto nominal = LookupType->getAnyNominal();

  // Look in each extension of this type.
  for (auto extension : nominal->getExtensions()) {
    if (!evaluateOrDefault(CurrDC->getASTContext().evaluator,
        IsDeclApplicableRequest(DeclApplicabilityOwner(CurrDC, BaseType,
                                                       extension)), false))
      continue;

    synthesizePropertyWrapperStorageWrapperProperties(extension);

    collectVisibleMemberDecls(CurrDC, LS, BaseType, extension, FoundDecls);
  }

  // Handle shadowing.
  removeShadowedDecls(FoundDecls, CurrDC);
}

/// Enumerate immediate members of the type \c LookupType and its
/// extensions, as seen from the context \c CurrDC.
///
/// Don't do lookup into superclasses or implemented protocols.  Uses
/// \p BaseType as the underlying type when checking any constraints on the
/// extensions.
static void lookupTypeMembers(Type BaseType, Type LookupType,
                              VisibleDeclConsumer &Consumer,
                              const DeclContext *CurrDC, LookupState LS,
                              DeclVisibilityKind Reason) {
  NominalTypeDecl *D = LookupType->getAnyNominal();
  assert(D && "should have a nominal type");

  SmallVector<ValueDecl*, 2> FoundDecls;
  collectVisibleMemberDecls(CurrDC, LS, BaseType, D, FoundDecls);

  doGlobalExtensionLookup(BaseType, LookupType, FoundDecls, CurrDC, LS, Reason);

  // Report the declarations we found to the consumer.
  for (auto *VD : FoundDecls)
    Consumer.foundDecl(VD, Reason);
}

/// Enumerate AnyObject declarations as seen from context \c CurrDC.
static void doDynamicLookup(VisibleDeclConsumer &Consumer,
                            const DeclContext *CurrDC,
                            LookupState LS) {
  class DynamicLookupConsumer : public VisibleDeclConsumer {
    VisibleDeclConsumer &ChainedConsumer;
    LookupState LS;
    const DeclContext *CurrDC;
    llvm::DenseSet<std::pair<DeclBaseName, CanType>> FunctionsReported;
    llvm::DenseSet<CanType> SubscriptsReported;
    llvm::DenseSet<std::pair<Identifier, CanType>> PropertiesReported;

  public:
    explicit DynamicLookupConsumer(VisibleDeclConsumer &ChainedConsumer,
                                   LookupState LS, const DeclContext *CurrDC)
        : ChainedConsumer(ChainedConsumer), LS(LS), CurrDC(CurrDC) {}

    void foundDecl(ValueDecl *D, DeclVisibilityKind Reason,
                   DynamicLookupInfo) override {
      // If the declaration has an override, name lookup will also have found
      // the overridden method.  Skip this declaration, because we prefer the
      // overridden method.
      if (D->getOverriddenDecl())
        return;

      // If the declaration is not @objc, it cannot be called dynamically.
      if (!D->isObjC())
        return;

      if (D->isRecursiveValidation())
        return;

      switch (D->getKind()) {
#define DECL(ID, SUPER) \
      case DeclKind::ID:
#define VALUE_DECL(ID, SUPER)
#include "swift/AST/DeclNodes.def"
        llvm_unreachable("not a ValueDecl!");

      // Types cannot be found by dynamic lookup.
      case DeclKind::GenericTypeParam:
      case DeclKind::AssociatedType:
      case DeclKind::TypeAlias:
      case DeclKind::Enum:
      case DeclKind::Class:
      case DeclKind::Struct:
      case DeclKind::Protocol:
      case DeclKind::OpaqueType:
        return;

      // Initializers cannot be found by dynamic lookup.
      case DeclKind::Constructor:
      case DeclKind::Destructor:
        return;

      // These cases are probably impossible here but can also just
      // be safely ignored.
      case DeclKind::Param:
      case DeclKind::Module:
      case DeclKind::EnumElement:
        return;

      // For other kinds of values, check if we already reported a decl
      // with the same signature.

      case DeclKind::Accessor:
      case DeclKind::Func: {
        auto FD = cast<FuncDecl>(D);
        assert(FD->hasImplicitSelfDecl() && "should not find free functions");
        (void)FD;

        if (FD->isInvalid())
          break;

        // Get the type without the first uncurry level with 'self'.
        CanType T = FD->getMethodInterfaceType()->getCanonicalType();

        auto Signature = std::make_pair(D->getBaseName(), T);
        if (!FunctionsReported.insert(Signature).second)
          return;
        break;
      }

      case DeclKind::Subscript: {
        auto Signature = D->getInterfaceType()->getCanonicalType();
        if (!SubscriptsReported.insert(Signature).second)
          return;
        break;
      }

      case DeclKind::Var: {
        auto *VD = cast<VarDecl>(D);
        auto Signature =
            std::make_pair(VD->getName(),
                           VD->getInterfaceType()->getCanonicalType());
        if (!PropertiesReported.insert(Signature).second)
          return;
        break;
      }
      }

      if (isDeclVisibleInLookupMode(D, LS, CurrDC))
        ChainedConsumer.foundDecl(D, DeclVisibilityKind::DynamicLookup,
                                  DynamicLookupInfo::AnyObject);
    }
  };

  DynamicLookupConsumer ConsumerWrapper(Consumer, LS, CurrDC);

  for (auto Import : namelookup::getAllImports(CurrDC)) {
    Import.second->lookupClassMembers(Import.first, ConsumerWrapper);
  }
}

namespace {
  typedef llvm::SmallPtrSet<TypeDecl *, 8> VisitedSet;
} // end anonymous namespace

static DeclVisibilityKind getReasonForSuper(DeclVisibilityKind Reason) {
  switch (Reason) {
  case DeclVisibilityKind::MemberOfCurrentNominal:
  case DeclVisibilityKind::MemberOfProtocolImplementedByCurrentNominal:
  case DeclVisibilityKind::MemberOfSuper:
    return DeclVisibilityKind::MemberOfSuper;

  case DeclVisibilityKind::MemberOfOutsideNominal:
    return DeclVisibilityKind::MemberOfOutsideNominal;

  default:
    llvm_unreachable("should not see this kind");
  }
}

static void lookupDeclsFromProtocolsBeingConformedTo(
    Type BaseTy, VisibleDeclConsumer &Consumer, LookupState LS,
    const DeclContext *FromContext, DeclVisibilityKind Reason,
    VisitedSet &Visited) {
  NominalTypeDecl *CurrNominal = BaseTy->getAnyNominal();
  if (!CurrNominal)
    return;
  ModuleDecl *Module = FromContext->getParentModule();

  for (auto Conformance : CurrNominal->getAllConformances()) {
    auto Proto = Conformance->getProtocol();
    if (!Proto->isAccessibleFrom(FromContext))
      continue;

    // Skip unsatisfied conditional conformances.
    // We can't check them if this type has an UnboundGenericType or if they
    // couldn't be computed, so assume they conform in such cases.
    if (!BaseTy->hasUnboundGenericType()) {
      if (auto res = Conformance->getConditionalRequirementsIfAvailable()) {
        if (!res->empty() && !Module->conformsToProtocol(BaseTy, Proto))
          continue;
      }
    }

    DeclVisibilityKind ReasonForThisProtocol;
    if (Reason == DeclVisibilityKind::MemberOfCurrentNominal)
      ReasonForThisProtocol =
          DeclVisibilityKind::MemberOfProtocolImplementedByCurrentNominal;
    else
      ReasonForThisProtocol = getReasonForSuper(Reason);

    if (auto NormalConformance = dyn_cast<NormalProtocolConformance>(
          Conformance->getRootConformance())) {
      for (auto Member : Proto->getMembers()) {
        if (auto *ATD = dyn_cast<AssociatedTypeDecl>(Member)) {
          // Skip type decls if they aren't visible, or any type that has a
          // witness. This cuts down on duplicates.
          if (areTypeDeclsVisibleInLookupMode(LS) &&
              !Conformance->hasTypeWitness(ATD)) {
            Consumer.foundDecl(ATD, ReasonForThisProtocol);
          }
          continue;
        }
        if (auto *VD = dyn_cast<ValueDecl>(Member)) {
          if (!VD->isProtocolRequirement())
            continue;

          // Skip value requirements that have corresponding witnesses. This
          // cuts down on duplicates.
          auto witness = NormalConformance->getWitness(VD);
          if (witness && witness.getDecl()->getFullName() == VD->getFullName())
            continue;

          Consumer.foundDecl(VD, ReasonForThisProtocol);
        }
      }
    }

    // Add members from any extensions.
    SmallVector<ValueDecl *, 2> FoundDecls;
    doGlobalExtensionLookup(BaseTy, Proto->getDeclaredType(), FoundDecls,
                            FromContext, LS, ReasonForThisProtocol);
    for (auto *VD : FoundDecls)
      Consumer.foundDecl(VD, ReasonForThisProtocol);
  }
}

static void
lookupVisibleMemberDeclsImpl(Type BaseTy, VisibleDeclConsumer &Consumer,
                             const DeclContext *CurrDC, LookupState LS,
                             DeclVisibilityKind Reason,
                             GenericSignatureBuilder *GSB,
                             VisitedSet &Visited);

static void
  lookupVisibleProtocolMemberDecls(Type BaseTy, ProtocolType *PT,
                                   VisibleDeclConsumer &Consumer,
                                   const DeclContext *CurrDC, LookupState LS,
                                   DeclVisibilityKind Reason,
                                   GenericSignatureBuilder *GSB,
                                   VisitedSet &Visited) {
  if (!Visited.insert(PT->getDecl()).second)
    return;

  for (auto Proto : PT->getDecl()->getInheritedProtocols())
    lookupVisibleProtocolMemberDecls(BaseTy, Proto->getDeclaredType(), Consumer, CurrDC,
                                     LS, getReasonForSuper(Reason),
                                     GSB, Visited);
  lookupTypeMembers(BaseTy, PT, Consumer, CurrDC, LS, Reason);
}

// Generate '$' and '_' prefixed variables that have attached property
// wrappers.
static void
synthesizePropertyWrapperStorageWrapperProperties(IterableDeclContext *IDC) {
  auto SF = IDC->getDecl()->getDeclContext()->getParentSourceFile();
  if (!SF || SF->Kind == SourceFileKind::Interface)
    return;

  for (auto Member : IDC->getMembers())
    if (auto var = dyn_cast<VarDecl>(Member))
      if (var->hasAttachedPropertyWrapper())
        (void)var->getPropertyWrapperBackingPropertyInfo();
}

/// Trigger synthesizing implicit member declarations to make them "visible".
static void synthesizeMemberDeclsForLookup(NominalTypeDecl *NTD,
                                           const DeclContext *DC) {
  // Synthesize the memberwise initializer for structs or default initializer
  // for classes.
  if (!NTD->getASTContext().evaluator.hasActiveRequest(
          SynthesizeMemberwiseInitRequest{NTD}))
    TypeChecker::addImplicitConstructors(NTD);

  // Check all conformances to trigger the synthesized decl generation.
  // e.g. init(rawValue:) for RawRepresentable.
  for (auto Conformance : NTD->getAllConformances()) {
    auto Proto = Conformance->getProtocol();
    if (!Proto->isAccessibleFrom(DC))
      continue;
    auto NormalConformance = dyn_cast<NormalProtocolConformance>(
        Conformance->getRootConformance());
    if (!NormalConformance)
      continue;
    NormalConformance->forEachTypeWitness(
        [](AssociatedTypeDecl *, Type, TypeDecl *) { return false; },
        /*useResolver=*/true);
    NormalConformance->forEachValueWitness([](ValueDecl *, Witness) {},
                                           /*useResolver=*/true);
  }

  synthesizePropertyWrapperStorageWrapperProperties(NTD);
}

static void lookupVisibleMemberDeclsImpl(
    Type BaseTy, VisibleDeclConsumer &Consumer, const DeclContext *CurrDC,
    LookupState LS, DeclVisibilityKind Reason, GenericSignatureBuilder *GSB,
    VisitedSet &Visited) {
  // Just look through l-valueness.  It doesn't affect name lookup.
  assert(BaseTy && "lookup into null type");
  assert(!BaseTy->hasLValueType());

  // Handle metatype references, as in "some_type.some_member".  These are
  // special and can't have extensions.
  if (auto MTT = BaseTy->getAs<AnyMetatypeType>()) {
    // The metatype represents an arbitrary named type: dig through to the
    // declared type to see what we're dealing with.
    Type Ty = MTT->getInstanceType();
    if (Ty->is<AnyMetatypeType>())
      return;

    LookupState subLS = LookupState::makeQualified().withOnMetatype();
    if (LS.isIncludingInstanceMembers()) {
      subLS = subLS.withIncludedInstanceMembers();
    }

    // Just perform normal dot lookup on the type see if we find extensions or
    // anything else.  For example, type SomeTy.SomeMember can look up static
    // functions, and can even look up non-static functions as well (thus
    // getting the address of the member).
    lookupVisibleMemberDeclsImpl(Ty, Consumer, CurrDC, subLS, Reason,
                                 GSB, Visited);
    return;
  }

  // Lookup module references, as on some_module.some_member.  These are
  // special and can't have extensions.
  if (ModuleType *MT = BaseTy->getAs<ModuleType>()) {
    AccessFilteringDeclConsumer FilteringConsumer(CurrDC, Consumer);
    MT->getModule()->lookupVisibleDecls(ModuleDecl::AccessPathTy(),
                                        FilteringConsumer,
                                        NLKind::QualifiedLookup);
    return;
  }

  // If the base is AnyObject, we are doing dynamic lookup.
  if (BaseTy->isAnyObject()) {
    doDynamicLookup(Consumer, CurrDC, LS);
    return;
  }

  // If the base is a protocol, enumerate its members.
  if (ProtocolType *PT = BaseTy->getAs<ProtocolType>()) {
    lookupVisibleProtocolMemberDecls(BaseTy, PT, Consumer, CurrDC, LS, Reason,
                                     GSB, Visited);
    return;
  }

  // If the base is a protocol composition, enumerate members of the protocols.
  if (auto PC = BaseTy->getAs<ProtocolCompositionType>()) {
    for (auto Member : PC->getMembers())
      lookupVisibleMemberDeclsImpl(Member, Consumer, CurrDC, LS, Reason,
                                   GSB, Visited);
    return;
  }

  // Enumerate members of archetype's requirements.
  if (ArchetypeType *Archetype = BaseTy->getAs<ArchetypeType>()) {
    for (auto Proto : Archetype->getConformsTo())
      lookupVisibleProtocolMemberDecls(
          BaseTy, Proto->getDeclaredType(), Consumer, CurrDC, LS,
          Reason, GSB, Visited);

    if (auto superclass = Archetype->getSuperclass())
      lookupVisibleMemberDeclsImpl(superclass, Consumer, CurrDC, LS,
                                   Reason, GSB, Visited);
    return;
  }

  // If we're looking into a type parameter and we have a generic signature
  // builder, use the GSB to resolve where we should look.
  if (BaseTy->isTypeParameter() && GSB) {
    auto EquivClass =
      GSB->resolveEquivalenceClass(BaseTy,
                                   ArchetypeResolutionKind::CompleteWellFormed);
    if (!EquivClass) return;

    if (EquivClass->concreteType) {
      BaseTy = EquivClass->concreteType;
    } else {
      // Conformances
      for (const auto &Conforms : EquivClass->conformsTo) {
        lookupVisibleProtocolMemberDecls(
            BaseTy, Conforms.first->getDeclaredType(), Consumer, CurrDC,
            LS, getReasonForSuper(Reason), GSB, Visited);
      }

      // Superclass.
      if (EquivClass->superclass) {
        lookupVisibleMemberDeclsImpl(EquivClass->superclass, Consumer, CurrDC,
                                     LS, getReasonForSuper(Reason),
                                     GSB, Visited);
      }
      return;
    }
  }

  llvm::SmallPtrSet<ClassDecl *, 8> Ancestors;
  do {
    NominalTypeDecl *CurNominal = BaseTy->getAnyNominal();
    if (!CurNominal)
      break;

    synthesizeMemberDeclsForLookup(CurNominal, CurrDC);

    // Look in for members of a nominal type.
    lookupTypeMembers(BaseTy, BaseTy, Consumer, CurrDC, LS, Reason);
    lookupDeclsFromProtocolsBeingConformedTo(BaseTy, Consumer, LS, CurrDC,
                                             Reason, Visited);
    // If we have a class type, look into its superclass.
    auto *CurClass = dyn_cast<ClassDecl>(CurNominal);

    // FIXME: We check `getSuperclass()` here because we'll be using the
    // superclass Type below, and in ill-formed code `hasSuperclass()` could
    // be true while `getSuperclass()` returns null, because the latter
    // looks for a declaration.
    if (CurClass && CurClass->getSuperclass()) {
      // FIXME: This path is no substitute for an actual circularity check.
      // The real fix is to check that the superclass doesn't introduce a
      // circular reference before it's written into the AST.
      if (Ancestors.count(CurClass)) {
        break;
      }

      BaseTy = CurClass->getSuperclass();
      Reason = getReasonForSuper(Reason);

      bool InheritsSuperclassInitializers =
          CurClass->inheritsSuperclassInitializers();
      if (LS.isOnSuperclass() && !InheritsSuperclassInitializers)
        LS = LS.withoutInheritsSuperclassInitializers();
      else if (!LS.isOnSuperclass()) {
        LS = LS.withOnSuperclass();
        if (InheritsSuperclassInitializers)
          LS = LS.withInheritsSuperclassInitializers();
      }
    } else {
      break;
    }
    Ancestors.insert(CurClass);
  } while (1);
}

swift::DynamicLookupInfo::DynamicLookupInfo(
    SubscriptDecl *subscript, Type baseType,
    DeclVisibilityKind originalVisibility)
    : kind(KeyPathDynamicMember) {
  keypath.subscript = subscript;
  keypath.baseType = baseType;
  keypath.originalVisibility = originalVisibility;
}

const DynamicLookupInfo::KeyPathDynamicMemberInfo &
swift::DynamicLookupInfo::getKeyPathDynamicMember() const {
  assert(kind == KeyPathDynamicMember);
  return keypath;
}

namespace {

struct FoundDeclTy {
  ValueDecl *D;
  DeclVisibilityKind Reason;
  DynamicLookupInfo dynamicLookupInfo;

  FoundDeclTy(ValueDecl *D, DeclVisibilityKind Reason,
              DynamicLookupInfo dynamicLookupInfo)
      : D(D), Reason(Reason), dynamicLookupInfo(dynamicLookupInfo) {}

  friend bool operator==(const FoundDeclTy &LHS, const FoundDeclTy &RHS) {
    // If this ever changes - e.g. to include Reason - be sure to also update
    // DenseMapInfo<FoundDeclTy>::getHashValue().
    return LHS.D == RHS.D;
  }
};

} // end anonymous namespace

namespace llvm {

template <> struct DenseMapInfo<FoundDeclTy> {
  static inline FoundDeclTy getEmptyKey() {
    return FoundDeclTy{nullptr, DeclVisibilityKind::LocalVariable, {}};
  }

  static inline FoundDeclTy getTombstoneKey() {
    return FoundDeclTy{reinterpret_cast<ValueDecl *>(0x1),
                       DeclVisibilityKind::LocalVariable,
                       {}};
  }

  static unsigned getHashValue(const FoundDeclTy &Val) {
    // Note: FoundDeclTy::operator== only considers D, so don't hash Reason here.
    return llvm::hash_value(Val.D);
  }

  static bool isEqual(const FoundDeclTy &LHS, const FoundDeclTy &RHS) {
    return LHS == RHS;
  }
};

} // namespace llvm

// If a class 'Base' conforms to 'Proto', and my base type is a subclass
// 'Derived' of 'Base', use 'Base' not 'Derived' as the 'Self' type in the
// substitution map.
static Type getBaseTypeForMember(ModuleDecl *M, ValueDecl *OtherVD, Type BaseTy) {
  if (auto *Proto = OtherVD->getDeclContext()->getSelfProtocolDecl()) {
    if (BaseTy->getClassOrBoundGenericClass()) {
      if (auto Conformance = M->lookupConformance(BaseTy, Proto)) {
        auto *Superclass = Conformance.getConcrete()
                               ->getRootConformance()
                               ->getType()
                               ->getClassOrBoundGenericClass();
        return BaseTy->getSuperclassForDecl(Superclass);
      }
    }
  }

  return BaseTy;
}

namespace {

class OverrideFilteringConsumer : public VisibleDeclConsumer {
public:
  llvm::SetVector<FoundDeclTy> Results;
  llvm::SmallVector<ValueDecl *, 8> Decls;
  llvm::SetVector<FoundDeclTy> FilteredResults;
  llvm::DenseMap<DeclBaseName, llvm::SmallVector<ValueDecl *, 2>> DeclsByName;
  Type BaseTy;
  const DeclContext *DC;

  OverrideFilteringConsumer(Type BaseTy, const DeclContext *DC)
      : BaseTy(BaseTy->getMetatypeInstanceType()),
        DC(DC) {
    assert(!BaseTy->hasLValueType());
    assert(DC && BaseTy);
  }

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason,
                 DynamicLookupInfo dynamicLookupInfo) override {
    if (!Results.insert({VD, Reason, dynamicLookupInfo}))
      return;

    DeclsByName[VD->getBaseName()] = {};
    Decls.push_back(VD);
  }

  void filterDecls(VisibleDeclConsumer &Consumer) {
    removeOverriddenDecls(Decls);
    removeShadowedDecls(Decls, DC);

    size_t index = 0;
    for (auto DeclAndReason : Results) {
      if (index >= Decls.size())
        break;
      if (DeclAndReason.D != Decls[index])
        continue;

      index++;

      auto *VD = DeclAndReason.D;
      auto Reason = DeclAndReason.Reason;
      auto dynamicLookupInfo = DeclAndReason.dynamicLookupInfo;

      // If this kind of declaration doesn't participate in overriding, there's
      // no filtering to do here.
      if (!isa<AbstractFunctionDecl>(VD) &&
          !isa<AbstractStorageDecl>(VD) &&
          !isa<AssociatedTypeDecl>(VD)) {
        FilteredResults.insert(FoundDeclTy(VD, Reason, dynamicLookupInfo));
        continue;
      }

      if (VD->isRecursiveValidation())
        continue;

      auto &PossiblyConflicting = DeclsByName[VD->getBaseName()];

      if (VD->isInvalid()) {
        FilteredResults.insert(FoundDeclTy(VD, Reason, dynamicLookupInfo));
        PossiblyConflicting.push_back(VD);
        continue;
      }

      // Does it make sense to substitute types?

      // If the base type is AnyObject, we might be doing a dynamic
      // lookup, so the base type won't match the type of the member's
      // context type.
      //
      // If the base type is not a nominal type, we can't substitute
      // the member type.
      //
      // If the member is a free function and not a member of a type,
      // don't substitute either.
      bool shouldSubst = (Reason != DeclVisibilityKind::DynamicLookup &&
                          !BaseTy->isAnyObject() && !BaseTy->hasTypeVariable() &&
                          !BaseTy->hasUnboundGenericType() &&
                          (BaseTy->getNominalOrBoundGenericNominal() ||
                           BaseTy->is<ArchetypeType>()) &&
                          VD->getDeclContext()->isTypeContext());
      ModuleDecl *M = DC->getParentModule();

      auto FoundSignature = VD->getOverloadSignature();
      auto FoundSignatureType = VD->getOverloadSignatureType();
      if (FoundSignatureType && shouldSubst) {
        auto subs = BaseTy->getMemberSubstitutionMap(M, VD);
        auto CT = FoundSignatureType.subst(subs);
        if (!CT->hasError())
          FoundSignatureType = CT->getCanonicalType();
      }

      bool FoundConflicting = false;
      for (auto I = PossiblyConflicting.begin(), E = PossiblyConflicting.end();
           I != E; ++I) {
        auto *OtherVD = *I;
        if (OtherVD->isRecursiveValidation())
          continue;

        if (OtherVD->isInvalid())
          continue;

        auto OtherSignature = OtherVD->getOverloadSignature();
        auto OtherSignatureType = OtherVD->getOverloadSignatureType();
        if (OtherSignatureType && shouldSubst) {
          auto ActualBaseTy = getBaseTypeForMember(M, OtherVD, BaseTy);
          auto subs = ActualBaseTy->getMemberSubstitutionMap(M, OtherVD);
          auto CT = OtherSignatureType.subst(subs);
          if (!CT->hasError())
            OtherSignatureType = CT->getCanonicalType();
        }

        if (conflicting(M->getASTContext(), FoundSignature, FoundSignatureType,
                        OtherSignature, OtherSignatureType,
                        /*wouldConflictInSwift5*/nullptr,
                        /*skipProtocolExtensionCheck*/true)) {
          FoundConflicting = true;
          if (VD->getFormalAccess() > OtherVD->getFormalAccess() ||
              //Prefer available one.
              (!AvailableAttr::isUnavailable(VD) &&
               AvailableAttr::isUnavailable(OtherVD))) {
            FilteredResults.remove(
                FoundDeclTy(OtherVD, DeclVisibilityKind::LocalVariable, {}));
            FilteredResults.insert(FoundDeclTy(VD, Reason, dynamicLookupInfo));
            *I = VD;
          }
        }
      }

      if (!FoundConflicting) {
        FilteredResults.insert(FoundDeclTy(VD, Reason, dynamicLookupInfo));
        PossiblyConflicting.push_back(VD);
      }
    }

    for (auto Result : FilteredResults)
      Consumer.foundDecl(Result.D, Result.Reason, Result.dynamicLookupInfo);
  }

  bool seenBaseName(DeclBaseName name) {
    return DeclsByName.find(name) != DeclsByName.end();
  }
};

struct KeyPathDynamicMemberConsumer : public VisibleDeclConsumer {
  VisibleDeclConsumer &consumer;
  std::function<bool(DeclBaseName)> seenStaticBaseName;
  llvm::DenseSet<DeclBaseName> seen;

  SubscriptDecl *currentSubscript = nullptr;
  Type currentBaseType = Type();

  KeyPathDynamicMemberConsumer(VisibleDeclConsumer &consumer,
                               std::function<bool(DeclBaseName)> seenBaseName)
      : consumer(consumer), seenStaticBaseName(std::move(seenBaseName)) {}

  bool checkShadowed(ValueDecl *VD) {
    // Dynamic lookup members are only visible if they are not shadowed by
    // other members.
    return !isa<SubscriptDecl>(VD) && seen.insert(VD->getBaseName()).second &&
           !seenStaticBaseName(VD->getBaseName());
  }

  void foundDecl(ValueDecl *VD, DeclVisibilityKind reason,
                 DynamicLookupInfo dynamicLookupInfo) override {
    assert(dynamicLookupInfo.getKind() !=
           DynamicLookupInfo::KeyPathDynamicMember);

    // Only variables and subscripts are allowed in a keypath.
    if (!isa<AbstractStorageDecl>(VD))
      return;

    // Dynamic lookup members are only visible if they are not shadowed by
    // non-dynamic members.
    if (checkShadowed(VD))
      consumer.foundDecl(VD, DeclVisibilityKind::DynamicLookup,
                         {currentSubscript, currentBaseType, reason});
  }

  struct SubscriptChange {
    KeyPathDynamicMemberConsumer &consumer;
    SubscriptDecl *oldSubscript;
    Type oldBaseType;

    SubscriptChange(KeyPathDynamicMemberConsumer &consumer,
                    SubscriptDecl *newSubscript, Type newBaseType)
        : consumer(consumer), oldSubscript(newSubscript),
          oldBaseType(newBaseType) {
      std::swap(consumer.currentSubscript, oldSubscript);
      std::swap(consumer.currentBaseType, oldBaseType);
    }
    ~SubscriptChange() {
      consumer.currentSubscript = oldSubscript;
      consumer.currentBaseType = oldBaseType;
    }
  };
};
} // end anonymous namespace

static void lookupVisibleDynamicMemberLookupDecls(
    Type baseType, KeyPathDynamicMemberConsumer &consumer,
    const DeclContext *dc, LookupState LS, DeclVisibilityKind reason,
    GenericSignatureBuilder *GSB, VisitedSet &visited,
    llvm::DenseSet<TypeBase *> &seenDynamicLookup);

/// Enumerates all members of \c baseType, including both directly visible and
/// members visible by keypath dynamic member lookup.
///
/// \note This is an implementation detail of \c lookupVisibleMemberDecls and
/// exists to create the correct recursion for dynamic member lookup.
static void lookupVisibleMemberAndDynamicMemberDecls(
    Type baseType, VisibleDeclConsumer &consumer,
    KeyPathDynamicMemberConsumer &dynamicMemberConsumer, const DeclContext *DC,
    LookupState LS, DeclVisibilityKind reason, GenericSignatureBuilder *GSB,
    VisitedSet &visited, llvm::DenseSet<TypeBase *> &seenDynamicLookup) {
  lookupVisibleMemberDeclsImpl(baseType, consumer, DC, LS, reason, GSB, visited);
  lookupVisibleDynamicMemberLookupDecls(baseType, dynamicMemberConsumer, DC, LS,
                                        reason, GSB, visited, seenDynamicLookup);
}

/// Enumerates all keypath dynamic members of \c baseType, as seen from the
/// context \c dc.
///
/// If \c baseType is \c \@dynamicMemberLookup, this looks up any keypath
/// dynamic member subscripts and looks up the members of the keypath's root
/// type.
static void lookupVisibleDynamicMemberLookupDecls(
    Type baseType, KeyPathDynamicMemberConsumer &consumer,
    const DeclContext *dc, LookupState LS, DeclVisibilityKind reason,
    GenericSignatureBuilder *GSB, VisitedSet &visited,
    llvm::DenseSet<TypeBase *> &seenDynamicLookup) {
  if (!seenDynamicLookup.insert(baseType.getPointer()).second)
    return;

  if (!evaluateOrDefault(dc->getASTContext().evaluator,
         HasDynamicMemberLookupAttributeRequest{baseType.getPointer()}, false))
    return;

  auto &ctx = dc->getASTContext();

  // Lookup the `subscript(dynamicMember:)` methods in this type.
  auto subscriptName =
      DeclName(ctx, DeclBaseName::createSubscript(), ctx.Id_dynamicMember);

  SmallVector<ValueDecl *, 2> subscripts;
  dc->lookupQualified(baseType, subscriptName, NL_QualifiedDefault,
                      subscripts);

  for (ValueDecl *VD : subscripts) {
    auto *subscript = dyn_cast<SubscriptDecl>(VD);
    if (!subscript)
      continue;

    auto rootType = evaluateOrDefault(subscript->getASTContext().evaluator,
      RootTypeOfKeypathDynamicMemberRequest{subscript}, Type());
    if (rootType.isNull())
      continue;

    auto subs =
        baseType->getMemberSubstitutionMap(dc->getParentModule(), subscript);
    auto memberType = rootType.subst(subs);
    if (!memberType->mayHaveMembers())
      continue;

    KeyPathDynamicMemberConsumer::SubscriptChange sub(consumer, subscript,
                                                      baseType);

    lookupVisibleMemberAndDynamicMemberDecls(memberType, consumer, consumer, dc,
                                             LS, reason, GSB, visited,
                                             seenDynamicLookup);
  }
}

/// Enumerate all members in \c BaseTy (including members of extensions,
/// superclasses and implemented protocols), as seen from the context \c CurrDC.
///
/// This operation corresponds to a standard "dot" lookup operation like "a.b"
/// where 'self' is the type of 'a'.  This operation is only valid after name
/// binding.
static void lookupVisibleMemberDecls(
    Type BaseTy, VisibleDeclConsumer &Consumer, const DeclContext *CurrDC,
    LookupState LS, DeclVisibilityKind Reason, GenericSignatureBuilder *GSB) {
  OverrideFilteringConsumer overrideConsumer(BaseTy, CurrDC);
  KeyPathDynamicMemberConsumer dynamicConsumer(
      Consumer,
      [&](DeclBaseName name) { return overrideConsumer.seenBaseName(name); });

  VisitedSet Visited;
  llvm::DenseSet<TypeBase *> seenDynamicLookup;
  lookupVisibleMemberAndDynamicMemberDecls(
      BaseTy, overrideConsumer, dynamicConsumer, CurrDC, LS, Reason,
      GSB, Visited, seenDynamicLookup);

  // Report the declarations we found to the real consumer.
  overrideConsumer.filterDecls(Consumer);
}

static void lookupVisibleDeclsImpl(VisibleDeclConsumer &Consumer,
                                   const DeclContext *DC,
                                   bool IncludeTopLevel, SourceLoc Loc) {
  const SourceManager &SM = DC->getASTContext().SourceMgr;
  auto Reason = DeclVisibilityKind::MemberOfCurrentNominal;

  // If we are inside of a method, check to see if there are any ivars in scope,
  // and if so, whether this is a reference to one of them.
  while (!DC->isModuleScopeContext()) {
    GenericParamList *GenericParams = nullptr;
    Type ExtendedType;
    auto LS = LookupState::makeUnqualified();

    // Skip initializer contexts, we will not find any declarations there.
    if (isa<Initializer>(DC)) {
      DC = DC->getParent();
      LS = LS.withOnMetatype();
    }

    // We don't look for generic parameters if we are in the context of a
    // nominal type: they will be looked up anyways via `lookupVisibleMemberDecls`.
    if (DC && !isa<NominalTypeDecl>(DC)) {
      if (auto *decl = DC->getAsDecl()) {
        if (auto GC = decl->getAsGenericContext()) {
          auto params = GC->getGenericParams();
          namelookup::FindLocalVal(SM, Loc, Consumer).checkGenericParams(params);
        }
      }
    }

    if (auto *SE = dyn_cast<SubscriptDecl>(DC)) {
      ExtendedType = SE->getDeclContext()->getSelfTypeInContext();
      DC = DC->getParent();
    } else if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DC)) {

      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      // FIXME: when we can parse and typecheck the function body partially for
      // code completion, AFD->getBody() check can be removed.
      if (Loc.isValid() &&
          AFD->getSourceRange().isValid() &&
          SM.rangeContainsTokenLoc(AFD->getSourceRange(), Loc) &&
          AFD->getBody()) {
        namelookup::FindLocalVal(SM, Loc, Consumer).visit(AFD->getBody());
      }

      if (auto *P = AFD->getImplicitSelfDecl()) {
        namelookup::FindLocalVal(SM, Loc, Consumer).checkValueDecl(
          const_cast<ParamDecl *>(P), DeclVisibilityKind::FunctionParameter);
      }

      namelookup::FindLocalVal(SM, Loc, Consumer).checkParameterList(
        AFD->getParameters());

      GenericParams = AFD->getGenericParams();

      if (AFD->getDeclContext()->isTypeContext()) {
        ExtendedType = AFD->getDeclContext()->getSelfTypeInContext();
        DC = DC->getParent();

        if (auto *FD = dyn_cast<FuncDecl>(AFD))
          if (FD->isStatic())
            ExtendedType = MetatypeType::get(ExtendedType);
      }
    } else if (auto CE = dyn_cast<ClosureExpr>(DC)) {
      if (Loc.isValid()) {
        namelookup::FindLocalVal(SM, Loc, Consumer).visit(CE->getBody());
        if (auto P = CE->getParameters()) {
          namelookup::FindLocalVal(SM, Loc, Consumer).checkParameterList(P);
        }
      }
    } else if (auto ED = dyn_cast<ExtensionDecl>(DC)) {
      ExtendedType = ED->getSelfTypeInContext();
    } else if (auto ND = dyn_cast<NominalTypeDecl>(DC)) {
      ExtendedType = ND->getSelfTypeInContext();
    }

    // If we're inside a function context, we've already moved to
    // the parent DC, so we have to check the function's generic
    // parameters first.
    if (GenericParams) {
      namelookup::FindLocalVal localVal(SM, Loc, Consumer);
      localVal.checkGenericParams(GenericParams);
    }

    // Check the generic parameters of our context.
    GenericParamList *dcGenericParams = nullptr;
    if (auto nominal = dyn_cast<NominalTypeDecl>(DC))
      dcGenericParams = nominal->getGenericParams();
    else if (auto ext = dyn_cast<ExtensionDecl>(DC))
      dcGenericParams = ext->getGenericParams();
    else if (auto subscript = dyn_cast<SubscriptDecl>(DC))
      dcGenericParams = subscript->getGenericParams();

    while (dcGenericParams) {
      namelookup::FindLocalVal localVal(SM, Loc, Consumer);
      localVal.checkGenericParams(dcGenericParams);
      dcGenericParams = dcGenericParams->getOuterParameters();
    }

    if (ExtendedType)
      ::lookupVisibleMemberDecls(ExtendedType, Consumer, DC, LS, Reason,
                                 nullptr);

    DC = DC->getParent();
    Reason = DeclVisibilityKind::MemberOfOutsideNominal;
  }

  if (auto SF = dyn_cast<SourceFile>(DC)) {
    if (Loc.isValid()) {
      // Look for local variables in top-level code; normally, the parser
      // resolves these for us, but it can't do the right thing for
      // local types.
      namelookup::FindLocalVal(SM, Loc, Consumer).checkSourceFile(*SF);
    }

    if (IncludeTopLevel) {
      auto &cached = SF->getCachedVisibleDecls();
      if (!cached.empty()) {
        for (auto result : cached)
          Consumer.foundDecl(result, DeclVisibilityKind::VisibleAtTopLevel);
        return;
      }
    }
  }

  if (IncludeTopLevel) {
    using namespace namelookup;
    SmallVector<ValueDecl *, 0> moduleResults;
    lookupVisibleDeclsInModule(DC, {}, moduleResults,
                               NLKind::UnqualifiedLookup,
                               ResolutionKind::Overloadable,
                               DC);
    for (auto result : moduleResults)
      Consumer.foundDecl(result, DeclVisibilityKind::VisibleAtTopLevel);

    if (auto SF = dyn_cast<SourceFile>(DC))
      SF->cacheVisibleDecls(std::move(moduleResults));
  }
}

void swift::lookupVisibleDecls(VisibleDeclConsumer &Consumer,
                               const DeclContext *DC,
                               bool IncludeTopLevel,
                               SourceLoc Loc) {
  if (Loc.isInvalid()) {
    lookupVisibleDeclsImpl(Consumer, DC, IncludeTopLevel, Loc);
    return;
  }

  // Filtering out unusable values.
  class LocalConsumer : public VisibleDeclConsumer {
    const SourceManager &SM;
    SourceLoc Loc;
    VisibleDeclConsumer &Consumer;

    bool isUsableValue(ValueDecl *VD, DeclVisibilityKind Reason) {

      // Check "use within its own initial value" case.
      if (auto *varD = dyn_cast<VarDecl>(VD)) {
        if (auto *initExpr = varD->getParentInitializer())
          if (SM.rangeContainsTokenLoc(initExpr->getSourceRange(), Loc))
            return false;
      }

      switch (Reason) {
      case DeclVisibilityKind::LocalVariable:
        // Use of 'TypeDecl's before declaration is allowed.
        if (isa<TypeDecl>(VD))
          return true;

        return SM.isBeforeInBuffer(VD->getLoc(), Loc);

      case DeclVisibilityKind::VisibleAtTopLevel:
        // TODO: Implement forward reference rule for script mode? Currently,
        // it's not needed because the rest of the file hasn't been parsed.
        // See: https://bugs.swift.org/browse/SR-284 for the rule.
        return true;

      default:
        // Other visibility kind are always usable.
        return true;
      }
    }

  public:
    LocalConsumer(const SourceManager &SM, SourceLoc Loc,
                  VisibleDeclConsumer &Consumer)
        : SM(SM), Loc(Loc), Consumer(Consumer) {}

    void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason,
                   DynamicLookupInfo dynamicLookupInfo) override {
      if (isUsableValue(VD, Reason))
        Consumer.foundDecl(VD, Reason, dynamicLookupInfo);
    }
  } LocalConsumer(DC->getASTContext().SourceMgr, Loc, Consumer);

  lookupVisibleDeclsImpl(LocalConsumer, DC, IncludeTopLevel, Loc);
}

void swift::lookupVisibleMemberDecls(VisibleDeclConsumer &Consumer, Type BaseTy,
                                     const DeclContext *CurrDC,
                                     bool includeInstanceMembers,
                                     GenericSignatureBuilder *GSB) {
  assert(CurrDC);
  LookupState ls = LookupState::makeQualified();
  if (includeInstanceMembers) {
    ls = ls.withIncludedInstanceMembers();
  }

  ::lookupVisibleMemberDecls(BaseTy, Consumer, CurrDC, ls,
                             DeclVisibilityKind::MemberOfCurrentNominal,
                             GSB);
}
