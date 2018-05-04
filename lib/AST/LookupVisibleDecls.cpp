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

#include "NameLookupImpl.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Sema/IDETypeChecking.h"
#include "llvm/ADT/SetVector.h"
#include <set>

using namespace swift;

void VisibleDeclConsumer::anchor() {}
void VectorDeclConsumer::anchor() {}
void NamedDeclConsumer::anchor() {}

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
} // unnamed namespace

static bool areTypeDeclsVisibleInLookupMode(LookupState LS) {
  // Nested type declarations can be accessed only with unqualified lookup or
  // on metatypes.
  return !LS.isQualified() || LS.isOnMetatype();
}

static bool isDeclVisibleInLookupMode(ValueDecl *Member, LookupState LS,
                                      const DeclContext *FromContext,
                                      LazyResolver *TypeResolver) {
  // Accessors are never visible directly in the source language.
  if (isa<AccessorDecl>(Member))
    return false;

  if (TypeResolver) {
    TypeResolver->resolveDeclSignature(Member);
    TypeResolver->resolveAccessControl(Member);
  }

  // Check access when relevant.
  if (!Member->getDeclContext()->isLocalContext() &&
      !isa<GenericTypeParamDecl>(Member) && !isa<ParamDecl>(Member) &&
      FromContext->getASTContext().LangOpts.EnableAccessControl) {
    if (Member->isInvalid() && !Member->hasAccess())
      return false;
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

/// Lookup members in extensions of \p LookupType, using \p BaseType as the
/// underlying type when checking any constraints on the extensions.
static void doGlobalExtensionLookup(Type BaseType,
                                    Type LookupType,
                                    SmallVectorImpl<ValueDecl *> &FoundDecls,
                                    const DeclContext *CurrDC,
                                    LookupState LS,
                                    DeclVisibilityKind Reason,
                                    LazyResolver *TypeResolver) {
  auto nominal = LookupType->getAnyNominal();

  // Look in each extension of this type.
  for (auto extension : nominal->getExtensions()) {
    if (!isExtensionApplied(*const_cast<DeclContext*>(CurrDC), BaseType,
                            extension))
      continue;

    bool validatedExtension = false;
    for (auto Member : extension->getMembers()) {
      if (auto VD = dyn_cast<ValueDecl>(Member))
        if (isDeclVisibleInLookupMode(VD, LS, CurrDC, TypeResolver)) {
          // Resolve the extension, if we haven't done so already.
          if (!validatedExtension && TypeResolver) {
            TypeResolver->resolveExtension(extension);
            validatedExtension = true;
          }

          FoundDecls.push_back(VD);
        }
    }
  }

  // Handle shadowing.
  removeShadowedDecls(FoundDecls, CurrDC->getParentModule(), TypeResolver);
}

/// \brief Enumerate immediate members of the type \c LookupType and its
/// extensions, as seen from the context \c CurrDC.
///
/// Don't do lookup into superclasses or implemented protocols.  Uses
/// \p BaseType as the underlying type when checking any constraints on the
/// extensions.
static void lookupTypeMembers(Type BaseType, Type LookupType,
                              VisibleDeclConsumer &Consumer,
                              const DeclContext *CurrDC, LookupState LS,
                              DeclVisibilityKind Reason,
                              LazyResolver *TypeResolver) {
  NominalTypeDecl *D = LookupType->getAnyNominal();
  assert(D && "should have a nominal type");

  bool LookupFromChildDeclContext = false;
  const DeclContext *TempDC = CurrDC;
  while (!TempDC->isModuleContext()) {
    if (TempDC == D) {
      LookupFromChildDeclContext = true;
      break;
    }
    TempDC = TempDC->getParent();
  }

  SmallVector<ValueDecl*, 2> FoundDecls;

  if (LookupFromChildDeclContext) {
    // Current decl context is contained inside 'D', so generic parameters
    // are visible.
    if (D->getGenericParams())
      for (auto Param : *D->getGenericParams())
        if (isDeclVisibleInLookupMode(Param, LS, CurrDC, TypeResolver))
          FoundDecls.push_back(Param);
  }

  for (Decl *Member : D->getMembers()) {
    if (auto *VD = dyn_cast<ValueDecl>(Member))
      if (isDeclVisibleInLookupMode(VD, LS, CurrDC, TypeResolver))
        FoundDecls.push_back(VD);
  }
  doGlobalExtensionLookup(BaseType, LookupType, FoundDecls, CurrDC, LS, Reason,
                          TypeResolver);

  // Report the declarations we found to the consumer.
  for (auto *VD : FoundDecls)
    Consumer.foundDecl(VD, Reason);
}

/// Enumerate AnyObject declarations as seen from context \c CurrDC.
static void doDynamicLookup(VisibleDeclConsumer &Consumer,
                            const DeclContext *CurrDC,
                            LookupState LS,
                            LazyResolver *TypeResolver) {
  class DynamicLookupConsumer : public VisibleDeclConsumer {
    VisibleDeclConsumer &ChainedConsumer;
    LookupState LS;
    const DeclContext *CurrDC;
    LazyResolver *TypeResolver;
    llvm::DenseSet<std::pair<DeclBaseName, CanType>> FunctionsReported;
    llvm::DenseSet<CanType> SubscriptsReported;
    llvm::DenseSet<std::pair<Identifier, CanType>> PropertiesReported;

  public:
    explicit DynamicLookupConsumer(VisibleDeclConsumer &ChainedConsumer,
                                   LookupState LS, const DeclContext *CurrDC,
                                   LazyResolver *TypeResolver)
        : ChainedConsumer(ChainedConsumer), LS(LS), CurrDC(CurrDC),
          TypeResolver(TypeResolver) {}

    void foundDecl(ValueDecl *D, DeclVisibilityKind Reason) override {
      // If the declaration has an override, name lookup will also have found
      // the overridden method.  Skip this declaration, because we prefer the
      // overridden method.
      if (D->getOverriddenDecl())
        return;

      // If the declaration is not @objc, it cannot be called dynamically.
      if (!D->isObjC())
        return;

      // Ensure that the declaration has a type.
      if (!D->hasInterfaceType()) {
        if (!TypeResolver) return;
        TypeResolver->resolveDeclSignature(D);
        if (!D->hasInterfaceType()) return;
      }

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
        return;

      // Initializers cannot be found by dynamic lookup.
      case DeclKind::Constructor:
      case DeclKind::Destructor:
        return;

      // These cases are probably impossible here but can also just
      // be safely ignored.
      case DeclKind::EnumElement:
      case DeclKind::Param:
      case DeclKind::Module:
        return;

      // For other kinds of values, check if we already reported a decl
      // with the same signature.

      case DeclKind::Accessor:
      case DeclKind::Func: {
        auto FD = cast<FuncDecl>(D);
        assert(FD->getImplicitSelfDecl() && "should not find free functions");
        (void)FD;

        if (FD->isInvalid())
          break;

        // Get the type without the first uncurry level with 'self'.
        CanType T = D->getInterfaceType()
                        ->castTo<AnyFunctionType>()
                        ->getResult()
                        ->getCanonicalType();

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

      if (isDeclVisibleInLookupMode(D, LS, CurrDC, TypeResolver))
        ChainedConsumer.foundDecl(D, DeclVisibilityKind::DynamicLookup);
    }
  };

  DynamicLookupConsumer ConsumerWrapper(Consumer, LS, CurrDC, TypeResolver);

  CurrDC->getParentSourceFile()->forAllVisibleModules(
      [&](ModuleDecl::ImportedModule Import) {
        Import.second->lookupClassMembers(Import.first, ConsumerWrapper);
      });
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
    LazyResolver *TypeResolver, VisitedSet &Visited) {
  NominalTypeDecl *CurrNominal = BaseTy->getAnyNominal();
  if (!CurrNominal)
    return;

  for (auto Conformance : CurrNominal->getAllConformances()) {
    auto Proto = Conformance->getProtocol();
    if (!Proto->isAccessibleFrom(FromContext))
      continue;

    DeclVisibilityKind ReasonForThisProtocol;
    if (Reason == DeclVisibilityKind::MemberOfCurrentNominal)
      ReasonForThisProtocol =
          DeclVisibilityKind::MemberOfProtocolImplementedByCurrentNominal;
    else
      ReasonForThisProtocol = getReasonForSuper(Reason);

    auto NormalConformance = Conformance->getRootNormalConformance();
    for (auto Member : Proto->getMembers()) {
      if (auto *ATD = dyn_cast<AssociatedTypeDecl>(Member)) {
        // Skip type decls if they aren't visible, or any type that has a
        // witness. This cuts down on duplicates.
        if (areTypeDeclsVisibleInLookupMode(LS) &&
            !NormalConformance->hasTypeWitness(ATD)) {
          Consumer.foundDecl(ATD, ReasonForThisProtocol);
        }
        continue;
      }
      if (auto *VD = dyn_cast<ValueDecl>(Member)) {
        if (TypeResolver)
          TypeResolver->resolveDeclSignature(VD);

        // Skip value requirements that have corresponding witnesses. This cuts
        // down on duplicates.
        if (!NormalConformance->hasWitness(VD) ||
            !NormalConformance->getWitness(VD, nullptr) ||
            NormalConformance->getWitness(VD, nullptr).getDecl()->getFullName()
              != VD->getFullName()) {
          Consumer.foundDecl(VD, ReasonForThisProtocol);
        }
      }
    }

    // Add members from any extensions.
    SmallVector<ValueDecl *, 2> FoundDecls;
    doGlobalExtensionLookup(BaseTy, Proto->getDeclaredType(), FoundDecls,
                            FromContext, LS, ReasonForThisProtocol,
                            TypeResolver);
    for (auto *VD : FoundDecls)
      Consumer.foundDecl(VD, ReasonForThisProtocol);
  }
}

static void
lookupVisibleMemberDeclsImpl(Type BaseTy, VisibleDeclConsumer &Consumer,
                             const DeclContext *CurrDC, LookupState LS,
                             DeclVisibilityKind Reason,
                             LazyResolver *TypeResolver,
                             GenericSignatureBuilder *GSB,
                             VisitedSet &Visited);

static void lookupVisibleProtocolMemberDecls(
    Type BaseTy, ProtocolType *PT, VisibleDeclConsumer &Consumer,
    const DeclContext *CurrDC, LookupState LS, DeclVisibilityKind Reason,
                                             LazyResolver *TypeResolver, GenericSignatureBuilder *GSB,
    VisitedSet &Visited) {
  if (!Visited.insert(PT->getDecl()).second)
    return;

  for (auto Proto : PT->getDecl()->getInheritedProtocols())
    lookupVisibleProtocolMemberDecls(BaseTy, Proto->getDeclaredType(), Consumer, CurrDC,
                                 LS, getReasonForSuper(Reason), TypeResolver,
                                 GSB, Visited);

  lookupTypeMembers(BaseTy, PT, Consumer, CurrDC, LS, Reason, TypeResolver);
}

static void lookupVisibleMemberDeclsImpl(
    Type BaseTy, VisibleDeclConsumer &Consumer, const DeclContext *CurrDC,
    LookupState LS, DeclVisibilityKind Reason, LazyResolver *TypeResolver,
    GenericSignatureBuilder *GSB, VisitedSet &Visited) {
  // Just look through l-valueness.  It doesn't affect name lookup.
  assert(BaseTy && "lookup into null type");
  assert(!BaseTy->hasLValueType());

  // Handle metatype references, as in "some_type.some_member".  These are
  // special and can't have extensions.
  if (auto MTT = BaseTy->getAs<AnyMetatypeType>()) {
    // The metatype represents an arbitrary named type: dig through to the
    // declared type to see what we're dealing with.
    Type Ty = MTT->getInstanceType();

    LookupState subLS = LookupState::makeQualified().withOnMetatype();
    if (LS.isIncludingInstanceMembers()) {
      subLS = subLS.withIncludedInstanceMembers();
    }

    // Just perform normal dot lookup on the type see if we find extensions or
    // anything else.  For example, type SomeTy.SomeMember can look up static
    // functions, and can even look up non-static functions as well (thus
    // getting the address of the member).
    lookupVisibleMemberDeclsImpl(Ty, Consumer, CurrDC, subLS, Reason,
                                 TypeResolver, GSB, Visited);
    return;
  }

  // Lookup module references, as on some_module.some_member.  These are
  // special and can't have extensions.
  if (ModuleType *MT = BaseTy->getAs<ModuleType>()) {
    AccessFilteringDeclConsumer FilteringConsumer(CurrDC, Consumer,
                                                  TypeResolver);
    MT->getModule()->lookupVisibleDecls(ModuleDecl::AccessPathTy(),
                                        FilteringConsumer,
                                        NLKind::QualifiedLookup);
    return;
  }

  // If the base is AnyObject, we are doing dynamic lookup.
  if (BaseTy->isAnyObject()) {
    doDynamicLookup(Consumer, CurrDC, LS, TypeResolver);
    return;
  }

  // If the base is a protocol, enumerate its members.
  if (ProtocolType *PT = BaseTy->getAs<ProtocolType>()) {
    lookupVisibleProtocolMemberDecls(BaseTy, PT, Consumer, CurrDC, LS, Reason,
                                     TypeResolver, GSB, Visited);
    return;
  }

  // If the base is a protocol composition, enumerate members of the protocols.
  if (auto PC = BaseTy->getAs<ProtocolCompositionType>()) {
    for (auto Member : PC->getMembers())
      lookupVisibleMemberDeclsImpl(Member, Consumer, CurrDC, LS, Reason,
                                   TypeResolver, GSB, Visited);
    return;
  }

  // Enumerate members of archetype's requirements.
  if (ArchetypeType *Archetype = BaseTy->getAs<ArchetypeType>()) {
    for (auto Proto : Archetype->getConformsTo())
      lookupVisibleProtocolMemberDecls(
          BaseTy, Proto->getDeclaredType(), Consumer, CurrDC, LS,
          getReasonForSuper(Reason), TypeResolver, GSB, Visited);

    if (auto superclass = Archetype->getSuperclass())
      lookupVisibleMemberDeclsImpl(superclass, Consumer, CurrDC, LS,
                                   getReasonForSuper(Reason), TypeResolver,
                                   GSB, Visited);
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
            LS, getReasonForSuper(Reason), TypeResolver, GSB, Visited);
      }

      // Superclass.
      if (EquivClass->superclass) {
        lookupVisibleMemberDeclsImpl(EquivClass->superclass, Consumer, CurrDC,
                                     LS, getReasonForSuper(Reason),
                                     TypeResolver, GSB, Visited);
      }
      return;
    }
  }

  llvm::SmallPtrSet<ClassDecl *, 8> Ancestors;
  do {
    NominalTypeDecl *CurNominal = BaseTy->getAnyNominal();
    if (!CurNominal)
      break;

    // Look in for members of a nominal type.
    lookupTypeMembers(BaseTy, BaseTy, Consumer, CurrDC, LS, Reason,
                      TypeResolver);
    lookupDeclsFromProtocolsBeingConformedTo(BaseTy, Consumer, LS, CurrDC,
                                             Reason, TypeResolver, Visited);
    // If we have a class type, look into its superclass.
    auto *CurClass = dyn_cast<ClassDecl>(CurNominal);

    if (CurClass && CurClass->hasSuperclass()) {
      // FIXME: This path is no substitute for an actual circularity check.
      // The real fix is to check that the superclass doesn't introduce a
      // circular reference before it's written into the AST.
      if (Ancestors.count(CurClass)) {
        break;
      }

      BaseTy = CurClass->getSuperclass();
      Reason = getReasonForSuper(Reason);

      bool InheritsSuperclassInitializers =
          CurClass->inheritsSuperclassInitializers(TypeResolver);
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

namespace {

struct FoundDeclTy {
  ValueDecl *D;
  DeclVisibilityKind Reason;

  FoundDeclTy(ValueDecl *D, DeclVisibilityKind Reason)
      : D(D), Reason(Reason) {}

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
    return FoundDeclTy{nullptr, DeclVisibilityKind::LocalVariable};
  }

  static inline FoundDeclTy getTombstoneKey() {
    return FoundDeclTy{reinterpret_cast<ValueDecl *>(0x1),
                       DeclVisibilityKind::LocalVariable};
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

namespace {

/// Hack to guess at whether substituting into the type of a declaration will
/// be okay.
/// FIXME: This is awful. We should either have Type::subst() work for
/// GenericFunctionType, or we should kill it outright.
static bool shouldSubstIntoDeclType(Type type) {
  auto genericFnType = type->getAs<GenericFunctionType>();
  if (!genericFnType) return true;

  return false;
}

class OverrideFilteringConsumer : public VisibleDeclConsumer {
public:
  std::set<ValueDecl *> AllFoundDecls;
  std::map<DeclBaseName, std::set<ValueDecl *>> FoundDecls;
  llvm::SetVector<FoundDeclTy> DeclsToReport;
  Type BaseTy;
  const DeclContext *DC;
  LazyResolver *TypeResolver;
  bool IsTypeLookup = false;

  OverrideFilteringConsumer(Type BaseTy, const DeclContext *DC,
                            LazyResolver *resolver)
      : BaseTy(BaseTy), DC(DC), TypeResolver(resolver) {
    assert(!BaseTy->hasLValueType());
    if (auto *MetaTy = BaseTy->getAs<AnyMetatypeType>()) {
      BaseTy = MetaTy->getInstanceType();
      IsTypeLookup = true;
    }
    assert(DC && BaseTy);
  }

  void foundDecl(ValueDecl *VD, DeclVisibilityKind Reason) override {
    if (!AllFoundDecls.insert(VD).second)
      return;

    // If this kind of declaration doesn't participate in overriding, there's
    // no filtering to do here.
    if (!isa<AbstractFunctionDecl>(VD) && !isa<AbstractStorageDecl>(VD)) {
      DeclsToReport.insert(FoundDeclTy(VD, Reason));
      return;
    }

    if (TypeResolver) {
      TypeResolver->resolveDeclSignature(VD);
      TypeResolver->resolveAccessControl(VD);
    }

    if (VD->isInvalid()) {
      FoundDecls[VD->getBaseName()].insert(VD);
      DeclsToReport.insert(FoundDeclTy(VD, Reason));
      return;
    }
    auto &PossiblyConflicting = FoundDecls[VD->getBaseName()];

    // Check all overridden decls.
    {
      auto *CurrentVD = VD->getOverriddenDecl();
      while (CurrentVD) {
        if (!AllFoundDecls.insert(CurrentVD).second)
          break;
        if (PossiblyConflicting.count(CurrentVD)) {
          PossiblyConflicting.erase(CurrentVD);
          PossiblyConflicting.insert(VD);

          bool Erased = DeclsToReport.remove(
              FoundDeclTy(CurrentVD, DeclVisibilityKind::LocalVariable));
          assert(Erased);
          (void)Erased;

          DeclsToReport.insert(FoundDeclTy(VD, Reason));
          return;
        }
        CurrentVD = CurrentVD->getOverriddenDecl();
      }
    }

    // Does it make sense to substitute types?

    // Don't pass UnboundGenericType here. If you see this assertion
    // being hit, fix the caller, don't remove it.
    assert(IsTypeLookup || !BaseTy->hasUnboundGenericType());

    // If the base type is AnyObject, we might be doing a dynamic
    // lookup, so the base type won't match the type of the member's
    // context type.
    //
    // If the base type is not a nominal type, we can't substitute
    // the member type.
    //
    // If the member is a free function and not a member of a type,
    // don't substitute either.
    bool shouldSubst = (!BaseTy->isAnyObject() &&
                        !BaseTy->hasTypeVariable() &&
                        BaseTy->getNominalOrBoundGenericNominal() &&
                        VD->getDeclContext()->isTypeContext());
    ModuleDecl *M = DC->getParentModule();

    // Hack; we shouldn't be filtering at this level anyway.
    if (!VD->hasInterfaceType()) {
      FoundDecls[VD->getBaseName()].insert(VD);
      DeclsToReport.insert(FoundDeclTy(VD, Reason));
      return;
    }

    auto FoundSignature = VD->getOverloadSignature();
    auto FoundSignatureType = VD->getOverloadSignatureType();
    if (FoundSignatureType && shouldSubst &&
        shouldSubstIntoDeclType(FoundSignatureType)) {
      auto subs = BaseTy->getMemberSubstitutionMap(M, VD);
      if (auto CT = FoundSignatureType.subst(subs))
        FoundSignatureType = CT->getCanonicalType();
    }

    for (auto I = PossiblyConflicting.begin(), E = PossiblyConflicting.end();
         I != E; ++I) {
      auto *OtherVD = *I;
      if (OtherVD->isInvalid() || !OtherVD->hasInterfaceType()) {
        // For some invalid decls it might be impossible to compute the
        // signature, for example, if the types could not be resolved.
        continue;
      }

      auto OtherSignature = OtherVD->getOverloadSignature();
      auto OtherSignatureType = OtherVD->getOverloadSignatureType();
      if (OtherSignatureType && shouldSubst &&
          shouldSubstIntoDeclType(OtherSignatureType)) {
        auto subs = BaseTy->getMemberSubstitutionMap(M, OtherVD);
        if (auto CT = OtherSignatureType.subst(subs))
          OtherSignatureType = CT->getCanonicalType();
      }

      if (conflicting(M->getASTContext(), FoundSignature, FoundSignatureType,
                      OtherSignature, OtherSignatureType,
                      /*wouldConflictInSwift5*/nullptr,
                      /*skipProtocolExtensionCheck*/true)) {
        if (VD->getFormalAccess() > OtherVD->getFormalAccess()) {
          PossiblyConflicting.erase(I);
          PossiblyConflicting.insert(VD);

          bool Erased = DeclsToReport.remove(
              FoundDeclTy(OtherVD, DeclVisibilityKind::LocalVariable));
          assert(Erased);
          (void)Erased;

          DeclsToReport.insert(FoundDeclTy(VD, Reason));
        }
        return;
      }
    }

    PossiblyConflicting.insert(VD);
    DeclsToReport.insert(FoundDeclTy(VD, Reason));
  }
};
} // unnamed namespace

/// \brief Enumerate all members in \c BaseTy (including members of extensions,
/// superclasses and implemented protocols), as seen from the context \c CurrDC.
///
/// This operation corresponds to a standard "dot" lookup operation like "a.b"
/// where 'self' is the type of 'a'.  This operation is only valid after name
/// binding.
static void lookupVisibleMemberDecls(
    Type BaseTy, VisibleDeclConsumer &Consumer, const DeclContext *CurrDC,
    LookupState LS, DeclVisibilityKind Reason, LazyResolver *TypeResolver,
    GenericSignatureBuilder *GSB) {
  OverrideFilteringConsumer ConsumerWrapper(BaseTy, CurrDC, TypeResolver);
  VisitedSet Visited;
  lookupVisibleMemberDeclsImpl(BaseTy, ConsumerWrapper, CurrDC, LS, Reason,
                               TypeResolver, GSB, Visited);

  // Report the declarations we found to the real consumer.
  for (const auto &DeclAndReason : ConsumerWrapper.DeclsToReport)
    Consumer.foundDecl(DeclAndReason.D, DeclAndReason.Reason);
}

void swift::lookupVisibleDecls(VisibleDeclConsumer &Consumer,
                               const DeclContext *DC,
                               LazyResolver *TypeResolver,
                               bool IncludeTopLevel,
                               SourceLoc Loc) {
  const ModuleDecl &M = *DC->getParentModule();
  const SourceManager &SM = DC->getASTContext().SourceMgr;
  auto Reason = DeclVisibilityKind::MemberOfCurrentNominal;

  // If we are inside of a method, check to see if there are any ivars in scope,
  // and if so, whether this is a reference to one of them.
  while (!DC->isModuleScopeContext()) {
    const ValueDecl *BaseDecl = nullptr;
    Type ExtendedType;
    auto LS = LookupState::makeUnqualified();

    // Skip initializer contexts, we will not find any declarations there.
    if (isa<Initializer>(DC)) {
      DC = DC->getParent();
      LS = LS.withOnMetatype();
    }

    GenericParamList *GenericParams = DC->getGenericParamsOfContext();

    if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
      // Look for local variables; normally, the parser resolves these
      // for us, but it can't do the right thing inside local types.
      // FIXME: when we can parse and typecheck the function body partially for
      // code completion, AFD->getBody() check can be removed.
      if (Loc.isValid() && AFD->getBody()) {
        namelookup::FindLocalVal(SM, Loc, Consumer).visit(AFD->getBody());
      }

      for (auto *P : AFD->getParameterLists())
        namelookup::FindLocalVal(SM, Loc, Consumer).checkParameterList(P);

      // Constructors and destructors don't have 'self' in parameter patterns.
      if (isa<ConstructorDecl>(AFD) || isa<DestructorDecl>(AFD))
        if (auto *selfParam = AFD->getImplicitSelfDecl())
          Consumer.foundDecl(const_cast<ParamDecl*>(selfParam),
                             DeclVisibilityKind::FunctionParameter);

      if (AFD->getDeclContext()->isTypeContext()) {
        ExtendedType = AFD->getDeclContext()->getSelfTypeInContext();
        BaseDecl = AFD->getImplicitSelfDecl();
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
      ExtendedType = ED->getExtendedType();
      if (ExtendedType)
        BaseDecl = ExtendedType->getNominalOrBoundGenericNominal();
    } else if (auto ND = dyn_cast<NominalTypeDecl>(DC)) {
      ExtendedType = ND->getDeclaredTypeInContext();
      BaseDecl = ND;
    }

    if (BaseDecl && ExtendedType) {
      ::lookupVisibleMemberDecls(ExtendedType, Consumer, DC, LS, Reason,
                                 TypeResolver, nullptr);
    }

    // Check any generic parameters for something with the given name.
    namelookup::FindLocalVal(SM, Loc, Consumer)
          .checkGenericParams(GenericParams);

    DC = DC->getParent();
    Reason = DeclVisibilityKind::MemberOfOutsideNominal;
  }

  SmallVector<ModuleDecl::ImportedModule, 8> extraImports;
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

      SF->getImportedModules(extraImports, ModuleDecl::ImportFilter::Private);
    }
  }

  if (IncludeTopLevel) {
    using namespace namelookup;
    SmallVector<ValueDecl *, 0> moduleResults;
    auto &mutableM = const_cast<ModuleDecl&>(M);
    lookupVisibleDeclsInModule(&mutableM, {}, moduleResults,
                               NLKind::UnqualifiedLookup,
                               ResolutionKind::Overloadable,
                               TypeResolver, DC, extraImports);
    for (auto result : moduleResults)
      Consumer.foundDecl(result, DeclVisibilityKind::VisibleAtTopLevel);

    if (auto SF = dyn_cast<SourceFile>(DC))
      SF->cacheVisibleDecls(std::move(moduleResults));
  }
}

void swift::lookupVisibleMemberDecls(VisibleDeclConsumer &Consumer, Type BaseTy,
                                     const DeclContext *CurrDC,
                                     LazyResolver *TypeResolver,
                                     bool includeInstanceMembers,
                                     GenericSignatureBuilder *GSB) {
  assert(CurrDC);
  LookupState ls = LookupState::makeQualified();
  if (includeInstanceMembers) {
    ls = ls.withIncludedInstanceMembers();
  }

  ::lookupVisibleMemberDecls(BaseTy, Consumer, CurrDC, ls,
                             DeclVisibilityKind::MemberOfCurrentNominal,
                             TypeResolver, GSB);
}
