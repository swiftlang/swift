//===--- ArchetypeBuilder.cpp - Generic Requirement Builder -------------===//
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
// Support for collecting a set of generic requirements, both explicitly stated
// and inferred, and computing the archetypes and required witness tables from
// those requirements.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/TypeRepr.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using llvm::DenseMap;

ArchetypeBuilder::PotentialArchetype::~PotentialArchetype() {
  for (auto Nested : NestedTypes) {
    if (Nested.second != this) {
      delete Nested.second;
    }
  }
}

void ArchetypeBuilder::PotentialArchetype::buildFullName(
       SmallVectorImpl<char> &Result) const {
  if (Parent) {
    Parent->buildFullName(Result);
    Result.push_back('.');
  }
  Result.append(getName().begin(), getName().end());
}

std::string ArchetypeBuilder::PotentialArchetype::getFullName() const {
  llvm::SmallString<64> Result;
  buildFullName(Result);
  return Result.str().str();
}

unsigned ArchetypeBuilder::PotentialArchetype::getNestingDepth() const {
  unsigned Depth = 0;
  for (auto P = Parent; P; P = P->Parent)
    ++Depth;
  return Depth;
}

auto ArchetypeBuilder::PotentialArchetype::getRepresentative()
                                             -> PotentialArchetype *{
  // Find the representative.
  PotentialArchetype *Result = Representative;
  while (Result != Result->Representative)
    Result = Result->Representative;

  // Perform (full) path compression.
  PotentialArchetype *FixUp = this;
  while (FixUp != FixUp->Representative) {
    PotentialArchetype *Next = FixUp->Representative;
    FixUp->Representative = Result;
    FixUp = Next;
  }

  return Result;
}

auto ArchetypeBuilder::PotentialArchetype::getNestedType(Identifier Name)
                                             -> PotentialArchetype * {
  // Retrieve the nested type from the representation of this set.
  if (Representative != this)
    return getRepresentative()->getNestedType(Name);

  PotentialArchetype *&Result = NestedTypes[Name];
  if (!Result) {
    Result = new PotentialArchetype(this, Name);
  }

  return Result;
}

auto ArchetypeBuilder::PotentialArchetype::getArchetype(
                                             ProtocolDecl *rootProtocol,
                                             Module &mod)
                                                -> ArchetypeType * {
  // Retrieve the archetype from the representation of this set.
  if (Representative != this)
    return getRepresentative()->getArchetype(rootProtocol, mod);

  ArchetypeType::AssocTypeOrProtocolType assocTypeOrProto = rootProtocol;
  if (!Archetype) {
    // Allocate a new archetype.
    ArchetypeType *ParentArchetype = nullptr;
    if (Parent) {
      assert(assocTypeOrProto.isNull() &&
             "root protocol type given for non-root archetype");
      ParentArchetype = Parent->getArchetype(nullptr, mod);

      if (!ParentArchetype)
        return nullptr;

      // Find the protocol that has an associated type with this name.
      for (auto proto : ParentArchetype->getConformsTo()) {
        SmallVector<ValueDecl *, 2> decls;
        if (mod.lookupQualified(proto->getDeclaredType(), Name,
                               NL_VisitSupertypes, nullptr, decls)) {
          for (auto decl : decls) {
            if (auto assocType = dyn_cast<AssociatedTypeDecl>(decl)) {
              assocTypeOrProto = assocType;
              break;
            }
          }
        }
      }

      // FIXME: If assocTypeOrProto is null, we will diagnose it later.
      // It would be far nicer to know now, and be able to recover, e.g.,
      // via typo correction.
    }

    // If we ended up building our parent archetype, then we'll have
    // already filled in our own archetype.
    if (Archetype)
      return Archetype;

    SmallVector<ProtocolDecl *, 4> Protos(ConformsTo.begin(),
                                          ConformsTo.end());
    Archetype = ArchetypeType::getNew(mod.getASTContext(), ParentArchetype,
                                      assocTypeOrProto, Name, Protos,
                                      Superclass, Index);

    // Collect the set of nested types of this archetype, and put them into
    // the archetype itself.
    SmallVector<std::pair<Identifier, ArchetypeType *>, 4> FlatNestedTypes;
    for (auto Nested : NestedTypes) {
      FlatNestedTypes.push_back({ Nested.first,
                                  Nested.second->getArchetype(nullptr, mod) });
    }
    Archetype->setNestedTypes(mod.getASTContext(), FlatNestedTypes);
  }

  return Archetype;
}

AssociatedTypeDecl *
ArchetypeBuilder::PotentialArchetype::getAssociatedType(Module &mod,
                                                        Identifier name) {
  for (auto proto : getRepresentative()->getConformsTo()) {
    SmallVector<ValueDecl *, 2> decls;
    if (mod.lookupQualified(proto->getDeclaredType(), name,
                           NL_VisitSupertypes, nullptr, decls)) {
      for (auto decl : decls) {
        if (auto assocType = dyn_cast<AssociatedTypeDecl>(decl))
          return assocType;
      }
    }
  }

  return nullptr;
}

void ArchetypeBuilder::PotentialArchetype::dump(llvm::raw_ostream &Out,
                                                unsigned Indent) {
  // Print name.
  Out.indent(Indent) << getName();

  // Print superclass.
  if (Superclass) {
    Out << " : ";
    Superclass.print(Out);
  }

  // Print requirements.
  if (!ConformsTo.empty()) {
    Out << " : ";

    if (ConformsTo.size() != 1)
      Out << "protocol<";

    bool First = true;
    for (auto Proto : ConformsTo) {
      if (First)
        First = false;
      else
        Out << ", ";

      Out << Proto->getName().str();
    }

    if (ConformsTo.size() != 1)
      Out << ">";
  }

  if (Representative != this) {
    Out << " [represented by " << getRepresentative()->getFullName() << "]";
  }

  Out << "\n";

  // Print nested types.
  for (const auto &Nested : NestedTypes) {
    Nested.second->dump(Out, Indent + 2);
  }
}

/// The identifying information for a generic parameter.

namespace {
struct GenericTypeParamKey {
  unsigned Depth : 16;
  unsigned Index : 16;
  
  static GenericTypeParamKey forDecl(GenericTypeParamDecl *d) {
    return {d->getDepth(), d->getIndex()};
  }
  
  static GenericTypeParamKey forType(GenericTypeParamType *t) {
    return {t->getDepth(), t->getIndex()};
  }
};
}

namespace llvm {

template<>
struct DenseMapInfo<GenericTypeParamKey> {
  static inline GenericTypeParamKey getEmptyKey() { return {~0U, ~0U}; }
  static inline GenericTypeParamKey getTombstoneKey() { return {~1U, ~1U}; }
  static inline unsigned getHashValue(GenericTypeParamKey k) {
    return DenseMapInfo<unsigned>::getHashValue(k.Depth << 16 | k.Index);
  }
  static bool isEqual(GenericTypeParamKey a, GenericTypeParamKey b) {
    return a.Depth == b.Depth && a.Index == b.Index;
  }
};
  
}

struct ArchetypeBuilder::Implementation {
  std::function<ArrayRef<ProtocolDecl *>(ProtocolDecl *)> getInheritedProtocols;
  std::function<ArrayRef<ProtocolDecl *>(AbstractTypeParamDecl *)> getConformsTo;
  SmallVector<GenericTypeParamKey, 4> GenericParams;
  DenseMap<GenericTypeParamKey, std::pair<ProtocolDecl*, PotentialArchetype*>>
    PotentialArchetypes;
  DenseMap<GenericTypeParamKey, ArchetypeType *> PrimaryArchetypeMap;
  SmallVector<ArchetypeType *, 4> AllArchetypes;

  SmallVector<std::pair<PotentialArchetype *, PotentialArchetype *>, 4>
    SameTypeRequirements;
};

ArchetypeBuilder::ArchetypeBuilder(Module &mod, DiagnosticEngine &diags)
  : Mod(mod), Context(mod.getASTContext()), Diags(diags),
    Impl(new Implementation)
{
  Impl->getInheritedProtocols = [](ProtocolDecl *protocol) {
    return protocol->getProtocols();
  };
  Impl->getConformsTo = [](AbstractTypeParamDecl *assocType) {
    return assocType->getProtocols();
  };
}

ArchetypeBuilder::ArchetypeBuilder(
  Module &mod, DiagnosticEngine &diags,
  std::function<ArrayRef<ProtocolDecl *>(ProtocolDecl *)> getInheritedProtocols,
  std::function<ArrayRef<ProtocolDecl *>(AbstractTypeParamDecl*)> getConformsTo)
  : Mod(mod), Context(mod.getASTContext()), Diags(diags),
    Impl(new Implementation)
{
  Impl->getInheritedProtocols = std::move(getInheritedProtocols);
  Impl->getConformsTo = std::move(getConformsTo);
}

ArchetypeBuilder::ArchetypeBuilder(ArchetypeBuilder &&) = default;

ArchetypeBuilder::~ArchetypeBuilder() {
  if (!Impl)
    return;

  for (auto PA : Impl->PotentialArchetypes)
    delete PA.second.second;
}

auto ArchetypeBuilder::resolveType(Type type) -> PotentialArchetype * {
  if (auto genericParam = type->getAs<GenericTypeParamType>()) {
    auto known
      = Impl->PotentialArchetypes.find(GenericTypeParamKey::forType(genericParam));
    if (known == Impl->PotentialArchetypes.end())
      return nullptr;

    return known->second.second;
  }

  if (auto dependentMember = type->getAs<DependentMemberType>()) {
    auto base = resolveType(dependentMember->getBase());
    if (!base)
      return nullptr;

    return base->getNestedType(dependentMember->getName());
  }

  return nullptr;
}

auto ArchetypeBuilder::addGenericParameter(ProtocolDecl *RootProtocol,
                                           Identifier ParamName,
                                           unsigned ParamDepth,
                                           unsigned ParamIndex,
                                           Optional<unsigned> Index)
  -> PotentialArchetype *
{
  GenericTypeParamKey Key{ParamDepth, ParamIndex};
  
  Impl->GenericParams.push_back(Key);

  // Create a potential archetype for this type parameter.
  assert(!Impl->PotentialArchetypes[Key].second);
  auto PA = new PotentialArchetype(nullptr, ParamName, Index);
  Impl->PotentialArchetypes[Key] = {RootProtocol, PA};
  
  return PA;
}

bool ArchetypeBuilder::addGenericParameter(GenericTypeParamDecl *GenericParam,
                                           Optional<unsigned> Index) {
  PotentialArchetype *PA
    = addGenericParameter(dyn_cast<ProtocolDecl>(GenericParam->getDeclContext()),
                          GenericParam->getName(),
                          GenericParam->getDepth(),
                          GenericParam->getIndex(), Index);
  
  if (!PA)
    return true;
  
  // Add each of the conformance requirements placed on this type parameter.
  for (auto Proto : GenericParam->getProtocols()) {
    if (addConformanceRequirement(PA, Proto))
      return true;
  }

  // If the type parameter has a superclass, add that requirement.
  if (auto superclassTy = GenericParam->getSuperclass()) {
    // FIXME: Poor location info.
    addSuperclassRequirement(PA, GenericParam->getLoc(), superclassTy);
  }

  return false;
}

bool ArchetypeBuilder::addGenericParameter(GenericTypeParamType *GenericParam,
                                           Optional<unsigned> Index) {
  PotentialArchetype *PA = addGenericParameter(nullptr,
                                               GenericParam->getName(),
                                               GenericParam->getDepth(),
                                               GenericParam->getIndex());
  return !PA;
}

bool ArchetypeBuilder::addConformanceRequirement(PotentialArchetype *T,
                                                 ProtocolDecl *Proto){
  // Add the requirement to the representative.
  T = T->getRepresentative();

  // If we've already added this requirement, we're done.
  if (!T->ConformsTo.insert(Proto))
    return false;

  // Add all of the inherited protocol requirements, recursively.
  for (auto InheritedProto : Impl->getInheritedProtocols(Proto)) {
    if (addConformanceRequirement(T, InheritedProto))
      return true;
  }

  // Add requirements for each of the associated types.
  for (auto Member : Proto->getMembers()) {
    if (auto AssocType = dyn_cast<AssociatedTypeDecl>(Member)) {
      // Add requirements placed directly on this associated type.
      auto AssocPA = T->getNestedType(AssocType->getName());
      for (auto InheritedProto : Impl->getConformsTo(AssocType)) {
        if (addConformanceRequirement(AssocPA, InheritedProto))
          return true;
      }

      continue;
    }

    // FIXME: Requirement declarations.
  }

  return false;
}

bool ArchetypeBuilder::addSuperclassRequirement(PotentialArchetype *T,
                                                SourceLoc ColonLoc,
                                                Type Superclass) {
  // If T already has a superclass, make sure it's the same superclass.
  // FIXME: We should compute the meet here.
  if (T->Superclass) {
    if (!T->Superclass->isEqual(Superclass)) {
      Diags.diagnose(ColonLoc, diag::requires_superclass_conflict, T->Name,
                     T->Superclass, Superclass);
      return true;
    }

    return false;
  }

  // Set the superclass.
  T->Superclass = Superclass;

  return false;
}

bool ArchetypeBuilder::addSameTypeRequirement(PotentialArchetype *T1,
                                              SourceLoc EqualLoc,
                                              PotentialArchetype *T2) {
  auto OrigT1 = T1, OrigT2 = T2;

  // Operate on the representatives
  T1 = T1->getRepresentative();
  T2 = T2->getRepresentative();

  // If the representives are already the same, we're done.
  if (T1 == T2)
    return false;

  // FIXME: Do we want to restrict which potential archetypes can be made
  // equivalent? For example, can two generic parameters T and U be made
  // equivalent? What about a generic parameter and a concrete type?

  // Merge into the potential archetype with the smaller nesting depth. We
  // prefer lower-depth potential archetypes both for better diagnostics (use
  // T.A rather than U.B.C. when the two are required to be the same type) and
  // because we want to select a generic parameter as a representative over an
  // associated type.
  unsigned T1Depth = T1->getNestingDepth();
  unsigned T2Depth = T2->getNestingDepth();
  if (T2Depth < T1Depth)
    std::swap(T1, T2);

  // Don't allow two generic parameters to be equivalent, because then we
  // don't actually have two parameters.
  // FIXME: Should we simply allow this?
  if (T1Depth == 0 && T2Depth == 0) {
    Diags.diagnose(EqualLoc, diag::requires_generic_param_equal,
                   T1->Name, T2->Name);
    return true;
  }

  // Make T1 the representative of T2, merging the equivalence classes.
  T2->Representative = T1;

  // Record this same-type requirement.
  Impl->SameTypeRequirements.push_back({ OrigT1, OrigT2 });

  // Add all of the protocol conformance requirements of T2 to T1.
  for (auto Proto : T2->ConformsTo)
    T1->ConformsTo.insert(Proto);

  // Recursively merge the associated types of T2 into T1.
  for (auto T2Nested : T2->NestedTypes) {
    auto T1Nested = T1->getNestedType(T2Nested.first);
    if (addSameTypeRequirement(T1Nested, EqualLoc, T2Nested.second))
      return true;
  }

  return false;
}

bool ArchetypeBuilder::addRequirement(const RequirementRepr &Req) {
  switch (Req.getKind()) {
  case RequirementKind::Conformance: {
    PotentialArchetype *PA = resolveType(Req.getSubject());
    if (!PA) {
      // FIXME: Poor location information.
      // FIXME: Delay diagnostic until after type validation?
      Diags.diagnose(Req.getColonLoc(), diag::requires_not_suitable_archetype,
                     0, Req.getSubjectLoc(), 0);
      return true;
    }

    // Check whether this is a supertype requirement.
    if (Req.getConstraint()->getClassOrBoundGenericClass()) {
      return addSuperclassRequirement(PA, Req.getColonLoc(),
                                      Req.getConstraint());
    }

    SmallVector<ProtocolDecl *, 4> ConformsTo;
    if (!Req.getConstraint()->isExistentialType(ConformsTo)) {
      // FIXME: Diagnose this failure here, rather than over in type-checking.
      return true;
    }

    // Add each of the protocols.
    for (auto Proto : ConformsTo)
      if (addConformanceRequirement(PA, Proto))
        return true;

    return false;
  }

  case RequirementKind::SameType: {
    // FIXME: Allow one of the types to not be a potential archetype, e.g.,
    // T.Element == Int?
    PotentialArchetype *FirstPA = resolveType(Req.getFirstType());
    if (!FirstPA) {
      // FIXME: Poor location information.
      // FIXME: Delay diagnostic until after type validation?
      Diags.diagnose(Req.getEqualLoc(), diag::requires_not_suitable_archetype,
                     1, Req.getFirstTypeLoc(), 1);
      return true;
    }

    PotentialArchetype *SecondPA = resolveType(Req.getSecondType());
    if (!SecondPA) {
      // FIXME: Poor location information.
      // FIXME: Delay diagnostic until after type validation?
      Diags.diagnose(Req.getEqualLoc(), diag::requires_not_suitable_archetype,
                     2, Req.getSecondTypeLoc(), 1);
      return true;
    }
    return addSameTypeRequirement(FirstPA, Req.getEqualLoc(), SecondPA);
  }

  case RequirementKind::ValueWitnessMarker:
    llvm_unreachable("Value witness marker in requirement");
  }

  llvm_unreachable("Unhandled requirement?");
}

void ArchetypeBuilder::addRequirement(const Requirement &req) {
  switch (req.getKind()) {
  case RequirementKind::Conformance: {
    PotentialArchetype *pa = resolveType(req.getFirstType());
    assert(pa && "Re-introducing invalid requirement");

    if (req.getSecondType()->getClassOrBoundGenericClass()) {
      addSuperclassRequirement(pa, SourceLoc(), req.getSecondType());
      return;
    }

    SmallVector<ProtocolDecl *, 4> conformsTo;
    bool existential = req.getSecondType()->isExistentialType(conformsTo);
    assert(existential && "Re-introducing invalid requirement");
    (void)existential;

    // Add each of the protocols.
    for (auto proto : conformsTo) {
      bool invalid = addConformanceRequirement(pa, proto);
      assert(!invalid && "Re-introducing invalid requirement");
      (void)invalid;
    }

    return;
  }

  case RequirementKind::SameType: {
    PotentialArchetype *firstPA = resolveType(req.getFirstType());
    assert(firstPA && "Re-introducing invalid requirement");

    PotentialArchetype *secondPA = resolveType(req.getSecondType());
    assert(secondPA && "Re-introducing invalid requirement");
    addSameTypeRequirement(firstPA, SourceLoc(), secondPA);
    return;
  }

  case RequirementKind::ValueWitnessMarker:
    return;
  }

  llvm_unreachable("Unhandled requirement?");
}

bool ArchetypeBuilder::addImplicitConformance(GenericTypeParamDecl *Param,
                                              ProtocolDecl *Proto) {
  auto Key = GenericTypeParamKey::forDecl(Param);
  assert(Impl->PotentialArchetypes[Key].second != nullptr && "Unknown parameter");
  return addConformanceRequirement(Impl->PotentialArchetypes[Key].second, Proto);
}

/// Determine whether this archetype is a protocol's 'Self' archetype or
/// an associated type derived from it.
///
/// Archetypes based on a protocol's 'Self' archetype are not listed as part
/// of the archetypes in a polymorphic function type, because they are
/// handled implicitly by clients.
static bool isArchetypeSelfDerived(ArchetypeType *archetype) {
  while (auto parent = archetype->getParent())
    archetype = parent;
  return archetype->getSelfProtocol();
}

/// \brief Add the nested archetypes of the given archetype to the set of
/// all archetypes.
static void addNestedArchetypes(ArchetypeType *Archetype,
                                llvm::SmallPtrSet<ArchetypeType *, 8> &Known,
                                SmallVectorImpl<ArchetypeType *> &All) {
  for (auto Nested : Archetype->getNestedTypes()) {
    if (Known.insert(Nested.second)) {
      assert(!Nested.second->isPrimary() && "Unexpected primary archetype");
      if (!isArchetypeSelfDerived(Nested.second))
        All.push_back(Nested.second);
      addNestedArchetypes(Nested.second, Known, All);
    }
  }
}

/// AST walker that infers requirements from type representations.
class ArchetypeBuilder::InferRequirementsWalker : public ASTWalker {
  ArchetypeBuilder &Builder;
  bool HadError = false;

public:
  explicit InferRequirementsWalker(ArchetypeBuilder &builder)
    : Builder(builder) { }

  bool hadError() const { return HadError; }

  bool walkToTypeReprPost(TypeRepr *T) override {
    if (HadError)
      return false;

    auto identRepr = dyn_cast<IdentTypeRepr>(T);
    if (!identRepr)
      return true;

    // For each of the components...
    for (auto &comp : identRepr->Components) {
      // If there is no type binding, we don't care.
      if (!comp.isBoundType())
        continue;

      // If there are no generic arguments, we don't care.
      if (comp.getGenericArgs().empty())
        continue;

      // If it's not a bound generic type, we don't care.
      auto boundGeneric = comp.getBoundType()->getAs<BoundGenericType>();
      if (!boundGeneric)
        continue;

      // Infer superclass and protocol-conformance requirements from the
      // generic parameters.
      auto params = boundGeneric->getDecl()->getGenericParams()->getParams();
      auto args = boundGeneric->getGenericArgs();
      for (unsigned i = 0, n = params.size(); i != n; ++i) {
        auto arg = args[i];
        auto param = params[i].getAsTypeParam();

        // Try to resolve the argument to a potential archetype.
        auto argPA = Builder.resolveType(arg);
        if (!argPA)
          continue;

        // Add implicit conformances for all of the protocol requirements on
        // FIXME: This won't capture same-type requirements that map down to
        // fixed types, although that isn't permitted yet.
        for (auto proto : param->getArchetype()->getConformsTo()) {
          if (Builder.addConformanceRequirement(argPA, proto)) {
            HadError = true;
            return false;
          }
        }
      }
    }

    return true;
  }
};

bool ArchetypeBuilder::inferRequirements(TypeRepr *type) {
  InferRequirementsWalker walker(*this);
  type->walk(walker);
  return walker.hadError();
}

bool ArchetypeBuilder::inferRequirements(Pattern *pattern) {
  InferRequirementsWalker walker(*this);
  pattern->walk(walker);
  return walker.hadError();
}

void ArchetypeBuilder::assignArchetypes() {
  // Compute the archetypes for each of the potential archetypes (i.e., the
  // generic parameters).
  for (const auto& PA : Impl->PotentialArchetypes) {
    auto Archetype = PA.second.second->getArchetype(
                       PA.second.first,
                       Mod);
    Impl->PrimaryArchetypeMap[PA.first] = Archetype;
  }
}

ArchetypeType *
ArchetypeBuilder::getArchetype(GenericTypeParamDecl *GenericParam) const {
  auto Key = GenericTypeParamKey::forDecl(GenericParam);
  auto Pos = Impl->PrimaryArchetypeMap.find(Key);
  assert(Pos != Impl->PrimaryArchetypeMap.end() && "Not a parameter!");
  return Pos->second;
}

ArchetypeType *
ArchetypeBuilder::getArchetype(GenericTypeParamType *GenericParam) const {
  auto Key = GenericTypeParamKey::forType(GenericParam);
  auto Pos = Impl->PrimaryArchetypeMap.find(Key);
  assert(Pos != Impl->PrimaryArchetypeMap.end() && "Not a parameter!");
  return Pos->second;
}

ArchetypeType *
ArchetypeBuilder::getArchetype(DependentMemberType *AssocType) {
  // FIXME: root protocol?
  return resolveType(AssocType)->getArchetype(nullptr, Mod);
}

ArrayRef<ArchetypeType *> ArchetypeBuilder::getAllArchetypes() {
  if (Impl->AllArchetypes.empty()) {
    // Collect the primary archetypes first.
    llvm::SmallPtrSet<ArchetypeType *, 8> KnownArchetypes;
    for (auto GP : Impl->GenericParams) {
      auto Archetype = Impl->PrimaryArchetypeMap[GP];
      if (Archetype->isPrimary() && KnownArchetypes.insert(Archetype) &&
          !isArchetypeSelfDerived(Archetype))
        Impl->AllArchetypes.push_back(Archetype);
    }

    // Collect all of the remaining archetypes.
    for (auto GP : Impl->GenericParams) {
      auto Archetype = Impl->PrimaryArchetypeMap[GP];
      addNestedArchetypes(Archetype, KnownArchetypes, Impl->AllArchetypes);
    }
  }

  return Impl->AllArchetypes;
}

ArrayRef<std::pair<ArchetypeBuilder::PotentialArchetype *,
                   ArchetypeBuilder::PotentialArchetype *>>
ArchetypeBuilder::getSameTypeRequirements() const {
  return Impl->SameTypeRequirements;
}

void ArchetypeBuilder::dump() {
  llvm::errs() << "Archetypes to build:\n";
  for (const auto& PA : Impl->PotentialArchetypes) {
    PA.second.second->dump(llvm::errs(), 2);
  }
}

Type ArchetypeBuilder::mapTypeIntoContext(DeclContext *dc, Type type) {
  // If the type is not dependent, there's nothing to map.
  if (!type->isDependentType())
    return type;

  auto genericParams = dc->getGenericParamsOfContext();
  assert(genericParams && "Missing generic parameters for dependent context");
  
  return mapTypeIntoContext(dc->getASTContext(), genericParams, type);
}

Type ArchetypeBuilder::mapTypeIntoContext(ASTContext &C,
                                          GenericParamList *genericParams,
                                          Type type) {
  // If the type is not dependent, there's nothing to map.
  if (!type->isDependentType())
    return type;

  unsigned genericParamsDepth = genericParams->getDepth();
  return type.transform(C, [&](Type type) -> Type {
    // Map a generic parameter type to its archetype.
    if (auto gpType = type->getAs<GenericTypeParamType>()) {
      auto index = gpType->getIndex();
      unsigned depth = gpType->getDepth();

      // Skip down to the generic parameter list that houses the corresponding
      // generic parameter.
      auto myGenericParams = genericParams;
      unsigned skipLevels = genericParamsDepth - depth;
      while (skipLevels > 0) {
        myGenericParams = genericParams->getOuterParameters();
        assert(myGenericParams && "Wrong number of levels?");
        --skipLevels;
      }

      // Extract the generic parameter.
      auto gp = myGenericParams->getParams()[index];

      // Return the archetype.
      return gp.getAsTypeParam()->getArchetype();
    }

    // Map a dependent member to the corresponding nested archetype.
    if (auto dependentMember = type->getAs<DependentMemberType>()) {
      auto base = mapTypeIntoContext(C, genericParams,
                                     dependentMember->getBase());
      auto baseArchetype = base->castTo<ArchetypeType>();
      return baseArchetype->getNestedType(dependentMember->getName());
    }

    return type;
  });
}

