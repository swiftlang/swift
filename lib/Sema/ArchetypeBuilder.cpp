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
#include "TypeChecker.h"
#include "swift/AST/ArchetypeBuilder.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Pattern.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using llvm::DenseMap;

/// \brief A type that will be mapped down to some archetype, which gathers
/// all of the requirements and nested types of that archetype.
struct ArchetypeBuilder::PotentialArchetype {
  PotentialArchetype(PotentialArchetype *Parent, Identifier Name,
                     Optional<unsigned> Index = Nothing)
    : Parent(Parent), Name(Name), Index(Index), Representative(this),
      Archetype(nullptr) { }

  ~PotentialArchetype() {
    for (auto Nested : NestedTypes) {
      if (Nested.second != this) {
        delete Nested.second;
      }
    }
  }

  /// \brief The parent of this potential archetype, which will be non-null
  /// when this potential archetype is an associated type.
  PotentialArchetype *Parent;

  /// \brief The name of this potential archetype.
  Identifier Name;

  /// \brief The index of the computed archetype.
  Optional<unsigned> Index;

  /// \brief The representative of the equivalent class of potential archetypes
  /// to which this potential archetype belongs.
  PotentialArchetype *Representative;

  /// \brief The superclass of this archetype, if specified.
  Type Superclass;

  /// \brief The list of protocols to which this archetype will conform.
  llvm::SetVector<ProtocolDecl *> ConformsTo;

  /// \brief The set of nested typed stores within this archetype.
  DenseMap<Identifier, PotentialArchetype *> NestedTypes;

  /// \brief The actual archetype, once it has been assigned.
  ArchetypeType *Archetype;

  /// \brief Retrieve the name of this potential archetype.
  StringRef getName() const { return Name.str(); }

private:
  /// \brief Recursively build the full name.
  void buildFullName(SmallVectorImpl<char> &Result) const {
    if (Parent) {
      Parent->buildFullName(Result);
      Result.push_back('.');
    }
    Result.append(getName().begin(), getName().end());
  }

public:
  /// \brief Retrieve the full display name of this potential archetype.
  std::string getFullName() const {
    llvm::SmallString<64> Result;
    buildFullName(Result);
    return Result.str().str();
  }

  /// \brief Determine the nesting depth of this potential archetype, e.g.,
  /// the number of associated type references.
  unsigned getNestingDepth() const {
    unsigned Depth = 0;
    for (auto P = Parent; P; P = P->Parent)
      ++Depth;
    return Depth;
  }

  /// \brief Retrieve the representative for this archetype, performing
  /// path compression on the way.
  PotentialArchetype *getRepresentative() {
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

  /// \brief Retrieve (or create) a nested type with the given name.
  PotentialArchetype *getNestedType(Identifier Name) {
    // Retrieve the nested type from the representation of this set.
    if (Representative != this)
      return getRepresentative()->getNestedType(Name);

    PotentialArchetype *&Result = NestedTypes[Name];
    if (!Result) {
      // FIXME: The 'Self' hack is pretty ugly.
      if (Name.str() == "Self")
        Result = this;
      else
        Result = new PotentialArchetype(this, Name);
    }

    return Result;
  }

  /// \brief Retrieve (or build) the archetype corresponding to the potential
  /// archetype.
  ArchetypeType *getArchetype(AssociatedTypeDecl * /*nullable*/ rootAssocTy,
                              TranslationUnit &tu) {
    // Retrieve the archetype from the representation of this set.
    if (Representative != this)
      return getRepresentative()->getArchetype(rootAssocTy, tu);

    AssociatedTypeDecl *assocType = rootAssocTy;
    if (!Archetype) {
      // Allocate a new archetype.
      ArchetypeType *ParentArchetype = nullptr;
      if (Parent) {
        assert(!rootAssocTy &&
               "root associated type given for non-root archetype");
        ParentArchetype = Parent->getArchetype(nullptr, tu);

        if (!ParentArchetype)
          return nullptr;

        // Find the protocol that has an associated type with this name.
        llvm::SmallSetVector<AssociatedTypeDecl *, 2> assocTypes;
        for (auto proto : ParentArchetype->getConformsTo()) {
          SmallVector<ValueDecl *, 2> decls;
          if (tu.lookupQualified(proto->getDeclaredType(), Name,
                                 NL_VisitSupertypes, decls)) {
            for (auto decl : decls) {
              assocType = dyn_cast<AssociatedTypeDecl>(decl);
              if (assocType)
                break;
            }
          }
        }

        // FIXME: If assocType is null, we will diagnose it later.
        // It would be far nicer to know now, and be able to recover, e.g.,
        // via typo correction.
      }

      // If we ended up building our parent archetype, then we'll have
      // already filled in our own archetype.
      if (Archetype)
        return Archetype;

      SmallVector<ProtocolDecl *, 4> Protos(ConformsTo.begin(),
                                            ConformsTo.end());
      Archetype = ArchetypeType::getNew(tu.getASTContext(), ParentArchetype,
                                        assocType, Name, Protos, Superclass,
                                        Index);

      // Collect the set of nested types of this archetype, and put them into
      // the archetype itself.
      SmallVector<std::pair<Identifier, ArchetypeType *>, 4> FlatNestedTypes;
      for (auto Nested : NestedTypes) {
        FlatNestedTypes.push_back({ Nested.first,
                                    Nested.second->getArchetype(nullptr, tu) });
      }
      Archetype->setNestedTypes(tu.getASTContext(), FlatNestedTypes);
    }

    return Archetype;
  }

  void dump(llvm::raw_ostream &Out, unsigned Indent) {
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
};

struct ArchetypeBuilder::Implementation {
  std::function<ArrayRef<ProtocolDecl *>(ProtocolDecl *)> getInheritedProtocols;
  std::function<ArrayRef<ProtocolDecl *>(AbstractTypeParamDecl *)> getConformsTo;
  SmallVector<AbstractTypeParamDecl *, 4> GenericParams;
  DenseMap<AbstractTypeParamDecl *, PotentialArchetype *> PotentialArchetypes;
  DenseMap<AbstractTypeParamDecl *, ArchetypeType *> PrimaryArchetypeMap;
  SmallVector<ArchetypeType *, 4> AllArchetypes;
};

ArchetypeBuilder::ArchetypeBuilder(TranslationUnit &tu, DiagnosticEngine &diags)
  : TU(tu), Context(tu.getASTContext()), Diags(diags), Impl(new Implementation)
{
  Impl->getInheritedProtocols = [](ProtocolDecl *protocol) {
    return protocol->getProtocols();
  };
  Impl->getConformsTo = [](AbstractTypeParamDecl *assocType) {
    return assocType->getProtocols();
  };
}

ArchetypeBuilder::ArchetypeBuilder(
  TranslationUnit &tu, DiagnosticEngine &diags,
  std::function<ArrayRef<ProtocolDecl *>(ProtocolDecl *)> getInheritedProtocols,
  std::function<ArrayRef<ProtocolDecl *>(AbstractTypeParamDecl *)> getConformsTo)
  : TU(tu), Context(tu.getASTContext()), Diags(diags), Impl(new Implementation)
{
  Impl->getInheritedProtocols = std::move(getInheritedProtocols);
  Impl->getConformsTo = std::move(getConformsTo);
}

ArchetypeBuilder::ArchetypeBuilder(ArchetypeBuilder &&) = default;

ArchetypeBuilder::~ArchetypeBuilder() {
  if (!Impl)
    return;

  for (auto PA : Impl->PotentialArchetypes)
    delete PA.second;
}

auto ArchetypeBuilder::resolveType(Type type) -> PotentialArchetype * {
  if (auto genericParam = type->getAs<GenericTypeParamType>()) {
    // FIXME: Shouldn't have to rely on getDecl() here...
    auto known = Impl->PotentialArchetypes.find(genericParam->getDecl());
    if (known == Impl->PotentialArchetypes.end())
      return nullptr;

    return known->second;
  }

  if (auto dependentMember = type->getAs<DependentMemberType>()) {
    auto base = resolveType(dependentMember->getBase());
    if (!base)
      return nullptr;

    return base->getNestedType(dependentMember->getName());
  }

  return nullptr;
}

bool ArchetypeBuilder::addGenericParameter(AbstractTypeParamDecl *GenericParam,
                                           Optional<unsigned> Index) {
  Impl->GenericParams.push_back(GenericParam);

  // Create a potential archetype for this type parameter.
  assert(!Impl->PotentialArchetypes[GenericParam]);
  auto PA = new PotentialArchetype(nullptr, GenericParam->getName(), Index);
  Impl->PotentialArchetypes[GenericParam] = PA;

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
      // Nothing to do for 'Self'.
      if (AssocType->isSelf())
        continue;

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

  // FIXME: Add protocol conformances for all of the protocols to which this
  // class conforms? Needed for type checking.
  return false;
}

bool ArchetypeBuilder::addSameTypeRequirement(PotentialArchetype *T1,
                                              SourceLoc EqualLoc,
                                              PotentialArchetype *T2) {
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

bool ArchetypeBuilder::addRequirement(const Requirement &Req) {
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
  }

  llvm_unreachable("Unhandled requirement?");
}

bool ArchetypeBuilder::addImplicitConformance(AbstractTypeParamDecl *Param,
                                              ProtocolDecl *Proto) {
  assert(Impl->PotentialArchetypes[Param] != nullptr && "Unknown parameter");
  return addConformanceRequirement(Impl->PotentialArchetypes[Param], Proto);
}

/// \brief Add the nested archetypes of the given archetype to the set of
/// all archetypes.
static void addNestedArchetypes(ArchetypeType *Archetype,
                                llvm::SmallPtrSet<ArchetypeType *, 8> &Known,
                                SmallVectorImpl<ArchetypeType *> &All) {
  for (auto Nested : Archetype->getNestedTypes()) {
    if (Known.insert(Nested.second)) {
      assert(!Nested.second->isPrimary() && "Unexpected primary archetype");
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

  virtual bool walkToTypeReprPost(TypeRepr *T) {
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
    auto Archetype
      = PA.second->getArchetype(dyn_cast<AssociatedTypeDecl>(PA.first), TU);
    Impl->PrimaryArchetypeMap[PA.first] = Archetype;
  }
}

ArchetypeType *
ArchetypeBuilder::getArchetype(AbstractTypeParamDecl *GenericParam) const {
  auto Pos = Impl->PrimaryArchetypeMap.find(GenericParam);
  assert(Pos != Impl->PrimaryArchetypeMap.end() && "Not a parameter!");
  return Pos->second;
}

ArrayRef<ArchetypeType *> ArchetypeBuilder::getAllArchetypes() {
  if (Impl->AllArchetypes.empty()) {
    // Collect the primary archetypes first.
    llvm::SmallPtrSet<ArchetypeType *, 8> KnownArchetypes;
    for (auto GP : Impl->GenericParams) {
      auto Archetype = Impl->PrimaryArchetypeMap[GP];
      if (Archetype->isPrimary() && KnownArchetypes.insert(Archetype))
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

void ArchetypeBuilder::dump() {
  llvm::errs() << "Archetypes to build:\n";
  for (const auto& PA : Impl->PotentialArchetypes) {
    PA.second->dump(llvm::errs(), 2);
  }
}
