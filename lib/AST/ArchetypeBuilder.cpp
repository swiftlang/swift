//===--- ArchetypeBuilder.cpp - Generic Requirement Builder ---------------===//
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
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Module.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeRepr.h"
#include "swift/AST/TypeWalker.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>

using namespace swift;
using llvm::DenseMap;

void RequirementSource::dump(SourceManager *srcMgr) const {
  dump(llvm::errs(), srcMgr);
}

void RequirementSource::dump(llvm::raw_ostream &out, 
                             SourceManager *srcMgr) const {
  switch (getKind()) {
  case Explicit:
    out << "explicit";
    break;

  case Protocol:
    out << "protocol";
    break;

  case Inferred:
    out << "inferred";
    break;

  case OuterScope:
    out << "outer";
    break;
  }

  if (srcMgr && getLoc().isValid()) {
    out << " @ ";
    getLoc().dump(*srcMgr);
  }
}

namespace {
  /// A class whose instances capture the potential update of an
  /// existing requirement source when another equivalent requirement
  /// is found.
  class UpdateRequirementSource {
    RequirementSource &Source;
    const RequirementSource &NewSource;
    bool IsRedundant = false;
    bool ShouldUpdate = false;
    bool OriginalIsRedundant = false;

  public:
    UpdateRequirementSource(RequirementSource &source,
                            const RequirementSource &newSource);

    ~UpdateRequirementSource() {
      if (ShouldUpdate)
        Source = NewSource;
    }

    /// Determine whether one of the requirements is considered
    /// redundant, which can be diagnosed.
    explicit operator bool() const { return IsRedundant; }

    /// Determine whether it is the original source (vs. the new
    /// source) that is redundant.
    bool originalIsRedundant() const { return OriginalIsRedundant; }
  };
}

UpdateRequirementSource::UpdateRequirementSource(
                           RequirementSource &source,
                           const RequirementSource &newSource)
  : Source(source), NewSource(newSource) 
{
  // If one of the requirements is explicit, it is redundant.
  if (source.getKind() == RequirementSource::Explicit ||
      newSource.getKind() == RequirementSource::Explicit) {
    IsRedundant = true;
    
    // If the new source isn't explicit, override the old source
    // with the new. We don't maintain redundant explicit
    // requirements.
    if (newSource.getKind() != RequirementSource::Explicit) {
      ShouldUpdate = true;
      OriginalIsRedundant = true;
    }

    return;
  }

  // If the source kinds are the same, there is nothing to do.
  if (source.getKind() == newSource.getKind())
    return;

  switch (newSource.getKind()) {
  case RequirementSource::Explicit:
    llvm_unreachable("Handled above");

  case RequirementSource::Inferred:
    // A new inferred source will never override an existing source.
    return;

  case RequirementSource::Protocol: {
    // A new protocol source will override an inferred source.
    switch (source.getKind()) {
    case RequirementSource::Explicit:
    case RequirementSource::Protocol:
      llvm_unreachable("Handled above");

    case RequirementSource::OuterScope:
      return;

    case RequirementSource::Inferred:
      ShouldUpdate = true;
      return;
    }
  }

  case RequirementSource::OuterScope:
    // An outer-scope source always overrides an existing source.
    ShouldUpdate = true;
    return;
  }

  return;
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
  static inline GenericTypeParamKey getEmptyKey() { return {0xFFFF, 0xFFFF}; }
  static inline GenericTypeParamKey getTombstoneKey() { return {0xFFFE, 0xFFFE}; }
  static inline unsigned getHashValue(GenericTypeParamKey k) {
    return DenseMapInfo<unsigned>::getHashValue(k.Depth << 16 | k.Index);
  }
  static bool isEqual(GenericTypeParamKey a, GenericTypeParamKey b) {
    return a.Depth == b.Depth && a.Index == b.Index;
  }
};
  
}

struct ArchetypeBuilder::Implementation {
  /// Callback that produces the array of protocols describing the protocols
  /// inherited by its argument.
  std::function<ArrayRef<ProtocolDecl *>(ProtocolDecl *)> getInheritedProtocols;

  /// Callback that produces the superclass and protocol requirements placed
  /// on the given generic type parameter.
  GetConformsToCallback getConformsTo;

  /// Compute the protocol conformance when a given type conforms to the given
  /// protocol.
  std::function<ProtocolConformance *(Module &, Type, ProtocolDecl*)>
    conformsToProtocol;

  /// The list of potential archetypes that correspond to generic parameters.
  SmallVector<PotentialArchetype *, 4> RootPotentialArchetypes;

  /// A mapping from generic parameters to the corresponding potential
  /// archetypes.
  DenseMap<GenericTypeParamKey, PotentialArchetype*> PotentialArchetypes;

  /// A vector containing all of the archetypes, expanded out.
  /// FIXME: This notion should go away, because it's impossible to expand
  /// out "all" archetypes
  SmallVector<ArchetypeType *, 4> AllArchetypes;

  /// A vector containing the same-type requirements introduced into the
  /// system.
  SmallVector<SameTypeRequirement, 4> SameTypeRequirements;
};

ArchetypeBuilder::PotentialArchetype::~PotentialArchetype() {
  for (const auto &nested : NestedTypes) {
    for (auto pa : nested.second) {
      if (pa != this)
        delete pa;
    }
  }
}

void ArchetypeBuilder::PotentialArchetype::buildFullName(
       bool forDebug,
       SmallVectorImpl<char> &result) const {
  if (auto parent = getParent()) {
    parent->buildFullName(forDebug, result);

    // When building the name for debugging purposes, include the
    // protocol into which the associated type was resolved.
    if (forDebug) {
      if (auto assocType = getResolvedAssociatedType()) {
        result.push_back('[');
        result.push_back('.');
        result.append(assocType->getProtocol()->getName().str().begin(), 
                      assocType->getProtocol()->getName().str().end());
        result.push_back(']');
      }
    }

    result.push_back('.');
  }
  result.append(getName().str().begin(), getName().str().end());
}

Identifier ArchetypeBuilder::PotentialArchetype::getName() const { 
  if (auto assocType = NameOrAssociatedType.dyn_cast<AssociatedTypeDecl *>())
    return assocType->getName();
  
  return NameOrAssociatedType.get<Identifier>();
}

std::string ArchetypeBuilder::PotentialArchetype::getFullName() const {
  llvm::SmallString<64> result;
  buildFullName(false, result);
  return result.str().str();
}

std::string ArchetypeBuilder::PotentialArchetype::getDebugName() const {
  llvm::SmallString<64> result;
  buildFullName(true, result);
  return result.str().str();
}

unsigned ArchetypeBuilder::PotentialArchetype::getNestingDepth() const {
  unsigned Depth = 0;
  for (auto P = getParent(); P; P = P->getParent())
    ++Depth;
  return Depth;
}

bool ArchetypeBuilder::PotentialArchetype::addConformance(
       ProtocolDecl *proto, 
       const RequirementSource &source) {
  auto rep = getRepresentative();
  if (rep != this)
    return rep->addConformance(proto, source);

  // Check whether we already know about this conformance.
  auto known = ConformsTo.find(proto);
  if (known != ConformsTo.end()) {
    // We already have this requirement. Update the requirement source
    // appropriately.
    UpdateRequirementSource(known->second, source);
    return false;
  }

  // Add this conformance.
  ConformsTo.insert(std::make_pair(proto, source));

  // Check whether any associated types in this protocol resolve
  // nested types of this potential archetype.
  // FIXME: Eventually, will need to add more same-type requirements
  // here as well.
  for (auto member : proto->getMembers()) {
    auto assocType = dyn_cast<AssociatedTypeDecl>(member);
    if (!assocType)
      continue;

    auto known = NestedTypes.find(assocType->getName());
    if (known == NestedTypes.end())
      continue;

    // If the nested type was not already resolved, do so now.
    if (!known->second.front()->getResolvedAssociatedType()) {
      known->second.front()->NameOrAssociatedType = assocType;
      continue;
    }

    // Otherwise, create a new potential archetype for this associated type
    // and make it equivalent to the first potential archetype we encountered.
    auto otherPA = new PotentialArchetype(this, assocType);
    otherPA->Representative = known->second.front();
    otherPA->SameTypeSource = RequirementSource(RequirementSource::Inferred,
                                                source.getLoc());
    known->second.push_back(otherPA);
  }

  return true;
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

auto ArchetypeBuilder::PotentialArchetype::getNestedType(
    Identifier nestedName, Identifier *parentName) -> PotentialArchetype *{
  // Retrieve the nested type from the representation of this set.
  if (Representative != this)
    return getRepresentative()->getNestedType(nestedName);

  llvm::TinyPtrVector<PotentialArchetype *> &nested = NestedTypes[nestedName];
    
  if (!nested.empty())
    return nested.front();

  // FIXME: The fact that we've been checking for "new" nested archetypes on
  // a purely lexical basis is both fragile and not-quite-correct. We need
  // to move to a lazier model for adding conformance requirements that will
  // allow us to deal with recursive dependencies without comparing identifier
  // names.
  if (parentName &&
      this->getParent() &&
      (*parentName == this->getParent()->getName() ||
       this->getParent()->getName().str().equals("Self") ||
       this->getParent()->getName() == this->getName()) &&
      nestedName == this->getName()) {
    nested.push_back(this);
  } else {
    // Attempt to resolve this nested type to an associated type
    // of one of the protocols to which the parent potential
    // archetype conforms.
    for (const auto &conforms : ConformsTo) {
      for (auto member : conforms.first->lookupDirect(nestedName)) {
        auto assocType = dyn_cast<AssociatedTypeDecl>(member);
        if (!assocType)
          continue;

        // Resolve this nested type to this associated type.
        auto pa = new PotentialArchetype(this, assocType);

        // If we have resolved this nested type to more than one associated
        // type, create same-type constraints between them.
        if (!nested.empty()) {
          pa->Representative = nested.front();
          pa->SameTypeSource = RequirementSource(RequirementSource::Inferred,
                                                 SourceLoc());
        }

        // Add this resolved nested type.
        nested.push_back(pa);
      }
    }

    // We couldn't resolve the nested type yet, so create an
    // unresolved associated type.  
    // FIXME: Record this somewhere.
    if (nested.empty())
      nested.push_back(new PotentialArchetype(this, nestedName));
  }
  
  return nested.front();
}

ArchetypeType::NestedType
ArchetypeBuilder::PotentialArchetype::getType(ArchetypeBuilder &builder) {
  // Retrieve the archetype from the representation of this set.
  if (Representative != this)
    return getRepresentative()->getType(builder);
  
  // Return a concrete type or archetype we've already resolved.
  if (ArchetypeOrConcreteType) {
    return ArchetypeOrConcreteType;
  }
  
  ArchetypeType::AssocTypeOrProtocolType assocTypeOrProto = RootProtocol;
  // Allocate a new archetype.
  ArchetypeType *ParentArchetype = nullptr;
  auto &mod = builder.getModule();
  if (auto parent = getParent()) {
    assert(assocTypeOrProto.isNull() &&
           "root protocol type given for non-root archetype");
    auto parentTy = parent->getType(builder);
    if (!parentTy)
      return {};
    ParentArchetype = parentTy.dyn_cast<ArchetypeType*>();
    if (!ParentArchetype)
      return {};

    // Find the protocol that has an associated type with this name.
    for (auto proto : ParentArchetype->getConformsTo()) {
      SmallVector<ValueDecl *, 2> decls;
      if (mod.lookupQualified(proto->getDeclaredType(), getName(),
                              NL_VisitSupertypes | NL_IgnoreAccessibility,
                              /*FIXME:*/nullptr, decls)) {
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
  if (auto arch = ArchetypeOrConcreteType.get<ArchetypeType*>())
    return arch;

  SmallVector<ProtocolDecl *, 4> Protos;
  for (const auto &conforms : ConformsTo) {
    Protos.push_back(conforms.first);
  }

  auto arch
    = ArchetypeType::getNew(mod.getASTContext(), ParentArchetype,
                            assocTypeOrProto, getName(), Protos,
                            Superclass, this->isRecursive);

  ArchetypeOrConcreteType = arch;
  
  // Collect the set of nested types of this archetype, and put them into
  // the archetype itself.
  SmallVector<std::pair<Identifier, ArchetypeType::NestedType>, 4>
    FlatNestedTypes;
  for (auto Nested : NestedTypes) {
    FlatNestedTypes.push_back({ Nested.first,
                                Nested.second.front()->getType(builder) });
  }
  arch->setNestedTypes(mod.getASTContext(), FlatNestedTypes);
  
  return arch;
}

Type ArchetypeBuilder::PotentialArchetype::getDependentType(
       ArchetypeBuilder &builder) {
  if (auto parent = getParent()) {
    Type parentType = parent->getDependentType(builder);

    // If we've resolved to an associated type, use it.
    if (auto assocType = getResolvedAssociatedType())
      return DependentMemberType::get(parentType, assocType, builder.Context);

    return DependentMemberType::get(parentType, getName(), builder.Context);
  }

  assert(getGenericParam() && "Not a generic parameter?");
  return getGenericParam();
}

AssociatedTypeDecl *
ArchetypeBuilder::PotentialArchetype::getAssociatedType(Module &mod,
                                                        Identifier name) {
  for (const auto &conforms : getRepresentative()->getConformsTo()) {
    auto proto = conforms.first;
    SmallVector<ValueDecl *, 2> decls;
    // FIXME: lookupDirect should suffice here.
    if (mod.lookupQualified(proto->getDeclaredType(), name,
                            NL_VisitSupertypes | NL_IgnoreAccessibility,
                            /*FIXME:*/nullptr, decls)) {
      for (auto decl : decls) {
        // FIXME: Collect multiple results equate them with same-type
        // constraints.
        if (auto assocType = dyn_cast<AssociatedTypeDecl>(decl))
          return assocType;
      }
    }
  }

  return nullptr;
}

void ArchetypeBuilder::PotentialArchetype::dump(llvm::raw_ostream &Out,
                                                SourceManager *SrcMgr,
                                                unsigned Indent) {
  // Print name.
  Out.indent(Indent) << getName();

  // Print superclass.
  if (Superclass) {
    Out << " : ";
    Superclass.print(Out);
    Out << " [";
    SuperclassSource->dump(Out, SrcMgr);
    Out << "]";
  }

  // Print requirements.
  if (!ConformsTo.empty()) {
    Out << " : ";

    if (ConformsTo.size() != 1)
      Out << "protocol<";

    bool First = true;
    for (const auto &ProtoAndSource : ConformsTo) {
      if (First)
        First = false;
      else
        Out << ", ";

      Out << ProtoAndSource.first->getName().str() << " [";
      ProtoAndSource.second.dump(Out, SrcMgr);
      Out << "]";
    }

    if (ConformsTo.size() != 1)
      Out << ">";
  }

  if (Representative != this) {
    Out << " [represented by " << getRepresentative()->getFullName() << "]";
  }

  Out << "\n";

  // Print nested types.
  for (const auto &nestedVec : NestedTypes) {
    for (auto nested : nestedVec.second) {
      nested->dump(Out, SrcMgr, Indent + 2);
    }
  }
}

ArchetypeBuilder::ArchetypeBuilder(Module &mod, DiagnosticEngine &diags)
  : Mod(mod), Context(mod.getASTContext()), Diags(diags),
    Impl(new Implementation)
{
  Impl->getInheritedProtocols = [](ProtocolDecl *protocol) {
    return protocol->getProtocols();
  };
  Impl->getConformsTo = [](AbstractTypeParamDecl *assocType) {
    return std::make_pair(assocType->getSuperclass(), 
                          assocType->getProtocols());
  };
  Impl->conformsToProtocol = [](Module &M, Type t, ProtocolDecl *protocol)
  -> ProtocolConformance* {
    auto res = M.lookupConformance(t, protocol, nullptr);
    if (res.getInt() == ConformanceKind::DoesNotConform)
      return nullptr;
    return res.getPointer();
  };
}

ArchetypeBuilder::ArchetypeBuilder(
  Module &mod, DiagnosticEngine &diags, 
  LazyResolver *lazyResolver,
  std::function<ArrayRef<ProtocolDecl *>(ProtocolDecl *)> getInheritedProtocols,
  GetConformsToCallback getConformsTo,
  std::function<ProtocolConformance * (Module &, Type, ProtocolDecl*)>
    conformsToProtocol)
  : Mod(mod), Context(mod.getASTContext()), Diags(diags),
    Resolver(lazyResolver), Impl(new Implementation)
{
  Impl->getInheritedProtocols = std::move(getInheritedProtocols);
  Impl->getConformsTo = std::move(getConformsTo);
  Impl->conformsToProtocol = std::move(conformsToProtocol);
}

ArchetypeBuilder::ArchetypeBuilder(ArchetypeBuilder &&) = default;

ArchetypeBuilder::~ArchetypeBuilder() {
  if (!Impl)
    return;

  for (auto PA : Impl->PotentialArchetypes)
    delete PA.second;
}

auto ArchetypeBuilder::resolveArchetype(Type type) -> PotentialArchetype * {
  if (auto genericParam = type->getAs<GenericTypeParamType>()) {
    auto known
      = Impl->PotentialArchetypes.find(GenericTypeParamKey::forType(genericParam));
    if (known == Impl->PotentialArchetypes.end())
      return nullptr;

    return known->second;
  }

  if (auto dependentMember = type->getAs<DependentMemberType>()) {
    auto base = resolveArchetype(dependentMember->getBase());
    if (!base)
      return nullptr;

    return base->getNestedType(dependentMember->getName());
  }

  return nullptr;
}

auto ArchetypeBuilder::addGenericParameter(GenericTypeParamType *GenericParam,
                                           ProtocolDecl *RootProtocol,
                                           Identifier ParamName)
       -> PotentialArchetype *
{
  GenericTypeParamKey Key{GenericParam->getDepth(), GenericParam->getIndex()};
  
  // Create a potential archetype for this type parameter.
  assert(!Impl->PotentialArchetypes[Key]);
  auto PA = new PotentialArchetype(GenericParam, RootProtocol, ParamName);

  Impl->PotentialArchetypes[Key] = PA;
  Impl->RootPotentialArchetypes.push_back(PA);  
  return PA;
}

bool ArchetypeBuilder::addGenericParameter(GenericTypeParamDecl *GenericParam) {
  PotentialArchetype *PA
    = addGenericParameter(
        GenericParam->getDeclaredType()->castTo<GenericTypeParamType>(),
        dyn_cast<ProtocolDecl>(GenericParam->getDeclContext()),
        GenericParam->getName());
  
  if (!PA)
    return true;
  
  // FIXME: Poor source-location information.
  RequirementSource source(RequirementSource::Explicit, GenericParam->getLoc());

  // Add each of the conformance requirements placed on this type parameter.
  // FIXME: Would prefer not the walk the protocols. Walk the "inherited" types
  // directly instead, so we have good location information.
  for (auto Proto : GenericParam->getProtocols()) {
    if (addConformanceRequirement(PA, Proto, source))
      return true;
  }

  // If the type parameter has a superclass, add that requirement.
  if (auto superclassTy = GenericParam->getSuperclass()) {
    // FIXME: Poor location info.
    addSuperclassRequirement(PA, superclassTy, source);
  }

  return false;
}

bool ArchetypeBuilder::addGenericParameter(GenericTypeParamType *GenericParam) {
  auto name = GenericParam->getName();
  // Trim '$' so that archetypes are more readily discernible from abstract
  // parameters.
  if (name.str().startswith("$"))
    name = Context.getIdentifier(name.str().slice(1, name.str().size()));
  
  PotentialArchetype *PA = addGenericParameter(GenericParam,
                                               nullptr,
                                               name);
  return !PA;
}

bool ArchetypeBuilder::addConformanceRequirement(PotentialArchetype *PAT,
                                                 ProtocolDecl *Proto,
                                                 RequirementSource Source){
  // Add the requirement to the representative.
  auto T = PAT->getRepresentative();

  // Add the requirement, if we haven't done so already.
  if (!T->addConformance(Proto, Source))
    return false;

  RequirementSource InnerSource(RequirementSource::Protocol, Source.getLoc());

  // Add all of the inherited protocol requirements, recursively.
  for (auto InheritedProto : Impl->getInheritedProtocols(Proto)) {
    if (addConformanceRequirement(T, InheritedProto, InnerSource))
      return true;
  }

  // Add requirements for each of the associated types.
  // FIXME: This should use the generic signature, not walk the members.
  for (auto Member : Proto->getMembers()) {
    if (auto AssocType = dyn_cast<AssociatedTypeDecl>(Member)) {
      // Add requirements placed directly on this associated type.
      auto parentName = Proto->getName();
      auto AssocPA = T->getNestedType(AssocType->getName(),
                                      &parentName);
      
      if (AssocPA != T) {
        auto superclassAndConformsTo = Impl->getConformsTo(AssocType);
        if (auto superclassTy = superclassAndConformsTo.first) {
          if (addSuperclassRequirement(AssocPA, superclassTy, InnerSource))
            return true;
        }

        for (auto InheritedProto : superclassAndConformsTo.second) {
          // If it's a recursive requirement, add it directly to the associated
          // archetype.
          if (Proto == InheritedProto) {
            AssocPA->setIsRecursive();
            AssocPA->addConformance(Proto, Source);
            continue;
          }
          
          if (addConformanceRequirement(AssocPA, InheritedProto, InnerSource))
            return true;
        }
      }

      continue;
    }

    // FIXME: Requirement declarations.
  }

  return false;
}

bool ArchetypeBuilder::addSuperclassRequirement(PotentialArchetype *T,
                                                Type Superclass,
                                                RequirementSource Source) {
  // If T already has a superclass, make sure it's the same superclass.
  // FIXME: We should compute the meet here.
  if (T->Superclass) {
    if (!T->Superclass->isEqual(Superclass)) {
      Diags.diagnose(Source.getLoc(),
                     diag::requires_superclass_conflict, T->getName(),
                     T->Superclass, Superclass)
        .highlight(T->SuperclassSource->getLoc());
      return true;
    }

    UpdateRequirementSource update(*T->SuperclassSource, Source);

    return false;
  }

  // Set the superclass.
  T->Superclass = Superclass;
  T->SuperclassSource = Source;

  return false;
}

bool ArchetypeBuilder::addSameTypeRequirementBetweenArchetypes(
       PotentialArchetype *T1,
       PotentialArchetype *T2,
       RequirementSource Source) 
{
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
    Diags.diagnose(Source.getLoc(), diag::requires_generic_params_made_equal,
                   T1->getName(), T2->getName());
    return true;
  }
  
  // Merge any concrete constraints.
  Type concrete1 = T1->ArchetypeOrConcreteType.dyn_cast<Type>();
  Type concrete2 = T2->ArchetypeOrConcreteType.dyn_cast<Type>();
  
  if (concrete1 && concrete2) {
    if (!concrete1->isEqual(concrete2)) {
      Diags.diagnose(Source.getLoc(), diag::requires_same_type_conflict,
                     T1->getName(), concrete1, concrete2);
      return true;
      
    }
  } else if (concrete1) {
    assert(!T2->ArchetypeOrConcreteType.dyn_cast<ArchetypeType*>()
           && "already formed archetype for concrete-constrained parameter");
    T2->ArchetypeOrConcreteType = concrete1;
    T2->SameTypeSource = T1->SameTypeSource;
  } else if (concrete2) {
    assert(!T1->ArchetypeOrConcreteType.dyn_cast<ArchetypeType*>()
           && "already formed archetype for concrete-constrained parameter");
    T1->ArchetypeOrConcreteType = concrete2;
    T1->SameTypeSource = T2->SameTypeSource;
  }
  
  // Make T1 the representative of T2, merging the equivalence classes.
  T2->Representative = T1;
  T2->SameTypeSource = Source;

  // Record this same-type requirement.
  Impl->SameTypeRequirements.push_back({ OrigT1, OrigT2 });

  // FIXME: superclass requirements!

  // Add all of the protocol conformance requirements of T2 to T1.
  for (auto conforms : T2->ConformsTo) {
    T1->addConformance(conforms.first, conforms.second);
  }

  // Recursively merge the associated types of T2 into T1.
  for (auto T2Nested : T2->NestedTypes) {
    auto T1Nested = T1->getNestedType(T2Nested.first);
    if (addSameTypeRequirementBetweenArchetypes(T1Nested,
                                                T2Nested.second.front(),
                                                Source))
      return true;
  }

  return false;
}

bool ArchetypeBuilder::addSameTypeRequirementToConcrete(
       PotentialArchetype *T,
       Type Concrete,
       RequirementSource Source) {
  // Operate on the representative.
  auto OrigT = T;
  T = T->getRepresentative();
  
  assert(!T->ArchetypeOrConcreteType.dyn_cast<ArchetypeType*>()
         && "already formed archetype for concrete-constrained parameter");
  
  // If we've already been bound to a type, we're either done, or we have a
  // problem.
  if (auto oldConcrete = T->ArchetypeOrConcreteType.dyn_cast<Type>()) {
    if (!oldConcrete->isEqual(Concrete)) {
      Diags.diagnose(Source.getLoc(), diag::requires_same_type_conflict,
                     T->getName(), oldConcrete, Concrete);
      return true;
    }
    return false;
  }
  
  // Don't allow a generic parameter to be equivalent to a concrete type,
  // because then we don't actually have a parameter.
  // FIXME: Should we simply allow this?
  if (T->getNestingDepth() == 0) {
    Diags.diagnose(Source.getLoc(), 
                   diag::requires_generic_param_made_equal_to_concrete,
                   T->getName());
    return true;
  }
  
  // Make sure the concrete type fulfills the requirements on the archetype.
  DenseMap<ProtocolDecl *, ProtocolConformance*> conformances;
  for (auto conforms : T->getConformsTo()) {
    auto protocol = conforms.first;
    auto conformance = Impl->conformsToProtocol(Mod, Concrete, protocol);
    if (!conformance) {
      Diags.diagnose(Source.getLoc(),
                     diag::requires_generic_param_same_type_does_not_conform,
                     Concrete, protocol->getName());
      return true;
    }
    conformances[protocol] = conformance;
  }
  
  // Record the requirement.
  T->ArchetypeOrConcreteType = Concrete;
  T->SameTypeSource = Source;

  Impl->SameTypeRequirements.push_back({OrigT, Concrete});
  
  // Recursively resolve the associated types to their concrete types.
  for (auto nested : T->getNestedTypes()) {
    AssociatedTypeDecl *assocType = T->getAssociatedType(Mod, nested.first);
    auto witness = conformances[assocType->getProtocol()]
          ->getTypeWitness(assocType, getLazyResolver());
    addSameTypeRequirementToConcrete(
      nested.second.front(),
      witness.getReplacement()->getDesugaredType(),
      Source);
  }
  
  return false;
}
                                                               
bool ArchetypeBuilder::addSameTypeRequirement(Type Reqt1, Type Reqt2,
                                              RequirementSource Source) {
  // Find the potential archetypes.
  PotentialArchetype *T1 = resolveArchetype(Reqt1);
  PotentialArchetype *T2 = resolveArchetype(Reqt2);
  
  // Require that at least one side of the requirement be a potential archetype.
  if (!T1 && !T2) {
    assert(Source.getLoc().isValid() && "reintroducing invalid requirement");
    Diags.diagnose(Source.getLoc(), diag::requires_no_same_type_archetype);
    return true;
  }
  
  // If both sides of the requirement are open archetypes, combine them.
  if (T1 && T2)
    return addSameTypeRequirementBetweenArchetypes(T1, T2, Source);
  
  // Otherwise, we're binding an open archetype.
  if (T1)
    return addSameTypeRequirementToConcrete(T1, Reqt2, Source);
  return addSameTypeRequirementToConcrete(T2, Reqt1, Source);
}

bool ArchetypeBuilder::addRequirement(const RequirementRepr &Req) {
  switch (Req.getKind()) {
  case RequirementKind::Conformance: {
    PotentialArchetype *PA = resolveArchetype(Req.getSubject());
    if (!PA) {
      // FIXME: Poor location information.
      // FIXME: Delay diagnostic until after type validation?
      Diags.diagnose(Req.getColonLoc(), diag::requires_not_suitable_archetype,
                     0, Req.getSubjectLoc(), 0);
      return true;
    }

    // Check whether this is a supertype requirement.
    RequirementSource source(RequirementSource::Explicit,
                             Req.getConstraintLoc().getSourceRange().Start);
    if (Req.getConstraint()->getClassOrBoundGenericClass()) {
      return addSuperclassRequirement(PA, Req.getConstraint(), source);
    }

    SmallVector<ProtocolDecl *, 4> ConformsTo;
    if (!Req.getConstraint()->isExistentialType(ConformsTo)) {
      // FIXME: Diagnose this failure here, rather than over in type-checking.
      return true;
    }

    // Add each of the protocols.
    for (auto Proto : ConformsTo)
      if (addConformanceRequirement(PA, Proto, source))
        return true;

    return false;
  }

  case RequirementKind::SameType:
    return addSameTypeRequirement(Req.getFirstType(), 
                                  Req.getSecondType(),
                                  RequirementSource(RequirementSource::Explicit,
                                                    Req.getEqualLoc()));

  case RequirementKind::WitnessMarker:
    llvm_unreachable("Value witness marker in requirement");
  }

  llvm_unreachable("Unhandled requirement?");
}

void ArchetypeBuilder::addRequirement(const Requirement &req, 
                                      RequirementSource source) {
  switch (req.getKind()) {
  case RequirementKind::Conformance: {
    PotentialArchetype *pa = resolveArchetype(req.getFirstType());
    assert(pa && "Re-introducing invalid requirement");

    if (req.getSecondType()->getClassOrBoundGenericClass()) {
      addSuperclassRequirement(pa, req.getSecondType(), source);
      return;
    }

    SmallVector<ProtocolDecl *, 4> conformsTo;
    bool existential = req.getSecondType()->isExistentialType(conformsTo);
    assert(existential && "Re-introducing invalid requirement");
    (void)existential;

    // Add each of the protocols.
    for (auto proto : conformsTo) {
      bool invalid = addConformanceRequirement(pa, proto, source);
      assert(!invalid && "Re-introducing invalid requirement");
      (void)invalid;
    }

    return;
  }

  case RequirementKind::SameType:
    addSameTypeRequirement(req.getFirstType(), req.getSecondType(), source);
    return;
    
  case RequirementKind::WitnessMarker:
    return;
  }

  llvm_unreachable("Unhandled requirement?");
}

/// AST walker that infers requirements from type representations.
class ArchetypeBuilder::InferRequirementsWalker : public TypeWalker {
  ArchetypeBuilder &Builder;
  SourceLoc Loc;
  bool HadError = false;

public:
  InferRequirementsWalker(ArchetypeBuilder &builder, SourceLoc loc)
    : Builder(builder), Loc(loc) { }

  bool hadError() const { return HadError; }

  virtual Action walkToTypePost(Type ty) override { 
    auto boundGeneric = ty->getAs<BoundGenericType>();
    if (!boundGeneric)
      return Action::Continue; 

    auto genericSig = boundGeneric->getDecl()->getGenericSignature();
    auto params = genericSig->getGenericParams();
    auto args = boundGeneric->getGenericArgs();

    // Produce substitutions from the generic parameters to the actual
    // arguments.
    TypeSubstitutionMap substitutions;
    for (unsigned i = 0, n = params.size(); i != n; ++i) {
      substitutions[params[i]->getCanonicalType()->castTo<SubstitutableType>()]
        = args[i];
    }

    // Handle the requirements.
    RequirementSource source(RequirementSource::Inferred, Loc);
    for (const auto &req : genericSig->getRequirements()) {
      switch (req.getKind()) {
      case RequirementKind::WitnessMarker:
        break;

      case RequirementKind::SameType: {
        auto firstType = req.getFirstType().subst(&Builder.getModule(), 
                                                  substitutions,
                                                  true,
                                                  Builder.getLazyResolver());
        if (!firstType)
          break;

        auto firstPA = Builder.resolveArchetype(firstType);

        auto secondType = req.getSecondType().subst(&Builder.getModule(), 
                                                    substitutions,
                                                    true,
                                                    Builder.getLazyResolver());
        if (!secondType)
          break;
        auto secondPA = Builder.resolveArchetype(secondType);

        if (firstPA && secondPA) {
          if (Builder.addSameTypeRequirementBetweenArchetypes(firstPA, secondPA,
                                                              source)) {
            HadError = true;
            return Action::Stop;
          }
        } else if (firstPA || secondPA) {
          auto PA = firstPA ? firstPA : secondPA;
          auto concrete = firstPA ? secondType : firstType;
          if (Builder.addSameTypeRequirementToConcrete(PA, concrete, source)) {
            HadError = true;
            return Action::Stop;
          }
        }
        break;
      }

      case RequirementKind::Conformance: {
        auto subjectType = req.getFirstType().subst(&Builder.getModule(),
                                                    substitutions,
                                                    true,
                                                    Builder.getLazyResolver());
        if (!subjectType)
          break;

        auto subjectPA = Builder.resolveArchetype(subjectType);
        if (!subjectPA) {
          break;
        }
        
        if (auto proto = req.getSecondType()->getAs<ProtocolType>()) {
          if (Builder.addConformanceRequirement(subjectPA, proto->getDecl(),
                                                source)) {
            HadError = true;
            return Action::Stop;
          }
        } else if (Builder.addSuperclassRequirement(subjectPA, 
                                                    req.getSecondType(),
                                                    source)) {
          HadError = true;
          return Action::Stop;
        }
        break;
      }
      }
    }

    return Action::Continue;
  }
};

bool ArchetypeBuilder::inferRequirements(TypeLoc type) {
  // FIXME: Crummy source-location information.
  InferRequirementsWalker walker(*this, type.getSourceRange().Start);
  type.getType().walk(walker);
  return walker.hadError();
}

bool ArchetypeBuilder::inferRequirements(Pattern *pattern) {
  // FIXME: Crummy source-location information.
  InferRequirementsWalker walker(*this, pattern->getSourceRange().Start);
  pattern->getType().walk(walker);
  return walker.hadError();
}

ArchetypeType *
ArchetypeBuilder::getArchetype(GenericTypeParamDecl *GenericParam) {
  auto known = Impl->PotentialArchetypes.find(
                 GenericTypeParamKey::forDecl(GenericParam));
  if (known == Impl->PotentialArchetypes.end())
    return nullptr;

  return known->second->getType(*this).dyn_cast<ArchetypeType *>();
}

ArrayRef<ArchetypeType *> ArchetypeBuilder::getAllArchetypes() {
  // This should be kept in sync with GenericParamList::deriveAllArchetypes().
  if (Impl->AllArchetypes.empty()) {
    // Collect the primary archetypes first.
    llvm::SmallPtrSet<ArchetypeType *, 8> KnownArchetypes;
    for (auto PA : Impl->RootPotentialArchetypes) {
      if (PA->isPrimary()) {
        auto Archetype = PA->getType(*this).get<ArchetypeType *>();
        assert(Archetype->isPrimary() && "isPrimary mismatch");
        if (KnownArchetypes.insert(Archetype))
          Impl->AllArchetypes.push_back(Archetype);
      }
    }

    // Collect all of the remaining archetypes.
    for (auto PA : Impl->RootPotentialArchetypes) {
      if (!PA->isConcreteType()) {
        auto Archetype = PA->getType(*this).get<ArchetypeType *>();
        GenericParamList::addNestedArchetypes(Archetype, KnownArchetypes,
                                              Impl->AllArchetypes);
      }
    }
  }

  return Impl->AllArchetypes;
}

ArrayRef<ArchetypeBuilder::SameTypeRequirement>
ArchetypeBuilder::getSameTypeRequirements() const {
  return Impl->SameTypeRequirements;
}

template<typename F>
void ArchetypeBuilder::visitPotentialArchetypes(F f) {
  // Stack containing all of the potential archetypes to visit.
  SmallVector<PotentialArchetype *, 4> stack;
  llvm::SmallPtrSet<PotentialArchetype *, 4> visited;

  // Add top-level potential archetypes to the stack.
  for (const auto &pa : Impl->PotentialArchetypes) {
    if (visited.insert(pa.second))
      stack.push_back(pa.second);
  }

  // Visit all of the potential archetypes.
  while (!stack.empty()) {
    PotentialArchetype *pa = stack.back();
    stack.pop_back();
    f(pa);

    // Visit nested potential archetypes.
    for (const auto &nested : pa->getNestedTypes()) {
      for (auto nestedPA : nested.second) {
        if (visited.insert(nestedPA)) {
          stack.push_back(nestedPA);
        }
      }
    }
  }

}

template<typename F>
void ArchetypeBuilder::enumerateRequirements(F f) {
  visitPotentialArchetypes([&](PotentialArchetype *archetype) {
    // If this is not the representative, produce a same-type
    // constraint to the representative.
    if (archetype->getRepresentative() != archetype) {
      f(RequirementKind::SameType, archetype, 
        archetype->getRepresentative()->getDependentType(*this),
        archetype->getSameTypeSource());
      return;
    }

    // If we have a concrete type, produce a same-type requirement.
    if (archetype->isConcreteType()) {
      Type concreteType = archetype->getType(*this).get<Type>();
      f(RequirementKind::SameType, archetype, concreteType,
        archetype->getSameTypeSource());
      return;
    }

    // If we have a superclass, produce a superclass requirement
    // (FIXME: Currently described as a conformance requirement)
    if (Type superclass = archetype->getSuperclass()) {
      f(RequirementKind::Conformance, archetype, superclass,
        archetype->getSuperclassSource());
    }

    // Enumerate conformance requirements.
    for (const auto &conforms : archetype->getConformsTo()) {
      f(RequirementKind::Conformance, archetype, 
        conforms.first->getDeclaredInterfaceType(),
        conforms.second);
    }
  });
}

void ArchetypeBuilder::dump() {
  dump(llvm::errs());
}

void ArchetypeBuilder::dump(llvm::raw_ostream &out) {
  out << "Requirements:";
  enumerateRequirements([&](RequirementKind kind, 
                            PotentialArchetype *archetype,
                            Type type,
                            RequirementSource source) {
    out << "\n  ";
    switch (kind) {
    case RequirementKind::Conformance:
      out << archetype->getDebugName() << " : " 
          << type.getString() << " [";
      source.dump(out, &Context.SourceMgr);
      out << "]";
      break;

    case RequirementKind::SameType:
      out << archetype->getDebugName() << " == " << type.getString() << " [";
      source.dump(out, &Context.SourceMgr);
      out << "]";
      break;

    case RequirementKind::WitnessMarker:
      break;
    }
  });
  out << "\n";
}

Type ArchetypeBuilder::mapTypeIntoContext(DeclContext *dc, Type type) {
  // If the type is not dependent, there's nothing to map.
  if (!type->isDependentType())
    return type;

  auto genericParams = dc->getGenericParamsOfContext();
  assert(genericParams && "Missing generic parameters for dependent context");
  
  return mapTypeIntoContext(dc->getParentModule(), genericParams, type);
}

Type ArchetypeBuilder::mapTypeIntoContext(Module *M,
                                          GenericParamList *genericParams,
                                          Type type) {
  // If the type is not dependent, or we have no generic params, there's nothing
  // to map.
  if (!genericParams || !type->isDependentType())
    return type;

  unsigned genericParamsDepth = genericParams->getDepth();
  return type.transform([&](Type type) -> Type {
    // Map a generic parameter type to its archetype.
    if (auto gpType = type->getAs<GenericTypeParamType>()) {
      auto index = gpType->getIndex();
      unsigned depth = gpType->getDepth();

      // Skip down to the generic parameter list that houses the corresponding
      // generic parameter.
      auto myGenericParams = genericParams;
      assert(genericParamsDepth >= depth);
      unsigned skipLevels = genericParamsDepth - depth;
      while (skipLevels > 0) {
        myGenericParams = genericParams->getOuterParameters();
        assert(myGenericParams && "Wrong number of levels?");
        --skipLevels;
      }

      // Return the archetype.
      // FIXME: Use the allArchetypes vector instead of the generic param if
      // available because of cross-module archetype serialization woes.
      if (!myGenericParams->getAllArchetypes().empty())
        return myGenericParams->getPrimaryArchetypes()[index];

      // During type-checking, we may try to mapTypeInContext before
      // AllArchetypes has been built, so fall back to the generic params.
      return myGenericParams->getParams()[index]->getArchetype();
    }

    // Map a dependent member to the corresponding nested archetype.
    if (auto dependentMember = type->getAs<DependentMemberType>()) {
      auto base = mapTypeIntoContext(M, genericParams,
                                     dependentMember->getBase());
      return dependentMember->substBaseType(M, base);
    }

    return type;
  });
}

bool ArchetypeBuilder::addGenericSignature(GenericSignature *sig) {
  if (!sig) return false;
  for (auto param : sig->getGenericParams()) {
    if (addGenericParameter(param))
      return true;
  }

  RequirementSource source(RequirementSource::OuterScope, SourceLoc());
  for (auto &reqt : sig->getRequirements()) {
    addRequirement(reqt, source);
  }
  return false;
}

/// A type transformer that replaces dependent types with contextual types
/// from an ArchetypeBuilder.
static Type substDependentTypes(ArchetypeBuilder &Archetypes, Type ty) {
  auto &M = Archetypes.getModule();
  // Resolve generic type parameters to their types.
  if (auto genType = ty->getAs<GenericTypeParamType>()) {
    if (auto potentialArchetype = Archetypes.resolveArchetype(genType)) {
      return ArchetypeType::getNestedTypeValue(
               potentialArchetype->getType(Archetypes));
    }

    return genType;
  }
  
  // Resolve dependent member types.
  if (auto depType = ty->getAs<DependentMemberType>()) {
    // See if the type directly references an associated archetype.
    if (auto potentialArchetype = Archetypes.resolveArchetype(depType)) {
      return ArchetypeType::getNestedTypeValue(
               potentialArchetype->getType(Archetypes));
    }
    
    // If not, resolve the base type, then look up the associated type in
    // a conformance.
    Type base = depType->getBase().transform([&](Type t) -> Type {
      return substDependentTypes(Archetypes, t);
    });
    assert(!base->isDependentType());
    
    if (auto baseArch = base->getAs<ArchetypeType>()) {
      return baseArch->getNestedTypeValue(depType->getName());
    }
    
    auto assocType = depType->getAssocType();
    auto proto = assocType->getProtocol();
    auto conformance = M.lookupConformance(base, proto,
                                           Archetypes.getLazyResolver());
    switch (conformance.getInt()) {
    case ConformanceKind::DoesNotConform:
    case ConformanceKind::UncheckedConforms:
      llvm_unreachable("substituted base does not conform to protocol?!");
        
    case ConformanceKind::Conforms:
      return conformance.getPointer()->getTypeWitness(
               assocType, Archetypes.getLazyResolver()).getReplacement();
    }
  }
  
  return ty;
}

Type ArchetypeBuilder::substDependentType(Type type) {
  return type.transform([&](Type t) -> Type {
    return substDependentTypes(*this, t);
  });
}
