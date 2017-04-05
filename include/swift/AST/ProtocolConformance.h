//===--- ProtocolConformance.h - AST Protocol Conformance -------*- C++ -*-===//
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
// This file defines the protocol conformance data structures.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_PROTOCOLCONFORMANCE_H
#define SWIFT_AST_PROTOCOLCONFORMANCE_H

#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/SubstitutionList.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/AST/Witness.h"
#include "swift/Basic/Compiler.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/SmallPtrSet.h"
#include <utility>

namespace swift {

class ASTContext;
class DiagnosticEngine;
class GenericParamList;
class NormalProtocolConformance;
class ProtocolConformance;
class ModuleDecl;
class SubstitutableType;
enum class AllocationArena;

/// \brief Type substitution mapping from substitutable types to their
/// replacements.
typedef llvm::DenseMap<SubstitutableType *, Type> TypeSubstitutionMap;

/// Map from non-type requirements to the corresponding conformance witnesses.
typedef llvm::DenseMap<ValueDecl *, Witness> WitnessMap;

/// Map from associated type requirements to the corresponding type and
/// the type declaration that was used to satisfy the requirement.
typedef llvm::DenseMap<AssociatedTypeDecl *, std::pair<Type, TypeDecl*>>
  TypeWitnessMap;

/// Describes the kind of protocol conformance structure used to encode
/// conformance.
enum class ProtocolConformanceKind {
  /// "Normal" conformance of a (possibly generic) nominal type, which
  /// contains complete mappings.
  Normal,
  /// Conformance for a specialization of a generic type, which projects the
  /// underlying generic conformance.
  Specialized,
  /// Conformance of a generic class type projected through one of its
  /// superclass's conformances.
  Inherited
};

/// Describes the state of a protocol conformance, which may be complete,
/// incomplete, or currently being checked.
enum class ProtocolConformanceState {
  /// The conformance has been fully checked.
  Complete,
  /// The conformance is known but is not yet complete.
  Incomplete,
  /// The conformance's type witnesses are currently being resolved.
  CheckingTypeWitnesses,
  /// The conformance is being checked.
  Checking,
};

/// \brief Describes how a particular type conforms to a given protocol,
/// providing the mapping from the protocol members to the type (or extension)
/// members that provide the functionality for the concrete type.
///
/// ProtocolConformance is an abstract base class, implemented by subclasses
/// for the various kinds of conformance (normal, specialized, inherited).
class alignas(1 << DeclAlignInBits) ProtocolConformance {
  /// The kind of protocol conformance.
  ProtocolConformanceKind Kind;

  /// \brief The type that conforms to the protocol, in the context of the
  /// conformance definition.
  Type ConformingType;
  
  /// \brief The interface type that conforms to the protocol.
  Type ConformingInterfaceType;

protected:
  ProtocolConformance(ProtocolConformanceKind kind, Type conformingType,
                      Type conformingInterfaceType)
    : Kind(kind), ConformingType(conformingType),
      ConformingInterfaceType(conformingInterfaceType) { }

public:
  /// Determine the kind of protocol conformance.
  ProtocolConformanceKind getKind() const { return Kind; }

  /// Get the conforming type.
  Type getType() const { return ConformingType; }

  /// Get the conforming interface type.
  Type getInterfaceType() const { return ConformingInterfaceType; }
  
  /// Get the protocol being conformed to.
  ProtocolDecl *getProtocol() const;

  /// Get the declaration context that contains the conforming extension or
  /// nominal type declaration.
  DeclContext *getDeclContext() const;

  /// Retrieve the state of this conformance.
  ProtocolConformanceState getState() const;

  /// Determine whether this conformance is complete.
  bool isComplete() const {
    return getState() == ProtocolConformanceState::Complete;
  }

  /// Determine whether this conformance is invalid.
  bool isInvalid() const;

  /// Determine whether this conformance is incomplete.
  bool isIncomplete() const {
    return getState() == ProtocolConformanceState::Incomplete ||
           getState() == ProtocolConformanceState::CheckingTypeWitnesses ||
           getState() == ProtocolConformanceState::Checking;
  }

  /// Return true if the conformance has a witness for the given associated
  /// type.
  bool hasTypeWitness(AssociatedTypeDecl *assocType,
                      LazyResolver *resolver = nullptr) const;

  /// Retrieve the type witness for the given associated type.
  Type getTypeWitness(AssociatedTypeDecl *assocType,
                      LazyResolver *resolver) const;

  /// Retrieve the type witness and type decl (if one exists)
  /// for the given associated type.
  std::pair<Type, TypeDecl *>
  getTypeWitnessAndDecl(AssociatedTypeDecl *assocType,
                        LazyResolver *resolver) const;

  /// Apply the given function object to each type witness within this
  /// protocol conformance.
  ///
  /// The function object should accept an \c AssociatedTypeDecl* for the
  /// requirement followed by the \c Type for the witness and a
  /// (possibly null) \c TypeDecl* that explicitly declared the type.
  /// It should return true to indicate an early exit.
  ///
  /// \returns true if the function ever returned true
  template<typename F>
  bool forEachTypeWitness(LazyResolver *resolver, F f) const {
    const ProtocolDecl *protocol = getProtocol();
    for (auto req : protocol->getMembers()) {
      auto assocTypeReq = dyn_cast<AssociatedTypeDecl>(req);
      if (!assocTypeReq || req->isInvalid())
        continue;

      // If we don't have and cannot resolve witnesses, skip it.
      if (!resolver && !hasTypeWitness(assocTypeReq))
        continue;

      const auto &TWInfo = getTypeWitnessAndDecl(assocTypeReq, resolver);
      if (f(assocTypeReq, TWInfo.first, TWInfo.second))
        return true;
    }

    return false;
  }

  /// Retrieve the non-type witness for the given requirement.
  Witness getWitness(ValueDecl *requirement, LazyResolver *resolver) const;

private:
  /// Determine whether we have a witness for the given requirement.
  bool hasWitness(ValueDecl *requirement) const;

public:
  /// Apply the given function object to each value witness within this
  /// protocol conformance.
  ///
  /// The function object should accept a \c ValueDecl* for the requirement
  /// followed by the \c Witness for the witness. Note that a generic
  /// witness will only be specialized if the conformance came from the current
  /// file.
  template<typename F>
  void forEachValueWitness(LazyResolver *resolver, F f) const {
    const ProtocolDecl *protocol = getProtocol();
    for (auto req : protocol->getMembers()) {
      auto valueReq = dyn_cast<ValueDecl>(req);
      if (!valueReq || isa<AssociatedTypeDecl>(valueReq) ||
          valueReq->isInvalid())
        continue;

      if (!valueReq->isProtocolRequirement())
        continue;

      // If we don't have and cannot resolve witnesses, skip it.
      if (!resolver && !hasWitness(valueReq))
        continue;

      f(valueReq, getWitness(valueReq, resolver));
    }
  }

  /// Retrieve the protocol conformance for the inherited protocol.
  ProtocolConformance *getInheritedConformance(ProtocolDecl *protocol) const;

  /// Given a dependent type expressed in terms of the self parameter,
  /// map it into the context of this conformance.
  Type getAssociatedType(Type assocType,
                         LazyResolver *resolver = nullptr) const;

  /// Given that the requirement signature of the protocol directly states
  /// that the given dependent type must conform to the given protocol,
  /// return its associated conformance.
  ProtocolConformanceRef
  getAssociatedConformance(Type assocType, ProtocolDecl *protocol,
                           LazyResolver *resolver = nullptr) const;
 
  /// Get the generic parameters open on the conforming type.
  GenericEnvironment *getGenericEnvironment() const;

  /// Get the generic signature containing the parameters open on the conforming
  /// interface type.
  GenericSignature *getGenericSignature() const;

  /// Get the underlying normal conformance.
  const NormalProtocolConformance *getRootNormalConformance() const;

  /// Get the underlying normal conformance.
  NormalProtocolConformance *getRootNormalConformance() {
    return const_cast<NormalProtocolConformance *>(
             const_cast<const ProtocolConformance *>(this)
               ->getRootNormalConformance());
  }

  /// Determine whether this protocol conformance is visible from the
  /// given declaration context.
  bool isVisibleFrom(const DeclContext *dc) const;

  /// Determine whether the witness for the given requirement
  /// is either the default definition or was otherwise deduced.
  bool usesDefaultDefinition(AssociatedTypeDecl *requirement) const;
  
  // Make vanilla new/delete illegal for protocol conformances.
  void *operator new(size_t bytes) = delete;
  void operator delete(void *data) SWIFT_DELETE_OPERATOR_DELETED;

  // Only allow allocation of protocol conformances using the allocator in
  // ASTContext or by doing a placement new.
  void *operator new(size_t bytes, ASTContext &context,
                     AllocationArena arena,
                     unsigned alignment = alignof(ProtocolConformance));
  void *operator new(size_t bytes, void *mem) {
    assert(mem);
    return mem;
  }
  
  /// Print a parseable and human-readable description of the identifying
  /// information of the protocol conformance.
  void printName(raw_ostream &os,
                 const PrintOptions &PO = PrintOptions()) const;
  
  /// True if the conformance is for a property behavior instantiation.
  bool isBehaviorConformance() const;
  
  /// Get the property declaration for a behavior conformance, if this is one.
  AbstractStorageDecl *getBehaviorDecl() const;

  /// Substitute the conforming type and produce a ProtocolConformance that
  /// applies to the substituted type.
  ProtocolConformance *subst(Type substType,
                             TypeSubstitutionFn subs,
                             LookupConformanceFn conformances) const;
  
  void dump() const;
  void dump(llvm::raw_ostream &out, unsigned indent = 0) const;
};

/// Normal protocol conformance, which involves mapping each of the protocol
/// requirements to a witness.
///
/// Normal protocol conformance is used for the explicit conformances placed on
/// nominal types and extensions. For example:
///
/// \code
/// protocol P { func foo() }
/// struct A : P { func foo() { } }
/// class B<T> : P { func foo() { } }
/// \endcode
///
/// Here, there is a normal protocol conformance for both \c A and \c B<T>,
/// providing the witnesses \c A.foo and \c B<T>.foo, respectively, for the
/// requirement \c foo.
class NormalProtocolConformance : public ProtocolConformance,
                                  public llvm::FoldingSetNode
{
  /// \brief The protocol being conformed to and its current state.
  llvm::PointerIntPair<ProtocolDecl *, 2, ProtocolConformanceState>
    ProtocolAndState;

  /// The location of this protocol conformance in the source.
  SourceLoc Loc;

  using Context = llvm::PointerUnion<DeclContext *, AbstractStorageDecl *>;

  /// The declaration context containing the ExtensionDecl or
  /// NominalTypeDecl that declared the conformance, or the VarDecl whose
  /// behavior this conformance represents.
  ///
  /// Also stores the "invalid" bit.
  llvm::PointerIntPair<Context, 1, bool> ContextAndInvalid;

  /// \brief The mapping of individual requirements in the protocol over to
  /// the declarations that satisfy those requirements.
  mutable WitnessMap Mapping;

  /// The mapping from associated type requirements to their types.
  mutable TypeWitnessMap TypeWitnesses;

  /// Conformances that satisfy each of conformance requirements of the
  /// requirement signature of the protocol.
  ArrayRef<ProtocolConformanceRef> SignatureConformances;

  LazyMemberLoader *Resolver = nullptr;
  uint64_t ResolverContextData;

  friend class ASTContext;

  NormalProtocolConformance(Type conformingType, ProtocolDecl *protocol,
                            SourceLoc loc, DeclContext *dc,
                            ProtocolConformanceState state)
    : ProtocolConformance(ProtocolConformanceKind::Normal, conformingType,
                          // FIXME: interface type should be passed in
                          dc->getDeclaredInterfaceType()),
      ProtocolAndState(protocol, state), Loc(loc), ContextAndInvalid(dc, false)
  {
  }

  NormalProtocolConformance(Type conformingType,
                            Type conformingInterfaceType,
                            ProtocolDecl *protocol,
                            SourceLoc loc, AbstractStorageDecl *behaviorStorage,
                            ProtocolConformanceState state)
    : ProtocolConformance(ProtocolConformanceKind::Normal, conformingType,
                          // FIXME: interface type should be passed in
                          conformingInterfaceType),
      ProtocolAndState(protocol, state), Loc(loc),
      ContextAndInvalid(behaviorStorage, false)
  {
  }

  void resolveLazyInfo() const;

public:
  /// Get the protocol being conformed to.
  ProtocolDecl *getProtocol() const { return ProtocolAndState.getPointer(); }

  /// Retrieve the location of this 
  SourceLoc getLoc() const { return Loc; }

  /// Get the declaration context that contains the conforming extension or
  /// nominal type declaration.
  DeclContext *getDeclContext() const {
    auto context = ContextAndInvalid.getPointer();
    if (auto DC = context.dyn_cast<DeclContext *>()) {
      return DC;
    } else {
      return context.get<AbstractStorageDecl *>()->getDeclContext();
    }
  }

  /// Retrieve the state of this conformance.
  ProtocolConformanceState getState() const {
    return ProtocolAndState.getInt();
  }

  /// Set the state of this conformance.
  void setState(ProtocolConformanceState state) {
    ProtocolAndState.setInt(state);
  }

  /// Determine whether this conformance is invalid.
  bool isInvalid() const {
    return ContextAndInvalid.getInt();
  }
  
  /// Mark this conformance as invalid.
  void setInvalid() {
    ContextAndInvalid.setInt(true);
  }

  /// Determine whether this conformance is lazily resolved.
  ///
  /// This only matters to the AST verifier.
  bool isLazilyResolved() const { return Resolver != nullptr; }

  /// True if the conformance describes a property behavior.
  bool isBehaviorConformance() const {
    return ContextAndInvalid.getPointer().is<AbstractStorageDecl *>();
  }
  
  /// Return the declaration using the behavior for this conformance, or null
  /// if this isn't a behavior conformance.
  AbstractStorageDecl *getBehaviorDecl() const {
    return ContextAndInvalid.getPointer().dyn_cast<AbstractStorageDecl *>();
  }
  
  /// Retrieve the type witness and type decl (if one exists)
  /// for the given associated type.
  std::pair<Type, TypeDecl *>
  getTypeWitnessAndDecl(AssociatedTypeDecl *assocType,
                        LazyResolver *resolver) const;

  /// Determine whether the protocol conformance has a type witness for the
  /// given associated type.
  bool hasTypeWitness(AssociatedTypeDecl *assocType,
                      LazyResolver *resolver = nullptr) const;

  /// Set the type witness for the given associated type.
  /// \param typeDecl the type decl the witness type came from, if one exists.
  void setTypeWitness(AssociatedTypeDecl *assocType, Type type,
                      TypeDecl *typeDecl) const;

  /// Given that the requirement signature of the protocol directly states
  /// that the given dependent type must conform to the given protocol,
  /// return its associated conformance.
  ProtocolConformanceRef
  getAssociatedConformance(Type assocType, ProtocolDecl *protocol,
                           LazyResolver *resolver = nullptr) const;

  /// Retrieve the value witness corresponding to the given requirement.
  ///
  /// Note that a generic witness will only be specialized if the conformance
  /// came from the current file.
  ///
  /// FIXME: The 'only specialized if from the same file' bit is awful.
  Witness getWitness(ValueDecl *requirement, LazyResolver *resolver) const;

  /// Determine whether the protocol conformance has a witness for the given
  /// requirement.
  bool hasWitness(ValueDecl *requirement) const {
    if (Resolver)
      resolveLazyInfo();
    return Mapping.count(requirement) > 0;
  }

  /// Set the witness for the given requirement.
  void setWitness(ValueDecl *requirement, Witness witness) const;

  /// Retrieve the protocol conformances that satisfy the requirements of the
  /// protocol, which line up with the conformance constraints in the
  /// protocol's requirement signature.
  ArrayRef<ProtocolConformanceRef> getSignatureConformances() const {
    return SignatureConformances;
  }

  /// Copy the given protocol conformances for the requirement signature into
  /// the normal conformance.
  void setSignatureConformances(ArrayRef<ProtocolConformanceRef> conformances);

  /// Determine whether the witness for the given type requirement
  /// is the default definition.
  bool usesDefaultDefinition(AssociatedTypeDecl *requirement) const {
    return getTypeWitnessAndDecl(requirement, nullptr)
        .second->isImplicit();
  }

  void setLazyLoader(LazyMemberLoader *resolver, uint64_t contextData);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getProtocol(), getDeclContext());
  }

  static void Profile(llvm::FoldingSetNodeID &ID, ProtocolDecl *protocol,
                      DeclContext *dc) {
    ID.AddPointer(protocol);
    ID.AddPointer(dc);
  }

  static bool classof(const ProtocolConformance *conformance) {
    return conformance->getKind() == ProtocolConformanceKind::Normal;
  }
};

/// Specialized protocol conformance, which projects a generic protocol
/// conformance to one of the specializations of the generic type.
///
/// For example:
/// \code
/// protocol P { func foo() }
/// class A<T> : P { func foo() { } }
/// \endcode
///
/// \c A<T> conforms to \c P via normal protocol conformance. Any specialization
/// of \c A<T> conforms to \c P via a specialized protocol conformance. For
/// example, \c A<Int> conforms to \c P via a specialized protocol conformance
/// that refers to the normal protocol conformance \c A<T> to \c P with the
/// substitution \c T -> \c Int.
class SpecializedProtocolConformance : public ProtocolConformance,
                                       public llvm::FoldingSetNode {
  /// The generic conformance from which this conformance was derived.
  ProtocolConformance *GenericConformance;

  /// The substitutions applied to the generic conformance to produce this
  /// conformance.
  SubstitutionList GenericSubstitutions;

  /// The mapping from associated type requirements to their substitutions.
  ///
  /// This mapping is lazily produced by specializing the underlying,
  /// generic conformance.
  mutable TypeWitnessMap TypeWitnesses;

  friend class ASTContext;

  SpecializedProtocolConformance(Type conformingType,
                                 ProtocolConformance *genericConformance,
                                 SubstitutionList substitutions);

public:
  /// Get the generic conformance from which this conformance was derived,
  /// if there is one.
  ProtocolConformance *getGenericConformance() const {
    return GenericConformance;
  }

  /// Get the substitutions used to produce this specialized conformance from
  /// the generic conformance.
  SubstitutionList getGenericSubstitutions() const {
    return GenericSubstitutions;
  }

  /// Get the protocol being conformed to.
  ProtocolDecl *getProtocol() const {
    return GenericConformance->getProtocol();
  }

  /// Get the declaration context that contains the conforming extension or
  /// nominal type declaration.
  DeclContext *getDeclContext() const {
    return GenericConformance->getDeclContext();
  }

  /// Retrieve the state of this conformance.
  ProtocolConformanceState getState() const {
    return GenericConformance->getState();
  }

  bool hasTypeWitness(AssociatedTypeDecl *assocType,
                      LazyResolver *resolver = nullptr) const;

  /// Retrieve the type witness and type decl (if one exists)
  /// for the given associated type.
  std::pair<Type, TypeDecl *>
  getTypeWitnessAndDecl(AssociatedTypeDecl *assocType,
                        LazyResolver *resolver) const;

  /// Retrieve the value witness corresponding to the given requirement.
  Witness getWitness(ValueDecl *requirement, LazyResolver *resolver) const;


  /// Given that the requirement signature of the protocol directly states
  /// that the given dependent type must conform to the given protocol,
  /// return its associated conformance.
  ProtocolConformanceRef
  getAssociatedConformance(Type assocType, ProtocolDecl *protocol,
                           LazyResolver *resolver = nullptr) const;

  /// Determine whether the witness for the given requirement
  /// is either the default definition or was otherwise deduced.
  bool usesDefaultDefinition(AssociatedTypeDecl *requirement) const {
    return GenericConformance->usesDefaultDefinition(requirement);
  }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getType(), getGenericConformance());
  }

  static void Profile(llvm::FoldingSetNodeID &ID, Type type,
                      ProtocolConformance *genericConformance) {
    // FIXME: Consider profiling substitutions here. They could differ in
    // some crazy cases that also require major diagnostic work, where the
    // substitutions involve conformances of the same type to the same
    // protocol drawn from different imported modules.
    ID.AddPointer(type.getPointer());
    ID.AddPointer(genericConformance);
  }

  static bool classof(const ProtocolConformance *conformance) {
    return conformance->getKind() == ProtocolConformanceKind::Specialized;
  }
};

/// Inherited protocol conformance, which projects the conformance of a
/// superclass to its subclasses.
///
/// An example:
/// \code
/// protocol P { func foo() }
/// class A : P { func foo() { } }
/// class B : A { }
/// \endcode
///
/// \c A conforms to \c P via normal protocol conformance. The subclass \c B
/// of \c A conforms to \c P via an inherited protocol conformance.
class InheritedProtocolConformance : public ProtocolConformance,
                                     public llvm::FoldingSetNode {
  /// The conformance inherited from the superclass.
  ProtocolConformance *InheritedConformance;

  friend class ASTContext;

  InheritedProtocolConformance(Type conformingType,
                               ProtocolConformance *inheritedConformance)
    : ProtocolConformance(ProtocolConformanceKind::Inherited, conformingType,
            // FIXME: interface type should be passed in
            inheritedConformance->getDeclContext()->getDeclaredInterfaceType()),
      InheritedConformance(inheritedConformance)
  {
  }

public:
  /// Retrieve the conformance for the inherited type.
  ProtocolConformance *getInheritedConformance() const {
    return InheritedConformance;
  }

  /// Get the protocol being conformed to.
  ProtocolDecl *getProtocol() const {
    return InheritedConformance->getProtocol();
  }

  /// Get the declaration context that contains the conforming extension or
  /// nominal type declaration.
  DeclContext *getDeclContext() const {
    auto bgc = getType()->getClassOrBoundGenericClass();

    // In some cases, we may not have a BGC handy, in which case we should
    // delegate to the inherited conformance for the decl context.
    return bgc ? bgc : InheritedConformance->getDeclContext();
  }

  /// Retrieve the state of this conformance.
  ProtocolConformanceState getState() const {
    return InheritedConformance->getState();
  }

  bool hasTypeWitness(AssociatedTypeDecl *assocType,
                      LazyResolver *resolver = nullptr) const {
    return InheritedConformance->hasTypeWitness(assocType, resolver);
  }

  /// Retrieve the type witness and type decl (if one exists)
  /// for the given associated type.
  std::pair<Type, TypeDecl *>
  getTypeWitnessAndDecl(AssociatedTypeDecl *assocType,
                        LazyResolver *resolver) const {
    return InheritedConformance->getTypeWitnessAndDecl(assocType, resolver);
  }

  /// Retrieve the value witness corresponding to the given requirement.
  Witness getWitness(ValueDecl *requirement, LazyResolver *resolver) const {
    // FIXME: Substitute!
    return InheritedConformance->getWitness(requirement, resolver);
  }

  /// Given that the requirement signature of the protocol directly states
  /// that the given dependent type must conform to the given protocol,
  /// return its associated conformance.
  ProtocolConformanceRef
  getAssociatedConformance(Type assocType, ProtocolDecl *protocol,
                           LazyResolver *resolver = nullptr) const;

  /// Determine whether the witness for the given requirement
  /// is either the default definition or was otherwise deduced.
  bool usesDefaultDefinition(AssociatedTypeDecl *requirement) const {
    return InheritedConformance->usesDefaultDefinition(requirement);
  }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getType(), getInheritedConformance());
  }

  static void Profile(llvm::FoldingSetNodeID &ID, Type type,
                      ProtocolConformance *inheritedConformance) {
    ID.AddPointer(type->getCanonicalType().getPointer());
    ID.AddPointer(inheritedConformance);
  }

  static bool classof(const ProtocolConformance *conformance) {
    return conformance->getKind() == ProtocolConformanceKind::Inherited;
  }
};

inline bool ProtocolConformance::isInvalid() const {
  return getRootNormalConformance()->isInvalid();
}

inline bool ProtocolConformance::hasWitness(ValueDecl *requirement) const {
  return getRootNormalConformance()->hasWitness(requirement);
}

} // end namespace swift

#endif // LLVM_SWIFT_AST_PROTOCOLCONFORMANCE_H
