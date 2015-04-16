//===--- ProtocolConformance.h - AST Protocol Conformance -------*- C++ -*-===//
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
// This file defines the protocol conformance data structures.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_PROTOCOLCONFORMANCE_H
#define SWIFT_AST_PROTOCOLCONFORMANCE_H

#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Substitution.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
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
class Module;
enum class AllocationArena;
  
/// \brief Type substitution mapping from substitutable types to their
/// replacements.
typedef llvm::DenseMap<TypeBase *, Type> TypeSubstitutionMap;

/// Map from non-type requirements to the corresponding conformance witnesses.
typedef llvm::DenseMap<ValueDecl *, ConcreteDeclRef> WitnessMap;

/// Map from associated type requirements to the corresponding substitution,
/// which captures the replacement type along with any conformances it requires.
typedef llvm::DenseMap<AssociatedTypeDecl *, std::pair<Substitution, TypeDecl*>>
  TypeWitnessMap;

/// Map from a directly-inherited protocol to its corresponding protocol
/// conformance.
typedef llvm::DenseMap<ProtocolDecl *, ProtocolConformance *>
  InheritedConformanceMap;

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
/// incomplete, or invalid.
enum class ProtocolConformanceState {
  /// The conformance has been fully checked and is complete and well-formed.
  Complete,
  /// The conformance is known but is not yet complete.
  Incomplete,
  /// The conformance is being checked.
  Checking,
  /// The conformance has been found to be invalid and should not be
  /// used.
  Invalid
};

/// \brief Describes how a particular type conforms to a given protocol,
/// providing the mapping from the protocol members to the type (or extension)
/// members that provide the functionality for the concrete type.
///
/// ProtocolConformance is an abstract base class, implemented by subclasses
/// for the various kinds of conformance (normal, specialized, inherited).
class ProtocolConformance {
  /// The kind of protocol conformance.
  ProtocolConformanceKind Kind;

  /// \brief The type that conforms to the protocol.
  Type ConformingType;

protected:
  ProtocolConformance(ProtocolConformanceKind kind, Type conformingType)
    : Kind(kind), ConformingType(conformingType) { }

public:
  /// Determine the kind of protocol conformance.
  ProtocolConformanceKind getKind() const { return Kind; }

  /// Get the conforming type.
  Type getType() const { return ConformingType; }

  /// Get the conforming interface type.
  Type getInterfaceType() const;
  
  /// Get the protocol being conformed to.
  ProtocolDecl *getProtocol() const;

  /// Get the declaration context that contains the conforming extension or
  /// nominal type declaration.
  DeclContext *getDeclContext() const;

  /// Retrieve the state of this conformance.
  ProtocolConformanceState getState() const;

  /// Determine whether this conformance is complete and well-formed.
  bool isComplete() const {
    return getState() == ProtocolConformanceState::Complete;
  }

  /// Determine whether this conformance is invalid.
  bool isInvalid() const {
    return getState() == ProtocolConformanceState::Invalid;
  }

  /// Determine whether this conformance is incomplete.
  bool isIncomplete() const {
    return getState() == ProtocolConformanceState::Incomplete ||
           getState() == ProtocolConformanceState::Checking;
  }

  /// Return true if the conformance has a witness for the given associated
  /// type.
  bool hasTypeWitness(AssociatedTypeDecl *assocType,
                      LazyResolver *resolver = nullptr) const;

  /// Retrieve the type witness substitution for the given associated type.
  const Substitution &getTypeWitness(AssociatedTypeDecl *assocType,
                                     LazyResolver *resolver) const;

  /// Retrieve the type witness substitution and type decl (if one exists)
  /// for the given associated type.
  std::pair<const Substitution &, TypeDecl *>
  getTypeWitnessSubstAndDecl(AssociatedTypeDecl *assocType,
                             LazyResolver *resolver) const;

  static Type
  getTypeWitnessByName(Type type,
                       ProtocolConformance *conformance,
                       Identifier name,
                       LazyResolver *resolver);

  /// Apply the given function object to each type witness within this
  /// protocol conformance.
  ///
  /// The function object should accept an \c AssociatedTypeDecl* for the
  /// requirement followed by the \c Substitution for the witness. It should
  /// return true to indicate an early exit.
  template<typename F>
  bool forEachTypeWitness(LazyResolver *resolver, F f) const {
    const ProtocolDecl *protocol = getProtocol();
    for (auto req : protocol->getMembers()) {
      auto assocTypeReq = dyn_cast<AssociatedTypeDecl>(req);
      if (!assocTypeReq || req->isInvalid())
        continue;

      const auto &TWInfo = getTypeWitnessSubstAndDecl(assocTypeReq, resolver);
      if (f(assocTypeReq, TWInfo.first, TWInfo.second))
        return true;
    }

    return false;
  }

  /// Retrieve the non-type witness for the given requirement.
  ConcreteDeclRef getWitness(ValueDecl *requirement, 
                             LazyResolver *resolver) const;

  /// Apply the given function object to each value witness within this
  /// protocol conformance.
  ///
  /// The function object should accept a \c ValueDecl* for the requirement
  /// followed by the \c ConcreteDeclRef for the witness.
  template<typename F>
  void forEachValueWitness(LazyResolver *resolver, F f) const {
    const ProtocolDecl *protocol = getProtocol();
    for (auto req : protocol->getMembers()) {
      auto valueReq = dyn_cast<ValueDecl>(req);
      if (!valueReq || isa<AssociatedTypeDecl>(valueReq) ||
          valueReq->isInvalid())
        continue;
      
      // Ignore accessors.
      if (auto *FD = dyn_cast<FuncDecl>(valueReq))
        if (FD->isAccessor())
          continue;

      f(valueReq, getWitness(valueReq, resolver));
    }
  }

  /// Retrieve the protocol conformance for the inherited protocol.
  ProtocolConformance *getInheritedConformance(ProtocolDecl *protocol) const;

  /// Retrieve the complete set of protocol conformances for directly inherited
  /// protocols.
  const InheritedConformanceMap &getInheritedConformances() const;
  
  /// Get the generic parameters open on the conforming type.
  /// FIXME: Retire in favor of getGenericSignature().
  GenericParamList *getGenericParams() const;

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

  /// Determine whether the witness for the given requirement
  /// is either the default definition or was otherwise deduced.
  ///
  /// FIXME: This is a crummy API. This information should be recorded in the
  /// witnesses themselves.
  bool usesDefaultDefinition(ValueDecl *requirement) const;
  
  // Make vanilla new/delete illegal for protocol conformances.
  void *operator new(size_t bytes) = delete;
  void operator delete(void *data) = delete;

  // Only allow allocation of protocol conformances using the allocator in
  // ASTContext or by doing a placement new.
  void *operator new(size_t bytes, ASTContext &context,
                     AllocationArena arena,
                     unsigned alignment = alignof(ProtocolConformance));
  void *operator new(size_t bytes, void *mem) {
    assert(mem);
    return mem;
  }
  
  /// Print a parsable and human-readable description of the identifying
  /// information of the protocol conformance.
  void printName(raw_ostream &os,
                 const PrintOptions &PO = PrintOptions()) const;
  
  void dump() const;

private:
  friend class Substitution;
  /// Substitute the conforming type and produce a ProtocolConformance that
  /// applies to the substituted type.
  ProtocolConformance *subst(Module *module,
                             Type substType,
                             ArrayRef<Substitution> subs,
                             TypeSubstitutionMap &subMap,
                             ArchetypeConformanceMap &conformanceMap);
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
                                  public llvm::FoldingSetNode {
  /// \brief The protocol being conformed to and its current state.
  llvm::PointerIntPair<ProtocolDecl *, 2, ProtocolConformanceState>
    ProtocolAndState;

  /// The location of this protocol conformance in the source.
  SourceLoc Loc;

  /// The declaration context containing the ExtensionDecl or
  /// NominalTypeDecl that declared the conformance.
  mutable DeclContext *DC;

  /// \brief The mapping of individual requirements in the protocol over to
  /// the declarations that satisfy those requirements.
  mutable WitnessMap Mapping;

  /// The mapping from associated type requirements to their substitutions.
  mutable TypeWitnessMap TypeWitnesses;

  /// \brief The mapping from any directly-inherited protocols over to the
  /// protocol conformance structures that indicate how the given type meets
  /// the requirements of those protocols.
  InheritedConformanceMap InheritedMapping;

  /// The set of requirements for which we have used default definitions or
  /// otherwise deduced the result.
  llvm::SmallPtrSet<ValueDecl *, 4> DefaultedDefinitions;

  friend class ASTContext;

  NormalProtocolConformance(Type conformingType, ProtocolDecl *protocol,
                            SourceLoc loc, DeclContext *dc,
                            ProtocolConformanceState state)
    : ProtocolConformance(ProtocolConformanceKind::Normal, conformingType),
      ProtocolAndState(protocol, state), Loc(loc), DC(dc)
  {
  }

public:
  /// Get the protocol being conformed to.
  ProtocolDecl *getProtocol() const { return ProtocolAndState.getPointer(); }

  /// Retrieve the location of this 
  SourceLoc getLoc() const { return Loc; }

  /// Get the declaration context that contains the conforming extension or
  /// nominal type declaration.
  DeclContext *getDeclContext() const { return DC; }

  /// Set the declaration context that contains the conforming extension or
  /// nominal type declaration.
  void setDeclContext(DeclContext *dc) { DC = dc; }

  /// Retrieve the state of this conformance.
  ProtocolConformanceState getState() const {
    return ProtocolAndState.getInt();
  }

  /// Set the state of this conformance.
  void setState(ProtocolConformanceState state) {
    ProtocolAndState.setInt(state);
  }

  /// Retrieve the type witness substitution and type decl (if one exists)
  /// for the given associated type.
  std::pair<const Substitution &, TypeDecl *>
  getTypeWitnessSubstAndDecl(AssociatedTypeDecl *assocType,
                             LazyResolver *resolver) const;

  /// Determine whether the protocol conformance has a type witness for the
  /// given associated type.
  bool hasTypeWitness(AssociatedTypeDecl *assocType,
                      LazyResolver *resolver = nullptr) const;

  /// Set the type witness for the given associated type.
  /// \param typeDecl the type decl the witness type came from, if one exists.
  void setTypeWitness(AssociatedTypeDecl *assocType,
                      const Substitution &substitution,
                      TypeDecl *typeDecl) const;

  /// Retrieve the value witness corresponding to the given requirement.
  ConcreteDeclRef getWitness(ValueDecl *requirement, 
                             LazyResolver *resolver) const;

  /// Determine whether the protocol conformance has a witness for the given
  /// requirement.
  bool hasWitness(ValueDecl *requirement) const {
    return Mapping.count(requirement) > 0;
  }

  /// Set the witness for the given requirement.
  void setWitness(ValueDecl *requirement, ConcreteDeclRef witness) const;

  /// Retrieve the protocol conformances directly-inherited protocols.
  const InheritedConformanceMap &getInheritedConformances() const {
    return InheritedMapping;
  }

  /// Determine whether the protocol conformance has a particular inherited
  /// conformance.
  ///
  /// Only usable on incomplete or invalid protocol conformances.
  bool hasInheritedConformance(ProtocolDecl *proto) const {
    return InheritedMapping.count(proto) > 0;
  }

  /// Set the given inherited conformance.
  void setInheritedConformance(ProtocolDecl *proto,
                               ProtocolConformance *conformance) {
    assert(InheritedMapping.count(proto) == 0 &&
           "Already recorded inherited conformance");
    assert(!isComplete() && "Conformance already complete?");
    InheritedMapping[proto] = conformance;
  }
  /// Determine whether the witness for the given requirement
  /// is either the default definition or was otherwise deduced.
  bool usesDefaultDefinition(ValueDecl *requirement) const {
    return DefaultedDefinitions.count(requirement) > 0;
  }

  /// Retrieve the complete set of defaulted definitions.
  const llvm::SmallPtrSet<ValueDecl *, 4> &getDefaultedDefinitions() const {
    return DefaultedDefinitions;
  }

  /// Note that the given requirement was a default definition.
  void addDefaultDefinition(ValueDecl *requirement) {
    DefaultedDefinitions.insert(requirement);
  }

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

/// Specalized protocol conformance, which projects a generic protocol
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
  ArrayRef<Substitution> GenericSubstitutions;

  /// The mapping from associated type requirements to their substitutions.
  ///
  /// This mapping is lazily produced by specializing the underlying,
  /// generic conformance.
  mutable TypeWitnessMap TypeWitnesses;

  friend class ASTContext;

  SpecializedProtocolConformance(Type conformingType,
                                 ProtocolConformance *genericConformance,
                                 ArrayRef<Substitution> substitutions);

public:
  /// Get the generic conformance from which this conformance was derived,
  /// if there is one.
  ProtocolConformance *getGenericConformance() const {
    return GenericConformance;
  }

  /// Get the substitutions used to produce this specialized conformance from
  /// the generic conformance.
  ArrayRef<Substitution> getGenericSubstitutions() const {
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

  /// Retrieve the type witness substitution and type decl (if one exists)
  /// for the given associated type.
  std::pair<const Substitution &, TypeDecl *>
  getTypeWitnessSubstAndDecl(AssociatedTypeDecl *assocType,
                             LazyResolver *resolver) const;

  /// Retrieve the value witness corresponding to the given requirement.
  ConcreteDeclRef getWitness(ValueDecl *requirement, 
                             LazyResolver *resolver) const;


  /// Retrieve the protocol conformances directly-inherited protocols.
  const InheritedConformanceMap &getInheritedConformances() const {
    return GenericConformance->getInheritedConformances();
  }

  /// Determine whether the witness for the given requirement
  /// is either the default definition or was otherwise deduced.
  bool usesDefaultDefinition(ValueDecl *requirement) const {
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
    ID.AddPointer(type->getCanonicalType().getPointer());
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
    : ProtocolConformance(ProtocolConformanceKind::Inherited,
                          conformingType),
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
    return InheritedConformance->getDeclContext();
  }

  /// Retrieve the state of this conformance.
  ProtocolConformanceState getState() const {
    return InheritedConformance->getState();
  }

  bool hasTypeWitness(AssociatedTypeDecl *assocType,
                      LazyResolver *resolver = nullptr) const {
    return InheritedConformance->hasTypeWitness(assocType, resolver);
  }

  /// Retrieve the type witness substitution and type decl (if one exists)
  /// for the given associated type.
  std::pair<const Substitution &, TypeDecl *>
  getTypeWitnessSubstAndDecl(AssociatedTypeDecl *assocType,
                             LazyResolver *resolver) const {
    return InheritedConformance->getTypeWitnessSubstAndDecl(assocType,resolver);
  }

  /// Retrieve the value witness corresponding to the given requirement.
  ConcreteDeclRef getWitness(ValueDecl *requirement, 
                             LazyResolver *resolver) const {
    return InheritedConformance->getWitness(requirement, resolver);
  }

  /// Retrieve the protocol conformances directly-inherited protocols.
  const InheritedConformanceMap &getInheritedConformances() const {
    return InheritedConformance->getInheritedConformances();
  }

  /// Determine whether the witness for the given requirement
  /// is either the default definition or was otherwise deduced.
  bool usesDefaultDefinition(ValueDecl *requirement) const {
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

} // end namespace swift

#endif // LLVM_SWIFT_AST_PROTOCOLCONFORMANCE_H
