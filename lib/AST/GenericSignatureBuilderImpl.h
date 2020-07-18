//
//  GenericSignatureBuilderImpl.h
//  Swift
//
//  Created by Doug Gregor on 12/17/18.
//

#ifndef SWIFT_AST_GENERIC_SIGNATURE_BUILDER_IMPL_H
#define SWIFT_AST_GENERIC_SIGNATURE_BUILDER_IMPL_H

#include "swift/AST/GenericSignatureBuilder.h"

namespace swift {

class GenericSignatureBuilder::ResolvedType {
  llvm::PointerUnion<PotentialArchetype *, Type> type;
  EquivalenceClass *equivClass;

  /// For a type that could not be resolved further unless the given
  /// equivalence class changes.
  ResolvedType(EquivalenceClass *equivClass)
    : type(), equivClass(equivClass) { }

public:
  /// A specific resolved potential archetype.
  ResolvedType(PotentialArchetype *pa)
    : type(pa), equivClass(nullptr) { }

  /// A resolved type within the given equivalence class.
  ResolvedType(Type type, EquivalenceClass *equivClass)
      : type(type), equivClass(equivClass) {
    assert(type->isTypeParameter() == static_cast<bool>(equivClass) &&
           "type parameters must have equivalence classes");
  }

  /// Return an unresolved result, which could be resolved when we
  /// learn more information about the given equivalence class.
  static ResolvedType forUnresolved(EquivalenceClass *equivClass) {
    return ResolvedType(equivClass);
  }

  /// Return a result for a concrete type.
  static ResolvedType forConcrete(Type concreteType) {
    return ResolvedType(concreteType, nullptr);
  }

  /// Determine whether this result was resolved.
  explicit operator bool() const { return !type.isNull(); }

  /// Retrieve the dependent type.
  Type getDependentType(GenericSignatureBuilder &builder) const;

  /// Retrieve the concrete type, or a null type if this result doesn't store
  /// a concrete type.
  Type getAsConcreteType() const {
    assert(*this && "Doesn't contain any result");
    if (equivClass) return Type();
    return type.dyn_cast<Type>();
  }

  /// Realize a potential archetype for this type parameter.
  PotentialArchetype *realizePotentialArchetype(
                                            GenericSignatureBuilder &builder);

  /// Retrieve the potential archetype, if already known.
  PotentialArchetype *getPotentialArchetypeIfKnown() const {
    return type.dyn_cast<PotentialArchetype *>();
  }

  /// Retrieve the equivalence class into which a resolved type refers.
  EquivalenceClass *getEquivalenceClass(
                     GenericSignatureBuilder &builder) const {
    assert(*this && "Only for resolved types");
    if (equivClass) return equivClass;

    // Create the equivalence class now.
    return type.get<PotentialArchetype *>()
             ->getOrCreateEquivalenceClass(builder);
  }

  /// Retrieve the equivalence class into which a resolved type refers.
  EquivalenceClass *getEquivalenceClassIfPresent() const {
    assert(*this && "Only for resolved types");
    if (equivClass) return equivClass;

    // Create the equivalence class now.
    return type.get<PotentialArchetype *>()->getEquivalenceClassIfPresent();
  }

  /// Retrieve the unresolved result.
  EquivalenceClass *getUnresolvedEquivClass() const {
    assert(!*this);
    return equivClass;
  }

  /// Return an unresolved type.
  ///
  /// This loses equivalence-class information that could be useful, which
  /// is unfortunate.
  UnresolvedType getUnresolvedType() const {
    return type;
  }
};

} // end namepsace swift

#endif // SWIFT_AST_GENERIC_SIGNATURE_BUILDER_IMPL_H
