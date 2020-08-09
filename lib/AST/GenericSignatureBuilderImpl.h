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
  llvm::PointerUnion<PotentialArchetype *, Type, EquivalenceClass *> storage;

  /// For a type that could not be resolved further unless the given
  /// equivalence class changes.
  ResolvedType(EquivalenceClass *equivClass) : storage(equivClass) { }

  /// A concrete resolved type.
  ResolvedType(Type type) : storage(type) {
    assert(!type->isTypeParameter());
  }

public:
  /// A specific resolved potential archetype.
  ResolvedType(PotentialArchetype *pa) : storage(pa) { }

  /// Return an unresolved result, which could be resolved when we
  /// learn more information about the given equivalence class.
  static ResolvedType forUnresolved(EquivalenceClass *equivClass) {
    return ResolvedType(equivClass);
  }

  /// Return a result for a concrete type.
  static ResolvedType forConcrete(Type concreteType) {
    return ResolvedType(concreteType);
  }

  /// Determine whether this result was resolved.
  explicit operator bool() const {
    return storage.is<PotentialArchetype *>() || storage.is<Type>();
  }

  /// Retrieve the dependent type.
  Type getDependentType(GenericSignatureBuilder &builder) const;

  /// Retrieve the concrete type, or a null type if this result doesn't store
  /// a concrete type.
  Type getAsConcreteType() const {
    assert(*this && "Doesn't contain any result");
    return storage.dyn_cast<Type>();
  }

  /// Retrieve the potential archetype, if already known.
  PotentialArchetype *getPotentialArchetypeIfKnown() const {
    return storage.dyn_cast<PotentialArchetype *>();
  }

  /// Retrieve the equivalence class into which a resolved type refers.
  EquivalenceClass *getEquivalenceClass(
                     GenericSignatureBuilder &builder) const {
    return storage.get<PotentialArchetype *>()
             ->getOrCreateEquivalenceClass(builder);
  }

  /// Retrieve the equivalence class into which a resolved type refers.
  EquivalenceClass *getEquivalenceClassIfPresent() const {
    return storage.get<PotentialArchetype *>()
            ->getEquivalenceClassIfPresent();
  }

  /// Retrieve the unresolved result.
  EquivalenceClass *getUnresolvedEquivClass() const {
    return storage.dyn_cast<EquivalenceClass *>();
  }

  /// Return an unresolved type.
  UnresolvedType getUnresolvedType() const {
    assert(!storage.is<EquivalenceClass *>());
    if (storage.is<PotentialArchetype *>())
      return storage.get<PotentialArchetype *>();
    return storage.get<Type>();
  }
};

} // end namepsace swift

#endif // SWIFT_AST_GENERIC_SIGNATURE_BUILDER_IMPL_H
