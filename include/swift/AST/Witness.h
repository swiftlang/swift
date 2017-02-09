//===--- Witness.h - AST Witness Representation -----------------*- C++ -*-===//
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
// This file defines the \c Witness data structure, used as part of protocol
// conformances.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_WITNESS_H
#define SWIFT_AST_WITNESS_H

#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/SubstitutionMap.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/Compiler.h"

namespace swift {

class GenericEnvironment;
class GenericSignature;
class ValueDecl;

/// Describes a specific witness to a (non-type) requirement.
///
/// A witness to a requirement is a specific definition that is used to satisfy
/// that requirement within a particular protocol conformance. For example:
///
/// \code
/// protocol P {
///   func f()
/// }
///
/// struct X : P {
///   func f() { }
/// }
/// \endcode
///
/// Here, the function `X.f()` is the witness for the requirement `P.f()`.
///
/// The relationship between a requirement and its witness is more detailed
/// than the declaration of the witness, particularly when the requirement
/// is itself generic. Consider this more-involved example:
///
/// \code
/// protocol P {
///   associatedtype A
/// }
///
/// protocol Q : P { }
///
/// protocol R {
///   associatedtype B
///   func foo<T : Q>(x: T) where T.A == B
/// }
///
/// struct X<U, V> : R {
///   typealias B = U
///
///   func foo<W: P>(x: W) where W.A == U { }
/// }
/// \endcode
///
/// The witness for \c R.foo(x:) is \c X<U, V>.foo(x:), but the generic
/// functions that describe the generic requirement in \c R and the generic
/// method in \c X have very different signatures. To handle this case, the
/// \c Witness class produces a "synthetic" environment that pulls together
/// all of the information needed to map from the requirement to the witness.
/// It is a generic environment that combines the constraints of the
/// requirement with the constraints from the context of the protocol
/// conformance itself, which is the environment needed to build the witness
/// "thunk" in SILGen. Specifically, the outer generic parameters of the
/// environment are those of the context of the protocol conformance (\c U
/// and \c V, in the example above) and the innermost generic parameters are
/// those of the generic requirement (\c T, in the example above). The
/// \c Witness class contains this synthetic environment (both its generic
/// signature and a generic environment providing archetypes), a substitution
/// map that allows one to map the interface types of the requirement into
/// the interface types of the synthetic domain, and the set of substitutions
/// required to use the witness from the synthetic domain (e.g., how one would
/// call the witness from the witness thunk).
class Witness {
  struct StoredWitness {
    /// The witness declaration, along with the substitutions needed to use
    /// the witness declaration from the synthetic environment.
    ConcreteDeclRef declRef;
    GenericEnvironment *syntheticEnvironment;
    SubstitutionMap reqToSyntheticEnvMap;
  };

  llvm::PointerUnion<ValueDecl *, StoredWitness *> storage;

public:
  /// Create an empty witness, which describes missing witnesses.
  Witness() : storage(static_cast<ValueDecl*>(nullptr)) { }

  /// Create a witness that requires no substitutions.
  ///
  /// A witness requires no substitutions when the requirement is
  /// not generic (excepting \c Self)  and the conforming type is non-generic.
  Witness(ValueDecl *witness) : storage(witness) { assert(witness != nullptr); }

  /// Create a witness that requires substitutions.
  ///
  /// \param decl The declaration for the witness.
  ///
  /// \param substitutions The substitutions required to use the witness from
  /// the synthetic environment.
  ///
  /// \param syntheticEnv The synthetic environment.
  ///
  /// \param reqToSyntheticEnvMap The mapping from the interface types of the
  /// requirement into the interface types of the synthetic environment.
  Witness(ValueDecl *decl, SubstitutionList substitutions,
          GenericEnvironment *syntheticEnv,
          SubstitutionMap reqToSyntheticEnvMap);

  /// Retrieve the witness declaration reference, which includes the
  /// substitutions needed to use the witness from the synthetic environment
  /// (if any).
  ConcreteDeclRef getDeclRef() const {
    if (auto stored = storage.dyn_cast<StoredWitness *>())
      return stored->declRef;

    return storage.dyn_cast<ValueDecl *>();
  }

  /// Retrieve the witness declaration.
  ValueDecl *getDecl() const { return getDeclRef().getDecl(); }

  /// Determines whether there is a witness at all.
  explicit operator bool() const { return !storage.isNull(); }

  /// Implicit conversion to the \c ConcreteDeclRef, which is used by a
  /// number of clients.
  ///
  /// FIXME: We probably want this to go away eventually, because clients using
  /// it will all need to be cognizant of the synthetic environment.
  operator ConcreteDeclRef() const { return getDeclRef(); }

  /// Determine whether this witness requires any substitutions.
  bool requiresSubstitution() const { return storage.is<StoredWitness *>(); }

  /// Retrieve the substitutions required to use this witness from the
  /// synthetic environment.
  ///
  /// The substitutions are substitutions for the witness, providing interface
  /// types from the synthetic environment.
  SubstitutionList getSubstitutions() const {
    return getDeclRef().getSubstitutions();
  }

  /// Retrieve the synthetic generic environment.
  GenericEnvironment *getSyntheticEnvironment() const {
    assert(requiresSubstitution() && "No substitutions required for witness");
    return storage.get<StoredWitness *>()->syntheticEnvironment;
  }

  /// Retrieve the substitution map that maps the interface types of the
  /// requirement to the interface types of the synthetic environment.
  const SubstitutionMap &getRequirementToSyntheticMap() const {
    assert(requiresSubstitution() && "No substitutions required for witness");
    return storage.get<StoredWitness *>()->reqToSyntheticEnvMap;
  }

  LLVM_ATTRIBUTE_DEPRECATED(
    void dump() const LLVM_ATTRIBUTE_USED,
    "only for use within the debugger");

  void dump(llvm::raw_ostream &out) const;
};

} // end namespace swift

#endif // SWIFT_AST_WITNESS_H
