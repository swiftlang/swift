//===--- RequirementEnvironment.h - Requirement Environments ----*- C++ -*-===//
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
// This file defines the RequirementEnvironment class, which is used to
// capture how a witness to a protocol requirement maps type parameters.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_REQUIREMENT_ENVIRONMENT_H
#define SWIFT_AST_REQUIREMENT_ENVIRONMENT_H

#include "swift/AST/SubstitutionMap.h"

namespace swift {

class RootProtocolConformance;

/// Describes the environment of a requirement that will be used when
/// matching witnesses against the requirement and to form the resulting
/// \c Witness value.
///
/// The produced generic environment will have a fresh set of archetypes that
/// describe the combined constraints of the requirement (because those
/// are available to all potential witnesses) as well as the constraints from
/// the context to which the protocol conformance is ascribed, which may
/// include additional constraints beyond those of the extended type if the
/// conformance is conditional. The type parameters for the generic
/// environment are the type parameters of the conformance context
/// (\c conformanceDC) with another (deeper) level of type parameters for
/// generic requirements. See the \c Witness class for more information about
/// the witness thunk signature.
class RequirementEnvironment {
  /// A generic signature that combines the generic parameters of the
  /// concrete conforming type with the generic parameters of the
  /// requirement.
  ///
  ///
  /// For example, if you have:
  ///
  /// protocol P { func f<T>(_: T) }
  /// struct S<A, B> : P { func f<T>(_: T) }
  ///
  /// The requirement and witness signatures are, respectively:
  ///
  /// <Self : P, T>
  /// <A, B, T>
  ///
  /// The witness thunk signature in this case is just the witness signature.
  ///
  /// It may be that the witness is more generic than the requirement,
  /// for example:
  ///
  /// protocol P { func f(_: Int) }
  /// struct S<A, B> : P { func f<T>(_: T) { } }
  ///
  /// Here, the requirement signature and witness signatures are:
  ///
  /// <Self : P>
  /// <A, B, T>
  ///
  /// The witness thunk signature is just:
  ///
  /// <A, B>
  ///
  /// The witness thunk emitted by SILGen uses the witness thunk signature.
  /// Therefore one invariant we preserve is that the witness thunk is
  /// ABI compatible with the requirement's function type.
  GenericSignature witnessThunkSig = GenericSignature();

  /// The generic signature of the protocol requirement member.
  GenericSignature reqSig = GenericSignature();

  /// A substitution map mapping the requirement signature to the
  /// generic parameters of the witness thunk signature.
  SubstitutionMap reqToWitnessThunkSigMap;

public:
  /// Create a new environment for matching the given requirement within a
  /// particular conformance.
  ///
  /// \param conformanceDC The \c DeclContext to which the protocol
  /// conformance is ascribed, which provides additional constraints.
  ///
  /// \param reqSig The generic signature of the requirement for which we
  /// are creating a generic environment.
  ///
  /// \param proto The protocol containing the requirement.
  ///
  /// \param conformance The protocol conformance, or null if there is no
  /// conformance (because we're finding default implementations).
  RequirementEnvironment(DeclContext *conformanceDC,
                         GenericSignature reqSig,
                         ProtocolDecl *proto,
                         ClassDecl *covariantSelf,
                         RootProtocolConformance *conformance);

  /// Retrieve the generic signature of the requirement.
  GenericSignature getRequirementSignature() const {
    return reqSig;
  }

  /// Retrieve the generic signature of the witness thunk.
  GenericSignature getWitnessThunkSignature() const {
    return witnessThunkSig;
  }

  /// Retrieve the substitution map that maps the interface types of the
  /// requirement to the archetypes of the witness thunk signature's
  /// generic environment.
  SubstitutionMap getRequirementToWitnessThunkSubs() const {
    return reqToWitnessThunkSigMap;
  }
};

}

#endif // SWIFT_AST_REQUIREMENT_ENVIRONMENT_H
