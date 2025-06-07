//===--- ApplyInverses.cpp - Resolve `~Protocol` anti-constraints ---------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// The `applyInverses` function takes the syntactic representation of a generic
// signature and applies implicit default constraints on generic parameters for
// core type capabilities like `Copyable` and `Escapable`. In doing so, it
// looks for explicit constraint suppression "requirements" like `T: ~Copyable`
// or same-type constraints that would contradict the implicit requirements and
// filters out unwanted default requirements.
//
//===----------------------------------------------------------------------===//

#include "RequirementLowering.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/RequirementSignature.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeMatcher.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SetVector.h"
#include "Diagnostics.h"
#include "RewriteContext.h"
#include "NameLookup.h"

using namespace swift;
using namespace rewriting;

void swift::rewriting::applyInverses(
    ASTContext &ctx,
    ArrayRef<Type> gps,
    ArrayRef<InverseRequirement> inverseList,
    ArrayRef<StructuralRequirement> explicitRequirements,
    SmallVectorImpl<StructuralRequirement> &result,
    SmallVectorImpl<RequirementError> &errors) {

  // Are there even any inverses or same-type requirements to validate?
  if (inverseList.empty() && explicitRequirements.empty()) {
    return;
  }

  const bool allowInverseOnAssocType =
      ctx.LangOpts.hasFeature(Feature::SuppressedAssociatedTypes);
      
  llvm::DenseMap<CanType, CanType> representativeGPs;
  
  // Start with an identity mapping.
  for (auto gp : gps) {
    auto canGP = gp->getCanonicalType();
    representativeGPs.insert({canGP, canGP});
  }
  bool hadSameTypeConstraintInScope = false;

  // Return the in-scope generic parameter that represents the equivalence class
  // for `gp`, or return null if the parameter is constrained out of scope.
  auto representativeGPFor = [&](CanType gp) -> CanType {
    while (true) {
      auto found = representativeGPs.find(gp);
      if (found == representativeGPs.end()) {
        return CanType();
      }
      if (found->second == CanType()) {
        return CanType();
      }
      
      if (found->second == gp) {
        return gp;
      }
      
      gp = found->second;
    }
  };

  // Look for same-type constraints that equate multiple generic parameters
  // within the scope so we can treat the equivalence class as a unit.
  for (auto &explicitReqt : explicitRequirements) {
    if (explicitReqt.req.getKind() != RequirementKind::SameType) {
      continue;
    }

    // If one end of the same-type requirement is in scope, and the other is
    // a concrete type or out-of-scope generic parameter, then the other
    // parameter is also effectively out of scope.
    auto firstTy = explicitReqt.req.getFirstType()->getCanonicalType();
    auto secondTy = explicitReqt.req.getSecondType()->getCanonicalType();
    if (!representativeGPs.count(firstTy)
        && !representativeGPs.count(secondTy)) {
      // Same type constraint doesn't involve any in-scope generic parameters.
      continue;
    }

    CanType typeInScope;
    CanType typeOutOfScope;

    if (representativeGPs.count(firstTy)
        && !representativeGPs.count(secondTy)){
      // First type is constrained out of scope.
      typeInScope = firstTy;
      typeOutOfScope = secondTy;
    } else if (!representativeGPs.count(firstTy)
               && representativeGPs.count(secondTy)) {
      // Second type is constrained out of scope.
      typeInScope = secondTy;
      typeOutOfScope = firstTy;
    } else {
      // Otherwise, both ends of the same-type constraint are in scope.
      // Fold the lexicographically-greater parameter with the lesser.
      auto firstGP = cast<GenericTypeParamType>(firstTy);
      auto secondGP = cast<GenericTypeParamType>(secondTy);
      
      if (firstGP == secondGP) {
        // `T == T` has no effect.
        continue;
      }
      
      if (firstGP->getDepth() > secondGP->getDepth()
          || (firstGP->getDepth() == secondGP->getDepth()
              && firstGP->getIndex() > secondGP->getIndex())) {
        std::swap(firstGP, secondGP);
      }
      
      hadSameTypeConstraintInScope = true;
      representativeGPs.insert_or_assign(secondGP, representativeGPFor(firstGP));
      continue;
    }

    // If the out-of-scope type is another type parameter or associated type,
    // then ignore this same-type constraint and allow defaulting to continue.
    //
    // It would probably have been more principled to suppress any defaulting
    // in this case, but this behavior shipped in Swift 6.0 and 6.1, so we
    // need to maintain source compatibility.
    if (typeOutOfScope->isTypeParameter()) {
      continue;
    }
    
    // If the out-of-scope type contains errors, then similarly, ignore the
    // same type constraint. Any additional diagnostics arising from the type
    // parameter being left ~Copyable or ~Escapable might be misleading if the
    // corrected code is attempting to refer to a Copyable or Escapable type.
    if (typeOutOfScope->hasError()) {
      continue;
    }
    
    representativeGPs.insert_or_assign(representativeGPFor(typeInScope),
                                       CanType());
    hadSameTypeConstraintInScope = true;
  }

  // Summarize the inverses and diagnose ones that are incorrect.
  llvm::DenseMap<CanType, InvertibleProtocolSet> inverses;
  for (auto inverse : inverseList) {
    auto canSubject = inverse.subject->getCanonicalType();

    // Inverses on associated types are experimental.
    if (!allowInverseOnAssocType && canSubject->is<DependentMemberType>()) {
      // Special exception: allow if we're building the stdlib.
      if (!ctx.MainModule->isStdlibModule()) {
        errors.push_back(RequirementError::forInvalidInverseSubject(inverse));
        continue;
      }
    }

    // Noncopyable checking support for parameter packs is not implemented yet.
    if (canSubject->isParameterPack()) {
      errors.push_back(RequirementError::forInvalidInverseSubject(inverse));
      continue;
    }

    // Value generics never have inverse requirements (or the positive thereof).
    if (canSubject->isValueParameter()) {
      continue;
    }

    // If the inverse is on a subject that wasn't permitted by our caller, then
    // remove and diagnose as an error. This can happen when an inner context
    // has a constraint on some outer generic parameter, e.g.,
    //
    //     protocol P {
    //       func f() where Self: ~Copyable
    //     }
    //
    if (representativeGPs.find(canSubject) == representativeGPs.end()) {
      errors.push_back(
          RequirementError::forInvalidInverseOuterSubject(inverse));
      continue;
    }

    auto representativeSubject = representativeGPFor(canSubject);
    
    // If the subject is in scope, but same-type constrained to a type out of
    // scope, then allow inverses to be stated even though they are redundant.
    // This is because older versions of Swift not only accepted but required
    // `extension Foo where T == NonCopyableType, T: ~Copyable {}` to be
    // written, so we need to continue to accept that formulation for source
    // compatibility.
    if (!representativeSubject) {
      continue;
    }

    auto state = inverses.getOrInsertDefault(representativeSubject);

    // Check if this inverse has already been seen.
    auto inverseKind = inverse.getKind();
    if (state.contains(inverseKind))
      continue;

    state.insert(inverseKind);
    inverses[representativeSubject] = state;
  }

  // Fast-path: if there are no valid inverses or same-type constraints, then
  // there are no requirements to be removed.
  if (inverses.empty() && !hadSameTypeConstraintInScope) {
    return;
  }

  // Scan the structural requirements and cancel out any inferred requirements
  // based on the inverses we saw.
  result.erase(llvm::remove_if(result, [&](StructuralRequirement structReq) {
    auto req = structReq.req;

    if (req.getKind() != RequirementKind::Conformance)
      return false;

    // Only consider requirements involving an invertible protocol.
    auto proto = req.getProtocolDecl()->getInvertibleProtocolKind();
    if (!proto) {
      return false;
    }

    // See if this subject is in-scope.
    auto subject = req.getFirstType()->getCanonicalType();
    auto representative = representativeGPs.find(subject);
    if (representative == representativeGPs.end()) {
      return false;
    }
      
    // If this type is same-type constrained into another equivalence class,
    // then it doesn't need its own defaulted requirements.
    if (representative->second != subject) {
      return true;
    }

    // We now have found the inferred constraint 'Subject : Proto'.
    // So, remove it if we have recorded a 'Subject : ~Proto'.
    auto foundInverses = inverses.find(subject);
    if (foundInverses == inverses.end()) {
      return false;
    }
    auto recordedInverses = foundInverses->getSecond();
    return recordedInverses.contains(*proto);
  }), result.end());
}
