//===--- TypeCheckProtocolInference.cpp - Associated Type Inference -------===//
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
// This file implements semantic analysis for protocols, in particular, checking
// whether a given type conforms to a given protocol.
//===----------------------------------------------------------------------===//
#include "TypeCheckProtocol.h"
#include "DerivedConformances.h"
#include "TypeChecker.h"

#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/TypeMatcher.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Defer.h"
#include "swift/ClangImporter/ClangModule.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/TinyPtrVector.h"

#define DEBUG_TYPE "Associated type inference"
#include "llvm/Support/Debug.h"

STATISTIC(NumSolutionStates, "# of solution states visited");
STATISTIC(NumSolutionStatesFailedCheck,
          "# of solution states that failed constraints check");
STATISTIC(NumConstrainedExtensionChecks,
          "# of constrained extension checks");
STATISTIC(NumConstrainedExtensionChecksFailed,
          "# of constrained extension checks failed");
STATISTIC(NumDuplicateSolutionStates,
          "# of duplicate solution states ");

using namespace swift;

void InferredAssociatedTypesByWitness::dump() const {
  dump(llvm::errs(), 0);
}

void InferredAssociatedTypesByWitness::dump(llvm::raw_ostream &out,
                                            unsigned indent) const {
  out << "\n";
  out.indent(indent) << "(";
  if (Witness) {
    Witness->dumpRef(out);
  }

  for (const auto &inferred : Inferred) {
    out << "\n";
    out.indent(indent + 2);
    out << inferred.first->getName() << " := "
        << inferred.second.getString();
  }

  for (const auto &inferred : NonViable) {
    out << "\n";
    out.indent(indent + 2);
    out << std::get<0>(inferred)->getName() << " := "
        << std::get<1>(inferred).getString();
    auto type = std::get<2>(inferred).getRequirement();
    out << " [failed constraint " << type.getString() << "]";
  }

  out << ")";
}

void InferredTypeWitnessesSolution::dump() const {
  llvm::errs() << "Type Witnesses:\n";
  for (auto &typeWitness : TypeWitnesses) {
    llvm::errs() << "  " << typeWitness.first->getName() << " := ";
    typeWitness.second.first->print(llvm::errs());
    llvm::errs() << " value " << typeWitness.second.second << '\n';
  }
  llvm::errs() << "Value Witnesses:\n";
  for (unsigned i : indices(ValueWitnesses)) {
    auto &valueWitness = ValueWitnesses[i];
    llvm::errs() << i << ":  " << (Decl*)valueWitness.first
    << ' ' << valueWitness.first->getBaseName() << '\n';
    valueWitness.first->getDeclContext()->dumpContext();
    llvm::errs() << "    for " << (Decl*)valueWitness.second
    << ' ' << valueWitness.second->getBaseName() << '\n';
    valueWitness.second->getDeclContext()->dumpContext();
  }
}

namespace {
  void dumpInferredAssociatedTypesByWitnesses(
        const InferredAssociatedTypesByWitnesses &inferred,
        llvm::raw_ostream &out,
        unsigned indent) {
    for (const auto &value : inferred) {
      value.dump(out, indent);
    }
  }

  void dumpInferredAssociatedTypesByWitnesses(
        const InferredAssociatedTypesByWitnesses &inferred) LLVM_ATTRIBUTE_USED;

  void dumpInferredAssociatedTypesByWitnesses(
                          const InferredAssociatedTypesByWitnesses &inferred) {
    dumpInferredAssociatedTypesByWitnesses(inferred, llvm::errs(), 0);
  }

  void dumpInferredAssociatedTypes(const InferredAssociatedTypes &inferred,
                                   llvm::raw_ostream &out,
                                   unsigned indent) {
    for (const auto &value : inferred) {
      out << "\n";
      out.indent(indent) << "(";
      value.first->dumpRef(out);
      dumpInferredAssociatedTypesByWitnesses(value.second, out, indent + 2);
      out << ")";
    }
    out << "\n";
  }

  void dumpInferredAssociatedTypes(
         const InferredAssociatedTypes &inferred) LLVM_ATTRIBUTE_USED;

  void dumpInferredAssociatedTypes(const InferredAssociatedTypes &inferred) {
    dumpInferredAssociatedTypes(inferred, llvm::errs(), 0);
  }
}

AssociatedTypeInference::AssociatedTypeInference(
                                       TypeChecker &tc,
                                       NormalProtocolConformance *conformance)
  : tc(tc), conformance(conformance), proto(conformance->getProtocol()),
    dc(conformance->getDeclContext()),
    adoptee(conformance->getType())
{
}

static bool associatedTypesAreSameEquivalenceClass(AssociatedTypeDecl *a,
                                                   AssociatedTypeDecl *b) {
  if (a == b)
    return true;

  // TODO: Do a proper equivalence check here by looking for some relationship
  // between a and b's protocols. In practice today, it's unlikely that
  // two same-named associated types can currently be independent, since we
  // don't have anything like `@implements(P.foo)` to rename witnesses (and
  // we still fall back to name lookup for witnesses in more cases than we
  // should).
  if (a->getName() == b->getName())
    return true;

  return false;
}

InferredAssociatedTypesByWitnesses
AssociatedTypeInference::inferTypeWitnessesViaValueWitnesses(
                    ConformanceChecker &checker,
                    const llvm::SetVector<AssociatedTypeDecl *> &allUnresolved,
                    ValueDecl *req) {
  // Conformances constructed by the ClangImporter should have explicit type
  // witnesses already.
  if (isa<ClangModuleUnit>(conformance->getDeclContext()->getModuleScopeContext())) {
    llvm::errs() << "Cannot infer associated types for imported conformance:\n";
    conformance->getType().dump(llvm::errs());
    for (auto assocTypeDecl : allUnresolved)
      assocTypeDecl->dump(llvm::errs());
    abort();
  }

  InferredAssociatedTypesByWitnesses result;

  auto isExtensionUsableForInference = [&](ExtensionDecl *extension) -> bool {

    // The extension where the conformance being checked is declared.
    auto conformanceExtension = checker.Conformance->
      getDeclContext()->getAsDecl();
    if (extension == conformanceExtension)
      return true;

    auto *extendedNominal = extension->getExtendedNominal();

    // Invalid case.
    if (extendedNominal == nullptr)
      return true;
    
    // Assume unconstrained concrete extensions we found witnesses in are
    // always viable.
    if (!isa<ProtocolDecl>(extendedNominal))
      return !extension->isConstrainedExtension();
    
    // FIXME: The extension may not have a generic signature set up yet as
    // resolving signatures may trigger associated type inference.  This cycle
    // is now detectable and we should look into untangling it
    // - see rdar://55263708
    if (!extension->hasComputedGenericSignature())
      return true;

    // Build a generic signature.
    auto *extensionSig = extension->getGenericSignature();
    
    // The condition here is a bit more fickle than
    // `isExtensionApplied`. That check would prematurely reject
    // extensions like `P where AssocType == T` if we're relying on a
    // default implementation inside the extension to infer `AssocType == T`
    // in the first place. Only check conformances on the `Self` type,
    // because those have to be explicitly declared on the type somewhere
    // so won't be affected by whatever answer inference comes up with.
    auto selfTy = extension->getSelfInterfaceType();
    for (const Requirement &reqt : extensionSig->getRequirements()) {
      switch (reqt.getKind()) {
      case RequirementKind::Conformance:
      case RequirementKind::Superclass:
        // FIXME: This is the wrong check
        if (selfTy->isEqual(reqt.getFirstType())
            && !tc.isSubtypeOf(conformance->getType(),reqt.getSecondType(), dc))
          return false;
        break;

      case RequirementKind::Layout:
      case RequirementKind::SameType:
        break;
      }
    }

    return true;
  };

  auto typeInContext =
    conformance->getDeclContext()->mapTypeIntoContext(conformance->getType());

  for (auto witness :
       checker.lookupValueWitnesses(req, /*ignoringNames=*/nullptr)) {
    LLVM_DEBUG(llvm::dbgs() << "Inferring associated types from decl:\n";
               witness->dump(llvm::dbgs()));

    // If the potential witness came from an extension, and our `Self`
    // type can't use it regardless of what associated types we end up
    // inferring, skip the witness.
    if (auto extension = dyn_cast<ExtensionDecl>(witness->getDeclContext()))
      if (!isExtensionUsableForInference(extension))
        continue;

    // Try to resolve the type witness via this value witness.
    auto witnessResult = inferTypeWitnessesViaValueWitness(req, witness);

    // Filter out duplicated inferred types as well as inferred types
    // that don't meet the requirements placed on the associated type.
    llvm::DenseSet<std::pair<AssociatedTypeDecl *, CanType>> known;
    for (unsigned i = 0; i < witnessResult.Inferred.size(); /*nothing*/) {
#define REJECT {\
  witnessResult.Inferred.erase(witnessResult.Inferred.begin() + i); \
  continue; \
}
      auto &result = witnessResult.Inferred[i];

      LLVM_DEBUG(llvm::dbgs() << "Considering whether "
                              << result.first->getName()
                              << " can infer to:\n";
                 result.second->dump(llvm::dbgs()));

      // Filter out errors.
      if (result.second->hasError()) {
        LLVM_DEBUG(llvm::dbgs() << "-- has error type\n");
        REJECT;
      }

      // Filter out duplicates.
      if (!known.insert({result.first, result.second->getCanonicalType()})
                .second) {
        LLVM_DEBUG(llvm::dbgs() << "-- duplicate\n");
        REJECT;
      }

      // Filter out circular possibilities, e.g. that
      // AssocType == S.AssocType or
      // AssocType == Foo<S.AssocType>.
      bool canInferFromOtherAssociatedType = false;
      bool containsTautologicalType =
        result.second.findIf([&](Type t) -> bool {
          auto dmt = t->getAs<DependentMemberType>();
          if (!dmt)
            return false;
          if (!associatedTypesAreSameEquivalenceClass(dmt->getAssocType(),
                                                      result.first))
            return false;
          if (!dmt->getBase()->isEqual(typeInContext))
            return false;

          // If this associated type is same-typed to another associated type
          // on `Self`, then it may still be an interesting candidate if we find
          // an answer for that other type.
          auto witnessContext = witness->getDeclContext();
          if (witnessContext->getExtendedProtocolDecl()
              && witnessContext->getGenericSignatureOfContext()) {
            auto selfTy = witnessContext->getSelfInterfaceType();
            auto selfAssocTy = DependentMemberType::get(selfTy,
                                                        dmt->getAssocType());
            for (auto &reqt : witnessContext->getGenericSignatureOfContext()
                                            ->getRequirements()) {
              switch (reqt.getKind()) {
              case RequirementKind::Conformance:
              case RequirementKind::Superclass:
              case RequirementKind::Layout:
                break;

              case RequirementKind::SameType:
                Type other;
                if (reqt.getFirstType()->isEqual(selfAssocTy)) {
                  other = reqt.getSecondType();
                } else if (reqt.getSecondType()->isEqual(selfAssocTy)) {
                  other = reqt.getFirstType();
                } else {
                  break;
                }

                if (auto otherAssoc = other->getAs<DependentMemberType>()) {
                  if (otherAssoc->getBase()->isEqual(selfTy)) {
                    auto otherDMT = DependentMemberType::get(dmt->getBase(),
                                                    otherAssoc->getAssocType());

                    // We may be able to infer one associated type from the
                    // other.
                    result.second = result.second.transform([&](Type t) -> Type{
                      if (t->isEqual(dmt))
                        return otherDMT;
                      return t;
                    });
                    canInferFromOtherAssociatedType = true;
                    LLVM_DEBUG(llvm::dbgs() << "++ we can same-type to:\n";
                               result.second->dump(llvm::dbgs()));
                    return false;
                  }
                }
                break;
              }
            }
          }

          return true;
        });

      if (containsTautologicalType) {
        LLVM_DEBUG(llvm::dbgs() << "-- tautological\n");
        REJECT;
      }

      // Check that the type witness doesn't contradict an
      // explicitly-given type witness. If it does contradict, throw out the
      // witness completely.
      if (!allUnresolved.count(result.first)) {
        auto existingWitness =
          conformance->getTypeWitness(result.first);
        existingWitness = dc->mapTypeIntoContext(existingWitness);

        // If the deduced type contains an irreducible
        // DependentMemberType, that indicates a dependency
        // on another associated type we haven't deduced,
        // so we can't tell whether there's a contradiction
        // yet.
        auto newWitness = result.second->getCanonicalType();
        if (!newWitness->hasTypeParameter() &&
            !newWitness->hasDependentMember() &&
            !existingWitness->isEqual(newWitness)) {
          LLVM_DEBUG(llvm::dbgs() << "** contradicts explicit type witness, "
                                     "rejecting inference from this decl\n");
          goto next_witness;
        }
      }

      // If we same-typed to another unresolved associated type, we won't
      // be able to check conformances yet.
      if (!canInferFromOtherAssociatedType) {
        // Check that the type witness meets the
        // requirements on the associated type.
        if (auto failed = checkTypeWitness(tc, dc, proto, result.first,
                                           result.second)) {
          witnessResult.NonViable.push_back(
                          std::make_tuple(result.first,result.second,failed));
          LLVM_DEBUG(llvm::dbgs() << "-- doesn't fulfill requirements\n");
          REJECT;
        }
      }

      LLVM_DEBUG(llvm::dbgs() << "++ seems legit\n");
      ++i;
    }
#undef REJECT

    // If no inferred types remain, skip this witness.
    if (witnessResult.Inferred.empty() && witnessResult.NonViable.empty())
      continue;

    // If there were any non-viable inferred associated types, don't
    // infer anything from this witness.
    if (!witnessResult.NonViable.empty())
      witnessResult.Inferred.clear();

    result.push_back(std::move(witnessResult));
next_witness:;
}

  return result;
}

InferredAssociatedTypes
AssociatedTypeInference::inferTypeWitnessesViaValueWitnesses(
  ConformanceChecker &checker,
  const llvm::SetVector<AssociatedTypeDecl *> &assocTypes)
{
  InferredAssociatedTypes result;
  for (auto member : proto->getMembers()) {
    auto req = dyn_cast<ValueDecl>(member);
    if (!req)
      continue;
    if (!req->isProtocolRequirement())
      continue;

    // Infer type witnesses for associated types.
    if (auto assocType = dyn_cast<AssociatedTypeDecl>(req)) {
      // If this is not one of the associated types we are trying to infer,
      // just continue.
      if (assocTypes.count(assocType) == 0)
        continue;

      auto reqInferred = inferTypeWitnessesViaAssociatedType(checker,
                                                             assocTypes,
                                                             assocType);
      if (!reqInferred.empty())
        result.push_back({req, std::move(reqInferred)});

      continue;
    }

    // Skip operator requirements, because they match globally and
    // therefore tend to cause deduction mismatches.
    // FIXME: If we had some basic sanity checking of Self, we might be able to
    // use these.
    if (auto func = dyn_cast<FuncDecl>(req)) {
      if (func->isOperator() || isa<AccessorDecl>(func))
        continue;
    }

    // Validate the requirement.
    tc.validateDecl(req);
    if (req->isInvalid() || !req->hasInterfaceType())
      continue;

    // Check whether any of the associated types we care about are
    // referenced in this value requirement.
    bool anyAssocTypeMatches = false;
    for (auto assocType : checker.getReferencedAssociatedTypes(req)) {
      if (assocTypes.count(assocType) > 0) {
        anyAssocTypeMatches = true;
        break;
      }
    }

    // We cannot deduce anything from the witnesses of this
    // requirement; skip it.
    if (!anyAssocTypeMatches)
      continue;

    // Infer associated types from the potential value witnesses for
    // this requirement.
    auto reqInferred =
      inferTypeWitnessesViaValueWitnesses(checker, assocTypes, req);
    if (reqInferred.empty())
      continue;

    result.push_back({req, std::move(reqInferred)});
  }

  return result;
}

/// Map error types back to their original types.
static Type mapErrorTypeToOriginal(Type type) {
  if (auto errorType = type->getAs<ErrorType>()) {
    if (auto originalType = errorType->getOriginalType())
      return originalType.transform(mapErrorTypeToOriginal);
  }

  return type;
}

/// Produce the type when matching a witness.
static Type getWitnessTypeForMatching(TypeChecker &tc,
                                      NormalProtocolConformance *conformance,
                                      ValueDecl *witness) {
  if (!witness->hasInterfaceType())
    tc.validateDecl(witness);

  if (witness->isInvalid() || !witness->hasInterfaceType())
    return Type();

  if (!witness->getDeclContext()->isTypeContext()) {
    // FIXME: Could we infer from 'Self' to make these work?
    return witness->getInterfaceType();
  }

  // Retrieve the set of substitutions to be applied to the witness.
  Type model =
    conformance->getDeclContext()->mapTypeIntoContext(conformance->getType());
  TypeSubstitutionMap substitutions = model->getMemberSubstitutions(witness);
  Type type = witness->getInterfaceType()->getReferenceStorageReferent();

  if (substitutions.empty())
    return type;

  // Strip off the requirements of a generic function type.
  // FIXME: This doesn't actually break recursion when substitution
  // looks for an inferred type witness, but it makes it far less
  // common, because most of the recursion involves the requirements
  // of the generic type.
  if (auto genericFn = type->getAs<GenericFunctionType>()) {
    type = FunctionType::get(genericFn->getParams(),
                             genericFn->getResult(),
                             genericFn->getExtInfo());
  }

  // Remap associated types that reference other protocols into this
  // protocol.
  auto proto = conformance->getProtocol();
  type = type.transformRec([proto](TypeBase *type) -> Optional<Type> {
    if (auto depMemTy = dyn_cast<DependentMemberType>(type)) {
      if (depMemTy->getAssocType() &&
          depMemTy->getAssocType()->getProtocol() != proto) {
        if (auto *assocType = proto->getAssociatedType(depMemTy->getName())) {
          auto origProto = depMemTy->getAssocType()->getProtocol();
          if (proto->inheritsFrom(origProto))
            return Type(DependentMemberType::get(depMemTy->getBase(),
                                                 assocType));
        }
      }
    }

    return None;
  });

  ModuleDecl *module = conformance->getDeclContext()->getParentModule();
  auto resultType = type.subst(QueryTypeSubstitutionMap{substitutions},
                               LookUpConformanceInModule(module));
  if (!resultType->hasError()) return resultType;

  // Map error types with original types *back* to the original, dependent type.
  return resultType.transform(mapErrorTypeToOriginal);
}


/// Remove the 'self' type from the given type, if it's a method type.
static Type removeSelfParam(ValueDecl *value, Type type) {
  if (auto func = dyn_cast<AbstractFunctionDecl>(value)) {
    if (func->getDeclContext()->isTypeContext())
      return type->castTo<AnyFunctionType>()->getResult();
  }

  return type;
}

InferredAssociatedTypesByWitnesses
AssociatedTypeInference::inferTypeWitnessesViaAssociatedType(
                   ConformanceChecker &checker,
                   const llvm::SetVector<AssociatedTypeDecl *> &allUnresolved,
                   AssociatedTypeDecl *assocType) {
  auto &tc = checker.TC;

  // Form the default name _Default_Foo.
  Identifier defaultName;
  {
    SmallString<32> defaultNameStr;
    {
      llvm::raw_svector_ostream out(defaultNameStr);
      out << "_Default_";
      out << assocType->getName().str();
    }

    defaultName = tc.Context.getIdentifier(defaultNameStr);
  }

  // Look for types with the given default name that have appropriate
  // @_implements attributes.
  InferredAssociatedTypesByWitnesses result;
  auto lookupOptions = defaultMemberTypeLookupOptions;
  lookupOptions -= NameLookupFlags::PerformConformanceCheck;
  for (auto candidate : tc.lookupMember(dc, adoptee, defaultName,
                                        lookupOptions)) {
    // We want type declarations.
    auto typeDecl = dyn_cast<TypeDecl>(candidate.getValueDecl());
    if (!typeDecl || isa<AssociatedTypeDecl>(typeDecl))
      continue;

    // We only find these within a protocol extension.
    auto defaultProto = typeDecl->getDeclContext()->getSelfProtocolDecl();
    if (!defaultProto)
      continue;

    // Determine the witness type.
    Type witnessType = getWitnessTypeForMatching(tc, conformance, typeDecl);
    if (!witnessType) continue;

    if (auto witnessMetaType = witnessType->getAs<AnyMetatypeType>())
      witnessType = witnessMetaType->getInstanceType();
    else
      continue;

    // Add this result.
    InferredAssociatedTypesByWitness inferred;
    inferred.Witness = typeDecl;
    inferred.Inferred.push_back({assocType, witnessType});
    result.push_back(std::move(inferred));
  }

  return result;
}

Type swift::adjustInferredAssociatedType(Type type, bool &noescapeToEscaping) {
  // If we have an optional type, adjust its wrapped type.
  if (auto optionalObjectType = type->getOptionalObjectType()) {
    auto newOptionalObjectType =
      adjustInferredAssociatedType(optionalObjectType, noescapeToEscaping);
    if (newOptionalObjectType.getPointer() == optionalObjectType.getPointer())
      return type;

    return OptionalType::get(newOptionalObjectType);
  }

  // If we have a noescape function type, make it escaping.
  if (auto funcType = type->getAs<FunctionType>()) {
    if (funcType->isNoEscape()) {
      noescapeToEscaping = true;
      return FunctionType::get(funcType->getParams(), funcType->getResult(),
                               funcType->getExtInfo().withNoEscape(false));
    }
  }
  return type;
}

/// Attempt to resolve a type witness via a specific value witness.
InferredAssociatedTypesByWitness
AssociatedTypeInference::inferTypeWitnessesViaValueWitness(ValueDecl *req,
                                                           ValueDecl *witness) {
  InferredAssociatedTypesByWitness inferred;
  inferred.Witness = witness;

  // Compute the requirement and witness types we'll use for matching.
  Type fullWitnessType = getWitnessTypeForMatching(tc, conformance, witness);
  if (!fullWitnessType) {
    return inferred;
  }

  auto setup = [&]() -> std::tuple<Optional<RequirementMatch>, Type, Type> {
    fullWitnessType = removeSelfParam(witness, fullWitnessType);
    return std::make_tuple(
        None,
        removeSelfParam(req, req->getInterfaceType()),
        fullWitnessType);
  };

  /// Visits a requirement type to match it to a potential witness for
  /// the purpose of deducing associated types.
  ///
  /// The visitor argument is the witness type. If there are any
  /// obvious conflicts between the structure of the two types,
  /// returns true. The conflict checking is fairly conservative, only
  /// considering rough structure.
  class MatchVisitor : public TypeMatcher<MatchVisitor> {
    NormalProtocolConformance *Conformance;
    InferredAssociatedTypesByWitness &Inferred;

  public:
    MatchVisitor(NormalProtocolConformance *conformance,
                 InferredAssociatedTypesByWitness &inferred)
      : Conformance(conformance), Inferred(inferred) { }

    /// Structural mismatches imply that the witness cannot match.
    bool mismatch(TypeBase *firstType, TypeBase *secondType,
                  Type sugaredFirstType) {
      // If either type hit an error, don't stop yet.
      if (firstType->hasError() || secondType->hasError())
        return true;

      // FIXME: Check whether one of the types is dependent?
      return false;
    }

    /// Deduce associated types from dependent member types in the witness.
    bool mismatch(DependentMemberType *firstDepMember,
                  TypeBase *secondType, Type sugaredFirstType) {
      // If the second type is an error, don't look at it further.
      if (secondType->hasError())
        return true;

      // Adjust the type to a type that can be written explicitly.
      bool noescapeToEscaping = false;
      Type inferredType =
        adjustInferredAssociatedType(secondType, noescapeToEscaping);
      if (!inferredType->isMaterializable())
        return true;

      // If the type contains a type parameter, there is nothing we can infer
      // from it.
      // FIXME: This is a weird state introduced by associated type inference
      // that should not exist.
      if (inferredType->hasTypeParameter())
        return true;

      auto proto = Conformance->getProtocol();
      if (auto assocType = getReferencedAssocTypeOfProtocol(firstDepMember,
                                                            proto)) {
        Inferred.Inferred.push_back({assocType, inferredType});
      }

      // Always allow mismatches here.
      return true;
    }

    /// FIXME: Recheck the type of Self against the second type?
    bool mismatch(GenericTypeParamType *selfParamType,
                  TypeBase *secondType, Type sugaredFirstType) {
      return true;
    }
  };

  // Match a requirement and witness type.
  MatchVisitor matchVisitor(conformance, inferred);
  auto matchTypes = [&](Type reqType, Type witnessType)
                      -> Optional<RequirementMatch> {
    if (!matchVisitor.match(reqType, witnessType)) {
      return RequirementMatch(witness, MatchKind::TypeConflict,
                              fullWitnessType);
    }

    return None;
  };

  // Finalization of the checking is pretty trivial; just bundle up a
  // result we can look at.
  auto finalize = [&](bool anyRenaming, ArrayRef<OptionalAdjustment>)
                    -> RequirementMatch {
    return RequirementMatch(witness,
                            anyRenaming ? MatchKind::RenamedMatch
                                        : MatchKind::ExactMatch,
                            fullWitnessType);

  };

  // Match the witness. If we don't succeed, throw away the inference
  // information.
  // FIXME: A renamed match might be useful to retain for the failure case.
  if (matchWitness(dc, req, witness, setup, matchTypes, finalize)
          .Kind != MatchKind::ExactMatch) {
    inferred.Inferred.clear();
  }

  return inferred;
}

AssociatedTypeDecl *AssociatedTypeInference::findDefaultedAssociatedType(
                                             TypeChecker &tc,
                                             AssociatedTypeDecl *assocType) {
  // If this associated type has a default, we're done.
  tc.validateDecl(assocType);
  if (assocType->hasDefaultDefinitionType())
    return assocType;

  // Look at overridden associated types.
  SmallPtrSet<CanType, 4> canonicalTypes;
  SmallVector<AssociatedTypeDecl *, 2> results;
  for (auto overridden : assocType->getOverriddenDecls()) {
    auto overriddenDefault = findDefaultedAssociatedType(tc, overridden);
    if (!overriddenDefault) continue;

    Type overriddenType =
      overriddenDefault->getDefaultDefinitionType();
    assert(overriddenType);
    if (!overriddenType) continue;

    CanType key = overriddenType->getCanonicalType();
    if (canonicalTypes.insert(key).second)
      results.push_back(overriddenDefault);
  }

  // If there was a single result, return it.
  // FIXME: We could find *all* of the non-covered, defaulted associated types.
  return results.size() == 1 ? results.front() : nullptr;
}

Type AssociatedTypeInference::computeFixedTypeWitness(
                                            AssociatedTypeDecl *assocType) {
  // Look at all of the inherited protocols to determine whether they
  // require a fixed type for this associated type.
  Type dependentType = assocType->getDeclaredInterfaceType();
  Type resultType;
  for (auto conformedProto : adoptee->getAnyNominal()->getAllProtocols()) {
    if (!conformedProto->inheritsFrom(assocType->getProtocol()))
      continue;

    auto genericSig = conformedProto->getGenericSignature();
    if (!genericSig) return Type();

    Type concreteType = genericSig->getConcreteType(dependentType);
    if (!concreteType) continue;

    if (!resultType) {
      resultType = concreteType;
      continue;
    }

    // FIXME: Bailing out on ambiguity.
    if (!resultType->isEqual(concreteType))
      return Type();
  }

  return resultType;
}

Type AssociatedTypeInference::computeDefaultTypeWitness(
                                              AssociatedTypeDecl *assocType) {
  // Go find a default definition.
  auto defaultedAssocType = findDefaultedAssociatedType(tc, assocType);
  if (!defaultedAssocType) return Type();

  // If we don't have a default definition, we're done.
  auto selfType = proto->getSelfInterfaceType();

  // Create a set of type substitutions for all known associated type.
  // FIXME: Base this on dependent types rather than archetypes?
  TypeSubstitutionMap substitutions;
  substitutions[proto->mapTypeIntoContext(selfType)
                  ->castTo<ArchetypeType>()] = dc->mapTypeIntoContext(adoptee);
  for (auto assocType : proto->getAssociatedTypeMembers()) {
    auto archetype = proto->mapTypeIntoContext(
                       assocType->getDeclaredInterfaceType())
                         ->getAs<ArchetypeType>();
    if (!archetype)
      continue;
    if (conformance->hasTypeWitness(assocType)) {
      substitutions[archetype] =
        dc->mapTypeIntoContext(conformance->getTypeWitness(assocType));
    } else {
      auto known = typeWitnesses.begin(assocType);
      if (known != typeWitnesses.end())
        substitutions[archetype] = known->first;
      else
        substitutions[archetype] = ErrorType::get(archetype);
    }
  }

  Type defaultType = defaultedAssocType->getDefaultDefinitionType();

  // FIXME: Circularity
  if (!defaultType)
    return Type();

  // Map it into our protocol's context.
  defaultType = proto->mapTypeIntoContext(defaultType);
  defaultType = defaultType.subst(
                          QueryTypeSubstitutionMap{substitutions},
                          LookUpConformanceInModule(dc->getParentModule()));

  if (defaultType->hasError())
    return Type();

  if (auto failed = checkTypeWitness(tc, dc, proto, assocType, defaultType)) {
    // Record the failure, if we haven't seen one already.
    if (!failedDefaultedAssocType && !failed.isError()) {
      failedDefaultedAssocType = defaultedAssocType;
      failedDefaultedWitness = defaultType;
      failedDefaultedResult = failed;
    }

    return Type();
  }

  return defaultType;
}

Type AssociatedTypeInference::computeDerivedTypeWitness(
                                              AssociatedTypeDecl *assocType) {
  if (adoptee->hasError())
    return Type();

  // Can we derive conformances for this protocol and adoptee?
  NominalTypeDecl *derivingTypeDecl = adoptee->getAnyNominal();
  if (!DerivedConformance::derivesProtocolConformance(dc, derivingTypeDecl,
                                                      proto))
    return Type();

  // Try to derive the type witness.
  Type derivedType = tc.deriveTypeWitness(dc, derivingTypeDecl, assocType);
  if (!derivedType)
    return Type();

  // Make sure that the derived type is sane.
  if (checkTypeWitness(tc, dc, proto, assocType, derivedType)) {
    /// FIXME: Diagnose based on this.
    failedDerivedAssocType = assocType;
    failedDerivedWitness = derivedType;
    return Type();
  }

  return derivedType;
}

Type
AssociatedTypeInference::computeAbstractTypeWitness(
                                              AssociatedTypeDecl *assocType,
                                              bool allowDerived) {
  // We don't have a type witness for this associated type, so go
  // looking for more options.
  if (Type concreteType = computeFixedTypeWitness(assocType))
    return concreteType;

  // If we can form a default type, do so.
  if (Type defaultType = computeDefaultTypeWitness(assocType))
    return defaultType;

  // If we can derive a type witness, do so.
  if (allowDerived) {
    if (Type derivedType = computeDerivedTypeWitness(assocType))
      return derivedType;
  }

  // If there is a generic parameter of the named type, use that.
  if (auto *genericSig = dc->getGenericSignatureOfContext()) {
    for (auto gp : genericSig->getInnermostGenericParams()) {
      if (gp->getName() == assocType->getName())
        return dc->mapTypeIntoContext(gp);
    }
  }

  return Type();
}

Type AssociatedTypeInference::substCurrentTypeWitnesses(Type type) {
  // Local function that folds dependent member types with non-dependent
  // bases into actual member references.
  std::function<Type(Type)> foldDependentMemberTypes;
  llvm::DenseSet<AssociatedTypeDecl *> recursionCheck;
  foldDependentMemberTypes = [&](Type type) -> Type {
    if (auto depMemTy = type->getAs<DependentMemberType>()) {
      auto baseTy = depMemTy->getBase().transform(foldDependentMemberTypes);
      if (baseTy.isNull() || baseTy->hasTypeParameter())
        return nullptr;

      auto assocType = depMemTy->getAssocType();
      if (!assocType)
        return nullptr;

      if (!recursionCheck.insert(assocType).second)
        return nullptr;

      SWIFT_DEFER { recursionCheck.erase(assocType); };

      // Try to substitute into the base type.
      Type result = depMemTy->substBaseType(dc->getParentModule(), baseTy);
      if (!result->hasError())
        return result;

      // If that failed, check whether it's because of the conformance we're
      // evaluating.
      auto localConformance
        = TypeChecker::conformsToProtocol(
                          baseTy, assocType->getProtocol(), dc,
                          ConformanceCheckFlags::SkipConditionalRequirements);
      if (!localConformance || localConformance->isAbstract() ||
          (localConformance->getConcrete()->getRootConformance()
             != conformance)) {
        return nullptr;
      }

      // Find the tentative type witness for this associated type.
      auto known = typeWitnesses.begin(assocType);
      if (known == typeWitnesses.end())
        return nullptr;

      return known->first.transform(foldDependentMemberTypes);
    }

    // The presence of a generic type parameter indicates that we
    // cannot use this type binding.
    if (type->is<GenericTypeParamType>()) {
      return nullptr;
    }

    return type;
  };

  return type.transform(foldDependentMemberTypes);
}

/// "Sanitize" requirements for conformance checking, removing any requirements
/// that unnecessarily refer to associated types of other protocols.
static void sanitizeProtocolRequirements(
                                     ProtocolDecl *proto,
                                     ArrayRef<Requirement> requirements,
                                     SmallVectorImpl<Requirement> &sanitized) {
  std::function<Type(Type)> sanitizeType;
  sanitizeType = [&](Type outerType) {
    return outerType.transformRec([&] (TypeBase *type) -> Optional<Type> {
      if (auto depMemTy = dyn_cast<DependentMemberType>(type)) {
        if (!depMemTy->getAssocType() ||
            depMemTy->getAssocType()->getProtocol() != proto) {

          if (auto *assocType = proto->getAssociatedType(depMemTy->getName())) {
            Type sanitizedBase = sanitizeType(depMemTy->getBase());
            if (!sanitizedBase)
              return Type();
            return Type(DependentMemberType::get(sanitizedBase,
                                                  assocType));
          }

          if (depMemTy->getBase()->is<GenericTypeParamType>())
            return Type();
        }
      }

      return None;
    });
  };

  for (const auto &req : requirements) {
    switch (req.getKind()) {
    case RequirementKind::Conformance:
    case RequirementKind::SameType:
    case RequirementKind::Superclass: {
      Type firstType = sanitizeType(req.getFirstType());
      Type secondType = sanitizeType(req.getSecondType());
      if (firstType && secondType) {
        sanitized.push_back({req.getKind(), firstType, secondType});
      }
      break;
    }

    case RequirementKind::Layout: {
      Type firstType = sanitizeType(req.getFirstType());
      if (firstType) {
        sanitized.push_back({req.getKind(), firstType,
                             req.getLayoutConstraint()});
      }
      break;
    }
    }
  }
}

SubstOptions
AssociatedTypeInference::getSubstOptionsWithCurrentTypeWitnesses() {
  SubstOptions options(None);
  AssociatedTypeInference *self = this;
  options.getTentativeTypeWitness =
    [self](const NormalProtocolConformance *conformance,
           AssociatedTypeDecl *assocType) -> TypeBase * {
      auto thisProto = self->conformance->getProtocol();
      if (conformance == self->conformance) {
        // Okay: we have the associated type we need.
      } else if (conformance->getType()->isEqual(
                   self->conformance->getType()) &&
                 thisProto->inheritsFrom(conformance->getProtocol())) {
        // Find an associated type with the same name in the given
        // protocol.
        auto *foundAssocType = thisProto->getAssociatedType(
            assocType->getName());
        if (!foundAssocType) return nullptr;
        assocType = foundAssocType;
      } else {
        return nullptr;
      }

      Type type = self->typeWitnesses.begin(assocType)->first;
      return type->mapTypeOutOfContext().getPointer();
    };
  return options;
}

bool AssociatedTypeInference::checkCurrentTypeWitnesses(
       const SmallVectorImpl<std::pair<ValueDecl *, ValueDecl *>>
         &valueWitnesses) {
  // Check any same-type requirements in the protocol's requirement signature.
  SubstOptions options = getSubstOptionsWithCurrentTypeWitnesses();

  auto typeInContext = dc->mapTypeIntoContext(adoptee);

  auto substitutions =
    SubstitutionMap::getProtocolSubstitutions(
                                    proto, typeInContext,
                                    ProtocolConformanceRef(conformance));

  SmallVector<Requirement, 4> sanitizedRequirements;
  sanitizeProtocolRequirements(proto, proto->getRequirementSignature(),
                               sanitizedRequirements);
  auto result =
    tc.checkGenericArguments(dc, SourceLoc(), SourceLoc(),
                             typeInContext,
                             { proto->getSelfInterfaceType() },
                             sanitizedRequirements,
                             QuerySubstitutionMap{substitutions},
                             TypeChecker::LookUpConformance(dc),
                             None, nullptr, options);
  switch (result) {
  case RequirementCheckResult::Failure:
    ++NumSolutionStatesFailedCheck;
    return true;

  case RequirementCheckResult::Success:
  case RequirementCheckResult::SubstitutionFailure:
    break;
  }

  // Check for extra requirements in the constrained extensions that supply
  // defaults.
  SmallPtrSet<ExtensionDecl *, 4> checkedExtensions;
  for (const auto &valueWitness : valueWitnesses) {
    // We only perform this additional checking for default associated types.
    if (!isa<TypeDecl>(valueWitness.first)) continue;

    auto witness = valueWitness.second;
    if (!witness) continue;

    auto ext = dyn_cast<ExtensionDecl>(witness->getDeclContext());
    if (!ext) continue;

    if (!ext->isConstrainedExtension()) continue;
    if (!checkedExtensions.insert(ext).second) continue;

    ++NumConstrainedExtensionChecks;
    if (checkConstrainedExtension(ext)) {
      ++NumConstrainedExtensionChecksFailed;
      return true;
    }
  }

  return false;
}

bool AssociatedTypeInference::checkConstrainedExtension(ExtensionDecl *ext) {
  auto typeInContext = dc->mapTypeIntoContext(adoptee);
  auto subs = typeInContext->getContextSubstitutions(ext);

  SubstOptions options = getSubstOptionsWithCurrentTypeWitnesses();
  switch (tc.checkGenericArguments(
                       dc, SourceLoc(), SourceLoc(), adoptee,
                       ext->getGenericSignature()->getGenericParams(),
                       ext->getGenericSignature()->getRequirements(),
                       QueryTypeSubstitutionMap{subs},
                       LookUpConformanceInModule(ext->getModuleContext()),
                       ConformanceCheckFlags::InExpression,
                       nullptr, options)) {
  case RequirementCheckResult::Success:
  case RequirementCheckResult::SubstitutionFailure:
    return false;

  case RequirementCheckResult::Failure:
    return true;
  }
  llvm_unreachable("unhandled result");
}

void AssociatedTypeInference::findSolutions(
                   ArrayRef<AssociatedTypeDecl *> unresolvedAssocTypes,
                   SmallVectorImpl<InferredTypeWitnessesSolution> &solutions) {
  SmallVector<InferredTypeWitnessesSolution, 4> nonViableSolutions;
  SmallVector<std::pair<ValueDecl *, ValueDecl *>, 4> valueWitnesses;
  findSolutionsRec(unresolvedAssocTypes, solutions, nonViableSolutions,
                   valueWitnesses, 0, 0, 0);
}

void AssociatedTypeInference::findSolutionsRec(
          ArrayRef<AssociatedTypeDecl *> unresolvedAssocTypes,
          SmallVectorImpl<InferredTypeWitnessesSolution> &solutions,
          SmallVectorImpl<InferredTypeWitnessesSolution> &nonViableSolutions,
          SmallVector<std::pair<ValueDecl *, ValueDecl *>, 4> &valueWitnesses,
          unsigned numTypeWitnesses,
          unsigned numValueWitnessesInProtocolExtensions,
          unsigned reqDepth) {
  using TypeWitnessesScope = decltype(typeWitnesses)::ScopeTy;

  // If we hit the last requirement, record and check this solution.
  if (reqDepth == inferred.size()) {
    // Introduce a hash table scope; we may add type witnesses here.
    TypeWitnessesScope typeWitnessesScope(typeWitnesses);

    // Check for completeness of the solution
    for (auto assocType : unresolvedAssocTypes) {
      // Local function to record a missing associated type.
      auto recordMissing = [&] {
        if (!missingTypeWitness)
          missingTypeWitness = assocType;
      };

      auto typeWitness = typeWitnesses.begin(assocType);
      if (typeWitness != typeWitnesses.end()) {
        // The solution contains an error.
        if (typeWitness->first->hasError()) {
          recordMissing();
          return;
        }

        continue;
      }

      // Try to compute the type without the aid of a specific potential
      // witness.
      if (Type type = computeAbstractTypeWitness(assocType,
                                                 /*allowDerived=*/true)) {
        if (type->hasError()) {
          recordMissing();
          return;
        }

        typeWitnesses.insert(assocType, {type, reqDepth});
        continue;
      }

      // The solution is incomplete.
      recordMissing();
      return;
    }

    ++NumSolutionStates;

    // Fold the dependent member types within this type.
    for (auto assocType : proto->getAssociatedTypeMembers()) {
      if (conformance->hasTypeWitness(assocType))
        continue;

      // If the type binding does not have a type parameter, there's nothing
      // to do.
      auto known = typeWitnesses.begin(assocType);
      assert(known != typeWitnesses.end());
      if (!known->first->hasTypeParameter() &&
          !known->first->hasDependentMember())
        continue;

      Type replaced = substCurrentTypeWitnesses(known->first);
      if (replaced.isNull())
        return;

      known->first = replaced;
    }

    // Check whether our current solution matches the given solution.
    auto matchesSolution =
        [&](const InferredTypeWitnessesSolution &solution) {
      for (const auto &existingTypeWitness : solution.TypeWitnesses) {
        auto typeWitness = typeWitnesses.begin(existingTypeWitness.first);
        if (!typeWitness->first->isEqual(existingTypeWitness.second.first))
          return false;
      }

      return true;
    };

    // If we've seen this solution already, bail out; there's no point in
    // checking further.
    if (llvm::any_of(solutions, matchesSolution) ||
        llvm::any_of(nonViableSolutions, matchesSolution)) {
      ++NumDuplicateSolutionStates;
      return;
    }

    /// Check the current set of type witnesses.
    bool invalid = checkCurrentTypeWitnesses(valueWitnesses);

    auto &solutionList = invalid ? nonViableSolutions : solutions;
    solutionList.push_back(InferredTypeWitnessesSolution());
    auto &solution = solutionList.back();

    // Copy the type witnesses.
    for (auto assocType : unresolvedAssocTypes) {
      auto typeWitness = typeWitnesses.begin(assocType);
      solution.TypeWitnesses.insert({assocType, *typeWitness});
    }

    // Copy the value witnesses.
    solution.ValueWitnesses = valueWitnesses;
    solution.NumValueWitnessesInProtocolExtensions
      = numValueWitnessesInProtocolExtensions;

    // We're done recording the solution.
    return;
  }

  // Iterate over the potential witnesses for this requirement,
  // looking for solutions involving each one.
  const auto &inferredReq = inferred[reqDepth];
  for (const auto &witnessReq : inferredReq.second) {
    llvm::SaveAndRestore<unsigned> savedNumTypeWitnesses(numTypeWitnesses);

    // If we inferred a type witness via a default, try both with and without
    // the default.
    if (isa<TypeDecl>(inferredReq.first)) {
      // Recurse without considering this type.
      valueWitnesses.push_back({inferredReq.first, nullptr});
      findSolutionsRec(unresolvedAssocTypes, solutions, nonViableSolutions,
                       valueWitnesses, numTypeWitnesses,
                       numValueWitnessesInProtocolExtensions, reqDepth + 1);
      valueWitnesses.pop_back();

      ++numTypeWitnesses;
      for (const auto &typeWitness : witnessReq.Inferred) {
        auto known = typeWitnesses.begin(typeWitness.first);
        if (known != typeWitnesses.end()) continue;

        // Enter a new scope for the type witnesses hash table.
        TypeWitnessesScope typeWitnessesScope(typeWitnesses);
        typeWitnesses.insert(typeWitness.first, {typeWitness.second, reqDepth});

        valueWitnesses.push_back({inferredReq.first, witnessReq.Witness});
        findSolutionsRec(unresolvedAssocTypes, solutions, nonViableSolutions,
                         valueWitnesses, numTypeWitnesses,
                         numValueWitnessesInProtocolExtensions, reqDepth + 1);
        valueWitnesses.pop_back();
      }

      continue;
    }

    // Enter a new scope for the type witnesses hash table.
    TypeWitnessesScope typeWitnessesScope(typeWitnesses);

    // Record this value witness, popping it when we exit the current scope.
    valueWitnesses.push_back({inferredReq.first, witnessReq.Witness});
    if (!isa<TypeDecl>(inferredReq.first) &&
        witnessReq.Witness->getDeclContext()->getExtendedProtocolDecl())
      ++numValueWitnessesInProtocolExtensions;
    SWIFT_DEFER {
      if (!isa<TypeDecl>(inferredReq.first) &&
          witnessReq.Witness->getDeclContext()->getExtendedProtocolDecl())
        --numValueWitnessesInProtocolExtensions;

      valueWitnesses.pop_back();
    };

    // Introduce each of the type witnesses into the hash table.
    bool failed = false;
    for (const auto &typeWitness : witnessReq.Inferred) {
      // If we've seen a type witness for this associated type that
      // conflicts, there is no solution.
      auto known = typeWitnesses.begin(typeWitness.first);
      if (known != typeWitnesses.end()) {
        // Don't overwrite a defaulted associated type witness.
        if (isa<TypeDecl>(valueWitnesses[known->second].second))
          continue;

        // If witnesses for two different requirements inferred the same
        // type, we're okay.
        if (known->first->isEqual(typeWitness.second))
          continue;

        // If one has a type parameter remaining but the other does not,
        // drop the one with the type parameter.
        if ((known->first->hasTypeParameter() ||
             known->first->hasDependentMember())
            != (typeWitness.second->hasTypeParameter() ||
                typeWitness.second->hasDependentMember())) {
          if (typeWitness.second->hasTypeParameter() ||
              typeWitness.second->hasDependentMember())
            continue;

          known->first = typeWitness.second;
          continue;
        }

        if (!typeWitnessConflict ||
            numTypeWitnesses > numTypeWitnessesBeforeConflict) {
          typeWitnessConflict = {typeWitness.first,
                                 typeWitness.second,
                                 inferredReq.first,
                                 witnessReq.Witness,
                                 known->first,
                                 valueWitnesses[known->second].first,
                                 valueWitnesses[known->second].second};
          numTypeWitnessesBeforeConflict = numTypeWitnesses;
        }

        failed = true;
        break;
      }

      // Record the type witness.
      ++numTypeWitnesses;
      typeWitnesses.insert(typeWitness.first, {typeWitness.second, reqDepth});
    }

    if (failed)
      continue;

    // Recurse
    findSolutionsRec(unresolvedAssocTypes, solutions, nonViableSolutions,
                     valueWitnesses, numTypeWitnesses,
                     numValueWitnessesInProtocolExtensions, reqDepth + 1);
  }
}

static Comparison
compareDeclsForInference(TypeChecker &TC, DeclContext *DC,
                         ValueDecl *decl1, ValueDecl *decl2) {
  // TC.compareDeclarations assumes that it's comparing two decls that
  // apply equally well to a call site. We haven't yet inferred the
  // associated types for a type, so the ranking algorithm used by
  // compareDeclarations to score protocol extensions is inappropriate,
  // since we may have potential witnesses from extensions with mutually
  // exclusive associated type constraints, and compareDeclarations will
  // consider these unordered since neither extension's generic signature
  // is a superset of the other.

  // If one of the declarations is null, it implies that we're working with
  // a skipped associated type default. Prefer that default to something
  // that came from a protocol extension.
  if (!decl1 || !decl2) {
    if (!decl1 &&
        decl2 && decl2->getDeclContext()->getExtendedProtocolDecl())
      return Comparison::Worse;

    if (!decl2 &&
        decl1 && decl1->getDeclContext()->getExtendedProtocolDecl())
      return Comparison::Better;

    return Comparison::Unordered;
  }


  // If the witnesses come from the same decl context, score normally.
  auto dc1 = decl1->getDeclContext();
  auto dc2 = decl2->getDeclContext();

  if (dc1 == dc2)
    return TC.compareDeclarations(DC, decl1, decl2);

  auto isProtocolExt1 = (bool)dc1->getExtendedProtocolDecl();
  auto isProtocolExt2 = (bool)dc2->getExtendedProtocolDecl();

  // If one witness comes from a protocol extension, favor the one
  // from a concrete context.
  if (isProtocolExt1 != isProtocolExt2) {
    return isProtocolExt1 ? Comparison::Worse : Comparison::Better;
  }

  // If both witnesses came from concrete contexts, score normally.
  // Associated type inference shouldn't impact the result.
  // FIXME: It could, if someone constrained to ConcreteType.AssocType...
  if (!isProtocolExt1)
    return TC.compareDeclarations(DC, decl1, decl2);

  // Compare protocol extensions by which protocols they require Self to
  // conform to. If one extension requires a superset of the other's
  // constraints, it wins.
  auto sig1 = dc1->getGenericSignatureOfContext();
  auto sig2 = dc2->getGenericSignatureOfContext();

  // FIXME: Extensions sometimes have null generic signatures while
  // checking the standard library...
  if (!sig1 || !sig2)
    return TC.compareDeclarations(DC, decl1, decl2);

  auto selfParam = GenericTypeParamType::get(0, 0, TC.Context);

  // Collect the protocols required by extension 1.
  Type class1;
  SmallPtrSet<ProtocolDecl*, 4> protos1;

  std::function<void (ProtocolDecl*)> insertProtocol;
  insertProtocol = [&](ProtocolDecl *p) {
    if (!protos1.insert(p).second)
      return;

    for (auto parent : p->getInheritedProtocols())
      insertProtocol(parent);
  };

  for (auto &reqt : sig1->getRequirements()) {
    if (!reqt.getFirstType()->isEqual(selfParam))
      continue;
    switch (reqt.getKind()) {
    case RequirementKind::Conformance: {
      auto *proto = reqt.getSecondType()->castTo<ProtocolType>()->getDecl();
      insertProtocol(proto);
      break;
    }
    case RequirementKind::Superclass:
      class1 = reqt.getSecondType();
      break;

    case RequirementKind::SameType:
    case RequirementKind::Layout:
      break;
    }
  }

  // Compare with the protocols required by extension 2.
  Type class2;
  SmallPtrSet<ProtocolDecl*, 4> protos2;
  bool protos2AreSubsetOf1 = true;
  std::function<void (ProtocolDecl*)> removeProtocol;
  removeProtocol = [&](ProtocolDecl *p) {
    if (!protos2.insert(p).second)
      return;

    protos2AreSubsetOf1 &= protos1.erase(p);
    for (auto parent : p->getInheritedProtocols())
      removeProtocol(parent);
  };

  for (auto &reqt : sig2->getRequirements()) {
    if (!reqt.getFirstType()->isEqual(selfParam))
      continue;
    switch (reqt.getKind()) {
    case RequirementKind::Conformance: {
      auto *proto = reqt.getSecondType()->castTo<ProtocolType>()->getDecl();
      removeProtocol(proto);
      break;
    }
    case RequirementKind::Superclass:
      class2 = reqt.getSecondType();
      break;

    case RequirementKind::SameType:
    case RequirementKind::Layout:
      break;
    }
  }

  auto isClassConstraintAsStrict = [&](Type t1, Type t2) -> bool {
    if (!t1)
      return !t2;

    if (!t2)
      return true;

    return t2->isExactSuperclassOf(t1);
  };

  bool protos1AreSubsetOf2 = protos1.empty();
  // If the second extension requires strictly more protocols than the
  // first, it's better.
  if (protos1AreSubsetOf2 > protos2AreSubsetOf1
      && isClassConstraintAsStrict(class2, class1)) {
    return Comparison::Worse;
  // If the first extension requires strictly more protocols than the
  // second, it's better.
  } else if (protos2AreSubsetOf1 > protos1AreSubsetOf2
             && isClassConstraintAsStrict(class1, class2)) {
    return Comparison::Better;
  }

  // If they require the same set of protocols, or non-overlapping
  // sets, judge them normally.
  return TC.compareDeclarations(DC, decl1, decl2);
}

bool AssociatedTypeInference::isBetterSolution(
                      const InferredTypeWitnessesSolution &first,
                      const InferredTypeWitnessesSolution &second) {
  assert(first.ValueWitnesses.size() == second.ValueWitnesses.size());
  bool firstBetter = false;
  bool secondBetter = false;
  for (unsigned i = 0, n = first.ValueWitnesses.size(); i != n; ++i) {
    assert(first.ValueWitnesses[i].first == second.ValueWitnesses[i].first);
    auto firstWitness = first.ValueWitnesses[i].second;
    auto secondWitness = second.ValueWitnesses[i].second;
    if (firstWitness == secondWitness)
      continue;

    switch (compareDeclsForInference(tc, dc, firstWitness, secondWitness)) {
    case Comparison::Better:
      if (secondBetter)
        return false;

      firstBetter = true;
      break;

    case Comparison::Worse:
      if (firstBetter)
        return false;

      secondBetter = true;
      break;

    case Comparison::Unordered:
      break;
    }
  }

  return firstBetter;
}

bool AssociatedTypeInference::findBestSolution(
                   SmallVectorImpl<InferredTypeWitnessesSolution> &solutions) {
  if (solutions.empty()) return true;
  if (solutions.size() == 1) return false;

  // Find the smallest number of value witnesses found in protocol extensions.
  // FIXME: This is a silly heuristic that should go away.
  unsigned bestNumValueWitnessesInProtocolExtensions
    = solutions.front().NumValueWitnessesInProtocolExtensions;
  for (unsigned i = 1, n = solutions.size(); i != n; ++i) {
    bestNumValueWitnessesInProtocolExtensions
      = std::min(bestNumValueWitnessesInProtocolExtensions,
                 solutions[i].NumValueWitnessesInProtocolExtensions);
  }

  // Erase any solutions with more value witnesses in protocol
  // extensions than the best.
  solutions.erase(
    std::remove_if(solutions.begin(), solutions.end(),
                   [&](const InferredTypeWitnessesSolution &solution) {
                     return solution.NumValueWitnessesInProtocolExtensions >
                              bestNumValueWitnessesInProtocolExtensions;
                   }),
    solutions.end());

  // If we're down to one solution, success!
  if (solutions.size() == 1) return false;

  // Find a solution that's at least as good as the solutions that follow it.
  unsigned bestIdx = 0;
  for (unsigned i = 1, n = solutions.size(); i != n; ++i) {
    if (isBetterSolution(solutions[i], solutions[bestIdx]))
      bestIdx = i;
  }

  // Make sure that solution is better than any of the other solutions.
  bool ambiguous = false;
  for (unsigned i = 1, n = solutions.size(); i != n; ++i) {
    if (i != bestIdx && !isBetterSolution(solutions[bestIdx], solutions[i])) {
      ambiguous = true;
      break;
    }
  }

  // If the result was ambiguous, fail.
  if (ambiguous) {
    assert(solutions.size() != 1 && "should have succeeded somewhere above?");
    return true;

  }
  // Keep the best solution, erasing all others.
  if (bestIdx != 0)
    solutions[0] = std::move(solutions[bestIdx]);
  solutions.erase(solutions.begin() + 1, solutions.end());
  return false;
}

namespace {
  /// A failed type witness binding.
  struct FailedTypeWitness {
    /// The value requirement that triggered inference.
    ValueDecl *Requirement;

    /// The corresponding value witness from which the type witness
    /// was inferred.
    ValueDecl *Witness;

    /// The actual type witness that was inferred.
    Type TypeWitness;

    /// The failed type witness result.
    CheckTypeWitnessResult Result;
  };
} // end anonymous namespace

bool AssociatedTypeInference::diagnoseNoSolutions(
                         ArrayRef<AssociatedTypeDecl *> unresolvedAssocTypes,
                         ConformanceChecker &checker) {
  // If a defaulted type witness failed, diagnose it.
  if (failedDefaultedAssocType) {
    auto failedDefaultedAssocType = this->failedDefaultedAssocType;
    auto failedDefaultedWitness = this->failedDefaultedWitness;
    auto failedDefaultedResult = this->failedDefaultedResult;

    checker.diagnoseOrDefer(failedDefaultedAssocType, true,
      [failedDefaultedAssocType, failedDefaultedWitness,
       failedDefaultedResult](NormalProtocolConformance *conformance) {
        auto proto = conformance->getProtocol();
        auto &diags = proto->getASTContext().Diags;
        diags.diagnose(failedDefaultedAssocType,
                       diag::default_associated_type_req_fail,
                       failedDefaultedWitness,
                       failedDefaultedAssocType->getFullName(),
                       proto->getDeclaredType(),
                       failedDefaultedResult.getRequirement(),
                       failedDefaultedResult.isConformanceRequirement());
      });

    return true;
  }

  // Form a mapping from associated type declarations to failed type
  // witnesses.
  llvm::DenseMap<AssociatedTypeDecl *, SmallVector<FailedTypeWitness, 2>>
    failedTypeWitnesses;
  for (const auto &inferredReq : inferred) {
    for (const auto &inferredWitness : inferredReq.second) {
      for (const auto &nonViable : inferredWitness.NonViable) {
        failedTypeWitnesses[std::get<0>(nonViable)]
          .push_back({inferredReq.first, inferredWitness.Witness,
                      std::get<1>(nonViable), std::get<2>(nonViable)});
      }
    }
  }

  // Local function to attempt to diagnose potential type witnesses
  // that failed requirements.
  auto tryDiagnoseTypeWitness = [&](AssociatedTypeDecl *assocType) -> bool {
    auto known = failedTypeWitnesses.find(assocType);
    if (known == failedTypeWitnesses.end())
      return false;

    auto failedSet = std::move(known->second);
    checker.diagnoseOrDefer(assocType, true,
      [assocType, failedSet](NormalProtocolConformance *conformance) {
        auto proto = conformance->getProtocol();
        auto &diags = proto->getASTContext().Diags;
        diags.diagnose(assocType, diag::bad_associated_type_deduction,
                       assocType->getFullName(), proto->getFullName());
        for (const auto &failed : failedSet) {
          if (failed.Result.isError())
            continue;

          if ((!failed.TypeWitness->getAnyNominal() ||
               failed.TypeWitness->isExistentialType()) &&
              failed.Result.isConformanceRequirement()) {
            diags.diagnose(failed.Witness,
                           diag::associated_type_witness_conform_impossible,
                           assocType->getName(), failed.TypeWitness,
                           failed.Result.getRequirement());
            continue;
          }
          if (!failed.TypeWitness->getClassOrBoundGenericClass() &&
              failed.Result.isSuperclassRequirement()) {
            diags.diagnose(failed.Witness,
                           diag::associated_type_witness_inherit_impossible,
                           assocType->getName(), failed.TypeWitness,
                           failed.Result.getRequirement());
            continue;
          }

          diags.diagnose(failed.Witness,
                         diag::associated_type_deduction_witness_failed,
                         assocType->getName(),
                         failed.TypeWitness,
                         failed.Result.getRequirement(),
                         failed.Result.isConformanceRequirement());
        }
      });

    return true;
  };

  // Try to diagnose the first missing type witness we encountered.
  if (missingTypeWitness && tryDiagnoseTypeWitness(missingTypeWitness))
    return true;

  // Failing that, try to diagnose any type witness that failed a
  // requirement.
  for (auto assocType : unresolvedAssocTypes) {
    if (tryDiagnoseTypeWitness(assocType))
      return true;
  }

  // If we saw a conflict, complain about it.
  if (typeWitnessConflict) {
    auto typeWitnessConflict = this->typeWitnessConflict;

    checker.diagnoseOrDefer(typeWitnessConflict->AssocType, true,
      [typeWitnessConflict](NormalProtocolConformance *conformance) {
        auto &diags = conformance->getDeclContext()->getASTContext().Diags;
        diags.diagnose(typeWitnessConflict->AssocType,
                       diag::ambiguous_associated_type_deduction,
                       typeWitnessConflict->AssocType->getFullName(),
                       typeWitnessConflict->FirstType,
                       typeWitnessConflict->SecondType);

        diags.diagnose(typeWitnessConflict->FirstWitness,
                       diag::associated_type_deduction_witness,
                       typeWitnessConflict->FirstRequirement->getFullName(),
                       typeWitnessConflict->FirstType);
        diags.diagnose(typeWitnessConflict->SecondWitness,
                       diag::associated_type_deduction_witness,
                       typeWitnessConflict->SecondRequirement->getFullName(),
                       typeWitnessConflict->SecondType);
      });

    return true;
  }

  return false;
}

bool AssociatedTypeInference::diagnoseAmbiguousSolutions(
                  ArrayRef<AssociatedTypeDecl *> unresolvedAssocTypes,
                  ConformanceChecker &checker,
                  SmallVectorImpl<InferredTypeWitnessesSolution> &solutions) {
  for (auto assocType : unresolvedAssocTypes) {
    // Find two types that conflict.
    auto &firstSolution = solutions.front();

    // Local function to retrieve the value witness for the current associated
    // type within the given solution.
    auto getValueWitness = [&](InferredTypeWitnessesSolution &solution) {
      unsigned witnessIdx = solution.TypeWitnesses[assocType].second;
      if (witnessIdx < solution.ValueWitnesses.size())
        return solution.ValueWitnesses[witnessIdx];

      return std::pair<ValueDecl *, ValueDecl *>(nullptr, nullptr);
    };

    Type firstType = firstSolution.TypeWitnesses[assocType].first;

    // Extract the value witness used to deduce this associated type, if any.
    auto firstMatch = getValueWitness(firstSolution);

    Type secondType;
    std::pair<ValueDecl *, ValueDecl *> secondMatch;
    for (auto &solution : solutions) {
      Type typeWitness = solution.TypeWitnesses[assocType].first;
      if (!typeWitness->isEqual(firstType)) {
        secondType = typeWitness;
        secondMatch = getValueWitness(solution);
        break;
      }
    }

    if (!secondType)
      continue;

    // We found an ambiguity. diagnose it.
    checker.diagnoseOrDefer(assocType, true,
      [assocType, firstType, firstMatch, secondType, secondMatch](
        NormalProtocolConformance *conformance) {
        auto &diags = assocType->getASTContext().Diags;
        diags.diagnose(assocType, diag::ambiguous_associated_type_deduction,
                       assocType->getFullName(), firstType, secondType);

        auto diagnoseWitness = [&](std::pair<ValueDecl *, ValueDecl *> match,
                                   Type type){
          // If we have a requirement/witness pair, diagnose it.
          if (match.first && match.second) {
            diags.diagnose(match.second,
                           diag::associated_type_deduction_witness,
                           match.first->getFullName(), type);

            return;
          }

          // Otherwise, we have a default.
          diags.diagnose(assocType, diag::associated_type_deduction_default,
                         type)
            .highlight(assocType->getDefaultDefinitionTypeRepr()->getSourceRange());
        };

        diagnoseWitness(firstMatch, firstType);
        diagnoseWitness(secondMatch, secondType);
      });

    return true;
  }

  return false;
}

auto AssociatedTypeInference::solve(ConformanceChecker &checker)
    -> Optional<InferredTypeWitnesses> {
  // Track when we are checking type witnesses.
  ProtocolConformanceState initialState = conformance->getState();
  conformance->setState(ProtocolConformanceState::CheckingTypeWitnesses);
  SWIFT_DEFER { conformance->setState(initialState); };

  // Try to resolve type witnesses via name lookup.
  llvm::SetVector<AssociatedTypeDecl *> unresolvedAssocTypes;
  for (auto assocType : proto->getAssociatedTypeMembers()) {
    // If we already have a type witness, do nothing.
    if (conformance->hasTypeWitness(assocType))
      continue;

    // Try to resolve this type witness via name lookup, which is the
    // most direct mechanism, overriding all others.
    switch (checker.resolveTypeWitnessViaLookup(assocType)) {
    case ResolveWitnessResult::Success:
      // Success. Move on to the next.
      continue;

    case ResolveWitnessResult::ExplicitFailed:
      continue;

    case ResolveWitnessResult::Missing:
      // Note that we haven't resolved this associated type yet.
      unresolvedAssocTypes.insert(assocType);
      break;
    }
  }

  // Result variable to use for returns so that we get NRVO.
  Optional<InferredTypeWitnesses> result = InferredTypeWitnesses();

  // If we resolved everything, we're done.
  if (unresolvedAssocTypes.empty())
    return result;

  // Infer potential type witnesses from value witnesses.
  inferred = inferTypeWitnessesViaValueWitnesses(checker,
                                                 unresolvedAssocTypes);
  LLVM_DEBUG(llvm::dbgs() << "Candidates for inference:\n";
             dumpInferredAssociatedTypes(inferred));

  // Compute the set of solutions.
  SmallVector<InferredTypeWitnessesSolution, 4> solutions;
  findSolutions(unresolvedAssocTypes.getArrayRef(), solutions);

  // Go make sure that type declarations that would act as witnesses
  // did not get injected while we were performing checks above. This
  // can happen when two associated types in different protocols have
  // the same name, and validating a declaration (above) triggers the
  // type-witness generation for that second protocol, introducing a
  // new type declaration.
  // FIXME: This is ridiculous.
  for (auto assocType : unresolvedAssocTypes) {
    switch (checker.resolveTypeWitnessViaLookup(assocType)) {
    case ResolveWitnessResult::Success:
    case ResolveWitnessResult::ExplicitFailed:
      // A declaration that can become a witness has shown up. Go
      // perform the resolution again now that we have more
      // information.
      return solve(checker);

    case ResolveWitnessResult::Missing:
      // The type witness is still missing. Keep going.
      break;
    }
  }

  // Find the best solution.
  if (!findBestSolution(solutions)) {
    assert(solutions.size() == 1 && "Not a unique best solution?");
    // Form the resulting solution.
    auto &typeWitnesses = solutions.front().TypeWitnesses;
    for (auto assocType : unresolvedAssocTypes) {
      assert(typeWitnesses.count(assocType) == 1 && "missing witness");
      auto replacement = typeWitnesses[assocType].first;
      // FIXME: We can end up here with dependent types that were not folded
      // away for some reason.
      if (replacement->hasDependentMember())
        return None;

      if (replacement->hasArchetype()) {
        replacement = replacement->mapTypeOutOfContext();
      }

      result->push_back({assocType, replacement});
    }

    return result;
  }

  // Diagnose the complete lack of solutions.
  if (solutions.empty() &&
      diagnoseNoSolutions(unresolvedAssocTypes.getArrayRef(), checker))
    return None;

  // Diagnose ambiguous solutions.
  if (!solutions.empty() &&
      diagnoseAmbiguousSolutions(unresolvedAssocTypes.getArrayRef(), checker,
                                 solutions))
    return None;

  // Save the missing type witnesses for later diagnosis.
  checker.GlobalMissingWitnesses.insert(unresolvedAssocTypes.begin(),
                                        unresolvedAssocTypes.end());
  return None;
}

void ConformanceChecker::resolveTypeWitnesses() {
  // Attempt to infer associated type witnesses.
  AssociatedTypeInference inference(TC, Conformance);
  if (auto inferred = inference.solve(*this)) {
    for (const auto &inferredWitness : *inferred) {
      recordTypeWitness(inferredWitness.first, inferredWitness.second,
                        /*typeDecl=*/nullptr);
    }

    return;
  }

  // Conformance failed. Record errors for each of the witnesses.
  Conformance->setInvalid();

  // We're going to produce an error below. Mark each unresolved
  // associated type witness as erroneous.
  for (auto assocType : Proto->getAssociatedTypeMembers()) {
    // If we already have a type witness, do nothing.
    if (Conformance->hasTypeWitness(assocType))
      continue;

    recordTypeWitness(assocType, ErrorType::get(TC.Context), nullptr);
  }
}

void ConformanceChecker::resolveSingleTypeWitness(
       AssociatedTypeDecl *assocType) {
  // Ensure we diagnose if the witness is missing.
  SWIFT_DEFER {
    diagnoseMissingWitnesses(MissingWitnessDiagnosisKind::ErrorFixIt);
  };
  switch (resolveTypeWitnessViaLookup(assocType)) {
  case ResolveWitnessResult::Success:
  case ResolveWitnessResult::ExplicitFailed:
    // We resolved this type witness one way or another.
    return;

  case ResolveWitnessResult::Missing:
    // The type witness is still missing. Resolve all of the type witnesses.
    resolveTypeWitnesses();
    return;
  }
}

void ConformanceChecker::resolveSingleWitness(ValueDecl *requirement) {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Not a value witness");
  assert(!Conformance->hasWitness(requirement) && "Already resolved");

  // Note that we're resolving this witness.
  assert(ResolvingWitnesses.count(requirement) == 0 && "Currently resolving");
  ResolvingWitnesses.insert(requirement);
  SWIFT_DEFER { ResolvingWitnesses.erase(requirement); };

  // Make sure we've validated the requirement.
  if (!requirement->hasInterfaceType())
    TC.validateDecl(requirement);

  if (requirement->isInvalid() || !requirement->hasInterfaceType()) {
    Conformance->setInvalid();
    return;
  }

  if (!requirement->isProtocolRequirement())
    return;

  // Resolve all associated types before trying to resolve this witness.
  resolveTypeWitnesses();

  // If any of the type witnesses was erroneous, don't bother to check
  // this value witness: it will fail.
  for (auto assocType : getReferencedAssociatedTypes(requirement)) {
    if (Conformance->getTypeWitness(assocType)->hasError()) {
      Conformance->setInvalid();
      return;
    }
  }

  // Try to resolve the witness.
  switch (resolveWitnessTryingAllStrategies(requirement)) {
  case ResolveWitnessResult::Success:
    return;

  case ResolveWitnessResult::ExplicitFailed:
    Conformance->setInvalid();
    recordInvalidWitness(requirement);
    return;

  case ResolveWitnessResult::Missing:
    llvm_unreachable("Should have failed");
  }
}
