//===--- RequirementMachineRequests.cpp - Request evaluator requests ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the main entry points for computing minimized generic
// signatures using the requirement machine via the request evaluator.
//
// The actual logic for finding a minimal set of rewrite rules is implemented in
// HomotopyReduction.cpp and MinimalConformances.cpp.
//
//===----------------------------------------------------------------------===//

#include "RequirementMachine.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DiagnosticsSema.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/RequirementSignature.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Statistic.h"
#include <memory>
#include <vector>
#include "RequirementLowering.h"

using namespace swift;
using namespace rewriting;

/// Builds the requirement signatures for each protocol in this strongly
/// connected component.
llvm::DenseMap<const ProtocolDecl *, RequirementSignature>
RequirementMachine::computeMinimalProtocolRequirements() {
  auto protos = System.getProtocols();

  assert(protos.size() > 0 &&
         "Not a protocol connected component rewrite system");
  assert(Params.empty() &&
         "Not a protocol connected component rewrite system");

  System.minimizeRewriteSystem();

  if (Dump) {
    llvm::dbgs() << "Minimized rewrite system:\n";
    dump(llvm::dbgs());
  }

  auto rules = System.getMinimizedProtocolRules();

  auto &ctx = Context.getASTContext();

  // Note that we build 'result' by iterating over 'protos' rather than
  // 'rules'; this is intentional, so that even if a protocol has no
  // rules, we still end up creating an entry for it in 'result'.
  llvm::DenseMap<const ProtocolDecl *, RequirementSignature> result;
  for (const auto *proto : protos) {
    auto genericParams = proto->getGenericSignature().getGenericParams();

    const auto &entry = rules[proto];

    std::vector<Requirement> reqs;
    std::vector<ProtocolTypeAlias> aliases;
    buildRequirementsFromRules(entry.Requirements,
                               entry.TypeAliases,
                               genericParams,
                               /*reconstituteSugar=*/true,
                               reqs, aliases);

    result[proto] = RequirementSignature(ctx.AllocateCopy(reqs),
                                         ctx.AllocateCopy(aliases),
                                         getErrors());
  }

  return result;
}

RequirementSignature
RequirementSignatureRequestRQM::evaluate(Evaluator &evaluator,
                                         ProtocolDecl *proto) const {
  ASTContext &ctx = proto->getASTContext();

  // First check if we have a deserializable requirement signature.
  if (proto->hasLazyRequirementSignature()) {
    // FIXME: (transitional) increment the redundant "always-on" counter.
    if (ctx.Stats)
      ++ctx.Stats->getFrontendCounters().NumLazyRequirementSignaturesLoaded;

    auto contextData = static_cast<LazyProtocolData *>(
        ctx.getOrCreateLazyContextData(proto, nullptr));

    SmallVector<Requirement, 2> requirements;
    SmallVector<ProtocolTypeAlias, 2> typeAliases;
    contextData->loader->loadRequirementSignature(
        proto, contextData->requirementSignatureData,
        requirements, typeAliases);
    return RequirementSignature(ctx.AllocateCopy(requirements),
                                ctx.AllocateCopy(typeAliases));
  }

  // We build requirement signatures for all protocols in a strongly connected
  // component at the same time.
  auto component = ctx.getRewriteContext().getProtocolComponent(proto);

  // Heap-allocate the requirement machine to save stack space.
  std::unique_ptr<RequirementMachine> machine(new RequirementMachine(
      ctx.getRewriteContext()));

  SmallVector<RequirementError, 4> errors;

  auto status = machine->initWithProtocolWrittenRequirements(component);
  if (status.first != CompletionResult::Success) {
    // All we can do at this point is diagnose and give each protocol an empty
    // requirement signature.
    for (const auto *otherProto : component) {
      ctx.Diags.diagnose(otherProto->getLoc(),
                         diag::requirement_machine_completion_failed,
                         /*protocol=*/1,
                         unsigned(status.first));

      auto rule = machine->getRuleAsStringForDiagnostics(status.second);
      ctx.Diags.diagnose(otherProto->getLoc(),
                         diag::requirement_machine_completion_rule,
                         rule);

      if (otherProto != proto) {
        ctx.evaluator.cacheOutput(
          RequirementSignatureRequestRQM{const_cast<ProtocolDecl *>(otherProto)},
          RequirementSignature(GenericSignatureErrorFlags::CompletionFailed));
      }
    }

    return RequirementSignature(GenericSignatureErrorFlags::CompletionFailed);
  }

  auto minimalRequirements = machine->computeMinimalProtocolRequirements();

  bool debug = machine->getDebugOptions().contains(DebugFlags::Minimization);

  // The requirement signature for the actual protocol that the result
  // was kicked off with.
  Optional<RequirementSignature> result;

  if (debug) {
    llvm::dbgs() << "\nRequirement signatures:\n";
  }

  for (const auto &pair : minimalRequirements) {
    auto *otherProto = pair.first;
    const auto &reqs = pair.second;

    // Dump the result if requested.
    if (debug) {
      llvm::dbgs() << "- Protocol " << otherProto->getName() << ": ";

      auto sig = GenericSignature::get(
          otherProto->getGenericSignature().getGenericParams(),
          reqs.getRequirements());

      PrintOptions opts;
      opts.ProtocolQualifiedDependentMemberTypes = true;
      sig.print(llvm::dbgs(), opts);
      llvm::dbgs() << "\n";
    }

    // Don't call setRequirementSignature() on the original proto; the
    // request evaluator will do it for us.
    if (otherProto == proto)
      result = reqs;
    else {
      auto temp = reqs;
      ctx.evaluator.cacheOutput(
        RequirementSignatureRequestRQM{const_cast<ProtocolDecl *>(otherProto)},
        std::move(temp));
    }
  }

  if (ctx.LangOpts.RequirementMachineProtocolSignatures ==
      RequirementMachineMode::Enabled) {
    machine->System.computeRedundantRequirementDiagnostics(errors);
    diagnoseRequirementErrors(ctx, errors,
                              /*allowConcreteGenericParams=*/false);
  }

  // Return the result for the specific protocol this request was kicked off on.
  return *result;
}

/// Builds the top-level generic signature requirements for this rewrite system.
std::vector<Requirement>
RequirementMachine::computeMinimalGenericSignatureRequirements(
    bool reconstituteSugar) {
  assert(System.getProtocols().empty() &&
         "Not a top-level generic signature rewrite system");
  assert(!Params.empty() &&
         "Not a from-source top-level generic signature rewrite system");

  System.minimizeRewriteSystem();

  if (Dump) {
    llvm::dbgs() << "Minimized rewrite system:\n";
    dump(llvm::dbgs());
  }

  auto rules = System.getMinimizedGenericSignatureRules();

  std::vector<Requirement> reqs;
  std::vector<ProtocolTypeAlias> aliases;

  buildRequirementsFromRules(rules, ArrayRef<unsigned>(), getGenericParams(),
                             reconstituteSugar, reqs, aliases);
  assert(aliases.empty());

  return reqs;
}

/// Check whether the inputs to the \c AbstractGenericSignatureRequest are
/// all canonical.
static bool isCanonicalRequest(GenericSignature baseSignature,
                               ArrayRef<GenericTypeParamType *> genericParams,
                               ArrayRef<Requirement> requirements) {
  if (baseSignature && !baseSignature->isCanonical())
    return false;

  for (auto gp : genericParams) {
    if (!gp->isCanonical())
      return false;
  }

  for (const auto &req : requirements) {
    if (!req.isCanonical())
      return false;
  }

  return true;
}

GenericSignatureWithError
AbstractGenericSignatureRequestRQM::evaluate(
         Evaluator &evaluator,
         const GenericSignatureImpl *baseSignatureImpl,
         SmallVector<GenericTypeParamType *, 2> addedParameters,
         SmallVector<Requirement, 2> addedRequirements) const {
  GenericSignature baseSignature = GenericSignature{baseSignatureImpl};
  // If nothing is added to the base signature, just return the base
  // signature.
  if (addedParameters.empty() && addedRequirements.empty())
    return GenericSignatureWithError(baseSignature, GenericSignatureErrors());

  ASTContext &ctx = addedParameters.empty()
      ? addedRequirements.front().getFirstType()->getASTContext()
      : addedParameters.front()->getASTContext();

  SmallVector<GenericTypeParamType *, 4> genericParams(
      baseSignature.getGenericParams().begin(),
      baseSignature.getGenericParams().end());
  genericParams.append(
      addedParameters.begin(),
      addedParameters.end());

  // If there are no added requirements, we can form the signature directly
  // with the added parameters.
  if (addedRequirements.empty()) {
    auto result = GenericSignature::get(genericParams,
                                        baseSignature.getRequirements());
    return GenericSignatureWithError(result, GenericSignatureErrors());
  }

  // If the request is non-canonical, we won't need to build our own
  // generic signature builder.
  if (!isCanonicalRequest(baseSignature, addedParameters, addedRequirements)) {
    // Canonicalize the inputs so we can form the canonical request.
    auto canBaseSignature = baseSignature.getCanonicalSignature();

    SmallVector<GenericTypeParamType *, 2> canAddedParameters;
    canAddedParameters.reserve(addedParameters.size());
    for (auto gp : addedParameters) {
      auto canGP = gp->getCanonicalType()->castTo<GenericTypeParamType>();
      canAddedParameters.push_back(canGP);
    }

    SmallVector<Requirement, 2> canAddedRequirements;
    canAddedRequirements.reserve(addedRequirements.size());
    for (const auto &req : addedRequirements) {
      canAddedRequirements.push_back(req.getCanonical());
    }

    // Build the canonical signature.
    auto canSignatureResult = evaluateOrDefault(
        ctx.evaluator,
        AbstractGenericSignatureRequestRQM{
          canBaseSignature.getPointer(), std::move(canAddedParameters),
          std::move(canAddedRequirements)},
        GenericSignatureWithError());
    if (!canSignatureResult.getPointer())
      return GenericSignatureWithError();

    // Substitute in the original generic parameters to form the sugared
    // result the original request wanted.
    auto canSignature = canSignatureResult.getPointer();
    SmallVector<GenericTypeParamType *, 2> resugaredParameters;
    resugaredParameters.reserve(canSignature.getGenericParams().size());
    if (baseSignature) {
      resugaredParameters.append(baseSignature.getGenericParams().begin(),
                                 baseSignature.getGenericParams().end());
    }
    resugaredParameters.append(addedParameters.begin(), addedParameters.end());
    assert(resugaredParameters.size() ==
               canSignature.getGenericParams().size());

    SmallVector<Requirement, 2> resugaredRequirements;
    resugaredRequirements.reserve(canSignature.getRequirements().size());
    for (const auto &req : canSignature.getRequirements()) {
      auto resugaredReq = req.subst(
          [&](SubstitutableType *type) {
            if (auto gp = dyn_cast<GenericTypeParamType>(type)) {
              unsigned ordinal = canSignature->getGenericParamOrdinal(gp);
              return Type(resugaredParameters[ordinal]);
            }
            return Type(type);
          },
          MakeAbstractConformanceForGenericType(),
          SubstFlags::AllowLoweredTypes);
      resugaredRequirements.push_back(*resugaredReq);
    }

    return GenericSignatureWithError(
        GenericSignature::get(resugaredParameters, resugaredRequirements),
        canSignatureResult.getInt());
  }

  SmallVector<StructuralRequirement, 4> requirements;
  for (auto req : baseSignature.getRequirements())
    requirements.push_back({req, SourceLoc(), /*wasInferred=*/false});

  // We need to create this errors vector to pass to
  // desugarRequirement, but this request should never
  // diagnose errors.
  SmallVector<RequirementError, 4> errors;

  // The requirements passed to this request may have been substituted,
  // meaning the subject type might be a concrete type and not a type
  // parameter.
  //
  // Also, the right hand side of conformance requirements here might be
  // a protocol composition.
  //
  // Desugaring converts these kinds of requirements into "proper"
  // requirements where the subject type is always a type parameter,
  // which is what the RuleBuilder expects.
  for (auto req : addedRequirements) {
    SmallVector<Requirement, 2> reqs;
    desugarRequirement(req, reqs, errors);
    for (auto req : reqs)
      requirements.push_back({req, SourceLoc(), /*wasInferred=*/false});
  }

  // Preprocess requirements to eliminate conformances on generic parameters
  // which are made concrete.
  if (ctx.LangOpts.EnableRequirementMachineConcreteContraction) {
    SmallVector<StructuralRequirement, 4> contractedRequirements;
    if (performConcreteContraction(requirements, contractedRequirements,
                                   ctx.getRewriteContext().getDebugOptions()
                                      .contains(DebugFlags::ConcreteContraction))) {
      std::swap(contractedRequirements, requirements);
    }
  }

  // Heap-allocate the requirement machine to save stack space.
  std::unique_ptr<RequirementMachine> machine(new RequirementMachine(
      ctx.getRewriteContext()));

  auto status =
      machine->initWithWrittenRequirements(genericParams, requirements);
  machine->checkCompletionResult(status.first);

  // We pass reconstituteSugar=false to ensure that if the original
  // requirements were canonical, the final signature remains canonical.
  auto minimalRequirements =
    machine->computeMinimalGenericSignatureRequirements(
        /*reconstituteSugar=*/false);

  auto result = GenericSignature::get(genericParams, minimalRequirements);
  auto errorFlags = machine->getErrors();

  if (!errorFlags)
    result.verify();

  return GenericSignatureWithError(result, errorFlags);
}

GenericSignatureWithError
InferredGenericSignatureRequestRQM::evaluate(
        Evaluator &evaluator,
        ModuleDecl *parentModule,
        const GenericSignatureImpl *parentSigImpl,
        GenericParamList *genericParamList,
        WhereClauseOwner whereClause,
        SmallVector<Requirement, 2> addedRequirements,
        SmallVector<TypeLoc, 2> inferenceSources,
        bool allowConcreteGenericParams) const {
  GenericSignature parentSig(parentSigImpl);

  auto &ctx = parentModule->getASTContext();

  SmallVector<GenericTypeParamType *, 4> genericParams(
      parentSig.getGenericParams().begin(),
      parentSig.getGenericParams().end());

  SmallVector<StructuralRequirement, 4> requirements;
  SmallVector<RequirementError, 4> errors;
  for (const auto &req : parentSig.getRequirements())
    requirements.push_back({req, SourceLoc(), /*wasInferred=*/false});

  const auto visitRequirement = [&](const Requirement &req,
                                    RequirementRepr *reqRepr) {
    realizeRequirement(req, reqRepr, parentModule, requirements, errors);
    return false;
  };

  SourceLoc loc;
  if (genericParamList) {
    loc = genericParamList->getLAngleLoc();

    // Extensions never have a parent signature.
    assert(genericParamList->getOuterParameters() == nullptr || !parentSig);

    // Collect all outer generic parameter lists.
    SmallVector<GenericParamList *, 2> gpLists;
    for (auto *outerParamList = genericParamList;
         outerParamList != nullptr;
         outerParamList = outerParamList->getOuterParameters()) {
      gpLists.push_back(outerParamList);
    }

    // The generic parameter lists must appear from innermost to outermost.
    // We walk them backwards to order outer parameters before inner
    // parameters.
    for (auto *gpList : llvm::reverse(gpLists)) {
      assert(gpList->size() > 0 &&
             "Parsed an empty generic parameter list?");

      for (auto *gpDecl : *gpList) {
        auto *gpType = gpDecl->getDeclaredInterfaceType()
                             ->castTo<GenericTypeParamType>();
        genericParams.push_back(gpType);

        realizeInheritedRequirements(gpDecl, gpType, parentModule,
                                     requirements, errors);
      }

      auto *lookupDC = (*gpList->begin())->getDeclContext();

      // Add the generic parameter list's 'where' clause to the builder.
      //
      // The only time generic parameter lists have a 'where' clause is
      // in SIL mode; all other generic declarations have a free-standing
      // 'where' clause, which will be visited below.
      WhereClauseOwner(lookupDC, gpList)
        .visitRequirements(TypeResolutionStage::Structural,
                           visitRequirement);
    }
  }

  if (whereClause) {
    if (loc.isInvalid())
      loc = whereClause.getLoc();

    std::move(whereClause).visitRequirements(
        TypeResolutionStage::Structural,
        visitRequirement);
  }

  // Perform requirement inference from function parameter and result
  // types and such.
  for (auto sourcePair : inferenceSources) {
    auto *typeRepr = sourcePair.getTypeRepr();
    auto loc = typeRepr ? typeRepr->getStartLoc() : SourceLoc();

    inferRequirements(sourcePair.getType(), loc, parentModule, requirements);
  }

  // Finish by adding any remaining requirements. This is used to introduce
  // inferred same-type requirements when building the generic signature of
  // an extension whose extended type is a generic typealias.
  for (const auto &req : addedRequirements)
    requirements.push_back({req, SourceLoc(), /*wasInferred=*/true});

  // Preprocess requirements to eliminate conformances on generic parameters
  // which are made concrete.
  if (ctx.LangOpts.EnableRequirementMachineConcreteContraction) {
    SmallVector<StructuralRequirement, 4> contractedRequirements;
    if (performConcreteContraction(requirements, contractedRequirements,
                                   ctx.getRewriteContext().getDebugOptions()
                                      .contains(DebugFlags::ConcreteContraction))) {
      std::swap(contractedRequirements, requirements);
    }
  }

  // Heap-allocate the requirement machine to save stack space.
  std::unique_ptr<RequirementMachine> machine(new RequirementMachine(
      ctx.getRewriteContext()));

  auto status =
      machine->initWithWrittenRequirements(genericParams, requirements);
  if (status.first != CompletionResult::Success) {
    ctx.Diags.diagnose(loc,
                       diag::requirement_machine_completion_failed,
                       /*protocol=*/0,
                       unsigned(status.first));

    auto rule = machine->getRuleAsStringForDiagnostics(status.second);
    ctx.Diags.diagnose(loc,
                       diag::requirement_machine_completion_rule,
                       rule);

    auto result = GenericSignature::get(genericParams,
                                        parentSig.getRequirements());
    return GenericSignatureWithError(
        result, GenericSignatureErrorFlags::CompletionFailed);
  }

  auto minimalRequirements =
    machine->computeMinimalGenericSignatureRequirements(
        /*reconstituteSugar=*/true);

  auto result = GenericSignature::get(genericParams, minimalRequirements);
  auto errorFlags = machine->getErrors();

  if (ctx.LangOpts.RequirementMachineInferredSignatures ==
      RequirementMachineMode::Enabled) {
    machine->System.computeRedundantRequirementDiagnostics(errors);
    diagnoseRequirementErrors(ctx, errors, allowConcreteGenericParams);
  }

  // FIXME: Handle allowConcreteGenericParams

  // Check invariants.
  if (!errorFlags)
    result.verify();

  return GenericSignatureWithError(result, errorFlags);
}
