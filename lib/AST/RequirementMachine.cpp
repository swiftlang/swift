//===--- RequirementMachine.cpp - Generics with term rewriting --*- C++ -*-===//
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

#include "swift/AST/RequirementMachine.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/ProtocolGraph.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/RewriteSystem.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <vector>

using namespace swift;
using namespace rewriting;

namespace {

struct RewriteSystemBuilder {
  ASTContext &Context;

  ProtocolGraph Protocols;
  std::vector<std::pair<Term, Term>> Rules;

  RewriteSystemBuilder(ASTContext &ctx) : Context(ctx) {}
  void addGenericSignature(CanGenericSignature sig);
  void addAssociatedType(const AssociatedTypeDecl *type,
                         const ProtocolDecl *proto);
  void addInheritedAssociatedType(const AssociatedTypeDecl *type,
                                  const ProtocolDecl *inherited,
                                  const ProtocolDecl *proto);
  void addRequirement(const Requirement &req,
                      const ProtocolDecl *proto);
};

} // end namespace

void RewriteSystemBuilder::addGenericSignature(CanGenericSignature sig) {
  Protocols.visitRequirements(sig->getRequirements());
  Protocols.computeTransitiveClosure();
  Protocols.computeLinearOrder();
  Protocols.computeInheritedAssociatedTypes();

  for (auto *proto : Protocols.Protocols) {
    if (Context.LangOpts.DebugRequirementMachine) {
      llvm::dbgs() << "protocol " << proto->getName() << " {\n";
    }

    const auto &info = Protocols.getProtocolInfo(proto);

    for (auto *type : info.AssociatedTypes)
      addAssociatedType(type, proto);

    for (auto *inherited : info.Inherited) {
      auto inheritedTypes = Protocols.getProtocolInfo(inherited).AssociatedTypes;
      for (auto *inheritedType : inheritedTypes) {
        addInheritedAssociatedType(inheritedType, inherited, proto);
      }
    }

    for (auto req : info.Requirements)
      addRequirement(req.getCanonical(), proto);

    if (Context.LangOpts.DebugRequirementMachine) {
      llvm::dbgs() << "}\n";
    }
  }

  for (const auto &req : sig->getRequirements())
    addRequirement(req, /*proto=*/nullptr);
}

void RewriteSystemBuilder::addAssociatedType(const AssociatedTypeDecl *type,
                                             const ProtocolDecl *proto) {
  Term lhs;
  lhs.add(Atom::forProtocol(proto));
  lhs.add(Atom::forName(type->getName()));

  Term rhs;
  rhs.add(Atom::forAssociatedType(proto, type->getName()));

  Rules.emplace_back(lhs, rhs);
}

void RewriteSystemBuilder::addInheritedAssociatedType(
                                                const AssociatedTypeDecl *type,
                                                const ProtocolDecl *inherited,
                                                const ProtocolDecl *proto) {
  Term lhs;
  lhs.add(Atom::forProtocol(proto));
  lhs.add(Atom::forAssociatedType(inherited, type->getName()));

  Term rhs;
  rhs.add(Atom::forAssociatedType(proto, type->getName()));

  Rules.emplace_back(lhs, rhs);
}

void RewriteSystemBuilder::addRequirement(const Requirement &req,
                                          const ProtocolDecl *proto) {
  if (Context.LangOpts.DebugRequirementMachine) {
    llvm::dbgs() << "+ ";
    req.dump(llvm::dbgs());
    llvm::dbgs() << "\n";
  }

  auto subjectType = CanType(req.getFirstType());
  auto subjectTerm = getTermForType(subjectType, proto);

  switch (req.getKind()) {
  case RequirementKind::Conformance: {
    auto *proto = req.getProtocolDecl();

    auto constraintTerm = subjectTerm;
    constraintTerm.add(Atom::forProtocol(proto));

    Rules.emplace_back(subjectTerm, constraintTerm);
    break;
  }
  case RequirementKind::Superclass:
    break;
  case RequirementKind::Layout: {
    auto constraintTerm = subjectTerm;
    constraintTerm.add(Atom::forLayout(req.getLayoutConstraint()));

    Rules.emplace_back(subjectTerm, constraintTerm);
    break;
  }
  case RequirementKind::SameType: {
    auto otherType = CanType(req.getSecondType());

    // FIXME: Handle concrete types
    if (!otherType->isTypeParameter())
      break;

    auto otherTerm = getTermForType(otherType, proto);

    Rules.emplace_back(subjectTerm, otherTerm);
    break;
  }
  }
}

Term swift::rewriting::getTermForType(CanType paramType,
                                      const ProtocolDecl *proto) {
  assert(paramType->isTypeParameter());

  SmallVector<Atom, 3> atoms;
  while (auto memberType = dyn_cast<DependentMemberType>(paramType)) {
    atoms.push_back(Atom::forName(memberType->getName()));
    paramType = memberType.getBase();
  }

  if (proto) {
    assert(proto->getSelfInterfaceType()->isEqual(paramType));
    atoms.push_back(Atom::forProtocol(proto));
  } else {
    atoms.push_back(Atom::forGenericParam(cast<GenericTypeParamType>(paramType)));
  }

  std::reverse(atoms.begin(), atoms.end());
  return Term(atoms);
}

struct RequirementMachine::Implementation {
  RewriteSystem System;
  bool Complete = false;

  Implementation() {}
};

RequirementMachine::RequirementMachine(ASTContext &ctx) : Context(ctx) {
  Impl = new Implementation();
}

RequirementMachine::~RequirementMachine() {
  delete Impl;
}

void RequirementMachine::addGenericSignature(CanGenericSignature sig) {
  PrettyStackTraceGenericSignature debugStack("building rewrite system for", sig);

  auto *Stats = Context.Stats;

  FrontendStatsTracer tracer(Stats, "build-rewrite-system");

  if (Context.LangOpts.DebugRequirementMachine) {
    llvm::dbgs() << "Adding generic signature " << sig << " {\n";
  }

  RewriteSystemBuilder builder(Context);
  builder.addGenericSignature(sig);

  Impl->System.initialize(std::move(builder.Rules),
                          std::move(builder.Protocols));

  auto result = Impl->System.computeConfluentCompletion(
      Context.LangOpts.RequirementMachineStepLimit,
      Context.LangOpts.RequirementMachineDepthLimit);

  switch (result) {
  case RewriteSystem::CompletionResult::Success:
    break;

  case RewriteSystem::CompletionResult::MaxIterations:
    llvm::errs() << "Generic signature " << sig
                 << " exceeds maximum completion step count\n";
    Impl->System.dump(llvm::errs());
    abort();

  case RewriteSystem::CompletionResult::MaxDepth:
    llvm::errs() << "Generic signature " << sig
                 << " exceeds maximum completion depth\n";
    Impl->System.dump(llvm::errs());
    abort();
  }

  markComplete();

  if (Context.LangOpts.DebugRequirementMachine) {
    llvm::dbgs() << "}\n";
  }
}

bool RequirementMachine::isComplete() const {
  return Impl->Complete;
}

void RequirementMachine::markComplete() {
  if (Context.LangOpts.DebugRequirementMachine) {
    Impl->System.dump(llvm::dbgs());
  }
  assert(!Impl->Complete);
  Impl->Complete = true;
}