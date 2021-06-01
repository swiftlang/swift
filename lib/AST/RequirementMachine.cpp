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
#include "swift/AST/Requirement.h"
#include "swift/AST/RewriteSystem.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <vector>

using namespace swift;
using namespace rewriting;

namespace {

struct ProtocolInfo {
  ArrayRef<ProtocolDecl *> Inherited;
  llvm::TinyPtrVector<AssociatedTypeDecl *> AssociatedTypes;
  ArrayRef<Requirement> Requirements;

  // Used by computeDepth() to detect circularity.
  unsigned Mark : 1;

  // Longest chain of protocol refinements, including this one.
  // Greater than zero on valid code, might be zero if there's
  // a cycle.
  unsigned Depth : 31;

  // Index of the protocol in the linear order.
  unsigned Index : 32;

  ProtocolInfo() {
    Mark = 0;
    Depth = 0;
    Index = 0;
  }

  ProtocolInfo(ArrayRef<ProtocolDecl *> inherited,
               llvm::TinyPtrVector<AssociatedTypeDecl *> &&types,
               ArrayRef<Requirement> reqs)
    : Inherited(inherited),
      AssociatedTypes(types),
      Requirements(reqs) {
    Mark = 0;
    Depth = 0;
    Index = 0;
  }
};

struct ProtocolGraph {
  llvm::DenseMap<const ProtocolDecl *, ProtocolInfo> Info;
  std::vector<const ProtocolDecl *> Protocols;
  bool Debug = false;

  void visitRequirements(ArrayRef<Requirement> reqs) {
    for (auto req : reqs) {
      if (req.getKind() == RequirementKind::Conformance) {
        addProtocol(req.getProtocolDecl());
      }
    }
  }

  const ProtocolInfo &getProtocolInfo(
      const ProtocolDecl *proto) const {
    auto found = Info.find(proto);
    assert(found != Info.end());
    return found->second;
  }

  void addProtocol(const ProtocolDecl *proto) {
    if (Info.count(proto) > 0)
      return;

    Info[proto] = {proto->getInheritedProtocols(),
                   proto->getAssociatedTypeMembers(),
                   proto->getRequirementSignature()};
    Protocols.push_back(proto);
  }

  void computeTransitiveClosure() {
    unsigned i = 0;
    while (i < Protocols.size()) {
      auto *proto = Protocols[i++];
      visitRequirements(getProtocolInfo(proto).Requirements);
    }
  }

  void computeLinearOrder() {
    for (const auto *proto : Protocols) {
      (void) computeProtocolDepth(proto);
    }

    std::sort(
        Protocols.begin(), Protocols.end(),
        [&](const ProtocolDecl *lhs,
            const ProtocolDecl *rhs) -> bool {
          const auto &lhsInfo = getProtocolInfo(lhs);
          const auto &rhsInfo = getProtocolInfo(rhs);

          // protocol Base {} // depth 1
          // protocol Derived : Base {} // depth 2
          //
          // Derived < Base in the linear order.
          if (lhsInfo.Depth != rhsInfo.Depth)
            return lhsInfo.Depth > rhsInfo.Depth;

          return TypeDecl::compare(lhs, rhs) < 0;
        });

    for (unsigned i : indices(Protocols)) {
      Info[Protocols[i]].Index = i;
    }

    if (Debug) {
      for (const auto *proto : Protocols) {
        const auto &info = getProtocolInfo(proto);
        llvm::dbgs() << "@ Protocol " << proto->getName()
                     << " Depth=" << info.Depth
                     << " Index=" << info.Index << "\n";
      }
    }
  }

  void computeInheritedAssociatedTypes() {
    for (const auto *proto : Protocols) {
      auto &info = Info[proto];

      llvm::SmallDenseSet<const AssociatedTypeDecl *, 4> visited;
      for (const auto *inherited : info.Inherited) {
        if (inherited == proto)
          continue;

        for (auto *inheritedType : getProtocolInfo(inherited).AssociatedTypes) {
          if (!visited.insert(inheritedType).second)
            continue;

          // The 'if (inherited == proto)' above avoids a potential
          // iterator invalidation here.
          info.AssociatedTypes.push_back(inheritedType);
        }
      }
    }
  }

private:
  unsigned computeProtocolDepth(const ProtocolDecl *proto) {
    auto &info = Info[proto];

    if (info.Mark) {
      // Already computed, or we have a cycle. Cycles are diagnosed
      // elsewhere in the type checker, so we don't have to do
      // anything here.
      return info.Depth;
    }

    info.Mark = true;
    unsigned depth = 0;

    for (auto *inherited : info.Inherited) {
      unsigned inheritedDepth = computeProtocolDepth(inherited);
      depth = std::max(inheritedDepth, depth);
    }

    depth++;

    info.Depth = depth;
    return depth;
  }
};

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
  ProtocolGraph Protocols;
  ProtocolOrder Order;
  RewriteSystem System;
  bool Complete = false;

  Implementation()
      : Order([&](const ProtocolDecl *lhs,
                  const ProtocolDecl *rhs) -> int {
          const auto &infoLHS = Protocols.getProtocolInfo(lhs);
          const auto &infoRHS = Protocols.getProtocolInfo(rhs);

          return infoLHS.Index - infoRHS.Index;
        }),
        System(Order) {}
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

  Impl->Protocols = builder.Protocols;

  std::sort(builder.Rules.begin(), builder.Rules.end(),
            [&](std::pair<Term, Term> lhs,
                std::pair<Term, Term> rhs) -> int {
              return lhs.first.compare(rhs.first, Impl->Order) < 0;
            });
  for (const auto &rule : builder.Rules)
    Impl->System.addRule(rule.first, rule.second);

  // FIXME: Add command line flag
  auto result = Impl->System.computeConfluentCompletion(1000, 10);

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