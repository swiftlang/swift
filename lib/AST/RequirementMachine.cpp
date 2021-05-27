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
#include <vector>

using namespace swift;
using namespace rewriting;

struct RequirementMachine::Implementation {
  llvm::DenseSet<const ProtocolDecl *> VisitedProtocols;
  std::vector<const ProtocolDecl *> Worklist;
  RewriteSystem System;
  bool Complete = false;
};

RequirementMachine::RequirementMachine(ASTContext &ctx)
    : Context(ctx) {
  Impl = new Implementation();
}

RequirementMachine::~RequirementMachine() {
  delete Impl;
}

void RequirementMachine::addGenericSignature(CanGenericSignature sig) {
  PrettyStackTraceGenericSignature debugStack("building rewrite system for", sig);

  if (Context.LangOpts.DebugRequirementMachine) {
    llvm::dbgs() << "Adding generic signature " << sig << " {\n";
  }

  for (const auto &req : sig->getRequirements())
    addRequirement(req, /*proto=*/nullptr);

  processWorklist();

  // FIXME: Add command line flag
  Impl->System.computeConfluentCompletion(100);

  markComplete();

  if (Context.LangOpts.DebugRequirementMachine) {
    llvm::dbgs() << "}\n";
  }
}

void RequirementMachine::addProtocolRequirementSignature(
    const ProtocolDecl *proto) {
  auto inserted = Impl->VisitedProtocols.insert(proto);
  if (!inserted.second)
    return;

  Impl->Worklist.push_back(proto);
}

void RequirementMachine::addRequirement(const Requirement &req,
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

    Impl->System.addRule(subjectTerm, constraintTerm);

    addProtocolRequirementSignature(proto);
    break;
  }
  case RequirementKind::Superclass:
    break;
  case RequirementKind::Layout:
    break;
  case RequirementKind::SameType: {
    auto otherType = CanType(req.getSecondType());

    // FIXME: Handle concrete types
    if (!otherType->isTypeParameter())
      break;

    auto otherTerm = getTermForType(otherType, proto);

    Impl->System.addRule(subjectTerm, otherTerm);
    break;
  }
  }
}

void RequirementMachine::addAssociatedType(const AssociatedTypeDecl *type,
                                           const ProtocolDecl *proto) {
  Term lhs;
  lhs.add(Atom::forProtocol(proto));
  lhs.add(Atom::forName(type->getName()));

  Term rhs;
  rhs.add(Atom::forAssociatedType(type));

  Impl->System.addRule(lhs, rhs);
}

void RequirementMachine::processWorklist() {
  while (!Impl->Worklist.empty()) {
    const auto *proto = Impl->Worklist.back();
    Impl->Worklist.pop_back();

    if (Context.LangOpts.DebugRequirementMachine) {
      llvm::dbgs() << "protocol "
                   << proto->getName() << " {\n";
    }

    for (const auto *type : proto->getAssociatedTypeMembers()) {
      addAssociatedType(type, proto);
    }

    for (const auto &req : proto->getRequirementSignature()) {
      addRequirement(req.getCanonical(), proto);
    }

    if (Context.LangOpts.DebugRequirementMachine) {
      llvm::dbgs() << "}\n";
    }
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

Term RequirementMachine::getTermForType(CanType paramType,
                                        const ProtocolDecl *proto) const {
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