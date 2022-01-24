//===--- RewriteContext.cpp - Term rewriting allocation arena -------------===//
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

#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "RequirementMachine.h"
#include "RewriteSystem.h"
#include "RewriteContext.h"

using namespace swift;
using namespace rewriting;

/// Build a DebugOptions by parsing a comma-separated list of debug flags.
static DebugOptions parseDebugFlags(StringRef debugFlags) {
  DebugOptions result;

  SmallVector<StringRef, 2> debug;
  debugFlags.split(debug, ',');
  for (auto flagStr : debug) {
    auto flag = llvm::StringSwitch<Optional<DebugFlags>>(flagStr)
      .Case("simplify", DebugFlags::Simplify)
      .Case("add", DebugFlags::Add)
      .Case("merge", DebugFlags::Merge)
      .Case("completion", DebugFlags::Completion)
      .Case("concrete-unification", DebugFlags::ConcreteUnification)
      .Case("concretize-nested-types", DebugFlags::ConcretizeNestedTypes)
      .Case("homotopy-reduction", DebugFlags::HomotopyReduction)
      .Case("minimal-conformances", DebugFlags::MinimalConformances)
      .Case("protocol-dependencies", DebugFlags::ProtocolDependencies)
      .Case("minimization", DebugFlags::Minimization)
      .Default(None);
    if (!flag) {
      llvm::errs() << "Unknown debug flag in -debug-requirement-machine "
                   << flagStr << "\n";
      abort();
    }

    result |= *flag;
  }

  return result;
}

RewriteContext::RewriteContext(ASTContext &ctx)
    : Context(ctx),
      Stats(ctx.Stats),
      SymbolHistogram(Symbol::NumKinds),
      TermHistogram(4, /*Start=*/1),
      RuleTrieHistogram(16, /*Start=*/1),
      RuleTrieRootHistogram(16),
      PropertyTrieHistogram(16, /*Start=*/1),
      PropertyTrieRootHistogram(16),
      ConformanceRulesHistogram(16),
      MinimalConformancesHistogram(8, /*Start=*/2) {
  auto debugFlags = StringRef(ctx.LangOpts.DebugRequirementMachine);
  if (!debugFlags.empty())
    Debug = parseDebugFlags(debugFlags);
}

const llvm::TinyPtrVector<const ProtocolDecl *> &
RewriteContext::getInheritedProtocols(const ProtocolDecl *proto) {
  auto found = AllInherited.find(proto);
  if (found != AllInherited.end())
    return found->second;

  AllInherited.insert(std::make_pair(proto, TinyPtrVector<const ProtocolDecl *>()));

  llvm::SmallDenseSet<const ProtocolDecl *, 4> visited;
  llvm::TinyPtrVector<const ProtocolDecl *> protos;

  for (auto *inheritedProto : proto->getInheritedProtocols()) {
    if (!visited.insert(inheritedProto).second)
      continue;

    protos.push_back(inheritedProto);
    const auto &allInherited = getInheritedProtocols(inheritedProto);

    for (auto *otherProto : allInherited) {
      if (!visited.insert(otherProto).second)
        continue;

      protos.push_back(otherProto);
    }
  }

  auto &result = AllInherited[proto];
  std::swap(protos, result);
  return result;
}

unsigned RewriteContext::getProtocolSupport(
    const ProtocolDecl *proto) {
  return getInheritedProtocols(proto).size() + 1;
}

unsigned RewriteContext::getProtocolSupport(
    ArrayRef<const ProtocolDecl *> protos) {
  auto found = Support.find(protos);
  if (found != Support.end())
    return found->second;

  unsigned result;
  if (protos.size() == 1) {
    result = getProtocolSupport(protos[0]);
  } else {
    llvm::DenseSet<const ProtocolDecl *> visited;
    for (const auto *proto : protos) {
      visited.insert(proto);
      for (const auto *inheritedProto : getInheritedProtocols(proto))
        visited.insert(inheritedProto);
    }

    result = visited.size();
  }

  Support[protos] = result;
  return result;
}

int RewriteContext::compareProtocols(const ProtocolDecl *lhs,
                                     const ProtocolDecl *rhs) {
  unsigned lhsSupport = getProtocolSupport(lhs);
  unsigned rhsSupport = getProtocolSupport(rhs);

  if (lhsSupport != rhsSupport)
    return rhsSupport - lhsSupport;

  return TypeDecl::compare(lhs, rhs);
}

RequirementMachine *RewriteContext::getRequirementMachine(
    CanGenericSignature sig) {
  auto &machine = Machines[sig];
  if (machine) {
    if (!machine->isComplete()) {
      llvm::errs() << "Re-entrant construction of requirement "
                   << "machine for " << sig << "\n";
      abort();
    }

    return machine;
  }

  // Store this requirement machine before adding the signature,
  // to catch re-entrant construction via initWithGenericSignature()
  // below.
  auto *newMachine = new rewriting::RequirementMachine(*this);
  machine = newMachine;

  // This might re-entrantly invalidate 'machine', which is a reference
  // into Protos.
  newMachine->initWithGenericSignature(sig);
  return newMachine;
}

bool RewriteContext::isRecursivelyConstructingRequirementMachine(
    CanGenericSignature sig) {
  auto found = Machines.find(sig);
  if (found == Machines.end())
    return false;

  return !found->second->isComplete();
}

/// Implement Tarjan's algorithm to compute strongly-connected components in
/// the protocol dependency graph.
void RewriteContext::getProtocolComponentRec(
    const ProtocolDecl *proto,
    SmallVectorImpl<const ProtocolDecl *> &stack) {
  assert(Protos.count(proto) == 0);

  // Initialize the next component index and push the entry
  // on the stack
  {
    auto &entry = Protos[proto];
    entry.Index = NextComponentIndex;
    entry.LowLink = NextComponentIndex;
    entry.OnStack = 1;
  }

  NextComponentIndex++;
  stack.push_back(proto);

  // Look at each successor.
  for (auto *depProto : proto->getProtocolDependencies()) {
    auto found = Protos.find(depProto);
    if (found == Protos.end()) {
      // Successor has not yet been visited. Recurse.
      getProtocolComponentRec(depProto, stack);

      auto &entry = Protos[proto];
      assert(Protos.count(depProto) != 0);
      entry.LowLink = std::min(entry.LowLink, Protos[depProto].LowLink);
    } else if (found->second.OnStack) {
      // Successor is on the stack and hence in the current SCC.
      auto &entry = Protos[proto];
      entry.LowLink = std::min(entry.LowLink, found->second.Index);
    }
  }

  auto &entry = Protos[proto];

  // If this a root node, pop the stack and generate an SCC.
  if (entry.LowLink == entry.Index) {
    unsigned id = Components.size();
    SmallVector<const ProtocolDecl *, 3> protos;

    const ProtocolDecl *depProto = nullptr;
    do {
      depProto = stack.back();
      stack.pop_back();

      assert(Protos.count(depProto) != 0);
      Protos[depProto].OnStack = false;
      Protos[depProto].ComponentID = id;

      protos.push_back(depProto);
    } while (depProto != proto);

    if (Debug.contains(DebugFlags::ProtocolDependencies)) {
      llvm::dbgs() << "Connected component: [";
      bool first = true;
      for (auto *depProto : protos) {
        if (!first) {
          llvm::dbgs() << ", ";
        } else {
          first = false;
        }
        llvm::dbgs() << depProto->getName();
      }
      llvm::dbgs() << "]\n";
    }

    Components[id].Protos = Context.AllocateCopy(protos);
  }
}

/// Lazily construct a requirement machine for the given protocol's strongly
/// connected component (SCC) in the protocol dependency graph.
///
/// This can only be called once, to prevent multiple requirement machines
/// for being built with the same component.
ArrayRef<const ProtocolDecl *> RewriteContext::getProtocolComponent(
    const ProtocolDecl *proto) {
  auto found = Protos.find(proto);
  if (found == Protos.end()) {
    SmallVector<const ProtocolDecl *, 3> stack;
    getProtocolComponentRec(proto, stack);
    assert(stack.empty());

    found = Protos.find(proto);
    assert(found != Protos.end());
  }

  assert(Components.count(found->second.ComponentID) != 0);
  auto &component = Components[found->second.ComponentID];

  if (component.InProgress) {
    llvm::errs() << "Re-entrant construction of requirement "
                 << "machine for:";
    for (auto *proto : component.Protos)
      llvm::errs() << " " << proto->getName();
    llvm::errs() << "\n";
    abort();
  }

  component.InProgress = true;

  return component.Protos;
}

bool RewriteContext::isRecursivelyConstructingRequirementMachine(
    const ProtocolDecl *proto) {
  if (proto->isRequirementSignatureComputed())
    return false;

  auto found = Protos.find(proto);
  if (found == Protos.end())
    return false;

  auto component = Components.find(found->second.ComponentID);
  if (component == Components.end())
    return false;

  return component->second.InProgress;
}

/// We print stats in the destructor, which should get executed at the end of
/// a compilation job.
RewriteContext::~RewriteContext() {
  for (const auto &pair : Machines)
    delete pair.second;

  Machines.clear();

  if (Context.LangOpts.AnalyzeRequirementMachine) {
    llvm::dbgs() << "--- Requirement Machine Statistics ---\n";
    llvm::dbgs() << "\n* Symbol kind:\n";
    SymbolHistogram.dump(llvm::dbgs(), Symbol::Kinds);
    llvm::dbgs() << "\n* Term length:\n";
    TermHistogram.dump(llvm::dbgs());
    llvm::dbgs() << "\n* Rule trie fanout:\n";
    RuleTrieHistogram.dump(llvm::dbgs());
    llvm::dbgs() << "\n* Rule trie root fanout:\n";
    RuleTrieRootHistogram.dump(llvm::dbgs());
    llvm::dbgs() << "\n* Property trie fanout:\n";
    PropertyTrieHistogram.dump(llvm::dbgs());
    llvm::dbgs() << "\n* Property trie root fanout:\n";
    PropertyTrieRootHistogram.dump(llvm::dbgs());
    llvm::dbgs() << "\n* Conformance rules:\n";
    ConformanceRulesHistogram.dump(llvm::dbgs());
    llvm::dbgs() << "\n* Minimal conformance equations:\n";
    MinimalConformancesHistogram.dump(llvm::dbgs());
  }
}
