//===--- DependencyGraph.cpp - Track intra-module dependencies ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Driver/DependencyGraph.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;

namespace {
  using DependencyMaskTy = DependencyGraphImpl::DependencyMaskTy;
  enum class DependencyKind : DependencyMaskTy {
    Name = 1 << 0,
    Type = 1 << 1
  };

  enum class DependencyDirection : bool {
    Depends,
    Provides
  };
} // end anonymous namespace

static bool
parseDependencyFile(llvm::MemoryBuffer &buffer, bool providesOnly,
                    llvm::function_ref<void(StringRef, DependencyKind,
                                            DependencyDirection)> callback) {
  using namespace llvm;

  // FIXME: Switch to a format other than YAML.
  llvm::SourceMgr SM;
  yaml::Stream stream(buffer.getMemBufferRef(), SM);
  auto I = stream.begin();
  if (I == stream.end() || !I->getRoot())
    return true;

  auto *topLevelMap = dyn_cast<yaml::MappingNode>(I->getRoot());
  if (!topLevelMap)
    return !isa<yaml::NullNode>(I->getRoot());

  SmallString<64> scratch;
  // FIXME: LLVM's YAML support does incremental parsing in such a way that
  // for-range loops break.
  for (auto i = topLevelMap->begin(), e = topLevelMap->end(); i != e; ++i) {
    if (isa<yaml::NullNode>(i->getValue()))
      continue;

    auto *key = cast<yaml::ScalarNode>(i->getKey());

    using KindPair = std::pair<DependencyKind, DependencyDirection>;

    KindPair dirAndKind = llvm::StringSwitch<KindPair>(key->getValue(scratch))
      .Case("top-level", std::make_pair(DependencyKind::Name,
                                        DependencyDirection::Depends))
      .Case("member-access", std::make_pair(DependencyKind::Type,
                                            DependencyDirection::Depends))
      .Case("provides", std::make_pair(DependencyKind::Name,
                                       DependencyDirection::Provides))
      .Case("nominals", std::make_pair(DependencyKind::Type,
                                       DependencyDirection::Provides));

    if (providesOnly && dirAndKind.second != DependencyDirection::Provides)
      continue;

    auto *entries = cast<yaml::SequenceNode>(i->getValue());
    for (const yaml::Node &rawEntry : *entries) {
      auto *entry = cast<yaml::ScalarNode>(&rawEntry);
      callback(entry->getValue(scratch), dirAndKind.first, dirAndKind.second);
    }
  }
  
  return false;
}

bool DependencyGraphImpl::loadFromPath(const void *node, StringRef path) {
  auto buffer = llvm::MemoryBuffer::getFile(path);
  if (!buffer)
    return true;
  return loadFromBuffer(node, *buffer.get());
}

bool DependencyGraphImpl::loadFromString(const void *node, StringRef data) {
  auto buffer = llvm::MemoryBuffer::getMemBuffer(data);
  return loadFromBuffer(node, *buffer);
}

bool DependencyGraphImpl::loadFromBuffer(const void *node,
                                         llvm::MemoryBuffer &buffer) {
  auto &provides = Provides[node];
  provides.clear();
  return parseDependencyFile(buffer, /*providesOnly=*/isMarked(node),
                             [this, node, &provides](StringRef name,
                                                     DependencyKind kind,
                                                     DependencyDirection dir) {
    auto kindAsMask = static_cast<DependencyMaskTy>(kind);
    switch (dir) {
    case DependencyDirection::Depends: {
      auto &entries = Dependencies[name];
      auto iter = std::find_if(entries.begin(), entries.end(),
                               [node](const DependencyPairTy &entry) -> bool {
        return node == entry.second;
      });
      if (iter == entries.end())
        entries.emplace_back(kindAsMask, node);
      else
        iter->first |= kindAsMask;
      break;
    }

    case DependencyDirection::Provides: {
      auto iter = std::find_if(provides.begin(), provides.end(),
                               [name](const ProvidesPairTy &entry) -> bool {
        return name == entry.second;
      });
      if (iter == provides.end())
        provides.emplace_back(kindAsMask, name);
      else
        iter->first |= kindAsMask;
      break;
    }
    }
  });
}

void
DependencyGraphImpl::markTransitive(SmallVectorImpl<const void *> &newlyMarked,
                                    const void *node) {
  assert(Provides.count(node) && "node is not in the graph");
  SmallVector<const void *, 16> worklist;

  auto addDependentsToWorklist = [&](const void *next) {
    auto allProvided = Provides.find(next);
    if (allProvided == Provides.end())
      return;

    for (const auto &provided : allProvided->second) {
      auto allDependents = Dependencies.find(provided.second);
      if (allDependents == Dependencies.end())
        continue;

      for (const auto &dependent : allDependents->second) {
        if ((provided.first & dependent.first) == 0)
          continue;
        if (isMarked(dependent.second))
          continue;
        worklist.push_back(dependent.second);
      }
    }
  };

  // Always mark through the starting node, even if it's already marked.
  Marked.insert(node);
  addDependentsToWorklist(node);

  while (!worklist.empty()) {
    const void *next = worklist.pop_back_val();
    if (!Marked.insert(next).second)
      continue;
    newlyMarked.push_back(next);
    addDependentsToWorklist(next);
  }
}
