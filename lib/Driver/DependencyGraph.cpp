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

enum class DependencyGraphImpl::DependencyKind : uint8_t {
  Name = 1 << 0,
  Type = 1 << 1
};

using LoadResult = DependencyGraphImpl::LoadResult;
using DependencyKind = DependencyGraphImpl::DependencyKind;
using DependencyCallbackTy = LoadResult(StringRef, DependencyKind);

static LoadResult
parseDependencyFile(llvm::MemoryBuffer &buffer,
                    llvm::function_ref<DependencyCallbackTy> providesCallback,
                    llvm::function_ref<DependencyCallbackTy> dependsCallback) {
  using namespace llvm;

  // FIXME: Switch to a format other than YAML.
  llvm::SourceMgr SM;
  yaml::Stream stream(buffer.getMemBufferRef(), SM);
  auto I = stream.begin();
  if (I == stream.end() || !I->getRoot())
    return LoadResult::HadError;

  auto *topLevelMap = dyn_cast<yaml::MappingNode>(I->getRoot());
  if (!topLevelMap) {
    if (isa<yaml::NullNode>(I->getRoot()))
      return LoadResult::Valid;
    return LoadResult::HadError;
  }

  LoadResult result = LoadResult::Valid;
  SmallString<64> scratch;
  // FIXME: LLVM's YAML support does incremental parsing in such a way that
  // for-range loops break.
  for (auto i = topLevelMap->begin(), e = topLevelMap->end(); i != e; ++i) {
    if (isa<yaml::NullNode>(i->getValue()))
      continue;

    auto *key = cast<yaml::ScalarNode>(i->getKey());

    enum class DependencyDirection : bool {
      Depends,
      Provides
    };
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

    auto *entries = cast<yaml::SequenceNode>(i->getValue());
    for (const yaml::Node &rawEntry : *entries) {
      auto *entry = cast<yaml::ScalarNode>(&rawEntry);

      auto &callback =
        (dirAndKind.second == DependencyDirection::Depends) ? dependsCallback
                                                            : providesCallback;

      switch (callback(entry->getValue(scratch), dirAndKind.first)) {
      case LoadResult::HadError:
        return LoadResult::HadError;
      case LoadResult::Valid:
        break;
      case LoadResult::NeedsRebuilding:
        result = LoadResult::NeedsRebuilding;
        break;
      }
    }
  }
  
  return result;
}

LoadResult DependencyGraphImpl::loadFromPath(const void *node, StringRef path) {
  auto buffer = llvm::MemoryBuffer::getFile(path);
  if (!buffer)
    return LoadResult::HadError;
  return loadFromBuffer(node, *buffer.get());
}

LoadResult
DependencyGraphImpl::loadFromString(const void *node, StringRef data) {
  auto buffer = llvm::MemoryBuffer::getMemBuffer(data);
  return loadFromBuffer(node, *buffer);
}

LoadResult DependencyGraphImpl::loadFromBuffer(const void *node,
                                               llvm::MemoryBuffer &buffer) {
  auto &provides = Provides[node];

  auto dependsCallback = [this, node](StringRef name,
                                      DependencyKind kind) -> LoadResult {

    auto &entries = Dependencies[name];
    auto iter = std::find_if(entries.begin(), entries.end(),
                             [node](const DependencyEntryTy &entry) -> bool {
      return node == entry.node;
    });

    if (iter == entries.end())
      entries.push_back({node, kind});
    else
      iter->kindMask |= kind;

    // FIXME: This should return NeedsRebuilding if the dependency has already
    // been marked.
    return LoadResult::Valid;
  };

  auto providesCallback =
      [this, node, &provides](StringRef name,
                              DependencyKind kind) -> LoadResult {
    auto iter = std::find_if(provides.begin(), provides.end(),
                             [name](const ProvidesEntryTy &entry) -> bool {
      return name == entry.name;
    });

    if (iter == provides.end())
      provides.push_back({name, kind});
    else
      iter->kindMask |= kind;

    return LoadResult::Valid;
  };

  return parseDependencyFile(buffer, providesCallback, dependsCallback);
}

void
DependencyGraphImpl::markTransitive(SmallVectorImpl<const void *> &visited,
                                    const void *node) {
  assert(Provides.count(node) && "node is not in the graph");
  SmallVector<const void *, 16> worklist;

  auto addDependentsToWorklist = [&](const void *next) {
    auto allProvided = Provides.find(next);
    if (allProvided == Provides.end())
      return;

    for (const auto &provided : allProvided->second) {
      auto allDependents = Dependencies.find(provided.name);
      if (allDependents == Dependencies.end())
        continue;

      for (const auto &dependent : allDependents->second) {
        if (!(provided.kindMask & dependent.kindMask))
          continue;
        if (isMarked(dependent.node))
          continue;
        worklist.push_back(dependent.node);
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
    visited.push_back(next);
    addDependentsToWorklist(next);
  }
}
