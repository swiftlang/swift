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
  TopLevelName = 1 << 0,
  DynamicLookupName = 1 << 1,
  NominalType = 1 << 2,
  ExternalFile = 1 << 3,
};
enum class DependencyGraphImpl::DependencyFlags : uint8_t {
  IsCascading = 1 << 0
};

using LoadResult = DependencyGraphImpl::LoadResult;
using DependencyKind = DependencyGraphImpl::DependencyKind;
using DependencyCallbackTy = LoadResult(StringRef, DependencyKind, bool);

static LoadResult
parseDependencyFile(llvm::MemoryBuffer &buffer,
                    llvm::function_ref<DependencyCallbackTy> providesCallback,
                    llvm::function_ref<DependencyCallbackTy> dependsCallback) {
  namespace yaml = llvm::yaml;

  // FIXME: Switch to a format other than YAML.
  llvm::SourceMgr SM;
  yaml::Stream stream(buffer.getMemBufferRef(), SM);
  auto I = stream.begin();
  if (I == stream.end() || !I->getRoot())
    return LoadResult::HadError;

  auto *topLevelMap = dyn_cast<yaml::MappingNode>(I->getRoot());
  if (!topLevelMap) {
    if (isa<yaml::NullNode>(I->getRoot()))
      return LoadResult::UpToDate;
    return LoadResult::HadError;
  }

  LoadResult result = LoadResult::UpToDate;
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
      .Case("top-level", std::make_pair(DependencyKind::TopLevelName,
                                        DependencyDirection::Depends))
      .Case("member-access", std::make_pair(DependencyKind::NominalType,
                                            DependencyDirection::Depends))
      .Case("dynamic-lookup", std::make_pair(DependencyKind::DynamicLookupName,
                                             DependencyDirection::Depends))
      .Case("cross-module", std::make_pair(DependencyKind::ExternalFile,
                                           DependencyDirection::Depends))
      .Case("provides", std::make_pair(DependencyKind::TopLevelName,
                                       DependencyDirection::Provides))
      .Case("nominals", std::make_pair(DependencyKind::NominalType,
                                       DependencyDirection::Provides))
      .Case("class-members", std::make_pair(DependencyKind::DynamicLookupName,
                                            DependencyDirection::Provides));

    auto *entries = cast<yaml::SequenceNode>(i->getValue());
    for (const yaml::Node &rawEntry : *entries) {
      auto *entry = cast<yaml::ScalarNode>(&rawEntry);

      auto &callback =
        (dirAndKind.second == DependencyDirection::Depends) ? dependsCallback
                                                            : providesCallback;

      switch (callback(entry->getValue(scratch), dirAndKind.first,
                       entry->getRawTag() != "!private")) {
      case LoadResult::HadError:
        return LoadResult::HadError;
      case LoadResult::UpToDate:
        break;
      case LoadResult::AffectsDownstream:
        result = LoadResult::AffectsDownstream;
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

  auto dependsCallback = [this, node](StringRef name, DependencyKind kind,
                                      bool isCascading) -> LoadResult {
    if (kind == DependencyKind::ExternalFile)
      ExternalDependencies.insert(name);

    auto &entries = Dependencies[name];
    auto iter = std::find_if(entries.first.begin(), entries.first.end(),
                             [node](const DependencyEntryTy &entry) -> bool {
      return node == entry.node;
    });

    DependencyFlagsTy flags;
    if (isCascading)
      flags |= DependencyFlags::IsCascading;

    if (iter == entries.first.end()) {
      entries.first.push_back({node, kind, flags});
    } else {
      iter->kindMask |= kind;
      iter->flags |= flags;
    }

    if (isCascading && (entries.second & kind))
      return LoadResult::AffectsDownstream;
    return LoadResult::UpToDate;
  };

  auto providesCallback =
      [this, node, &provides](StringRef name, DependencyKind kind,
                              bool isCascading) -> LoadResult {
    assert(isCascading);
    auto iter = std::find_if(provides.begin(), provides.end(),
                             [name](const ProvidesEntryTy &entry) -> bool {
      return name == entry.name;
    });

    if (iter == provides.end())
      provides.push_back({name, kind});
    else
      iter->kindMask |= kind;

    return LoadResult::UpToDate;
  };

  return parseDependencyFile(buffer, providesCallback, dependsCallback);
}

void DependencyGraphImpl::markExternal(SmallVectorImpl<const void *> &visited,
                                       StringRef externalDependency) {
  auto allDependents = Dependencies.find(externalDependency);
  assert(allDependents != Dependencies.end() && "not a dependency!");
  allDependents->second.second |= DependencyKind::ExternalFile;

  for (const auto &dependent : allDependents->second.first) {
    if (!dependent.kindMask.contains(DependencyKind::ExternalFile))
      continue;
    if (isMarked(dependent.node))
      continue;
    assert(dependent.flags & DependencyFlags::IsCascading);
    visited.push_back(dependent.node);
    markTransitive(visited, dependent.node);
  }
}

void
DependencyGraphImpl::markTransitive(SmallVectorImpl<const void *> &visited,
                                    const void *node) {
  assert(Provides.count(node) && "node is not in the graph");
  SmallVector<std::pair<const void *, bool>, 16> worklist;

  auto addDependentsToWorklist = [&](const void *next) {
    auto allProvided = Provides.find(next);
    if (allProvided == Provides.end())
      return;

    for (const auto &provided : allProvided->second) {
      auto allDependents = Dependencies.find(provided.name);
      if (allDependents == Dependencies.end())
        continue;

      // Record that we've traversed this dependency.
      allDependents->second.second |= provided.kindMask;

      for (const auto &dependent : allDependents->second.first) {
        if (!(provided.kindMask & dependent.kindMask))
          continue;
        if (isMarked(dependent.node))
          continue;
        bool isCascading{dependent.flags & DependencyFlags::IsCascading};
        worklist.push_back({ dependent.node, isCascading });
      }
    }
  };

  // Always mark through the starting node, even if it's already marked.
  markIntransitive(node);
  addDependentsToWorklist(node);

  while (!worklist.empty()) {
    auto next = worklist.pop_back_val();

    // Is this a non-cascading dependency?
    if (!next.second) {
      if (!isMarked(next.first))
        visited.push_back(next.first);
      continue;
    }

    addDependentsToWorklist(next.first);
    if (!markIntransitive(next.first))
      continue;
    visited.push_back(next.first);
  }
}
