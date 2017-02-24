//===--- DependencyGraph.cpp - Track intra-module dependencies ------------===//
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

#include "swift/Driver/DependencyGraph.h"
#include "swift/Basic/Demangle.h"
#include "llvm/ADT/SmallString.h"
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
  NominalTypeMember = 1 << 3,
  ExternalFile = 1 << 4,
};
enum class DependencyGraphImpl::DependencyFlags : uint8_t {
  IsCascading = 1 << 0
};

class DependencyGraphImpl::MarkTracerImpl::Entry {
public:
  const void *Node;
  StringRef Name;
  DependencyMaskTy KindMask;
};

DependencyGraphImpl::MarkTracerImpl::MarkTracerImpl() = default;
DependencyGraphImpl::MarkTracerImpl::~MarkTracerImpl() = default;

using LoadResult = DependencyGraphImpl::LoadResult;
using DependencyKind = DependencyGraphImpl::DependencyKind;
using DependencyCallbackTy = LoadResult(StringRef, DependencyKind, bool);
using InterfaceHashCallbackTy = LoadResult(StringRef);

static LoadResult
parseDependencyFile(llvm::MemoryBuffer &buffer,
                    llvm::function_ref<DependencyCallbackTy> providesCallback,
                    llvm::function_ref<DependencyCallbackTy> dependsCallback,
                    llvm::function_ref<InterfaceHashCallbackTy> interfaceHashCallback) {
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

  // After an entry, we know more about the node as a whole.
  // Update the "result" variable above.
  // This is a macro rather than a lambda because it contains a return.
#define UPDATE_RESULT(update) switch (update) {\
    case LoadResult::HadError: \
      return LoadResult::HadError; \
    case LoadResult::UpToDate: \
      break; \
    case LoadResult::AffectsDownstream: \
      result = LoadResult::AffectsDownstream; \
      break; \
    } \

  // FIXME: LLVM's YAML support does incremental parsing in such a way that
  // for-range loops break.
  for (auto i = topLevelMap->begin(), e = topLevelMap->end(); i != e; ++i) {
    if (isa<yaml::NullNode>(i->getValue()))
      continue;

    auto *key = dyn_cast<yaml::ScalarNode>(i->getKey());
    if (!key)
      return LoadResult::HadError;
    StringRef keyString = key->getValue(scratch);

    if (keyString == "interface-hash") {
      auto *value = dyn_cast<yaml::ScalarNode>(i->getValue());
      if (!value)
        return LoadResult::HadError;

      StringRef valueString = value->getValue(scratch);
      UPDATE_RESULT(interfaceHashCallback(valueString));

    } else {
      enum class DependencyDirection : bool {
        Depends,
        Provides
      };
      using KindPair = std::pair<DependencyKind, DependencyDirection>;

      KindPair dirAndKind = llvm::StringSwitch<KindPair>(key->getValue(scratch))
        .Case("depends-top-level",
              std::make_pair(DependencyKind::TopLevelName,
                             DependencyDirection::Depends))
        .Case("depends-nominal",
              std::make_pair(DependencyKind::NominalType,
                             DependencyDirection::Depends))
        .Case("depends-member",
              std::make_pair(DependencyKind::NominalTypeMember,
                             DependencyDirection::Depends))
        .Case("depends-dynamic-lookup",
              std::make_pair(DependencyKind::DynamicLookupName,
                             DependencyDirection::Depends))
        .Case("depends-external",
              std::make_pair(DependencyKind::ExternalFile,
                             DependencyDirection::Depends))
        .Case("provides-top-level",
              std::make_pair(DependencyKind::TopLevelName,
                             DependencyDirection::Provides))
        .Case("provides-nominal",
              std::make_pair(DependencyKind::NominalType,
                             DependencyDirection::Provides))
        .Case("provides-member",
              std::make_pair(DependencyKind::NominalTypeMember,
                             DependencyDirection::Provides))
        .Case("provides-dynamic-lookup",
              std::make_pair(DependencyKind::DynamicLookupName,
                             DependencyDirection::Provides))
        .Default(std::make_pair(DependencyKind(),
                                DependencyDirection::Depends));
      if (dirAndKind.first == DependencyKind())
        return LoadResult::HadError;

      auto *entries = dyn_cast<yaml::SequenceNode>(i->getValue());
      if (!entries)
        return LoadResult::HadError;

      if (dirAndKind.first == DependencyKind::NominalTypeMember) {
        // Handle member dependencies specially. Rather than being a single
        // string, they come in the form ["{MangledBaseName}", "memberName"].
        for (yaml::Node &rawEntry : *entries) {
          bool isCascading = rawEntry.getRawTag() != "!private";

          auto *entry = dyn_cast<yaml::SequenceNode>(&rawEntry);
          if (!entry)
            return LoadResult::HadError;

          auto iter = entry->begin();
          auto *base = dyn_cast<yaml::ScalarNode>(&*iter);
          if (!base)
            return LoadResult::HadError;
          ++iter;

          auto *member = dyn_cast<yaml::ScalarNode>(&*iter);
          if (!member)
            return LoadResult::HadError;
          ++iter;

          // FIXME: LLVM's YAML support doesn't implement == correctly for end
          // iterators.
          assert(!(iter != entry->end()));

          bool isDepends = dirAndKind.second == DependencyDirection::Depends;
          auto &callback = isDepends ? dependsCallback : providesCallback;

          // Smash the type and member names together so we can continue using
          // StringMap.
          SmallString<64> appended;
          appended += base->getValue(scratch);
          appended.push_back('\0');
          appended += member->getValue(scratch);

          UPDATE_RESULT(callback(appended.str(), dirAndKind.first,
                                 isCascading));
        }
      } else {
        for (const yaml::Node &rawEntry : *entries) {
          auto *entry = dyn_cast<yaml::ScalarNode>(&rawEntry);
          if (!entry)
            return LoadResult::HadError;

          bool isDepends = dirAndKind.second == DependencyDirection::Depends;
          auto &callback = isDepends ? dependsCallback : providesCallback;

          UPDATE_RESULT(callback(entry->getValue(scratch), dirAndKind.first,
                                 entry->getRawTag() != "!private"));
        }
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

  auto interfaceHashCallback = [this, node](StringRef hash) -> LoadResult {
    auto insertResult = InterfaceHashes.insert(std::make_pair(node, hash));

    if (insertResult.second) {
      // Treat a newly-added hash as up-to-date. This includes the initial
      // load of the file.
      return LoadResult::UpToDate;
    }

    auto iter = insertResult.first;
    if (hash != iter->second) {
      iter->second = hash;
      return LoadResult::AffectsDownstream;
    }

    return LoadResult::UpToDate;
  };

  return parseDependencyFile(buffer, providesCallback, dependsCallback,
                             interfaceHashCallback);
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
                                    const void *node, MarkTracerImpl *tracer) {
  assert(Provides.count(node) && "node is not in the graph");
  llvm::SpecificBumpPtrAllocator<MarkTracerImpl::Entry> scratchAlloc;

  struct WorklistEntry {
    ArrayRef<MarkTracerImpl::Entry> Reason;
    const void *Node;
    bool IsCascading;
  };

  SmallVector<WorklistEntry, 16> worklist;
  SmallPtrSet<const void *, 16> visitedSet;

  auto addDependentsToWorklist = [&](const void *next,
                                     ArrayRef<MarkTracerImpl::Entry> reason) {
    auto allProvided = Provides.find(next);
    if (allProvided == Provides.end())
      return;

    for (const auto &provided : allProvided->second) {
      auto allDependents = Dependencies.find(provided.name);
      if (allDependents == Dependencies.end())
        continue;

      if (allDependents->second.second.contains(provided.kindMask))
        continue;

      // Record that we've traversed this dependency.
      allDependents->second.second |= provided.kindMask;

      for (const auto &dependent : allDependents->second.first) {
        if (dependent.node == next)
          continue;
        auto intersectingKinds = provided.kindMask & dependent.kindMask;
        if (!intersectingKinds)
          continue;
        if (isMarked(dependent.node))
          continue;
        bool isCascading{dependent.flags & DependencyFlags::IsCascading};

        MutableArrayRef<MarkTracerImpl::Entry> newReason;
        if (tracer) {
          newReason = {scratchAlloc.Allocate(reason.size()+1), reason.size()+1};
          std::uninitialized_copy(reason.begin(), reason.end(),
                                  newReason.begin());
          new (&newReason.back()) MarkTracerImpl::Entry({next, provided.name,
                                                         intersectingKinds});
        }
        worklist.push_back({ newReason, dependent.node, isCascading });
      }
    }
  };

  auto record = [&](WorklistEntry next) {
    if (!visitedSet.insert(next.Node).second)
      return;
    visited.push_back(next.Node);
    if (tracer) {
      auto &savedReason = tracer->Table[next.Node];
      savedReason.clear();
      savedReason.append(next.Reason.begin(), next.Reason.end());
    }
  };

  // Always mark through the starting node, even if it's already marked.
  markIntransitive(node);
  addDependentsToWorklist(node, {});

  while (!worklist.empty()) {
    auto next = worklist.pop_back_val();

    // Is this a non-cascading dependency?
    if (!next.IsCascading) {
      if (!isMarked(next.Node))
        record(next);
      continue;
    }

    addDependentsToWorklist(next.Node, next.Reason);
    if (!markIntransitive(next.Node))
      continue;
    record(next);
  }
}

void DependencyGraphImpl::MarkTracerImpl::printPath(
    raw_ostream &out,
    const void *item,
    llvm::function_ref<void (const void *)> printItem) const {
  for (const Entry &entry : Table.lookup(item)) {
    out << "\t";
    printItem(entry.Node);
    if (entry.KindMask.contains(DependencyKind::TopLevelName)) {
      out << " provides top-level name '" << entry.Name << "'\n";

    } else if (entry.KindMask.contains(DependencyKind::NominalType)) {
      SmallString<64> name{entry.Name};
      if (name.front() == 'P')
        name.push_back('_');
      out << " provides type '"
          << swift::Demangle::demangleTypeAsString(name.str())
          << "'\n";

    } else if (entry.KindMask.contains(DependencyKind::NominalTypeMember)) {
      SmallString<64> name{entry.Name};
      size_t splitPoint = name.find('\0');
      assert(splitPoint != StringRef::npos);

      StringRef typePart;
      if (name.front() == 'P') {
        name[splitPoint] = '_';
        typePart = name.str().slice(0, splitPoint+1);
      } else {
        typePart = name.str().slice(0, splitPoint);
      }
      StringRef memberPart = name.str().substr(splitPoint+1);

      out << " provides member '" << memberPart << "' of type '"
          << swift::Demangle::demangleTypeAsString(typePart)
          << "'\n";

    } else if (entry.KindMask.contains(DependencyKind::DynamicLookupName)) {
      out << " provides AnyObject member '" << entry.Name << "'\n";

    } else {
      llvm_unreachable("not a dependency kind between nodes");
    }
  }
}
