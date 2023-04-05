//===--- BlockList.cpp - BlockList utilities ------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/YAMLParser.h"
#include "llvm/Support/YAMLTraits.h"
#include "swift/Basic/BlockList.h"
#include "swift/Basic/SourceManager.h"

struct swift::BlockListStore::Implementation {
  llvm::StringMap<std::vector<BlockListAction>> ModuleActionDict;
  llvm::StringMap<std::vector<BlockListAction>> ProjectActionDict;
  void addConfigureFilePath(StringRef path);
  bool hasBlockListAction(StringRef key, BlockListKeyKind keyKind,
                          BlockListAction action);
};

swift::BlockListStore::BlockListStore(): Impl(*new Implementation()) {}

swift::BlockListStore::~BlockListStore() { delete &Impl; }

bool swift::BlockListStore::hasBlockListAction(StringRef key,
    BlockListKeyKind keyKind, BlockListAction action) {
  return Impl.hasBlockListAction(key, keyKind, action);
}

void swift::BlockListStore::addConfigureFilePath(StringRef path) {
  Impl.addConfigureFilePath(path);
}

bool swift::BlockListStore::Implementation::hasBlockListAction(StringRef key,
    BlockListKeyKind keyKind, BlockListAction action) {
  auto *dict = keyKind == BlockListKeyKind::ModuleName ? &ModuleActionDict :
    &ProjectActionDict;
  auto it = dict->find(key);
  if (it == dict->end())
    return false;
  return llvm::is_contained(it->second, action);
}

void swift::BlockListStore::Implementation::addConfigureFilePath(StringRef path) {

}
