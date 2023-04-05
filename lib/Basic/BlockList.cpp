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

#include "swift/Basic/BlockList.h"

struct swift::BlockListStore::Implementation {
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
  return false;
}

void swift::BlockListStore::Implementation::addConfigureFilePath(StringRef path) {

}
