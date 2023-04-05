//===--- BlockList.h ---------------------------------------------*- C++ -*-===//
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
//
// This file defines some miscellaneous overloads of hash_value() and
// simple_display().
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_BLOCKLIST_H
#define SWIFT_BASIC_BLOCKLIST_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

enum class BlockListAction: uint8_t {
  ShouldUseBinaryModule = 0,
  ShouldUseTextualModule,
};

enum class BlockListKeyKind: uint8_t {
  ModuleName,
  ProjectName
};

class BlockListStore {
public:
  struct Implementation;
  bool hasBlockListAction(StringRef key, BlockListKeyKind keyKind,
                          BlockListAction action);
  BlockListStore();
  ~BlockListStore();
private:
  friend class ASTContext;
  void addConfigureFilePath(StringRef path);
  Implementation &Impl;
};

} // namespace swift

#endif // SWIFT_BASIC_BLOCKLIST_H
