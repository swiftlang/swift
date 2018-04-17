//===--- SyntaxArena.h - Syntax Tree Memory Allocation ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines SyntaxArena that is Memory manager for Syntax nodes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_SYNTAXARENA_H
#define SWIFT_SYNTAX_SYNTAXARENA_H

#include "llvm/Support/Allocator.h"

namespace swift {
namespace syntax {

/// Memory manager for Syntax nodes.
class SyntaxArena {
  SyntaxArena(const SyntaxArena &) = delete;
  void operator=(const SyntaxArena &) = delete;

public:
  struct Implementation;
  Implementation &Impl;

  SyntaxArena();
  ~SyntaxArena();

  llvm::BumpPtrAllocator &getAllocator() const;
  void *Allocate(size_t size, size_t alignment);
  void *AllocateRawSyntax(size_t size, size_t alignment);
};

} // namespace syntax
} // namespace swift

#endif
