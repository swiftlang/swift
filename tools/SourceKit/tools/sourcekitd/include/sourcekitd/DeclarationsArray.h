//===--- DeclarationsArray.h - ----------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This is an array used in the response to editor.open.interface requests.
// It contains all declarations, identified by their Kind, Offset, and Length,
// and optionally includes a USR, if the declaration has one.
//===----------------------------------------------------------------------===//
#ifndef LLVM_SOURCEKITD_DECLARATIONS_ARRAY_H
#define LLVM_SOURCEKITD_DECLARATIONS_ARRAY_H

#include "sourcekitd/Internal.h"

namespace sourcekitd {

VariantFunctions *getVariantFunctionsForDeclarationsArray();

/// Builds an array for declarations by kind, offset, length, and optionally USR
class DeclarationsArrayBuilder {
public:
  DeclarationsArrayBuilder();
  ~DeclarationsArrayBuilder();

  void add(SourceKit::UIdent Kind, unsigned Offset, unsigned Length,
           llvm::StringRef USR);

  bool empty() const;

  std::unique_ptr<llvm::MemoryBuffer> createBuffer();

private:
  struct Implementation;
  Implementation &Impl;
};

} // namespace sourcekitd

#endif
