//===--- TokenAnnotationsArray.h - ------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKITD_TOKEN_ANNOTATIONS_ARRAY_H
#define LLVM_SOURCEKITD_TOKEN_ANNOTATIONS_ARRAY_H

#include "sourcekitd/Internal.h"

namespace sourcekitd {

VariantFunctions *getVariantFunctionsForTokenAnnotationsArray();

class TokenAnnotationsArrayBuilder {
public:
  TokenAnnotationsArrayBuilder();
  ~TokenAnnotationsArrayBuilder();

  void add(SourceKit::UIdent Kind,
           unsigned Offset,
           unsigned Length,
           bool IsSystem);

  bool empty() const;

  std::unique_ptr<llvm::MemoryBuffer> createBuffer();

private:
  struct Implementation;
  Implementation &Impl;
};

}

#endif
