//===--- CodeCompletionResultsArray.h - -------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKITD_CODECOMPLETION_RESULTS_ARRAY_H
#define LLVM_SOURCEKITD_CODECOMPLETION_RESULTS_ARRAY_H

#include "sourcekitd/Internal.h"

namespace sourcekitd {

VariantFunctions *getVariantFunctionsForCodeCompletionResultsArray();

class CodeCompletionResultsArrayBuilder {
public:
  CodeCompletionResultsArrayBuilder();
  ~CodeCompletionResultsArrayBuilder();

  void add(SourceKit::UIdent Kind, llvm::StringRef Name,
           llvm::StringRef Description, llvm::StringRef SourceText,
           llvm::StringRef TypeName, std::optional<llvm::StringRef> ModuleName,
           std::optional<llvm::StringRef> DocBrief,
           std::optional<llvm::StringRef> AssocUSRs,
           SourceKit::UIdent SemanticContext, SourceKit::UIdent TypeRelation,
           bool NotRecommended, bool IsSystem, unsigned NumBytesToErase);

  std::unique_ptr<llvm::MemoryBuffer> createBuffer();

private:
  struct Implementation;
  Implementation &Impl;
};

}

#endif
