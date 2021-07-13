//===--- VariableTypeArray.h - ----------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SOURCEKITD_VARIABLE_TYPE_ARRAY_H
#define LLVM_SOURCEKITD_VARIABLE_TYPE_ARRAY_H

#include "sourcekitd/Internal.h"

namespace SourceKit {
struct VariableType;
}

namespace sourcekitd {
VariantFunctions *getVariantFunctionsForVariableTypeArray();

class VariableTypeArrayBuilder {
public:
  VariableTypeArrayBuilder(llvm::StringRef PrintedTypes);
  ~VariableTypeArrayBuilder();

  void add(const SourceKit::VariableType &VarType);
  std::unique_ptr<llvm::MemoryBuffer> createBuffer();
  static VariantFunctions Funcs;

private:
  struct Implementation;
  Implementation &Impl;
};

} // namespace sourcekitd

#endif
