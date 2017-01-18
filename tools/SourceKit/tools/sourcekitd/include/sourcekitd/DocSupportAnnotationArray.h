//===--- DocSupportAnnotationArray.h - --------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKITD_DOCSUPPORT_ANNOTATION_ARRAY_H
#define LLVM_SOURCEKITD_DOCSUPPORT_ANNOTATION_ARRAY_H

#include "sourcekitd/Internal.h"
#include "llvm/ADT/SmallString.h"

namespace SourceKit {
  struct DocEntityInfo;
}

namespace sourcekitd {

VariantFunctions *getVariantFunctionsForDocSupportAnnotationArray();

class DocSupportAnnotationArrayBuilder {
public:
  DocSupportAnnotationArrayBuilder();
  ~DocSupportAnnotationArrayBuilder();

  void add(const SourceKit::DocEntityInfo &Info);
  std::unique_ptr<llvm::MemoryBuffer> createBuffer();

private:
  struct Implementation;
  Implementation &Impl;
};

}

#endif
