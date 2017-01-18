//===--- DocStructureArray.h - ----------------------------------*- C++ -*-===//
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

#ifndef LLVM_SOURCEKITD_DOC_STRUCTURE_ARRAY_H
#define LLVM_SOURCEKITD_DOC_STRUCTURE_ARRAY_H

#include "sourcekitd/Internal.h"
#include "llvm/ADT/SmallString.h"

namespace sourcekitd {

VariantFunctions *getVariantFunctionsForDocStructureArray();
VariantFunctions *getVariantFunctionsForDocStructureElementArray();
VariantFunctions *getVariantFunctionsForInheritedTypesArray();
VariantFunctions *getVariantFunctionsForAttributesArray();

class DocStructureArrayBuilder {
public:
  DocStructureArrayBuilder();
  ~DocStructureArrayBuilder();

  void beginSubStructure(unsigned Offset, unsigned Length,
                         SourceKit::UIdent Kind, SourceKit::UIdent AccessLevel,
                         SourceKit::UIdent SetterAccessLevel,
                         unsigned NameOffset, unsigned NameLength,
                         unsigned BodyOffset, unsigned BodyLength,
                         llvm::StringRef DisplayName, llvm::StringRef TypeName,
                         llvm::StringRef RuntimeName,
                         llvm::StringRef SelectorName,
                         llvm::ArrayRef<llvm::StringRef> InheritedTypes,
                         llvm::ArrayRef<SourceKit::UIdent> Attrs);

  void addElement(SourceKit::UIdent Kind, unsigned Offset, unsigned Length);

  void endSubStructure();

  std::unique_ptr<llvm::MemoryBuffer> createBuffer();

private:
  struct Implementation;
  Implementation &impl;
};

} // end namespace sourcekitd

#endif // LLVM_SOURCEKITD_DOC_STRUCTURE_ARRAY_H
