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
